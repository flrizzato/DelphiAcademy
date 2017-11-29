unit uHandles;

interface

uses
  Classes, System.SysUtils, System.JSON,
  IW.Content.Base, HTTPApp, IWApplication, IW.HTTP.Request, IW.HTTP.Reply,
  FireDAC.Comp.DataSet, FireDAC.Stan.Param;

type
  TJTable = class
    class procedure MapStringsToParams(const AStrings: TStrings;
      AFDParams: TFDParams);
    class function PrepareResponse(AJSONValue: TJSONValue): string;
  end;

type
  TPeopleActions = class(TContentBase)
  protected
    function Execute(aRequest: THttpRequest; aReply: THttpReply;
      const aPathname: string; aSession: TIWApplication; aParams: TStrings)
      : boolean; override;
  public
    constructor Create; override;
  end;

implementation

uses DataSetConverter4D.Helper, System.RegularExpressions,
  System.IOUtils, ServerController, UserSessionUnit;

{ TJTableUtil }

class procedure TJTable.MapStringsToParams(const AStrings: TStrings;
  AFDParams: TFDParams);
var
  i: Integer;
begin
  for i := 0 to AStrings.Count - 1 do
  begin
    if AStrings.ValueFromIndex[i].IsEmpty then
      AFDParams.ParamByName(AStrings.Names[i].ToUpper).Clear()
    else
      AFDParams.ParamByName(AStrings.Names[i].ToUpper).Value :=
        AStrings.ValueFromIndex[i];
  end;
end;

class function TJTable.PrepareResponse(AJSONValue: TJSONValue): string;
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.Create;
  try
    JObj.AddPair('Result', 'OK');
    if Assigned(AJSONValue) then
    begin
      if AJSONValue is TJSONArray then
        JObj.AddPair('Records', AJSONValue)
      else
        JObj.AddPair('Record', AJSONValue)
    end;
    Result := JObj.ToString;
  finally
    JObj.Free;
  end;
end;

{ TGetPeople }

constructor TPeopleActions.Create;
begin
  inherited;
  mFileMustExist := False;
end;

function TPeopleActions.Execute(aRequest: THttpRequest; aReply: THttpReply;
  const aPathname: string; aSession: TIWApplication; aParams: TStrings)
  : boolean;
var
  SQL: string;
  OrderBy: string;
  JResult: string;
begin
  Result := False;

  if aRequest.PathInfo.ToLower = '/getpeople' then
  begin
    SQL := 'SELECT * FROM PEOPLE ';
    OrderBy := aRequest.QueryFields.Values['jtSorting'].Trim.ToUpper;
    if OrderBy.IsEmpty then
      SQL := SQL + 'ORDER BY FIRST_NAME ASC'
    else
    begin
      if TRegEx.IsMatch(OrderBy, '^[A-Z,_]+[ ]+(ASC|DESC)$') then
        SQL := SQL + 'ORDER BY ' + OrderBy
      else
        raise Exception.Create('Invalid order clause syntax');
    end;

    UserSession.qryPeople.Open(SQL);
    try
      JResult := TJTable.PrepareResponse(UserSession.qryPeople.AsJSONArray);
    finally
      UserSession.qryPeople.Close;
    end;
  end

  else if aRequest.PathInfo.ToLower = '/createperson' then
  begin
    TJTable.MapStringsToParams(aRequest.ContentFields,
      UserSession.cmdInsertPerson.Params);

    UserSession.Connection.StartTransaction;
    try
      UserSession.cmdInsertPerson.Execute;
      UserSession.Connection.Commit;
    except
      on E: Exception do
      begin
        UserSession.Connection.Rollback;
        raise Exception.Create('createperson:' + E.Message);
      end;
    end;

    UserSession.qryPeople.Open('SELECT * FROM PEOPLE WHERE ID = ?',
      [UserSession.Connection.GetLastAutoGenValue('GEN_PEOPLE_ID')]);

    JResult := TJTable.PrepareResponse(UserSession.qryPeople.AsJSONObject);
  end

  else if aRequest.PathInfo.ToLower = '/updateperson' then
  begin
    TJTable.MapStringsToParams(aRequest.ContentFields,
      UserSession.cmdUpdatePerson.Params);

    UserSession.Connection.StartTransaction;
    try
      UserSession.cmdUpdatePerson.Execute;
      UserSession.Connection.Commit;
    except
      on E: Exception do
      begin
        UserSession.Connection.Rollback;
        raise Exception.Create('updateperson:' + E.Message);
      end;
    end;

    UserSession.qryPeople.Open('SELECT * FROM PEOPLE WHERE ID = ?',
      [aRequest.ContentFields.Values['id'].ToInteger]);

    JResult := TJTable.PrepareResponse(UserSession.qryPeople.AsJSONObject);
  end

  else if aRequest.PathInfo.ToLower = '/deleteperson' then
  begin
    UserSession.Connection.StartTransaction;
    try
      UserSession.cmdDeletePerson.Params[0].AsInteger :=
        aRequest.ContentFields.Values['id'].ToInteger;
      UserSession.cmdDeletePerson.Execute;
      UserSession.Connection.Commit;
    except
      on E: Exception do
      begin
        UserSession.Connection.Rollback;
        raise Exception.Create('deleteperson:' + E.Message);
      end;
    end;

    JResult := TJTable.PrepareResponse(nil);
  end;

  if Assigned(aReply) then
  begin
    Result := True;
    aReply.Code := 200;
    aReply.ContentType := 'application/json';
    aReply.WriteString(JResult);
  end;
end;

end.
