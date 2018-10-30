unit WebModuleU;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, Data.DBXJSON,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Phys.IBBase, FireDAC.Phys.IB,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Moni.Base,
  FireDAC.Moni.FlatFile, FireDAC.Moni.Custom, System.JSON,
  FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait;

type
  TwmMain = class(TWebModule)
    WebFileDispatcher1: TWebFileDispatcher;
    Connection: TFDConnection;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    qryPeople: TFDQuery;
    qryPeopleID: TFDAutoIncField;
    qryPeopleFIRST_NAME: TStringField;
    qryPeopleLAST_NAME: TStringField;
    qryPeopleWORK_PHONE_NUMBER: TStringField;
    qryPeopleMOBILE_PHONE_NUMBER: TStringField;
    qryPeopleEMAIL: TStringField;
    cmdUpdatePerson: TFDCommand;
    cmdInsertPerson: TFDCommand;
    procedure wmMainDefaultHandlerAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure wmMainwaGetPeopleAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure wmMainwaDeletePersonAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure wmMainwaSavePersonAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure ConnectionBeforeConnect(Sender: TObject);
  private
    procedure PrepareResponse(AJSONValue: TJSONValue;
      AWebResponse: TWebResponse);
  end;

var
  WebModuleClass: TComponentClass = TwmMain;

implementation

{$R *.dfm}

uses
  DataSetConverter4D.Helper,
  System.RegularExpressions,
  System.IOUtils;

procedure TwmMain.wmMainDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.SendRedirect('/index.html');
end;

procedure TwmMain.wmMainwaDeletePersonAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Connection.ExecSQL('DELETE FROM PEOPLE WHERE ID = ?',
    [Request.ContentFields.Values['id']]);
  PrepareResponse(nil, Response);
end;

procedure TwmMain.wmMainwaGetPeopleAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  JPeople: TJSONArray;
  SQL: string;
  OrderBy: string;
begin
  SQL := 'SELECT * FROM PEOPLE ';
  OrderBy := Request.QueryFields.Values['jtSorting'].Trim.ToUpper;
  if OrderBy.IsEmpty then
    SQL := SQL + 'ORDER BY FIRST_NAME ASC'
  else
  begin
    if TRegEx.IsMatch(OrderBy, '^[A-Z,_]+[ ]+(ASC|DESC)$') then
      SQL := SQL + 'ORDER BY ' + OrderBy
    else
      raise Exception.Create('Invalid order clause syntax');
  end;

  // execute query and prepare response
  qryPeople.Open(SQL);
  try
    JPeople := qryPeople.AsJSONArray;
  finally
    qryPeople.Close;
  end;
  PrepareResponse(JPeople, Response);
end;

procedure TwmMain.wmMainwaSavePersonAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  InsertMode: Boolean;
  LastID: Integer;
  HTTPFields: TStrings;
  procedure MapStringsToParams(AStrings: TStrings; AFDParams: TFDParams);
  var
    i: Integer;
  begin
    for i := 0 to HTTPFields.Count - 1 do
    begin
      if AStrings.ValueFromIndex[i].IsEmpty then
        AFDParams.ParamByName(AStrings.Names[i].ToUpper).Clear()
      else
        AFDParams.ParamByName(AStrings.Names[i].ToUpper).Value :=
          AStrings.ValueFromIndex[i];
    end;
  end;

begin
  HTTPFields := Request.ContentFields;
  InsertMode := HTTPFields.IndexOfName('id') = -1;
  if InsertMode then
  begin
    MapStringsToParams(HTTPFields, cmdInsertPerson.Params);
    cmdInsertPerson.Execute();
    LastID := Connection.GetLastAutoGenValue('GEN_PEOPLE_ID');
  end
  else
  begin
    MapStringsToParams(HTTPFields, cmdUpdatePerson.Params);
    cmdUpdatePerson.Execute();
    LastID := HTTPFields.Values['id'].ToInteger;
  end;

  // execute query and prepare response
  qryPeople.Open('SELECT * FROM PEOPLE WHERE ID = ?', [LastID]);
  try
    PrepareResponse(qryPeople.AsJSONObject, Response);
  finally
    qryPeople.Close;
  end;
end;

procedure TwmMain.ConnectionBeforeConnect(Sender: TObject);
begin
  Connection.Params.Values['Database'] :=
    TPath.GetDirectoryName(WebApplicationFileName) + '\..\SAMPLES.IB';
end;

procedure TwmMain.PrepareResponse(AJSONValue: TJSONValue;
  AWebResponse: TWebResponse);
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
    AWebResponse.Content := JObj.ToString;
    AWebResponse.StatusCode := 200;
    AWebResponse.ContentType := 'application/json';
  finally
    JObj.Free;
  end;
end;

end.
