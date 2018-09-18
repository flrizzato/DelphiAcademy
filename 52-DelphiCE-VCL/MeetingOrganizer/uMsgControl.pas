unit uMsgControl;

interface

uses
  uAbstractControl, Classes;

type
  TMsgControl = class(TAbstractControl)
  public
    procedure PostNotification(email: string; Text: string);
    class function GetInstance: TMsgControl;
    procedure CreateDefaultForm; override;
    procedure CreateDefaultDM; override;
  strict private
    class var FInstance: TMsgControl;
  end;

implementation

uses
  Forms, SysUtils, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.DBX.Migrate;

procedure TMsgControl.PostNotification(email: string; Text: string);
var
  sqlPost: TFDQuery;
  bLocalTrans: boolean;
begin
  sqlPost := TFDQuery.Create(nil);
  try
    sqlPost.Connection := DBConnection;

    sqlPost.SQL.Add('INSERT INTO MSGLOG (');
    sqlPost.SQL.Add('    MSGLOG_ID,');
    sqlPost.SQL.Add('    EMAIL,');
    sqlPost.SQL.Add('    MSG,');
    sqlPost.SQL.Add('    SENT');
    sqlPost.SQL.Add('    ) VALUES (');
    sqlPost.SQL.Add('    0, ');
    sqlPost.SQL.Add(QuotedSTR(email) + ', ');
    sqlPost.SQL.Add(QuotedSTR(Text) + ', ');
    sqlPost.SQL.Add(QuotedSTR('N') + ')');

    bLocalTrans := False;
    if not DBConnection.InTransaction then
    begin
      bLocalTrans := True;
      DBConnection.StartTransaction;
    end;

    try
      sqlPost.ExecSQL(True);
      if (bLocalTrans) then
        DBConnection.Commit;
    except
      on E: Exception do
      begin
        if (bLocalTrans) then
          DBConnection.Rollback;
        raise Exception.Create(MO_ExceptionMsg + E.Message);
      end;
    end;

  finally
    sqlPost.Free;
  end;
end;

class function TMsgControl.GetInstance: TMsgControl;
begin
  if FInstance = nil then
  begin
    FInstance := uMsgControl.TMsgControl.Create(Application);
  end;
  Result := FInstance;
end;

procedure TMsgControl.CreateDefaultForm;
begin
  inherited;
end;

procedure TMsgControl.CreateDefaultDM;
begin
  inherited;
end;

end.
