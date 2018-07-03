unit uUserControl;

interface

uses
  Classes, uAbstractControl, uUserDM;

type
  TUserControl = class(TAbstractControl)
  strict private
  var
    fUserDM: TUserDM;
    class var FInstance: TUserControl;
  public
    fUserID: integer;
    fUserLogin: string;
    fUserName: string;
    fUserIsAdmin: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateDefaultForm; override;
    procedure CreateDefaultDM; override;
    function DoUserLogin: boolean;
    function ValidateUserLogin(UserLogin, Password: string): boolean;
    function FindLoginName(UserID: integer; LoginName: string): boolean;
    function GetUserMail(UserID: integer): string;
    class function GetInstance: TUserControl;
    procedure AuthorizationLog;
  end;

implementation

uses
  Forms, SysUtils, Controls, DB, uUserLoginForm, uUserForm, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.DBX.Migrate;

{ TUserControl }

constructor TUserControl.Create(AOwner: TComponent);
begin
  fUserDM := nil;
  fUserForm := nil;
  inherited Create(AOwner);
end;

procedure TUserControl.CreateDefaultForm;
begin
  inherited;
  if fUserForm = nil then
  begin
    fUserForm := TUserForm.Create(Self);
    fUserForm.datControl.DataSet := fUserDM.cdsControl;
    fUserForm.datControl.DataSet.Open;
  end
  else
    fUserForm.Show;
end;

procedure TUserControl.CreateDefaultDM;
begin
  inherited;
  fUserDM := TUserDM.Create(Self);
  fUserDM.DBConnection := DBConnection;
end;

destructor TUserControl.Destroy;
begin
  inherited;
end;

function TUserControl.DoUserLogin: boolean;
var
  frmUserLoginForm: TUserLoginForm;
begin
  frmUserLoginForm := TUserLoginForm.Create(nil);
  try
    result := (frmUserLoginForm.ShowModal = mrOK);
  finally
    frmUserLoginForm.Free;
  end;
end;

function TUserControl.GetUserMail(UserID: integer): string;
var
  sqlFind: TFDQuery;
begin
  sqlFind := TFDQuery.Create(nil);
  try
    sqlFind.Connection := DBConnection;

    sqlFind.SQL.Add('SELECT EMAIL FROM USERS');
    sqlFind.SQL.Add(' WHERE USER_ID = ' + IntToStr(UserID));
    sqlFind.Open;

    if not sqlFind.IsEmpty then
      result := sqlFind.FieldByName('EMAIL').AsString
    else
      result := '';
  finally
    sqlFind.Free;
  end;
end;

function TUserControl.FindLoginName(UserID: integer; LoginName: string)
  : boolean;
var
  sqlFind: TFDQuery;
begin
  sqlFind := TFDQuery.Create(nil);
  try
    sqlFind.Connection := DBConnection;

    sqlFind.SQL.Add('SELECT * FROM USERS');
    sqlFind.SQL.Add(' WHERE LOGIN = ' + QuotedStr(LoginName));
    sqlFind.SQL.Add('   AND USER_ID <> ' + IntToStr(UserID));
    sqlFind.Open;

    result := (sqlFind.RecordCount > 0);
  finally
    sqlFind.Free;
  end;
end;

class function TUserControl.GetInstance: TUserControl;
begin
  if FInstance = nil then
  begin
    FInstance := uUserControl.TUserControl.Create(Application);
  end;
  result := FInstance;
end;

function TUserControl.ValidateUserLogin(UserLogin, Password: string): boolean;
var
  sqlLogin: TFDQuery;
begin
  sqlLogin := TFDQuery.Create(nil);
  try
    sqlLogin.Connection := DBConnection;

    sqlLogin.SQL.Add('SELECT * FROM USERS');
    sqlLogin.SQL.Add(' WHERE LOGIN = ' + QuotedStr(UserLogin));
    sqlLogin.SQL.Add('   AND PASSW = ' + QuotedStr(Password));
    sqlLogin.Open;

    if sqlLogin.RecordCount = 0 then
      result := false
    else
    begin
      fUserID := sqlLogin.FieldByName('USER_ID').AsInteger;
      fUserLogin := sqlLogin.FieldByName('LOGIN').AsString;
      fUserName := sqlLogin.FieldByName('NAME').AsString;
      fUserIsAdmin := (sqlLogin.FieldByName('ISADMIN').AsString = 'Y');

      result := True;
    end;
  finally
    sqlLogin.Free;
  end;
end;

procedure TUserControl.AuthorizationLog;
var
  sqlAutLog: TFDQuery;
  bLocalTrans: boolean;
begin
  sqlAutLog := TFDQuery.Create(nil);
  try
    sqlAutLog.Connection := DBConnection;

    sqlAutLog.SQL.Add('INSERT INTO AUTLOG (');
    sqlAutLog.SQL.Add('    AUTLOG_ID,');
    sqlAutLog.SQL.Add('    LOGINDATE,');
    sqlAutLog.SQL.Add('    LOGINTIME,');
    sqlAutLog.SQL.Add('    LOGINNAME');
    sqlAutLog.SQL.Add('    ) VALUES (');
    sqlAutLog.SQL.Add('    0, ');
    sqlAutLog.SQL.Add('CURRENT_DATE, ');
    sqlAutLog.SQL.Add('CURRENT_TIME, ');
    sqlAutLog.SQL.Add(QuotedStr(fUserLogin) + ')');

    bLocalTrans := false;
    if not DBConnection.InTransaction then
    begin
      bLocalTrans := True;
      DBConnection.StartTransaction;
    end;

    try
      sqlAutLog.ExecSQL(True);
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
    sqlAutLog.Free;
  end;
end;

end.
