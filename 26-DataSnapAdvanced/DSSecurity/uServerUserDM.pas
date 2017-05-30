unit uServerUserDM;

interface

uses
  System.SysUtils, System.Classes, Forms, System.Generics.Collections;

type
  TUser = class
    UserName: string;
    Password: string;
    UserLevel: string;
  end;

type
  TServerUserDM = class(TDataModule)
  private
    { Private declarations }
    aUserList: TDictionary<String, TUser>;
  strict private
    class var FInstance: TServerUserDM;
    { Private declarations }
    constructor Create(aOwner: TComponent); reintroduce;
  public
    class function GetInstance: TServerUserDM;
    function UserValidate(sUserName, sUserPass: string;
      var sUserLevel: string): boolean;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

constructor TServerUserDM.Create(aOwner: TComponent);
var
  aUser: TUser;
begin
  inherited;
  aUserList := TDictionary<String, TUser>.Create;

  aUser := TUser.Create;
  aUser.UserName := 'User1';
  aUser.Password := 'Pass1';
  aUser.UserLevel := 'Level1';
  aUserList.Add(aUser.UserName, aUser);

  aUser := TUser.Create;
  aUser.UserName := 'User2';
  aUser.Password := 'Pass2';
  aUser.UserLevel := 'Level2';
  aUserList.Add(aUser.UserName, aUser);

  aUser := TUser.Create;
  aUser.UserName := 'User3';
  aUser.Password := 'Pass3';
  aUser.UserLevel := 'Level3';
  aUserList.Add(aUser.UserName, aUser);
end;

class function TServerUserDM.GetInstance: TServerUserDM;
begin
  If FInstance = nil Then
  begin
    FInstance := uServerUserDM.TServerUserDM.Create(Application);
  end;
  Result := FInstance;
end;

function TServerUserDM.UserValidate(sUserName, sUserPass: string;
  var sUserLevel: string): boolean;
var
  aUser: TUser;
begin
  Result := False;
  if aUserList.TryGetValue(sUserName, aUser) then
   begin
     if aUser.Password = sUserPass then
      begin
        sUserLevel := aUser.UserLevel;
        Result := True;
      end;
   end;
end;

end.
