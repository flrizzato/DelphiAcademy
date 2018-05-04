unit ClientModuleUnit3;

interface

uses
  System.SysUtils, System.Classes, ClientClassesUnit3, Datasnap.DSClientRest,
  System.NET.URLClient;

type
  TClientModule3 = class(TDataModule)
    DSRestConnection1: TDSRestConnection;
    procedure DSRestConnection1Authentication(const Sender: TObject;
      AnAuthTarget: TAuthTargetType; const ARealm, AURL: string;
      var AUserName, APassword: string; var AbortAuth: Boolean;
      var Persistence: TAuthPersistenceType);
  private
    FInstanceOwner: Boolean;
    FServerMethods1Client: TServerMethods1Client;
    function GetServerMethods1Client: TServerMethods1Client;
    { Private declarations }
  public
    fUser: string;
    fPass: string;
    fAddress: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property InstanceOwner: Boolean read FInstanceOwner write FInstanceOwner;
    property ServerMethods1Client: TServerMethods1Client
      read GetServerMethods1Client write FServerMethods1Client;
  end;

var
  ClientModule3: TClientModule3;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

constructor TClientModule3.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := True;
end;

destructor TClientModule3.Destroy;
begin
  FServerMethods1Client.Free;
  inherited;
end;

procedure TClientModule3.DSRestConnection1Authentication(const Sender: TObject;
  AnAuthTarget: TAuthTargetType; const ARealm, AURL: string;
  var AUserName, APassword: string; var AbortAuth: Boolean;
  var Persistence: TAuthPersistenceType);
begin
//  AUserName := fUser;
//  APassword := fPass;
end;

function TClientModule3.GetServerMethods1Client: TServerMethods1Client;
begin
  if FServerMethods1Client = nil then
  begin
    //DSRestConnection1.Host := fAddress;
    FServerMethods1Client := TServerMethods1Client.Create(DSRestConnection1,
      FInstanceOwner);
  end;
  Result := FServerMethods1Client;
end;

end.
