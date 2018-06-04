unit ClientModuleUnit3;

interface

uses
  System.SysUtils, System.Classes, ClientClassesUnit3, Datasnap.DSClientRest,
  System.NET.URLClient;

type
  TClientModule3 = class(TDataModule)
    DSRestConnection1: TDSRestConnection;
  private
    FInstanceOwner: Boolean;
    FServerMethods1Client: TServerMethods1Client;
    function GetServerMethods1Client: TServerMethods1Client;
    { Private declarations }
  public
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

function TClientModule3.GetServerMethods1Client: TServerMethods1Client;
begin
  if FServerMethods1Client = nil then
  begin
    FServerMethods1Client := TServerMethods1Client.Create(DSRestConnection1,
      FInstanceOwner);
  end;
  Result := FServerMethods1Client;
end;

end.
