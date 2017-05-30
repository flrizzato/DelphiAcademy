unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, System.Json,
    Datasnap.DSServer, Datasnap.DSAuth;

type
{$METHODINFO ON}
  TServerMethods1 = class(TDataModule)
  private
    { Private declarations }
  public
    { Public declarations }
    [TRoleAuth('Level1')]
    function EchoString(Value: string): string;
    [TRoleAuth('Level1, Level2')]
    function ReverseString(Value: string): string;
    [TRoleAuth('Level1, Level2, Level3')]
    function ServerDateTime: string;
  end;
{$METHODINFO OFF}

implementation


{$R *.dfm}

uses System.StrUtils;

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethods1.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

function TServerMethods1.ServerDateTime: string;
begin
  Result := FormatDateTime('ddmmyyyyhhnnss', Now);
end;

end.

