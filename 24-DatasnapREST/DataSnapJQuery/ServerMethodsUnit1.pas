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
    function Sum(const A, B: Double): Double;
  end;
{$METHODINFO OFF}

implementation


{$R *.dfm}


{ TServerMethods1 }

function TServerMethods1.Sum(const A, B: Double): Double;
begin
  Result := (A + B);
end;

end.

