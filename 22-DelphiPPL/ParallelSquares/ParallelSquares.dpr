program ParallelSquares;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitFormMain in 'UnitFormMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
