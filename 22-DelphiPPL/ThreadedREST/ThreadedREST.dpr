program ThreadedREST;

uses
  Vcl.Forms,
  UnitFormMain in 'UnitFormMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
