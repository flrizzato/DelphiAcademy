program ClassHelpersDemo;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uClassHelpersDemo in 'uClassHelpersDemo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
