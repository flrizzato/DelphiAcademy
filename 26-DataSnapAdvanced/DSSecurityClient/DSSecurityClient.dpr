program DSSecurityClient;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  ClientClassesUnit1 in 'ClientClassesUnit1.pas',
  ClientModuleUnit1 in 'ClientModuleUnit1.pas' {ClientModule1: TDataModule},
  Datasnap.DSClientRest in 'Datasnap.DSClientRest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TClientModule1, ClientModule1);
  Application.Run;
end.
