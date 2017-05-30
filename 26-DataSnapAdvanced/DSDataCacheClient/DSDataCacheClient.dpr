program DSDataCacheClient;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  ClientClassesUnit2 in 'ClientClassesUnit2.pas',
  ClientModuleUnit2 in 'ClientModuleUnit2.pas' {ClientModule2: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TClientModule2, ClientModule2);
  Application.Run;
end.
