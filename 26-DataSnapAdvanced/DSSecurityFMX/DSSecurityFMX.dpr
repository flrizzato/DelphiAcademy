program DSSecurityFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  {$IFDEF ANDROID}
  System.Net.HttpClient.Android in 'lib\System.Net.HttpClient.Android.pas',
  {$ENDIF}
  uMainForm in 'uMainForm.pas' {MainForm},
  ClientClassesUnit3 in 'ClientClassesUnit3.pas',
  ClientModuleUnit3 in 'ClientModuleUnit3.pas' {ClientModule3: TDataModule},
  System.Net.HttpClient.Android in 'System.Net.HttpClient.Android.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TClientModule3, ClientModule3);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
