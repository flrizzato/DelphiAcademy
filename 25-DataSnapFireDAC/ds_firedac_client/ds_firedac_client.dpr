program ds_firedac_client;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  ClientClassesUnit2 in 'ClientClassesUnit2.pas',
  ClientModuleUnit2 in 'ClientModuleUnit2.pas' {ClientModule2: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TClientModule2, ClientModule2);
  Application.Run;
end.
