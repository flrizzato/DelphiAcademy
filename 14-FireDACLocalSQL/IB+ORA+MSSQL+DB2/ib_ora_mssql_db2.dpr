program ib_ora_mssql_db2;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uMainDM in 'uMainDM.pas' {MainDM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainDM, MainDM);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
