program BaaS_Client;

uses
  System.StartUpCopy,
  FMX.Forms,
  uClientForm in 'uClientForm.pas' {MainForm},
  uClientDM in 'uClientDM.pas' {ClientDM: TDataModule},
  uSQLUtil in '..\Common\uSQLUtil.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TClientDM, ClientDM);
  Application.Run;
end.
