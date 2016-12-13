program GoogleCalendar;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uMainDM in 'uMainDM.pas' {MainDM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainDM, MainDM);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
