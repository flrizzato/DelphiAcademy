program mdb_xml;

uses
  Vcl.Forms,
  mainformu in 'mainformu.pas' {MainForm},
  datamodu in 'datamodu.pas' {MainDM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainDM, MainDM);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
