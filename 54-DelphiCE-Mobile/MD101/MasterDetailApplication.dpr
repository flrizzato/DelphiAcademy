program MasterDetailApplication;

uses
  System.StartUpCopy,
  FMX.Forms,
  MasterDetail in 'MasterDetail.pas' {MasterDetailForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMasterDetailForm, MasterDetailForm);
  Application.Run;
end.
