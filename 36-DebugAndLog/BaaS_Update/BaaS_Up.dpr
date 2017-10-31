program BaaS_Up;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  DataSetConverter4D.Helper in '..\Common\DataSetConverter4D.Helper.pas',
  DataSetConverter4D.Impl in '..\Common\DataSetConverter4D.Impl.pas',
  DataSetConverter4D in '..\Common\DataSetConverter4D.pas',
  DataSetConverter4D.Util in '..\Common\DataSetConverter4D.Util.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
