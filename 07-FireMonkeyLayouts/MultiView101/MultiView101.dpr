program MultiView101;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uFormBase in 'uFormBase.pas' {FormBase},
  uFormOne in 'uFormOne.pas' {FormOne},
  uFormThree in 'uFormThree.pas' {FormThree},
  uFormTwo in 'uFormTwo.pas' {FormTwo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
