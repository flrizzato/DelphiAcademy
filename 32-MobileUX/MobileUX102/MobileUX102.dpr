program MobileUX102;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uFormOne in 'uFormOne.pas' {Form1},
  uFormTwo in 'uFormTwo.pas' {Form2},
  uFormThree in 'uFormThree.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
