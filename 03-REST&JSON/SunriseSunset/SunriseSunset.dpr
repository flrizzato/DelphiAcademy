program SunriseSunset;

uses
  System.StartUpCopy,
  System.Net.HttpClient in 'System.Net.HttpClient.pas',
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
