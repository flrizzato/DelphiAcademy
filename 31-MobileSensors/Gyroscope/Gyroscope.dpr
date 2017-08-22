program Gyroscope;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {GyroscopeForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGyroscopeForm, GyroscopeForm);
  Application.Run;
end.
