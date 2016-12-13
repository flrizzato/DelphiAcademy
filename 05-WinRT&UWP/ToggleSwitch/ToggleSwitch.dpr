program ToggleSwitch;

uses
  Vcl.Forms,
  uToggleSwitch in 'uToggleSwitch.pas' {ToggleSwitchForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'TToggleSwitch Demo';
  Application.CreateForm(TToggleSwitchForm, ToggleSwitchForm);
  Application.Run;
end.
