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
  TStyleManager.TrySetStyle('Windows10');
  Application.Title := 'TToggleSwitch Demo';
  Application.CreateForm(TToggleSwitchForm, ToggleSwitchForm);
  Application.Run;
end.
