program ActivityIndicator;

uses
  Vcl.Forms,
  uActivityIndicator in 'uActivityIndicator.pas' {ActivityIndicatorForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10');
  Application.CreateForm(TActivityIndicatorForm, ActivityIndicatorForm);
  Application.Run;
end.
