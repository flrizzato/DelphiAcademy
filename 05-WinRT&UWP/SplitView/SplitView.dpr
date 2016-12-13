program SplitView;

uses
  Vcl.Forms,
  uSplitView in 'uSplitView.pas' {SplitViewForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'TSplitView Demo';
  Application.CreateForm(TSplitViewForm, SplitViewForm);
  Application.Run;
end.
