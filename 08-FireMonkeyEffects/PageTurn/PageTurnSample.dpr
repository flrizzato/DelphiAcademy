program PageTurnSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  PageTurnUnit1 in 'PageTurnUnit1.pas' {PageTurnSampleForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPageTurnSampleForm, PageTurnSampleForm);
  Application.Run;
end.
