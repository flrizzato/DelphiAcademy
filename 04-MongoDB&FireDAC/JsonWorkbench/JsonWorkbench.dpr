program JsonWorkbench;

uses
  System.StartUpCopy,
  FMX.Forms,
  Converters in 'Converters.pas',
  fmWorkBench in 'fmWorkBench.pas' {WorkBenchForm},
  Writers in 'Writers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWorkBenchForm, WorkBenchForm);
  Application.Run;
end.
