program TaskFutureExample;

uses
  System.StartUpCopy,
  FMX.Forms,
  TaskFutureUnit in 'TaskFutureUnit.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
