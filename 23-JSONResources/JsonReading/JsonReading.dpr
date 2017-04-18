program JsonReading;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormJsonRead in 'uFormJsonRead.pas' {FormJsonRead},
  uCustomTypes in 'uCustomTypes.pas',
  JsonUtils in 'JsonUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormJsonRead, FormJsonRead);
  Application.Run;
end.
