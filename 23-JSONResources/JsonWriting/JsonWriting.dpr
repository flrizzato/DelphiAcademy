program JsonWriting;

uses
  System.StartUpCopy,
  FMX.Forms,
  uFormJsonWrite in 'uFormJsonWrite.pas' {FormJsonWrite};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormJsonWrite, FormJsonWrite);
  Application.Run;
end.
