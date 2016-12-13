program TransitionSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  TransitionUnit1 in 'TransitionUnit1.pas' {Form27};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm27, Form27);
  Application.Run;
end.
