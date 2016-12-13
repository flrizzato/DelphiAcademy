program EffectSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  EffectUnit1 in 'EffectUnit1.pas' {Form30};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm30, Form30);
  Application.Run;
end.
