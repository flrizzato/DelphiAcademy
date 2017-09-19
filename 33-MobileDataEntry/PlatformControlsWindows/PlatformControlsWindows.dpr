program PlatformControlsWindows;

uses
  System.StartUpCopy,
  FMX.Forms,
  PlatformControlsEditMemoWindows in 'PlatformControlsEditMemoWindows.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
