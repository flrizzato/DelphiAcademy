program RubikCube;

uses
  System.StartUpCopy,
  FMX.Forms,
  UCommon in 'UCommon.pas' {DmCommon: TDataModule},
  ULaunchWebbrowser in 'ULaunchWebbrowser.pas',
  UMain in 'UMain.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDmCommon, DmCommon);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
