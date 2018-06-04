{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

program EMSManagementConsole;







uses
  System.StartUpCopy,
  FMX.Forms,
  EMSManagementConsole.Types in 'EMSManagementConsole.Types.pas',
  EMSManagementConsole.ModuleBackend in 'EMSManagementConsole.ModuleBackend.pas' {DataModule1: TDataModule},
  EMSManagementSetUp.Config in 'EMSManagementSetUp.Config.pas',
  EMSManagementConsole.Data in 'EMSManagementConsole.Data.pas',
  EMSManagementConsole.Consts in 'EMSManagementConsole.Consts.pas',
  EMSManagementConsole.TypesViews in 'EMSManagementConsole.TypesViews.pas',
  EMSManagementConsole.DlgModifyU in 'EMSManagementConsole.DlgModifyU.pas' {FormAddDlg},
  EMSManagementConsole.DlgPushChannelsU in 'EMSManagementConsole.DlgPushChannelsU.pas' {Dialog},
  EMSManagementConsole.DlgPushDataU in 'EMSManagementConsole.DlgPushDataU.pas' {Dialog},
  EMSManagementConsole.DlgPushWhereU in 'EMSManagementConsole.DlgPushWhereU.pas' {Dialog},
  EMSManagementSetUp.QueryU in 'EMSManagementSetUp.QueryU.pas',
  EMSManagementConsole.FrameJSONGridU in 'EMSManagementConsole.FrameJSONGridU.pas' {FrameJSONGrid: TFrame},
  EMSManagementConsole.Form in 'EMSManagementConsole.Form.pas' {Form2},
  EMSManagementConsole.FrameAdd in 'EMSManagementConsole.FrameAdd.pas' {AddFrame: TFrame},
  EMSManagementConsole.FramePush in 'EMSManagementConsole.FramePush.pas' {PushFrame: TFrame},
  EMSManagementConsole.FrameSettings in 'EMSManagementConsole.FrameSettings.pas' {SettingsFrame: TFrame},
  EMSManagementConsole.FrameViews in 'EMSManagementConsole.FrameViews.pas' {ViewsFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
