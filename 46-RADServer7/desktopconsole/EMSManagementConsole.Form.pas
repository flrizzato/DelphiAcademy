{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.StdCtrls, FMX.Layouts, Winapi.Windows, Winapi.ShellAPI,
  EMSManagementConsole.FrameAdd, EMSManagementConsole.FramePush,
  EMSManagementConsole.FrameViews, EMSManagementConsole.FrameSettings,
  FMX.Controls.Presentation, FMX.Grid.Style, FMX.ScrollBox;
  
type
  TForm2 = class(TForm)
    ViewsLayout: TLayout;
    ToolBar1: TToolBar;
    MainMenu: TMainMenu;
    ProfileMenu: TMenuItem;
    RenameProfile: TMenuItem;
    ExportProfile: TMenuItem;
    ImportProfile: TMenuItem;
    NewProfile: TMenuItem;
    ViewsFrame: TViewsFrame;
    RemoveProfile: TMenuItem;
    FileMenu: TMenuItem;
    SettingsFrame: TSettingsFrame;
    HelpMenu: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure NewProfileClick(Sender: TObject);
    procedure ImportProfileClick(Sender: TObject);
    procedure ExportProfileClick(Sender: TObject);
    procedure RenameProfileClick(Sender: TObject);
    procedure RemoveProfileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpMenuClick(Sender: TObject);
  private
    { Private declarations }
    FSelection: string;
    procedure SaveProfile;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  System.StrUtils, EMSManagementConsole.Consts, EMSManagementConsole.ModuleBackend;

{$R *.fmx}

procedure TForm2.ExportProfileClick(Sender: TObject);
var
  LFileName: string;
  ButtonSelected : Integer;
  LDialog: TSaveDialog;
begin
  LDialog := TSaveDialog.Create(Self);
  try
    LDialog.Filter := strProfileFile + '|*.emsp';
    if LDialog.Execute then
    begin
      LFileName := LDialog.FileName;
      if not AnsiEndsStr('.emsp', LFileName) then
        LFileName := LFileName + '.emsp';
      if FileExists(LFileName) then
      begin
        ButtonSelected := MessageDlg(strFileExist + #10#13 + strReplaceIt, TMsgDlgType.mtWarning,
          [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0);
        if ButtonSelected =  mrYes then
          SettingsFrame.WriteConfiguratonFile(LFileName)
      end
      else
        SettingsFrame.WriteConfiguratonFile(LFileName);
    end;
  finally
    LDialog.Free;
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DataModule1.Closing := True;
  SaveProfile;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  DefaultLoaded: Boolean;
begin
  FSelection := '';
  DefaultLoaded := SettingsFrame.ShowDefaultProfile;
  ToolBar1.Enabled := DefaultLoaded;
  ViewsLayout.Enabled := DefaultLoaded;
end;

procedure TForm2.HelpMenuClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar('http://docwiki.embarcadero.com/RADStudio' + strURLtoHelp), '', '', SW_SHOWNORMAL);
end;

procedure TForm2.ImportProfileClick(Sender: TObject);
var
  LDialog: TOpenDialog;
begin
  LDialog := TOpenDialog.Create(Self);
  try
    LDialog.Filter := strProfileFile + '|*.emsp';
    if LDialog.Execute then
      SettingsFrame.OpenProfiles(LDialog.FileName)
  finally
    LDialog.Free;
  end;
end;

procedure TForm2.NewProfileClick(Sender: TObject);
begin
  DataModule1.Closing := False;
  SaveProfile;
  SettingsFrame.NewConnection;
  ToolBar1.Enabled := True;
  ViewsLayout.Enabled := True;
end;

procedure TForm2.RemoveProfileClick(Sender: TObject);
begin
  if SettingsFrame.ComboProfile.ItemIndex >= 0 then
    SettingsFrame.DeleteFromRegistry(SettingsFrame.ComboProfile.Selected.Text);
  if SettingsFrame.ComboProfile.Items.Count <= 0 then
  begin
    ViewsLayout.Enabled := False;
    ToolBar1.Enabled := False;
  end;
end;

procedure TForm2.RenameProfileClick(Sender: TObject);
var
  AName: string;
begin
  if SettingsFrame.ComboProfile.ItemIndex >= 0 then
  begin
    AName := InputBox(strSaveProfileReg, strTypeProfName, '');
    if AName <> '' then
      SettingsFrame.RenameProfile(AName);
  end;
end;

procedure TForm2.SaveProfile;
begin
  if (SettingsFrame.ComboProfile.ItemIndex >= 0) and
    SettingsFrame.Registry.RegSettingsMofified then
    SettingsFrame.SaveToRegistry(SettingsFrame.ComboProfile.Selected.Text);
end;

end.
