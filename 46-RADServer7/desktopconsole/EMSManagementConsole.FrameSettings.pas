{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.FrameSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox,
  EMSManagementConsole.Data,
  EMSManagementSetUp.Config, FMX.Layouts;

type
  TSettingsFrame = class(TFrame)
    ConnectionInfoControl: TTabControl;
    ConnectionTabItem: TTabItem;
    KeysTabItem: TTabItem;
    ComboProfile: TComboBox;
    ProxyTabItem: TTabItem;
    AuthenticateTabItem: TTabItem;
    Label1: TLabel;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    HostLabel: TLabel;
    PortLabel: TLabel;
    URLLabel: TLabel;
    Layout4: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    Layout7: TLayout;
    Layout8: TLayout;
    Layout9: TLayout;
    CheckBoxHTTPS: TCheckBox;
    HostEdit: TEdit;
    PortEdit: TEdit;
    TestConnectionButton: TButton;
    URLEdit: TEdit;
    ProxyPortLabel: TLabel;
    ProxyServerLabel: TLabel;
    ProxyPortEdit: TEdit;
    ProxyServerEdit: TEdit;
    ApplicationIDLabel: TLabel;
    AppSecretLabel: TLabel;
    AppIDEdit: TEdit;
    AppSecretEdit: TEdit;
    MasterSecretLabel: TLabel;
    PassLabel: TLabel;
    UserLabel: TLabel;
    LoginButton: TButton;
    LogoutButton: TButton;
    MasterSecretEdit: TEdit;
    MSecretCheckBox: TCheckBox;
    PassEdit: TEdit;
    UserEdit: TEdit;
    TabItem1: TTabItem;
    Layout10: TLayout;
    TenantSecretEdit: TEdit;
    TenantIDEdit: TEdit;
    Layout11: TLayout;
    TenantSecretLabel: TLabel;
    TenantIDLabel: TLabel;
    procedure EditChange(Sender: TObject);
    procedure MSecretCheckBoxChange(Sender: TObject);
    procedure ComboProfileChange(Sender: TObject);
    procedure TestConnectionButtonClick(Sender: TObject);
    procedure UserEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure LoginButtonClick(Sender: TObject);
    procedure LogoutButtonClick(Sender: TObject);
    procedure HostEditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
    FRegistry: TEMSManagementRegistry;
    procedure LoadProfileNamesToCombo(const AName: string);
    procedure SetConnectionInfo;
    procedure SetCredentialsInfo;
    function LoadConfiguratonFile(const AFileName: string): Boolean;
    procedure SetFieldsToEdits;
    function GetConnectionProperties: TEMSManagementRegistry.TConnectionProperties;
    function GetCredentialsProperties: TEMSManagementRegistry.TCredentialsProperties;
    function GetProtocol: String;
    procedure ResetConnection;
    procedure ResetCredentials;
    function LoadDefaultProfileFromRegistry: Boolean;
    function GetNewProfileName: string;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NewConnection;
    function ShowDefaultProfile: Boolean;
    procedure SaveToRegistry(const AName: string);
    procedure DeleteFromRegistry(const AName: string);
    procedure OpenProfiles(AFileName: TFileName);
    procedure RenameProfile(const AName: string);
    function WriteConfiguratonFile(const AFileName: string): Boolean;
    procedure SetRequestInfo;
    property Registry: TEMSManagementRegistry read FRegistry write FRegistry;
  end;

implementation

uses EMSManagementConsole.Consts, REST.Backend.Providers,
  EMSManagementConsole.ModuleBackend;

{$R *.fmx}

{ TSettingsFrame }

constructor TSettingsFrame.Create(AOwner: TComponent);
begin
  inherited;
  Registry := TEMSManagementRegistry.Create;
  Registry.RegSettingsMofified := False;
  SetRequestInfo;
end;

destructor TSettingsFrame.Destroy;
begin
  Registry.Free;
  inherited;
end;

procedure TSettingsFrame.SetFieldsToEdits;
begin
  HostEdit.Text := Registry.Connection.Host;
  PortEdit.Text := IntToStr(Registry.Connection.Port);
  CheckBoxHTTPS.IsChecked := (CheckBoxHTTPS.Text = Registry.Connection.Protocol);
  URLEdit.Text := Registry.Connection.BaseURL;
  ProxyServerEdit.Text := Registry.Connection.ProxyServer;
  ProxyPortEdit.Text := IntToStr(Registry.Connection.ProxyPort);

  UserEdit.Text := Registry.Credentials.ServerUserName;
  PassEdit.Text := Registry.Credentials.ServerPassword;

  MasterSecretEdit.Text := Registry.Credentials.MasterSecret;
  AppSecretEdit.Text := Registry.Credentials.AppSecret;
  AppIDEdit.Text := Registry.Credentials.ApplicationID;

  TenantIDEdit.Text := Registry.Credentials.TenantID;
  TenantSecretEdit.Text := Registry.Credentials.TenantSecret;

  MSecretCheckBox.IsChecked := Registry.Credentials.UseMasterSecret;
  if MSecretCheckBox.IsChecked then
    MSecretCheckBox.Enabled := True
  else
    MasterSecretEdit.Enabled := False;

  LoginButton.Enabled := not MSecretCheckBox.ISChecked  and (DataModule1.SessionID = '') and (UserEdit.Text <> '');
  LogoutButton.Enabled := not MSecretCheckBox.ISChecked and (DataModule1.SessionID <> '');
end;

function TSettingsFrame.GetConnectionProperties:TEMSManagementRegistry.TConnectionProperties;
begin
  Result.Host := HostEdit.Text;
  try
    Result.Port := StrToInt(PortEdit.Text);
  except
    Result.Port := 0;
    PortEdit.Text := '0';
    raise;
  end;
  Result.Protocol := GetProtocol;
  Result.BaseURL := URLEdit.Text;
  Result.ProxyServer := ProxyServerEdit.Text;
  try
    Result.ProxyPort := StrToInt(ProxyPortEdit.Text);
  except
    Result.ProxyPort := 0;
    ProxyPortEdit.Text := '0';
    raise;
  end;
end;

function TSettingsFrame.GetCredentialsProperties: TEMSManagementRegistry.TCredentialsProperties;
begin
  Result.ServerUserName := UserEdit.Text;
  Result.ServerPassword := PassEdit.Text;
  Result.UseMasterSecret := MSecretCheckBox.IsChecked;
  Result.MasterSecret := MasterSecretEdit.Text;
  Result.AppSecret := AppSecretEdit.Text;
  Result.ApplicationID := AppIDEdit.Text;
  Result.TenantID := TenantIDEdit.Text;
  Result.TenantSecret := TenantSecretEdit.Text;
end;

function TSettingsFrame.GetNewProfileName: string;
var
  i: Integer;
begin
  I := 1;
  while ComboProfile.Items.IndexOf(strNewProfile + IntToStr(I)) >= 0 do
    Inc(I);
  Result := strNewProfile + IntToStr(I);
end;

function TSettingsFrame.GetProtocol: String;
begin
  if CheckBoxHTTPS.IsChecked then
    Result := 'HTTPS'
  else
    Result := 'HTTP'
end;

procedure TSettingsFrame.HostEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  Registry.RegSettingsMofified := True;
  SetRequestInfo;
end;

procedure TSettingsFrame.UserEditKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  Registry.RegSettingsMofified := True;
  if UserEdit.Text = '' then
    LoginButton.Enabled := False
  else
    LoginButton.Enabled := True;
  SetRequestInfo;
end;

procedure TSettingsFrame.LoadProfileNamesToCombo(const AName: string);
var
  I: integer;
  LProfiles: TStringList;
begin
  ComboProfile.Clear;
  LProfiles := TStringList.Create;
  try
    Registry.LoadProfileNamesFromRegistry(LProfiles);
    for I := 0 to LProfiles.Count - 1 do
      ComboProfile.Items.Add(LProfiles.Strings[I]);
    if AName <> '' then
      ComboProfile.ItemIndex := ComboProfile.Items.IndexOf(AName);
  finally
    LProfiles.Free;
  end;
end;

procedure TSettingsFrame.RenameProfile(const AName: string);
var
  OldName: string;
begin
  if AName <> '' then
  begin
    OldName := ComboProfile.Selected.Text;
    SaveToRegistry(AName);
    DeleteFromRegistry(OldName);
    LoadProfileNamesToCombo(AName);
  end
end;

procedure TSettingsFrame.ResetConnection;
begin
  HostEdit.Text := '';
  PortEdit.Text := '0';
  CheckBoxHTTPS.IsChecked := False;
  URLEdit.Text := '';
  ProxyServerEdit.Text := '';
  ProxyPortEdit.Text := '0';

  SetConnectionInfo;
  Registry.RegSettingsMofified := False;
end;

procedure TSettingsFrame.ResetCredentials;
begin
  UserEdit.Text := '';
  PassEdit.Text := '';
  MSecretCheckBox.IsChecked := False;
  MasterSecretEdit.Text := '';
  AppSecretEdit.Text := '';
  AppIDEdit.Text := '';

  TenantIDEdit.Text := '';
  TenantSecretEdit.Text := '';

  SetCredentialsInfo;
  Registry.RegSettingsMofified := False;
  LoginButton.Enabled := False;
  LogoutButton.Enabled := False;
end;

procedure TSettingsFrame.SaveToRegistry(const AName: string);
begin
  Registry.SaveProfileToRegistryAs(GetConnectionProperties, GetCredentialsProperties, AName);
  Registry.RegSettingsMofified := False;
end;

procedure TSettingsFrame.DeleteFromRegistry(const AName: string);
begin
  ResetConnection;
  ResetCredentials;
  Registry.DeleteProfileFromRegistry(AName);
  LoadProfileNamesToCombo('');
end;

procedure TSettingsFrame.SetConnectionInfo;
begin
  Registry.Connection := GetConnectionProperties;
  DataModule1.SetProviderConnectionInfo(Registry);
end;

procedure TSettingsFrame.SetCredentialsInfo;
begin
  Registry.Credentials := GetCredentialsProperties;
  DataModule1.SetProviderCredentials(Registry);
end;

function TSettingsFrame.ShowDefaultProfile: Boolean;
begin
  LoadProfileNamesToCombo('');
  Result := LoadDefaultProfileFromRegistry;
end;

procedure TSettingsFrame.ComboProfileChange(Sender: TObject);
begin
  if DataModule1.SessionID <> '' then
    LogoutButtonClick(Self);
  Registry.LoadProfileFromRegistry(ComboProfile.Items[ComboProfile.ItemIndex]);
  SetFieldsToEdits;
  Registry.RegSettingsMofified := False;
  SetRequestInfo;
end;

function TSettingsFrame.LoadDefaultProfileFromRegistry: Boolean;
begin
  Result := False;
  if ComboProfile.Count > 0 then
  begin
    ComboProfile.ItemIndex := ComboProfile.Items.IndexOf(Registry.GetLastProfile);
    Registry.RegSettingsMofified := False;
    Result := True;
  end;
end;

procedure TSettingsFrame.MSecretCheckBoxChange(Sender: TObject);
begin
  MasterSecretEdit.Enabled := MSecretCheckBox.IsChecked;
  UserEdit.Enabled := not MSecretCheckBox.IsChecked;
  PassEdit.Enabled := not MSecretCheckBox.ISChecked;
  LoginButton.Enabled := not MSecretCheckBox.ISChecked  and (DataModule1.SessionID = '');
  LogoutButton.Enabled := not MSecretCheckBox.ISChecked and (DataModule1.SessionID <> '');
  Registry.RegSettingsMofified := True;
  SetRequestInfo;
end;

procedure TSettingsFrame.EditChange(Sender: TObject);
begin
  Registry.RegSettingsMofified := True;
  SetRequestInfo;
end;

procedure TSettingsFrame.TestConnectionButtonClick(Sender: TObject);
begin
  SetRequestInfo;
  DataModule1.AppHandshake;
end;

procedure TSettingsFrame.NewConnection;
var
  ANewName: string;
begin
  if DataModule1.SessionID <> '' then
    LogoutButtonClick(Self);
  ANewName := GetNewProfileName;
  ComboProfile.Items.Add(ANewName);
  ComboProfile.ItemIndex := ComboProfile.Items.IndexOf(ANewName);
  ComboProfile.Enabled := True;
  ResetConnection;
  ResetCredentials;
end;

procedure TSettingsFrame.OpenProfiles(AFileName: TFileName);
begin
  if DataModule1.SessionID <> '' then
    LogoutButtonClick(Self);
  LoadConfiguratonFile(AFileName);
end;

function TSettingsFrame.LoadConfiguratonFile(const AFileName: string): Boolean;
var
  LLastProfile: string;
begin
  LLastProfile := Registry.GetLastProfile;
  Registry.Filename := AFileName;
  Registry.RegSettingsMofified := Registry.ImportProfiles;
  LoadProfileNamesToCombo(LLastProfile);
  SetFieldsToEdits;
  DataModule1.SetProviderConnectionInfo(Registry);
  DataModule1.SetProviderCredentials(Registry);

  Result := Registry.RegSettingsMofified;
end;

function TSettingsFrame.WriteConfiguratonFile(const AFileName: string): Boolean;
begin
if Registry.RegSettingsMofified then
  begin
    Registry.Connection := GetConnectionProperties;
    Registry.Credentials := GetCredentialsProperties;
  end;
  Registry.Filename := AFileName;
  Result := Registry.ExportProfiles;
end;

procedure TSettingsFrame.SetRequestInfo;
begin
  SetConnectionInfo;
  SetCredentialsInfo;
end;

procedure TSettingsFrame.LoginButtonClick(Sender: TObject);
begin
  SetRequestInfo;
  if DataModule1.Login(not MSecretCheckBox.IsChecked) then
  begin
    LoginButton.Enabled := False;
    UserEdit.Enabled := False;
    PassEdit.Enabled := False;
    MasterSecretEdit.Enabled := False;
    AppSecretEdit.Enabled := False;
    AppIDEdit.Enabled := False;
    MSecretCheckBox.Enabled := False;
    LogoutButton.Enabled := True;
    ProxyServerEdit.Enabled := False;
    ProxyPortEdit.Enabled := False;
    HostEdit.Enabled := False;
    PortEdit.Enabled := False;
    URLEdit.Enabled := False;
    CheckBoxHTTPS.Enabled := False;
    TenantIDEdit.Enabled := False;
    TenantSecretEdit.Enabled := False;
  end;
end;

procedure TSettingsFrame.LogoutButtonClick(Sender: TObject);
begin
  if DataModule1.Logout then
  begin
    if MSecretCheckBox.IsChecked then
    begin
      UserEdit.Enabled := False;
      PassEdit.Enabled := False;
      MasterSecretEdit.Enabled := True;
    end
    else
    begin
      UserEdit.Enabled := True;
      PassEdit.Enabled := True;
      MasterSecretEdit.Enabled := False;
    end;
    LoginButton.Enabled := True;
    MSecretCheckBox.Enabled := True;
    AppSecretEdit.Enabled := True;
    AppIDEdit.Enabled := True;
    LogoutButton.Enabled := False;
    ConnectionTabItem.Enabled := True;
    ProxyServerEdit.Enabled := True;
    ProxyPortEdit.Enabled := True;
    HostEdit.Enabled := True;
    PortEdit.Enabled := True;
    URLEdit.Enabled := True;
    CheckBoxHTTPS.Enabled := True;
    TenantIDEdit.Enabled := True;
    TenantSecretEdit.Enabled := True;
  end;
end;

end.
