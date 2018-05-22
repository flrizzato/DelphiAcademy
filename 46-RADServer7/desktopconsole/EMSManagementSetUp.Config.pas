{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementSetUp.Config;

interface

uses System.Classes, System.IniFiles;

type
  TEMSManagementRegistry = class
  public type
    TConnectionProperties = record
    public
      ProfileName: string;
      Host: string;
      Port: integer;
      Protocol: string;
      BaseURL: string;
      ProxyServer: string;
      ProxyPort: integer;
    end;
  public type
    TCredentialsProperties = record
    public
      ServerPassword: string;
      ServerUserName: string;
      UseMasterSecret: Boolean;
      MasterSecret: string;
      AppSecret: string;
      ApplicationID: string;
      GCMAppID: string;
      TenantID: string;
      TenantSecret: string;
    end;
  public const
    sRegKeyProfiles = '\Software\Embarcadero\EMS\Profiles';
    sEMSConnectionProperties = 'Default';
    sLastProfUsed = 'LastProfileUsed';
    sConnection = 'Connection';
    sCredentials = 'Credentials';
    sProfileName = 'ProfileName';
    sHost = 'Host';
    sPort = 'Port';
    sProtocol = 'Protocol';
    sBaseURL = 'BaseURL';
    sProxyServer = 'ProxyServer';
    sProxyPort = 'ProxyPort';
    sServerPassword = 'Password';
    sServerUserName = 'User';
    sUseMasterSecret = 'UseMasterSevret';
    sMasterSecret = 'MasterSecret';
    sAppSecret = 'AppSecret';
    sApplicationID = 'ApplicationID';
    sGCMAppID = 'GCMAppID';
    sTenantID = 'TenantID';
    sTenantSecret = 'TenantSecret';
  private
    FFileName: String;
    FRegSettingMofified: Boolean;
    FConnection: TConnectionProperties;
    FCredentials: TCredentialsProperties;
    procedure SetConnectionRegistry(const Value: TConnectionProperties);
    procedure SetCredentialsRegistry(const Value: TCredentialsProperties);
    procedure ReadValuesFromRegistry(const ConnectionSection, CredentialSection: string; Registry: TCustomIniFile);
    procedure WriteValuesToRegistry(const ConnectionSection, CredentialSection, ProfileName: string; Registry: TCustomIniFile);
  public
    property Filename: String read FFileName write FFileName;
    property RegSettingsMofified: Boolean read FRegSettingMofified write FRegSettingMofified;
    property Connection: TConnectionProperties read FConnection write SetConnectionRegistry;
    property Credentials: TCredentialsProperties read FCredentials write SetCredentialsRegistry;
    function SaveProfileToRegistry(const AProfileName: string = ''): Boolean;
    function SavedLastProfileUsed(const AProfileName: string): Boolean;
    function SaveProfileToRegistryAs(AConnection: TConnectionProperties;
      ACredentials: TCredentialsProperties; const AName: string = ''): Boolean;
    function ExportProfiles: Boolean;
    function SaveProfileFile(const AProfileName, ConnectionIndex: string): Boolean;
    function DeleteProfileFromRegistry(const AName: string): Boolean;
    function ImportProfiles: Boolean;
    function LoadProfileFile(const AIndex: string): Boolean;
    function LoadProfileNamesFromRegistry(var AProfilesList: TStringList): Boolean;
    function LoadProfileFromRegistry(const AProfileName: string = ''): Boolean;
    function GetLastProfile: string;
  end;

implementation

uses Winapi.Windows, System.Win.Registry,
     System.SysUtils, System.NetEncoding;

{ TEMSManagementRegistry }


function TEMSManagementRegistry.GetLastProfile: string;
var
  LReg: TRegistryIniFile;
begin
  Result := sEMSConnectionProperties;
  LReg := TRegistryIniFile.Create('', KEY_READ);
  try
    if LReg.RegIniFile.OpenKey(sRegKeyProfiles, False) then
    begin
      Result := LReg.ReadString('', sLastProfUsed, sEMSConnectionProperties);
    end
  finally
    LReg.Free;
  end;
end;

function TEMSManagementRegistry.LoadProfileFromRegistry(const AProfileName: string = ''): Boolean;
var
  LReg: TRegistryIniFile;
  LsEMSConnectionProperties: string;
begin
  LsEMSConnectionProperties := sEMSConnectionProperties;
  Result := False;
  if AProfileName <> '' then
    LsEMSConnectionProperties := AProfileName;
  LReg := TRegistryIniFile.Create('', KEY_READ);
  try
    if LReg.RegIniFile.OpenKey(sRegKeyProfiles, False) then
    begin
      SavedLastProfileUsed(AProfileName);
      ReadValuesFromRegistry(LsEMSConnectionProperties, LsEMSConnectionProperties, LReg);

      Result := True;
    end
  finally
    LReg.Free;
  end;
end;

function TEMSManagementRegistry.DeleteProfileFromRegistry(const AName: string): Boolean;
var
  LReg: TRegistryIniFile;
  LsEMSConnectionProperties: string;
begin
  LsEMSConnectionProperties := sEMSConnectionProperties;
  Result := False;
  LReg := TRegistryIniFile.Create('', KEY_WRITE);
  if AName <> '' then
    LsEMSConnectionProperties := AName;
  try
    if LReg.RegIniFile.OpenKey(sRegKeyProfiles, True) then
    begin
        LReg.EraseSection(AName);
        Result := True;
    end
    else
      Assert(False);
  finally
    LReg.Free;
  end;
end;

function TEMSManagementRegistry.ImportProfiles: Boolean;
var
  I, LIndex: integer;
  LIndexStr: string;
  LProfiles: TStringList;
begin
  LProfiles := TStringList.Create;
  try
    LoadProfileNamesFromRegistry(LProfiles);
    I := 0;
    while LoadProfileFile(IntToStr(I)) do
    begin
      LIndex := 0;
      LIndexStr := '';
      while LProfiles.IndexOf(FConnection.ProfileName + LIndexStr) >= 0 do
      begin
        Inc(LIndex);
        LIndexStr := '.' + IntToStr(LIndex)
      end;
      SaveProfileToRegistry(FConnection.ProfileName + LIndexStr);
      Inc(I);
    end;
    Result := True;
  finally
    LProfiles.Free
  end;
end;

function TEMSManagementRegistry.LoadProfileFile(const AIndex: string): Boolean;
var
  LReg: TIniFile;
begin
  Result := False;
  LReg := TIniFile.Create(FFileName);
  try
    ReadValuesFromRegistry(sConnection + AIndex, sCredentials + AIndex, LReg);

    if FConnection.ProfileName <> '' then
      Result := True;
  finally
    LReg.Free;
  end;
end;

function TEMSManagementRegistry.LoadProfileNamesFromRegistry(var AProfilesList: TStringList): Boolean;
var
  LReg: TRegistryIniFile;
begin
  Result := False;
  LReg := TRegistryIniFile.Create('', KEY_READ);
  try
    if LReg.RegIniFile.OpenKey(sRegKeyProfiles, False) then
    begin
        LReg.ReadSections(AProfilesList);
        Result := True;
    end;
  finally
    LReg.Free;
  end;
end;

function TEMSManagementRegistry.SaveProfileToRegistryAs(AConnection: TConnectionProperties;
  ACredentials: TCredentialsProperties; const AName: string = ''): Boolean;
begin
  SetConnectionRegistry(AConnection);
  SetCredentialsRegistry(ACredentials);
  Result := SaveProfileToRegistry(AName);
end;

function TEMSManagementRegistry.SavedLastProfileUsed(const AProfileName: string): Boolean;
var
  LReg: TRegistryIniFile;
  LsEMSConnectionProperties: string;
begin
  LsEMSConnectionProperties := sEMSConnectionProperties;
  Result := False;
  LReg := TRegistryIniFile.Create('', KEY_WRITE);
  if AProfileName <> '' then
    LsEMSConnectionProperties := AProfileName;
  try
    if LReg.RegIniFile.OpenKey(sRegKeyProfiles, True) then
    begin
      LReg.WriteString('', sLastProfUsed, AProfileName);
      Result := True;
    end
    else
      Assert(False);
  finally
    LReg.Free;
  end;
end;

function TEMSManagementRegistry.SaveProfileToRegistry(const AProfileName: string = ''): Boolean;
var
  LReg: TRegistryIniFile;
  LsEMSConnectionProperties: string;
begin
  LsEMSConnectionProperties := sEMSConnectionProperties;
  Result := False;
  LReg := TRegistryIniFile.Create('', KEY_WRITE);
  if AProfileName <> '' then
    LsEMSConnectionProperties := AProfileName;
  try
    if LReg.RegIniFile.OpenKey(sRegKeyProfiles, True) then
    begin
      LReg.WriteString('', sLastProfUsed, AProfileName);
      WriteValuesToRegistry(LsEMSConnectionProperties, LsEMSConnectionProperties, AProfileName, LReg);

      Result := True;
    end
    else
      Assert(False);
  finally
    LReg.Free;
  end;
end;

function TEMSManagementRegistry.ExportProfiles: Boolean;
var
  I: integer;
  LProfiles: TStringList;
begin
  LProfiles := TStringList.Create;
  try
    LoadProfileNamesFromRegistry(LProfiles);
    for I := 0 to LProfiles.Count - 1 do
    begin
      LoadProfileFromRegistry(LProfiles[I]);
      SaveProfileFile(LProfiles[I], IntToStr(I));
    end;
    Result := True;
  finally
    LProfiles.Free;
  end;
end;

function TEMSManagementRegistry.SaveProfileFile(const AProfileName, ConnectionIndex: string): Boolean;
var
  LReg: TIniFile;
begin
  LReg := TIniFile.Create(FFileName);
  try
    LReg.WriteString(sConnection + ConnectionIndex, sProfileName, AProfileName);
    WriteValuesToRegistry(sConnection + ConnectionIndex, sCredentials + ConnectionIndex, AProfileName, LReg);

    Result := True;
  finally
    LReg.Free;
  end;
end;

procedure TEMSManagementRegistry.SetConnectionRegistry(
  const Value: TConnectionProperties);
begin
  FConnection := Value;
end;

procedure TEMSManagementRegistry.SetCredentialsRegistry(
  const Value: TCredentialsProperties);
begin
  FCredentials := Value;
end;

procedure TEMSManagementRegistry.ReadValuesFromRegistry(const ConnectionSection, CredentialSection: string;
  Registry: TCustomIniFile);
begin
  FConnection.ProfileName := Registry.ReadString(ConnectionSection, sProfileName, '');
  FConnection.Host := Registry.ReadString(ConnectionSection, sHost, '');
  FConnection.Port := Registry.ReadInteger(ConnectionSection, sPort, 0);
  FConnection.Protocol := Registry.ReadString(ConnectionSection, sProtocol, '');
  FConnection.BaseURL := Registry.ReadString(ConnectionSection, sBaseURL, '');
  FConnection.ProxyServer := Registry.ReadString(ConnectionSection, sProxyServer, '');
  FConnection.ProxyPort := Registry.ReadInteger(ConnectionSection, sProxyPort, 0);

  FCredentials.ServerPassword := TNetEncoding.Base64.Decode(Registry.ReadString(CredentialSection, sServerPassword, ''));
  FCredentials.ServerUserName := Registry.ReadString(CredentialSection, sServerUserName, '');
  FCredentials.UseMasterSecret := Registry.ReadBool(CredentialSection, sUseMasterSecret, False);
  FCredentials.MasterSecret := Registry.ReadString(CredentialSection, sMasterSecret, '');
  FCredentials.AppSecret := Registry.ReadString(CredentialSection, sAppSecret, '');
  FCredentials.ApplicationID := Registry.ReadString(CredentialSection, sApplicationID, '');
  FCredentials.GCMAppID := Registry.ReadString(CredentialSection, sGCMAppID, '');
  FCredentials.TenantID := Registry.ReadString(CredentialSection, sTenantID, '');
  FCredentials.TenantSecret := Registry.ReadString(CredentialSection, sTenantSecret, '');
end;

procedure TEMSManagementRegistry.WriteValuesToRegistry(const ConnectionSection,
  CredentialSection, ProfileName: string; Registry: TCustomIniFile);
begin
  Registry.WriteString(ConnectionSection, sProfileName, ProfileName);
  Registry.WriteString(ConnectionSection, sHost, FConnection.Host);
  Registry.WriteInteger(ConnectionSection, sPort, FConnection.Port);
  Registry.WriteString(ConnectionSection, sProtocol, FConnection.Protocol);
  Registry.WriteString(ConnectionSection, sBaseURL, FConnection.BaseURL);
  Registry.WriteString(ConnectionSection, sProxyServer, FConnection.ProxyServer);
  Registry.WriteInteger(ConnectionSection, sProxyPort, FConnection.ProxyPort);

  Registry.WriteString(CredentialSection, sServerPassword, TNetEncoding.Base64.Encode(FCredentials.ServerPassword));
  Registry.WriteString(CredentialSection, sServerUserName, FCredentials.ServerUserName);
  Registry.WriteBool(CredentialSection, sUseMasterSecret, FCredentials.UseMasterSecret);
  Registry.WriteString(CredentialSection, sMasterSecret, FCredentials.MasterSecret);
  Registry.WriteString(CredentialSection, sAppSecret, FCredentials.AppSecret);
  Registry.WriteString(CredentialSection, sApplicationID, FCredentials.ApplicationID);
  Registry.WriteString(CredentialSection, sGCMAppID, FCredentials.GCMAppID);
  Registry.WriteString(CredentialSection, sTenantID, FCredentials.TenantID);
  Registry.WriteString(CredentialSection, sTenantSecret, FCredentials.TenantSecret);
end;

end.
