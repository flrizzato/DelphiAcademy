{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.Types;

interface

uses System.Generics.Collections, System.Classes, REST.Client, System.JSON;

type
  TDetail = TPair<string, string>;

  TDetailInt = TPair<string, integer>;

  TInstallDetail = record
    ID: string;
    Token: string;
    DeviceType: string;
  end;

  TUserInfo = record
    UserID: string;
    UserName: string;
    Creator : string;
    Created : string;
    Details: TArray<TDetail>;
    Groups: TStringList;
  public const
    StaticFileds = 4;
    cUserName = 'username';
    cID = '_id';
    cCreator = 'creator';
    cCreated = 'created';
    cUserFieldName = 'username';
    cUserFieldPass = 'password';
    cResource = 'Users';
  end;

  TGroupInfo = record
    GroupName : string;
    Creator : string;
    Created  : string;
    Details: TArray<TDetail>;
    Users: TStringList;
  public const
    StaticFileds = 3;
    cGroupName = 'groupname';
    cCreator = 'creator';
    cCreated = 'created';

    cResource = 'Groups';
    cUsers = 'users';
  end;

  TInstallationInfo = record
    InstallationID: string;
    DeviceToken: string;
    DeviceType: string;
    Creator : string;
    Created : string;
    Details: TArray<TDetail>;
    Channels: TArray<string>;
  public const
    StaticFileds = 4;
    cID = '_id';
    cDeviceToken = 'deviceToken';
    cDeviceType = 'deviceType';
    cMeta = '_meta';
    cChannels = 'channels';
    cResource = 'Installations';
  end;

  TEdgeModuleInfo = record
    ModuleID: string;
    ModuleName: string;
    Protocol : string;
    ProtocolProps : string;
    Creator : string;
    Created : string;
    Details: TArray<TDetail>;
  public const
    StaticFileds = 4;
    cID = '_id';
    cModuleName = 'modulename';
    cModuleProtocol = 'protocol';
    cModuleProtocolProps = 'protocolprops';
    cCreator = 'creator';
    cCreated = 'created';
  end;

  TResourcesInfo = record
    ResourceID: string;
    ResourceName: string;
    ModuleName: string;
    ModuleID: string;
    EdgePointsID: string;
    Creator : string;
    Created : string;
    Details: TArray<TDetail>;
  public const
    StaticFileds = 1;
    cID = '_id';
    cModuleName = 'modulename';
    cModuleID = 'moduleid';
    cResourceName = 'resourcename';
    cCreator = 'creator';
    cCreated = 'created';
  end;

  TChannelsInfo = record
    ChannelName: string;
    Installations: TList<TInstallDetail>;
  end;

  TUsersInfoArray = TArray<TUserInfo>;

  TGroupsInfoArray = TArray<TGroupInfo>;

  TInstallationInfoArray = TArray<TInstallationInfo>;

  TChannelsInfoArray = TArray<TChannelsInfo>;

  TEdgeModulesInfoArray = TArray<TEdgeModuleInfo>;

  TResourcesInfoArray = TArray<TResourcesInfo>;

  TCell = record
    col: integer;
    row: integer;
  end;

  TManagerEndPoints = class
  public const
    cUsersEndPoint = 'Users';
    cGroupsEndPoint = 'Groups';
    cInstallationsEndPoint = 'Installations';
    cEdgeModulessEndPoint= 'Modules';
    cEdgeModulesResourcessEndPoint = 'Resources';
    cPush = 'Push';
    cSessionID = 'sessionid';
  end;

  TAdapterJSONValue =  class(TInterfacedObject, IRESTResponseJSON)
  private
    FJSONValue: TJSONValue;
  protected
    { IRESTResponseJSON }
    procedure AddJSONChangedEvent(const ANotify: TNotifyEvent);
    procedure RemoveJSONChangedEvent(const ANotify: TNotifyEvent);
    procedure GetJSONResponse(out AJSONValue: TJSONValue; out AHasOwner: Boolean);
    function HasJSONResponse: Boolean;
    function HasResponseContent: Boolean;
  public
    constructor Create(const AJSONValue: TJSONValue);
    destructor Destroy; override;
  end;

implementation

{ TAdapterJSONValue }

procedure TAdapterJSONValue.AddJSONChangedEvent(const ANotify: TNotifyEvent);
begin
  // Not implemented because we pass JSON in constructor and do not change it
end;

constructor TAdapterJSONValue.Create(const AJSONValue: TJSONValue);
begin
  FJSONValue := AJSONValue;
end;

destructor TAdapterJSONValue.Destroy;
begin
  // We own the JSONValue, so free it.
  FJSONValue.Free;
  inherited;
end;

procedure TAdapterJSONValue.GetJSONResponse(out AJSONValue: TJSONValue;
  out AHasOwner: Boolean);
begin
  AJSONValue := FJSONValue;
  AHasOwner := True; // We own this object
end;

function TAdapterJSONValue.HasJSONResponse: Boolean;
begin
  Result := FJSONValue <> nil;
end;

function TAdapterJSONValue.HasResponseContent: Boolean;
begin
  Result := FJSONValue <> nil;
end;

procedure TAdapterJSONValue.RemoveJSONChangedEvent(const ANotify: TNotifyEvent);
begin
  // Not implemented because we pass JSON in constructor and do not change it
end;

end.
