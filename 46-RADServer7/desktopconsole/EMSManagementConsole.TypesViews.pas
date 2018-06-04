{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.TypesViews;

interface

uses  System.Classes, System.SysUtils, System.JSON,
  FMX.TabControl, FMX.Grid, System.Generics.Collections,
  EMSManagementConsole.FrameAdd, FMX.ListBox,
  EMSManagementConsole.Data, EMSManagementConsole.Types,
  EMSManagementConsole.DlgModifyU, EMSManagementConsole.Consts,
  Data.Bind.Grid, FireDAC.Comp.Client, REST.Response.Adapter,
  Data.Bind.DBScope, Data.Bind.Components, EMSManagementConsole.FrameJSONGridU;

type
  TEMSTabItem = class(TTabItem)
  public type
    TGetJSONEvent = procedure(Sender: TObject; const AJSON: TJSONArray) of object;
    TGetJSONArrayEvent = procedure(Sender: TObject; const AJSON: TJSONArray) of object;
  private
    FFrameJSONGrid: TFrameJSONGrid;
    FColumnNames: TList<string>;
    FColumnsReadOnly: TList<integer>;
    FEMSConsoleData: TEMSConsoleData;
    FLoadedData: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateView(AIndetifier: string; ACellList: TList<TCell>); virtual; abstract;
    procedure ShowFrame(ADialog: TFormAddDlg); virtual; abstract;
    procedure SetObjectInfoToAddDlg(ADialog: TFormAddDlg); virtual; abstract;
    function GetObjectIdentifier(const Row: Integer):string; virtual; abstract;
    function Delete(const AID: string): Boolean; virtual; abstract;
    procedure Load(const AID: string);
    function GetDefaultStyleLookupName: string; override;
    property EMSConsoleData: TEMSConsoleData read FEMSConsoleData write FEMSConsoleData;
    property ColumnsReadOnly: TList<integer> read FColumnsReadOnly write FColumnsReadOnly;
    property FrameJSONGrid: TFrameJSONGrid read FFrameJSONGrid write FFrameJSONGrid;
    procedure OnTabItemClick(Sender: TObject);
    function GetCombo: TComboBox;
    function GetHeaderIndex(AHeaderName: string): integer;
  end;

  TUsersTabItem = class(TEMSTabItem)
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateView(AIndetifier: string; ACellList: TList<TCell>); override;
    procedure ShowFrame(ADialog: TFormAddDlg); override;
    procedure SetObjectInfoToAddDlg(ADialog: TFormAddDlg); override;
    function GetObjectIdentifier(const Row: Integer):string; override;
    function Delete(const AID: string): Boolean; override;
    procedure OnGetUsersData(Sender: TObject; const AJSON: TJSONArray);
    procedure OnGetUsersFields(Sender: TObject; const AJSON: TJSONArray);
  end;

  TGroupsTabItem = class(TEMSTabItem)
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateView(AIndetifier: string; ACellList: TList<TCell>); override;
     procedure ShowFrame(ADialog: TFormAddDlg); override;
    procedure SetObjectInfoToAddDlg(ADialog: TFormAddDlg); override;
    function GetObjectIdentifier(const Row: Integer):string; override;
    function Delete(const AID: string): Boolean; override;
    procedure OnGetGroupsSData(Sender: TObject; const AJSON: TJSONArray);
    procedure OnGetGroupsFields(Sender: TObject; const AJSON: TJSONArray);
  end;

  TInstallationTabItem = class(TEMSTabItem)
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateView(AIndetifier: string; ACellList: TList<TCell>); override;
     procedure ShowFrame(ADialog: TFormAddDlg); override;
    procedure SetObjectInfoToAddDlg(ADialog: TFormAddDlg); override;
    function GetObjectIdentifier(const Row: Integer):string; override;
    function Delete(const AID: string): Boolean; override;
    procedure OnGetInstallationsData(Sender: TObject; const AJSON: TJSONArray);
    procedure OnGetInstallationsFields(Sender: TObject; const AJSON: TJSONArray);
  end;

   TEdgeModuleTabItem = class(TEMSTabItem)
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateView(AIndetifier: string; ACellList: TList<TCell>); override;
     procedure ShowFrame(ADialog: TFormAddDlg); override;
    procedure SetObjectInfoToAddDlg(ADialog: TFormAddDlg); override;
    function GetObjectIdentifier(const Row: Integer):string; override;
    function Delete(const AID: string): Boolean; override;
    procedure OnGetEdgeModulesData(Sender: TObject; const AJSON: TJSONArray);
    procedure OnGetEdgeModulesFields(Sender: TObject; const AJSON: TJSONArray);
  end;

   TResourceTabItem = class(TEMSTabItem)
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateView(AIndetifier: string; ACellList: TList<TCell>); override;
     procedure ShowFrame(ADialog: TFormAddDlg); override;
    procedure SetObjectInfoToAddDlg(ADialog: TFormAddDlg); override;
    function GetObjectIdentifier(const Row: Integer):string; override;
    function Delete(const AID: string): Boolean; override;
    procedure OnGetResourcesData(Sender: TObject; const AJSON: TJSONArray);
    procedure OnGetResourcesFields(Sender: TObject; const AJSON: TJSONArray);
    function GetSelectedModule: string;
    procedure LoadComboModules;
  end;

implementation

uses EMSManagementConsole.FrameViews, System.UITypes, FMX.Types,
  EMSManagementConsole.ModuleBackend, REST.Backend.EMSApi, REST.Backend.EMSProvider,
  FireDAC.Stan.Option, Data.DB, EMSManagementConsole.Form, FMX.Dialogs;

{ TEMSTabItem }

function TEMSTabItem.GetCombo: TComboBox;
begin
  Result := FFrameJSONGrid.ComboBoxExtras;
end;

function TEMSTabItem.GetDefaultStyleLookupName: string;
begin
  Result := 'TabItemstyle';
end;

function TEMSTabItem.GetHeaderIndex(AHeaderName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to  FFrameJSONGrid.StringGrid.ColumnCount - 1 do
    if  AHeaderName = FFrameJSONGrid.StringGrid.Columns[I].Header then
      Result := I;
end;

function TResourceTabItem.GetSelectedModule: string;
begin;
  FFrameJSONGrid.LayoutExtras.Visible := True;
  FFrameJSONGrid.LabelExtras.Text := strEdgeModules;
  if (GetCombo.Selected <> nil) and (GetCombo.Selected.Text <> strAllEdgeModule) then
    Result := GetCombo.ListItems[GetCombo.ItemIndex].ItemData.Detail
  else
    Result := '';
end;

procedure TEMSTabItem.Load(const AID: string);
begin
  TForm2(root).ViewsFrame.EnableTabs;
  FrameJSONGrid.Refresh;
end;

procedure TEMSTabItem.OnTabItemClick(Sender: TObject);
begin
  if not FLoadedData then
  begin
    FFrameJSONGrid.Refresh;
    FLoadedData := True;
  end;
end;

destructor TEMSTabItem.Destroy;
begin
  FFrameJSONGrid.Free;
  FColumnNames.Free;
  FColumnsReadOnly.Free;
  inherited;
end;

constructor TEMSTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FFrameJSONGrid := TFrameJSONGrid.Create(Self);
  FFrameJSONGrid.Parent := Self;
  FFrameJSONGrid.Align := TAlignLayout.Client;
  FFrameJSONGrid.StringGrid.Options := FFrameJSONGrid.StringGrid.Options + [TGridOption.RowSelect, TGridOption.AlwaysShowSelection] - [TGridOption.Editing];
  FColumnNames := TList<string>.Create;
  FColumnsReadOnly := TList<integer>.Create;
  OnClick := OnTabItemClick;
end;

{ TUsersTabItem }

procedure TUsersTabItem.OnGetUsersData(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetUsers(Sender, AJSON);
end;

procedure TUsersTabItem.OnGetUsersFields(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetUsersFields(Sender, AJSON);
end;

procedure TUsersTabItem.SetObjectInfoToAddDlg(ADialog: TFormAddDlg);
var
  I, LColIndex: integer;
  LDetailsKeys: TArray<string>;
  LHeaderList: TList<string>;
  LUserInfo: TUserInfo;
begin
  LUserInfo.UserName := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TUserInfo.cUserName), FFrameJSONGrid.StringGrid.Selected];
  LUserInfo.UserId := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TUserInfo.cID), FFrameJSONGrid.StringGrid.Selected];

  LDetailsKeys := EMSConsoleData.GetUsersFieldsArray;

  SetLength(LUserInfo.Details, Length(LDetailsKeys));
  LHeaderList := TList<string>.Create;
  try
    for I := 0 to FFrameJSONGrid.StringGrid.ColumnCount - 1 do
    begin
      LHeaderList.Add(FFrameJSONGrid.StringGrid.Columns[I].Header);
    end;
    for I := Low(LDetailsKeys) to High(LDetailsKeys) do
    begin
      LUserInfo.Details[I].Key := LDetailsKeys[I];
      LColIndex := LHeaderList.IndexOf(LDetailsKeys[I]);
      LUserInfo.Details[I].Value := FFrameJSONGrid.StringGrid.Cells[LColIndex, FFrameJSONGrid.StringGrid.Selected];
    end;
    ADialog.UserInfo := LUserInfo;
  finally
    LHeaderList.Free;
  end;
end;

procedure TUsersTabItem.ShowFrame(ADialog: TFormAddDlg);
begin
  ADialog.AddFrame.ShowUserAddTab;
end;

procedure TUsersTabItem.UpdateView(AIndetifier: string;
  ACellList: TList<TCell>);
var
  LJSON: TJSONObject;
  I, Col, Row: Integer;
begin
  LJSON := TJSONObject.Create;
   try
    for I := 0 to ACellList.Count - 1 do
    begin
      Col := ACellList.Items[I].col;
      Row := ACellList.Items[I].row;
      LJSON.AddPair(FFrameJSONGrid.StringGrid.Columns[Col].Header, FFrameJSONGrid.StringGrid.Cells[Col, Row]);
    end;
    FEMSConsoleData.UpdateUser(AIndetifier, LJSON);
   finally
    LJSON.Free;
   end;
end;

constructor TUsersTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FrameJSONGrid.OnGetData := OnGetUsersData;
  FrameJSONGrid.OnGetFields := OnGetUsersFields;
  TabOrder := 0;
end;

function TUsersTabItem.Delete(const AID: string): Boolean;
begin
  Result := FEMSConsoleData.DeleteUser(AID);
end;

function TUsersTabItem.GetObjectIdentifier(const Row: Integer): string;
var
  ColIndex: Integer;
begin
  ColIndex := GetHeaderIndex(TUserInfo.cID);
  if ColIndex >= 0 then
    Result := FFrameJSONGrid.StringGrid.Cells[ColIndex, Row];
end;

{ TGroupsTabItem }

procedure TGroupsTabItem.OnGetGroupsFields(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetGroupsFields(Sender, AJSON);
end;

procedure TGroupsTabItem.OnGetGroupsSData(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetGroups(Sender, AJSON);
end;

procedure TGroupsTabItem.SetObjectInfoToAddDlg(ADialog: TFormAddDlg);
var
  I, LColIndex: integer;
  LDetailsKeys: TArray<string>;
  LUsersString: string;
  LHeaderList: TList<string>;
  LOutPutList: TStringList;
  LGroupInfo: TGroupInfo;
  LJSONArray: TJSONArray;
begin
  LGroupInfo.GroupName := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TGroupInfo.cGroupName), FFrameJSONGrid.StringGrid.Selected];
  LJSONArray := TJSONArray.Create;
  try
    FEMSConsoleData.GetGroup(Self, LGroupInfo.GroupName, LJSONArray);
    if TJSONObject(LJSONArray.Items[0]).GetValue(TGroupInfo.cUsers) <> nil then
      LUsersString := TJSONObject(LJSONArray.Items[0]).GetValue(TGroupInfo.cUsers).ToString;
  finally
    LJSONArray.Free
  end;
  LUsersString := LUsersString.Substring(1, LUsersString.Length - 2);
  LOutPutList := TStringList.Create;
  LGroupInfo.Users := TStringList.Create;
  try
    LOutPutList.Delimiter := ',';
    LOutPutList.DelimitedText := LUsersString;
    for I := 0 to LOutPutList.Count - 1 do
      LGroupInfo.Users.Add(LOutPutList[I]);
  finally
    LOutPutList.Free;
  end;
  LDetailsKeys := EMSConsoleData.GetGroupsFieldsArray;
  SetLength(LGroupInfo.Details, Length(LDetailsKeys));
  LHeaderList := TList<string>.Create;
  try
    for I := 0 to FFrameJSONGrid.StringGrid.ColumnCount - 1 do
    begin
      LHeaderList.Add(FFrameJSONGrid.StringGrid.Columns[I].Header);
    end;
    for I := Low(LDetailsKeys) to High(LDetailsKeys) do
    begin
      LGroupInfo.Details[I].Key := LDetailsKeys[I];
      LColIndex := LHeaderList.IndexOf(LDetailsKeys[I]);
      LGroupInfo.Details[I].Value := FFrameJSONGrid.StringGrid.Cells[LColIndex, FFrameJSONGrid.StringGrid.Selected];
    end;
    ADialog.GroupInfo := LGroupInfo;
  finally
    LHeaderList.Free;
  end;
end;

procedure TGroupsTabItem.ShowFrame(ADialog: TFormAddDlg);
begin
  ADialog.AddFrame.ShowGroupAddTab;
end;

procedure TGroupsTabItem.UpdateView(AIndetifier: string;
  ACellList: TList<TCell>);
var
  LJSON: TJSONObject;
  I, Col, Row: Integer;
begin
  LJSON := TJSONObject.Create;
   try
    for I := 0 to ACellList.Count - 1 do
    begin
      Col := ACellList.Items[I].col;
      Row := ACellList.Items[I].row;
      LJSON.AddPair(FFrameJSONGrid.StringGrid.Columns[Col].Header, FFrameJSONGrid.StringGrid.Cells[Col, Row]);
    end;
    FEMSConsoleData.UpdateGroup(AIndetifier, LJSON);
   finally
    LJSON.Free;
   end;
end;


constructor TGroupsTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FrameJSONGrid.OnGetData := OnGetGroupsSData;
  FrameJSONGrid.OnGetFields := OnGetGroupsFields;
end;


function TGroupsTabItem.Delete(const AID: string): Boolean;
begin
  Result := FEMSConsoleData.DeleteGroup(AID);
end;

function TGroupsTabItem.GetObjectIdentifier(const Row: Integer): string;
var
  ColIndex: Integer;
begin
  ColIndex := GetHeaderIndex(TGroupInfo.cGroupName);
  if ColIndex >= 0 then
    Result := FFrameJSONGrid.StringGrid.Cells[ColIndex, Row];
end;

{ TInstallationTabItem }

procedure TInstallationTabItem.OnGetInstallationsData(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetInstallations(Sender, AJSON);
end;

procedure TInstallationTabItem.OnGetInstallationsFields(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetInstallationsFields(Sender, AJSON);
end;

procedure TInstallationTabItem.SetObjectInfoToAddDlg(ADialog: TFormAddDlg);
var
  I, LColIndex: integer;
  LDetailsKeys: TArray<string>;
  LHeaderList: TList<string>;
  LChannelsString: string;
  LOutPutList: TStringList;
  LInstallationInfo: TInstallationInfo;
begin
  LInstallationInfo.InstallationID := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TInstallationInfo.cID), FFrameJSONGrid.StringGrid.Selected];
  LInstallationInfo.DeviceToken := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TInstallationInfo.cDeviceToken), FFrameJSONGrid.StringGrid.Selected];
  LInstallationInfo.DeviceType := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TInstallationInfo.cDeviceType), FFrameJSONGrid.StringGrid.Selected];

  LChannelsString := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TInstallationInfo.cChannels), FFrameJSONGrid.StringGrid.Selected];
  LChannelsString := LChannelsString.Substring(1, LChannelsString.Length - 2);
  LOutPutList := TStringList.Create;
  try
    LOutPutList.Delimiter := ',';
    LOutPutList.DelimitedText := LChannelsString;
    SetLength(LInstallationInfo.Channels, LOutPutList.Count);
    for I := 0 to LOutPutList.Count - 1 do
      LInstallationInfo.Channels[I] :=  LOutPutList[I];
  finally
    LOutPutList.Free;
  end;

  LDetailsKeys := EMSConsoleData.GetInstallationsFieldsArray;

  SetLength(LInstallationInfo.Details, Length(LDetailsKeys));
  LHeaderList := TList<string>.Create;
  try
    for I := 0 to FFrameJSONGrid.StringGrid.ColumnCount - 1 do
    begin
      LHeaderList.Add(FFrameJSONGrid.StringGrid.Columns[I].Header);
    end;
    for I := Low(LDetailsKeys) to High(LDetailsKeys) do
    begin
      LInstallationInfo.Details[I].Key := LDetailsKeys[I];
      LColIndex := LHeaderList.IndexOf(LDetailsKeys[I]);
      LInstallationInfo.Details[I].Value := FFrameJSONGrid.StringGrid.Cells[LColIndex, FFrameJSONGrid.StringGrid.Selected];
    end;
    ADialog.InstallationInfo := LInstallationInfo;
  finally
    LHeaderList.Free;
  end;
end;

procedure TInstallationTabItem.ShowFrame(ADialog: TFormAddDlg);
begin
  ADialog.AddFrame.ShowInstallationAddTab;
end;

procedure TInstallationTabItem.UpdateView(AIndetifier: string;
  ACellList: TList<TCell>);
var
  LJSON: TJSONObject;
  I, Col, Row: Integer;
begin
  LJSON := TJSONObject.Create;
   try
    for I := 0 to ACellList.Count - 1 do
    begin
      Col := ACellList.Items[I].col;
      Row := ACellList.Items[I].row;
      LJSON.AddPair(FFrameJSONGrid.StringGrid.Columns[Col].Header, FFrameJSONGrid.StringGrid.Cells[Col, Row]);
    end;
    FEMSConsoleData.UpdateInstallation(AIndetifier, LJSON);
   finally
    LJSON.Free;
   end;
end;


constructor TInstallationTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FrameJSONGrid.OnGetData := OnGetInstallationsData;
  FrameJSONGrid.OnGetFields := OnGetInstallationsFields;
end;


function TInstallationTabItem.Delete(const AID: string): Boolean;
begin
  Result := FEMSConsoleData.DeleteInstallation(AID);
end;

function TInstallationTabItem.GetObjectIdentifier(const Row: Integer): string;
var
  ColIndex: Integer;
begin
  ColIndex := GetHeaderIndex(TInstallationInfo.cID);
  if ColIndex >= 0 then
    Result := FFrameJSONGrid.StringGrid.Cells[ColIndex, Row];
end;

{ TEdgeModuleTabItem }

constructor TEdgeModuleTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FrameJSONGrid.OnGetData := OnGetEdgeModulesData;
  FrameJSONGrid.OnGetFields := OnGetEdgeModulesFields;
  TabOrder := 0;
end;

function TEdgeModuleTabItem.Delete(const AID: string): Boolean;
begin
  Result := FEMSConsoleData.DeleteEdgeModule(AID);
end;

function TEdgeModuleTabItem.GetObjectIdentifier(const Row: Integer): string;
var
  ColIndex: Integer;
begin
  ColIndex := GetHeaderIndex(TEdgeModuleInfo.cID);
  if ColIndex >= 0 then
    Result := FFrameJSONGrid.StringGrid.Cells[ColIndex, Row];
end;

procedure TEdgeModuleTabItem.OnGetEdgeModulesData(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetEdgeModules(Sender, AJSON);
end;

procedure TEdgeModuleTabItem.OnGetEdgeModulesFields(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetEdgeModulesFields(Sender, AJSON);
end;

procedure TEdgeModuleTabItem.SetObjectInfoToAddDlg(ADialog: TFormAddDlg);
var
  I, LColIndex: integer;
  LDetailsKeys: TArray<string>;
  LHeaderList: TList<string>;
  LEdgeModuleInfo: TEdgeModuleInfo;
begin
  LEdgeModuleInfo.ModuleName := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TEdgeModuleInfo.cModuleName), FFrameJSONGrid.StringGrid.Selected];
  LEdgeModuleInfo.ModuleID := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TEdgeModuleInfo.cID), FFrameJSONGrid.StringGrid.Selected];
  LEdgeModuleInfo.Protocol := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TEdgeModuleInfo.cModuleProtocol), FFrameJSONGrid.StringGrid.Selected];
  LEdgeModuleInfo.ProtocolProps := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TEdgeModuleInfo.cModuleProtocolProps), FFrameJSONGrid.StringGrid.Selected];

  LDetailsKeys := EMSConsoleData.GetEdgeModulesFieldsArray;

  SetLength(LEdgeModuleInfo.Details, Length(LDetailsKeys));
  LHeaderList := TList<string>.Create;
  try
    for I := 0 to FFrameJSONGrid.StringGrid.ColumnCount - 1 do
    begin
      LHeaderList.Add(FFrameJSONGrid.StringGrid.Columns[I].Header);
    end;
    for I := Low(LDetailsKeys) to High(LDetailsKeys) do
    begin
      LEdgeModuleInfo.Details[I].Key := LDetailsKeys[I];
      LColIndex := LHeaderList.IndexOf(LDetailsKeys[I]);
      LEdgeModuleInfo.Details[I].Value := FFrameJSONGrid.StringGrid.Cells[LColIndex, FFrameJSONGrid.StringGrid.Selected]
    end;
    ADialog.EdgeModuleInfo := LEdgeModuleInfo;
  finally
    LHeaderList.Free;
  end;
end;

procedure TEdgeModuleTabItem.ShowFrame(ADialog: TFormAddDlg);
begin
  ADialog.AddFrame.ShowEdgeModuleAddTab;
end;

procedure TEdgeModuleTabItem.UpdateView(AIndetifier: string;
  ACellList: TList<TCell>);
var
  LJSON: TJSONObject;
  I, Col, Row: Integer;
begin
  LJSON := TJSONObject.Create;
   try
    for I := 0 to ACellList.Count - 1 do
    begin
      Col := ACellList.Items[I].col;
      Row := ACellList.Items[I].row;
      LJSON.AddPair(FFrameJSONGrid.StringGrid.Columns[Col].Header, FFrameJSONGrid.StringGrid.Cells[Col, Row]);
    end;
    FEMSConsoleData.UpdateEdgeModule(AIndetifier, '',
      FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TEdgeModuleInfo.cModuleProtocol), Row],
      FrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TEdgeModuleInfo.cModuleProtocolProps), Row], LJSON);
   finally
    LJSON.Free;
   end;
end;

{ TResourceTabItem }

constructor TResourceTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FrameJSONGrid.OnGetData := OnGetResourcesData;
  FrameJSONGrid.OnGetFields := OnGetResourcesFields;
//  FrameJSONGrid.ComboBoxExtras.Items.Clear;
  TabOrder := 0;
end;

function TResourceTabItem.Delete(const AID: string): Boolean;
begin
  Result := FEMSConsoleData.DeleteResource(FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TResourcesInfo.cModuleID), FFrameJSONGrid.StringGrid.Selected], AID);
end;

function TResourceTabItem.GetObjectIdentifier(const Row: Integer): string;
var
  ColIndex: Integer;
begin
  ColIndex := GetHeaderIndex(TResourcesInfo.cResourceName);
  if ColIndex >= 0 then
    Result := FFrameJSONGrid.StringGrid.Cells[ColIndex, Row];
end;

procedure TResourceTabItem.LoadComboModules;
var
  LJSON: TJSONArray;
  LJSONValue: TJSONValue;
  I, Index: Integer;
begin
  LJSON := TJSONArray.Create;
  try
    FEMSConsoleData.GetEdgeModules(Self, LJSON);
    if GetCombo.Items.Count <> LJSON.Count + 1 then
    begin
      GetCombo.Clear;
      GetCombo.Items.Add(strAllEdgeModule);
      for I := 0 to LJSON.Count - 1 do
      begin
        LJSONValue := LJSON.Items[I];
        Index := GetCombo.Items.Add(LJSONValue.GetValue<string>(TResourcesInfo.cModuleName));
        GetCombo.ListItems[Index].ItemData.Detail := LJSONValue.GetValue<string>(TResourcesInfo.cID);
      end;
      GetCombo.ItemIndex := 0;
    end;
  finally
    LJSON.Free
  end;
end;

procedure TResourceTabItem.OnGetResourcesData(Sender: TObject;
  const AJSON: TJSONArray);
var
  LSelectedModule: string;
begin
  LSelectedModule := GetSelectedModule;
  if LSelectedModule = '' then
    LoadComboModules;
  FEMSConsoleData.GetResource(Sender, LSelectedModule, AJSON);
end;

procedure TResourceTabItem.OnGetResourcesFields(Sender: TObject;
  const AJSON: TJSONArray);
begin
  FEMSConsoleData.GetResourcesFields(Sender, AJSON);
end;

procedure TResourceTabItem.SetObjectInfoToAddDlg(ADialog: TFormAddDlg);
var
  I, LColIndex: integer;
  LDetailsKeys: TArray<string>;
  LHeaderList: TList<string>;
  LResourceInfo: TResourcesInfo;
begin
  LResourceInfo.ResourceName := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TResourcesInfo.cResourceName), FFrameJSONGrid.StringGrid.Selected];
  LResourceInfo.ModuleName := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TResourcesInfo.cModuleName), FFrameJSONGrid.StringGrid.Selected];
  LResourceInfo.ModuleID := FFrameJSONGrid.StringGrid.Cells[GetHeaderIndex(TResourcesInfo.cModuleID), FFrameJSONGrid.StringGrid.Selected];

  LDetailsKeys := EMSConsoleData.GetResourcesFieldsArray;

  SetLength(LResourceInfo.Details, Length(LDetailsKeys));
  LHeaderList := TList<string>.Create;
  try
    for I := 0 to FFrameJSONGrid.StringGrid.ColumnCount - 1 do
    begin
      LHeaderList.Add(FFrameJSONGrid.StringGrid.Columns[I].Header);
    end;
    for I := Low(LDetailsKeys) to High(LDetailsKeys) do
    begin
      LResourceInfo.Details[I].Key := LDetailsKeys[I];
      LColIndex := LHeaderList.IndexOf(LDetailsKeys[I]);
      LResourceInfo.Details[I].Value := FFrameJSONGrid.StringGrid.Cells[LColIndex, FFrameJSONGrid.StringGrid.Selected];
    end;
    ADialog.ResourceInfo := LResourceInfo;
  finally
    LHeaderList.Free;
  end;
end;

procedure TResourceTabItem.ShowFrame(ADialog: TFormAddDlg);
begin
  ADialog.AddFrame.ShowResourceAddTab;
end;

procedure TResourceTabItem.UpdateView(AIndetifier: string;
  ACellList: TList<TCell>);
var
  LJSON: TJSONObject;
  I, Col, Row, ColIndex: Integer;
begin
  LJSON := TJSONObject.Create;
   try
    for I := 0 to ACellList.Count - 1 do
    begin
      Col := ACellList.Items[I].col;
      Row := ACellList.Items[I].row;
      LJSON.AddPair(FFrameJSONGrid.StringGrid.Columns[Col].Header, FFrameJSONGrid.StringGrid.Cells[Col, Row]);
    end;
    ColIndex :=  GetHeaderIndex(TResourcesInfo.cModuleID);
    FEMSConsoleData.UpdateResource(FFrameJSONGrid.StringGrid.Cells[ColIndex, Row], AIndetifier, LJSON);
   finally
    LJSON.Free;
   end;
end;

end.
