{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.FrameAdd;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, FMX.Controls.Presentation, FMX.Edit, System.Generics.Collections,
  EMSManagementConsole.Data, System.JSON, System.Rtti, FMX.Grid, FMX.Layouts,
  EMSManagementConsole.Types, FMX.Memo, FMX.ListBox, IPPeerClient,
  REST.Backend.PushTypes, REST.Backend.MetaTypes, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Backend.BindSource, REST.Backend.ServiceComponents,
  FMX.ComboEdit, FMX.ListView.Types, FMX.ListView, FMX.Grid.Style, FMX.ScrollBox;

type
  TAddFrame = class(TFrame)
    AddTabControl: TTabControl;
    AddUserTabItem: TTabItem;
    AddGroupTabItem: TTabItem;
    PassEdit: TEdit;
    PassLabel: TLabel;
    UserEdit: TEdit;
    UserLabel: TLabel;
    AddUserButton: TButton;
    CustomFieldsGrid: TStringGrid;
    StringColumnName: TStringColumn;
    StringColumnValue: TStringColumn;
    CustomGroupFieldsGrid: TStringGrid;
    StringGroupColumnName: TStringColumn;
    StringGroupColumnValue: TStringColumn;
    GroupNameLabel: TLabel;
    GroupNameEdit: TEdit;
    GroupBoxGroups: TVertScrollBox;
    GroupsLabel: TLabel;
    GroupBoxUsers: TVertScrollBox;
    AddInstalltionTabItem: TTabItem;
    UsersLabel: TLabel;
    Button1: TButton;
    Button2: TButton;
    AddUserCancel: TButton;
    AddEdgeModuleTabItem: TTabItem;
    AddResourceTabItem: TTabItem;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    Layout7: TLayout;
    Layout8: TLayout;
    DeviceLabel: TLabel;
    DeviceTypeCombo: TComboBox;
    LabeToken: TLabel;
    TokenEdit: TEdit;
    AddChannelsListBox: TListBox;
    AddInstallButton: TButton;
    ChannelEdit: TEdit;
    AddChannel: TSpeedButton;
    ChannelsListBox: TListBox;
    ChannelsListLabel: TLabel;
    ChannlesBoxLabel: TLabel;
    CustomInstallGrid: TStringGrid;
    StringInstallolumnName: TStringColumn;
    StringInstallolumnValue: TStringColumn;
    ToAddLabel: TLabel;
    Layout9: TLayout;
    Layout10: TLayout;
    AddGroupButton: TButton;
    Layout11: TLayout;
    AddResourceButton: TButton;
    Button4: TButton;
    Layout12: TLayout;
    Layout13: TLayout;
    Layout14: TLayout;
    Layout15: TLayout;
    AddModuleButton: TButton;
    Button6: TButton;
    EditModuleName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    EditProtocol: TEdit;
    Label4: TLabel;
    EditProtocolProps: TEdit;
    CustomEdgeModuleStringGrid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    CustomResourcesStringGrid: TStringGrid;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    EditResourceName: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    EditModuleIDResource: TEdit;
    Label1: TLabel;
    EditModuleNameResource: TEdit;
    procedure CheckBoxChange(Sender: TObject);
    procedure AddUserButtonClick(Sender: TObject);
    procedure AddGroupButtonClick(Sender: TObject);
    procedure ClearUserButtonClick(Sender: TObject);
    procedure ClearGroupButtonClick(Sender: TObject);
    procedure AddInstallButtonClick(Sender: TObject);
    procedure ClearInstallButtonClick(Sender: TObject);
    procedure AddChannelClick(Sender: TObject);
    procedure ChannelsListBoxItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure AddChannelsListBoxItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FGroupsList: TArray<string>;
    FUsersList: TArray<string>;
    FChannlesList: TArray<string>;
    FInitialsChannels: integer;
    FEMSConsoleData: TEMSConsoleData;
    FActiveGrid: TStringGrid;
    FModifiedCheckBox: TList<TCheckBox>;
    FGridModified: Boolean;
    { Private declarations }
    procedure ClearGrid;
    procedure AddGroupsToBox(AUID: string);
    procedure AddUsersToBox(AUsersList: TStringList = nil);
    function AddChekBox(AVertScrollBox: TVertScrollBox; const AName: string): TCheckBox overload;
    procedure CloseModal;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowUserAddTab;
    procedure ShowGroupAddTab;
    procedure ShowInstallationAddTab;
    procedure ShowEdgeModuleAddTab;
    procedure ShowResourceAddTab;
    procedure AddCustomFieldsToGrid;
    procedure SetGroupsCheckBoxes(const AUID: string = '');
    procedure SetUsersCheckBoxes(AUsersList: TStringList = nil);
    procedure SetDeviceTypeCombo(const AType: string = '');
    procedure AddChannelsToBox;
    procedure AddInstallationChannelsToBox(AChannelsArray: TArray<string>);
    function GetUsersGroupBox(IsSelected: Boolean): TArray<string>;
    function GetSelectedUsers:  TArray<string>;
    function GetUnSelectedUsers:  TArray<string>;
    function GetGroupsGroupBox(IsSelected: Boolean): TArray<string>;
    function GetSelectedGroups:  TArray<string>;
    function GetUnSelectedGroups:  TArray<string>;
    property EMSConsoleData: TEMSConsoleData read FEMSConsoleData write FEMSConsoleData;
    property GroupsList: TArray<string> read FGroupsList write FGroupsList;
    property ChannlesList: TArray<string> read FChannlesList write FChannlesList;
    property ActiveGrid: TStringGrid read FActiveGrid;
    property InitialsChannels: integer  read FInitialsChannels write FInitialsChannels;
    property GridModified: Boolean  read FGridModified write FGridModified;
  end;

implementation

uses EMSManagementConsole.Consts, EMSManagementConsole.Form,
  EMSManagementConsole.ModuleBackend, EMSManagementConsole.FrameViews,
  REST.Backend.EMSApi, REST.Backend.EMSProvider, EMSManagementConsole.TypesViews;

{$R *.fmx}

{ TAddFrame }

constructor TAddFrame.Create(AOwner: TComponent);
begin
  inherited;
  FModifiedCheckBox := TList<TCheckBox>.Create;
end;

destructor TAddFrame.Destroy;
begin
  FModifiedCheckBox.Free;
  inherited;
end;

procedure TAddFrame.ShowUserAddTab;
begin
//  AddUserTabItem.Visible := True;
  AddTabControl.ActiveTab := AddUserTabItem;

  AddCustomFieldsToGrid;
  SetGroupsCheckBoxes;
end;

procedure TAddFrame.ShowGroupAddTab;
begin
//  AddGroupTabItem.Visible := True;
  AddTabControl.ActiveTab := AddGroupTabItem;

  AddCustomFieldsToGrid;
  SetUsersCheckBoxes;
end;

procedure TAddFrame.ShowInstallationAddTab;
begin
//  AddInstalltionTabItem.Visible := True;
  AddTabControl.ActiveTab := AddInstalltionTabItem;
  SetDeviceTypeCombo;
  AddChannelsToBox;

  AddCustomFieldsToGrid;
end;

procedure TAddFrame.ShowEdgeModuleAddTab;
begin
  AddTabControl.ActiveTab := AddEdgeModuleTabItem;

  AddCustomFieldsToGrid;
// TO-DO
end;

procedure TAddFrame.ShowResourceAddTab;
begin
  AddTabControl.ActiveTab := AddResourceTabItem;

  AddCustomFieldsToGrid;
//TO-DO
end;

procedure TAddFrame.AddChannelClick(Sender: TObject);
begin
  if (ChannelEdit.Text <> '') and
  (AddChannelsListBox.Items.IndexOf(ChannelEdit.Text) < 0)  then
  begin
    AddChannelsListBox.Items.Add(ChannelEdit.Text);
    ChannelEdit.Text := '';
    AddInstallButton.Enabled := True;
  end;
end;

procedure TAddFrame.AddCustomFieldsToGrid;
var
  I, LColIni: Integer;
  LCustomFields: TArray<string>;
begin
  LColIni := 0;
  if AddTabControl.ActiveTab = AddUserTabItem then
  begin
    FActiveGrid := CustomFieldsGrid;
    LCustomFields := FEMSConsoleData.GetUsersFieldsArray;
    LColIni := Length(LCustomFields) - Length(FEMSConsoleData.GroupsInfo);
  end
 else if AddTabControl.ActiveTab = AddGroupTabItem then
  begin
    FActiveGrid := CustomGroupFieldsGrid;
    LCustomFields := FEMSConsoleData.GetGroupsFieldsArray;
    LColIni := Length(LCustomFields);
  end
  else if AddTabControl.ActiveTab = AddInstalltionTabItem then
  begin
    FActiveGrid := CustomInstallGrid;
    LCustomFields := FEMSConsoleData.GetInstallationsFieldsArray;
    LColIni := Length(LCustomFields);
  end
  else if AddTabControl.ActiveTab = AddEdgeModuleTabItem then
  begin
    FActiveGrid := CustomEdgeModuleStringGrid;
    LCustomFields := FEMSConsoleData.GetEdgeModulesFieldsArray;
    LColIni := Length(LCustomFields);
  end
  else if AddTabControl.ActiveTab = AddResourceTabItem then
  begin
    FActiveGrid := CustomResourcesStringGrid;
    LCustomFields := FEMSConsoleData.GetResourcesFieldsArray;
    LColIni := Length(LCustomFields);
  end;

  ClearGrid;

  if FActiveGrid <>  nil then
  begin
    FActiveGrid.BeginUpdate;
    for I := 0 to LColIni - 1 do
      FActiveGrid.Cells[0, I] := LCustomFields[I];
    FActiveGrid.EndUpdate;
  end;
end;

procedure TAddFrame.AddChannelsToBox;
var
  I: Integer;
begin
  ChannelsListBox.Clear;
  AddChannelsListBox.Clear;
  FChannlesList := FEMSConsoleData.GetChannelsNames;
  for I := Low(FChannlesList) to High(FChannlesList) do
    ChannelsListBox.Items.Add(FChannlesList[I]);
end;

procedure TAddFrame.AddInstallationChannelsToBox(AChannelsArray: TArray<string>);
var
  I: Integer;
begin
  for I := Low(AChannelsArray) to High(AChannelsArray) do
  begin
    AddChannelsListBox.Items.Add(AChannelsArray[I]);
    ChannelsListBox.RemoveObject(ChannelsListBox.ItemByIndex(ChannelsListBox.Items.IndexOf(AChannelsArray[I])));
  end;
  FInitialsChannels := Length(AChannelsArray);
end;

procedure TAddFrame.SetDeviceTypeCombo(const AType: string = '');
begin
  DeviceTypeCombo.Clear;
  DeviceTypeCombo.Items.Add(striOS);
  DeviceTypeCombo.Items.Add(strAndroid);
  if AType <> '' then
    DeviceTypeCombo.ItemIndex := DeviceTypeCombo.Items.IndexOf(AType);
end;

procedure TAddFrame.SetGroupsCheckBoxes(const AUID: string = '');
begin
  FGroupsList := FEMSConsoleData.GetGroupsNames;
  AddGroupsToBox(AUID);
end;

procedure TAddFrame.SetUsersCheckBoxes(AUsersList: TStringList = nil);
begin
  FUsersList := FEMSConsoleData.GetUserNames;
  AddUsersToBox(AUsersList);
end;

procedure TAddFrame.AddGroupsToBox(AUID: string);
var
  I: Integer;
  LChekBox: TCheckBox;
  LGroupsNamesList: TList<string>;
  LGroupsArray: TArray<string>;
begin
  for I := GroupBoxGroups.ComponentCount -1 downto 0  do
    if GroupBoxGroups.Components[I] is TCheckBox then
      GroupBoxGroups.Components[I].Destroy;
    LGroupsNamesList := TList<string>.Create;
    try
      if AUID <> '' then
        LGroupsArray := FEMSConsoleData.GetUserGroups(AUID);
      for I := 0 to Length(LGroupsArray) - 1 do
        LGroupsNamesList.Add(LGroupsArray[I]);

      for I := 0 to Length(FGroupsList) - 1 do
      begin
        LChekBox := AddChekBox(GroupBoxGroups, FGroupsList[I]);
        if (LGroupsNamesList <> nil) and (LGroupsNamesList.IndexOf(FGroupsList[I]) >= 0) then
            LChekBox.IsChecked := True;
        LChekBox.OnChange := CheckBoxChange;
      end;
    finally
      LGroupsNamesList.Free
    end;
end;

procedure TAddFrame.AddInstallButtonClick(Sender: TObject);
var
  LInstallId: string;
begin
  if DeviceTypeCombo.Selected = nil then
    MessageDlg(strDeviteTypeNeeded, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0)
  else if FEMSConsoleData.AddInstallationData(DeviceTypeCombo.Selected.Text, TokenEdit.Text,
    CustomInstallGrid, AddChannelsListBox, LInstallId) then
    CloseModal;
end;

procedure TAddFrame.AddUsersToBox(AUsersList: TStringList = nil);
var
  I: Integer;
  LChekBox: TCheckBox;
  LUserOut: TEMSClientAPI.TUser;
  LUsersNamesList: TList<string>;
begin
  for I := GroupBoxUsers.ComponentCount -1 downto 0  do
    if GroupBoxUsers.Components[I] is TCheckBox then
      GroupBoxUsers.Components[I].Destroy;

  LUsersNamesList := TList<string>.Create;
  try
    if (AUsersList <> nil) then
      for I := 0 to AUsersList.Count - 1 do
      begin
        (DataModule1.BackendPush1.ProviderService as IGetEMSApi).EMSAPI.RetrieveUser(AUsersList[I], LUserOut, nil);
        if LUserOut.UserName <> '' then
          LUsersNamesList.Add(LUserOut.UserName)
      end;

    for I := Low(FUsersList) to High(FUsersList) do
    begin
      LChekBox := AddChekBox(GroupBoxUsers, FUsersList[I]);
      if LUsersNamesList.IndexOf(FUsersList[I]) >= 0 then
      begin
          LChekBox.IsChecked := True;
      end;
      LChekBox.OnChange := CheckBoxChange;
    end;
  finally
    LUsersNamesList.Free
  end;
end;

procedure TAddFrame.ButtonCancelClick(Sender: TObject);
begin
  Visible := False;
  if TForm(Self.Parent).caption = '' then
    TForm2(Root).ViewsFrame.Visible := True
  else
    TForm(Self.Parent).Close;
end;

function TAddFrame.AddChekBox(AVertScrollBox: TVertScrollBox; const AName: string): TCheckBox;
begin
  Result := TCheckBox.Create(AVertScrollBox);
  Result.Parent := AVertScrollBox;
  Result.Text := AName;
  Result.Align := TAlignLayout.Top;
end;

procedure TAddFrame.ClearGrid;
var
  I, J: Integer;
begin
  FActiveGrid.BeginUpdate;
  for I := 0 to FActiveGrid.ColumnCount - 1 do
    for J := 0 to FActiveGrid.RowCount - 1 do
      FActiveGrid.Cells[I, J] := '';
  FActiveGrid.EndUpdate;
  FActiveGrid.Selected := 0;
  FActiveGrid.SetFocus;
end;

procedure TAddFrame.ChannelsListBoxItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  ChannelsListBox.RemoveObject(Item);
  AddInstallButton.Enabled := True;
  if (AddChannelsListBox.Items.IndexOf(Item.Text) < 0) then
    AddChannelsListBox.Items.Add(Item.Text);
end;


procedure TAddFrame.AddChannelsListBoxItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  AddChannelsListBox.RemoveObject(Item);
  AddInstallButton.Enabled := True;
  if (ChannelsListBox.Items.IndexOf(Item.Text) < 0) then
    ChannelsListBox.Items.Add(Item.Text);
end;

procedure TAddFrame.CheckBoxChange(Sender: TObject);
begin
  if FModifiedCheckBox.Contains(TCheckBox(Sender)) then
    FModifiedCheckBox.Remove(TCheckBox(Sender))
  else
    FModifiedCheckBox.Add(TCheckBox(Sender));

  if FModifiedCheckBox.Count > 0 then
  begin
    AddUserButton.Enabled := True;
    AddGroupButton.Enabled := True;
  end
  else if not FGridModified then
  begin
    AddUserButton.Enabled := False;
    AddGroupButton.Enabled := False;
  end;
end;

procedure TAddFrame.AddUserButtonClick(Sender: TObject);
var
  LUserId: string;
  LGroups:  TArray<string>;
begin
  FEMSConsoleData.AddUser(UserEdit.Text, PassEdit.Text, CustomFieldsGrid, LUserId);
  if LUserId <> '' then
  begin
    LGroups := GetSelectedGroups;
    FEMSConsoleData.AddUserToGroups(LUserId, LGroups);
    FModifiedCheckBox.Clear;
    CloseModal;
  end;
end;

procedure TAddFrame.AddGroupButtonClick(Sender: TObject);
var
  LUsers: TArray<string>;
begin
  if FEMSConsoleData.AddGroup(GroupNameEdit.Text, CustomGroupFieldsGrid) then
  begin
    LUsers := GetSelectedUsers;
    FEMSConsoleData.AddUsersToGroup(LUsers, GroupNameEdit.Text);
    FModifiedCheckBox.Clear;
    CloseModal;
  end;
end;

procedure TAddFrame.ClearUserButtonClick(Sender: TObject);
Begin
  ShowUserAddTab;
end;

procedure TAddFrame.CloseModal;
begin
  TForm(Parent).ModalResult := mrOK;
  TForm(Parent).CloseModal;
end;

procedure TAddFrame.ClearGroupButtonClick(Sender: TObject);
begin
   ShowGroupAddTab;
end;

procedure TAddFrame.ClearInstallButtonClick(Sender: TObject);
begin
  ShowInstallationAddTab;
end;

function TAddFrame.GetUsersGroupBox(IsSelected: Boolean): TArray<string>;
var
  I: Integer;
  LStringList: TStringList;
begin
  LStringList := TStringList.Create;
  try
   for I := 0 to GroupBoxUsers.ComponentCount -1 do
    if GroupBoxUsers.Components[I] is TCheckBox then
      if (TCheckBox(GroupBoxUsers.Components[I]).IsChecked = IsSelected) and
       FModifiedCheckBox.Contains(TCheckBox(GroupBoxUsers.Components[I])) then
         LStringList.Add(TCheckBox(GroupBoxUsers.Components[I]).Text);
    Result := LStringList.ToStringArray;
  finally
    LStringList.Free;
  end;
end;

function TAddFrame.GetGroupsGroupBox(IsSelected: Boolean): TArray<string>;
var
  I: Integer;
  LStringList: TStringList;
begin
  LStringList := TStringList.Create;
  try
   for I := 0 to GroupBoxGroups.ComponentCount -1 do
    if GroupBoxGroups.Components[I] is TCheckBox then
      if (TCheckBox(GroupBoxGroups.Components[I]).IsChecked = IsSelected) and
       FModifiedCheckBox.Contains(TCheckBox(GroupBoxGroups.Components[I])) then
        LStringList.Add(TCheckBox(GroupBoxGroups.Components[I]).Text);

    Result := LStringList.ToStringArray;
  finally
    LStringList.Free;
  end;
end;

function TAddFrame.GetSelectedGroups: TArray<string>;
begin
  Result := GetGroupsGroupBox(True);
end;

function TAddFrame.GetSelectedUsers: TArray<string>;
begin
  Result := GetUsersGroupBox(True);
end;

function TAddFrame.GetUnSelectedGroups: TArray<string>;
begin
  Result := GetGroupsGroupBox(False);
end;

function TAddFrame.GetUnSelectedUsers: TArray<string>;
begin
    Result := GetUsersGroupBox(False);
end;

end.
