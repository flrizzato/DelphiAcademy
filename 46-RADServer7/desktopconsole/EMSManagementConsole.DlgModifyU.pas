{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.DlgModifyU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.Layouts, FMX.Grid,
  EMSManagementConsole.FrameAdd, System.Generics.Collections,
  EMSManagementConsole.Types, EMSManagementConsole.Data;

type
  TFormAddDlg = class(TForm)
    AddFrame: TAddFrame;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FUserInfo: TUserInfo;
    FGroupInfo: TGroupInfo;
    FInstallationInfo: TInstallationInfo;
    FEdgeModuleInfo: TEdgeModuleInfo;
    FResourceInfo: TResourcesInfo;
    FDataModified: TDictionary<string, TList<TCell>>;
    procedure GridEditingDone(Sender: TObject; const Col, Row: Integer);
    procedure SetModifyUserDialog;
    procedure SetModifyGroupDialog;
    procedure SetModifyInstallationDialog;
    procedure SetModifyEdgeModule;
    procedure SetModifyResource;
    procedure SetUsersCustomFields;
    procedure SetGroupsCustomFields;
    procedure SetInstallCustomFields;
    procedure SetEdgeModuleCustomFields;
    procedure SetResourceCustomFields;
    procedure UpdateUserButtonClick(Sender: TObject);
    procedure UpdateGroupButtonClick(Sender: TObject);
    procedure UpdateInstallButtonClick(Sender: TObject);
    procedure UpdateEdgeModuleButtonClick(Sender: TObject);
    procedure UpdateResourceButtonClick(Sender: TObject);
    function UpdateUser: Boolean;
    function UpdateGroup: Boolean;
    function UpdateInstallation: Boolean;
    function UpdateEdgeModule: Boolean;
    function UpdateResources: Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; AEMSConsoleData: TEMSConsoleData); reintroduce; overload;
    destructor Destroy; override;
    property UserInfo: TUserInfo read FUserInfo write FUserInfo;
    property GroupInfo: TGroupInfo read FGroupInfo write FGroupInfo;
    property InstallationInfo: TInstallationInfo read FInstallationInfo write FInstallationInfo;
    property EdgeModuleInfo: TEdgeModuleInfo read FEdgeModuleInfo write FEdgeModuleInfo;
    property ResourceInfo: TResourcesInfo read FResourceInfo write FResourceInfo;
  end;

implementation

uses EMSManagementConsole.Consts, System.JSON, FMX.TabControl;

{$R *.fmx}

constructor TFormAddDlg.Create(AOwner: TComponent; AEMSConsoleData: TEMSConsoleData);
begin
  inherited Create(AOwner);
  AddFrame.EMSConsoleData := AEMSConsoleData;
  FDataModified := TDictionary<string, TList<TCell>>.Create;
end;

destructor TFormAddDlg.Destroy;
begin
  FDataModified.Free;
  inherited;
end;

procedure TFormAddDlg.SetModifyUserDialog;
begin
  AddFrame.AddTabControl.ActiveTab := AddFrame.AddUserTabItem;

  AddFrame.UserEdit.Enabled := False;
  AddFrame.PassEdit.Visible := False;
  AddFrame.PassLabel.Visible := False;
  AddFrame.AddUserButton.Enabled := False;
  AddFrame.AddUserTabItem.Visible := True;
  AddFrame.UserEdit.Text := FUserInfo.UserName;

  AddFrame.AddUserButton.OnClick := UpdateUserButtonClick;
  AddFrame.CustomFieldsGrid.OnEditingDone :=  GridEditingDone;
  AddFrame.SetGroupsCheckBoxes(FUserInfo.UserID);

  AddFrame.AddCustomFieldsToGrid;
  SetUsersCustomFields;
end;

procedure TFormAddDlg.SetResourceCustomFields;
var
  I: integer;
begin
  AddFrame.CustomResourcesStringGrid.BeginUpdate;
  for I := 0 to Length(FResourceInfo.Details) - 1 do
  begin
    AddFrame.CustomResourcesStringGrid.Cells[0, I] := FResourceInfo.Details[I].Key;
    AddFrame.CustomResourcesStringGrid.Cells[1, I] := FResourceInfo.Details[I].Value;
  end;
  AddFrame.CustomResourcesStringGrid.EndUpdate;
end;

procedure TFormAddDlg.SetModifyGroupDialog;
begin
  AddFrame.AddTabControl.ActiveTab := AddFrame.AddGroupTabItem;

  AddFrame.GroupNameEdit.Enabled := False;
  AddFrame.AddGroupButton.Enabled := False;
  AddFrame.AddGroupTabItem.Visible := True;
  AddFrame.GroupNameEdit.Text := FGroupInfo.GroupName;

  AddFrame.AddGroupButton.OnClick := UpdateGroupButtonClick;
  AddFrame.CustomGroupFieldsGrid.OnEditingDone :=  GridEditingDone;
  AddFrame.SetUsersCheckBoxes(FGroupInfo.Users);

  AddFrame.AddCustomFieldsToGrid;
  SetGroupsCustomFields;
end;

procedure TFormAddDlg.SetModifyInstallationDialog;
begin
  AddFrame.AddTabControl.ActiveTab := AddFrame.AddInstalltionTabItem;
  AddFrame.SetDeviceTypeCombo(FInstallationInfo.DeviceType);
  AddFrame.AddChannelsToBox;

   AddFrame.AddInstallationChannelsToBox(FInstallationInfo.Channels);
  AddFrame.AddInstallButton.Enabled := False;
  AddFrame.AddInstalltionTabItem.Visible := True;
  AddFrame.TokenEdit.Text := FInstallationInfo.DeviceToken;

  AddFrame.AddInstallButton.OnClick := UpdateInstallButtonClick;
  AddFrame.CustomInstallGrid.OnEditingDone :=  GridEditingDone;
  AddFrame.TokenEdit.Enabled := False;
  AddFrame.DeviceTypeCombo.Enabled := False;


  AddFrame.AddCustomFieldsToGrid;
  SetInstallCustomFields;
end;

procedure TFormAddDlg.SetModifyEdgeModule;
begin
  AddFrame.AddTabControl.ActiveTab := AddFrame.AddEdgeModuleTabItem;

  AddFrame.AddModuleButton.Enabled := False;
  AddFrame.AddEdgeModuleTabItem.Visible := True;
  AddFrame.EditModuleName.Text := FEdgeModuleInfo.ModuleName;
  AddFrame.EditProtocol.Text := FEdgeModuleInfo.Protocol;
  AddFrame.EditProtocolProps.Text := FEdgeModuleInfo.ProtocolProps;

  AddFrame.AddModuleButton.OnClick := UpdateEdgeModuleButtonClick;
  AddFrame.CustomEdgeModuleStringGrid.OnEditingDone :=  GridEditingDone;

  AddFrame.EditModuleName.Enabled := False;
  AddFrame.EditProtocol.Enabled := False;
  AddFrame.EditProtocolProps.Enabled := False;

  AddFrame.AddCustomFieldsToGrid;
  SetEdgeModuleCustomFields;
end;

procedure TFormAddDlg.SetModifyResource;
begin
  AddFrame.AddTabControl.ActiveTab := AddFrame.AddResourceTabItem;

  AddFrame.AddResourceButton.Enabled := False;
  AddFrame.AddResourceTabItem.Visible := True;
  AddFrame.EditResourceName.Text := FResourceInfo.ResourceName;
  AddFrame.EditModuleIDResource.Text := FResourceInfo.ModuleID;
   AddFrame.EditModuleNameResource.Text := FResourceInfo.ModuleName;

  AddFrame.AddResourceButton.OnClick := UpdateResourceButtonClick;
  AddFrame.CustomResourcesStringGrid.OnEditingDone :=  GridEditingDone;

  AddFrame.EditResourceName.Enabled := False;
  AddFrame.EditModuleNameResource.Enabled := False;

  AddFrame.AddCustomFieldsToGrid;
  SetResourceCustomFields;
end;

procedure TFormAddDlg.FormShow(Sender: TObject);
begin
  if FUserInfo.UserName <> '' then
    SetModifyUserDialog
  else if FGroupInfo.GroupName <> '' then
    SetModifyGroupDialog
  else if FInstallationInfo.InstallationID <> '' then
    SetModifyInstallationDialog
  else if FEdgeModuleInfo.ModuleName <> '' then
    SetModifyEdgeModule
  else if FResourceInfo.ResourceName <> '' then
    SetModifyResource;
end;

procedure TFormAddDlg.GridEditingDone(Sender: TObject; const Col,
  Row: Integer);
var
  LCell: TCell;
  LCellList: TList<TCell>;
  LIdentifier: string;
begin
  LCell.col := Col;
  LCell.row := Row;
  if FUserInfo.UserID <> '' then
  begin
    LIdentifier := UserInfo.UserID;
    AddFrame.AddUserButton.Enabled := True;
  end;
  if FGroupInfo.GroupName <> '' then
  begin
    LIdentifier := GroupInfo.GroupName;
    AddFrame.AddGroupButton.Enabled := True;
  end;
  if FInstallationInfo.InstallationID <> '' then
  begin
    LIdentifier := InstallationInfo.InstallationID;
    AddFrame.AddInstallButton.Enabled := True;
  end;
  if FEdgeModuleInfo.ModuleName <> '' then
  begin
    LIdentifier := EdgeModuleInfo.ModuleName;
    AddFrame.AddModuleButton.Enabled := True;
  end;
  if FResourceInfo.ResourceName <> '' then
  begin
    LIdentifier := ResourceInfo.ResourceName;
    AddFrame.AddResourceButton.Enabled := True;
  end;

  if FDataModified.ContainsKey(LIdentifier) then
  begin
    LCellList :=  FDataModified.Items[LIdentifier];
    FDataModified.Remove(LIdentifier);
    if not LCellList.Contains(LCell) then
      LCellList.Add(LCell);
    FDataModified.Add(LIdentifier, LCellList);
  end
  else
  begin
    LCellList:= TList<TCell>.Create;
    LCellList.Add(LCell);
    FDataModified.Add(LIdentifier, LCellList);
  end;
  AddFrame.GridModified := True;
end;

procedure TFormAddDlg.SetUsersCustomFields;
var
  I: integer;
begin
  AddFrame.CustomFieldsGrid.BeginUpdate;
  for I := 0 to Length(FUserInfo.Details) - 1 do
  begin
    AddFrame.CustomFieldsGrid.Cells[0, I] := FUserInfo.Details[I].Key;
    AddFrame.CustomFieldsGrid.Cells[1, I] := FUserInfo.Details[I].Value;
  end;
  AddFrame.CustomFieldsGrid.EndUpdate;
end;

procedure TFormAddDlg.SetEdgeModuleCustomFields;
var
  I: integer;
begin
  AddFrame.CustomEdgeModuleStringGrid.BeginUpdate;
  for I := 0 to Length(FEdgeModuleInfo.Details) - 1 do
  begin
    AddFrame.CustomEdgeModuleStringGrid.Cells[0, I] := FEdgeModuleInfo.Details[I].Key;
    AddFrame.CustomEdgeModuleStringGrid.Cells[1, I] := FEdgeModuleInfo.Details[I].Value;
  end;
  AddFrame.CustomEdgeModuleStringGrid.EndUpdate;
end;

procedure TFormAddDlg.SetGroupsCustomFields;
var
  I: integer;
begin
  AddFrame.CustomGroupFieldsGrid.BeginUpdate;
  for I := 0 to Length(FGroupInfo.Details) - 1 do
  begin
    AddFrame.CustomGroupFieldsGrid.Cells[0, I] := FGroupInfo.Details[I].Key;
    AddFrame.CustomGroupFieldsGrid.Cells[1, I] := FGroupInfo.Details[I].Value;
  end;
  AddFrame.CustomGroupFieldsGrid.EndUpdate;
end;

procedure TFormAddDlg.SetInstallCustomFields;
var
  I: integer;
begin
  AddFrame.CustomInstallGrid.BeginUpdate;
  for I := 0 to Length(FInstallationInfo.Details) - 1 do
  begin
    AddFrame.CustomInstallGrid.Cells[0, I] := FInstallationInfo.Details[I].Key;
    AddFrame.CustomInstallGrid.Cells[1, I] := FInstallationInfo.Details[I].Value;
  end;
  AddFrame.CustomInstallGrid.EndUpdate;
end;

procedure TFormAddDlg.UpdateUserButtonClick(Sender: TObject);
begin
  if UpdateUser then
  begin
    Close;
    ModalResult := mrOK;
  end
  else
    MessageDlg(strUser + ': ' + FUserInfo.UserName + ' ' + strNotUpdated, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
end;

function TFormAddDlg.UpdateUser: Boolean;
var
  LJSON: TJSONObject;
  I: Integer;
  LGroups:  TArray<string>;
  LCellList: TList<TCell>;
begin
  LJSON := TJSONObject.Create;
  try
    if FDataModified.TryGetValue(FUserInfo.UserID, LCellList) then
    begin
      for I := 0 to LCellList.Count - 1 do
        if (AddFrame.CustomFieldsGrid.Cells[0,LCellList.Items[I].row] <> '')
        and (AddFrame.CustomFieldsGrid.Cells[1,LCellList.Items[I].row] <> '') then
          if LJSON.GetValue(AddFrame.CustomFieldsGrid.Cells[0,LCellList.Items[I].row]) = nil then
            LJSON.AddPair(AddFrame.CustomFieldsGrid.Cells[0,LCellList.Items[I].row],
              AddFrame.CustomFieldsGrid.Cells[1,LCellList.Items[I].row]);

        AddFrame.EMSConsoleData.UpdateUser(FUserInfo.UserID, LJSON)
    end;
    LGroups := AddFrame.GetSelectedGroups;
    if Length(LGroups) > 0 then
      AddFrame.EMSConsoleData.AddUserToGroups(FUserInfo.UserID, LGroups);
    LGroups := AddFrame.GetUnSelectedGroups;
    if Length(LGroups) > 0 then
      AddFrame.EMSConsoleData.RemoveUserFromGroups(FUserInfo.UserID, LGroups);
    Result := True;
  finally
    LJSON.Free;
  end;
end;

procedure TFormAddDlg.UpdateGroupButtonClick(Sender: TObject);
begin
  if UpdateGroup then
  begin
    Close;
    ModalResult := mrOK;
  end
  else
    MessageDlg(strGroup + ': ' + FGroupInfo.GroupName + ' ' + strNotUpdated, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
end;


function TFormAddDlg.UpdateEdgeModule: Boolean;
var
  LJSON: TJSONObject;
  I: Integer;
  LCellList: TList<TCell>;
begin
  LJSON := TJSONObject.Create;
  try
    if FDataModified.TryGetValue(FEdgeModuleInfo.ModuleName, LCellList) then
    begin
      for I := 0 to LCellList.Count - 1 do
        if (AddFrame.CustomEdgeModuleStringGrid.Cells[0,LCellList.Items[I].row] <> '')
        and (AddFrame.CustomEdgeModuleStringGrid.Cells[1,LCellList.Items[I].row] <> '') then
          if LJSON.GetValue(AddFrame.CustomEdgeModuleStringGrid.Cells[0,LCellList.Items[I].row]) = nil then
            LJSON.AddPair(AddFrame.CustomEdgeModuleStringGrid.Cells[0,LCellList.Items[I].row],
              AddFrame.CustomEdgeModuleStringGrid.Cells[1,LCellList.Items[I].row]);

        AddFrame.EMSConsoleData.UpdateEdgeModule(FEdgeModuleInfo.ModuleID, FEdgeModuleInfo.ModuleName, FEdgeModuleInfo.Protocol,
          FEdgeModuleInfo.ProtocolProps, LJSON)
    end;
    Result := True;
  finally
    LJSON.Free;
  end;
end;

procedure TFormAddDlg.UpdateEdgeModuleButtonClick(Sender: TObject);
begin
  if UpdateEdgeModule then
  begin
    Close;
    ModalResult := mrOK;
  end
  else
    MessageDlg(strEdgeModule + ': ' + FEdgeModuleInfo.ModuleName + ' ' + strNotUpdated, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
end;

function TFormAddDlg.UpdateGroup: Boolean;
var
  LJSON: TJSONObject;
  I: Integer;
  LUsers:  TArray<string>;
  LCellList: TList<TCell>;
begin
  LJSON := TJSONObject.Create;
  try
    if FDataModified.TryGetValue(FGroupInfo.GroupName, LCellList) then
    begin
      for I := 0 to LCellList.Count - 1 do
      if (AddFrame.CustomGroupFieldsGrid.Cells[0,LCellList.Items[I].row] <> '')
      and (AddFrame.CustomGroupFieldsGrid.Cells[1,LCellList.Items[I].row] <> '') then
        if LJSON.GetValue(AddFrame.CustomGroupFieldsGrid.Cells[0,LCellList.Items[I].row]) = nil then
          LJSON.AddPair(AddFrame.CustomGroupFieldsGrid.Cells[0,LCellList.Items[I].row],
            AddFrame.CustomGroupFieldsGrid.Cells[1,LCellList.Items[I].row]);

      AddFrame.EMSConsoleData.UpdateGroup(FGroupInfo.GroupName, LJSON);
    end;
    LUsers := AddFrame.GetSelectedUsers;
    if Length(LUsers) > 0 then
      AddFrame.EMSConsoleData.AddUsersToGroup(LUsers, FGroupInfo.GroupName);
    LUsers := AddFrame.GetUnSelectedUsers;
    if Length(LUsers) > 0 then
      AddFrame.EMSConsoleData.RemoveUsersFromGroup(LUsers, FGroupInfo.GroupName);
    Result := True;
  finally
    LJSON.Free;
  end;
end;

procedure TFormAddDlg.UpdateInstallButtonClick(Sender: TObject);
begin
  if UpdateInstallation then
  begin
    Close;
    ModalResult := mrOK;
  end
  else
    MessageDlg(strInstallation + ': ' + FInstallationInfo.DeviceToken + ' ' + strNotUpdated, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
end;

procedure TFormAddDlg.UpdateResourceButtonClick(Sender: TObject);
begin
  if UpdateResources then
  begin
    Close;
    ModalResult := mrOK;
  end
  else
    MessageDlg(strEdgeModule + ': ' + FEdgeModuleInfo.ModuleName + ' ' + strNotUpdated, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
end;

function TFormAddDlg.UpdateResources: Boolean;
var
  LJSON: TJSONObject;
  I: Integer;
  LCellList: TList<TCell>;
begin
  LJSON := TJSONObject.Create;
  try
    if FDataModified.TryGetValue(FResourceInfo.ResourceName, LCellList) then
    begin
      for I := 0 to LCellList.Count - 1 do
        if (AddFrame.CustomResourcesStringGrid.Cells[0,LCellList.Items[I].row] <> '')
        and (AddFrame.CustomResourcesStringGrid.Cells[1,LCellList.Items[I].row] <> '') then
          if LJSON.GetValue(AddFrame.CustomResourcesStringGrid.Cells[0,LCellList.Items[I].row]) = nil then
            LJSON.AddPair(AddFrame.CustomResourcesStringGrid.Cells[0,LCellList.Items[I].row],
              AddFrame.CustomResourcesStringGrid.Cells[1,LCellList.Items[I].row]);

        AddFrame.EMSConsoleData.UpdateResource(FResourceInfo.ModuleID, FResourceInfo.ResourceName, LJSON)
    end;
    Result := True;
  finally
    LJSON.Free;
  end;
end;

function TFormAddDlg.UpdateInstallation: Boolean;
var
  LJSON: TJSONObject;
  LJSONArray: TJSONArray;
  I: Integer;
  LCellList: TList<TCell>;
begin
  LJSON := TJSONObject.Create;
  LJSONArray := TJSONArray.Create;
  try
    if AddFrame.AddInstallButton.Enabled then
    begin
      LJSON.AddPair(TInstallationInfo.cDeviceType, AddFrame.DeviceTypeCombo.Selected.Text);
      if FDataModified.TryGetValue(FInstallationInfo.InstallationID, LCellList) then
      begin
        for I := 0 to LCellList.Count - 1 do
        if (AddFrame.CustomInstallGrid.Cells[0,LCellList.Items[I].row] <> '')
        and (AddFrame.CustomInstallGrid.Cells[1,LCellList.Items[I].row] <> '') then
          if LJSON.GetValue(AddFrame.CustomInstallGrid.Cells[0,LCellList.Items[I].row]) = nil then
            LJSON.AddPair(AddFrame.CustomInstallGrid.Cells[0,LCellList.Items[I].row],
              AddFrame.CustomInstallGrid.Cells[1,LCellList.Items[I].row]);
      end;
      for I := 0 to AddFrame.AddChannelsListBox.Count - 1 do
        LJSONArray.Add(AddFrame.AddChannelsListBox.Items[I]);
      LJSON.AddPair(TInstallationInfo.cChannels, LJSONArray);
      AddFrame.EMSConsoleData.UpdateInstallation(FInstallationInfo.InstallationID, LJSON);
    end;
    Result := True;
  finally
    LJSON.Free;
  end;
end;

end.
