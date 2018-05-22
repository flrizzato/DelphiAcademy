{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.FrameViews;

interface

uses
  System.Classes, FMX.StdCtrls, FMX.Forms,
  EMSManagementConsole.Data, FMX.Grid, System.Generics.Collections,
  EMSManagementConsole.FrameAdd, FMX.TabControl,
  EMSManagementConsole.Types, EMSManagementConsole.TypesViews, FMX.Controls,
  FMX.Controls.Presentation, FMX.Types, EMSManagementConsole.FramePush;

type
  TViewsFrame = class(TFrame)
    ViewsControl: TTabControl;
    PushTabItem: TTabItem;
    PushFrame1: TPushFrame;
  private
    { Private declarations }
    FEMSConsoleData: TEMSConsoleData;
//    procedure OnTabItemClick(Sender: TObject);
    function CreateTab(const AName: string): TEMSTabItem;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableTabs(ATab: TEMSTabItem = nil);
    procedure EnableTabs;
    procedure CreateUsersView(const AUserID: string);
    procedure CreateGroupsView(const AGroupName: string);
    procedure CreateInstallationsView(const AInstallationID: string);
    procedure CreateEdgeModulesView(const AEdgePointID: string);
    procedure CreateResourcesView(const ARersourceID: string);
    property EMSConsoleData: TEMSConsoleData read FEMSConsoleData write FEMSConsoleData;
  end;

implementation

uses EMSManagementConsole.Consts;

{$R *.fmx}

{ TViewsFrame }

constructor TViewsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FEMSConsoleData := TEMSConsoleData.Create;
  CreateUsersView('');
  CreateGroupsView('');
  CreateInstallationsView('');
  CreateEdgeModulesView('');
  CreateResourcesView('');
  ViewsControl.ActiveTab := PushTabItem;
  ViewsControl.Repaint;
end;

procedure TViewsFrame.CreateUsersView(const AUserID: string);
var
  LTabItem: TEMSTabItem;
begin
  LTabItem := CreateTab(strUsers);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(0);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(1);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(2);
end;

procedure TViewsFrame.CreateGroupsView(const AGroupName: string);
var
  LTabItem: TEMSTabItem;
begin
  LTabItem := CreateTab(strGroups);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(0);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(1);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(2);
end;

procedure TViewsFrame.CreateInstallationsView(const AInstallationID: string);
var
  LTabItem: TEMSTabItem;
begin
  LTabItem := CreateTab(strInstallations);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(0);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(1);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(2);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(3);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(4);
{$IFNDEF DEBUG}
  LTabItem.FrameJSONGrid.AddItemButton.Visible := False;
  LTabItem.FrameJSONGrid.LabelAdd.Visible := False;
{$ENDIF}
end;

procedure TViewsFrame.CreateEdgeModulesView(const AEdgePointID: string);
var
  LTabItem: TEMSTabItem;
begin
  LTabItem := CreateTab(strEdgeModules);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(0);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(1);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(2);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(3);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(4);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(5);

  LTabItem.FrameJSONGrid.AddItemButton.Visible := False;
  LTabItem.FrameJSONGrid.LabelAdd.Visible := False;
end;

procedure TViewsFrame.CreateResourcesView(const ARersourceID: string);
var
  LTabItem: TEMSTabItem;
begin

  LTabItem := CreateTab(strResources);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(0);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(1);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(2);
  LTabItem.FrameJSONGrid.ColumnsReadOnly.Add(3);

  LTabItem.FrameJSONGrid.AddItemButton.Visible := False;
  LTabItem.FrameJSONGrid.LabelAdd.Visible := False;
end;

function TViewsFrame.CreateTab(const AName: string): TEMSTabItem;
begin
  Result := nil;
  if AName = strUsers then
    Result := TEMSTabItem(ViewsControl.Insert(0, TUsersTabItem));
  if AName = strGroups then
    Result := TEMSTabItem(ViewsControl.Insert(1, TGroupsTabItem));
  if AName = strInstallations then
    Result := TEMSTabItem(ViewsControl.Insert(2, TInstallationTabItem));
  if AName = strEdgeModules then
    Result := TEMSTabItem(ViewsControl.Insert(3, TEdgeModuleTabItem));
  if AName = strResources then
    Result := TEMSTabItem(ViewsControl.Insert(4, TResourceTabItem));

  Result.Name := AName + cTab;
  Result.Text := AName;
  Result.EMSConsoleData := FEMSConsoleData;
  ViewsControl.ActiveTab := Result;
end;

destructor TViewsFrame.Destroy;
begin
  FEMSConsoleData.Free;
  inherited;
end;

procedure TViewsFrame.DisableTabs(ATab: TEMSTabItem = nil);
var
  I: Integer;
begin
  for I := 0 to ViewsControl.TabCount - 1 do
    if ATab <> ViewsControl.Tabs[I] then
      ViewsControl.Tabs[I].Enabled := False;
end;

procedure TViewsFrame.EnableTabs;
var
  I: Integer;
begin
  for I := 0 to ViewsControl.TabCount - 1 do
    ViewsControl.Tabs[I].Enabled := True;
end;

end.
