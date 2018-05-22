{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementConsole.FrameJSONGridU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  System.Rtti, FMX.Layouts, FMX.Grid, REST.Response.Adapter, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, System.JSON, FMX.TabControl,
  REST.Client, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, EMSManagementConsole.Types,
  System.Generics.Collections, FMX.Controls.Presentation,
  EMSManagementConsole.FrameAdd, FMX.Edit, FMX.ListBox, FMX.Grid.Style, FMX.ScrollBox;

type
  TFrameJSONGrid = class(TFrame)
    StringGrid: TStringGrid;
    BindSourceDB: TBindSourceDB;
    BindingsList: TBindingsList;
    FDMemTable: TFDMemTable;
    RESTResponseDataSetAdapter: TRESTResponseDataSetAdapter;
    LinkGridToDataSourceBindSourceDB: TLinkGridToDataSource;
    ToolBar1: TToolBar;
    EditingPanel: TPanel;
    DoneButton: TSpeedButton;
    CancelButton: TSpeedButton;
    NoEditingPanel: TPanel;
    RefreshButton: TSpeedButton;
    EditModeButton: TSpeedButton;
    AddItemButton: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    LabelAdd: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    DeleteButton: TSpeedButton;
    LabelDelete: TLabel;
    LayoutExtras: TLayout;
    LabelExtras: TLabel;
    ComboBoxExtras: TComboBox;
    SpeedButton1: TSpeedButton;
    Label3: TLabel;
    procedure RESTResponseDataSetAdapterBeforeOpenDataSet(Sender: TObject);
    procedure StringGridHeaderClick(Column: TColumn);
    procedure StringGridColumnMoved(Column: TColumn; FromIndex,
      ToIndex: Integer);
    procedure RefreshButtonClick(Sender: TObject);
    procedure EditModeButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DoneButtonClick(Sender: TObject);
    procedure StringGridEditingDone(Sender: TObject; const Col, Row: Integer);
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddItemButtonClick(Sender: TObject);
    procedure ComboBoxExtrasChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  public type
    TGetJSONArrayEvent = procedure(Sender: TObject; const AJSON: TJSONArray) of object;
  private
    FEditing: Boolean;
    FDataModified: TDictionary<string, TList<TCell>>;
    FWidths: TDictionary<string, Single>;
    FColumnsReadOnly: TList<integer>;
    FOnGetData: TGetJSONArrayEvent;
    FOnGetFields: TGetJSONArrayEvent;
//    FOnEdit: TNotifyEvent;
    procedure RestoreGridLayout;
    procedure SaveGridLayout;
    procedure ShowAddFrame(ADialogType: string = '');
    function GetTabItem: TTabItem;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnGetData: TGetJSONArrayEvent read FOnGetData write FOnGetData;
    property OnGetFields: TGetJSONArrayEvent read FOnGetFields write FOnGetFields;
//    property OnEdit: TNotifyEvent read FOnEdit write FOnEdit;
    property ColumnsReadOnly: TList<integer> read FColumnsReadOnly write FColumnsReadOnly;
    property Editing: Boolean read FEditing write FEditing;
    property DataModified: TDictionary<string, TList<TCell>> read FDataModified write FDataModified;
    procedure Refresh;
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

uses  EMSManagementConsole.Form, EMSManagementConsole.TypesViews,
      EMSManagementConsole.DlgModifyU, EMSManagementConsole.Consts,
      EMSManagementConsole.ModuleBackend;

{$R *.fmx}

{ TFrame1 }

//procedure TFrameJSONGrid.ButtonEditClick(Sender: TObject);
//begin
//  if BindSourceDB.DataSet.RecNo > 0 then
//    if Assigned(FOnEdit) then
//      FOnEdit(Self);
//end;

procedure TFrameJSONGrid.AddItemButtonClick(Sender: TObject);
begin
  AddItemButton.IsPressed := False;
  if DataModule1.EMSProvider.BaseURL <> '' then
    ShowAddFrame(cAddTab)
  else
    raise Exception.Create(strURLBlank);
end;

procedure TFrameJSONGrid.CancelButtonClick(Sender: TObject);
begin
  TForm2(root).ViewsFrame.EnableTabs;
  FEditing := False;
  StringGrid.ReadOnly := True;
  StringGrid.Options := StringGrid.Options + [TGridOption.RowSelect] - [TGridOption.Editing];
  NoEditingPanel.Enabled := True;
  EditingPanel.Enabled := False;
  EditModeButton.IsPressed := False;
  if FDataModified <> nil then
    FDataModified.Clear;
  Refresh;
end;

procedure TFrameJSONGrid.ComboBoxExtrasChange(Sender: TObject);
begin
  Refresh;
end;

constructor TFrameJSONGrid.Create(AOwner: TComponent);
begin
  inherited;
  LayoutExtras.Visible := False;
  FWidths := TDictionary<string, Single>.Create;
  FColumnsReadOnly := TList<integer>.Create;
end;

procedure TFrameJSONGrid.DeleteButtonClick(Sender: TObject);
var
  LTabItem: TEMSTabItem;
  ButtonSelected : Integer;
begin
  if not FEditing and (StringGrid.Selected >= 0) and (StringGrid.RowCount > 0) then
  begin
    LTabItem := TEMSTabItem(GetTabItem);
    ButtonSelected := MessageDlg(strDeleteItem + LTabItem.GetObjectIdentifier(StringGrid.Selected),
      TMsgDlgType.mtWarning, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0);
        if ButtonSelected =  mrYes then
          if LTabItem.Delete(LTabItem.GetObjectIdentifier(StringGrid.Selected)) then
            Refresh;
  end;
end;

destructor TFrameJSONGrid.Destroy;
begin
  FWidths.Free;
  FColumnsReadOnly.Free;
  inherited;
end;

procedure TFrameJSONGrid.DoneButtonClick(Sender: TObject);
var
  LArray: TArray<TPair<string, TList<TCell>>>;
  I: integer;
  LTabItem: TEMSTabItem;
begin
  LTabItem := TEMSTabItem(GetTabItem);
  try
    LArray := FDataModified.ToArray;
    for I := Low(LArray) to High(LArray) do
      LTabItem.UpdateView(LArray[I].Key, LArray[I].Value)
  finally
    CancelButtonClick(Self);
  end;
end;

procedure TFrameJSONGrid.EditModeButtonClick(Sender: TObject);
begin
  EditModeButton.IsPressed := False;
  if not FEditing and (StringGrid.Selected >= 0) then
    if (DataModule1.EMSProvider.BaseURL <> '') then
    begin
      FEditing := True;
      StringGrid.ReadOnly := False;
      StringGrid.Options := StringGrid.Options - [TGridOption.RowSelect] + [TGridOption.Editing];
      FDataModified := TDictionary<string, TList<TCell>>.Create;
      NoEditingPanel.Enabled := False;
      EditingPanel.Enabled := True;
      TForm2(root).ViewsFrame.DisableTabs(TEMSTabItem(GetTabItem));
    end
    else
      raise Exception.Create(strURLBlank);
end;


function TFrameJSONGrid.GetTabItem: TTabItem;
begin
  Result := TTabItem(Parent.Parent);
end;

procedure TFrameJSONGrid.SaveGridLayout;
var
  I: Integer;
  LColumn: TColumn;
begin
  for I := 0 to StringGrid.ColumnCount - 1 do
  begin
    LColumn := StringGrid.Columns[I];
    FWidths.AddOrSetValue(LColumn.Header, LColumn.Width);
    if LColumn.ReadOnly then
      FColumnsReadOnly.Add(I);
  end;
end;

procedure TFrameJSONGrid.ShowAddFrame(ADialogType: string = '');
var
  LFormAddDialog: TFormAddDlg;
  LTabItem: TEMSTabItem;
  LOpenModal: Boolean;
begin
  LOpenModal := True;
  LTabItem := TEMSTabItem(GetTabItem);
  if not FEditing and Assigned(StringGrid) then
  begin
    LFormAddDialog := TFormAddDlg.Create(Self.Owner, LTabItem.EMSConsoleData);
    try
      if ADialogType <> cAddTab then
        if StringGrid.Selected >= 0 then
          LTabItem.SetObjectInfoToAddDlg(LFormAddDialog)
        else
          LOpenModal := False
      else
      begin
        LTabItem.ShowFrame(LFormAddDialog);
        LFormAddDialog.Caption := cAdd;
      end;
      if LOpenModal then
        if LFormAddDialog.ShowModal = mrOK then
          Refresh;
      //Needs to show message if adding failed
    finally
      LFormAddDialog.Free;
    end;
  end;
end;

procedure TFrameJSONGrid.SpeedButton1Click(Sender: TObject);
begin
  ShowAddFrame;
end;

procedure TFrameJSONGrid.StringGridColumnMoved(Column: TColumn; FromIndex,
  ToIndex: Integer);
begin
//
end;

procedure TFrameJSONGrid.StringGridEditingDone(Sender: TObject; const Col,
  Row: Integer);
var
  LCell: TCell;
  LCellList: TList<TCell>;
  LIdentifier: string;
  LTabItem: TEMSTabItem;
begin
  LTabItem := TEMSTabItem(GetTabItem);
  LCell.col := Col;
  LCell.row := Row;
  LIdentifier := LTabItem.GetObjectIdentifier(Row);
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
end;

procedure TFrameJSONGrid.StringGridHeaderClick(Column: TColumn);
begin
               
end;

procedure TFrameJSONGrid.RestoreGridLayout;
var
  I: Integer;
  LColumn: TColumn;
  LWidth: Single;
begin
  for I := 0 to StringGrid.ColumnCount - 1 do
  begin
    LColumn := StringGrid.Columns[I];
    if not FWidths.TryGetValue(LColumn.Header, LWidth) then
      LWidth := 200;
    LColumn.Width := LWidth;
    if FColumnsReadOnly.Contains(I) then
      LColumn.ReadOnly := True;
  end;
end;

procedure TFrameJSONGrid.RESTResponseDataSetAdapterBeforeOpenDataSet(
  Sender: TObject);
var
  LJSON: TJSONArray;
  LAdapter: TRESTResponseDataSetAdapter;
  LValue: TJSONValue;
begin
  if not DataModule1.Closing then
  begin
    LAdapter := Sender as TRESTREsponseDAtaSetAdapter;
    if Assigned(FOnGetFields) then
    begin
      LJSON := TJSONArray.Create;
      try
        FOnGetFields(Self, LJSON);
        if LJSON <> nil then
        begin
          LAdapter.Dataset.FieldDefs.Clear;
          for LValue in LJSON do
          begin
            LAdapter.Dataset.FieldDefs.Add(
              LValue.GetValue<string>('name'),
              TFieldType.ftWideString,
              200);
          end;
        end;
      finally
        LJSON.Free;
      end;
    end;
  end;
end;

procedure TFrameJSONGrid.Refresh;
var
  LJSON: TJSONArray;
begin
  if not DataModule1.Closing then
  begin
    SaveGridLayout;
    RESTResponseDataSetAdapter.Active := False;
    if Assigned(FOnGetData) then
    begin
      LJSON := TJSONArray.Create;
      FOnGetData(Self, LJSON);
      if LJSON <> nil then
      begin
        RESTResponseDataSetAdapter.ResponseJSON :=
          TAdapterJSONValue.Create(LJSON);
        StringGrid.BeginUpdate;
        try
          RESTResponseDataSetAdapter.Active := True;
          RestoreGridLayout;
        finally
          StringGrid.EndUpdate;
        end;
      end;
    end;
  end;
end;

procedure TFrameJSONGrid.RefreshButtonClick(Sender: TObject);
begin
  RefreshButton.IsPressed := False;
  Refresh;
end;

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
