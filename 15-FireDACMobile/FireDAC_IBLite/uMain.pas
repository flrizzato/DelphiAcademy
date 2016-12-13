// ---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// ---------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, Data.FMTBcd, Data.DB,
  Data.Bind.Components, Data.Bind.DBScope, FMX.StdCtrls, FMX.ListView.Types,
  System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors, FMX.ListView,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.IBBase, FireDAC.Phys.IB, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, FireDAC.Phys.IBDef, FMX.Controls.Presentation,
  FMX.ListView.Adapters.Base, FMX.ListView.Appearances, FireDAC.Phys.IBLiteDef;

type
  TIBLiteForm = class(TForm)
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    ToolBar1: TToolBar;
    AddButton: TButton;
    DeleteButton: TButton;
    Label1: TLabel;
    ListView1: TListView;
    LinkFillControlToField1: TLinkFillControlToField;
    FDQueryDelete: TFDQuery;
    FDQueryInsert: TFDQuery;
    FDTableTask: TFDTable;
    FireTaskList: TFDConnection;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TaskListBeforeConnect(Sender: TObject);
  private
    { Private declarations }
    procedure OnIdle(Sender: TObject; var ADone: Boolean);
  public
    { Public declarations }
  end;

var
  IBLiteForm: TIBLiteForm;

implementation

{$R *.fmx}

uses
  System.iOUTils, FMX.DialogService;

procedure TIBLiteForm.AddButtonClick(Sender: TObject);
var
  TaskName: String;
  LPrompt: String;
  LDefaultValue: String;
begin
  try
    LPrompt := 'Task';
    LDefaultValue := '';

    TDialogService.InputQuery('Enter New Task', LPrompt, LDefaultValue,
      procedure(const AResult: TModalResult; const AValues: array of string)
      begin
        if AResult = mrOk then
          TaskName := AValues[0]
        else
          TaskName := '';
        if not(TaskName.Trim = '') then
        begin
          FDQueryInsert.ParamByName('TaskName').AsString := TaskName;
          FDQueryInsert.ExecSQL();
          FDTableTask.Refresh;
          LinkFillControlToField1.BindList.FillList;
        end;
      end);

  except
    on e: Exception do
    begin
      ShowMessage(e.Message);
    end;
  end;
end;

procedure TIBLiteForm.DeleteButtonClick(Sender: TObject);
var
  TaskName: string;
begin
  TaskName := TAppearanceListViewItem(ListView1.Selected).Text;
  try
    FDQueryDelete.ParamByName('TaskName').AsString := TaskName;
    FDQueryDelete.ExecSQL;
    FDTableTask.Refresh;
    LinkFillControlToField1.BindList.FillList;
    if (ListView1.Selected = nil) and (ListView1.Items.Count > 0) then
      // Select last item
      ListView1.ItemIndex := ListView1.Items.Count - 1;
  except
    on e: Exception do
    begin
      ShowMessage(e.Message);
    end;
  end;
end;

procedure TIBLiteForm.FormCreate(Sender: TObject);
begin
  try
    // For unidirectional dataset, don't refill automatically when dataset is activated
    // because dataset is reactivated everytime use DataSet.First.
    LinkFillControlToField1.AutoActivate := False;
    LinkFillControlToField1.AutoFill := False;
    Application.OnIdle := OnIdle;
    FireTaskList.Connected := True;
    FDTableTask.Active := True;
    LinkFillControlToField1.BindList.FillList;
  except
    on e: Exception do
    begin
      ShowMessage(e.Message);
    end;
  end;
end;

procedure TIBLiteForm.OnIdle(Sender: TObject; var ADone: Boolean);
begin
  DeleteButton.Visible := ListView1.Selected <> nil;
end;

procedure TIBLiteForm.TaskListBeforeConnect(Sender: TObject);
begin
{$IF DEFINED(MSWINDOWS)}
  FireTaskList.Params.Values['Database'] :=
    IncludeTrailingBackslash(ExtractFilePath(ParamStr(0))) + 'tasks.gdb';
{$ELSE}
  FireTaskList.Params.Values['Database'] := TPath.GetDocumentsPath + PathDelim
    + 'tasks.gdb';
{$ENDIF}

end;

end.
