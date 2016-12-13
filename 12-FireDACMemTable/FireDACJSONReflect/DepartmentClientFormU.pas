//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit DepartmentClientFormU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, DataSnap.DSClientRest, FireDAC.Comp.DataSet, FireDAC.Comp.Client, System.Rtti,
  Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.Grid, FMX.Layouts,
  FMX.Grid, Data.Bind.DBScope, FMX.ListView.Types, FMX.ListView,
  Data.FireDACJSONReflect, FireDAC.Stan.StorageBin, FireDAC.Stan.StorageJSON,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.Grid.Style, FMX.ScrollBox;

type
  TDepartmentsClientForm = class(TForm)
    ButtonDepartments: TButton;
    FDMemTableDepartments: TFDMemTable;
    BindSourceDBDepartments: TBindSourceDB;
    BindingsList1: TBindingsList;
    StringGridDepartment: TStringGrid;
    ListViewDepartments: TListView;
    LinkFillControlToFieldDepartments: TLinkFillControlToField;
    FDMemTableDepartment: TFDMemTable;
    BindSourceDBDepartment: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB2: TLinkGridToDataSource;
    StringGridEmployee: TStringGrid;
    FDMemTableEmployee: TFDMemTable;
    BindSourceDBEmployee: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB3: TLinkGridToDataSource;
    Layout1: TLayout;
    Layout2: TLayout;
    ToolBar1: TToolBar;
    ButtonApplyUpdates: TButton;
    ButtonRefresh: TButton;
    ButtonNewSession: TButton;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    procedure ButtonDepartmentsClick(Sender: TObject);
    procedure ListViewDepartmentsChange(Sender: TObject);
    procedure ButtonApplyUpdatesClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonNewSessionClick(Sender: TObject);
  private
    procedure GetDepartmentEmployees(const ADEPTNO: string);
    procedure GetDepartmentNames;
    procedure UpdateDepartmentNames(const ADataSetList: TFDJSONDataSets);
    procedure UpdateDepartmentEmployees(ADataSetList: TFDJSONDataSets);
    procedure ApplyUpdates;
    function GetDeltas: TFDJSONDeltas;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DepartmentsClientForm: TDepartmentsClientForm;

implementation

{$R *.fmx}

uses ClientModuleUnit2, System.Generics.Collections,
 System.JSON;


procedure HandleRESTException(const AConnection: TDSRestConnection; const APrefix: string; const E: TDSRestProtocolException);
var
  LJSONValue: TJSONValue;
  LMessage: string;
  LPair: TJSONPair;
begin
  LJSONValue := TJSONObject.ParseJSONValue(BytesOf(E.ResponseText), 0);
  try
    if LJSONValue is TJSONObject then
    begin
      LPair := TJSONObject(LJSONValue).Get(0);
      if LPair.JsonString.Value = 'SessionExpired' then
        // Clear session id because it is no good
        AConnection.SessionID := '';
      // Extract error message
      LMessage := LPair.JSONValue.Value;
    end
    else
      LMessage :=  E.ResponseText;
  finally
    LJSONValue.Free;
  end;
  ShowMessageFmt('%s: %s', [APrefix, LMessage]);
end;

procedure TDepartmentsClientForm.ButtonDepartmentsClick(Sender: TObject);
begin
  GetDepartmentNames;
end;


const
  sEmployees = 'Employees';
  sDepartment = 'Department';

procedure TDepartmentsClientForm.UpdateDepartmentNames(const ADataSetList: TFDJSONDataSets);
begin
  // Update UI
  FDMemTableDepartments.Active  := False;
  Assert(TFDJSONDataSetsReader.GetListCount(ADataSetList) = 1);
  FDMemTableDepartments.AppendData(
    TFDJSONDataSetsReader.GetListValue(ADataSetList, 0));
end;

procedure TDepartmentsClientForm.GetDepartmentNames;
var
  LDataSetList: TFDJSONDataSets;
begin
  try
    // Get dataset list containing department names
    LDataSetList := ClientModule2.ServerMethods1Client.GetDepartmentNames();

    // Update UI
    UpdateDepartmentNames(LDataSetList);
  except
    on E: TDSRestProtocolException do
      HandleRestException(ClientModule2.DSRestConnection1, 'Get Departments error', E)
    else
      raise;
  end;

end;


procedure TDepartmentsClientForm.ButtonApplyUpdatesClick(Sender: TObject);
begin
  ApplyUpdates;
end;

function TDepartmentsClientForm.GetDeltas: TFDJSONDeltas;
begin
  // Post if editing
  if FDMemTableDepartment.State in dsEditModes then
  begin
    FDMemTableDepartment.Post;
  end;

  if FDMemTableEmployee.State in dsEditModes then
  begin
    FDMemTableEmployee.Post;
  end;

  // Create a delta list
  Result := TFDJSONDeltas.Create;
  // Add deltas
  TFDJSONDeltasWriter.ListAdd(Result, sEmployees, FDMemTableEmployee);
  TFDJSONDeltasWriter.ListAdd(Result, sDepartment, FDMemTableDepartment);
end;

procedure TDepartmentsClientForm.ApplyUpdates;
var
  LDeltaList: TFDJSONDeltas;
begin
  LDeltaList := GetDeltas;
  try
    // Call server method.  Pass the delta list.
    ClientModule2.ServerMethods1Client.ApplyChangesDepartmentEmployees(LDeltaList);
  except
    on E: TDSRestProtocolException do
      HandleRestException(ClientModule2.DSRestConnection1, 'Apply Updates error', E)
    else
      raise;
  end;
end;

procedure TDepartmentsClientForm.ButtonRefreshClick(Sender: TObject);
var
  LDEPTNO: string;
begin
  if FDMemTableDepartment.Active then
  begin
    // Show department/employee details
    LDEPTNO := FDMemTableDepartment.FieldByName('DEPT_NO').AsString;
    GetDepartmentEmployees(LDEPTNO);
  end;
end;

procedure TDepartmentsClientForm.ButtonNewSessionClick(Sender: TObject);
begin
  // For testing.  Forget about the server side session.
  ClientModule2.DSRestConnection1.SessionID := '';
end;

procedure TDepartmentsClientForm.ListViewDepartmentsChange(Sender: TObject);
var
  LDEPTNO: string;
begin
  // Show department/employee details
  LDEPTNO := TAppearanceListViewItem(ListViewDepartments.Selected).Detail;
  GetDepartmentEmployees(LDEPTNO);
end;

procedure TDepartmentsClientForm.UpdateDepartmentEmployees(ADataSetList: TFDJSONDataSets);
var
  LDataSet: TFDDataSet;
begin
  // Get department dataset
  LDataSet := TFDJSONDataSetsReader.GetListValueByName(ADataSetList, sDepartment);
  // Update UI
  FDMemTableDepartment.Active := False;
  FDMemTableDepartment.AppendData(LDataSet);

  // Get employees dataset
  LDataSet := TFDJSONDataSetsReader.GetListValueByName(ADataSetList, sEmployees);
  // Update UI
  FDMemTableEmployee.Active  := False;
  FDMemTableEmployee.AppendData(LDataSet);
end;

procedure TDepartmentsClientForm.GetDepartmentEmployees(const ADEPTNO: string);
var
  LDataSetList: TFDJSONDataSets;
begin
  try
    // Call server method to get a dataset list
    LDataSetList := ClientModule2.ServerMethods1Client.GetDepartmentEmployees(ADEPTNO);
    // Update UI
    UpdateDepartmentEmployees(LDataSetList);
  except
    on E: TDSRestProtocolException do
      HandleRestException(ClientModule2.DSRestConnection1, 'Get departments error', E)
    else
      raise;
  end;
end;

end.
