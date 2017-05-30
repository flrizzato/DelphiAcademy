unit uEmployeeServer;

interface

uses
  System.SysUtils, System.Classes, Datasnap.DSServer,
  Datasnap.DSAuth, Datasnap.DSProviderDataModuleAdapter,
  System.JSON, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB, FireDAC.Phys.IBDef,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Phys.IBBase, FireDAC.Comp.UI, FireDAC.ConsoleUI.Wait;

{$METHODINFO ON}

type
  TDSEmployeeServer = class(TDataModule)
    EmployeeConnection: TFDConnection;
    EmployeeTable: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    updEmployee: TFDUpdateSQL;
    EmployeeTableEMP_NO: TSmallintField;
    EmployeeTableFIRST_NAME: TStringField;
    EmployeeTableLAST_NAME: TStringField;
    EmployeeTablePHONE_EXT: TStringField;
    EmployeeTableHIRE_DATE: TSQLTimeStampField;
    EmployeeTableDEPT_NO: TStringField;
    EmployeeTableJOB_CODE: TStringField;
    EmployeeTableJOB_GRADE: TSmallintField;
    EmployeeTableJOB_COUNTRY: TStringField;
    EmployeeTableSALARY: TBCDField;
    EmployeeTableFULL_NAME: TStringField;
    procedure EmployeeConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Employee(const Key: string): TJSONArray;
    procedure acceptEmployee(const Key: string; jObject: TJSONObject);
    procedure updateEmployee(const Key: string; jObject: TJSONObject);
    procedure cancelEmployee(Key: string);
  end;
{$METHODINFO OFF}

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

uses DataSetConverter4D.Helper, DataSetConverter4D.Impl, System.IOUtils;

{$R *.dfm}

{ TDSEmployeeServer }

procedure TDSEmployeeServer.acceptEmployee(const Key: string;
  jObject: TJSONObject);
var
  sSQL: string;
begin
  sSQL := 'SELECT * FROM EMPLOYEE';
  sSQL := sSQL + ' WHERE 1=2';
  sSQL := sSQL + ' ORDER BY EMP_NO';

  EmployeeTable.SQL.Text := sSQL;
  EmployeeTable.Open;

  EmployeeTable.FromJSONObject(jObject);
  EmployeeTable.ApplyUpdates(-1);
end;

procedure TDSEmployeeServer.cancelEmployee(Key: string);
var
  Cmd: TFDCustomCommand;
begin
  EmployeeConnection.StartTransaction;
  try
    Cmd := updEmployee.Commands[arDelete];
    Cmd.ParamByName('OLD_EMP_NO').AsString := Key;
    Cmd.Execute;
    EmployeeConnection.Commit;
  except
    on E: Exception do
      EmployeeConnection.Rollback;
  end;
end;

function TDSEmployeeServer.Employee(const Key: string): TJSONArray;
var
  sSQL: string;
begin
  sSQL := 'SELECT * FROM EMPLOYEE';
  if not Key.IsEmpty then
    sSQL := sSQL + ' WHERE EMP_NO = ' + Key;
  sSQL := sSQL + ' ORDER BY EMP_NO';

  try
    EmployeeTable.SQL.Text := sSQL;
    EmployeeTable.Open;

    FSetExceptMask($32);
    Result := EmployeeTable.AsJSONArray;
  except
    on E: Exception do
      raise Exception.Create('Employee: ' + E.Message);
  end;
end;

procedure TDSEmployeeServer.EmployeeConnectionBeforeConnect(Sender: TObject);
begin
{$IFDEF LINUX}
  EmployeeConnection.Params.Database := '/data/employee.gdb';
{$ENDIF}
end;

procedure TDSEmployeeServer.updateEmployee(const Key: string;
  jObject: TJSONObject);
var
  sSQL: string;
begin
  sSQL := 'SELECT * FROM EMPLOYEE';
  if not Key.IsEmpty then
    sSQL := sSQL + ' WHERE EMP_NO = ' + Key;
  sSQL := sSQL + ' ORDER BY EMP_NO';

  EmployeeTable.SQL.Text := sSQL;
  EmployeeTable.Open;

  EmployeeTable.RecordFromJSONObject(jObject);
  EmployeeTable.ApplyUpdates(-1);
end;

end.
