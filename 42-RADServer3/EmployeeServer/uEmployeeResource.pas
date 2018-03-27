unit uEmployeeResource;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.StorageJSON, FireDAC.Phys.IBBase, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type

  [ResourceName('EmployeeServer')]
  TEmployeeServerResource1 = class(TDataModule)
    EmpCnn: TFDConnection;
    EmployeeTable: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
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
    procedure EmpCnnBeforeConnect(Sender: TObject);
  published
    procedure Get(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses DataSetConverter4D.Helper, System.IniFiles;

{$R *.dfm}

procedure TEmployeeServerResource1.EmpCnnBeforeConnect(Sender: TObject);
var
  fFileName: string;
  fIniFile: TIniFile;
begin
{$IFDEF LINUX64}
  fFileName := '/etc/ems/EmployeeLinux.ini';
{$ELSE}
  fFileName := IncludeTrailingPathDelimiter
    (ExtractFilePath(GetModuleName(HInstance))) + 'EmployeeServer.ini';
{$ENDIF}
  if FileExists(fFileName) then
  begin
    fIniFile := TIniFile.Create(fFileName);
    try
      EmpCnn.Params.Database := fIniFile.ReadString('Data', 'Database', '');
      EmpCnn.Params.UserName := fIniFile.ReadString('Data', 'UserName', '');
      EmpCnn.Params.Password := fIniFile.ReadString('Data', 'Password', '');
      EmpCnn.Params.Values['InstanceName'] := fIniFile.ReadString('Data',
        'InstanceName', '');
    finally
      fIniFile.Free;
    end;
  end;
end;

procedure TEmployeeServerResource1.Get(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  empDataObj: TJSONObject;
begin
  EmployeeTable.Open;
  empDataObj := TJSONObject.Create;
  empDataObj.AddPair('items', EmployeeTable.AsJSONArray);
  AResponse.Body.SetValue(empDataObj, True);
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TEmployeeServerResource1));
end;

initialization

Register;

end.
