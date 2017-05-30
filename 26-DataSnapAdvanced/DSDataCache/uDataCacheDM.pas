unit uDataCacheDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Data.FireDACJSONReflect, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.StorageBin, FireDAC.Stan.StorageJSON, FireDAC.Phys.IBBase,
  FireDAC.Comp.UI, Vcl.ExtCtrls;

type
{$METHODINFO ON}
  TDataCacheDM = class(TDataModule)
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    DataCache: TFDMemTable;
    EmployeeConnection: TFDConnection;
    DepartmentTable: TFDQuery;
    DepartmentTableDEPT_NO: TStringField;
    DepartmentTableDEPARTMENT: TStringField;
    DepartmentTableHEAD_DEPT: TStringField;
    DepartmentTableMNGR_NO: TSmallintField;
    DepartmentTableBUDGET: TBCDField;
    DepartmentTableLOCATION: TStringField;
    DepartmentTablePHONE_NO: TStringField;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDEventAlerter1: TFDEventAlerter;
    procedure DataModuleCreate(Sender: TObject);
    procedure FDEventAlerter1Alert(ASender: TFDCustomEventAlerter;
      const AEventName: string; const AArgument: Variant);
  private
    { Private declarations }
    procedure InternalDataCacheUpdate;
  public
    { Public declarations }
    procedure DataCacheUpdate;
    function GetDepartmentTable: TFDJSONDataSets;
  end;
{$METHODINFO OFF}

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses ServerContainerUnit1;

{$R *.dfm}

procedure TDataCacheDM.DataCacheUpdate;
begin
  InternalDataCacheUpdate;
end;

procedure TDataCacheDM.DataModuleCreate(Sender: TObject);
begin
  InternalDataCacheUpdate;
  FDEventAlerter1.Active := True;
end;

procedure TDataCacheDM.FDEventAlerter1Alert(ASender: TFDCustomEventAlerter;
  const AEventName: string; const AArgument: Variant);
begin
  if AEventName = 'DEPTO_UPDATED' then
    InternalDataCacheUpdate;
end;

function TDataCacheDM.GetDepartmentTable: TFDJSONDataSets;
begin
  Result := TFDJSONDataSets.Create;
  TFDJSONDataSetsWriter.ListAdd(Result, DataCache);
end;

procedure TDataCacheDM.InternalDataCacheUpdate;
begin
  DepartmentTable.Close;
  DepartmentTable.Open;

  DataCache.Close;
  DataCache.Data := DepartmentTable.Data;
end;

end.
