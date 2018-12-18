unit Query_DataModule;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  EMS.DataSetResource, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs;

type
  [ResourceName('TCustomer')]
  TTCustomerResource1 = class(TDataModule)
    EmployeeConnection: TFDConnection;
    CustomerTable: TFDQuery;
    [ResourceSuffix('list', '/')]
    [ResourceSuffix('get', '/{CustomerID}')]
    [ResourceSuffix('put', '/{CustomerID}')]
    [ResourceSuffix('post', '/')]
    [ResourceSuffix('delete', '/{CustomerID}')]
    CustomerResource: TEMSDataSetResource;
    FDQuery1: TFDQuery;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure Register;
begin
  // CustomerID;CompanyName;Country
  RegisterResource(TypeInfo(TTCustomerResource1));
end;

initialization
  Register;
end.


