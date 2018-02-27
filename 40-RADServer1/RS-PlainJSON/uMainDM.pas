unit uMainDM;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.ConsoleUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.IBBase, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  uJSONHelper;

type
  [ResourceName('RSPlainJSON')]
  TRSPlainJSONResource1 = class(TDataModule)
    EmployeeConnection: TFDConnection;
    CustomerTable: TFDQuery;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    CustomerTableCUST_NO: TFDAutoIncField;
    CustomerTableCUSTOMER: TStringField;
    CustomerTableCONTACT_FIRST: TStringField;
    CustomerTableCONTACT_LAST: TStringField;
    CustomerTablePHONE_NO: TStringField;
    CustomerTableADDRESS_LINE1: TStringField;
    CustomerTableADDRESS_LINE2: TStringField;
    CustomerTableCITY: TStringField;
    CustomerTableSTATE_PROVINCE: TStringField;
    CustomerTableCOUNTRY: TStringField;
    CustomerTablePOSTAL_CODE: TStringField;
    CustomerTableON_HOLD: TStringField;
  published
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure GetItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TRSPlainJSONResource1.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  CustomerTable.SQL.Text := 'SELECT * FROM CUSTOMER';
  AResponse.Body.SetStream(CustomerTable.AsJSONStream, 'application/json', True);
end;

procedure TRSPlainJSONResource1.GetItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  LItem := ARequest.Params.Values['item'];
  CustomerTable.SQL.Text := 'SELECT * FROM CUSTOMER WHERE CUST_NO = ' + LItem;
  AResponse.Body.SetStream(CustomerTable.AsJSONStream, 'application/json', True);
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TRSPlainJSONResource1));
end;

initialization
  Register;
end.


