unit uCustomer;

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

  [ResourceName('customer')]
  TCustomerResource = class(TDataModule)
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
    FDUpdateSQLCustomer: TFDUpdateSQL;
  published
    procedure Get(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure GetItem(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    procedure Post(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure PutItem(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure DeleteItem(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

procedure TCustomerResource.Get(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  CustomerTable.SQL.Text := 'SELECT * FROM CUSTOMER ORDER BY CUST_NO';
  AResponse.Body.SetStream(CustomerTable.AsJSONStream,
    'application/json', True);
end;

procedure TCustomerResource.GetItem(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  LItem := ARequest.Params.Values['item'];
  if LItem.IsEmpty then
    AResponse.RaiseBadRequest('missing parameter');

  CustomerTable.SQL.Text := 'SELECT * FROM CUSTOMER WHERE CUST_NO = :CUST_NO';
  CustomerTable.Params[0].AsString := LItem;

  AResponse.Body.SetStream(CustomerTable.AsJSONStream,
    'application/json', True);
end;

procedure TCustomerResource.Post(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LJSON: TJSONObject;
begin
  if not SameText(ARequest.Body.ContentType, 'application/json') then
    AResponse.RaiseBadRequest('content type error');

  if not ARequest.Body.TryGetObject(LJSON) then
    AResponse.RaiseBadRequest('no stream error');

  CustomerTable.Open('SELECT * FROM CUSTOMER WHERE (1=2)');
  CustomerTable.InsertFromJSON(LJSON);
  CustomerTable.ApplyUpdates(-1);
end;

procedure TCustomerResource.PutItem(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LJSON: TJSONObject;
  LItem: string;
begin
  if not SameText(ARequest.Body.ContentType, 'application/json') then
    AResponse.RaiseBadRequest('content type error');

  if not ARequest.Body.TryGetObject(LJSON) then
    AResponse.RaiseBadRequest('no stream error');

  LItem := ARequest.Params.Values['item'];
  if LItem.IsEmpty then
    AResponse.RaiseBadRequest('missing parameter');

  CustomerTable.SQL.Text := 'SELECT * FROM CUSTOMER WHERE CUST_NO = :CUSTO_NO';
  CustomerTable.Params[0].AsString := LItem;
  CustomerTable.Open;

  CustomerTable.UpdateFromJSON(LJSON);
  CustomerTable.ApplyUpdates(-1);
end;

procedure TCustomerResource.DeleteItem(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  LItem := ARequest.Params.Values['item'];
  if LItem.IsEmpty then
    AResponse.RaiseBadRequest('missing parameter');

  EmployeeConnection.StartTransaction;
  try
    CustomerTable.SQL.Text := FDUpdateSQLCustomer.DeleteSQL.Text;
    CustomerTable.Params[0].AsString := LItem;
    CustomerTable.ExecSQL;
    EmployeeConnection.Commit;
  except
    on E: Exception do
    begin
      EmployeeConnection.Rollback;
      AResponse.RaiseBadRequest(E.Message);
    end;
  end;
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TCustomerResource));
end;

initialization

Register;

end.
