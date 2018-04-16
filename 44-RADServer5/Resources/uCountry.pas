unit uCountry;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.Client, Data.DB,
  FireDAC.Comp.DataSet, uJSONHelper, FireDAC.ConsoleUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.IBBase;

type

  [ResourceName('country')]
  TCountryResource1 = class(TDataModule)
    EmployeeConnection: TFDConnection;
    CountryTable: TFDQuery;
    CountryTableCOUNTRY: TStringField;
    CountryTableCURRENCY: TStringField;
    FDUpdateSQLCountry: TFDUpdateSQL;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
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

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

procedure TCountryResource1.Get(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  CountryTable.SQL.Text := 'SELECT * FROM COUNTRY ORDER BY COUNTRY';
    AResponse.Body.SetStream(CountryTable.AsJSONStream,
      'application/json', True);
end;

procedure TCountryResource1.GetItem(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  LItem := ARequest.Params.Values['item'];
  if LItem.IsEmpty then
    AResponse.RaiseBadRequest('missing parameter');

  CountryTable.SQL.Text := 'SELECT * FROM COUNTRY WHERE COUNTRY = :COUNTRY';
  CountryTable.Params[0].AsString := LItem;

  AResponse.Body.SetStream(CountryTable.AsJSONStream, 'application/json', True);
end;

procedure TCountryResource1.Post(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LJSON: TJSONObject;
begin
  if not SameText(ARequest.Body.ContentType, 'application/json') then
    AResponse.RaiseBadRequest('content type error');

  if not ARequest.Body.TryGetObject(LJSON) then
    AResponse.RaiseBadRequest('no stream error');

  CountryTable.Open('SELECT * FROM COUNTRY WHERE (1=2)');
  CountryTable.InsertFromJSON(LJSON);
  CountryTable.ApplyUpdates(-1);
end;

procedure TCountryResource1.PutItem(const AContext: TEndpointContext;
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

  CountryTable.SQL.Text := 'SELECT * FROM COUNTRY WHERE COUNTRY = :COUNTRY';
  CountryTable.Params[0].AsString := LItem;
  CountryTable.Open;

  CountryTable.UpdateFromJSON(LJSON);
  CountryTable.ApplyUpdates(-1);
end;

procedure TCountryResource1.DeleteItem(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  LItem := ARequest.Params.Values['item'];
  if LItem.IsEmpty then
    AResponse.RaiseBadRequest('missing parameter');

  EmployeeConnection.StartTransaction;
  try
    CountryTable.SQL.Text := FDUpdateSQLCountry.DeleteSQL.Text;
    CountryTable.Params[0].AsString := LItem;
    CountryTable.ExecSQL;
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
  RegisterResource(TypeInfo(TCountryResource1));
end;

initialization

Register;

end.
