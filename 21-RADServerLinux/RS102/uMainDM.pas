unit uMainDM;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.StorageJSON, FireDAC.Comp.UI, FireDAC.Phys.IBBase,
  FireDAC.Comp.Client, Data.DB, FireDAC.Comp.DataSet;

type
  [ResourceName('RS102')]
  TRSResource1 = class(TDataModule)
    EmployeeConnection: TFDConnection;
    CustomerTable: TFDQuery;
    FDSchemaAdapter1: TFDSchemaAdapter;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
  published
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    procedure Post(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TRSResource1.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  fMem: TMemoryStream;
begin
  fMem := TMemoryStream.Create;
  try
    CustomerTable.Open;
    FDSchemaAdapter1.SaveToStream(fMem, TFDStorageFormat.sfJSON);
    AResponse.Body.SetStream(fMem, 'application/json', True);
  except
    on E: Exception do
    begin
      fMem.Free;
      raise;
    end;
  end;
end;

procedure TRSResource1.Post(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var fMem: TStream;
begin
  if not ARequest.Body.TryGetStream(fMem) then
    raise Exception.Create('Nada para atualizar...');
  fMem.Position := 0;
  FDSchemaAdapter1.LoadFromStream(fMem, TFDStorageFormat.sfJSON);
  FDSchemaAdapter1.ApplyUpdates;
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TRSResource1));
end;

initialization
  Register;
end.


