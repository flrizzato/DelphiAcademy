unit Plain_DataModule;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.ConsoleUI.Wait, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.Phys.IB, FireDAC.Phys.IBDef, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.UI, FireDAC.Stan.StorageJSON,
  FireDAC.Comp.DataSet;

type
  [ResourceName('plain')]
  TPlainResource1 = class(TDataModule)
    FDQuery1: TFDQuery;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    EmployeeConnection: TFDConnection;
  published
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TPlainResource1.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  AStream: TStream;
begin
  EmployeeConnection.Connected := True;
  FDQuery1.Open;
  AStream := TMemoryStream.Create;
  FDQuery1.SaveToStream(AStream, sfJSON);
  AResponse.Body.SetStream(AStream, 'application/json', True);
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TPlainResource1));
end;

initialization
  Register;
end.


