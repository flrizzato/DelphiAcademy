unit uMainResource;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes;

type

  [ResourceName('RSZipCode')]
  TEMSCEPResource1 = class(TDataModule)
  published
    procedure Get(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure GetItem(const AContext: TEndpointContext;
      const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{ %CLASSGROUP 'System.Classes.TPersistent' }

uses AtendeCliente1, uCEPManager,
  Xml.xmldom, Xml.omnixmldom;

{$R *.dfm}

procedure TEMSCEPResource1.Get(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  // Sample code
  AResponse.Body.SetValue(TJSONString.Create('missing parameter'), True)
end;

procedure TEMSCEPResource1.GetItem(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
  address: enderecoERP;
  fJSONObject: TJSONObject;
begin
  address := nil;
  LItem := ARequest.Params.Values['item'];
  if LItem <> '' then
  begin
    try
      DefaultDOMVendor := sOmniXmlVendor;
      address := AtendeCliente1.GetAtendeCliente.consultaCEP(LItem);
      fJSONObject := uCEPManager.TCEPManager.CEP2JSON(address);
      AResponse.Body.SetValue(fJSONObject, True);
    finally
      address.Free;
    end;
  end;
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TEMSCEPResource1));
end;

initialization

Register;

end.
