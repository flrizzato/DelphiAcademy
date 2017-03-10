unit uMainDM;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes;

type
  [ResourceName('RS101')]
  TRSResource1 = class(TDataModule)
  published
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure GetItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    procedure Post(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure PutItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure DeleteItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TRSResource1.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  if AContext.User = nil then
    raise Exception.Create('User is not authorized!');

  AResponse.Body.SetValue(TJSONString.Create('GET method was executed!'), True)
end;

procedure TRSResource1.GetItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  if AContext.User = nil then
    raise Exception.Create('User is not authorized!');

  LItem := ARequest.Params.Values['item'];
  AResponse.Body.SetValue(TJSONString.Create('GETITEM method was executed! Parameter: ' + LItem), True)
end;

procedure TRSResource1.Post(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  if AContext.User = nil then
    raise Exception.Create('User is not authorized!');

  AResponse.Body.SetValue(TJSONString.Create('POST method was executed!'), True)
end;

procedure TRSResource1.PutItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  if AContext.User = nil then
    raise Exception.Create('User is not authorized!');

  LItem := ARequest.Params.Values['item'];
  AResponse.Body.SetValue(TJSONString.Create('PUTITEM method was executed! Parameter: ' + LItem), True)
end;

procedure TRSResource1.DeleteItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  if AContext.User = nil then
    raise Exception.Create('User is not authorized!');

  LItem := ARequest.Params.Values['item'];
  AResponse.Body.SetValue(TJSONString.Create('DELETEITEM method was executed! Parameter: ' + LItem), True)
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TRSResource1));
end;

initialization
  Register;
end.


