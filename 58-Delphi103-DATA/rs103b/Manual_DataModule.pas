unit Manual_DataModule;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  [ResourceName('manual')]
  TManualResource1 = class(TDataModule)
    EmployeeConnection: TFDConnection;
    FDQuery1: TFDQuery;
  published
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TManualResource1.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  empDataObj: TJSONObject;
  data: TDataSet;
  emplArray: TJSONArray;
  emplObj: TJSONObject;
  aField: TField;
  callBackParam: string;
  strStream: TStringStream;
begin
  data := FDQuery1;
  data.Open;
  empDataObj := TJSONObject.Create;
  emplArray := TJSONArray.Create;

  while (not data.EOF){ and (count < 2)} do
  begin
    emplObj := TJSONObject.Create;
    for aField in data.Fields do
    begin
    //  if aField.FieldName <> 'HIRE_DATE' then
        emplObj.AddPair(LowerCase(aField.FieldName),
          aField.AsString);
    end;
    emplArray.Add(emplObj);
    data.Next;
  end;

  empDataObj.AddPair ('employee', emplArray);

  // now check for JSONP request
  if ARequest.Params.TryGetValue ('callback', callBackParam) then
  begin
    strStream := TStringStream.Create (
      callBackParam + '(' + empDataObj.ToString + ');');
    AResponse.Body.SetStream(strStream, 'text/javascript', True);
  end
  else
    AResponse.Body.SetValue(empDataObj, True);
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TManualResource1));
end;

initialization
  Register;
end.


