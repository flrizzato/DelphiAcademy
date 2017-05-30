unit ClientModuleUnit2;

interface

uses
  System.SysUtils, System.Classes, ClientClassesUnit2, Datasnap.DSClientRest,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.StorageBin,
  FireDAC.Stan.StorageJSON;

type
  TClientModule2 = class(TDataModule)
    DSRestConnection1: TDSRestConnection;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    FDDepartmentTable: TFDMemTable;
  private
    FInstanceOwner: Boolean;
    FServerMethods1Client: TServerMethods1Client;
    FDataCacheDMClient: TDataCacheDMClient;
    function GetServerMethods1Client: TServerMethods1Client;
    function GetDataCacheDMClient: TDataCacheDMClient;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property InstanceOwner: Boolean read FInstanceOwner write FInstanceOwner;
    property ServerMethods1Client: TServerMethods1Client
      read GetServerMethods1Client write FServerMethods1Client;
    property DataCacheDMClient: TDataCacheDMClient read GetDataCacheDMClient
      write FDataCacheDMClient;

    procedure FillDepartmentTable;
  end;

var
  ClientModule2: TClientModule2;

implementation

uses Data.FireDACJSONReflect;

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

constructor TClientModule2.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := True;
end;

destructor TClientModule2.Destroy;
begin
  FServerMethods1Client.Free;
  FDataCacheDMClient.Free;
  inherited;
end;

procedure TClientModule2.FillDepartmentTable;
var
  LDataSetList: TFDJSONDataSets;
begin
  LDataSetList := DataCacheDMClient.GetDepartmentTable;

  FDDepartmentTable.Close;
  FDDepartmentTable.AppendData(TFDJSONDataSetsReader.GetListValue
    (LDataSetList, 0));
  FDDepartmentTable.Open;
end;

function TClientModule2.GetServerMethods1Client: TServerMethods1Client;
begin
  if FServerMethods1Client = nil then
    FServerMethods1Client := TServerMethods1Client.Create(DSRestConnection1,
      FInstanceOwner);
  Result := FServerMethods1Client;
end;

function TClientModule2.GetDataCacheDMClient: TDataCacheDMClient;
begin
  if FDataCacheDMClient = nil then
    FDataCacheDMClient := TDataCacheDMClient.Create(DSRestConnection1,
      FInstanceOwner);
  Result := FDataCacheDMClient;
end;

end.
