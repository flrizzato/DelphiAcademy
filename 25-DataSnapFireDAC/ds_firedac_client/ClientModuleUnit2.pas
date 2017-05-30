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
    DSRestCnn: TDSRestConnection;
    CustomerMemTable: TFDMemTable;
    SalesMemTable: TFDMemTable;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    DataSource1: TDataSource;
    CustomerMemTableCUST_NO: TFDAutoIncField;
    CustomerMemTableCUSTOMER: TStringField;
    CustomerMemTableCONTACT_FIRST: TStringField;
    CustomerMemTableCONTACT_LAST: TStringField;
    CustomerMemTablePHONE_NO: TStringField;
    CustomerMemTableADDRESS_LINE1: TStringField;
    CustomerMemTableADDRESS_LINE2: TStringField;
    CustomerMemTableCITY: TStringField;
    CustomerMemTableSTATE_PROVINCE: TStringField;
    CustomerMemTableCOUNTRY: TStringField;
    CustomerMemTablePOSTAL_CODE: TStringField;
    CustomerMemTableON_HOLD: TStringField;
    SalesMemTablePO_NUMBER: TStringField;
    SalesMemTableCUST_NO: TIntegerField;
    SalesMemTableSALES_REP: TSmallintField;
    SalesMemTableORDER_STATUS: TStringField;
    SalesMemTableORDER_DATE: TSQLTimeStampField;
    SalesMemTableSHIP_DATE: TSQLTimeStampField;
    SalesMemTableDATE_NEEDED: TSQLTimeStampField;
    SalesMemTablePAID: TStringField;
    SalesMemTableQTY_ORDERED: TIntegerField;
    SalesMemTableTOTAL_VALUE: TCurrencyField;
    SalesMemTableDISCOUNT: TSingleField;
    SalesMemTableITEM_TYPE: TStringField;
    SalesMemTableAGED: TFMTBCDField;
  private
    FInstanceOwner: Boolean;
    FServerMethods1Client: TServerMethods1Client;
    function GetServerMethods1Client: TServerMethods1Client;
    { Private declarations }
  public
    procedure LoadCustomers(const CustomerID: string);
    procedure LoadSales(const CustomerID: string);
    procedure LoadCustomersAndSales;
    procedure SaveCustomersAndSales;
    procedure SaveCustomersAndSalesManual;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property InstanceOwner: Boolean read FInstanceOwner write FInstanceOwner;
    property ServerMethods1Client: TServerMethods1Client
      read GetServerMethods1Client write FServerMethods1Client;
  end;

var
  ClientModule2: TClientModule2;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

uses Data.FireDACJSONReflect;

constructor TClientModule2.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := True;
end;

destructor TClientModule2.Destroy;
begin
  FServerMethods1Client.Free;
  inherited;
end;

function TClientModule2.GetServerMethods1Client: TServerMethods1Client;
begin
  if FServerMethods1Client = nil then
    FServerMethods1Client := TServerMethods1Client.Create(DSRestCnn,
      FInstanceOwner);
  Result := FServerMethods1Client;
end;

procedure TClientModule2.LoadCustomers(const CustomerID: string);
var
  LDataSetList: TFDJSONDataSets;
begin
  LDataSetList := ServerMethods1Client.GetCustomers(CustomerID);

  CustomerMemTable.Close;
  CustomerMemTable.AppendData(TFDJSONDataSetsReader.GetListValue
    (LDataSetList, 0));
  CustomerMemTable.Open;
end;

procedure TClientModule2.LoadSales(const CustomerID: string);
var
  LDataSetList: TFDJSONDataSets;
begin
  LDataSetList := ServerMethods1Client.GetSales(CustomerID);

  SalesMemTable.Close;
  SalesMemTable.AppendData(TFDJSONDataSetsReader.GetListValue(LDataSetList, 0));
  SalesMemTable.Open;
end;

procedure TClientModule2.LoadCustomersAndSales;
var
  LDataSetList: TFDJSONDataSets;
begin
  LDataSetList := ServerMethods1Client.GetCustomersAndSales;

  CustomerMemTable.Close;
  CustomerMemTable.AppendData(TFDJSONDataSetsReader.GetListValue
    (LDataSetList, 0));
  CustomerMemTable.Open;

  SalesMemTable.Close;
  SalesMemTable.AppendData(TFDJSONDataSetsReader.GetListValue
    (LDataSetList, 1));
  SalesMemTable.Open;
end;

procedure TClientModule2.SaveCustomersAndSales;
var
  Delta: TFDJSONDeltas;
begin
  if CustomerMemTable.State in dsEditModes then
    CustomerMemTable.Post;

  if SalesMemTable.State in dsEditModes then
    SalesMemTable.Post;

  Delta := TFDJSONDeltas.Create;
  TFDJSONDeltasWriter.ListAdd(Delta, CustomerMemTable);
  TFDJSONDeltasWriter.ListAdd(Delta, SalesMemTable);

  ServerMethods1Client.ApplyCustomersAndSales(Delta);
end;

procedure TClientModule2.SaveCustomersAndSalesManual;
var
  Delta: TFDJSONDeltas;
begin
  if CustomerMemTable.State in dsEditModes then
    CustomerMemTable.Post;

  if SalesMemTable.State in dsEditModes then
    SalesMemTable.Post;

  Delta := TFDJSONDeltas.Create;
  TFDJSONDeltasWriter.ListAdd(Delta, CustomerMemTable);
  TFDJSONDeltasWriter.ListAdd(Delta, SalesMemTable);

  ServerMethods1Client.ApplyCustomersAndSalesManual(Delta);
end;

end.
