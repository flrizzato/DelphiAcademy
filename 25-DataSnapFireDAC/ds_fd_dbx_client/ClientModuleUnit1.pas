unit ClientModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, ClientClassesUnit1, Data.DBXDataSnap,
  Data.DBXCommon, Data.DbxHTTPLayer, Data.DB, Datasnap.DBClient, Data.SqlExpr,
  Datasnap.DSConnect;

type
  TClientModule1 = class(TDataModule)
    SQLConnection1: TSQLConnection;
    CustomerTable: TClientDataSet;
    DSProviderConnection1: TDSProviderConnection;
    CustomerTableCUST_NO: TAutoIncField;
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
    CustomerTableSalesTable: TDataSetField;
    SalesTable: TClientDataSet;
    SalesTablePO_NUMBER: TStringField;
    SalesTableCUST_NO: TIntegerField;
    SalesTableSALES_REP: TSmallintField;
    SalesTableORDER_STATUS: TStringField;
    SalesTableORDER_DATE: TSQLTimeStampField;
    SalesTableSHIP_DATE: TSQLTimeStampField;
    SalesTableDATE_NEEDED: TSQLTimeStampField;
    SalesTablePAID: TStringField;
    SalesTableQTY_ORDERED: TIntegerField;
    SalesTableTOTAL_VALUE: TCurrencyField;
    SalesTableDISCOUNT: TSingleField;
    SalesTableITEM_TYPE: TStringField;
    SalesTableAGED: TFMTBCDField;
  private
    FInstanceOwner: Boolean;
    FServerMethods1Client: TServerMethods1Client;
    function GetServerMethods1Client: TServerMethods1Client;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property InstanceOwner: Boolean read FInstanceOwner write FInstanceOwner;
    property ServerMethods1Client: TServerMethods1Client read GetServerMethods1Client write FServerMethods1Client;

end;

var
  ClientModule1: TClientModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

constructor TClientModule1.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := True;
end;

destructor TClientModule1.Destroy;
begin
  FServerMethods1Client.Free;
  inherited;
end;

function TClientModule1.GetServerMethods1Client: TServerMethods1Client;
begin
  if FServerMethods1Client = nil then
  begin
    SQLConnection1.Open;
    FServerMethods1Client:= TServerMethods1Client.Create(SQLConnection1.DBXConnection, FInstanceOwner);
  end;
  Result := FServerMethods1Client;
end;

end.
