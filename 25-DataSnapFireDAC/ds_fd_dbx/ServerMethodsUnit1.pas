unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, System.Json,
  Datasnap.DSServer, Datasnap.DSAuth, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef,  FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  Datasnap.Provider, FireDAC.Comp.Client,
  Datasnap.DSProviderDataModuleAdapter, FireDAC.Phys.IBBase, FireDAC.Comp.UI,
  FireDAC.ConsoleUI.Wait;

type
{$METHODINFO ON}
  TServerMethods1 = class(TDSServerModule)
    EmployeeConnection: TFDConnection;
    CustomerTable: TFDQuery;
    CustomerProvider: TDataSetProvider;
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
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    SalesTable: TFDQuery;
    CustomerDS: TDataSource;
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
    procedure EmployeeConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
  end;
{$METHODINFO OFF}

implementation

{$R *.dfm}

uses System.StrUtils, System.IOUtils;

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := Value;
end;

procedure TServerMethods1.EmployeeConnectionBeforeConnect(Sender: TObject);
begin
{$IFDEF LINUX}
  EmployeeConnection.Params.Database := TPath.GetHomePath + PathDelim +
    'employee.gdb';
{$ENDIF}
end;

function TServerMethods1.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

end.
