unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, System.Json,
  Datasnap.DSServer, Datasnap.DSAuth, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.ConsoleUI.Wait, Data.DB, FireDAC.Phys.IBBase,
  FireDAC.Comp.UI, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin,
  Data.FireDACJSONReflect;

type
{$METHODINFO ON}
  TServerMethods1 = class(TDataModule)
    EmployeeConnection: TFDConnection;
    CustomerTable: TFDQuery;
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
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    procedure EmployeeConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
    procedure PrepareCustomers(const CustomerID: string = '');
    procedure PrepareSales(const CustomerID: string = '');
  public
    { Public declarations }
    function GetCustomers(const CustomerID: string): TFDJSONDataSets;
    function GetSales(const CustomerID: string): TFDJSONDataSets;
    function GetCustomersAndSales: TFDJSONDataSets;
    procedure ApplyCustomersAndSales(const ADeltaList: TFDJSONDeltas);
    procedure ApplyCustomersAndSalesManual(const ADeltaList: TFDJSONDeltas);
  end;
{$METHODINFO OFF}

implementation

{$R *.dfm}

uses System.StrUtils, System.IOUtils;

procedure TServerMethods1.EmployeeConnectionBeforeConnect(Sender: TObject);
begin
{$IFDEF LINUX}
  EmployeeConnection.Params.Database := TPath.GetHomePath + PathDelim +
    'employee.gdb';
{$ENDIF}
end;

procedure TServerMethods1.PrepareCustomers(const CustomerID: string = '');
var
  sSQL: string;
begin
  sSQL := 'SELECT * FROM CUSTOMER';
  if not CustomerID.IsEmpty then
    sSQL := sSQL + ' WHERE CUST_NO = ' + CustomerID;
  sSQL := sSQL + ' ORDER BY CUST_NO';

  CustomerTable.Active := False;
  CustomerTable.SQL.Text := sSQL;
end;

procedure TServerMethods1.PrepareSales(const CustomerID: string = '');
var
  sSQL: string;
begin
  sSQL := 'SELECT * FROM SALES';
  if not CustomerID.IsEmpty then
    sSQL := sSQL + ' WHERE CUST_NO = ' + CustomerID;
  sSQL := sSQL + ' ORDER BY CUST_NO, PO_NUMBER';

  SalesTable.Active := False;
  SalesTable.SQL.Text := sSQL;
end;

function TServerMethods1.GetCustomers(const CustomerID: string)
  : TFDJSONDataSets;
begin
  PrepareCustomers(CustomerID);

  Result := TFDJSONDataSets.Create;
  TFDJSONDataSetsWriter.ListAdd(Result, CustomerTable);
end;

function TServerMethods1.GetSales(const CustomerID: string): TFDJSONDataSets;
begin
  PrepareSales(CustomerID);

  Result := TFDJSONDataSets.Create;
  TFDJSONDataSetsWriter.ListAdd(Result, SalesTable);
end;

function TServerMethods1.GetCustomersAndSales: TFDJSONDataSets;
begin
  PrepareCustomers;
  PrepareSales;

  Result := TFDJSONDataSets.Create;
  TFDJSONDataSetsWriter.ListAdd(Result, CustomerTable);
  TFDJSONDataSetsWriter.ListAdd(Result, SalesTable);
end;

procedure TServerMethods1.ApplyCustomersAndSales(const ADeltaList
  : TFDJSONDeltas);
var
  LApply: IFDJSONDeltasApplyUpdates;
begin
  LApply := TFDJSONDeltasApplyUpdates.Create(ADeltaList);

  EmployeeConnection.StartTransaction;
  try
    LApply.ApplyUpdates(0, CustomerTable.Command);
    if LApply.Errors.Count = 0 then
    begin
      LApply.ApplyUpdates(1, SalesTable.Command);
      if LApply.Errors.Count > 0 then
        raise Exception.Create(LApply.Errors.Strings.Text);
    end
    else
      Exception.Create(LApply.Errors.Strings.Text);
    EmployeeConnection.Commit;
  except
    on E: Exception do
    begin
      EmployeeConnection.Rollback;
      raise Exception.Create('Error Message: ' + E.Message);
    end;
  end;
end;

procedure TServerMethods1.ApplyCustomersAndSalesManual(const ADeltaList
  : TFDJSONDeltas);
var
  sSQL: string;
  tabC, TabS: TFDMemTable;
  LApply: IFDJSONDeltasApplyUpdates;
begin
  LApply := TFDJSONDeltasApplyUpdates.Create(ADeltaList);

  tabC := LApply.Values[0];
  tabC.FilterChanges := [rtModified, rtInserted, rtDeleted];
  tabC.First;

  TabS := LApply.Values[1];
  TabS.FilterChanges := [rtModified, rtInserted, rtDeleted];
  TabS.First;

  EmployeeConnection.StartTransaction;
  try
    while not tabC.Eof do
    begin
      if tabC.UpdateStatus = usModified then
      begin
        sSQL := 'UPDATE CUSTOMER SET CONTACT_FIRST = ' +
          QuotedSTR(tabC.FieldByName('CONTACT_FIRST').AsString) +
          ' WHERE CUST_NO = ' + tabC.FieldByName('CUST_NO').AsString;
        EmployeeConnection.ExecSQL(sSQL);
      end;
      tabC.Next;
    end;

    while not TabS.Eof do
    begin
      if TabS.UpdateStatus = usModified then
      begin
        sSQL := 'UPDATE SALES SET PAID = ' +
          QuotedSTR(TabS.FieldByName('PAID').AsString) + ' WHERE PO_NUMBER = ' +
          QuotedSTR(TabS.FieldByName('PO_NUMBER').AsString);
        EmployeeConnection.ExecSQL(sSQL);
      end;
      TabS.Next;
    end;

    EmployeeConnection.Commit;
  except
    on E: Exception do
    begin
      EmployeeConnection.Rollback;
      raise Exception.Create('Error Message: ' + E.Message);
    end;
  end;

end;

end.
