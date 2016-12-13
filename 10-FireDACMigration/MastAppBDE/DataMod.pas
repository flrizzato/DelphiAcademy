unit DataMod;

{ See the comments in MAIN.PAS for information about this project }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables;

type
  TMastData = class(TDataModule)
    Database: TDatabase;
    NextCust: TTable;
    NextCustNewCust: TFloatField;
    Parts: TTable;
    PartsPartNo: TFloatField;
    PartsDescription: TStringField;
    PartsOnHand: TFloatField;
    PartsOnOrder: TFloatField;
    PartsSource: TDataSource;
    PartsQuery: TQuery;
    PartsQueryPartNo: TFloatField;
    PartsQueryDescription: TStringField;
    PartsQueryOnHand: TFloatField;
    PartsQueryOnOrder: TFloatField;
    VendorSource: TDataSource;
    Vendors: TTable;
    PartsVendorNo: TFloatField;
    PartsCost: TCurrencyField;
    PartsListPrice: TCurrencyField;
    PartsBackOrd: TBooleanField;
    PartsQueryVendorNo: TFloatField;
    PartsQueryCost: TCurrencyField;
    PartsQueryListPrice: TCurrencyField;
    PartsQueryBackOrd: TBooleanField;
    Orders: TTable;
    OrdersOrderNo: TFloatField;
    OrdersCustNo: TFloatField;
    OrdersSaleDate: TDateTimeField;
    OrdersShipDate: TDateTimeField;
    OrdersShipToContact: TStringField;
    OrdersShipToAddr1: TStringField;
    OrdersShipToAddr2: TStringField;
    OrdersShipToCity: TStringField;
    OrdersShipToState: TStringField;
    OrdersShipToZip: TStringField;
    OrdersShipToCountry: TStringField;
    OrdersShipToPhone: TStringField;
    OrdersShipVIA: TStringField;
    OrdersPO: TStringField;
    OrdersEmpNo: TIntegerField;
    OrdersTerms: TStringField;
    OrdersPaymentMethod: TStringField;
    OrdersItemsTotal: TCurrencyField;
    OrdersTaxRate: TFloatField;
    OrdersTaxTotal: TCurrencyField;
    OrdersFreight: TCurrencyField;
    OrdersAmountPaid: TCurrencyField;
    OrdersAmountDue: TCurrencyField;
    OrdersSource: TDataSource;
    CustByOrd: TTable;
    CustByOrdCustNo: TFloatField;
    CustByOrdCompany: TStringField;
    CustByOrdAddr1: TStringField;
    CustByOrdAddr2: TStringField;
    CustByOrdCity: TStringField;
    CustByOrdState: TStringField;
    CustByOrdZip: TStringField;
    CustByOrdCountry: TStringField;
    CustByOrdPhone: TStringField;
    CustByOrdFAX: TStringField;
    CustByOrdTaxRate: TFloatField;
    CustByOrdContact: TStringField;
    CustByOrdLastInvoiceDate: TDateTimeField;
    CustByOrdSrc: TDataSource;
    Items: TTable;
    ItemsItemNo: TFloatField;
    ItemsOrderNo: TFloatField;
    ItemsDescription: TStringField;
    ItemsSellPrice: TCurrencyField;
    ItemsQty: TIntegerField;
    ItemsDiscount: TFloatField;
    ItemsExtPrice: TCurrencyField;
    ItemsSource: TDataSource;
    NextOrd: TTable;
    NextOrdNewKey: TFloatField;
    Emps: TTable;
    EmpsEmpNo: TIntegerField;
    EmpsFullName: TStringField;
    EmpsLastName: TStringField;
    EmpsFirstName: TStringField;
    EmpsPhoneExt: TStringField;
    EmpsHireDate: TDateTimeField;
    EmpsSalary: TFloatField;
    EmpsSource: TDataSource;
    LastItemQuery: TQuery;
    Cust: TTable;
    CustCustNo: TFloatField;
    CustCompany: TStringField;
    CustPhone: TStringField;
    CustLastInvoiceDate: TDateTimeField;
    CustSource: TDataSource;
    CustQuery: TQuery;
    CustQueryCustNo: TFloatField;
    CustQueryCompany: TStringField;
    CustQueryPhone: TStringField;
    CustQueryLastInvoiceDate: TDateTimeField;
    OrdByCustSrc: TDataSource;
    OrdByCust: TTable;
    OrdByCustOrderNo: TFloatField;
    OrdByCustCustNo: TFloatField;
    OrdByCustSaleDate: TDateTimeField;
    OrdByCustShipDate: TDateTimeField;
    OrdByCustItemsTotal: TCurrencyField;
    OrdByCustTaxRate: TFloatField;
    OrdByCustFreight: TCurrencyField;
    OrdByCustAmountPaid: TCurrencyField;
    OrdByCustAmountDue: TCurrencyField;
    ItemsPartNo: TFloatField;
    CustAddr1: TStringField;
    CustAddr2: TStringField;
    CustCity: TStringField;
    CustState: TStringField;
    CustZip: TStringField;
    CustCountry: TStringField;
    CustFAX: TStringField;
    CustTaxRate: TFloatField;
    CustContact: TStringField;
    CustMasterSrc: TDataSource;
    CustByComp: TTable;
    CustByCompSrc: TDataSource;
    CustByLastInvQuery: TQuery;
    CustByLastInvQueryCustNo: TFloatField;
    CustByLastInvQueryCompany: TStringField;
    CustByLastInvQueryAddr1: TStringField;
    CustByLastInvQueryAddr2: TStringField;
    CustByLastInvQueryCity: TStringField;
    CustByLastInvQueryState: TStringField;
    CustByLastInvQueryZip: TStringField;
    CustByLastInvQueryCountry: TStringField;
    CustByLastInvQueryPhone: TStringField;
    CustByLastInvQueryFAX: TStringField;
    CustByLastInvQueryTaxRate: TFloatField;
    CustByLastInvQueryContact: TStringField;
    CustByLastInvQueryLastInvoiceDate: TDateTimeField;
    OrdersByDateQuery: TQuery;
    OrdersSalesPerson: TStringField;
    OrdersByDateQueryOrderNo: TFloatField;
    OrdersByDateQueryCustNo: TFloatField;
    OrdersByDateQuerySaleDate: TDateTimeField;
    OrdersByDateQueryShipDate: TDateTimeField;
    OrdersByDateQueryEmpNo: TIntegerField;
    OrdersByDateQueryShipToContact: TStringField;
    OrdersByDateQueryShipToAddr1: TStringField;
    OrdersByDateQueryShipToAddr2: TStringField;
    OrdersByDateQueryShipToCity: TStringField;
    OrdersByDateQueryShipToState: TStringField;
    OrdersByDateQueryShipToZip: TStringField;
    OrdersByDateQueryShipToCountry: TStringField;
    OrdersByDateQueryShipToPhone: TStringField;
    OrdersByDateQueryShipVIA: TStringField;
    OrdersByDateQueryPO: TStringField;
    OrdersByDateQueryTerms: TStringField;
    OrdersByDateQueryPaymentMethod: TStringField;
    OrdersByDateQueryItemsTotal: TCurrencyField;
    OrdersByDateQueryTaxRate: TFloatField;
    OrdersByDateQueryFreight: TCurrencyField;
    OrdersByDateQueryAmountPaid: TCurrencyField;
    OrdersByDateQueryCompany: TStringField;
    procedure PartsBeforeOpen(DataSet: TDataSet);
    procedure PartsCalcFields(DataSet: TDataSet);
    procedure PartsQueryCalcFields(DataSet: TDataSet);
    procedure OrdersAfterCancel(DataSet: TDataSet);
    procedure OrdersAfterPost(DataSet: TDataSet);
    procedure OrdersBeforeCancel(DataSet: TDataSet);
    procedure OrdersBeforeClose(DataSet: TDataSet);
    procedure OrdersBeforeDelete(DataSet: TDataSet);
    procedure OrdersBeforeInsert(DataSet: TDataSet);
    procedure OrdersBeforeOpen(DataSet: TDataSet);
    procedure OrdersCalcFields(DataSet: TDataSet);
    procedure OrdersNewRecord(DataSet: TDataSet);
    procedure ItemsAfterDelete(DataSet: TDataSet);
    procedure ItemsAfterPost(DataSet: TDataSet);
    procedure EnsureOrdersEdit(DataSet: TDataSet);
    procedure ItemsBeforeEdit(DataSet: TDataSet);
    procedure ItemsBeforeOpen(DataSet: TDataSet);
    procedure ItemsBeforePost(DataSet: TDataSet);
    procedure ItemsCalcFields(DataSet: TDataSet);
    procedure ItemsNewRecord(DataSet: TDataSet);
    procedure EmpsCalcFields(DataSet: TDataSet);
    procedure OrdersCustNoChange(Sender: TField);
    procedure ItemsQtyValidate(Sender: TField);
    procedure OrdersFreightValidate(Sender: TField);
    procedure ItemsPartNoValidate(Sender: TField);
    procedure OrdersSaleDateValidate(Sender: TField);
    procedure CustBeforeOpen(DataSet: TDataSet);
    procedure OrdByCustCalcFields(DataSet: TDataSet);
    procedure CustBeforePost(DataSet: TDataSet);
    procedure OrdersAfterDelete(DataSet: TDataSet);
    procedure OrdersBeforeEdit(DataSet: TDataSet);
    procedure EditUpdateError(DataSet: TDataSet; E: EDatabaseError;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
  private
    PrevPartNo: Double;       { remembers Item's previous part# }
    PrevQty: Longint;         { remembers Item's previous qty }
    DeletingItems: Boolean;   { suppress totals calc. if deleting items }
    FItemNo: Integer;
    function DataDirectory: string;
    procedure SetDatabaseAlias(AliasName: string);
    procedure UpdateTotals;
    procedure DeleteItems;
  public
    procedure UseLocalData;
    procedure UseRemoteData;
    function DataSetApplyUpdates(DataSet: TDataSet; Apply: Boolean): Boolean;
  end;

function Confirm(Msg: string): Boolean;

var
  MastData: TMastData;

implementation

{$R *.dfm}

uses Variants;

{ Utility Functions }

function Confirm(Msg: string): Boolean;
begin
  Result := MessageDlg(Msg, mtConfirmation, mbYesNoCancel, 0) = mrYes;
end;

function TMastData.DataDirectory: string;
begin
  { Assume data is in ..\..\data relative to where we are }
  Result := ExtractFilePath(ParamStr(0));
  Result := ExpandFileName(Result + '..\..\DATA\');
end;

{ This function switches the database to a different alias }

procedure TMastData.SetDatabaseAlias(AliasName: string);
begin
  Screen.Cursor := crHourGlass;
  try
    Database.Close;
    Database.AliasName := AliasName;
    Database.Open;
  finally
    Screen.Cursor := crDefault;
  end;
end;

{ Create an alias for the local data if needed, then swith the Database
  to use it }

procedure TMastData.UseLocalData;
var
  DataDir: string;
begin
  { See if the target alias exists, if not then add it. }
  if not Session.IsAlias('DBDEMOS') then
  begin
    DataDir := DataDirectory;
    if not FileExists(DataDir+'ORDERS.DB') then
      raise Exception.Create('Cannot locate Paradox data files');
    Session.AddStandardAlias('DBDEMOS', DataDir, 'PARADOX');
  end;
  SetDatabaseAlias('DBDEMOS');
end;

{ Create an alias to point to the MastSQL.GDB file if needed }

procedure TMastData.UseRemoteData;
var
  Params: TStringList;
  DataFile: string;
begin

  { See if the alias exists.  if not then add it. }
  if not Session.IsAlias('MASTSQL') then
  begin
    DataFile := DataDirectory + 'MASTSQL.GDB';
    if not FileExists(DataFile) then
      raise Exception.Create('Cannot locate Interbase data file: MASTSQL.GDB');
    Params := TStringList.create;
    try
      Params.Values['SERVER NAME'] := DataFile;
      Params.Values['USER NAME'] := 'SYSDBA';
      Session.AddAlias('MASTSQL', 'INTRBASE', Params);
    finally
       Params.Free;
    end;
  end;
  SetDatabaseAlias('MASTSQL');
end;

{ Event Handlers }

procedure TMastData.PartsBeforeOpen(DataSet: TDataSet);
begin
  Vendors.Open;
end;

procedure TMastData.PartsCalcFields(DataSet: TDataSet);
begin
  PartsBackOrd.Value := PartsOnOrder.Value > PartsOnHand.Value;
end;

procedure TMastData.PartsQueryCalcFields(DataSet: TDataSet);
begin
  PartsQueryBackOrd.Value := PartsOnOrder.Value > PartsOnHand.Value;
end;

{ If user cancels the updates to the orders table, cancel the updates to
  the line items as well }

procedure TMastData.OrdersAfterCancel(DataSet: TDataSet);
begin
  Cust.CancelUpdates;
  Parts.CancelUpdates;
  Items.CancelUpdates;
  Orders.CancelUpdates;
end;

procedure TMastData.OrdersAfterDelete(DataSet: TDataSet);
begin
  Database.ApplyUpdates([Cust, Parts, Items, Orders]);
end;

{ Order Entry }

{ Post new LastInvoiceDate to CUST table. }

procedure TMastData.OrdersAfterPost(DataSet: TDataSet);

begin
  if Cust.Locate('CustNo', OrdersCustNo.Value, []) and
    (CustLastInvoiceDate.Value < OrdersShipDate.Value) then
  begin
    Cust.Edit;
    CustLastInvoiceDate.Value := OrdersShipDate.Value;
    Cust.Post;
  end;
  Database.ApplyUpdates([Orders, Items, Parts, Cust]);
end;

procedure TMastData.OrdersBeforeCancel(DataSet: TDataSet);
begin
  if (Orders.State = dsInsert) and not (Items.BOF and Items.EOF) then
    if not Confirm('Cancel order being inserted and delete all line items?') then
      Abort;
end;

procedure TMastData.OrdersBeforeClose(DataSet: TDataSet);
begin
  Items.Close;
  Emps.Close;
  CustByOrd.Close;
end;

procedure TMastData.OrdersBeforeDelete(DataSet: TDataSet);
begin
  if not Confirm('Delete order and line items?') then
    Abort
  else
    DeleteItems;
end;

procedure TMastData.OrdersBeforeInsert(DataSet: TDataSet);
begin
  if Orders.State in dsEditModes then
  begin
    if Confirm('An order is being processed.  Save changes and start a new one?') then
      Orders.Post
    else
      Abort;
  end;
  FItemNo := 1;
end;

procedure TMastData.OrdersBeforeOpen(DataSet: TDataSet);
begin
  CustByComp.Open;
  CustByOrd.Open;
  Cust.Open;
  Emps.Open;
  Items.Open;
end;

{ Calculate the order's tax totals and amount due }

procedure TMastData.OrdersCalcFields(DataSet: TDataSet);
begin
  OrdersTaxTotal.Value := OrdersItemsTotal.Value * (OrdersTaxRate.Value / 100);
  OrdersAmountDue.Value := OrdersItemsTotal.Value + OrdersTaxTotal.Value +
    OrdersFreight.Value - OrdersAmountPaid.Value;
end;

{ Inititializes the record values as a result of an Orders.Insert. }

procedure TMastData.OrdersNewRecord(DataSet: TDataSet);
begin

  { Get the Next Order Value from the NextOrd Table }

  with NextOrd do
  begin
    Open;
    try
      Edit;
      OrdersOrderNo.Value := NextOrdNewKey.Value;
      NextOrdNewKey.Value := NextOrdNewKey.Value + 1;
      Post;
    finally
      Close;
    end;
  end;
  OrdersSaleDate.Value := Date;
  OrdersShipVia.Value := 'UPS';
  OrdersTerms.Value := 'net 30';
  OrdersPaymentMethod.Value := 'Check';
  OrdersItemsTotal.Value := 0;
  OrdersTaxRate.Value := 0;
  OrdersFreight.Value := 0;
  OrdersAmountPaid.Value := 0;
end;

procedure TMastData.ItemsAfterDelete(DataSet: TDataSet);
begin
  UpdateTotals;
end;

{ Update the order totals and the Parts table }

procedure TMastData.ItemsAfterPost(DataSet: TDataSet);

  { Reduce/increase Parts table's OnOrder field }

  procedure UpdateParts(PartNo: Double; Qty : Longint);
  begin
    if (PartNo > 0) and (Qty <> 0) then
    try
      if not Parts.Locate('PartNo', PartNo, []) then Abort;
      Parts.Edit;
      PartsOnOrder.Value := PartsOnOrder.Value + Qty;
      Parts.Post;
    except
      on E: Exception do
        ShowMessage(Format('Error updating parts table for PartNo: %d', [PartNo]));
    end;
  end;

begin
  { Maintain next available item number }
  Inc(FItemNo);
  UpdateTotals;
  if not ((PrevPartNo = ItemsPartNo.Value) and (PrevQty = ItemsQty.Value)) then
  begin
   { Reduce previous Part#'s OnOrder field by previous Qty }
    UpdateParts(PrevPartNo, -PrevQty);
   { Increase new Part#'s OnOrder field by previous Qty }
    UpdateParts(ItemsPartNo.Value, ItemsQty.Value);
  end;
end;

{  When a change to the detail table affects a field in the master, always make
  sure the master (orders) table is in edit or insert mode before allowing the
  detail table to be modified. }

procedure TMastData.EnsureOrdersEdit(DataSet: TDataSet);
begin
  Orders.Edit;
end;

{ Remember previous PartNo and Qty for updating Parts.OnOrder after post.
  When a change to the detail table affects a field in the master, always make
  sure the master table is in edit or insert mode before allowing the
  detail table to be modified. }

procedure TMastData.ItemsBeforeEdit(DataSet: TDataSet);
begin
  Orders.Edit;
  PrevPartNo := ItemsPartNo.Value;
  PrevQty := ItemsQty.Value;
end;

{ Make sure the Parts table opens before the Items table, since there are
  lookups which depend on it. }

procedure TMastData.ItemsBeforeOpen(DataSet: TDataSet);
begin
  Parts.Open;
end;

{ Complete the item's key by initializing its NextItemNo field }

procedure TMastData.ItemsBeforePost(DataSet: TDataSet);
begin
  ItemsItemNo.Value := FItemNo;
end;

{ Lookup PartNo info for the item; calculate its extended price }

procedure TMastData.ItemsCalcFields(DataSet: TDataSet);
begin
  ItemsExtPrice.Value := ItemsQty.Value *
    ItemsSellPrice.Value * (100 - ItemsDiscount.Value) / 100;
end;

{ New item. Zero the "prev" buckets, initialize the key }

procedure TMastData.ItemsNewRecord(DataSet: TDataSet);
begin
  PrevPartNo := 0;
  PrevQty := 0;
  ItemsOrderNo.Value := OrdersOrderNo.Value;
  ItemsQty.Value := 1;
  ItemsDiscount.Value := 0;
end;

{ Concatenate last name + first name for the order's SoldBy DBLookupCombo }

procedure TMastData.EmpsCalcFields(DataSet: TDataSet);
begin
  EmpsFullName.Value := Format('%s, %s', [EmpsLastName.Value, EmpsFirstName.Value]);
end;

procedure TMastData.DeleteItems;
begin
  DeletingItems := True;    { suppress recalc of totals during delete }
  Items.DisableControls;    { for faster table traversal }
  try
    Items.First;
    while not Items.EOF do Items.Delete;
  finally
    DeletingItems := False;
    Items.EnableControls;   { always re-enable controls after disabling }
  end;
end;

{ Steps through Items and gathers sum of ExtPrice. After OrdersItemsTotal
  is calculated, OrdersCalcFields is automatically called (which
  updates other calculated fields. }
  
procedure TMastData.UpdateTotals;
var
  TempTotal: Extended;
  PrevRecord: TBookmark;
begin
  if DeletingItems then Exit;		{ don't calculate if deleting all items }
  PrevRecord := Items.GetBookmark;	{ returns nil if table is empty }
  try
    Items.DisableControls;
    Items.First;
    TempTotal := 0;			{ use temp for efficiency }
    while not Items.EOF do
    begin
      TempTotal := TempTotal + ItemsExtPrice.Value;
      Items.Next;
    end;
    OrdersItemsTotal.Value := TempTotal;
  finally
     Items.EnableControls;
     if PrevRecord <> nil then
     begin
       Items.GoToBookmark(PrevRecord);
       Items.FreeBookmark(PrevRecord);
     end;
  end;
end;

procedure TMastData.OrdersCustNoChange(Sender: TField);
var
  TaxRate: Variant;
begin
  OrdersShipToContact.Value := '';
  OrdersShipToPhone.Value := '';
  OrdersShipToAddr1.Value := '';
  OrdersShipToAddr2.Value := '';
  OrdersShipToCity.Value := '';
  OrdersShipToState.Value := '';
  OrdersShipToZip.Value := '';
  OrdersShipToCountry.Value := '';
  TaxRate := Cust.Lookup('CustNo', OrdersCustNo.Value, 'TaxRate');
  if not VarIsNull(TaxRate) then
    OrdersTaxRate.Value := TaxRate;
end;

{ Alternatively, could set the Qty field's Min and Max values in code
  or in the Object Inspector. }

procedure TMastData.ItemsQtyValidate(Sender: TField);
begin
  if ItemsQty.Value < 1 then
    raise Exception.Create('Must specify quantity');
end;

{ Alternatively, could set the Freight field's Min and Max values in code
  or in the Object Inspector. }

procedure TMastData.OrdersFreightValidate(Sender: TField);
begin
  if OrdersFreight.Value < 0 then
    raise Exception.Create('Freight cannot be less than zero');
end;

procedure TMastData.ItemsPartNoValidate(Sender: TField);
begin
  if not Parts.Locate('PartNo', ItemsPartNo.Value, []) then
    raise Exception.Create('You must specify a valid PartNo');
end;

procedure TMastData.OrdersSaleDateValidate(Sender: TField);
begin
  if OrdersSaleDate.Value > Now then
    raise Exception.Create('Cannot enter a future date');
end;

{ Browse Customers }

procedure TMastData.CustBeforeOpen(DataSet: TDataSet);
begin
  OrdByCust.Open;
end;

procedure TMastData.OrdByCustCalcFields(DataSet: TDataSet);
begin
  OrdByCustAmountDue.Value := OrdByCustItemsTotal.Value +
    OrdByCustItemsTotal.Value * OrdByCustTaxRate.Value / 100 +
    OrdByCustFreight.Value - OrdByCustAmountPaid.Value;
end;

{ Get the next available customer number from the NextCust table }

procedure TMastData.CustBeforePost(DataSet: TDataSet);
begin
  if Cust.State = dsInsert then
    with NextCust do
    begin
      Open;
      try
        Edit;
        CustCustNo.Value := NextCustNewCust.Value;
        NextCustNewCust.Value := NextCustNewCust.Value + 1;
        Post;
      finally
        Close;
      end;
    end;
end;

function TMastData.DataSetApplyUpdates(DataSet: TDataSet; Apply: Boolean): Boolean;
begin
  Result := True;
  with TDBDataSet(DataSet) do
  begin
    if (State in dsEditModes) or UpdatesPending then
    begin
      if Apply then
      begin
        Database.ApplyUpdates([DataSet as TDBDataSet]);
       { Always call CancelUpdates to remove any discard changes }
        CancelUpdates;
      end
      else
      begin
        if (MessageDlg('Unsaved changes, exit anyway?', mtConfirmation,
          [mbYes, mbCancel], 0) = mrYes) then
          CancelUpdates
        else
          Result := False;
      end;
    end;
  end;
end;

{ Determine the next available ItemNo for this order }

procedure TMastData.OrdersBeforeEdit(DataSet: TDataSet);
begin
  LastItemQuery.Close;
  LastItemQuery.Open;
  { SQL servers return Null for some aggregates if no items are present }
  with LastItemQuery.Fields[0] do
    if IsNull then FItemNo := 1
    else FItemNo := AsInteger + 1;
end;

procedure TMastData.EditUpdateError(DataSet: TDataSet; E: EDatabaseError;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
var
  Key: Variant;
const
  UpdErrMsg = '%s.'#13#10'Discard the edits to %S %S and continue updating?';
begin
  if UpdateKind = ukDelete then
    Key := Dataset.Fields[0].OldValue else
    Key := Dataset.Fields[0].NewValue;
  if MessageDlg(Format(UpdErrMsg, [E.Message, DataSet.Fields[0].DisplayLabel, Key]),
    mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
    UpdateAction := uaSkip else
    UpdateAction := uaAbort;
end;

end.

