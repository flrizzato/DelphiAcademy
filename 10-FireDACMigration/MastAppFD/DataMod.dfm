object MastData: TMastData
  OldCreateOrder = True
  Height = 337
  Width = 409
  object Database: TFDConnection
    ConnectionName = 'MAST'
    Params.Strings = (
      'Database=C:\DB_INTERBASE\MASTSQL.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=127.0.0.1'
      'CharacterSet=UTF8'
      'DriverID=IB')
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        PrecMax = 10
        PrecMin = 0
        ScaleMax = 0
        ScaleMin = 0
        SourceDataType = dtFmtBCD
        TargetDataType = dtInt32
      end
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtDouble
      end
      item
        SourceDataType = dtDateTimeStamp
        TargetDataType = dtDateTime
      end>
    TxOptions.Isolation = xiDirtyRead
    LoginPrompt = False
    BeforeConnect = DatabaseBeforeConnect
    Left = 332
    Top = 252
  end
  object NextCust: TFDTable
    ConnectionName = 'MAST'
    UpdateOptions.UpdateTableName = 'NEXTCUST'
    Exclusive = True
    TableName = 'NEXTCUST'
    Left = 156
    Top = 60
    object NextCustNewCust: TFloatField
      FieldName = 'NewCust'
    end
  end
  object Parts: TFDTable
    BeforeOpen = PartsBeforeOpen
    OnCalcFields = PartsCalcFields
    CachedUpdates = True
    OnUpdateError = PartsUpdateError
    ConnectionName = 'MAST'
    UpdateOptions.UpdateTableName = 'PARTS'
    TableName = 'PARTS'
    Left = 20
    Top = 157
    object PartsPartNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 8
      FieldName = 'PartNo'
      Required = True
    end
    object PartsDescription: TStringField
      DisplayWidth = 21
      FieldName = 'Description'
      Required = True
      Size = 30
    end
    object PartsVendorNo: TFloatField
      DisplayWidth = 9
      FieldName = 'VendorNo'
    end
    object PartsOnHand: TFloatField
      DisplayWidth = 9
      FieldName = 'OnHand'
    end
    object PartsOnOrder: TFloatField
      DisplayWidth = 10
      FieldName = 'OnOrder'
    end
    object PartsBackOrd: TBooleanField
      DisplayWidth = 9
      FieldKind = fkCalculated
      FieldName = 'BackOrd'
      DisplayValues = 'Yes;No'
      Calculated = True
    end
    object PartsCost: TCurrencyField
      DisplayWidth = 12
      FieldName = 'Cost'
    end
    object PartsListPrice: TCurrencyField
      DisplayWidth = 12
      FieldName = 'ListPrice'
    end
  end
  object PartsSource: TDataSource
    DataSet = Parts
    Left = 88
    Top = 157
  end
  object PartsQuery: TFDQuery
    OnCalcFields = PartsQueryCalcFields
    ConnectionName = 'MAST'
    SQL.Strings = (
      'select * from parts'
      ' where (parts.OnOrder > parts.OnHand)'
      '')
    Left = 156
    Top = 157
    object PartsQueryPartNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 8
      FieldName = 'PartNo'
    end
    object PartsQueryDescription: TStringField
      DisplayWidth = 21
      FieldName = 'Description'
      Size = 30
    end
    object PartsQueryVendorNo: TFloatField
      FieldName = 'VendorNo'
    end
    object PartsQueryOnHand: TFloatField
      DisplayWidth = 9
      FieldName = 'OnHand'
    end
    object PartsQueryOnOrder: TFloatField
      DisplayWidth = 10
      FieldName = 'OnOrder'
    end
    object PartsQueryBackOrd: TBooleanField
      DisplayWidth = 9
      FieldKind = fkCalculated
      FieldName = 'BackOrd'
      DisplayValues = 'Yes;No'
      Calculated = True
    end
    object PartsQueryCost: TCurrencyField
      FieldName = 'Cost'
    end
    object PartsQueryListPrice: TCurrencyField
      FieldName = 'ListPrice'
    end
  end
  object VendorSource: TDataSource
    DataSet = Vendors
    Left = 90
    Top = 252
  end
  object Vendors: TFDTable
    ConnectionName = 'MAST'
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate]
    UpdateOptions.EnableDelete = False
    UpdateOptions.EnableInsert = False
    UpdateOptions.EnableUpdate = False
    UpdateOptions.UpdateTableName = 'VENDORS'
    TableName = 'VENDORS'
    Left = 20
    Top = 252
  end
  object Orders: TFDTable
    BeforeOpen = OrdersBeforeOpen
    BeforeClose = OrdersBeforeClose
    BeforeInsert = OrdersBeforeInsert
    BeforeEdit = OrdersBeforeEdit
    AfterPost = OrdersAfterPost
    BeforeCancel = OrdersBeforeCancel
    AfterCancel = OrdersAfterCancel
    BeforeDelete = OrdersBeforeDelete
    AfterDelete = OrdersAfterDelete
    OnCalcFields = OrdersCalcFields
    OnNewRecord = OrdersNewRecord
    CachedUpdates = True
    IndexFieldNames = 'OrderNo'
    ConnectionName = 'MAST'
    UpdateOptions.UpdateTableName = 'ORDERS'
    TableName = 'ORDERS'
    Left = 20
    Top = 12
    object OrdersOrderNo: TFloatField
      Alignment = taLeftJustify
      FieldName = 'OrderNo'
    end
    object OrdersCustNo: TFloatField
      Alignment = taLeftJustify
      FieldName = 'CustNo'
      Required = True
      OnChange = OrdersCustNoChange
    end
    object OrdersSaleDate: TDateTimeField
      FieldName = 'SaleDate'
      OnValidate = OrdersSaleDateValidate
    end
    object OrdersShipDate: TDateTimeField
      FieldName = 'ShipDate'
    end
    object OrdersShipToContact: TStringField
      FieldName = 'ShipToContact'
    end
    object OrdersShipToAddr1: TStringField
      FieldName = 'ShipToAddr1'
      Size = 30
    end
    object OrdersShipToAddr2: TStringField
      FieldName = 'ShipToAddr2'
      Size = 30
    end
    object OrdersShipToCity: TStringField
      FieldName = 'ShipToCity'
      Size = 15
    end
    object OrdersShipToState: TStringField
      FieldName = 'ShipToState'
    end
    object OrdersShipToZip: TStringField
      FieldName = 'ShipToZip'
      Size = 10
    end
    object OrdersShipToCountry: TStringField
      FieldName = 'ShipToCountry'
    end
    object OrdersShipToPhone: TStringField
      FieldName = 'ShipToPhone'
      Size = 15
    end
    object OrdersShipVIA: TStringField
      FieldName = 'ShipVIA'
      Size = 7
    end
    object OrdersPO: TStringField
      FieldName = 'PO'
      Size = 15
    end
    object OrdersEmpNo: TIntegerField
      FieldName = 'EmpNo'
      Required = True
    end
    object OrdersTerms: TStringField
      FieldName = 'Terms'
      Size = 6
    end
    object OrdersPaymentMethod: TStringField
      FieldName = 'PaymentMethod'
      Size = 7
    end
    object OrdersItemsTotal: TCurrencyField
      FieldName = 'ItemsTotal'
    end
    object OrdersTaxRate: TFloatField
      FieldName = 'TaxRate'
      DisplayFormat = '0.00%;(0.00%);0%'
      MaxValue = 100.000000000000000000
    end
    object OrdersTaxTotal: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'TaxTotal'
      Calculated = True
    end
    object OrdersFreight: TCurrencyField
      FieldName = 'Freight'
      OnValidate = OrdersFreightValidate
    end
    object OrdersAmountPaid: TCurrencyField
      FieldName = 'AmountPaid'
    end
    object OrdersAmountDue: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'AmountDue'
      Calculated = True
    end
    object OrdersSalesPerson: TStringField
      FieldKind = fkLookup
      FieldName = 'SalesPerson'
      LookupDataSet = Emps
      LookupKeyFields = 'EmpNo'
      LookupResultField = 'FirstName'
      KeyFields = 'EmpNo'
      Size = 40
      Lookup = True
    end
  end
  object OrdersSource: TDataSource
    DataSet = Orders
    Left = 86
    Top = 12
  end
  object CustByOrd: TFDTable
    IndexFieldNames = 'CustNo'
    MasterSource = OrdersSource
    MasterFields = 'CustNo'
    ConnectionName = 'MAST'
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate]
    UpdateOptions.EnableDelete = False
    UpdateOptions.EnableInsert = False
    UpdateOptions.EnableUpdate = False
    UpdateOptions.UpdateTableName = 'CUSTOMER'
    TableName = 'CUSTOMER'
    Left = 20
    Top = 60
    object CustByOrdCustNo: TFloatField
      FieldName = 'CustNo'
    end
    object CustByOrdCompany: TStringField
      FieldName = 'Company'
      Size = 30
    end
    object CustByOrdAddr1: TStringField
      FieldName = 'Addr1'
      Size = 30
    end
    object CustByOrdAddr2: TStringField
      FieldName = 'Addr2'
      Size = 30
    end
    object CustByOrdCity: TStringField
      FieldName = 'City'
      Size = 15
    end
    object CustByOrdState: TStringField
      FieldName = 'State'
    end
    object CustByOrdZip: TStringField
      FieldName = 'Zip'
      Size = 10
    end
    object CustByOrdCountry: TStringField
      FieldName = 'Country'
    end
    object CustByOrdPhone: TStringField
      FieldName = 'Phone'
      Size = 15
    end
    object CustByOrdFAX: TStringField
      FieldName = 'FAX'
      Size = 15
    end
    object CustByOrdTaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object CustByOrdContact: TStringField
      FieldName = 'Contact'
    end
    object CustByOrdLastInvoiceDate: TDateTimeField
      FieldName = 'LastInvoiceDate'
    end
  end
  object CustByOrdSrc: TDataSource
    DataSet = CustByOrd
    Left = 86
    Top = 60
  end
  object Items: TFDTable
    BeforeOpen = ItemsBeforeOpen
    BeforeInsert = EnsureOrdersEdit
    BeforeEdit = ItemsBeforeEdit
    BeforePost = ItemsBeforePost
    AfterPost = ItemsAfterPost
    BeforeDelete = EnsureOrdersEdit
    AfterDelete = ItemsAfterDelete
    OnCalcFields = ItemsCalcFields
    OnNewRecord = ItemsNewRecord
    CachedUpdates = True
    IndexFieldNames = 'OrderNo'
    MasterSource = OrdersSource
    MasterFields = 'OrderNo'
    ConnectionName = 'MAST'
    UpdateOptions.UpdateTableName = 'ITEMS'
    TableName = 'ITEMS'
    Left = 20
    Top = 108
    object ItemsItemNo: TFloatField
      FieldName = 'ItemNo'
      Visible = False
    end
    object ItemsOrderNo: TFloatField
      FieldName = 'OrderNo'
      Visible = False
    end
    object ItemsPartNo: TFloatField
      FieldName = 'PartNo'
      OnValidate = ItemsPartNoValidate
    end
    object ItemsDescription: TStringField
      DisplayWidth = 28
      FieldKind = fkLookup
      FieldName = 'Description'
      LookupDataSet = Parts
      LookupKeyFields = 'PartNo'
      LookupResultField = 'Description'
      KeyFields = 'PartNo'
      ReadOnly = True
      Size = 30
      Lookup = True
    end
    object ItemsSellPrice: TCurrencyField
      DisplayWidth = 9
      FieldKind = fkLookup
      FieldName = 'SellPrice'
      LookupDataSet = Parts
      LookupKeyFields = 'PartNo'
      LookupResultField = 'ListPrice'
      KeyFields = 'PartNo'
      Lookup = True
    end
    object ItemsQty: TIntegerField
      DisplayWidth = 5
      FieldName = 'Qty'
      OnValidate = ItemsQtyValidate
    end
    object ItemsDiscount: TFloatField
      DisplayWidth = 7
      FieldName = 'Discount'
      DisplayFormat = '0.00%'
      MaxValue = 100.000000000000000000
    end
    object ItemsExtPrice: TCurrencyField
      DisplayWidth = 10
      FieldKind = fkCalculated
      FieldName = 'ExtPrice'
      Calculated = True
    end
  end
  object ItemsSource: TDataSource
    DataSet = Items
    Left = 86
    Top = 108
  end
  object NextOrd: TFDTable
    ConnectionName = 'MAST'
    UpdateOptions.UpdateTableName = 'NEXTORD'
    Exclusive = True
    TableName = 'NEXTORD'
    Left = 155
    Top = 12
    object NextOrdNewKey: TFloatField
      FieldName = 'NewKey'
    end
  end
  object Emps: TFDTable
    OnCalcFields = EmpsCalcFields
    IndexFieldNames = 'EmpNo'
    ConnectionName = 'MAST'
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate]
    UpdateOptions.EnableDelete = False
    UpdateOptions.EnableInsert = False
    UpdateOptions.EnableUpdate = False
    UpdateOptions.UpdateTableName = 'EMPLOYEE'
    TableName = 'EMPLOYEE'
    Left = 20
    Top = 204
    object EmpsEmpNo: TIntegerField
      FieldName = 'EmpNo'
    end
    object EmpsFullName: TStringField
      FieldKind = fkCalculated
      FieldName = 'FullName'
      Calculated = True
    end
    object EmpsLastName: TStringField
      FieldName = 'LastName'
    end
    object EmpsFirstName: TStringField
      FieldName = 'FirstName'
      Size = 15
    end
    object EmpsPhoneExt: TStringField
      FieldName = 'PhoneExt'
      Size = 4
    end
    object EmpsHireDate: TDateTimeField
      FieldName = 'HireDate'
    end
    object EmpsSalary: TFloatField
      FieldName = 'Salary'
    end
  end
  object EmpsSource: TDataSource
    DataSet = Emps
    Left = 89
    Top = 204
  end
  object LastItemQuery: TFDQuery
    MasterSource = OrdersSource
    MasterFields = 'OrderNo'
    ConnectionName = 'MAST'
    SQL.Strings = (
      'select max(ItemNo)  from Items'
      '  where OrderNo = :OrderNo')
    Left = 155
    Top = 108
    ParamData = <
      item
        Name = 'OrderNo'
        DataType = ftFloat
      end>
  end
  object Cust: TFDTable
    BeforeOpen = CustBeforeOpen
    BeforePost = CustBeforePost
    CachedUpdates = True
    ConnectionName = 'MAST'
    UpdateOptions.UpdateTableName = 'CUSTOMER'
    TableName = 'CUSTOMER'
    Left = 236
    Top = 13
    object CustCustNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 6
      FieldName = 'CustNo'
    end
    object CustCompany: TStringField
      DisplayWidth = 26
      FieldName = 'Company'
      Required = True
      Size = 30
    end
    object CustPhone: TStringField
      DisplayWidth = 15
      FieldName = 'Phone'
      Size = 15
    end
    object CustLastInvoiceDate: TDateTimeField
      DisplayLabel = 'LastInvoice'
      DisplayWidth = 10
      FieldName = 'LastInvoiceDate'
      DisplayFormat = 'DDDDD'
    end
    object CustAddr1: TStringField
      FieldName = 'Addr1'
      Size = 30
    end
    object CustAddr2: TStringField
      FieldName = 'Addr2'
      Size = 30
    end
    object CustCity: TStringField
      FieldName = 'City'
      Size = 15
    end
    object CustState: TStringField
      FieldName = 'State'
    end
    object CustZip: TStringField
      FieldName = 'Zip'
      Size = 10
    end
    object CustCountry: TStringField
      FieldName = 'Country'
    end
    object CustFAX: TStringField
      FieldName = 'FAX'
      Size = 15
    end
    object CustTaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object CustContact: TStringField
      FieldName = 'Contact'
    end
  end
  object CustSource: TDataSource
    DataSet = Cust
    Left = 312
    Top = 13
  end
  object CustQuery: TFDQuery
    ConnectionName = 'MAST'
    SQL.Strings = (
      'select Customer.CustNo, Customer.Company, '
      '         Customer.LastInvoiceDate, Customer.Phone '
      '  from customer'
      '  where '
      '    (customer.LastInvoiceDate >= :FromDate) and'
      '    (customer.LastInvoiceDate <= :ToDate)')
    Left = 236
    Top = 61
    ParamData = <
      item
        Name = 'FromDate'
      end
      item
        Name = 'ToDate'
      end>
    object CustQueryCustNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 6
      FieldName = 'CustNo'
    end
    object CustQueryCompany: TStringField
      DisplayWidth = 26
      FieldName = 'Company'
      Size = 30
    end
    object CustQueryPhone: TStringField
      DisplayWidth = 15
      FieldName = 'Phone'
      Size = 15
    end
    object CustQueryLastInvoiceDate: TDateTimeField
      DisplayLabel = 'LastInvoice'
      DisplayWidth = 10
      FieldName = 'LastInvoiceDate'
      DisplayFormat = 'DDDDD'
    end
  end
  object OrdByCustSrc: TDataSource
    DataSet = OrdByCust
    Left = 312
    Top = 108
  end
  object OrdByCust: TFDTable
    OnCalcFields = OrdByCustCalcFields
    IndexFieldNames = 'CUSTNO'
    MasterSource = CustMasterSrc
    MasterFields = 'CUSTNO'
    ConnectionName = 'MAST'
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate]
    UpdateOptions.EnableDelete = False
    UpdateOptions.EnableInsert = False
    UpdateOptions.EnableUpdate = False
    UpdateOptions.UpdateTableName = 'ORDERS'
    TableName = 'ORDERS'
    Left = 236
    Top = 108
    object OrdByCustOrderNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 10
      FieldName = 'OrderNo'
    end
    object OrdByCustCustNo: TFloatField
      FieldName = 'CustNo'
      Visible = False
    end
    object OrdByCustSaleDate: TDateTimeField
      DisplayWidth = 9
      FieldName = 'SaleDate'
      DisplayFormat = 'DDDDD'
    end
    object OrdByCustShipDate: TDateTimeField
      DisplayWidth = 9
      FieldName = 'ShipDate'
      DisplayFormat = 'DDDDD'
    end
    object OrdByCustItemsTotal: TCurrencyField
      FieldName = 'ItemsTotal'
      Visible = False
    end
    object OrdByCustTaxRate: TFloatField
      FieldName = 'TaxRate'
      Visible = False
    end
    object OrdByCustFreight: TCurrencyField
      FieldName = 'Freight'
      Visible = False
    end
    object OrdByCustAmountPaid: TCurrencyField
      DisplayWidth = 14
      FieldName = 'AmountPaid'
    end
    object OrdByCustAmountDue: TCurrencyField
      DisplayWidth = 14
      FieldKind = fkCalculated
      FieldName = 'AmountDue'
      Calculated = True
    end
  end
  object CustMasterSrc: TDataSource
    DataSet = Cust
    Left = 312
    Top = 60
  end
  object CustByComp: TFDTable
    IndexFieldNames = 'Company'
    ConnectionName = 'MAST'
    UpdateOptions.UpdateTableName = 'CUSTOMER'
    TableName = 'CUSTOMER'
    Left = 236
    Top = 156
  end
  object CustByCompSrc: TDataSource
    DataSet = CustByComp
    Left = 312
    Top = 156
  end
  object CustByLastInvQuery: TFDQuery
    ConnectionName = 'MAST'
    SQL.Strings = (
      'select * from customer'
      'order by'
      'LastInvoiceDate DESC')
    Left = 240
    Top = 208
    object CustByLastInvQueryCustNo: TFloatField
      FieldName = 'CustNo'
    end
    object CustByLastInvQueryCompany: TStringField
      FieldName = 'Company'
      Size = 30
    end
    object CustByLastInvQueryAddr1: TStringField
      FieldName = 'Addr1'
      Size = 30
    end
    object CustByLastInvQueryAddr2: TStringField
      FieldName = 'Addr2'
      Size = 30
    end
    object CustByLastInvQueryCity: TStringField
      FieldName = 'City'
      Size = 15
    end
    object CustByLastInvQueryState: TStringField
      FieldName = 'State'
    end
    object CustByLastInvQueryZip: TStringField
      FieldName = 'Zip'
      Size = 10
    end
    object CustByLastInvQueryCountry: TStringField
      FieldName = 'Country'
    end
    object CustByLastInvQueryPhone: TStringField
      FieldName = 'Phone'
      Size = 15
    end
    object CustByLastInvQueryFAX: TStringField
      FieldName = 'FAX'
      Size = 15
    end
    object CustByLastInvQueryTaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object CustByLastInvQueryContact: TStringField
      FieldName = 'Contact'
    end
    object CustByLastInvQueryLastInvoiceDate: TDateTimeField
      FieldName = 'LastInvoiceDate'
    end
  end
  object OrdersByDateQuery: TFDQuery
    ConnectionName = 'MAST'
    SQL.Strings = (
      'select * from orders'
      'where '
      '    (SaleDate >= :FromDate) and'
      '    (SaleDate <= :ToDate)'
      'order by '
      '  SaleDate')
    Left = 240
    Top = 256
    ParamData = <
      item
        Name = 'FromDate'
        DataType = ftDate
        Value = 34700d
      end
      item
        Name = 'ToDate'
        DataType = ftDate
        Value = 35488d
      end>
    object OrdersByDateQueryOrderNo: TFloatField
      FieldName = 'OrderNo'
    end
    object OrdersByDateQueryCustNo: TFloatField
      FieldName = 'CustNo'
    end
    object OrdersByDateQuerySaleDate: TDateTimeField
      FieldName = 'SaleDate'
    end
    object OrdersByDateQueryShipDate: TDateTimeField
      FieldName = 'ShipDate'
    end
    object OrdersByDateQueryEmpNo: TIntegerField
      FieldName = 'EmpNo'
    end
    object OrdersByDateQueryShipToContact: TStringField
      FieldName = 'ShipToContact'
    end
    object OrdersByDateQueryShipToAddr1: TStringField
      FieldName = 'ShipToAddr1'
      Size = 30
    end
    object OrdersByDateQueryShipToAddr2: TStringField
      FieldName = 'ShipToAddr2'
      Size = 30
    end
    object OrdersByDateQueryShipToCity: TStringField
      FieldName = 'ShipToCity'
      Size = 15
    end
    object OrdersByDateQueryShipToState: TStringField
      FieldName = 'ShipToState'
    end
    object OrdersByDateQueryShipToZip: TStringField
      FieldName = 'ShipToZip'
      Size = 10
    end
    object OrdersByDateQueryShipToCountry: TStringField
      FieldName = 'ShipToCountry'
    end
    object OrdersByDateQueryShipToPhone: TStringField
      FieldName = 'ShipToPhone'
      Size = 15
    end
    object OrdersByDateQueryShipVIA: TStringField
      FieldName = 'ShipVIA'
      Size = 7
    end
    object OrdersByDateQueryPO: TStringField
      FieldName = 'PO'
      Size = 15
    end
    object OrdersByDateQueryTerms: TStringField
      FieldName = 'Terms'
      Size = 6
    end
    object OrdersByDateQueryPaymentMethod: TStringField
      FieldName = 'PaymentMethod'
      Size = 7
    end
    object OrdersByDateQueryItemsTotal: TCurrencyField
      FieldName = 'ItemsTotal'
    end
    object OrdersByDateQueryTaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object OrdersByDateQueryFreight: TCurrencyField
      FieldName = 'Freight'
    end
    object OrdersByDateQueryAmountPaid: TCurrencyField
      FieldName = 'AmountPaid'
    end
    object OrdersByDateQueryCompany: TStringField
      FieldKind = fkLookup
      FieldName = 'Company'
      LookupDataSet = Cust
      LookupKeyFields = 'CustNo'
      LookupResultField = 'Company'
      KeyFields = 'CustNo'
      Size = 40
      Lookup = True
    end
  end
end
