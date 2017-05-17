object ClientModule1: TClientModule1
  OldCreateOrder = False
  Height = 354
  Width = 506
  object SQLConnection1: TSQLConnection
    DriverName = 'DataSnap'
    LoginPrompt = False
    Params.Strings = (
      'DriverUnit=Data.DBXDataSnap'
      'HostName=localhost'
      
        'DriverAssemblyLoader=Borland.Data.TDBXClientDriverLoader,Borland' +
        '.Data.DbxClientDriver,Version=24.0.0.0,Culture=neutral,PublicKey' +
        'Token=91d62ebb5b0d1b1b'
      'Port=8080'
      'CommunicationProtocol=http'
      'DatasnapContext=datasnap/'
      'Filters={}')
    Left = 48
    Top = 40
  end
  object CustomerTable: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'CustomerProvider'
    RemoteServer = DSProviderConnection1
    Left = 48
    Top = 152
    object CustomerTableCUST_NO: TAutoIncField
      FieldName = 'CUST_NO'
      Origin = 'CUST_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
    end
    object CustomerTableCUSTOMER: TStringField
      FieldName = 'CUSTOMER'
      Origin = 'CUSTOMER'
      Required = True
      Size = 25
    end
    object CustomerTableCONTACT_FIRST: TStringField
      FieldName = 'CONTACT_FIRST'
      Origin = 'CONTACT_FIRST'
      Size = 15
    end
    object CustomerTableCONTACT_LAST: TStringField
      FieldName = 'CONTACT_LAST'
      Origin = 'CONTACT_LAST'
    end
    object CustomerTablePHONE_NO: TStringField
      FieldName = 'PHONE_NO'
      Origin = 'PHONE_NO'
    end
    object CustomerTableADDRESS_LINE1: TStringField
      FieldName = 'ADDRESS_LINE1'
      Origin = 'ADDRESS_LINE1'
      Size = 30
    end
    object CustomerTableADDRESS_LINE2: TStringField
      FieldName = 'ADDRESS_LINE2'
      Origin = 'ADDRESS_LINE2'
      Size = 30
    end
    object CustomerTableCITY: TStringField
      FieldName = 'CITY'
      Origin = 'CITY'
      Size = 25
    end
    object CustomerTableSTATE_PROVINCE: TStringField
      FieldName = 'STATE_PROVINCE'
      Origin = 'STATE_PROVINCE'
      Size = 15
    end
    object CustomerTableCOUNTRY: TStringField
      FieldName = 'COUNTRY'
      Origin = 'COUNTRY'
      Size = 15
    end
    object CustomerTablePOSTAL_CODE: TStringField
      FieldName = 'POSTAL_CODE'
      Origin = 'POSTAL_CODE'
      Size = 12
    end
    object CustomerTableON_HOLD: TStringField
      FieldName = 'ON_HOLD'
      Origin = 'ON_HOLD'
      FixedChar = True
      Size = 1
    end
    object CustomerTableSalesTable: TDataSetField
      FieldName = 'SalesTable'
    end
  end
  object DSProviderConnection1: TDSProviderConnection
    ServerClassName = 'TServerMethods1'
    SQLConnection = SQLConnection1
    Left = 48
    Top = 96
  end
  object SalesTable: TClientDataSet
    Aggregates = <>
    DataSetField = CustomerTableSalesTable
    Params = <>
    Left = 128
    Top = 152
    object SalesTablePO_NUMBER: TStringField
      FieldName = 'PO_NUMBER'
      Required = True
      FixedChar = True
      Size = 8
    end
    object SalesTableCUST_NO: TIntegerField
      FieldName = 'CUST_NO'
      Required = True
    end
    object SalesTableSALES_REP: TSmallintField
      FieldName = 'SALES_REP'
    end
    object SalesTableORDER_STATUS: TStringField
      FieldName = 'ORDER_STATUS'
      Required = True
      Size = 7
    end
    object SalesTableORDER_DATE: TSQLTimeStampField
      FieldName = 'ORDER_DATE'
      Required = True
    end
    object SalesTableSHIP_DATE: TSQLTimeStampField
      FieldName = 'SHIP_DATE'
    end
    object SalesTableDATE_NEEDED: TSQLTimeStampField
      FieldName = 'DATE_NEEDED'
    end
    object SalesTablePAID: TStringField
      FieldName = 'PAID'
      FixedChar = True
      Size = 1
    end
    object SalesTableQTY_ORDERED: TIntegerField
      FieldName = 'QTY_ORDERED'
      Required = True
    end
    object SalesTableTOTAL_VALUE: TCurrencyField
      FieldName = 'TOTAL_VALUE'
      Required = True
    end
    object SalesTableDISCOUNT: TSingleField
      FieldName = 'DISCOUNT'
      Required = True
    end
    object SalesTableITEM_TYPE: TStringField
      FieldName = 'ITEM_TYPE'
      Required = True
      Size = 12
    end
    object SalesTableAGED: TFMTBCDField
      FieldName = 'AGED'
      Precision = 18
      Size = 9
    end
  end
end
