object ServerMethods1: TServerMethods1
  OldCreateOrder = False
  Height = 367
  Width = 461
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'Protocol=TCPIP'
      
        'Database=C:\Users\Public\Documents\Embarcadero\Studio\19.0\Sampl' +
        'es\Data\EMPLOYEE.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IB')
    LoginPrompt = False
    BeforeConnect = EmployeeConnectionBeforeConnect
    Left = 49
    Top = 24
  end
  object CustomerTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM CUSTOMER ORDER BY CUST_NO')
    Left = 49
    Top = 76
    object CustomerTableCUST_NO: TFDAutoIncField
      FieldName = 'CUST_NO'
      Origin = 'CUST_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      IdentityInsert = True
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
      AutoGenerateValue = arDefault
      FieldName = 'ON_HOLD'
      Origin = 'ON_HOLD'
      FixedChar = True
      Size = 1
    end
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 160
    Top = 24
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 264
    Top = 24
  end
  object SalesTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM SALES ORDER BY CUST_NO, PO_NUMBER')
    Left = 48
    Top = 128
    object SalesTablePO_NUMBER: TStringField
      FieldName = 'PO_NUMBER'
      Origin = 'PO_NUMBER'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      FixedChar = True
      Size = 8
    end
    object SalesTableCUST_NO: TIntegerField
      FieldName = 'CUST_NO'
      Origin = 'CUST_NO'
      Required = True
    end
    object SalesTableSALES_REP: TSmallintField
      FieldName = 'SALES_REP'
      Origin = 'SALES_REP'
    end
    object SalesTableORDER_STATUS: TStringField
      FieldName = 'ORDER_STATUS'
      Origin = 'ORDER_STATUS'
      Required = True
      Size = 7
    end
    object SalesTableORDER_DATE: TSQLTimeStampField
      FieldName = 'ORDER_DATE'
      Origin = 'ORDER_DATE'
      Required = True
    end
    object SalesTableSHIP_DATE: TSQLTimeStampField
      FieldName = 'SHIP_DATE'
      Origin = 'SHIP_DATE'
    end
    object SalesTableDATE_NEEDED: TSQLTimeStampField
      FieldName = 'DATE_NEEDED'
      Origin = 'DATE_NEEDED'
    end
    object SalesTablePAID: TStringField
      FieldName = 'PAID'
      Origin = 'PAID'
      FixedChar = True
      Size = 1
    end
    object SalesTableQTY_ORDERED: TIntegerField
      FieldName = 'QTY_ORDERED'
      Origin = 'QTY_ORDERED'
      Required = True
    end
    object SalesTableTOTAL_VALUE: TCurrencyField
      FieldName = 'TOTAL_VALUE'
      Origin = 'TOTAL_VALUE'
      Required = True
    end
    object SalesTableDISCOUNT: TSingleField
      FieldName = 'DISCOUNT'
      Origin = 'DISCOUNT'
      Required = True
    end
    object SalesTableITEM_TYPE: TStringField
      FieldName = 'ITEM_TYPE'
      Origin = 'ITEM_TYPE'
      Required = True
      Size = 12
    end
    object SalesTableAGED: TFMTBCDField
      FieldName = 'AGED'
      Origin = 'AGED'
      Precision = 18
      Size = 9
    end
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 264
    Top = 72
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 256
    Top = 128
  end
end
