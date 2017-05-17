object ClientModule2: TClientModule2
  OldCreateOrder = False
  Height = 369
  Width = 624
  object DSRestCnn: TDSRestConnection
    Port = 8080
    LoginPrompt = False
    Left = 56
    Top = 24
    UniqueId = '{28F9CEF3-146B-47A4-AA5B-2836DB3C80CA}'
  end
  object CustomerMemTable: TFDMemTable
    FieldDefs = <>
    CachedUpdates = True
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvPersistent, rvSilentMode]
    ResourceOptions.Persistent = True
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 56
    Top = 80
    object CustomerMemTableCUST_NO: TFDAutoIncField
      FieldName = 'CUST_NO'
      Origin = 'CUST_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      IdentityInsert = True
    end
    object CustomerMemTableCUSTOMER: TStringField
      FieldName = 'CUSTOMER'
      Origin = 'CUSTOMER'
      Required = True
      Size = 25
    end
    object CustomerMemTableCONTACT_FIRST: TStringField
      FieldName = 'CONTACT_FIRST'
      Origin = 'CONTACT_FIRST'
      Size = 15
    end
    object CustomerMemTableCONTACT_LAST: TStringField
      FieldName = 'CONTACT_LAST'
      Origin = 'CONTACT_LAST'
    end
    object CustomerMemTablePHONE_NO: TStringField
      FieldName = 'PHONE_NO'
      Origin = 'PHONE_NO'
    end
    object CustomerMemTableADDRESS_LINE1: TStringField
      FieldName = 'ADDRESS_LINE1'
      Origin = 'ADDRESS_LINE1'
      Size = 30
    end
    object CustomerMemTableADDRESS_LINE2: TStringField
      FieldName = 'ADDRESS_LINE2'
      Origin = 'ADDRESS_LINE2'
      Size = 30
    end
    object CustomerMemTableCITY: TStringField
      FieldName = 'CITY'
      Origin = 'CITY'
      Size = 25
    end
    object CustomerMemTableSTATE_PROVINCE: TStringField
      FieldName = 'STATE_PROVINCE'
      Origin = 'STATE_PROVINCE'
      Size = 15
    end
    object CustomerMemTableCOUNTRY: TStringField
      FieldName = 'COUNTRY'
      Origin = 'COUNTRY'
      Size = 15
    end
    object CustomerMemTablePOSTAL_CODE: TStringField
      FieldName = 'POSTAL_CODE'
      Origin = 'POSTAL_CODE'
      Size = 12
    end
    object CustomerMemTableON_HOLD: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'ON_HOLD'
      Origin = 'ON_HOLD'
      FixedChar = True
      Size = 1
    end
  end
  object SalesMemTable: TFDMemTable
    FieldDefs = <>
    CachedUpdates = True
    IndexDefs = <>
    IndexFieldNames = 'CUST_NO'
    MasterSource = DataSource1
    MasterFields = 'CUST_NO'
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvPersistent, rvSilentMode]
    ResourceOptions.Persistent = True
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 56
    Top = 136
    object SalesMemTablePO_NUMBER: TStringField
      FieldName = 'PO_NUMBER'
      Origin = 'PO_NUMBER'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      FixedChar = True
      Size = 8
    end
    object SalesMemTableCUST_NO: TIntegerField
      FieldName = 'CUST_NO'
      Origin = 'CUST_NO'
      Required = True
    end
    object SalesMemTableSALES_REP: TSmallintField
      FieldName = 'SALES_REP'
      Origin = 'SALES_REP'
    end
    object SalesMemTableORDER_STATUS: TStringField
      FieldName = 'ORDER_STATUS'
      Origin = 'ORDER_STATUS'
      Required = True
      Size = 7
    end
    object SalesMemTableORDER_DATE: TSQLTimeStampField
      FieldName = 'ORDER_DATE'
      Origin = 'ORDER_DATE'
      Required = True
    end
    object SalesMemTableSHIP_DATE: TSQLTimeStampField
      FieldName = 'SHIP_DATE'
      Origin = 'SHIP_DATE'
    end
    object SalesMemTableDATE_NEEDED: TSQLTimeStampField
      FieldName = 'DATE_NEEDED'
      Origin = 'DATE_NEEDED'
    end
    object SalesMemTablePAID: TStringField
      FieldName = 'PAID'
      Origin = 'PAID'
      FixedChar = True
      Size = 1
    end
    object SalesMemTableQTY_ORDERED: TIntegerField
      FieldName = 'QTY_ORDERED'
      Origin = 'QTY_ORDERED'
      Required = True
    end
    object SalesMemTableTOTAL_VALUE: TCurrencyField
      FieldName = 'TOTAL_VALUE'
      Origin = 'TOTAL_VALUE'
      Required = True
    end
    object SalesMemTableDISCOUNT: TSingleField
      FieldName = 'DISCOUNT'
      Origin = 'DISCOUNT'
      Required = True
    end
    object SalesMemTableITEM_TYPE: TStringField
      FieldName = 'ITEM_TYPE'
      Origin = 'ITEM_TYPE'
      Required = True
      Size = 12
    end
    object SalesMemTableAGED: TFMTBCDField
      FieldName = 'AGED'
      Origin = 'AGED'
      Precision = 18
      Size = 9
    end
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 152
    Top = 24
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 280
    Top = 24
  end
  object DataSource1: TDataSource
    DataSet = CustomerMemTable
    Left = 152
    Top = 80
  end
end
