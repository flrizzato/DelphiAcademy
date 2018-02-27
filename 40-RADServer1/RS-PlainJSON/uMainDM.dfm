object RSPlainJSONResource1: TRSPlainJSONResource1
  OldCreateOrder = False
  Height = 440
  Width = 503
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    Connected = True
    LoginPrompt = False
    Left = 64
    Top = 24
  end
  object CustomerTable: TFDQuery
    CachedUpdates = True
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM CUSTOMER')
    Left = 64
    Top = 80
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
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 288
    Top = 24
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 184
    Top = 24
  end
end
