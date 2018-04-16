object CustomerResource: TCustomerResource
  OldCreateOrder = False
  Height = 440
  Width = 503
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 64
    Top = 24
  end
  object CustomerTable: TFDQuery
    CachedUpdates = True
    Connection = EmployeeConnection
    FetchOptions.AssignedValues = [evMode, evUnidirectional]
    FetchOptions.Mode = fmAll
    FetchOptions.Unidirectional = True
    UpdateObject = FDUpdateSQLCustomer
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
  object FDUpdateSQLCustomer: TFDUpdateSQL
    Connection = EmployeeConnection
    InsertSQL.Strings = (
      'INSERT INTO CUSTOMER'
      '(CUSTOMER, CONTACT_FIRST, CONTACT_LAST, PHONE_NO, '
      '  ADDRESS_LINE1, ADDRESS_LINE2, CITY, STATE_PROVINCE, '
      '  COUNTRY, POSTAL_CODE, ON_HOLD)'
      
        'VALUES (:NEW_CUSTOMER, :NEW_CONTACT_FIRST, :NEW_CONTACT_LAST, :N' +
        'EW_PHONE_NO, '
      
        '  :NEW_ADDRESS_LINE1, :NEW_ADDRESS_LINE2, :NEW_CITY, :NEW_STATE_' +
        'PROVINCE, '
      '  :NEW_COUNTRY, :NEW_POSTAL_CODE, :NEW_ON_HOLD)')
    ModifySQL.Strings = (
      'UPDATE CUSTOMER'
      
        'SET CUST_NO = :NEW_CUST_NO, CUSTOMER = :NEW_CUSTOMER, CONTACT_FI' +
        'RST = :NEW_CONTACT_FIRST, '
      '  CONTACT_LAST = :NEW_CONTACT_LAST, PHONE_NO = :NEW_PHONE_NO, '
      
        '  ADDRESS_LINE1 = :NEW_ADDRESS_LINE1, ADDRESS_LINE2 = :NEW_ADDRE' +
        'SS_LINE2, '
      '  CITY = :NEW_CITY, STATE_PROVINCE = :NEW_STATE_PROVINCE, '
      '  COUNTRY = :NEW_COUNTRY, POSTAL_CODE = :NEW_POSTAL_CODE, '
      '  ON_HOLD = :NEW_ON_HOLD'
      'WHERE CUST_NO = :OLD_CUST_NO')
    DeleteSQL.Strings = (
      'DELETE FROM CUSTOMER'
      'WHERE CUST_NO = :OLD_CUST_NO')
    FetchRowSQL.Strings = (
      
        'SELECT CUST_NO, CUSTOMER, CONTACT_FIRST, CONTACT_LAST, PHONE_NO,' +
        ' ADDRESS_LINE1, '
      '  ADDRESS_LINE2, CITY, STATE_PROVINCE, COUNTRY, POSTAL_CODE, '
      '  ON_HOLD'
      'FROM CUSTOMER'
      'WHERE CUST_NO = :CUST_NO')
    Left = 64
    Top = 136
  end
end
