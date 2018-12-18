object RadserveraResource1: TRadserveraResource1
  OldCreateOrder = False
  Height = 329
  Width = 562
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtDateTimeStamp
        TargetDataType = dtDateTime
      end
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtDouble
      end>
    Connected = True
    LoginPrompt = False
    Left = 56
    Top = 16
  end
  object CustomerTable: TFDQuery
    Connection = EmployeeConnection
    UpdateObject = FDUpdateSQL1
    SQL.Strings = (
      'SELECT * FROM CUSTOMER')
    Left = 56
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
    Left = 168
    Top = 16
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 280
    Top = 16
  end
  object EMSDataSetResource1: TEMSDataSetResource
    AllowedActions = [List, Get, Post, Put, Delete]
    DataSet = CustomerTable
    KeyFields = 'CUST_NO'
    MappingMode = rmEntityToRecord
    PageParamName = 'pg'
    PageSize = 5
    Left = 240
    Top = 80
  end
  object FDUpdateSQL1: TFDUpdateSQL
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
    Left = 136
    Top = 80
  end
  object SalesTable: TFDQuery
    Connection = EmployeeConnection
    UpdateObject = FDUpdateSQL2
    SQL.Strings = (
      'SELECT * FROM SALES WHERE CUST_NO = :CUST_NO')
    Left = 56
    Top = 136
    ParamData = <
      item
        Name = 'CUST_NO'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
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
      AutoGenerateValue = arDefault
      FieldName = 'ORDER_STATUS'
      Origin = 'ORDER_STATUS'
      Size = 7
    end
    object SalesTableORDER_DATE: TDateTimeField
      AutoGenerateValue = arDefault
      FieldName = 'ORDER_DATE'
      Origin = 'ORDER_DATE'
    end
    object SalesTableSHIP_DATE: TDateTimeField
      FieldName = 'SHIP_DATE'
      Origin = 'SHIP_DATE'
    end
    object SalesTableDATE_NEEDED: TDateTimeField
      FieldName = 'DATE_NEEDED'
      Origin = 'DATE_NEEDED'
    end
    object SalesTablePAID: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'PAID'
      Origin = 'PAID'
      FixedChar = True
      Size = 1
    end
    object SalesTableQTY_ORDERED: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'QTY_ORDERED'
      Origin = 'QTY_ORDERED'
    end
    object SalesTableTOTAL_VALUE: TCurrencyField
      FieldName = 'TOTAL_VALUE'
      Origin = 'TOTAL_VALUE'
      Required = True
    end
    object SalesTableDISCOUNT: TSingleField
      AutoGenerateValue = arDefault
      FieldName = 'DISCOUNT'
      Origin = 'DISCOUNT'
    end
    object SalesTableITEM_TYPE: TStringField
      FieldName = 'ITEM_TYPE'
      Origin = 'ITEM_TYPE'
      Required = True
      Size = 12
    end
    object SalesTableAGED: TFloatField
      AutoGenerateValue = arDefault
      FieldName = 'AGED'
      Origin = 'AGED'
      ProviderFlags = []
      ReadOnly = True
    end
  end
  object EMSDataSetResource2: TEMSDataSetResource
    AllowedActions = [List]
    DataSet = SalesTable
    KeyFields = 'CUST_NO'
    Left = 240
    Top = 136
  end
  object FDUpdateSQL2: TFDUpdateSQL
    Connection = EmployeeConnection
    InsertSQL.Strings = (
      'INSERT INTO SALES'
      '(PO_NUMBER, CUST_NO, SALES_REP, ORDER_STATUS, '
      '  ORDER_DATE, SHIP_DATE, DATE_NEEDED, PAID, '
      '  QTY_ORDERED, TOTAL_VALUE, DISCOUNT, ITEM_TYPE)'
      
        'VALUES (:NEW_PO_NUMBER, :NEW_CUST_NO, :NEW_SALES_REP, :NEW_ORDER' +
        '_STATUS, '
      '  :NEW_ORDER_DATE, :NEW_SHIP_DATE, :NEW_DATE_NEEDED, :NEW_PAID, '
      
        '  :NEW_QTY_ORDERED, :NEW_TOTAL_VALUE, :NEW_DISCOUNT, :NEW_ITEM_T' +
        'YPE)')
    ModifySQL.Strings = (
      'UPDATE SALES'
      
        'SET PO_NUMBER = :NEW_PO_NUMBER, CUST_NO = :NEW_CUST_NO, SALES_RE' +
        'P = :NEW_SALES_REP, '
      
        '  ORDER_STATUS = :NEW_ORDER_STATUS, ORDER_DATE = :NEW_ORDER_DATE' +
        ', '
      '  SHIP_DATE = :NEW_SHIP_DATE, DATE_NEEDED = :NEW_DATE_NEEDED, '
      
        '  PAID = :NEW_PAID, QTY_ORDERED = :NEW_QTY_ORDERED, TOTAL_VALUE ' +
        '= :NEW_TOTAL_VALUE, '
      '  DISCOUNT = :NEW_DISCOUNT, ITEM_TYPE = :NEW_ITEM_TYPE'
      'WHERE PO_NUMBER = :OLD_PO_NUMBER')
    DeleteSQL.Strings = (
      'DELETE FROM SALES'
      'WHERE PO_NUMBER = :OLD_PO_NUMBER')
    FetchRowSQL.Strings = (
      
        'SELECT PO_NUMBER, CUST_NO, SALES_REP, ORDER_STATUS, ORDER_DATE, ' +
        'SHIP_DATE, '
      
        '  DATE_NEEDED, PAID, QTY_ORDERED, TOTAL_VALUE, DISCOUNT, ITEM_TY' +
        'PE, '
      '  AGED'
      'FROM SALES'
      'WHERE PO_NUMBER = :PO_NUMBER')
    Left = 136
    Top = 136
  end
end
