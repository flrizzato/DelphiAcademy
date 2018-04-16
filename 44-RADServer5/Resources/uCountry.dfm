object CountryResource1: TCountryResource1
  OldCreateOrder = False
  Height = 284
  Width = 390
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 56
    Top = 32
  end
  object CountryTable: TFDQuery
    CachedUpdates = True
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM COUNTRY')
    Left = 56
    Top = 80
    object CountryTableCOUNTRY: TStringField
      FieldName = 'COUNTRY'
      Origin = 'COUNTRY'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 15
    end
    object CountryTableCURRENCY: TStringField
      FieldName = 'CURRENCY'
      Origin = 'CURRENCY'
      Required = True
      Size = 10
    end
  end
  object FDUpdateSQLCountry: TFDUpdateSQL
    Connection = EmployeeConnection
    InsertSQL.Strings = (
      'INSERT INTO COUNTRY'
      '(CURRENCY)'
      'VALUES (:NEW_CURRENCY)')
    ModifySQL.Strings = (
      'UPDATE COUNTRY'
      'SET CURRENCY = :NEW_CURRENCY'
      'WHERE COUNTRY = :OLD_COUNTRY')
    DeleteSQL.Strings = (
      'DELETE FROM COUNTRY'
      'WHERE COUNTRY = :OLD_COUNTRY')
    FetchRowSQL.Strings = (
      'SELECT COUNTRY, CURRENCY'
      'FROM COUNTRY'
      'WHERE COUNTRY = :COUNTRY')
    Left = 56
    Top = 136
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 264
    Top = 32
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 160
    Top = 32
  end
end
