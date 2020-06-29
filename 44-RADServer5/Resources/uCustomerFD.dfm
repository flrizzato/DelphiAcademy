object RSResource1: TRSResource1
  OldCreateOrder = False
  Height = 327
  Width = 399
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\Public\Documents\Embarcadero\Studio\21.0\Sampl' +
        'es\Data\EMPLOYEE.GDB'
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 64
    Top = 24
  end
  object CustomerTable: TFDQuery
    CachedUpdates = True
    Connection = EmployeeConnection
    SchemaAdapter = FDSchemaAdapter1
    SQL.Strings = (
      'SELECT * FROM CUSTOMER')
    Left = 64
    Top = 80
  end
  object FDSchemaAdapter1: TFDSchemaAdapter
    Left = 64
    Top = 136
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 288
    Top = 32
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 184
    Top = 32
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 64
    Top = 192
  end
end
