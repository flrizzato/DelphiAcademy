object ClientModule1: TClientModule1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 271
  Width = 415
  object DSRestConnection1: TDSRestConnection
    Port = 8080
    LoginPrompt = False
    Left = 48
    Top = 16
    UniqueId = '{9B0A954B-A1A3-48F2-8654-0D4850897768}'
  end
  object FDCnn: TFDConnection
    Params.Strings = (
      'Database=C:\DelphiAcademy\34-MobileSync1\MASTSQL_MOBILE.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IBLite')
    Left = 48
    Top = 88
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 136
    Top = 88
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 240
    Top = 88
  end
  object qryPARTS: TFDQuery
    CachedUpdates = True
    Connection = FDCnn
    UpdateOptions.AssignedValues = [uvAutoCommitUpdates]
    UpdateOptions.AutoCommitUpdates = True
    UpdateOptions.KeyFields = 'PARTNO'
    SQL.Strings = (
      'SELECT * FROM PARTS ORDER BY PARTNO')
    Left = 48
    Top = 144
    object qryPARTSPARTNO: TFloatField
      FieldName = 'PARTNO'
      Origin = 'PARTNO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryPARTSVENDORNO: TFloatField
      FieldName = 'VENDORNO'
      Origin = 'VENDORNO'
      Required = True
    end
    object qryPARTSDESCRIPTION: TStringField
      FieldName = 'DESCRIPTION'
      Origin = 'DESCRIPTION'
      Required = True
      Size = 30
    end
    object qryPARTSONHAND: TFloatField
      FieldName = 'ONHAND'
      Origin = 'ONHAND'
    end
    object qryPARTSONORDER: TFloatField
      FieldName = 'ONORDER'
      Origin = 'ONORDER'
    end
    object qryPARTSCOST: TFloatField
      FieldName = 'COST'
      Origin = 'COST'
    end
    object qryPARTSLISTPRICE: TFloatField
      FieldName = 'LISTPRICE'
      Origin = 'LISTPRICE'
    end
    object qryPARTSLASTUPDATE: TSQLTimeStampField
      FieldName = 'LASTUPDATE'
      Origin = 'LASTUPDATE'
    end
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 160
    Top = 16
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 280
    Top = 16
  end
end
