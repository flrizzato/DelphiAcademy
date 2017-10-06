object ServerMethods1: TServerMethods1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 402
  Width = 671
  object FDCnn: TFDConnection
    Params.Strings = (
      'Database=C:\DelphiAcademy\34-MobileSync1\MASTSQL.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'Server=127.0.0.1'
      'SQLDialect='
      'Port=3050'
      'DriverID=IB')
    Left = 48
    Top = 24
  end
  object qryParts: TFDQuery
    Connection = FDCnn
    SQL.Strings = (
      'SELECT * FROM PARTS')
    Left = 48
    Top = 80
    object qryPartsPARTNO: TFloatField
      FieldName = 'PARTNO'
      Origin = 'PARTNO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryPartsVENDORNO: TFloatField
      FieldName = 'VENDORNO'
      Origin = 'VENDORNO'
      Required = True
    end
    object qryPartsDESCRIPTION: TStringField
      FieldName = 'DESCRIPTION'
      Origin = 'DESCRIPTION'
      Required = True
      Size = 30
    end
    object qryPartsONHAND: TFloatField
      FieldName = 'ONHAND'
      Origin = 'ONHAND'
    end
    object qryPartsONORDER: TFloatField
      FieldName = 'ONORDER'
      Origin = 'ONORDER'
    end
    object qryPartsCOST: TFloatField
      FieldName = 'COST'
      Origin = 'COST'
    end
    object qryPartsLISTPRICE: TFloatField
      FieldName = 'LISTPRICE'
      Origin = 'LISTPRICE'
    end
    object qryPartsLASTUPDATE: TSQLTimeStampField
      FieldName = 'LASTUPDATE'
      Origin = 'LASTUPDATE'
    end
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 136
    Top = 24
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 240
    Top = 24
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 360
    Top = 24
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 488
    Top = 24
  end
end
