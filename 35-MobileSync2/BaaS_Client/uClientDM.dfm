object ClientDM: TClientDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 374
  Width = 435
  object FDCnn: TFDConnection
    Params.Strings = (
      'Database=C:\DelphiAcademy\35-MobileSync2\MASTSQL_MOBILE.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IBLite')
    LoginPrompt = False
    BeforeConnect = FDCnnBeforeConnect
    Left = 32
    Top = 208
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 120
    Top = 208
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 224
    Top = 208
  end
  object qryPARTS: TFDQuery
    Connection = FDCnn
    UpdateOptions.AssignedValues = [uvAutoCommitUpdates]
    UpdateOptions.AutoCommitUpdates = True
    UpdateOptions.KeyFields = 'PARTNO'
    SQL.Strings = (
      'SELECT * FROM PARTS ORDER BY PARTNO')
    Left = 32
    Top = 264
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
    object qryPARTSOBJECTID: TStringField
      FieldName = 'OBJECTID'
      Origin = 'OBJECTID'
      Size = 100
    end
  end
  object ParseProvider1: TParseProvider
    ApiVersion = '1'
    AndroidPush.BlankID = True
    Left = 43
    Top = 24
  end
  object BackendQuery1: TBackendQuery
    Provider = ParseProvider1
    BackendClassName = 'parts'
    BackendService = 'Storage'
    QueryLines.Strings = (
      
        'where={"updatedAt":{"$gte":{ "__type": "Date", "iso": "2017-10-1' +
        '8T00:00:00.000Z" }}}')
    Left = 128
    Top = 24
  end
  object memDataSet: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 128
    Top = 128
  end
  object RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter
    Dataset = memDataSet
    FieldDefs = <>
    ResponseJSON = BackendQuery1
    Left = 128
    Top = 72
  end
end
