object DataCacheDM: TDataCacheDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 301
  Width = 418
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 168
    Top = 144
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 168
    Top = 200
  end
  object DataCache: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 56
    Top = 144
  end
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 56
    Top = 32
  end
  object DepartmentTable: TFDQuery
    Connection = EmployeeConnection
    FetchOptions.AssignedValues = [evMode, evUnidirectional]
    FetchOptions.Mode = fmAll
    FetchOptions.Unidirectional = True
    SQL.Strings = (
      'SELECT * FROM DEPARTMENT ORDER BY DEPT_NO')
    Left = 56
    Top = 88
    object DepartmentTableDEPT_NO: TStringField
      FieldName = 'DEPT_NO'
      Origin = 'DEPT_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      FixedChar = True
      Size = 3
    end
    object DepartmentTableDEPARTMENT: TStringField
      FieldName = 'DEPARTMENT'
      Origin = 'DEPARTMENT'
      Required = True
      Size = 25
    end
    object DepartmentTableHEAD_DEPT: TStringField
      FieldName = 'HEAD_DEPT'
      Origin = 'HEAD_DEPT'
      FixedChar = True
      Size = 3
    end
    object DepartmentTableMNGR_NO: TSmallintField
      FieldName = 'MNGR_NO'
      Origin = 'MNGR_NO'
    end
    object DepartmentTableBUDGET: TBCDField
      FieldName = 'BUDGET'
      Origin = 'BUDGET'
      Precision = 18
      Size = 2
    end
    object DepartmentTableLOCATION: TStringField
      FieldName = 'LOCATION'
      Origin = 'LOCATION'
      Size = 15
    end
    object DepartmentTablePHONE_NO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'PHONE_NO'
      Origin = 'PHONE_NO'
    end
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 168
    Top = 32
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 168
    Top = 88
  end
  object FDEventAlerter1: TFDEventAlerter
    Connection = EmployeeConnection
    Names.Strings = (
      'DEPTO_UPDATED')
    Options.Timeout = 10000
    Options.Synchronize = False
    Options.AutoRegister = True
    OnAlert = FDEventAlerter1Alert
    Left = 56
    Top = 200
  end
end
