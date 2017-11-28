object IWUserSession: TIWUserSession
  OldCreateOrder = False
  Height = 372
  Width = 529
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 43
    Top = 27
  end
  object DepartmentTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM DEPARTMENT')
    Left = 43
    Top = 83
    object DepartmentTableDEPT_NO: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'DEPT_NO'
      Origin = 'DEPT_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
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
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 176
    Top = 80
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 176
    Top = 24
  end
end
