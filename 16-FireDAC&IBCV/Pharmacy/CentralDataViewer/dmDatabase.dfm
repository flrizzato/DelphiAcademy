object dtmdDatabase: TdtmdDatabase
  OldCreateOrder = False
  Height = 327
  Width = 507
  object FDConnection: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      'Database=C:\Data\MEDICINES.IB'
      'DriverID=IB')
    ConnectedStoredUsage = [auDesignTime]
    LoginPrompt = False
    AfterConnect = FDConnectionAfterConnect
    Left = 48
    Top = 24
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 144
    Top = 24
  end
  object qryCategory: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    OnNewRecord = qryCategoryNewRecord
    IndexFieldNames = 'CATEGORY_ID'
    Connection = FDConnection
    SQL.Strings = (
      'Select * FROM CATEGORY')
    Left = 72
    Top = 136
  end
  object qryMedicine: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    OnNewRecord = qryMedicineNewRecord
    Connection = FDConnection
    SQL.Strings = (
      'SELECT * FROM MEDICINE')
    Left = 72
    Top = 192
  end
  object qryMedicineCategories: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    OnNewRecord = qryMedicineCategoriesNewRecord
    IndexFieldNames = 'CATEGORY_ID'
    MasterSource = dsCategory
    Connection = FDConnection
    SQL.Strings = (
      'SELECT * FROM MEDICINE_CATEGORIES'
      'WHERE CATEGORY_ID = :CATEGORY_ID')
    Left = 152
    Top = 264
    ParamData = <
      item
        Name = 'CATEGORY_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = Null
      end>
    object qryMedicineCategoriesCATEGORY_ID: TIntegerField
      FieldName = 'CATEGORY_ID'
      Origin = 'CATEGORY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object qryMedicineCategoriesMEDICINE_ID: TIntegerField
      FieldName = 'MEDICINE_ID'
      Origin = 'MEDICINE_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object qryMedicineCategoriesMEDICINE_NAME: TStringField
      FieldKind = fkLookup
      FieldName = 'MEDICINE_NAME'
      LookupDataSet = qryMedicine
      LookupKeyFields = 'MEDICINE_ID'
      LookupResultField = 'MEDICINE_NAME'
      KeyFields = 'MEDICINE_ID'
      Size = 100
      Lookup = True
    end
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 240
    Top = 144
  end
  object FDGUIxLoginDialog1: TFDGUIxLoginDialog
    Provider = 'Forms'
    Left = 320
    Top = 72
  end
  object FDStoredProc1: TFDStoredProc
    Connection = FDConnection
    StoredProcName = 'GETID'
    Left = 352
    Top = 232
    ParamData = <
      item
        Position = 1
        Name = 'NEWID'
        DataType = ftInteger
        ParamType = ptOutput
      end>
    object FDStoredProc1NEWID: TIntegerField
      FieldName = 'NEWID'
      Origin = 'NEWID'
    end
  end
  object dsCategory: TDataSource
    DataSet = qryCategory
    Left = 152
    Top = 208
  end
end
