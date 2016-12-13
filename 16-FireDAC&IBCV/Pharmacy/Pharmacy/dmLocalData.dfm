object dtmdLocalDB: TdtmdLocalDB
  OldCreateOrder = False
  Height = 417
  Width = 616
  object FDConnection: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      'Database=C:\Data\PHARMACY.IB'
      'DriverID=IB')
    ConnectedStoredUsage = [auDesignTime]
    LoginPrompt = False
    AfterConnect = FDConnectionAfterConnect
    BeforeDisconnect = FDConnectionBeforeDisconnect
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
    Connection = FDConnection
    SQL.Strings = (
      'Select * FROM CATEGORY'
      'ORDER BY CATEGORY_NAME')
    Left = 72
    Top = 104
    object qryCategoryCATEGORY_ID: TIntegerField
      FieldName = 'CATEGORY_ID'
      Origin = 'CATEGORY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryCategoryCATEGORY_NAME: TWideStringField
      FieldName = 'CATEGORY_NAME'
      Origin = 'CATEGORY_NAME'
      Required = True
      Size = 200
    end
    object qryCategoryDESCRIPTION: TMemoField
      FieldName = 'DESCRIPTION'
      Origin = 'DESCRIPTION'
      BlobType = ftMemo
    end
  end
  object qryMedicines: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    MasterSource = dsCategory
    MasterFields = 'CATEGORY_ID'
    Connection = FDConnection
    FetchOptions.AssignedValues = [evCache]
    FetchOptions.Cache = [fiBlobs, fiMeta]
    SQL.Strings = (
      'SELECT M.* FROM MEDICINE_CATEGORIES MC'
      'inner join MEDICINE M ON M.MEDICINE_ID = MC.MEDICINE_ID'
      '  WHERE MC.CATEGORY_ID = :CATEGORY_ID'
      'ORDER BY M.MEDICINE_NAME')
    Left = 72
    Top = 224
    ParamData = <
      item
        Name = 'CATEGORY_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = 1
      end>
    object qryMedicinesMEDICINE_ID: TIntegerField
      FieldName = 'MEDICINE_ID'
      Origin = 'MEDICINE_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryMedicinesMEDICINE_NAME: TWideStringField
      FieldName = 'MEDICINE_NAME'
      Origin = 'MEDICINE_NAME'
      Required = True
      Size = 400
    end
    object qryMedicinesURL: TWideStringField
      FieldName = 'URL'
      Origin = 'URL'
      Size = 1020
    end
    object qryMedicinesPATIENT_ADVICE: TMemoField
      FieldName = 'PATIENT_ADVICE'
      Origin = 'PATIENT_ADVICE'
      BlobType = ftMemo
    end
    object qryMedicinesSPECIAL_WARNINGS: TMemoField
      FieldName = 'SPECIAL_WARNINGS'
      Origin = 'SPECIAL_WARNINGS'
      BlobType = ftMemo
    end
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 264
    Top = 32
  end
  object dsPharmacy: TDataSource
    DataSet = qryPharmacy
    Left = 336
    Top = 120
  end
  object qryPharmacy: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    Connection = FDConnection
    SQL.Strings = (
      'SELECT * FROM PHARMACY')
    Left = 272
    Top = 112
    object qryPharmacyPHARMACY_ID: TIntegerField
      FieldName = 'PHARMACY_ID'
      Origin = 'PHARMACY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object qryPharmacyPHARMACY_ADDRESS: TMemoField
      FieldName = 'PHARMACY_ADDRESS'
      Origin = 'PHARMACY_ADDRESS'
      BlobType = ftMemo
    end
    object qryPharmacyPHARMACY_NAME: TWideStringField
      FieldName = 'PHARMACY_NAME'
      Origin = 'PHARMACY_NAME'
      Size = 400
    end
  end
  object dsCategory: TDataSource
    DataSet = qryCategory
    Left = 72
    Top = 160
  end
  object qryCustomer: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    AfterInsert = qryCustomerAfterInsert
    MasterSource = dsPharmacy
    MasterFields = 'PHARMACY_ID'
    DetailFields = 'PHARMACY_ID'
    Connection = FDConnection
    FetchOptions.AssignedValues = [evCache]
    FetchOptions.Cache = [fiBlobs, fiMeta]
    SQL.Strings = (
      'SELECT * FROM CUSTOMERS'
      'WHERE PHARMACY_ID = :PHARMACY_ID')
    Left = 336
    Top = 184
    ParamData = <
      item
        Name = 'PHARMACY_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = 1
      end>
    object qryCustomerCUSTOMER_ID: TIntegerField
      FieldName = 'CUSTOMER_ID'
      Origin = 'CUSTOMER_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object qryCustomerPHARMACY_ID: TIntegerField
      FieldName = 'PHARMACY_ID'
      Origin = 'PHARMACY_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object qryCustomerCUSTOMER_NAME: TWideStringField
      FieldName = 'CUSTOMER_NAME'
      Origin = 'CUSTOMER_NAME'
      Size = 400
    end
    object qryCustomerCUSTOMER_ADDRESS: TMemoField
      FieldName = 'CUSTOMER_ADDRESS'
      Origin = 'CUSTOMER_ADDRESS'
      BlobType = ftMemo
    end
  end
  object qryOrders: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    MasterSource = dsCustomer
    MasterFields = 'CUSTOMER_ID;PHARMACY_ID'
    DetailFields = 'CUSTOMER_ID;PHARMACY_ID'
    Connection = FDConnection
    FetchOptions.AssignedValues = [evCache]
    FetchOptions.Cache = [fiBlobs, fiMeta]
    SQL.Strings = (
      
        'SELECT * FROM ORDERS WHERE CUSTOMER_ID = :CUSTOMER_ID AND PHARMA' +
        'CY_ID = :PHARMACY_ID')
    Left = 408
    Top = 240
    ParamData = <
      item
        Name = 'CUSTOMER_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = 1
      end
      item
        Name = 'PHARMACY_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = 1
      end>
    object qryOrdersCUSTOMER_ID: TIntegerField
      FieldName = 'CUSTOMER_ID'
      Origin = 'CUSTOMER_ID'
    end
    object qryOrdersPHARMACY_ID: TIntegerField
      FieldName = 'PHARMACY_ID'
      Origin = 'PHARMACY_ID'
    end
    object qryOrdersMEDICINE_ID: TIntegerField
      FieldName = 'MEDICINE_ID'
      Origin = 'MEDICINE_ID'
    end
    object qryOrdersDATE_TIME: TSQLTimeStampField
      FieldName = 'DATE_TIME'
      Origin = 'DATE_TIME'
    end
    object qryOrdersQUANTITY: TIntegerField
      FieldName = 'QUANTITY'
      Origin = 'QUANTITY'
    end
    object qryOrdersMEDICINE_NAME: TStringField
      FieldKind = fkLookup
      FieldName = 'MEDICINE_NAME'
      LookupDataSet = qryMedLookup
      LookupKeyFields = 'MEDICINE_ID'
      LookupResultField = 'MEDICINE_NAME'
      KeyFields = 'MEDICINE_ID'
      Size = 100
      Lookup = True
    end
  end
  object dsCustomer: TDataSource
    DataSet = qryCustomer
    Left = 400
    Top = 184
  end
  object qryMedLookup: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'Select MEDICINE_ID, MEDICINE_NAME FROM MEDICINE')
    Left = 120
    Top = 304
  end
  object spGetID: TFDStoredProc
    Connection = FDConnection
    StoredProcName = 'GETID'
    Left = 280
    Top = 304
    ParamData = <
      item
        Position = 1
        Name = 'NEWID'
        DataType = ftInteger
        ParamType = ptOutput
      end>
    object spGetIDNEWID: TIntegerField
      FieldName = 'NEWID'
      Origin = 'NEWID'
    end
  end
  object dsOrders: TDataSource
    DataSet = qryOrders
    Left = 480
    Top = 240
  end
  object FDConnectionRemote: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      'Database=C:\Data\PHARMACY.IB'
      'DriverID=IB')
    ConnectedStoredUsage = [auDesignTime]
    Connected = True
    LoginPrompt = False
    AfterConnect = FDConnectionAfterConnect
    BeforeDisconnect = FDConnectionBeforeDisconnect
    Left = 488
    Top = 24
  end
end
