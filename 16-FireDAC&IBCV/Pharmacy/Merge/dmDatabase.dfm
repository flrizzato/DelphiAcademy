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
    Connected = True
    Transaction = FDTransaction1
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
    Connection = FDConnection
    SQL.Strings = (
      'Select * FROM CATEGORY')
    Left = 72
    Top = 136
  end
  object qryMedicine: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    Connection = FDConnection
    SQL.Strings = (
      'SELECT * FROM MEDICINE')
    Left = 72
    Top = 192
  end
  object qryMedicineCategories: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    Connection = FDConnection
    SQL.Strings = (
      'SELECT * FROM MEDICINE_CATEGORIES')
    Left = 72
    Top = 248
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 240
    Top = 144
  end
  object FDTransaction1: TFDTransaction
    Options.Isolation = xiSnapshot
    Connection = FDConnection
    Left = 144
    Top = 88
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    ListServers = False
    Left = 240
    Top = 200
  end
  object FDGUIxLoginDialog1: TFDGUIxLoginDialog
    Provider = 'Forms'
    Left = 320
    Top = 72
  end
end
