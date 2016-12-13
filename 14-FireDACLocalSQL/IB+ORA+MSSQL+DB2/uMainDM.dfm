object MainDM: TMainDM
  OldCreateOrder = False
  Height = 361
  Width = 516
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 424
    Top = 8
  end
  object FDPhysOracleDriverLink1: TFDPhysOracleDriverLink
    VendorHome = 'OraClient11g_home2'
    Left = 424
    Top = 112
  end
  object FDPhysDB2DriverLink1: TFDPhysDB2DriverLink
    Left = 424
    Top = 232
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 424
    Top = 176
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 424
    Top = 56
  end
  object FDQuery1: TFDQuery
    LocalSQL = FDLocalSQL1
    Connection = FDIBServer
    SQL.Strings = (
      'SELECT'
      '    t.CUSTOMERID,'
      '    t.FIRSTNAME,'
      '    t.LASTNAME,'
      '    t.COMPANY,'
      '    t.ADDRESS,'
      '    t.CITY,'
      '    t.STATE,'
      '    t.COUNTRY,'
      '    t.POSTALCODE,'
      '    t.PHONE,'
      '    t.FAX,'
      '    t.EMAIL,'
      '    t.SUPPORTREPID'
      'FROM CUSTOMER t'
      'ORDER BY t.CUSTOMERID')
    Left = 40
    Top = 72
  end
  object FDQuery2: TFDQuery
    LocalSQL = FDLocalSQL1
    Connection = FDORA
    SQL.Strings = (
      'SELECT'
      '    t.CUSTOMERID,'
      '    t.FIRSTNAME,'
      '    t.LASTNAME,'
      '    t.COMPANY,'
      '    t.ADDRESS,'
      '    t.CITY,'
      '    t.STATE,'
      '    t.COUNTRY,'
      '    t.POSTALCODE,'
      '    t.PHONE,'
      '    t.FAX,'
      '    t.EMAIL,'
      '    t.SUPPORTREPID'
      'FROM CUSTOMER t'
      'ORDER BY t.CUSTOMERID')
    Left = 112
    Top = 72
  end
  object FDQuery3: TFDQuery
    LocalSQL = FDLocalSQL1
    Connection = FDMSSQL
    SQL.Strings = (
      'SELECT'
      '    t.CUSTOMERID,'
      '    t.FIRSTNAME,'
      '    t.LASTNAME,'
      '    t.COMPANY,'
      '    t.ADDRESS,'
      '    t.CITY,'
      '    t.STATE,'
      '    t.COUNTRY,'
      '    t.POSTALCODE,'
      '    t.PHONE,'
      '    t.FAX,'
      '    t.EMAIL,'
      '    t.SUPPORTREPID'
      'FROM CUSTOMER t'
      'ORDER BY t.CUSTOMERID')
    Left = 192
    Top = 72
  end
  object FDQuery4: TFDQuery
    LocalSQL = FDLocalSQL1
    Connection = FDDB2
    SQL.Strings = (
      'SELECT'
      '    t.CUSTOMERID,'
      '    t.FIRSTNAME,'
      '    t.LASTNAME,'
      '    t.COMPANY,'
      '    t.ADDRESS,'
      '    t.CITY,'
      '    t.STATE,'
      '    t.COUNTRY,'
      '    t.POSTALCODE,'
      '    t.PHONE,'
      '    t.FAX,'
      '    t.EMAIL,'
      '    t.SUPPORTREPID'
      'FROM CUSTOMER t'
      'ORDER BY t.CUSTOMERID')
    Left = 272
    Top = 72
  end
  object FDIBServer: TFDConnection
    ConnectionName = 'FDIBServer'
    Params.Strings = (
      'Database=C:\IB_XE7\FIREDAC.IB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IB')
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtInt32
      end
      item
        SourceDataType = dtWideString
        TargetDataType = dtAnsiString
      end
      item
        SourceDataType = dtDateTimeStamp
        TargetDataType = dtDateTime
      end
      item
        SourceDataType = dtDate
        TargetDataType = dtDateTime
      end>
    LoginPrompt = False
    Left = 40
    Top = 8
  end
  object FDMSSQL: TFDConnection
    ConnectionName = 'FDMSSQL'
    Params.Strings = (
      'Database=C:\IB_XE7\FIREDAC.IB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IB')
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtInt32
      end
      item
        SourceDataType = dtWideString
        TargetDataType = dtAnsiString
      end
      item
        SourceDataType = dtDateTimeStamp
        TargetDataType = dtDateTime
      end
      item
        SourceDataType = dtDate
        TargetDataType = dtDateTime
      end>
    LoginPrompt = False
    Left = 192
    Top = 8
  end
  object FDDB2: TFDConnection
    ConnectionName = 'FDDB2'
    Params.Strings = (
      'Database=C:\IB_XE7\FIREDAC.IB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IB')
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtInt32
      end
      item
        SourceDataType = dtWideString
        TargetDataType = dtAnsiString
      end
      item
        SourceDataType = dtDateTimeStamp
        TargetDataType = dtDateTime
      end
      item
        SourceDataType = dtDate
        TargetDataType = dtDateTime
      end>
    LoginPrompt = False
    Left = 272
    Top = 8
  end
  object FDSQLLite: TFDConnection
    ConnectionName = 'FDSQLLite'
    Params.Strings = (
      'DriverID=SQLite')
    Left = 40
    Top = 184
  end
  object FDLocalSQL1: TFDLocalSQL
    Connection = FDSQLLite
    DataSets = <
      item
        DataSet = FDQuery1
        Name = 'IBTable'
      end
      item
        DataSet = FDQuery2
        Name = 'ORATable'
      end
      item
        DataSet = FDQuery3
        Name = 'MSSQLTable'
      end
      item
        DataSet = FDQuery4
        Name = 'DB2Table'
      end>
    Left = 40
    Top = 240
  end
  object FDQuery5: TFDQuery
    Connection = FDSQLLite
    SQL.Strings = (
      'SELECT * FROM ('
      '    SELECT * FROM IBTABLE'
      '    UNION'
      '    SELECT * FROM ORATABLE'
      '    UNION'
      '    SELECT * FROM MSSQLTABLE'
      '    UNION'
      '    SELECT * FROM DB2TABLE'
      ') ORDER BY CUSTOMERID')
    Left = 40
    Top = 296
  end
  object FDORA: TFDConnection
    ConnectionName = 'FDORA'
    Params.Strings = (
      'Database=C:\IB_XE7\FIREDAC.IB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IB')
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtInt32
      end
      item
        SourceDataType = dtWideString
        TargetDataType = dtAnsiString
      end
      item
        SourceDataType = dtDateTimeStamp
        TargetDataType = dtDateTime
      end
      item
        SourceDataType = dtDate
        TargetDataType = dtDateTime
      end>
    LoginPrompt = False
    Left = 112
    Top = 8
  end
end
