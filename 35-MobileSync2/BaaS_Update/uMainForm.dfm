object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'BaaS Updater'
  ClientHeight = 369
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 800
    Height = 347
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PARTNO'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'VENDORNO'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DESCRIPTION'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ONHAND'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ONORDER'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'COST'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'LISTPRICE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OBJECTID'
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 347
    Width = 800
    Height = 22
    DataSource = DataSource1
    Align = alBottom
    TabOrder = 1
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=C:\DelphiAcademy\35-MobileSync2\MASTSQL.GDB'
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      'Port=3050'
      'DriverID=IB')
    LoginPrompt = False
    Left = 48
    Top = 56
  end
  object FDQuery1: TFDQuery
    AfterPost = FDQuery1AfterPost
    CachedUpdates = True
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM PARTS')
    Left = 48
    Top = 104
    object FDQuery1PARTNO: TFloatField
      FieldName = 'PARTNO'
      Origin = 'PARTNO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object FDQuery1VENDORNO: TFloatField
      FieldName = 'VENDORNO'
      Origin = 'VENDORNO'
      Required = True
    end
    object FDQuery1DESCRIPTION: TStringField
      FieldName = 'DESCRIPTION'
      Origin = 'DESCRIPTION'
      Required = True
      Size = 30
    end
    object FDQuery1ONHAND: TFloatField
      FieldName = 'ONHAND'
      Origin = 'ONHAND'
    end
    object FDQuery1ONORDER: TFloatField
      FieldName = 'ONORDER'
      Origin = 'ONORDER'
    end
    object FDQuery1COST: TFloatField
      FieldName = 'COST'
      Origin = 'COST'
    end
    object FDQuery1LISTPRICE: TFloatField
      FieldName = 'LISTPRICE'
      Origin = 'LISTPRICE'
    end
    object FDQuery1OBJECTID: TStringField
      FieldName = 'OBJECTID'
      Origin = 'OBJECTID'
      Size = 100
    end
  end
  object DataSource1: TDataSource
    DataSet = FDQuery1
    Left = 48
    Top = 152
  end
  object ParseProvider1: TParseProvider
    ApiVersion = '1'
    AndroidPush.InstallationID = '56B9ED42-2F1F-40D8-8E4B-A870B33E15FB'
    Left = 136
    Top = 56
  end
  object BackendStorage1: TBackendStorage
    Provider = ParseProvider1
    Left = 136
    Top = 112
  end
end
