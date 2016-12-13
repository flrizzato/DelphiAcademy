object frmMain: TfrmMain
  Left = 0
  Top = 0
  ActiveControl = DBGrid1
  Caption = 'MongoDB Explore Demo'
  ClientHeight = 413
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    505
    413)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 55
    Height = 13
    Caption = 'Databases:'
  end
  object Label2: TLabel
    Left = 247
    Top = 8
    Width = 55
    Height = 13
    Caption = 'Collections:'
  end
  object Label3: TLabel
    Left = 8
    Top = 205
    Width = 27
    Height = 13
    Caption = 'Data:'
  end
  object Label4: TLabel
    Left = 39
    Top = 205
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 27
    Width = 233
    Height = 172
    DataSource = dsDatabases
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid2: TDBGrid
    Left = 247
    Top = 27
    Width = 250
    Height = 172
    Anchors = [akLeft, akTop, akRight]
    DataSource = dsCollections
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid3: TDBGrid
    Left = 8
    Top = 224
    Width = 489
    Height = 181
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dsData
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=172.16.121.240'
      'User_Name=test'
      'Password=test'
      'DriverID=Mongo')
    Left = 88
    Top = 288
  end
  object dsDatabases: TDataSource
    DataSet = mdDatabases
    OnDataChange = dsDatabasesDataChange
    Left = 136
    Top = 80
  end
  object dsCollections: TDataSource
    DataSet = mdCollections
    OnDataChange = dsCollectionsDataChange
    Left = 384
    Top = 80
  end
  object dsData: TDataSource
    DataSet = mqData
    OnDataChange = dsDataDataChange
    Left = 312
    Top = 216
  end
  object mdDatabases: TFDMongoDataSet
    FormatOptions.AssignedValues = [fvStrsTrim2Len]
    FormatOptions.StrsTrim2Len = True
    Connection = FDConnection1
    Left = 48
    Top = 80
  end
  object mdCollections: TFDMongoDataSet
    FormatOptions.AssignedValues = [fvStrsTrim2Len]
    FormatOptions.StrsTrim2Len = True
    Connection = FDConnection1
    Left = 296
    Top = 80
  end
  object mqData: TFDMongoQuery
    FormatOptions.AssignedValues = [fvStrsTrim2Len]
    FormatOptions.StrsTrim2Len = True
    Connection = FDConnection1
    Left = 208
    Top = 216
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 192
    Top = 288
  end
  object FDPhysMongoDriverLink1: TFDPhysMongoDriverLink
    Left = 304
    Top = 288
  end
end
