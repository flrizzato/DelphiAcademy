object Form9: TForm9
  Left = 0
  Top = 0
  Caption = 'Delphi 10 Seattle MongoDB LocalSQL Example'
  ClientHeight = 501
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    526
    501)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 35
    Width = 29
    Height = 13
    Caption = 'Match'
  end
  object Label2: TLabel
    Left = 16
    Top = 62
    Width = 20
    Height = 13
    Caption = 'Sort'
  end
  object Label3: TLabel
    Left = 16
    Top = 89
    Width = 48
    Height = 13
    Caption = 'Projection'
  end
  object Label4: TLabel
    Left = 16
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Database'
  end
  object Label5: TLabel
    Left = 232
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Collection'
  end
  object EditMatch: TEdit
    Left = 80
    Top = 32
    Width = 354
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = '{"address.zipcode":"11229", cuisine:{$ne:"Italian"}}'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 232
    Width = 510
    Height = 261
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dsQuery
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ButtonOpenMongo: TButton
    Left = 440
    Top = 3
    Width = 75
    Height = 50
    Anchors = [akTop, akRight]
    Caption = 'Open Mongo'
    Default = True
    TabOrder = 2
    OnClick = ButtonOpenMongoClick
  end
  object EditSort: TEdit
    Left = 80
    Top = 59
    Width = 354
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = '{cuisine:-1}'
  end
  object EditProjection: TEdit
    Left = 80
    Top = 86
    Width = 354
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = '{grades:0, address:0}'
  end
  object EditSQL: TMemo
    Left = 8
    Top = 112
    Width = 510
    Height = 114
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'select * from MongoDB')
    TabOrder = 5
  end
  object ButtonOpenSQL: TButton
    Left = 440
    Top = 59
    Width = 75
    Height = 48
    Anchors = [akTop, akRight]
    Caption = 'Open SQL'
    TabOrder = 6
    OnClick = ButtonOpenSQLClick
  end
  object EditDatabase: TEdit
    Left = 80
    Top = 5
    Width = 137
    Height = 21
    TabOrder = 7
    Text = 'test'
  end
  object EditCollection: TEdit
    Left = 296
    Top = 5
    Width = 137
    Height = 21
    TabOrder = 8
    Text = 'restaurants'
  end
  object FDMongoQuery1: TFDMongoQuery
    FormatOptions.AssignedValues = [fvStrsTrim2Len]
    FormatOptions.StrsTrim2Len = True
    UpdateOptions.KeyFields = '_id'
    Connection = FDConnection1
    DatabaseName = 'test'
    CollectionName = 'restaurants'
    Left = 272
    Top = 136
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=Mongo')
    LoginPrompt = False
    Left = 184
    Top = 136
  end
  object dsQuery: TDataSource
    Left = 232
    Top = 215
  end
  object FDLocalSQLConnection: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 184
    Top = 272
  end
  object FDLocalSQL1: TFDLocalSQL
    IncludeHiddenFields = False
    Connection = FDLocalSQLConnection
    DataSets = <
      item
        DataSet = FDMongoQuery1
        Name = 'MongoDB'
      end>
    Left = 272
    Top = 272
  end
  object FDQuery1: TFDQuery
    LocalSQL = FDLocalSQL1
    Connection = FDLocalSQLConnection
    SQL.Strings = (
      'select * from MongoDB')
    Left = 336
    Top = 272
  end
end
