object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Delphi 10 Seattle Geospatial Queries Sample'
  ClientHeight = 477
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    690
    477)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 33
    Width = 32
    Height = 13
    Caption = 'Origin:'
  end
  object Label2: TLabel
    Left = 17
    Top = 53
    Width = 16
    Height = 13
    Caption = 'Min'
  end
  object Label3: TLabel
    Left = 17
    Top = 73
    Width = 20
    Height = 13
    Caption = 'Max'
  end
  object Label4: TLabel
    Left = 223
    Top = 60
    Width = 33
    Height = 13
    Caption = 'Meters'
  end
  object Label5: TLabel
    Left = 87
    Top = 11
    Width = 93
    Height = 13
    Caption = 'Latitude, Longitude'
  end
  object EditOrigin: TEdit
    Left = 64
    Top = 30
    Width = 153
    Height = 21
    TabOrder = 0
    Text = '-73.958885, 40.7745559'
  end
  object EditMin: TEdit
    Left = 64
    Top = 50
    Width = 153
    Height = 21
    TabOrder = 1
    Text = '100'
  end
  object EditMax: TEdit
    Left = 64
    Top = 70
    Width = 153
    Height = 21
    TabOrder = 2
    Text = '200'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 97
    Width = 674
    Height = 372
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 264
    Top = 8
    Width = 75
    Height = 39
    Caption = 'Query'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 345
    Top = 8
    Width = 75
    Height = 39
    Caption = 'Indexes'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 426
    Top = 8
    Width = 75
    Height = 39
    Caption = 'Explain'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 296
    Top = 53
    Width = 75
    Height = 38
    Caption = 'Drop Idx'
    TabOrder = 7
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 377
    Top = 53
    Width = 75
    Height = 38
    Caption = 'Create Idx'
    TabOrder = 8
    OnClick = Button5Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=Mongo')
    LoginPrompt = False
    Left = 80
    Top = 152
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 80
    Top = 216
  end
  object FDMongoQuery1: TFDMongoQuery
    FormatOptions.AssignedValues = [fvStrsTrim2Len]
    FormatOptions.StrsTrim2Len = True
    Connection = FDConnection1
    DatabaseName = 'test'
    CollectionName = 'restaurants'
    Left = 240
    Top = 184
  end
  object DataSource1: TDataSource
    DataSet = FDMongoQuery1
    Left = 280
    Top = 248
  end
  object FDMongoDataSet1: TFDMongoDataSet
    FormatOptions.AssignedValues = [fvStrsTrim2Len]
    FormatOptions.StrsTrim2Len = True
    Connection = FDConnection1
    DatabaseName = 'test'
    CollectionName = 'restaurants'
    Left = 368
    Top = 256
  end
end
