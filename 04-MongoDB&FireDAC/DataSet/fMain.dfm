object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MongoDB DataSet Demo'
  ClientHeight = 482
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    635
    482)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 287
    Width = 38
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Coords:'
  end
  object Label2: TLabel
    Left = 159
    Top = 287
    Width = 38
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Grades:'
  end
  object Label3: TLabel
    Left = 135
    Top = 13
    Width = 63
    Height = 13
    Caption = 'JSON Query:'
  end
  object lblTimeElapsed: TLabel
    Left = 207
    Top = 45
    Width = 3
    Height = 13
  end
  object Label5: TLabel
    Left = 135
    Top = 45
    Width = 66
    Height = 13
    Caption = 'Time elapsed:'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Fetch Restaurants'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 72
    Width = 619
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = dsRestaurants
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid2: TDBGrid
    Left = 8
    Top = 304
    Width = 145
    Height = 170
    Anchors = [akLeft, akBottom]
    DataSource = dsCoords
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid3: TDBGrid
    Left = 159
    Top = 304
    Width = 468
    Height = 170
    Anchors = [akLeft, akRight, akBottom]
    DataSource = dsGrades
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object edtJSONQuery: TEdit
    Left = 204
    Top = 10
    Width = 423
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = '{"cuisine": "Italian", "address.zipcode": "10075"} '
  end
  object Button2: TButton
    Left = 8
    Top = 40
    Width = 121
    Height = 25
    Caption = 'Save Restaurants'
    TabOrder = 5
    OnClick = Button2Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=172.16.121.241'
      'UseSSL=false'
      'User_Name=test'
      'Password=test'
      'DriverID=Mongo')
    Left = 200
    Top = 144
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 288
    Top = 88
  end
  object FDPhysMongoDriverLink1: TFDPhysMongoDriverLink
    Left = 392
    Top = 144
  end
  object dsRestaurants: TDataSource
    DataSet = FDMongoQuery1
    Left = 312
    Top = 200
  end
  object dsCoords: TDataSource
    Left = 80
    Top = 368
  end
  object dsGrades: TDataSource
    Left = 368
    Top = 376
  end
  object FDMongoQuery1: TFDMongoQuery
    FormatOptions.AssignedValues = [fvStrsTrim2Len]
    FormatOptions.StrsTrim2Len = True
    Connection = FDConnection1
    DatabaseName = 'test'
    CollectionName = 'restaurants'
    Left = 216
    Top = 200
  end
end
