object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MongoDB BatchMove Demo'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    635
    300)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Move Data'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 39
    Width = 619
    Height = 253
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    Left = 64
    Top = 16
  end
  object FDBatchMoveSQLReader1: TFDBatchMoveSQLReader
    Connection = FDConnection1
    TableName = 'Orders'
    Left = 64
    Top = 72
  end
  object FDBatchMove1: TFDBatchMove
    Reader = FDBatchMoveSQLReader1
    Writer = FDBatchMoveDataSetWriter1
    Mappings = <>
    LogFileName = 'Data.log'
    Left = 184
    Top = 72
  end
  object FDBatchMoveDataSetWriter1: TFDBatchMoveDataSetWriter
    DataSet = FDMongoQuery1
    Optimise = False
    Left = 304
    Top = 72
  end
  object FDConnection2: TFDConnection
    Params.Strings = (
      'DriverID=Mongo')
    Left = 440
    Top = 16
  end
  object DataSource1: TDataSource
    DataSet = FDMongoQuery1
    Left = 440
    Top = 128
  end
  object FDMongoQuery1: TFDMongoQuery
    FormatOptions.AssignedValues = [fvStrsTrim2Len]
    FormatOptions.StrsTrim2Len = True
    Connection = FDConnection2
    Left = 440
    Top = 72
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 144
    Top = 224
  end
  object FDPhysMongoDriverLink1: TFDPhysMongoDriverLink
    Left = 408
    Top = 224
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 272
    Top = 224
  end
end
