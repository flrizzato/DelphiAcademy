object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'MongoDB General Demo'
  ClientHeight = 336
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    492
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 70
    Width = 476
    Height = 258
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object btnInsert: TButton
    Left = 8
    Top = 9
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 1
    OnClick = btnInsertClick
  end
  object btnPing: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Ping'
    TabOrder = 2
    OnClick = btnPingClick
  end
  object btnAggProj: TButton
    Left = 169
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Agg / Proj'
    TabOrder = 3
    OnClick = btnAggProjClick
  end
  object btnAggRedact: TButton
    Left = 250
    Top = 9
    Width = 75
    Height = 25
    Caption = 'Agg / Redact'
    TabOrder = 4
    OnClick = btnAggRedactClick
  end
  object btnInsFind: TButton
    Left = 331
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Find'
    TabOrder = 5
    OnClick = btnInsFindClick
  end
  object btnListCols: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'ListCols'
    TabOrder = 6
    OnClick = btnListColsClick
  end
  object btnUpdInc: TButton
    Left = 88
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Upd / Inc'
    TabOrder = 7
    OnClick = btnUpdIncClick
  end
  object btnUpdPush: TButton
    Left = 169
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Upd / Push'
    TabOrder = 8
    OnClick = btnUpdPushClick
  end
  object Button9: TButton
    Left = 250
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Bulk Ins'
    TabOrder = 9
    OnClick = Button9Click
  end
  object btnIterate: TButton
    Left = 331
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Iterate'
    TabOrder = 10
    OnClick = btnIterateClick
  end
  object btnCurrentOp: TButton
    Left = 412
    Top = 8
    Width = 75
    Height = 25
    Caption = 'CurrentOp'
    TabOrder = 11
    OnClick = btnCurrentOpClick
  end
  object FDPhysMongoDriverLink1: TFDPhysMongoDriverLink
    Left = 176
    Top = 176
  end
  object FDMoniFlatFileClientLink1: TFDMoniFlatFileClientLink
    Left = 176
    Top = 232
  end
  object FDGUIxErrorDialog1: TFDGUIxErrorDialog
    Provider = 'Forms'
    Left = 312
    Top = 232
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 312
    Top = 176
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=Mongo')
    Left = 64
    Top = 176
  end
end
