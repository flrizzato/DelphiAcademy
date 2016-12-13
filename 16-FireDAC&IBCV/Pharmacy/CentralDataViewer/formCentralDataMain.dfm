object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Simple Editor'
  ClientHeight = 443
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 353
    Top = 13
    Height = 294
    ExplicitLeft = 232
    ExplicitTop = 0
    ExplicitHeight = 336
  end
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 704
    Height = 13
    Align = alTop
    Caption = 
      'Simple Data Editor so you can change the central medicine databa' +
      'se and see Change Views in action at the remote pharmacy databas' +
      'e'
    ExplicitWidth = 648
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 307
    Width = 704
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = 353
    ExplicitTop = 13
    ExplicitWidth = 297
  end
  object DBGrid3: TDBGrid
    Left = 0
    Top = 310
    Width = 704
    Height = 92
    Align = alBottom
    DataSource = dsLinks
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 13
    Width = 353
    Height = 294
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitHeight = 317
    object DBGrid1: TDBGrid
      Left = 1
      Top = 1
      Width = 351
      Height = 267
      Align = alClient
      DataSource = dsCategory
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object DBNavigator1: TDBNavigator
      Left = 1
      Top = 268
      Width = 351
      Height = 25
      DataSource = dsCategory
      Align = alBottom
      TabOrder = 1
      ExplicitTop = 291
      ExplicitWidth = 316
    end
  end
  object Panel2: TPanel
    Left = 356
    Top = 13
    Width = 348
    Height = 294
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 2
    ExplicitLeft = 321
    ExplicitWidth = 314
    ExplicitHeight = 317
    object DBGrid2: TDBGrid
      Left = 1
      Top = 1
      Width = 346
      Height = 267
      Align = alClient
      DataSource = dsMedicine
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object DBNavigator2: TDBNavigator
      Left = 1
      Top = 268
      Width = 346
      Height = 25
      DataSource = dsMedicine
      Align = alBottom
      TabOrder = 1
      ExplicitTop = 291
      ExplicitWidth = 312
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 402
    Width = 704
    Height = 41
    Align = alBottom
    TabOrder = 3
    ExplicitWidth = 635
    DesignSize = (
      704
      41)
    object Panel4: TPanel
      Left = 259
      Top = 0
      Width = 185
      Height = 41
      Anchors = [akTop]
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 225
      DesignSize = (
        185
        41)
      object btnAdd: TButton
        Left = 6
        Top = 8
        Width = 75
        Height = 25
        Action = DatasetInsert1
        Anchors = [akTop]
        TabOrder = 0
      end
      object btnDelete: TButton
        Left = 102
        Top = 8
        Width = 75
        Height = 25
        Action = DatasetDelete1
        Anchors = [akTop]
        Caption = '&Delete Link'
        TabOrder = 1
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 72
    Top = 24
    object Data1: TMenuItem
      Caption = 'Data'
      object Refresh1: TMenuItem
        Caption = 'Refresh'
        OnClick = Refresh1Click
      end
    end
  end
  object dsCategory: TDataSource
    Left = 128
    Top = 152
  end
  object dsMedicine: TDataSource
    Left = 488
    Top = 152
  end
  object dsLinks: TDataSource
    Left = 488
    Top = 360
  end
  object ActionList1: TActionList
    Left = 16
    Top = 24
    object DatasetInsert1: TDataSetInsert
      Category = 'Dataset'
      Caption = '&Add link'
      Hint = 'Insert'
      ImageIndex = 4
      DataSource = dsLinks
    end
    object DatasetDelete1: TDataSetDelete
      Category = 'Dataset'
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 5
      DataSource = dsLinks
    end
  end
end
