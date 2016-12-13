object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'FireDAC 101'
  ClientHeight = 306
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 10
    Top = 50
    Width = 734
    Height = 246
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 754
    Height = 40
    Align = alTop
    TabOrder = 1
    object DBNavigator1: TDBNavigator
      Left = 118
      Top = 1
      Width = 635
      Height = 38
      DataSource = DataSource1
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 115
      ExplicitTop = 8
      ExplicitWidth = 620
      ExplicitHeight = 25
    end
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 11
      Top = 11
      Width = 97
      Height = 18
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alLeft
      Caption = 'Connected'
      TabOrder = 1
      OnClick = CheckBox1Click
      ExplicitLeft = 12
      ExplicitTop = 13
      ExplicitHeight = 17
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 48
    Top = 216
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM customer')
    Left = 136
    Top = 216
  end
  object DataSource1: TDataSource
    DataSet = FDQuery1
    Left = 208
    Top = 216
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 304
    Top = 216
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 408
    Top = 216
  end
end
