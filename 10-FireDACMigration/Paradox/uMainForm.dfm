object Form1: TForm1
  Left = 0
  Top = 0
  ActiveControl = DBGrid1
  Caption = 'Paradox via FireDAC'
  ClientHeight = 389
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 0
    Width = 650
    Height = 25
    DataSource = DataSource1
    Align = alTop
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 25
    Width = 650
    Height = 364
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ParadoxConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=Paradox')
    LoginPrompt = False
    Left = 48
    Top = 61
  end
  object DataSource1: TDataSource
    DataSet = CustomerTable
    Left = 48
    Top = 160
  end
  object FDPhysODBCDriverLink1: TFDPhysODBCDriverLink
    Left = 168
    Top = 64
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 160
    Top = 120
  end
  object CustomerTable: TFDTable
    Connection = ParadoxConnection
    UpdateOptions.UpdateTableName = 'customer'
    TableName = 'customer'
    Left = 48
    Top = 112
  end
end
