object PickRpt: TPickRpt
  Left = 263
  Top = 115
  HelpContext = 5
  BorderStyle = bsDialog
  Caption = 'Report Selection'
  ClientHeight = 106
  ClientWidth = 238
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 150
    Top = 11
    Width = 73
    Height = 25
    Caption = '&Print'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 0
    OnClick = OKBtnClick
  end
  object CloseBtn: TButton
    Left = 151
    Top = 69
    Width = 73
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 1
  end
  object ViewBtn: TButton
    Left = 151
    Top = 40
    Width = 73
    Height = 25
    Caption = '&View'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
    OnClick = ViewBtnClick
  end
  object ReportType: TRadioGroup
    Left = 9
    Top = 6
    Width = 129
    Height = 87
    Caption = '&Report'
    ItemIndex = 0
    Items.Strings = (
      'C&ustomers Report'
      '&Orders Report'
      '&Invoice Report')
    TabOrder = 3
  end
end
