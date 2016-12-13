object QueryCustDlg: TQueryCustDlg
  Left = 266
  Top = 100
  ActiveControl = FromEdit
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Specify Date Range'
  ClientHeight = 96
  ClientWidth = 318
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 5
    Top = 6
    Width = 225
    Height = 84
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 12
    Top = 34
    Width = 23
    Height = 13
    Caption = '&From'
    FocusControl = FromEdit
  end
  object Label2: TLabel
    Left = 12
    Top = 63
    Width = 13
    Height = 13
    Caption = '&To'
    FocusControl = ToEdit
  end
  object Msglab: TLabel
    Left = 12
    Top = 11
    Width = 197
    Height = 16
    AutoSize = False
    Caption = 'Customers with LastInvoiceDate ranging:'
    WordWrap = True
  end
  object PopupCalBtnFrom: TSpeedButton
    Left = 197
    Top = 31
    Width = 21
    Height = 21
    Hint = 'Browse calendar'
    Glyph.Data = {
      4E010000424D4E01000000000000760000002800000012000000120000000100
      040000000000D800000000000000000000000000000010000000000000000000
      BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333330000003333333333333333330000003338888888888888330000003304
      0404404040483300000033FFFFFFFFFFFF483300000033FFFFFFFFFFFF483300
      000033FF000F0007FF483300000033FFF0FF7F70FF483300000033FFF0FFFFF0
      FF483300000033FFF0FF0007FF483300000033FF00FF0FFFFF483300000033FF
      F0FF0000FF483300000033FFFFFFFFFFFF483300000033FFFFFFFFFFFF483300
      000033F7777777777F4833000000330000000000003333000000333333333333
      333333000000333333333333333333000000}
    Layout = blGlyphRight
    ParentShowHint = False
    ShowHint = True
    OnClick = PopupCalBtnFromClick
  end
  object PopupCalToBtn: TSpeedButton
    Left = 197
    Top = 60
    Width = 21
    Height = 21
    Hint = 'Browse calendar'
    Glyph.Data = {
      4E010000424D4E01000000000000760000002800000012000000120000000100
      040000000000D800000000000000000000000000000010000000000000000000
      BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333330000003333333333333333330000003338888888888888330000003304
      0404404040483300000033FFFFFFFFFFFF483300000033FFFFFFFFFFFF483300
      000033FF000F0007FF483300000033FFF0FF7F70FF483300000033FFF0FFFFF0
      FF483300000033FFF0FF0007FF483300000033FF00FF0FFFFF483300000033FF
      F0FF0000FF483300000033FFFFFFFFFFFF483300000033FFFFFFFFFFFF483300
      000033F7777777777F4833000000330000000000003333000000333333333333
      333333000000333333333333333333000000}
    Layout = blGlyphRight
    ParentShowHint = False
    ShowHint = True
    OnClick = PopupCalToBtnClick
  end
  object FromEdit: TEdit
    Left = 46
    Top = 31
    Width = 147
    Height = 21
    TabOrder = 0
  end
  object ToEdit: TEdit
    Left = 46
    Top = 60
    Width = 147
    Height = 21
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 240
    Top = 34
    Width = 70
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OkBtn: TButton
    Left = 240
    Top = 6
    Width = 70
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = OkBtnClick
  end
end
