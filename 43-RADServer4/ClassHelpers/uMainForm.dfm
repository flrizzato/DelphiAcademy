object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Class Helpers Demo'
  ClientHeight = 273
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object butTStrings: TButton
    Left = 16
    Top = 234
    Width = 225
    Height = 25
    Caption = 'TStrings'
    TabOrder = 0
    OnClick = butTStringsClick
  end
  object lsbSearch: TListBox
    Left = 16
    Top = 16
    Width = 225
    Height = 185
    ItemHeight = 13
    Items.Strings = (
      'Delphi'
      'Tokyo '
      '10.2.3')
    TabOrder = 1
  end
  object edtSearch: TEdit
    Left = 16
    Top = 207
    Width = 225
    Height = 21
    TabOrder = 2
    TextHint = 'Search string...'
  end
  object butTStringHelper: TButton
    Left = 262
    Top = 16
    Width = 225
    Height = 25
    Caption = 'System.SysUtils.TStringHelper'
    TabOrder = 3
    OnClick = butTStringHelperClick
  end
end
