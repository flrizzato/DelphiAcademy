object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'List Sensors (VCL)'
  ClientHeight = 308
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 128
    Top = 29
    Width = 51
    Height = 13
    Caption = 'Sensors: 0'
  end
  object Button1: TButton
    Left = 32
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Get Sensors'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 32
    Top = 72
    Width = 361
    Height = 209
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
end
