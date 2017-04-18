object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DataSnap JQuery Client'
  ClientHeight = 242
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 24
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '100'
  end
  object Edit2: TEdit
    Left = 24
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '200'
  end
  object Button1: TButton
    Left = 24
    Top = 112
    Width = 121
    Height = 25
    Caption = 'Sum'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Edit3: TEdit
    Left = 24
    Top = 160
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '000'
  end
end
