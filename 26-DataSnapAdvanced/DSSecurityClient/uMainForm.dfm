object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 253
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ediUser: TEdit
    Left = 24
    Top = 32
    Width = 125
    Height = 21
    TabOrder = 0
    Text = 'UserName'
    OnChange = ediUserChange
  end
  object ediPass: TEdit
    Left = 24
    Top = 72
    Width = 125
    Height = 21
    TabOrder = 1
    Text = 'Password'
    OnChange = ediUserChange
  end
  object Button1: TButton
    Left = 24
    Top = 120
    Width = 125
    Height = 25
    Caption = 'EchoString'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 160
    Width = 125
    Height = 25
    Caption = 'ReverseString'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 199
    Width = 125
    Height = 25
    Caption = 'ServerDateTime'
    TabOrder = 4
    OnClick = Button3Click
  end
  object memResult: TMemo
    Left = 176
    Top = 32
    Width = 425
    Height = 192
    Lines.Strings = (
      'memResult')
    TabOrder = 5
  end
end
