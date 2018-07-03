inherited UserLoginForm: TUserLoginForm
  BorderStyle = bsDialog
  Caption = 'User Login'
  ClientHeight = 137
  ClientWidth = 247
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 253
  ExplicitHeight = 166
  PixelsPerInch = 96
  TextHeight = 13
  object lblUserLogin: TLabel
    Left = 27
    Top = 20
    Width = 59
    Height = 13
    Caption = 'User Login'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPassword: TLabel
    Left = 35
    Top = 52
    Width = 54
    Height = 13
    Caption = 'Password'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edtUserLogin: TEdit
    Left = 99
    Top = 20
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = edtUserLoginChange
  end
  object edtPassword: TEdit
    Left = 99
    Top = 52
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
    OnChange = edtUserLoginChange
  end
  object btnOK: TBitBtn
    Left = 43
    Top = 92
    Width = 75
    Height = 25
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 131
    Top = 92
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end
