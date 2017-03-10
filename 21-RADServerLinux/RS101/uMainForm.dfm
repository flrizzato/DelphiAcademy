object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 369
  ClientWidth = 590
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
    Left = 16
    Top = 24
    Width = 125
    Height = 21
    TabOrder = 0
    TextHint = 'UserName'
  end
  object Edit2: TEdit
    Left = 16
    Top = 51
    Width = 125
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
    TextHint = 'Password'
  end
  object Button1: TButton
    Left = 16
    Top = 104
    Width = 125
    Height = 25
    Caption = 'Login'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 188
    Width = 125
    Height = 25
    Caption = 'GET'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 16
    Top = 219
    Width = 125
    Height = 25
    Caption = 'GETITEM'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 16
    Top = 250
    Width = 125
    Height = 25
    Caption = 'POST'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 16
    Top = 281
    Width = 125
    Height = 25
    Caption = 'PUTITEM'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 16
    Top = 312
    Width = 125
    Height = 25
    Caption = 'DELETEITEM'
    TabOrder = 7
    OnClick = Button6Click
  end
  object Memo1: TMemo
    Left = 160
    Top = 24
    Width = 409
    Height = 313
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object Button7: TButton
    Left = 16
    Top = 135
    Width = 125
    Height = 25
    Caption = 'Logout'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    OnClick = Button7Click
  end
  object EMSProvider1: TEMSProvider
    ApiVersion = '1'
    URLHost = '127.0.0.1'
    URLPort = 8080
    Left = 216
    Top = 48
  end
  object BackendEndpoint1: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Params = <>
    Resource = 'RS101'
    Left = 216
    Top = 104
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 220
    Top = 165
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = BackendEndpoint1
      FieldName = 'Response.JSONText'
      Control = Memo1
      Track = False
    end
  end
  object BackendAuth1: TBackendAuth
    Provider = EMSProvider1
    LoginPrompt = False
    UserDetails = <>
    DefaultAuthentication = Application
    OnLoggedIn = BackendAuth1LoggedIn
    OnLoggedOut = BackendAuth1LoggedOut
    Left = 300
    Top = 80
  end
end
