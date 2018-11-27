object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'VCL Media Receiver App'
  ClientHeight = 489
  ClientWidth = 378
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
    Left = 104
    Top = 440
    Width = 153
    Height = 29
    Caption = 'Photo Frame'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 24
    Top = 16
    Width = 329
    Height = 410
    BevelOuter = bvLowered
    TabOrder = 0
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 327
      Height = 408
      Align = alClient
      Stretch = True
      ExplicitLeft = 0
      ExplicitTop = 0
    end
  end
  object VCLMediaReceiver: TTetheringManager
    OnRequestManagerPassword = VCLMediaReceiverRequestManagerPassword
    Password = '1234'
    Text = 'VCLMediaReceiver'
    Left = 32
    Top = 440
  end
  object VCLMediaReceiverApp: TTetheringAppProfile
    Manager = VCLMediaReceiver
    Text = 'VCLMediaReceiverApp'
    Actions = <>
    Resources = <>
    OnResourceReceived = VCLMediaReceiverAppResourceReceived
    Left = 312
    Top = 432
  end
end
