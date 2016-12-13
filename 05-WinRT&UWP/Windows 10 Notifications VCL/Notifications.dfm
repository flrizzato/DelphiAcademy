object NotificationsForm: TNotificationsForm
  Left = 0
  Top = 0
  Caption = 'NotificationsForm'
  ClientHeight = 409
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmLog: TMemo
    Left = 24
    Top = 112
    Width = 393
    Height = 257
    Lines.Strings = (
      '')
    ReadOnly = True
    TabOrder = 0
  end
  object btnShow: TButton
    Left = 24
    Top = 40
    Width = 100
    Height = 25
    Caption = 'Show'
    TabOrder = 1
    OnClick = btnShowClick
  end
  object btnCancel: TButton
    Left = 170
    Top = 40
    Width = 100
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnCancelAll: TButton
    Left = 317
    Top = 40
    Width = 100
    Height = 25
    Caption = 'Cancel All'
    TabOrder = 3
    OnClick = btnCancelAllClick
  end
  object btnShowAnother: TButton
    Left = 24
    Top = 71
    Width = 100
    Height = 25
    Caption = 'Show Another'
    TabOrder = 4
    OnClick = btnShowAnotherClick
  end
  object btnCancelAnother: TButton
    Left = 170
    Top = 71
    Width = 100
    Height = 25
    Caption = 'Cancel Another'
    TabOrder = 5
    OnClick = btnCancelAnotherClick
  end
  object NotificationCenter1: TNotificationCenter
    OnReceiveLocalNotification = NotificationCenter1ReceiveLocalNotification
    Left = 300
    Top = 88
  end
end
