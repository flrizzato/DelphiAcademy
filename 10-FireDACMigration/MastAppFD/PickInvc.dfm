object PickOrderNoDlg: TPickOrderNoDlg
  Left = 236
  Top = 124
  AutoScroll = False
  Caption = 'Order No.'
  ClientHeight = 190
  ClientWidth = 166
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 1
    Width = 94
    Height = 13
    Caption = '&Select an Order No.'
    FocusControl = DBLookupListBox1
  end
  object CancelBtn: TButton
    Left = 86
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object OKBtn: TButton
    Left = 2
    Top = 160
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object DBLookupListBox1: TDBLookupListBox
    Left = 5
    Top = 16
    Width = 153
    Height = 134
    KeyField = 'OrderNo'
    ListField = 'OrderNo'
    ListSource = MastData.OrdersSource
    TabOrder = 2
  end
end
