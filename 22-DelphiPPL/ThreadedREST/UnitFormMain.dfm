object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Threaded REST request demo'
  ClientHeight = 354
  ClientWidth = 581
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 72
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 199
    Top = 72
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo2')
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 390
    Top = 72
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo3')
    TabOrder = 2
  end
  object Memo4: TMemo
    Left = 8
    Top = 200
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo4')
    TabOrder = 3
  end
  object Memo5: TMemo
    Left = 199
    Top = 200
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo5')
    TabOrder = 4
  end
  object Memo6: TMemo
    Left = 390
    Top = 200
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo6')
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 199
    Top = 8
    Width = 374
    Height = 25
    Step = 1
    TabOrder = 6
  end
  object Button1: TButton
    Tag = 971243
    Left = 8
    Top = 41
    Width = 185
    Height = 25
    Caption = 'Katy Perry'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button2: TButton
    Tag = 182738
    Left = 199
    Top = 41
    Width = 185
    Height = 25
    Caption = 'John Legend'
    TabOrder = 8
    OnClick = Button2Click
  end
  object Button3: TButton
    Tag = 4080
    Left = 390
    Top = 41
    Width = 183
    Height = 25
    Caption = 'Fink'
    TabOrder = 9
    OnClick = Button3Click
  end
  object Button4: TButton
    Tag = 321128
    Left = 8
    Top = 167
    Width = 185
    Height = 25
    Caption = 'Rihanna'
    TabOrder = 10
    OnClick = Button4Click
  end
  object Button5: TButton
    Tag = 29735
    Left = 199
    Top = 169
    Width = 185
    Height = 25
    Caption = 'Coldplay'
    TabOrder = 11
    OnClick = Button5Click
  end
  object Button6: TButton
    Tag = 457265
    Left = 390
    Top = 169
    Width = 183
    Height = 25
    Caption = 'Wende'
    TabOrder = 12
    OnClick = Button6Click
  end
  object CheckBoxUseThreading: TCheckBox
    Left = 8
    Top = 8
    Width = 137
    Height = 17
    Caption = 'CheckBoxUseThreading'
    TabOrder = 13
  end
  object RESTClient1: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'UTF-8, *;q=0.8'
    AcceptEncoding = 'identity'
    BaseURL = 'http://api.discogs.com'
    Params = <>
    HandleRedirects = True
    Left = 32
    Top = 304
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <
      item
        Kind = pkURLSEGMENT
        name = 'ID'
        Options = [poAutoCreated]
      end>
    Resource = 'artists/{ID}'
    Response = RESTResponse1
    SynchronizedEvents = False
    Left = 136
    Top = 304
  end
  object RESTResponse1: TRESTResponse
    Left = 240
    Top = 304
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 536
  end
end
