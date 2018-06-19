object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'VCL Styling for Common Dialog'
  ClientHeight = 269
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 207
    Width = 33
    Height = 13
    Caption = 'Styles:'
  end
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 130
    Height = 25
    Caption = 'TColorDialog'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 160
    Top = 16
    Width = 130
    Height = 25
    Caption = 'TOpenDialog'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button7: TButton
    Left = 8
    Top = 63
    Width = 130
    Height = 25
    Caption = 'TFontDialog'
    TabOrder = 2
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 160
    Top = 110
    Width = 130
    Height = 25
    Caption = 'TOpenTextFileDialog'
    TabOrder = 3
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 160
    Top = 63
    Width = 130
    Height = 25
    Caption = 'TOpenPictureDialog'
    TabOrder = 4
    OnClick = Button9Click
  end
  object Button3: TButton
    Left = 8
    Top = 110
    Width = 130
    Height = 25
    Caption = 'TFindDialog'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 157
    Width = 130
    Height = 25
    Caption = 'TSearchDialog'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 312
    Top = 16
    Width = 130
    Height = 25
    Caption = 'TPrintDialog'
    TabOrder = 7
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 312
    Top = 63
    Width = 130
    Height = 25
    Caption = 'TPageSetupDialog'
    TabOrder = 8
    OnClick = Button6Click
  end
  object Button10: TButton
    Left = 312
    Top = 110
    Width = 130
    Height = 25
    Caption = 'TPrinterSetupDialog'
    TabOrder = 9
    OnClick = Button10Click
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 227
    Width = 217
    Height = 21
    Style = csOwnerDrawFixed
    ItemHeight = 15
    TabOrder = 10
    OnClick = ComboBox1Click
  end
  object ColorDialog1: TColorDialog
    Left = 464
    Top = 16
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 528
    Top = 72
  end
  object OpenDialog1: TOpenDialog
    Left = 464
    Top = 128
  end
  object PrintDialog1: TPrintDialog
    Left = 528
    Top = 16
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 464
    Top = 72
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Left = 527
    Top = 186
  end
  object FindDialog1: TFindDialog
    Left = 526
    Top = 129
  end
  object PageSetupDialog1: TPageSetupDialog
    MinMarginLeft = 0
    MinMarginTop = 0
    MinMarginRight = 0
    MinMarginBottom = 0
    MarginLeft = 2500
    MarginTop = 2500
    MarginRight = 2500
    MarginBottom = 2500
    PageWidth = 21000
    PageHeight = 29700
    Left = 464
    Top = 184
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 592
    Top = 73
  end
  object ReplaceDialog1: TReplaceDialog
    Left = 592
    Top = 16
  end
end
