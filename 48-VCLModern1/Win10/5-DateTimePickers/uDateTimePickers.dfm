object DateTimePickersForm: TDateTimePickersForm
  Left = 0
  Top = 0
  Caption = 'TDatePicker and TTimePicker'
  ClientHeight = 449
  ClientWidth = 571
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
  object lblVclStyle: TLabel
    Left = 19
    Top = 256
    Width = 45
    Height = 13
    Caption = 'VCL Style'
  end
  object Label1: TLabel
    Left = 253
    Top = 256
    Width = 79
    Height = 13
    Caption = 'DropDownCount'
  end
  object lblDateFormats: TLabel
    Left = 56
    Top = 160
    Width = 65
    Height = 13
    Caption = 'Date Formats'
  end
  object lblTimeFormats: TLabel
    Left = 328
    Top = 160
    Width = 64
    Height = 13
    Caption = 'Time Formats'
  end
  object lblMinuteIncrement: TLabel
    Left = 328
    Top = 215
    Width = 84
    Height = 13
    Caption = 'Minute Increment'
  end
  object cbxVclStyles: TComboBox
    Left = 80
    Top = 253
    Width = 145
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 5
    OnChange = cbxVclStylesChange
  end
  object chkShowOkCancel: TCheckBox
    Left = 447
    Top = 255
    Width = 105
    Height = 17
    Caption = 'Show OK/Cancel'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = chkShowOkCancelClick
  end
  object grpColors: TGroupBox
    Left = 16
    Top = 293
    Width = 544
    Height = 140
    Caption = 'Colors (Widows Style Only)'
    TabOrder = 9
    object lblColor: TLabel
      Left = 16
      Top = 27
      Width = 25
      Height = 13
      Caption = 'Color'
    end
    object lblHighlightColor: TLabel
      Left = 16
      Top = 111
      Width = 66
      Height = 13
      Caption = 'HighlightColor'
    end
    object lblPopupColor: TLabel
      Left = 276
      Top = 27
      Width = 55
      Height = 13
      Caption = 'PopupColor'
    end
    object lblSelectionColor: TLabel
      Left = 276
      Top = 55
      Width = 68
      Height = 13
      Caption = 'SelectionColor'
    end
    object lblSelectionFontColor: TLabel
      Left = 276
      Top = 83
      Width = 90
      Height = 13
      Caption = 'SelectionFontColor'
    end
    object lblFontColor: TLabel
      Left = 16
      Top = 55
      Width = 50
      Height = 13
      Caption = 'Font Color'
    end
    object lblHotColor: TLabel
      Left = 16
      Top = 83
      Width = 45
      Height = 13
      Caption = 'Hot Color'
    end
    object cbxColor: TColorBox
      Left = 104
      Top = 24
      Width = 145
      Height = 22
      DefaultColorColor = clWindow
      Selected = clWindow
      TabOrder = 0
      OnChange = cbxColorChange
    end
    object cbxHighlightColor: TColorBox
      Left = 104
      Top = 108
      Width = 145
      Height = 22
      DefaultColorColor = clBtnFace
      Selected = clBtnFace
      TabOrder = 3
      OnChange = cbxHighlightColorChange
    end
    object cbxPopupColor: TColorBox
      Left = 387
      Top = 24
      Width = 145
      Height = 22
      DefaultColorColor = clWindow
      Selected = clWindow
      TabOrder = 4
      OnChange = cbxPopupColorChange
    end
    object cbxSelectionColor: TColorBox
      Left = 387
      Top = 52
      Width = 145
      Height = 22
      DefaultColorColor = clHighlight
      Selected = clHighlight
      TabOrder = 5
      OnChange = cbxSelectionColorChange
    end
    object cbxSelectionFontColor: TColorBox
      Left = 387
      Top = 80
      Width = 145
      Height = 22
      DefaultColorColor = clHighlightText
      Selected = clHighlightText
      TabOrder = 6
      OnChange = cbxSelectionFontColorChange
    end
    object cbxFontColor: TColorBox
      Left = 104
      Top = 52
      Width = 145
      Height = 22
      DefaultColorColor = clWindowText
      Selected = clWindowText
      TabOrder = 1
      OnChange = cbxFontColorChange
    end
    object cbxHotColor: TColorBox
      Left = 104
      Top = 80
      Width = 145
      Height = 22
      DefaultColorColor = clBtnHighlight
      Selected = clBtnHighlight
      TabOrder = 2
      OnChange = cbxHotColorChange
    end
  end
  object DatePicker1: TDatePicker
    Left = 56
    Top = 56
    Width = 185
    Date = 43002.000000000000000000
    DateFormat = 'M/d/yyyy'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 0
  end
  object TimePicker1: TTimePicker
    Left = 328
    Top = 56
    Width = 174
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 1
    Time = 43002.812495196760000000
    TimeFormat = 'h:mm AMPM'
  end
  object spnDropDownCount: TUpDown
    Left = 376
    Top = 253
    Width = 38
    Height = 21
    Associate = edtDropDownCount
    Min = 3
    Max = 15
    Orientation = udHorizontal
    Position = 7
    TabOrder = 7
    OnClick = spnDropDownCountClick
  end
  object edtDropDownCount: TEdit
    Left = 343
    Top = 253
    Width = 33
    Height = 21
    Alignment = taRightJustify
    TabOrder = 6
    Text = '7'
  end
  object cbxDateFormats: TComboBox
    Left = 56
    Top = 179
    Width = 185
    Height = 21
    TabOrder = 2
    Text = 'M/d/yyyy'
    OnChange = cbxDateFormatsChange
    Items.Strings = (
      'M/d/yyyy'
      'mm/dd/yyyy'
      'mm/yyyy'
      'mmm/d/yyyy'
      'mmmm/d/yyyy'
      'd/M/yyyy'
      'd/mmm/yyyy'
      'dd/mmmm/yyyy'
      'ddd/m/d')
  end
  object cbxTimeFormats: TComboBox
    Left = 328
    Top = 179
    Width = 174
    Height = 21
    TabOrder = 3
    Text = 'h:mm AMPM'
    OnChange = cbxTimeFormatsChange
    Items.Strings = (
      'h:mm AMPM'
      'h:mm:ss AMPM'
      'hh:mm'
      'hh:mm:ss')
  end
  object cbxMinuteIncrements: TComboBox
    Left = 432
    Top = 212
    Width = 70
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = '1'
    OnChange = cbxMinuteIncrementsChange
    Items.Strings = (
      '1'
      '2'
      '5'
      '10'
      '15'
      '30')
  end
end
