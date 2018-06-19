object ActivityIndicatorForm: TActivityIndicatorForm
  Left = 0
  Top = 0
  Caption = 'Test ActivityIndicator'
  ClientHeight = 258
  ClientWidth = 385
  Color = clGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 97
    Width = 60
    Height = 13
    Alignment = taRightJustify
    Caption = 'Frame Delay'
  end
  object Label2: TLabel
    Left = 30
    Top = 24
    Width = 45
    Height = 13
    Alignment = taRightJustify
    Caption = 'VCL Style'
  end
  object chkAnimate: TCheckBox
    Left = 36
    Top = 63
    Width = 63
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Animate'
    TabOrder = 1
    OnClick = chkAnimateClick
  end
  object trkFrameDelay: TTrackBar
    Left = 78
    Top = 93
    Width = 177
    Height = 45
    Max = 15
    Min = 3
    Position = 5
    TabOrder = 2
    OnChange = trkFrameDelayChange
  end
  object grpIndicatorType: TRadioGroup
    Left = 8
    Top = 144
    Width = 129
    Height = 105
    Caption = 'Indicator Type'
    ItemIndex = 0
    Items.Strings = (
      'aitMomentumDots'
      'aitRotatingSector'
      'aitSectorRing')
    TabOrder = 3
    OnClick = grpIndicatorTypeClick
  end
  object grpIndicatorSize: TRadioGroup
    Left = 152
    Top = 144
    Width = 105
    Height = 105
    Caption = 'Indicator Size'
    ItemIndex = 1
    Items.Strings = (
      'aisSmall'
      'aisMedium'
      'aisLarge'
      'aisXLarge')
    TabOrder = 4
    OnClick = grpIndicatorSizeClick
  end
  object grpIndicatorColor: TRadioGroup
    Left = 271
    Top = 144
    Width = 106
    Height = 105
    Caption = 'Indicator Color'
    ItemIndex = 0
    Items.Strings = (
      'aicBlack'
      'aicWhite')
    TabOrder = 5
    OnClick = grpIndicatorColorClick
  end
  object cbxVclStyles: TComboBox
    Left = 85
    Top = 21
    Width = 164
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbxVclStylesChange
  end
  object ActivityIndicator1: TActivityIndicator
    Left = 304
    Top = 48
  end
end
