object ToggleSwitchForm: TToggleSwitchForm
  Left = 0
  Top = 0
  Caption = 'TToggleSwitch'
  ClientHeight = 400
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object lblVclStyle: TLabel
    Left = 173
    Top = 101
    Width = 49
    Height = 15
    Caption = 'VCL Style'
  end
  object chkEnabled: TCheckBox
    Left = 296
    Top = 190
    Width = 133
    Height = 17
    Caption = 'Enabled'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = chkEnabledClick
  end
  object cbxVclStyles: TComboBox
    Left = 236
    Top = 98
    Width = 193
    Height = 23
    Style = csDropDownList
    TabOrder = 2
    OnChange = cbxVclStylesChange
  end
  object grpStateCaptions: TGroupBox
    Left = 16
    Top = 152
    Width = 249
    Height = 89
    Caption = 'State Captions'
    TabOrder = 3
    object lblCaptionOff: TLabel
      Left = 8
      Top = 26
      Width = 59
      Height = 15
      Caption = 'CaptionOff'
    end
    object lblCaptionOn: TLabel
      Left = 8
      Top = 57
      Width = 58
      Height = 15
      Caption = 'CaptionOn'
    end
    object edtCaptionOff: TEdit
      Left = 92
      Top = 23
      Width = 145
      Height = 23
      TabOrder = 0
      OnChange = edtCaptionOffChange
    end
    object edtCaptionOn: TEdit
      Left = 92
      Top = 54
      Width = 145
      Height = 23
      TabOrder = 1
      OnChange = edtCaptionOnChange
    end
  end
  object chkShowStateCaptions: TCheckBox
    Left = 296
    Top = 156
    Width = 133
    Height = 17
    Caption = 'ShowStateCaptions'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = chkShowStateCaptionsClick
  end
  object grpAlignment: TRadioGroup
    Left = 288
    Top = 268
    Width = 141
    Height = 117
    Caption = 'Alignment'
    ItemIndex = 1
    Items.Strings = (
      'taLeftJustify'
      'taRightJustify')
    TabOrder = 8
    OnClick = grpAlignmentClick
  end
  object grpState: TRadioGroup
    Left = 16
    Top = 80
    Width = 133
    Height = 49
    Caption = 'State'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Off'
      'On')
    TabOrder = 1
    OnClick = grpStateClick
  end
  object TS: TToggleSwitch
    Left = 200
    Top = 30
    Width = 72
    Height = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    SwitchHeight = 19
    SwitchWidth = 43
    TabOrder = 0
    ThumbWidth = 20
  end
  object chkReadOnly: TCheckBox
    Left = 296
    Top = 224
    Width = 133
    Height = 17
    Caption = 'Read Only'
    TabOrder = 6
    OnClick = chkReadOnlyClick
  end
  object grpColors: TGroupBox
    Left = 16
    Top = 268
    Width = 249
    Height = 117
    Caption = 'Colors (Windows Style Only)'
    TabOrder = 7
    object lblColor: TLabel
      Left = 8
      Top = 31
      Width = 29
      Height = 15
      Caption = 'Color'
    end
    object lblThumbColor: TLabel
      Left = 8
      Top = 59
      Width = 71
      Height = 15
      Caption = 'Thumb Color'
    end
    object lblFrameColor: TLabel
      Left = 8
      Top = 87
      Width = 65
      Height = 15
      Caption = 'Frame Color'
    end
    object cbxColor: TColorBox
      Left = 92
      Top = 28
      Width = 145
      Height = 22
      Selected = clWindow
      TabOrder = 0
      OnChange = cbxColorChange
    end
    object cbxThumbColor: TColorBox
      Left = 92
      Top = 56
      Width = 145
      Height = 22
      Selected = clWindowText
      TabOrder = 1
      OnChange = cbxThumbColorChange
    end
    object cbxFrameColor: TColorBox
      Left = 92
      Top = 84
      Width = 145
      Height = 22
      Selected = clWindowText
      TabOrder = 2
      OnChange = cbxFrameColorChange
    end
  end
end
