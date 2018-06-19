object StackPanelForm: TStackPanelForm
  Left = 0
  Top = 0
  Caption = 'TStackPanel'
  ClientHeight = 386
  ClientWidth = 594
  Color = clBtnFace
  Constraints.MinHeight = 425
  Constraints.MinWidth = 610
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 594
    Height = 113
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblVclStyle: TLabel
      Left = 16
      Top = 16
      Width = 45
      Height = 13
      Caption = 'VCL Style'
    end
    object lblSpacing: TLabel
      Left = 16
      Top = 51
      Width = 37
      Height = 13
      Caption = 'Spacing'
    end
    object lblPadding: TLabel
      Left = 16
      Top = 83
      Width = 38
      Height = 13
      Caption = 'Padding'
    end
    object cbxVclStyles: TComboBox
      Left = 77
      Top = 13
      Width = 145
      Height = 21
      Style = csDropDownList
      Sorted = True
      TabOrder = 0
      OnChange = cbxVclStylesChange
    end
    object grpOrientation: TRadioGroup
      Left = 260
      Top = 8
      Width = 325
      Height = 41
      Caption = 'Orientation'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Vertical'
        'Horizontal')
      TabOrder = 3
      OnClick = grpOrientationClick
    end
    object trkSpacing: TTrackBar
      Left = 70
      Top = 47
      Width = 159
      Height = 26
      Max = 20
      Position = 2
      TabOrder = 1
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = trkSpacingChange
    end
    object grpPositioning: TRadioGroup
      Left = 260
      Top = 55
      Width = 325
      Height = 42
      Caption = 'Positioning'
      Columns = 4
      ItemIndex = 0
      Items.Strings = (
        'sphpLeft'
        'sphpCenter'
        'sphpRight'
        'sphpFill')
      TabOrder = 4
      OnClick = grpPositioningClick
    end
    object trkPadding: TTrackBar
      Left = 70
      Top = 79
      Width = 159
      Height = 24
      Max = 20
      Position = 5
      TabOrder = 2
      TickMarks = tmBoth
      TickStyle = tsNone
      OnChange = trkPaddingChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 113
    Width = 169
    Height = 273
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object lblControlOverride: TLabel
      Left = 16
      Top = 8
      Width = 134
      Height = 13
      Caption = 'Override Control Positioning'
    end
    object grpControlPositioning: TRadioGroup
      Left = 16
      Top = 127
      Width = 137
      Height = 137
      Caption = 'Positioning Override'
      ItemIndex = 0
      Items.Strings = (
        'sphpDefault'
        'sphpLeft'
        'sphpCenter'
        'sphpRight'
        'sphpFill')
      TabOrder = 1
      OnClick = grpControlPositioningClick
    end
    object lstControls: TListBox
      Left = 16
      Top = 31
      Width = 137
      Height = 74
      ItemHeight = 13
      Items.Strings = (
        'Label1'
        'Edit1'
        'Button1'
        'Memo1')
      TabOrder = 0
      OnClick = lstControlsClick
    end
  end
  object StackPanel1: TStackPanel
    AlignWithMargins = True
    Left = 179
    Top = 123
    Width = 405
    Height = 253
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    ControlCollection = <
      item
        Control = Label1
      end
      item
        Control = Edit1
      end
      item
        Control = Button1
      end
      item
        Control = Memo1
      end>
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 2
    object Label1: TLabel
      Left = 6
      Top = 6
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Edit1: TEdit
      Left = 6
      Top = 21
      Width = 96
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Button1: TButton
      Left = 6
      Top = 44
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 1
    end
    object Memo1: TMemo
      Left = 6
      Top = 71
      Width = 136
      Height = 55
      Lines.Strings = (
        'Memo1')
      TabOrder = 2
    end
  end
end
