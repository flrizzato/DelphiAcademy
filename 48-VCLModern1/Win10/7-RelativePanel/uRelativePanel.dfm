object RelativePanelForm: TRelativePanelForm
  Left = 0
  Top = 0
  Caption = 'Test RelativePanel'
  ClientHeight = 524
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    521
    524)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 16
    Top = 20
    Width = 45
    Height = 13
    Caption = 'VCL Style'
  end
  object Label32: TLabel
    Left = 285
    Top = 280
    Width = 220
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = '* Drag and drop the controls to relocate them'
  end
  object cbxVclStyles: TComboBox
    Left = 80
    Top = 17
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbxVclStylesChange
  end
  object RelativePanel1: TRelativePanel
    Left = 16
    Top = 52
    Width = 489
    Height = 209
    ControlCollection = <
      item
        Control = Edit1
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = Button1
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end
      item
        Control = Shape1
        AlignBottomWithPanel = False
        AlignHorizontalCenterWithPanel = False
        AlignLeftWithPanel = False
        AlignRightWithPanel = False
        AlignTopWithPanel = False
        AlignVerticalCenterWithPanel = False
      end>
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnDragDrop = RelativePanel1DragDrop
    OnDragOver = RelativePanel1DragOver
    DesignSize = (
      489
      209)
    object Edit1: TEdit
      Left = 184
      Top = 96
      Width = 121
      Height = 21
      Anchors = []
      TabOrder = 0
      Text = 'Edit1'
      OnMouseDown = SampleControlMouseDown
    end
    object Button1: TButton
      Tag = 1
      Left = 208
      Top = 96
      Width = 75
      Height = 25
      Anchors = []
      Caption = 'Button1'
      TabOrder = 1
      OnMouseDown = SampleControlMouseDown
    end
    object Shape1: TShape
      Tag = 2
      Left = 216
      Top = 72
      Width = 65
      Height = 65
      Anchors = []
      Brush.Color = clRed
      OnMouseDown = SampleControlMouseDown
    end
  end
  object PageControl1: TPageControl
    Left = 16
    Top = 276
    Width = 489
    Height = 233
    ActivePage = TabSheet1
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = '  Edit1  '
      object Label1: TLabel
        Left = 32
        Top = 77
        Width = 31
        Height = 13
        Caption = 'LeftOf'
      end
      object Label3: TLabel
        Left = 32
        Top = 101
        Width = 64
        Height = 13
        Caption = 'AlignLeftWith'
      end
      object Label4: TLabel
        Left = 32
        Top = 125
        Width = 126
        Height = 13
        Caption = 'AlighHorizontalCenterWith'
      end
      object Label5: TLabel
        Left = 32
        Top = 149
        Width = 70
        Height = 13
        Caption = 'AlignRightWith'
      end
      object Label6: TLabel
        Left = 32
        Top = 173
        Width = 37
        Height = 13
        Caption = 'RightOf'
      end
      object Label7: TLabel
        Left = 267
        Top = 77
        Width = 31
        Height = 13
        Caption = 'Above'
      end
      object Label8: TLabel
        Left = 267
        Top = 101
        Width = 63
        Height = 13
        Caption = 'AlignTopWith'
      end
      object Label9: TLabel
        Left = 267
        Top = 125
        Width = 113
        Height = 13
        Caption = 'AlighVerticalCenterWith'
      end
      object Label10: TLabel
        Left = 267
        Top = 149
        Width = 79
        Height = 13
        Caption = 'AlignBottomWith'
      end
      object Label11: TLabel
        Left = 267
        Top = 173
        Width = 28
        Height = 13
        Caption = 'Below'
      end
      object Bevel1: TBevel
        Left = 237
        Top = 15
        Width = 11
        Height = 177
        Shape = bsLeftLine
      end
      object cb1AlignLeftWithPanel: TCheckBox
        Left = 16
        Top = 15
        Width = 170
        Height = 17
        Caption = 'AlignLeftWithPanel'
        TabOrder = 0
        OnClick = cbAlignLeftWithPanelClick
      end
      object cb1AlignHorizontalCenterWithPanel: TCheckBox
        Left = 16
        Top = 33
        Width = 170
        Height = 17
        Caption = 'AlignHorizontalCenterWithPanel'
        TabOrder = 1
        OnClick = cbAlignHorizontalCenterWithPanelClick
      end
      object cb1AlignRightWithPanel: TCheckBox
        Left = 16
        Top = 51
        Width = 170
        Height = 17
        Caption = 'AlignRightWithPanel'
        TabOrder = 2
        OnClick = cbAlignRightWithPanelClick
      end
      object cb1AlignTopWithPanel: TCheckBox
        Left = 251
        Top = 15
        Width = 170
        Height = 17
        Caption = 'AlignTopWithPanel'
        TabOrder = 3
        OnClick = cbAlignTopWithPanelClick
      end
      object cb1AlignVerticalCenterWithPanel: TCheckBox
        Left = 251
        Top = 33
        Width = 170
        Height = 17
        Caption = 'AlignVerticalCenterWithPanel'
        TabOrder = 4
        OnClick = cbAlignVerticalCenterWithPanelClick
      end
      object cb1AlignBottomWithPanel: TCheckBox
        Left = 251
        Top = 51
        Width = 170
        Height = 17
        Caption = 'AlignBottomWithPanel'
        TabOrder = 5
        OnClick = cbAlignBottomWithPanelClick
      end
      object cbx1LeftOf: TComboBox
        Left = 161
        Top = 74
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
        Text = '(none)'
        OnChange = cbxLeftOfChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1AlignLeftWith: TComboBox
        Left = 161
        Top = 98
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 7
        Text = '(none)'
        OnChange = cbxAlignLeftWithChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1AlignHorizontalCenterWith: TComboBox
        Left = 161
        Top = 122
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 8
        Text = '(none)'
        OnChange = cbxAlignHorizontalCenterWithChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1AlignRightWith: TComboBox
        Left = 161
        Top = 146
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 9
        Text = '(none)'
        OnChange = cbxAlignRightWithChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1RightOf: TComboBox
        Left = 161
        Top = 170
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 10
        Text = '(none)'
        OnChange = cbxRightOfChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1Above: TComboBox
        Left = 396
        Top = 74
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 11
        Text = '(none)'
        OnChange = cbxAboveChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1AlignTopWith: TComboBox
        Left = 396
        Top = 98
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 12
        Text = '(none)'
        OnChange = cbxAlignTopWithChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1AlignVerticalCenterWith: TComboBox
        Left = 396
        Top = 122
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 13
        Text = '(none)'
        OnChange = cbxAlignVerticalCenterWithChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1AlignBottomWith: TComboBox
        Left = 396
        Top = 146
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 14
        Text = '(none)'
        OnChange = cbxAlignBottomWithChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
      object cbx1Below: TComboBox
        Left = 396
        Top = 170
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 15
        Text = '(none)'
        OnChange = cbxBelowChange
        Items.Strings = (
          '(none)'
          'Button1'
          'Shape1')
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  Button1  '
      ImageIndex = 1
      object Label12: TLabel
        Left = 32
        Top = 77
        Width = 31
        Height = 13
        Caption = 'LeftOf'
      end
      object Label13: TLabel
        Left = 32
        Top = 101
        Width = 64
        Height = 13
        Caption = 'AlignLeftWith'
      end
      object Label14: TLabel
        Left = 32
        Top = 125
        Width = 126
        Height = 13
        Caption = 'AlighHorizontalCenterWith'
      end
      object Label15: TLabel
        Left = 32
        Top = 149
        Width = 70
        Height = 13
        Caption = 'AlignRightWith'
      end
      object Label16: TLabel
        Left = 32
        Top = 173
        Width = 37
        Height = 13
        Caption = 'RightOf'
      end
      object Label17: TLabel
        Left = 267
        Top = 77
        Width = 31
        Height = 13
        Caption = 'Above'
      end
      object Label18: TLabel
        Left = 267
        Top = 101
        Width = 63
        Height = 13
        Caption = 'AlignTopWith'
      end
      object Label19: TLabel
        Left = 267
        Top = 125
        Width = 113
        Height = 13
        Caption = 'AlighVerticalCenterWith'
      end
      object Label20: TLabel
        Left = 267
        Top = 149
        Width = 79
        Height = 13
        Caption = 'AlignBottomWith'
      end
      object Label21: TLabel
        Left = 267
        Top = 173
        Width = 28
        Height = 13
        Caption = 'Below'
      end
      object Bevel2: TBevel
        Left = 237
        Top = 15
        Width = 11
        Height = 177
        Shape = bsLeftLine
      end
      object cb2AlignLeftWithPanel: TCheckBox
        Tag = 1
        Left = 16
        Top = 15
        Width = 170
        Height = 17
        Caption = 'AlignLeftWithPanel'
        TabOrder = 0
        OnClick = cbAlignLeftWithPanelClick
      end
      object cb2AlignHorizontalCenterWithPanel: TCheckBox
        Tag = 1
        Left = 16
        Top = 33
        Width = 170
        Height = 17
        Caption = 'AlignHorizontalCenterWithPanel'
        TabOrder = 1
        OnClick = cbAlignHorizontalCenterWithPanelClick
      end
      object cb2AlignRightWithPanel: TCheckBox
        Tag = 1
        Left = 16
        Top = 51
        Width = 170
        Height = 17
        Caption = 'AlignRightWithPanel'
        TabOrder = 2
        OnClick = cbAlignRightWithPanelClick
      end
      object cb2AlignTopWithPanel: TCheckBox
        Tag = 1
        Left = 251
        Top = 15
        Width = 170
        Height = 17
        Caption = 'AlignTopWithPanel'
        TabOrder = 3
        OnClick = cbAlignTopWithPanelClick
      end
      object cb2AlignVerticalCenterWithPanel: TCheckBox
        Tag = 1
        Left = 251
        Top = 33
        Width = 170
        Height = 17
        Caption = 'AlignVerticalCenterWithPanel'
        TabOrder = 4
        OnClick = cbAlignVerticalCenterWithPanelClick
      end
      object cb2AlignBottomWithPanel: TCheckBox
        Tag = 1
        Left = 251
        Top = 51
        Width = 170
        Height = 17
        Caption = 'AlignBottomWithPanel'
        TabOrder = 5
        OnClick = cbAlignBottomWithPanelClick
      end
      object cbx2LeftOf: TComboBox
        Tag = 1
        Left = 161
        Top = 74
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
        Text = '(none)'
        OnChange = cbxLeftOfChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2AlignLeftWith: TComboBox
        Tag = 1
        Left = 161
        Top = 98
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 7
        Text = '(none)'
        OnChange = cbxAlignLeftWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2AlignHorizontalCenterWith: TComboBox
        Tag = 1
        Left = 161
        Top = 122
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 8
        Text = '(none)'
        OnChange = cbxAlignHorizontalCenterWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2AlignRightWith: TComboBox
        Tag = 1
        Left = 161
        Top = 146
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 9
        Text = '(none)'
        OnChange = cbxAlignRightWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2RightOf: TComboBox
        Tag = 1
        Left = 161
        Top = 170
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 10
        Text = '(none)'
        OnChange = cbxRightOfChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2Above: TComboBox
        Tag = 1
        Left = 396
        Top = 74
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 11
        Text = '(none)'
        OnChange = cbxAboveChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2AlignTopWith: TComboBox
        Tag = 1
        Left = 396
        Top = 98
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 12
        Text = '(none)'
        OnChange = cbxAlignTopWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2AlignVerticalCenterWith: TComboBox
        Tag = 1
        Left = 396
        Top = 122
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 13
        Text = '(none)'
        OnChange = cbxAlignVerticalCenterWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2AlignBottomWith: TComboBox
        Tag = 1
        Left = 396
        Top = 146
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 14
        Text = '(none)'
        OnChange = cbxAlignBottomWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
      object cbx2Below: TComboBox
        Tag = 1
        Left = 396
        Top = 170
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 15
        Text = '(none)'
        OnChange = cbxBelowChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Shape1')
      end
    end
    object TabSheet3: TTabSheet
      Caption = '  Shape1  '
      ImageIndex = 2
      object Label22: TLabel
        Left = 32
        Top = 77
        Width = 31
        Height = 13
        Caption = 'LeftOf'
      end
      object Label23: TLabel
        Left = 32
        Top = 101
        Width = 64
        Height = 13
        Caption = 'AlignLeftWith'
      end
      object Label24: TLabel
        Left = 32
        Top = 125
        Width = 126
        Height = 13
        Caption = 'AlighHorizontalCenterWith'
      end
      object Label25: TLabel
        Left = 32
        Top = 149
        Width = 70
        Height = 13
        Caption = 'AlignRightWith'
      end
      object Label26: TLabel
        Left = 32
        Top = 173
        Width = 37
        Height = 13
        Caption = 'RightOf'
      end
      object Label27: TLabel
        Left = 267
        Top = 77
        Width = 31
        Height = 13
        Caption = 'Above'
      end
      object Label28: TLabel
        Left = 267
        Top = 101
        Width = 63
        Height = 13
        Caption = 'AlignTopWith'
      end
      object Label29: TLabel
        Left = 267
        Top = 125
        Width = 113
        Height = 13
        Caption = 'AlighVerticalCenterWith'
      end
      object Label30: TLabel
        Left = 267
        Top = 149
        Width = 79
        Height = 13
        Caption = 'AlignBottomWith'
      end
      object Label31: TLabel
        Left = 267
        Top = 173
        Width = 28
        Height = 13
        Caption = 'Below'
      end
      object Bevel3: TBevel
        Left = 237
        Top = 15
        Width = 11
        Height = 177
        Shape = bsLeftLine
      end
      object cb3AlignLeftWithPanel: TCheckBox
        Tag = 2
        Left = 16
        Top = 15
        Width = 170
        Height = 17
        Caption = 'AlignLeftWithPanel'
        TabOrder = 0
        OnClick = cbAlignLeftWithPanelClick
      end
      object cb3AlignHorizontalCenterWithPanel: TCheckBox
        Tag = 2
        Left = 16
        Top = 33
        Width = 170
        Height = 17
        Caption = 'AlignHorizontalCenterWithPanel'
        TabOrder = 1
        OnClick = cbAlignHorizontalCenterWithPanelClick
      end
      object cb3AlignRightWithPanel: TCheckBox
        Tag = 2
        Left = 16
        Top = 51
        Width = 170
        Height = 17
        Caption = 'AlignRightWithPanel'
        TabOrder = 2
        OnClick = cbAlignRightWithPanelClick
      end
      object cb3AlignTopWithPanel: TCheckBox
        Tag = 2
        Left = 251
        Top = 15
        Width = 170
        Height = 17
        Caption = 'AlignTopWithPanel'
        TabOrder = 3
        OnClick = cbAlignTopWithPanelClick
      end
      object cb3AlignVerticalCenterWithPanel: TCheckBox
        Tag = 2
        Left = 251
        Top = 33
        Width = 170
        Height = 17
        Caption = 'AlignVerticalCenterWithPanel'
        TabOrder = 4
        OnClick = cbAlignVerticalCenterWithPanelClick
      end
      object cb3AlignBottomWithPanel: TCheckBox
        Tag = 2
        Left = 251
        Top = 51
        Width = 170
        Height = 17
        Caption = 'AlignBottomWithPanel'
        TabOrder = 5
        OnClick = cbAlignBottomWithPanelClick
      end
      object cbx3LeftOf: TComboBox
        Tag = 2
        Left = 161
        Top = 74
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
        Text = '(none)'
        OnChange = cbxLeftOfChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3AlignLeftWith: TComboBox
        Tag = 2
        Left = 161
        Top = 98
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 7
        Text = '(none)'
        OnChange = cbxAlignLeftWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3AlignHorizontalCenterWith: TComboBox
        Tag = 2
        Left = 161
        Top = 122
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 8
        Text = '(none)'
        OnChange = cbxAlignHorizontalCenterWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3AlignRightWith: TComboBox
        Tag = 2
        Left = 161
        Top = 146
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 9
        Text = '(none)'
        OnChange = cbxAlignRightWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3RightOf: TComboBox
        Tag = 2
        Left = 161
        Top = 170
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 10
        Text = '(none)'
        OnChange = cbxRightOfChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3Above: TComboBox
        Tag = 2
        Left = 396
        Top = 74
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 11
        Text = '(none)'
        OnChange = cbxAboveChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3AlignTopWith: TComboBox
        Tag = 2
        Left = 396
        Top = 98
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 12
        Text = '(none)'
        OnChange = cbxAlignTopWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3AlignVerticalCenterWith: TComboBox
        Tag = 2
        Left = 396
        Top = 122
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 13
        Text = '(none)'
        OnChange = cbxAlignVerticalCenterWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3AlignBottomWith: TComboBox
        Tag = 2
        Left = 396
        Top = 146
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 14
        Text = '(none)'
        OnChange = cbxAlignBottomWithChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
      object cbx3Below: TComboBox
        Tag = 2
        Left = 396
        Top = 170
        Width = 70
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 15
        Text = '(none)'
        OnChange = cbxBelowChange
        Items.Strings = (
          '(none)'
          'Edit1'
          'Button1')
      end
    end
  end
end
