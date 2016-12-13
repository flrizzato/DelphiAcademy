object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 437
  ClientWidth = 737
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
    Left = 320
    Top = 32
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 320
    Top = 72
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object ListBox1: TListBox
    Left = 48
    Top = 32
    Width = 209
    Height = 217
    ItemHeight = 13
    Items.Strings = (
      'um'
      'dois'
      'tres'
      'quatro'
      'cinco'
      'seis')
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 540
    Top = 37
    object LinkFillControlToPropertyCaption: TLinkFillControlToProperty
      Category = 'Quick Bindings'
      Track = True
      Control = ListBox1
      Component = Label1
      ComponentProperty = 'Caption'
      AutoFill = True
      FillExpressions = <>
      FillHeaderExpressions = <>
      FillBreakGroups = <>
    end
    object BindExpression1: TBindExpression
      Category = 'Binding Expressions'
      ControlComponent = Label2
      SourceComponent = ListBox1
      SourceExpression = #39'Selecionado Item: '#39' + ToStr(ItemIndex)'
      ControlExpression = 'Caption'
      NotifyOutputs = False
      Direction = dirSourceToControl
    end
  end
end
