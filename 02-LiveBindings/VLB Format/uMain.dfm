object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 398
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object NavigatorPrototypeBindSource1: TBindNavigator
    Left = 8
    Top = 8
    Width = 600
    Height = 25
    DataSource = PrototypeBindSource1
    Orientation = orHorizontal
    TabOrder = 0
  end
  object StringGrid1: TStringGrid
    Tag = 2
    Left = 8
    Top = 48
    Width = 600
    Height = 329
    ColCount = 2
    DefaultColWidth = 200
    FixedCols = 0
    RowCount = 201
    TabOrder = 1
  end
  object PrototypeBindSource1: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'ContactName1'
        Generator = 'ContactNames'
        ReadOnly = False
        CustomFormat = '"Mr " + UpperCase(%s)'
      end
      item
        Name = 'CurrencyField1'
        FieldType = ftCurrency
        Generator = 'Currency'
        ReadOnly = False
      end>
    ScopeMappings = <>
    Left = 528
    Top = 56
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 420
    Top = 53
    object LinkGridToDataSourcePrototypeBindSource1: TLinkGridToDataSource
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      GridControl = StringGrid1
      AutoBufferCount = False
      Columns = <>
    end
  end
end
