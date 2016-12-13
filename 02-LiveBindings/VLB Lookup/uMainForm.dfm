object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 429
  ClientWidth = 680
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TShape
    Left = 311
    Top = 59
    Width = 345
    Height = 334
    Brush.Color = 755384
  end
  object ListBox1: TListBox
    Left = 24
    Top = 32
    Width = 281
    Height = 361
    ItemHeight = 13
    Items.Strings = (
      'Antiquewhite'
      'Aquamarine'
      'Azure'
      'Beige'
      'Bisque'
      'Black'
      'Blanchedalmond'
      'Blue'
      'Blueviolet'
      'Brown'
      'Burlywood'
      'Cadetblue'
      'Chartreuse'
      'Chocolate'
      'Coral'
      'Cornflowerblue'
      'Cornsilk'
      'Cream'
      'Crimson'
      'Cyan'
      'Darkblue'
      'Darkcyan'
      'Darkgoldenrod'
      'Darkgreen'
      'Darkgrey'
      'Darkkhaki'
      'Darkmagenta'
      'Darkolivegreen'
      'Darkorange'
      'Darkorchid'
      'Darkred'
      'Darksalmon'
      'Darkseagreen'
      'Darkslateblue'
      'Darkslategrey'
      'Darkturquoise'
      'Darkviolet'
      'Deeppink'
      'Deepskyblue'
      'Dimgrey'
      'Dodgerblue'
      'Firebrick'
      'Floralwhite'
      'Forestgreen'
      'Gainsboro'
      'Ghostwhite'
      'Gold'
      'Goldenrod'
      'Green'
      'Greenyellow'
      'Grey'
      'Honeydew'
      'Hotpink'
      'Indianred'
      'Indigo'
      'Ivory'
      'Khaki'
      'Lavender'
      'Lavenderblush'
      'Lawngreen'
      'LegacySkyBlue'
      'Lemonchiffon'
      'Lightblue'
      'Lightcoral'
      'Lightcyan'
      'Lightgoldenrodyellow'
      'Lightgreen'
      'Lightgrey'
      'Lightpink'
      'Lightsalmon'
      'Lightseagreen'
      'Lightskyblue'
      'Lightslategrey'
      'Lightsteelblue'
      'Lightyellow'
      'Lime'
      'Limegreen'
      'Linen'
      'LtGray'
      'Magenta'
      'Maroon'
      'MedGray'
      'Mediumaquamarine'
      'Mediumblue'
      'Mediumorchid'
      'Mediumpurple'
      'Mediumseagreen'
      'Mediumslateblue'
      'Mediumspringgreen'
      'Mediumturquoise'
      'Mediumvioletred'
      'Midnightblue'
      'Mintcream'
      'Mistyrose'
      'Moccasin'
      'MoneyGreen'
      'Navajowhite'
      'Navy'
      'Oldlace'
      'Olive'
      'Olivedrab'
      'Orange'
      'Orangered'
      'Orchid'
      'Palegoldenrod'
      'Palegreen'
      'Paleturquoise'
      'Palevioletred'
      'Papayawhip'
      'Peachpuff'
      'Peru'
      'Pink'
      'Plum'
      'Powderblue'
      'Purple'
      'Red'
      'Rosybrown'
      'Royalblue'
      'Saddlebrown'
      'Salmon'
      'Sandybrown'
      'Seagreen'
      'Seashell'
      'Sienna'
      'Skyblue'
      'Slateblue'
      'Slategrey'
      'Snow'
      'Springgreen'
      'Steelblue'
      'Tan'
      'Teal'
      'Thistle'
      'Tomato'
      'Turquoise'
      'Violet'
      'Wheat'
      'White'
      'Whitesmoke'
      'Yellow'
      'Yellowgreen')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 311
    Top = 32
    Width = 345
    Height = 21
    TabOrder = 1
    Text = 'Darkgoldenrod'
  end
  object PrototypeBindSource1: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    RecordCount = 10
    FieldDefs = <
      item
        Name = 'Color1'
        FieldType = ftUInteger
        Generator = 'Colors'
        ReadOnly = False
      end>
    ScopeMappings = <>
    Left = 600
    Top = 32
  end
  object PrototypeBindSourceFill: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'AColor'
        FieldType = ftUInteger
        Generator = 'Colors'
        Options = []
        ReadOnly = False
      end
      item
        Name = 'AColorsName'
        Generator = 'ColorsNames'
        Options = []
        ReadOnly = False
      end>
    ScopeMappings = <>
    Left = 600
    Top = 89
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 500
    Top = 37
    object LinkFillControlToField1: TLinkFillControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Color1'
      Control = ListBox1
      Track = True
      FillDataSource = PrototypeBindSourceFill
      FillValueFieldName = 'AColor'
      FillDisplayFieldName = 'AColorsName'
      AutoFill = True
      FillExpressions = <>
      FillHeaderExpressions = <>
      FillBreakGroups = <>
    end
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Color1'
      Control = Edit1
      Track = True
      LookupDataSource = PrototypeBindSourceFill
      LookupKeyFieldName = 'AColor'
      LookupValueFieldName = 'AColorsName'
    end
    object LinkPropertyToFieldBrushColor: TLinkPropertyToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Color1'
      Component = Shape1
      ComponentProperty = 'Brush.Color'
    end
  end
end
