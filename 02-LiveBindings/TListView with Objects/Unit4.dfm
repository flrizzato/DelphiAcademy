object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 415
  ClientWidth = 559
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
    Left = 409
    Top = 0
    Width = 150
    Height = 415
    Align = alClient
    Caption = 
      'This demo shows 1000 objects representing employees from differe' +
      'nt offices. With LiveBindings we can easily group this data and ' +
      'show a great summary of the data'
    WordWrap = True
    ExplicitWidth = 149
    ExplicitHeight = 78
  end
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 409
    Height = 415
    Align = alLeft
    Columns = <
      item
        Caption = 'First Name'
        Width = 100
      end
      item
        Caption = 'Last Name'
        Width = 100
      end
      item
        Caption = 'Office'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Salary'
        Width = 80
      end>
    Groups = <
      item
        Header = 'Software Engineer'
        GroupID = 0
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Receptionist'
        GroupID = 1
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Accountant'
        GroupID = 2
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Product Specialist'
        GroupID = 3
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Sales Manager'
        GroupID = 4
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Vice President'
        GroupID = 5
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Sales Manager'
        GroupID = 6
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end
      item
        Header = 'Account Manager'
        GroupID = 7
        State = [lgsNormal]
        HeaderAlign = taLeftJustify
        FooterAlign = taLeftJustify
        TitleImage = -1
      end>
    Items.ItemData = {
      05960400000A00000000000000FFFFFFFFFFFFFFFF0400000002000000040000
      00084200690074006D006100700031003400084200690074006D006100700031
      003400000000000F4D0065006400690075006D00760069006F006C0065007400
      7200650064000000000005380031002E00380031000000000000000000000000
      0000FFFFFFFFFFFFFFFF040000000100000003000000084200690074006D0061
      00700031003600084200690074006D00610070003100360000000000104D0065
      006400690075006D0061007100750061006D006100720069006E006500000000
      0005370038002E003700380000000000000000000000000000FFFFFFFFFFFFFF
      FF040000000500000008000000084200690074006D0061007000310039000842
      00690074006D006100700031003900000000000442006C007500650000000000
      04360030002E00360000000000000000000000000000FFFFFFFFFFFFFFFF0400
      0000070000000A000000084200690074006D0061007000320032000842006900
      74006D006100700032003200000000000A50006F00770064006500720062006C
      00750065000000000005310035002E0031003500000000000000000000000000
      00FFFFFFFFFFFFFFFF040000000600000009000000084200690074006D006100
      700032003500084200690074006D006100700032003500000000000B47007200
      650065006E00790065006C006C006F0077000000000005370031002E00370031
      0000000000000000000000000000FFFFFFFFFFFFFFFF04000000000000000100
      0000084200690074006D006100700032003800084200690074006D0061007000
      32003800000000000D4400610072006B0067006F006C00640065006E0072006F
      0064000000000005360037002E003600370000000000000000000000000000FF
      FFFFFFFFFFFFFF040000000300000005000000084200690074006D0061007000
      34003100084200690074006D006100700034003100000000000A470068006F00
      73007400770068006900740065000000000005330037002E0033003700000000
      00000000000000000000FFFFFFFFFFFFFFFF0400000004000000060000000842
      00690074006D006100700034003300084200690074006D006100700034003300
      000000000A530061006E0064007900620072006F0077006E0000000000053400
      31002E003400310000000000000000000000000000FFFFFFFFFFFFFFFF040000
      000000000002000000084200690074006D006100700034003600084200690074
      006D0061007000340036000000000005490076006F0072007900000000000537
      0039002E003700390000000000000000000000000000FFFFFFFFFFFFFFFF0400
      00000500000007000000084200690074006D0061007000340037000842006900
      74006D006100700034003700000000000E43006F0072006E0066006C006F0077
      006500720062006C00750065000000000005340033002E003400330000000000
      0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    GroupView = True
    SortType = stBoth
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Button1: TButton
    Left = 448
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Total >>'
    TabOrder = 1
    OnClick = Button1Click
  end
  object PrototypeBindSource1: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    RecordCount = 10
    FieldDefs = <
      item
        Name = 'FirstName'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'LastName'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'Office'
        Generator = 'ContactTitles'
        ReadOnly = False
      end
      item
        Name = 'Salary'
        FieldType = ftCurrency
        Generator = 'Currency'
        ReadOnly = False
      end
      item
        Name = 'Team'
        Generator = 'ColorsNames'
        ReadOnly = False
      end>
    ScopeMappings = <>
    OnCreateAdapter = PrototypeBindSource1CreateAdapter
    Left = 464
    Top = 120
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 468
    Top = 181
    object LinkFillControlToField1: TLinkFillControlToField
      Category = 'Quick Bindings'
      Control = ListView1
      Track = True
      FillDataSource = PrototypeBindSource1
      FillDisplayFieldName = 'FirstName'
      AutoFill = True
      FillExpressions = <
        item
          SourceMemberName = 'LastName'
          ControlMemberName = 'SubItems[0]'
        end
        item
          SourceMemberName = 'Team'
          ControlMemberName = 'SubItems[1]'
        end
        item
          SourceMemberName = 'Salary'
          ControlMemberName = 'SubItems[2]'
        end>
      FillHeaderExpressions = <>
      FillHeaderFieldName = 'Office'
      FillBreakGroups = <>
    end
  end
end
