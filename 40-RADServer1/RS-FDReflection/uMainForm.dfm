object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'FD Reflection Client'
  ClientHeight = 461
  ClientWidth = 693
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object StringGridBindSourceDB1: TStringGrid
    Left = 0
    Top = 50
    Width = 693
    Height = 386
    Align = alClient
    ColCount = 1
    DrawingStyle = gdsGradient
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
    ExplicitTop = 53
  end
  object NavigatorBindSourceDB1: TBindNavigator
    Left = 0
    Top = 436
    Width = 693
    Height = 25
    DataSource = BindSourceDB1
    Align = alBottom
    Orientation = orHorizontal
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 693
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object butUpdateCustomers: TButton
      AlignWithMargins = True
      Left = 225
      Top = 5
      Width = 100
      Height = 40
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Post'
      TabOrder = 0
      OnClick = butUpdateCustomersClick
    end
    object butGetCustomers: TButton
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 100
      Height = 40
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Get All'
      TabOrder = 1
      OnClick = butGetCustomersClick
    end
    object butGetOneCustomer: TButton
      AlignWithMargins = True
      Left = 115
      Top = 5
      Width = 100
      Height = 40
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Get One'
      TabOrder = 2
      OnClick = butGetOneCustomerClick
    end
  end
  object EMSProvider1: TEMSProvider
    ApiVersion = '1'
    URLHost = '127.0.0.1'
    URLPort = 8080
    Left = 568
    Top = 8
  end
  object EMSFireDACClient1: TEMSFireDACClient
    Resource = 'RSFDReflection'
    Provider = EMSProvider1
    SchemaAdapter = FDSchemaAdapter1
    Left = 568
    Top = 64
  end
  object FDSchemaAdapter1: TFDSchemaAdapter
    Left = 568
    Top = 120
  end
  object FDTableAdapter1: TFDTableAdapter
    SchemaAdapter = FDSchemaAdapter1
    DatSTableName = 'CustomerTable'
    Left = 568
    Top = 176
  end
  object FDMemTable1: TFDMemTable
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Adapter = FDTableAdapter1
    Left = 568
    Top = 232
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 568
    Top = 288
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 500
    Top = 357
    object LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource
      DataSource = BindSourceDB1
      GridControl = StringGridBindSourceDB1
      Columns = <>
    end
  end
  object BindSourceDB1: TBindSourceDB
    DataSet = FDMemTable1
    ScopeMappings = <>
    Left = 584
    Top = 360
  end
end
