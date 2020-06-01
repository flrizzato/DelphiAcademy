object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'JSON Writer'
  ClientHeight = 443
  ClientWidth = 792
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 8
    Top = 8
    Width = 776
    Height = 193
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PO_NUMBER'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CUST_NO'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SALES_REP'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ORDER_STATUS'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ORDER_DATE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SHIP_DATE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DATE_NEEDED'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PAID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'QTY_ORDERED'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TOTAL_VALUE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DISCOUNT'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ITEM_TYPE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'AGED'
        Visible = True
      end>
  end
  object Button1: TButton
    Left = 8
    Top = 207
    Width = 85
    Height = 25
    Caption = 'Open'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 99
    Top = 208
    Width = 85
    Height = 25
    Caption = 'JSON'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 239
    Width = 776
    Height = 196
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object Button3: TButton
    Left = 190
    Top = 207
    Width = 85
    Height = 25
    Caption = 'Class Helper'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = Button3Click
  end
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 80
    Top = 40
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 184
    Top = 40
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 288
    Top = 40
  end
  object DataSource1: TDataSource
    AutoEdit = False
    DataSet = SalesTable
    Left = 80
    Top = 136
  end
  object FDBatchMove1: TFDBatchMove
    Reader = FDBatchMoveDataSetReader1
    Writer = FDBatchMoveJSONWriter1
    Mappings = <>
    LogFileName = 'C:\DelphiAcademy\40-RADServer1\FDBatchMoveJSONWriter\Data.log'
    Left = 88
    Top = 256
  end
  object FDBatchMoveDataSetReader1: TFDBatchMoveDataSetReader
    DataSet = SalesTable
    Optimise = False
    Left = 88
    Top = 304
  end
  object FDBatchMoveJSONWriter1: TFDBatchMoveJSONWriter
    FileName = 'C:\DelphiAcademy\40-RADServer1\FDBatchMoveJSONWriter\temp.json'
    DataDef.Fields = <>
    DataDef.DateTimeZoneHandling = Utc
    DataDef.Formatting = Indented
    DataDef.DateFormatHandling = FormatSettings
    Encoding = ecANSI
    Left = 88
    Top = 352
  end
  object SalesTable: TFDQuery
    Connection = EmployeeConnection
    FetchOptions.AssignedValues = [evMode, evItems, evRowsetSize, evCache, evUnidirectional]
    FetchOptions.RowsetSize = 100
    FetchOptions.Items = [fiBlobs, fiDetails]
    FetchOptions.Cache = []
    SQL.Strings = (
      'SELECT * FROM SALES ORDER BY PO_NUMBER')
    Left = 80
    Top = 88
    object SalesTablePO_NUMBER: TStringField
      FieldName = 'PO_NUMBER'
      Origin = 'PO_NUMBER'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      FixedChar = True
      Size = 8
    end
    object SalesTableCUST_NO: TIntegerField
      FieldName = 'CUST_NO'
      Origin = 'CUST_NO'
      Required = True
    end
    object SalesTableSALES_REP: TSmallintField
      FieldName = 'SALES_REP'
      Origin = 'SALES_REP'
    end
    object SalesTableORDER_STATUS: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'ORDER_STATUS'
      Origin = 'ORDER_STATUS'
      Size = 7
    end
    object SalesTableORDER_DATE: TSQLTimeStampField
      AutoGenerateValue = arDefault
      FieldName = 'ORDER_DATE'
      Origin = 'ORDER_DATE'
    end
    object SalesTableSHIP_DATE: TSQLTimeStampField
      FieldName = 'SHIP_DATE'
      Origin = 'SHIP_DATE'
    end
    object SalesTableDATE_NEEDED: TSQLTimeStampField
      FieldName = 'DATE_NEEDED'
      Origin = 'DATE_NEEDED'
    end
    object SalesTablePAID: TStringField
      AutoGenerateValue = arDefault
      FieldName = 'PAID'
      Origin = 'PAID'
      FixedChar = True
      Size = 1
    end
    object SalesTableQTY_ORDERED: TIntegerField
      AutoGenerateValue = arDefault
      FieldName = 'QTY_ORDERED'
      Origin = 'QTY_ORDERED'
    end
    object SalesTableTOTAL_VALUE: TCurrencyField
      FieldName = 'TOTAL_VALUE'
      Origin = 'TOTAL_VALUE'
      Required = True
    end
    object SalesTableDISCOUNT: TSingleField
      AutoGenerateValue = arDefault
      FieldName = 'DISCOUNT'
      Origin = 'DISCOUNT'
    end
    object SalesTableITEM_TYPE: TStringField
      FieldName = 'ITEM_TYPE'
      Origin = 'ITEM_TYPE'
      Required = True
      Size = 12
    end
    object SalesTableAGED: TFMTBCDField
      AutoGenerateValue = arDefault
      FieldName = 'AGED'
      Origin = 'AGED'
      ProviderFlags = []
      ReadOnly = True
      Precision = 18
      Size = 9
    end
  end
end
