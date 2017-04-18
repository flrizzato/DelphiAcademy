object Form1: TForm1
  Left = 0
  Top = 0
  ActiveControl = dbgJSON
  Caption = 'DataSet -> JSON'
  ClientHeight = 601
  ClientWidth = 813
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 813
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object butDSJSON: TButton
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 125
      Height = 40
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'DataSet -> JSON'
      TabOrder = 0
      OnClick = butDSJSONClick
    end
  end
  object memJSON: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 259
    Width = 807
    Height = 339
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object dbgJSON: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 53
    Width = 807
    Height = 200
    Align = alTop
    DataSource = SalesTableDS
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    Connected = True
    LoginPrompt = False
    Left = 678
    Top = 310
  end
  object SalesTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM SALES')
    Left = 678
    Top = 358
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
  object SalesTableDS: TDataSource
    AutoEdit = False
    DataSet = SalesTable
    Left = 680
    Top = 408
  end
end
