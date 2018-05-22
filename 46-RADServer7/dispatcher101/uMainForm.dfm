object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Message Dispatcher'
  ClientHeight = 414
  ClientWidth = 662
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 25
    Width = 662
    Height = 339
    Align = alClient
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
        FieldName = 'TOTAL_VALUE'
        Width = 88
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
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 0
    Width = 662
    Height = 25
    DataSource = DataSource1
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
    Align = alTop
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 364
    Width = 662
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 16
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Send Push'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 76
    Top = 78
  end
  object SalesTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM SALES')
    Left = 76
    Top = 126
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
  object DataSource1: TDataSource
    AutoEdit = False
    DataSet = SalesTable
    Left = 72
    Top = 176
  end
  object EMSProvider1: TEMSProvider
    AndroidPush.GCMAppID = '899125331012'
    ApiVersion = '1'
    ApplicationId = 'DelphiAcademyAppID'
    AppSecret = 'DelphiAcademyAppSecret'
    MasterSecret = 'DelphiAcademyMasterSecret'
    URLHost = '192.168.0.13'
    URLPort = 8080
    Left = 176
    Top = 80
  end
  object BackendPush1: TBackendPush
    Provider = EMSProvider1
    Extras = <>
    Left = 176
    Top = 136
  end
end
