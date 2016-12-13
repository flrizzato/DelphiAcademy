object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'FireDAC Cross-Plataform Database Access'
  ClientHeight = 562
  ClientWidth = 748
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 748
    Height = 562
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = '  FireDAC  '
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 740
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 0
        object btnConnIB: TSpeedButton
          Tag = 1
          AlignWithMargins = True
          Left = 10
          Top = 10
          Width = 85
          Height = 37
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alLeft
          AllowAllUp = True
          GroupIndex = 1
          Caption = 'IB'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = btnConnIBClick
          ExplicitLeft = 16
          ExplicitTop = 17
          ExplicitHeight = 22
        end
        object btnConnMSSQL: TSpeedButton
          Tag = 2
          AlignWithMargins = True
          Left = 115
          Top = 10
          Width = 85
          Height = 37
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alLeft
          AllowAllUp = True
          GroupIndex = 1
          Caption = 'MSSQL'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = btnConnMSSQLClick
          ExplicitLeft = 107
          ExplicitTop = 17
          ExplicitHeight = 22
        end
        object btnCnnORA: TSpeedButton
          Tag = 3
          AlignWithMargins = True
          Left = 220
          Top = 10
          Width = 85
          Height = 37
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alLeft
          AllowAllUp = True
          GroupIndex = 1
          Caption = 'ORA'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = btnCnnORAClick
          ExplicitLeft = 198
          ExplicitTop = 17
          ExplicitHeight = 22
        end
        object btnConnDB2: TSpeedButton
          Tag = 4
          AlignWithMargins = True
          Left = 325
          Top = 10
          Width = 85
          Height = 37
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alLeft
          AllowAllUp = True
          GroupIndex = 1
          Caption = 'DB2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = btnConnDB2Click
          ExplicitLeft = 289
          ExplicitTop = 17
          ExplicitHeight = 22
        end
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 515
        Width = 740
        Height = 19
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Panels = <>
        SimplePanel = True
        UseSystemFont = False
      end
      object Panel2: TPanel
        Left = 0
        Top = 57
        Width = 137
        Height = 458
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
        object btnViewCache: TButton
          Left = 16
          Top = 26
          Width = 105
          Height = 25
          Caption = 'View cache'
          TabOrder = 0
          OnClick = btnViewCacheClick
        end
        object btnUndoLastChange: TButton
          Left = 16
          Top = 86
          Width = 105
          Height = 25
          Caption = 'Undo last change'
          TabOrder = 1
          OnClick = btnUndoLastChangeClick
        end
        object btnRevert: TButton
          Left = 16
          Top = 116
          Width = 105
          Height = 25
          Caption = 'Revert record'
          TabOrder = 2
          OnClick = btnRevertClick
        end
        object btnCancelUpdates: TButton
          Left = 16
          Top = 146
          Width = 105
          Height = 25
          Caption = 'Cancel Updates'
          TabOrder = 3
          OnClick = btnCancelUpdatesClick
        end
        object btnCreateSavePoint: TButton
          Left = 16
          Top = 176
          Width = 105
          Height = 25
          Caption = 'Create savepoint'
          TabOrder = 4
          OnClick = btnCreateSavePointClick
        end
        object btnBackToSavePoint: TButton
          Left = 16
          Top = 206
          Width = 105
          Height = 25
          Caption = 'Back to SPoint'
          TabOrder = 5
          OnClick = btnBackToSavePointClick
        end
        object btnApplyUpdates: TButton
          Left = 16
          Top = 236
          Width = 105
          Height = 25
          Caption = 'Apply Updates'
          TabOrder = 6
          OnClick = btnApplyUpdatesClick
        end
        object btnOldValue: TButton
          Left = 16
          Top = 56
          Width = 105
          Height = 25
          Caption = 'View Old Value'
          TabOrder = 7
          OnClick = btnOldValueClick
        end
      end
      object Panel3: TPanel
        Left = 137
        Top = 57
        Width = 603
        Height = 458
        Align = alClient
        TabOrder = 3
        object DBGrid1: TDBGrid
          Left = 1
          Top = 26
          Width = 601
          Height = 121
          Align = alClient
          DataSource = dtsQry
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          Columns = <
            item
              Expanded = False
              FieldName = 'CUSTOMERID'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'FIRSTNAME'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'LASTNAME'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'COMPANY'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'ADDRESS'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'CITY'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'STATE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'COUNTRY'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'POSTALCODE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'PHONE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'FAX'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'EMAIL'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'SUPPORTREPID'
              Visible = True
            end>
        end
        object DBGrid2: TDBGrid
          Left = 1
          Top = 302
          Width = 601
          Height = 155
          Align = alBottom
          DataSource = dtsDet
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          Columns = <
            item
              Expanded = False
              FieldName = 'INVOICEID'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'CUSTOMERID'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'INVOICEDATE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'BILLINGADDRESS'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'BILLINGCITY'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'BILLINGSTATE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'BILLINGCOUNTRY'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'BILLINGPOSTALCODE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'TOTAL'
              Visible = True
            end>
        end
        object DBGrid3: TDBGrid
          Left = 1
          Top = 147
          Width = 601
          Height = 155
          Align = alBottom
          Color = clSkyBlue
          DataSource = dtsCache
          DrawingStyle = gdsClassic
          TabOrder = 2
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          Columns = <
            item
              Expanded = False
              FieldName = 'Status'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'CUSTOMERID'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'FIRSTNAME'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'LASTNAME'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'COMPANY'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'ADDRESS'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'CITY'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'STATE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'COUNTRY'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'POSTALCODE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'PHONE'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'FAX'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'EMAIL'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'SUPPORTREPID'
              Visible = True
            end>
        end
        object DBNavigator1: TDBNavigator
          Left = 1
          Top = 1
          Width = 601
          Height = 25
          DataSource = dtsQry
          Align = alTop
          TabOrder = 3
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = '  Tracing and Monitoring  '
      ImageIndex = 1
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 740
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 0
        object butEnvReport: TSpeedButton
          Tag = 1
          AlignWithMargins = True
          Left = 333
          Top = 10
          Width = 105
          Height = 37
          Margins.Left = 20
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alLeft
          AllowAllUp = True
          GroupIndex = 1
          Caption = 'Env Report'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = butEnvReportClick
          ExplicitLeft = 328
          ExplicitTop = 16
          ExplicitHeight = 22
        end
        object butDBMSInfo: TSpeedButton
          Tag = 1
          AlignWithMargins = True
          Left = 468
          Top = 10
          Width = 105
          Height = 37
          Margins.Left = 20
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alLeft
          AllowAllUp = True
          GroupIndex = 1
          Caption = 'DBMS Info'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          OnClick = butDBMSInfoClick
          ExplicitLeft = 439
          ExplicitTop = 16
          ExplicitHeight = 22
        end
        object tgsTracing: TToggleSwitch
          AlignWithMargins = True
          Left = 20
          Top = 10
          Width = 117
          Height = 37
          Margins.Left = 20
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          StateCaptions.CaptionOn = 'Tracing On'
          StateCaptions.CaptionOff = 'Tracing Off'
          TabOrder = 0
          OnClick = tgsTracingClick
          ExplicitHeight = 20
        end
        object tgsMonitoring: TToggleSwitch
          AlignWithMargins = True
          Left = 167
          Top = 10
          Width = 136
          Height = 37
          Margins.Left = 20
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          StateCaptions.CaptionOn = 'Monitoring On'
          StateCaptions.CaptionOff = 'Monitoring Off'
          TabOrder = 1
          OnClick = tgsMonitoringClick
          ExplicitHeight = 20
        end
      end
      object memLog: TMemo
        AlignWithMargins = True
        Left = 5
        Top = 62
        Width = 730
        Height = 467
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Align = alClient
        Color = clInfoBk
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
  end
  object FDCnn: TFDConnection
    ConnectionName = 'UniversalCnn'
    Params.Strings = (
      'Database=C:\IB_XE7\FIREDAC.IB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IB')
    FormatOptions.AssignedValues = [fvMapRules]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtInt32
      end
      item
        SourceDataType = dtWideString
        TargetDataType = dtAnsiString
      end
      item
        SourceDataType = dtDateTimeStamp
        TargetDataType = dtDateTime
      end
      item
        SourceDataType = dtDate
        TargetDataType = dtDateTime
      end>
    LoginPrompt = False
    Left = 560
    Top = 128
  end
  object FDQry: TFDQuery
    CachedUpdates = True
    OnUpdateError = FDQryUpdateError
    Connection = FDCnn
    FetchOptions.AssignedValues = [evItems]
    FetchOptions.Items = [fiBlobs, fiDetails]
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate, uvUpdateChngFields, uvUpdateMode, uvLockMode, uvLockWait, uvRefreshMode, uvCheckRequired, uvCheckReadOnly, uvCheckUpdatable]
    UpdateOptions.UpdateChangedFields = False
    UpdateOptions.LockWait = True
    UpdateOptions.CheckRequired = False
    UpdateOptions.CheckReadOnly = False
    UpdateOptions.CheckUpdatable = False
    UpdateOptions.KeyFields = 'CUSTOMERID'
    UpdateOptions.AutoIncFields = 'CUSTOMERID'
    SQL.Strings = (
      'SELECT'
      '    t.CUSTOMERID,'
      '    t.FIRSTNAME,'
      '    t.LASTNAME,'
      '    t.COMPANY,'
      '    t.ADDRESS,'
      '    t.CITY,'
      '    t.STATE,'
      '    t.COUNTRY,'
      '    t.POSTALCODE,'
      '    t.PHONE,'
      '    t.FAX,'
      '    t.EMAIL,'
      '    t.SUPPORTREPID'
      'FROM CUSTOMER t'
      'ORDER BY t.CUSTOMERID')
    Left = 560
    Top = 176
    object FDQryCUSTOMERID: TFDAutoIncField
      FieldName = 'CUSTOMERID'
      Origin = 'CUSTOMERID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object FDQryFIRSTNAME: TStringField
      FieldName = 'FIRSTNAME'
      Origin = 'FIRSTNAME'
      Required = True
      Size = 40
    end
    object FDQryLASTNAME: TStringField
      FieldName = 'LASTNAME'
      Origin = 'LASTNAME'
      Required = True
    end
    object FDQryCOMPANY: TStringField
      FieldName = 'COMPANY'
      Origin = 'COMPANY'
      Size = 80
    end
    object FDQryADDRESS: TStringField
      FieldName = 'ADDRESS'
      Origin = 'ADDRESS'
      Size = 70
    end
    object FDQryCITY: TStringField
      FieldName = 'CITY'
      Origin = 'CITY'
      Size = 40
    end
    object FDQrySTATE: TStringField
      FieldName = 'STATE'
      Origin = 'STATE'
      Size = 40
    end
    object FDQryCOUNTRY: TStringField
      FieldName = 'COUNTRY'
      Origin = 'COUNTRY'
      Size = 40
    end
    object FDQryPOSTALCODE: TStringField
      FieldName = 'POSTALCODE'
      Origin = 'POSTALCODE'
      Size = 10
    end
    object FDQryPHONE: TStringField
      FieldName = 'PHONE'
      Origin = 'PHONE'
      Size = 24
    end
    object FDQryFAX: TStringField
      FieldName = 'FAX'
      Origin = 'FAX'
      Size = 24
    end
    object FDQryEMAIL: TStringField
      FieldName = 'EMAIL'
      Origin = 'EMAIL'
      Required = True
      Size = 60
    end
    object FDQrySUPPORTREPID: TIntegerField
      FieldName = 'SUPPORTREPID'
      Origin = 'SUPPORTREPID'
    end
  end
  object dtsQry: TDataSource
    DataSet = FDQry
    Left = 560
    Top = 224
  end
  object ADPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 656
    Top = 128
  end
  object ADGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 656
    Top = 320
  end
  object ADPhysOracleDriverLink1: TFDPhysOracleDriverLink
    Left = 656
    Top = 224
  end
  object ADPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 656
    Top = 176
  end
  object FDPhysDB2DriverLink1: TFDPhysDB2DriverLink
    Left = 656
    Top = 272
  end
  object FDDet: TFDQuery
    CachedUpdates = True
    IndexFieldNames = 'CUSTOMERID'
    MasterSource = dtsQry
    MasterFields = 'CUSTOMERID'
    DetailFields = 'CUSTOMERID'
    Connection = FDCnn
    FetchOptions.AssignedValues = [evItems, evCache]
    FetchOptions.Items = [fiBlobs, fiDetails]
    UpdateOptions.AssignedValues = [uvEDelete, uvEInsert, uvEUpdate, uvRefreshMode]
    UpdateOptions.EnableDelete = False
    UpdateOptions.EnableInsert = False
    UpdateOptions.EnableUpdate = False
    UpdateOptions.KeyFields = 'INVOICEID'
    UpdateOptions.AutoIncFields = 'INVOICEID'
    SQL.Strings = (
      'SELECT'
      #9't.INVOICEID,'
      #9't.CUSTOMERID,'
      #9't.INVOICEDATE,'
      #9't.BILLINGADDRESS,'
      #9't.BILLINGCITY,'
      #9't.BILLINGSTATE,'
      #9't.BILLINGCOUNTRY,'
      #9't.BILLINGPOSTALCODE,'
      #9't.TOTAL'
      ' FROM INVOICE t '
      'WHERE t.CUSTOMERID = :CUSTOMERID')
    Left = 560
    Top = 368
    ParamData = <
      item
        Name = 'CUSTOMERID'
        DataType = ftInteger
        ParamType = ptInput
      end>
    object FDDetINVOICEID: TIntegerField
      FieldName = 'INVOICEID'
      Origin = 'INVOICEID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object FDDetCUSTOMERID: TIntegerField
      FieldName = 'CUSTOMERID'
      Origin = 'CUSTOMERID'
      Required = True
    end
    object FDDetINVOICEDATE: TDateTimeField
      FieldName = 'INVOICEDATE'
      Origin = 'INVOICEDATE'
    end
    object FDDetBILLINGADDRESS: TStringField
      FieldName = 'BILLINGADDRESS'
      Origin = 'BILLINGADDRESS'
      Size = 70
    end
    object FDDetBILLINGCITY: TStringField
      FieldName = 'BILLINGCITY'
      Origin = 'BILLINGCITY'
      Size = 40
    end
    object FDDetBILLINGSTATE: TStringField
      FieldName = 'BILLINGSTATE'
      Origin = 'BILLINGSTATE'
      Size = 40
    end
    object FDDetBILLINGCOUNTRY: TStringField
      FieldName = 'BILLINGCOUNTRY'
      Origin = 'BILLINGCOUNTRY'
      Size = 40
    end
    object FDDetBILLINGPOSTALCODE: TStringField
      FieldName = 'BILLINGPOSTALCODE'
      Origin = 'BILLINGPOSTALCODE'
      Size = 10
    end
    object FDDetTOTAL: TBCDField
      FieldName = 'TOTAL'
      Origin = 'TOTAL'
      Required = True
      Precision = 18
      Size = 2
    end
  end
  object dtsDet: TDataSource
    DataSet = FDDet
    Left = 560
    Top = 416
  end
  object FDCache: TFDMemTable
    OnCalcFields = FDCacheCalcFields
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 560
    Top = 272
    object FDCacheStatus: TStringField
      FieldKind = fkCalculated
      FieldName = 'Status'
      Size = 10
      Calculated = True
    end
    object FDCacheCUSTOMERID: TFDAutoIncField
      FieldName = 'CUSTOMERID'
      Origin = 'CUSTOMERID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
    end
    object FDCacheFIRSTNAME: TStringField
      FieldName = 'FIRSTNAME'
      Origin = 'FIRSTNAME'
      Required = True
      Size = 40
    end
    object FDCacheLASTNAME: TStringField
      FieldName = 'LASTNAME'
      Origin = 'LASTNAME'
      Required = True
    end
    object FDCacheCOMPANY: TStringField
      FieldName = 'COMPANY'
      Origin = 'COMPANY'
      Size = 80
    end
    object FDCacheADDRESS: TStringField
      FieldName = 'ADDRESS'
      Origin = 'ADDRESS'
      Size = 70
    end
    object FDCacheCITY: TStringField
      FieldName = 'CITY'
      Origin = 'CITY'
      Size = 40
    end
    object FDCacheSTATE: TStringField
      FieldName = 'STATE'
      Origin = 'STATE'
      Size = 40
    end
    object FDCacheCOUNTRY: TStringField
      FieldName = 'COUNTRY'
      Origin = 'COUNTRY'
      Size = 40
    end
    object FDCachePOSTALCODE: TStringField
      FieldName = 'POSTALCODE'
      Origin = 'POSTALCODE'
      Size = 10
    end
    object FDCachePHONE: TStringField
      FieldName = 'PHONE'
      Origin = 'PHONE'
      Size = 24
    end
    object FDCacheFAX: TStringField
      FieldName = 'FAX'
      Origin = 'FAX'
      Size = 24
    end
    object FDCacheEMAIL: TStringField
      FieldName = 'EMAIL'
      Origin = 'EMAIL'
      Required = True
      Size = 60
    end
    object FDCacheSUPPORTREPID: TIntegerField
      FieldName = 'SUPPORTREPID'
      Origin = 'SUPPORTREPID'
    end
  end
  object dtsCache: TDataSource
    AutoEdit = False
    DataSet = FDCache
    Left = 560
    Top = 320
  end
  object FDMonitor: TFDMoniRemoteClientLink
    Left = 232
    Top = 464
  end
  object FDTrace: TFDMoniFlatFileClientLink
    FileAppend = True
    ShowTraces = False
    Left = 180
    Top = 424
  end
end
