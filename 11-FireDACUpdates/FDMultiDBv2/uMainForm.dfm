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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 748
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 0
    object btnConnIB: TSpeedButton
      Tag = 1
      Left = 16
      Top = 17
      Width = 85
      Height = 22
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
    end
    object btnConnMSSQL: TSpeedButton
      Tag = 2
      Left = 107
      Top = 17
      Width = 85
      Height = 22
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
    end
    object btnCnnORA: TSpeedButton
      Tag = 3
      Left = 198
      Top = 17
      Width = 85
      Height = 22
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
    end
    object btnConnDB2: TSpeedButton
      Tag = 4
      Left = 289
      Top = 17
      Width = 85
      Height = 22
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
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 543
    Width = 748
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
    Height = 486
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
    Width = 611
    Height = 486
    Align = alClient
    TabOrder = 3
    object DBGrid1: TDBGrid
      Left = 1
      Top = 26
      Width = 609
      Height = 149
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
      Top = 330
      Width = 609
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
      Top = 175
      Width = 609
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
      Width = 609
      Height = 25
      DataSource = dtsQry
      Align = alTop
      TabOrder = 3
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
    Left = 176
    Top = 104
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
    Left = 240
    Top = 104
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
    Left = 240
    Top = 152
  end
  object ADPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 656
    Top = 88
  end
  object ADGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 656
    Top = 280
  end
  object ADPhysOracleDriverLink1: TFDPhysOracleDriverLink
    Left = 656
    Top = 184
  end
  object ADPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 656
    Top = 136
  end
  object FDPhysDB2DriverLink1: TFDPhysDB2DriverLink
    Left = 656
    Top = 232
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
    Left = 248
    Top = 416
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
    Left = 248
    Top = 464
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
    Left = 248
    Top = 264
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
    Left = 248
    Top = 312
  end
end
