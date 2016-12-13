object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'FireDAC Cross-Plataform Database Access'
  ClientHeight = 396
  ClientWidth = 586
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
    Left = 0
    Top = 57
    Width = 586
    Height = 320
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 586
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      586
      57)
    object SpeedButton1: TSpeedButton
      Tag = 1
      Left = 16
      Top = 17
      Width = 65
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
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Tag = 2
      Left = 87
      Top = 17
      Width = 65
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
      OnClick = SpeedButton1Click
    end
    object SpeedButton3: TSpeedButton
      Tag = 3
      Left = 158
      Top = 17
      Width = 65
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
      OnClick = SpeedButton1Click
    end
    object SpeedButton4: TSpeedButton
      Tag = 4
      Left = 229
      Top = 17
      Width = 65
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
      OnClick = SpeedButton1Click
    end
    object Button1: TButton
      Left = 496
      Top = 17
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Update >>'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 377
    Width = 586
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
  object FDCnn: TFDConnection
    ConnectionName = 'UniversalCnn'
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
      end>
    LoginPrompt = False
    Left = 48
    Top = 88
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
    UpdateOptions.RefreshMode = rmManual
    UpdateOptions.CheckRequired = False
    UpdateOptions.CheckReadOnly = False
    UpdateOptions.CheckUpdatable = False
    UpdateOptions.KeyFields = 'CUSTOMERID'
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
    Left = 48
    Top = 136
    object FDQryCUSTOMERID: TIntegerField
      FieldName = 'CUSTOMERID'
      Origin = 'CUSTOMERID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
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
  object DataSource1: TDataSource
    DataSet = FDQry
    Left = 48
    Top = 184
  end
  object ADPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 144
    Top = 88
  end
  object ADGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 256
    Top = 88
  end
  object ADPhysOracleDriverLink1: TFDPhysOracleDriverLink
    Left = 144
    Top = 200
  end
  object ADPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 144
    Top = 144
  end
  object FDPhysDB2DriverLink1: TFDPhysDB2DriverLink
    Left = 144
    Top = 256
  end
end
