object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'FireDAC - Update Join'
  ClientHeight = 475
  ClientWidth = 835
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
    Width = 835
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 0
    object btnConnIB: TSpeedButton
      Tag = 1
      AlignWithMargins = True
      Left = 5
      Top = 15
      Width = 85
      Height = 27
      Margins.Left = 5
      Margins.Top = 15
      Margins.Right = 0
      Margins.Bottom = 15
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
      Left = 95
      Top = 15
      Width = 85
      Height = 27
      Margins.Left = 5
      Margins.Top = 15
      Margins.Right = 0
      Margins.Bottom = 15
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
    object btnConnORA: TSpeedButton
      Tag = 3
      AlignWithMargins = True
      Left = 185
      Top = 15
      Width = 85
      Height = 27
      Margins.Left = 5
      Margins.Top = 15
      Margins.Right = 0
      Margins.Bottom = 15
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
      OnClick = btnConnORAClick
      ExplicitLeft = 198
      ExplicitTop = 17
      ExplicitHeight = 22
    end
    object btnConnDB2: TSpeedButton
      Tag = 4
      AlignWithMargins = True
      Left = 275
      Top = 15
      Width = 85
      Height = 27
      Margins.Left = 5
      Margins.Top = 15
      Margins.Right = 0
      Margins.Bottom = 15
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
    object btnApply: TButton
      AlignWithMargins = True
      Left = 730
      Top = 15
      Width = 100
      Height = 27
      Margins.Left = 5
      Margins.Top = 15
      Margins.Right = 5
      Margins.Bottom = 15
      Align = alRight
      Caption = 'Apply Updates'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = btnApplyClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 456
    Width = 835
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
  object DBGrid1: TDBGrid
    Left = 0
    Top = 57
    Width = 835
    Height = 374
    Align = alClient
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PLAYLIST'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ARTIST'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ALBUM'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TRACKID'
        Width = 85
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NAME'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'COMPOSER'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'UNITPRICE'
        Width = 85
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'GENRE'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'MEDIA'
        Width = 125
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'MILLISECONDS'
        Width = 85
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BYTES'
        Width = 85
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 431
    Width = 835
    Height = 25
    DataSource = DataSource1
    Align = alBottom
    TabOrder = 3
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=FIREDACDB')
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
    Left = 708
    Top = 101
  end
  object FDQuery1: TFDQuery
    CachedUpdates = True
    Connection = FDConnection1
    UpdateObject = FDUpdateSQL1
    SQL.Strings = (
      'SELECT PLA.NAME AS "PLAYLIST",'
      '       ART.NAME AS "ARTIST",'
      '       ALB.TITLE AS "ALBUM",'
      '       TRA.TRACKID,'
      '       TRA.NAME,'
      '       TRA.COMPOSER,'
      '       TRA.UNITPRICE,'
      '       GEN.NAME AS "GENRE",'
      '       MED.NAME AS "MEDIA",'
      '       TRA.MILLISECONDS,'
      '       TRA.BYTES'
      '  FROM TRACK TRA'
      'INNER JOIN PLAYLISTTRACK PTR ON (TRA.TRACKID = PTR.TRACKID)'
      'INNER JOIN PLAYLIST PLA ON (PTR.PLAYLISTID = PLA.PLAYLISTID)'
      'INNER JOIN ALBUM ALB ON (TRA.ALBUMID = ALB.ALBUMID)'
      'INNER JOIN ARTIST ART ON (ALB.ARTISTID = ART.ARTISTID)'
      'INNER JOIN MEDIATYPE MED ON (TRA.MEDIATYPEID = MED.MEDIATYPEID)'
      'INNER JOIN GENRE GEN ON (TRA.GENREID = GEN.GENREID)'
      'ORDER BY PLA.NAME, ART.NAME, ALB.TITLE, TRA.TRACKID')
    Left = 56
    Top = 104
    object FDQuery1PLAYLIST: TStringField
      FieldName = 'PLAYLIST'
      Origin = 'PLAYLIST'
      Size = 120
    end
    object FDQuery1ARTIST: TStringField
      FieldName = 'ARTIST'
      Origin = 'ARTIST'
      Size = 120
    end
    object FDQuery1ALBUM: TStringField
      FieldName = 'ALBUM'
      Origin = 'ALBUM'
      Required = True
      Size = 160
    end
    object FDQuery1TRACKID: TIntegerField
      FieldName = 'TRACKID'
      Origin = 'TRACKID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object FDQuery1NAME: TStringField
      FieldName = 'NAME'
      Origin = 'NAME'
      Required = True
      Size = 200
    end
    object FDQuery1COMPOSER: TStringField
      FieldName = 'COMPOSER'
      Origin = 'COMPOSER'
      Size = 220
    end
    object FDQuery1UNITPRICE: TBCDField
      FieldName = 'UNITPRICE'
      Origin = 'UNITPRICE'
      Required = True
      Precision = 18
      Size = 2
    end
    object FDQuery1GENRE: TStringField
      FieldName = 'GENRE'
      Origin = 'GENRE'
      Size = 120
    end
    object FDQuery1MEDIA: TStringField
      FieldName = 'MEDIA'
      Origin = 'MEDIA'
      Size = 120
    end
    object FDQuery1MILLISECONDS: TIntegerField
      FieldName = 'MILLISECONDS'
      Origin = 'MILLISECONDS'
      Required = True
    end
    object FDQuery1BYTES: TIntegerField
      FieldName = 'BYTES'
      Origin = 'BYTES'
    end
  end
  object DataSource1: TDataSource
    DataSet = FDQuery1
    Left = 120
    Top = 104
  end
  object ADPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 712
    Top = 144
  end
  object ADPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 712
    Top = 192
  end
  object ADPhysOracleDriverLink1: TFDPhysOracleDriverLink
    Left = 712
    Top = 240
  end
  object FDPhysDB2DriverLink1: TFDPhysDB2DriverLink
    Left = 712
    Top = 288
  end
  object ADGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 712
    Top = 336
  end
  object FDUpdateSQL1: TFDUpdateSQL
    Connection = FDConnection1
    InsertSQL.Strings = (
      'INSERT INTO TRACK'
      '(TRACKID, NAME, COMPOSER, MILLISECONDS, BYTES, '
      '  UNITPRICE)'
      
        'VALUES (:NEW_TRACKID, :NEW_NAME, :NEW_COMPOSER, :NEW_MILLISECOND' +
        'S, :NEW_BYTES, '
      '  :NEW_UNITPRICE)')
    ModifySQL.Strings = (
      'UPDATE TRACK'
      
        'SET TRACKID = :NEW_TRACKID, NAME = :NEW_NAME, COMPOSER = :NEW_CO' +
        'MPOSER, '
      
        '  MILLISECONDS = :NEW_MILLISECONDS, BYTES = :NEW_BYTES, UNITPRIC' +
        'E = :NEW_UNITPRICE'
      'WHERE TRACKID = :OLD_TRACKID')
    DeleteSQL.Strings = (
      'DELETE FROM TRACK'
      'WHERE TRACKID = :OLD_TRACKID')
    FetchRowSQL.Strings = (
      
        'SELECT TRACKID, NAME, ALBUMID, MEDIATYPEID, GENREID, COMPOSER, M' +
        'ILLISECONDS, '
      '  BYTES, UNITPRICE'
      'FROM TRACK'
      'WHERE TRACKID = :TRACKID')
    Left = 56
    Top = 152
  end
end
