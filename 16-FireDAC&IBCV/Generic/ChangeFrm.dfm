object frmChange: TfrmChange
  Left = 0
  Top = 0
  Caption = 'InterBase XE7 Change Views'
  ClientHeight = 660
  ClientWidth = 672
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
    Left = 374
    Top = 243
    Width = 8
    Height = 13
    Caption = '--'
  end
  object Label2: TLabel
    Left = 454
    Top = 243
    Width = 16
    Height = 13
    Caption = 'Old'
  end
  object Label3: TLabel
    Left = 476
    Top = 243
    Width = 8
    Height = 13
    Caption = '--'
  end
  object Label4: TLabel
    Left = 526
    Top = 243
    Width = 17
    Height = 13
    Caption = 'Cur'
  end
  object Label5: TLabel
    Left = 548
    Top = 243
    Width = 8
    Height = 13
    Caption = '--'
  end
  object Label6: TLabel
    Left = 597
    Top = 243
    Width = 21
    Height = 13
    Caption = 'New'
  end
  object Label7: TLabel
    Left = 620
    Top = 243
    Width = 8
    Height = 13
    Caption = '--'
  end
  object Label8: TLabel
    Left = 374
    Top = 447
    Width = 8
    Height = 13
    Caption = '--'
  end
  object Label9: TLabel
    Left = 454
    Top = 447
    Width = 16
    Height = 13
    Caption = 'Old'
  end
  object Label10: TLabel
    Left = 476
    Top = 447
    Width = 8
    Height = 13
    Caption = '--'
  end
  object Label11: TLabel
    Left = 526
    Top = 447
    Width = 17
    Height = 13
    Caption = 'Cur'
  end
  object Label12: TLabel
    Left = 548
    Top = 447
    Width = 8
    Height = 13
    Caption = '--'
  end
  object Label13: TLabel
    Left = 597
    Top = 447
    Width = 21
    Height = 13
    Caption = 'New'
  end
  object Label14: TLabel
    Left = 620
    Top = 447
    Width = 8
    Height = 13
    Caption = '--'
  end
  object Label15: TLabel
    Left = 8
    Top = 243
    Width = 56
    Height = 13
    Caption = 'Data mode:'
  end
  object Label16: TLabel
    Left = 176
    Top = 243
    Width = 71
    Height = 13
    Caption = 'Refresh mode:'
  end
  object Label17: TLabel
    Left = 91
    Top = 13
    Width = 258
    Height = 13
    Caption = 'Read Readme.txt in project folder for details !'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label18: TLabel
    Left = 8
    Top = 37
    Width = 65
    Height = 13
    Caption = 'Original data:'
  end
  object Label19: TLabel
    Left = 8
    Top = 266
    Width = 66
    Height = 13
    Caption = 'Change View:'
  end
  object Label20: TLabel
    Left = 8
    Top = 495
    Width = 74
    Height = 13
    Caption = '"Remote" data:'
  end
  object grdChanges: TDBGrid
    Left = 8
    Top = 285
    Width = 657
    Height = 150
    DataSource = dsChanges
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnColEnter = grdChangesColEnter
    OnDrawColumnCell = grdRemoteDrawColumnCell
  end
  object grdRemote: TDBGrid
    Left = 8
    Top = 512
    Width = 657
    Height = 140
    DataSource = dsRemote
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnColEnter = grdRemoteColEnter
    OnDrawColumnCell = grdRemoteDrawColumnCell
  end
  object btnMergeData: TButton
    Left = 8
    Top = 441
    Width = 67
    Height = 25
    Caption = 'Merge Data'
    TabOrder = 2
    OnClick = btnMergeDataClick
  end
  object btnCommitUpd: TButton
    Left = 154
    Top = 441
    Width = 67
    Height = 25
    Caption = 'CommitUpd'
    TabOrder = 3
    OnClick = btnCommitUpdClick
  end
  object grdOriginal: TDBGrid
    Left = 8
    Top = 56
    Width = 657
    Height = 173
    DataSource = dsOriginal
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object btnOpenDB: TButton
    Left = 8
    Top = 8
    Width = 67
    Height = 25
    Caption = 'Open DB'
    TabOrder = 5
    OnClick = btnOpenDBClick
  end
  object btnMergeDelta: TButton
    Left = 81
    Top = 441
    Width = 67
    Height = 25
    Caption = 'Merge Delta'
    TabOrder = 6
    OnClick = btnMergeDeltaClick
  end
  object btnUndoLast: TButton
    Left = 228
    Top = 441
    Width = 67
    Height = 25
    Caption = 'UndoLast'
    TabOrder = 7
    OnClick = btnUndoLastClick
  end
  object cbChangesData: TComboBox
    Left = 70
    Top = 239
    Width = 100
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 8
    Text = 'Original + delta'
    OnChange = cbChangesDataChange
    Items.Strings = (
      'Original'
      'Original + delta'
      'Delta')
  end
  object cbChangesRefresh: TComboBox
    Left = 253
    Top = 239
    Width = 100
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 9
    Text = 'On alert'
    OnChange = cbChangesRefreshChange
    Items.Strings = (
      'None'
      'On alert'
      'On timeout')
  end
  object cbxUseStream: TCheckBox
    Left = 13
    Top = 472
    Width = 120
    Height = 17
    Caption = 'Merge using stream'
    TabOrder = 10
  end
  object conOriginal: TFDConnection
    Params.Strings = (
      'Protocol=TCPIP'
      'Server=127.0.0.1'
      'Database=c:\data\sub.ib'
      'User_Name=sysdba'
      'Password=masterkey'
      'ExtendedMetadata=True'
      'DriverID=IB')
    Left = 40
    Top = 88
  end
  object qOriginal: TFDQuery
    Connection = conOriginal
    SQL.Strings = (
      'select * from tab')
    Left = 120
    Top = 88
  end
  object dsOriginal: TDataSource
    DataSet = qOriginal
    Left = 120
    Top = 152
  end
  object conChanges: TFDConnection
    TxOptions.Isolation = xiSnapshot
    Left = 40
    Top = 322
  end
  object qChanges: TFDQuery
    CachedUpdates = True
    FilterChanges = [rtModified, rtInserted, rtDeleted, rtUnmodified]
    ChangeAlerter = eaChanges
    Connection = conChanges
    ResourceOptions.AssignedValues = [rvStorePrettyPrint]
    ResourceOptions.StorePrettyPrint = True
    SQL.Strings = (
      'select * from tab')
    Left = 120
    Top = 322
  end
  object dsChanges: TDataSource
    DataSet = qChanges
    OnDataChange = dsChangesDataChange
    Left = 120
    Top = 378
  end
  object mtRemote: TFDMemTable
    CachedUpdates = True
    FilterChanges = [rtModified, rtInserted, rtDeleted, rtUnmodified]
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 112
    Top = 512
  end
  object dsRemote: TDataSource
    DataSet = mtRemote
    OnDataChange = dsRemoteDataChange
    Left = 112
    Top = 560
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    ScreenCursor = gcrHourGlass
    Left = 296
    Top = 96
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 376
    Top = 120
  end
  object FDStanStorageXMLLink1: TFDStanStorageXMLLink
    Left = 472
    Top = 96
  end
  object eaChanges: TFDEventAlerter
    Connection = conChanges
    SubscriptionName = 'sub'
    Options.MergeData = dmDeltaMerge
    Left = 216
    Top = 320
  end
end
