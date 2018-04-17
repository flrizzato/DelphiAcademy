object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TFDMemTable'
  ClientHeight = 531
  ClientWidth = 803
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 803
    Height = 531
    ActivePage = TabSheet4
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Demo #1'
      object Button1: TButton
        Left = 24
        Top = 463
        Width = 125
        Height = 25
        Caption = 'Create'
        TabOrder = 0
        OnClick = Button1Click
      end
      object DBGrid1: TDBGrid
        Left = 24
        Top = 16
        Width = 737
        Height = 425
        DataSource = DataSource1
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Demo #2'
      ImageIndex = 1
      object DBGrid2: TDBGrid
        Left = 24
        Top = 16
        Width = 737
        Height = 401
        DataSource = DataSource2
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object Button2: TButton
        Left = 16
        Top = 431
        Width = 125
        Height = 25
        Caption = 'Load File'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 147
        Top = 431
        Width = 125
        Height = 25
        Caption = 'Save File'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 147
        Top = 462
        Width = 125
        Height = 25
        Caption = 'Save Stream'
        TabOrder = 3
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 16
        Top = 462
        Width = 125
        Height = 25
        Caption = 'Load Stream'
        TabOrder = 4
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 636
        Top = 431
        Width = 125
        Height = 25
        Caption = 'Close DataSet'
        TabOrder = 5
        OnClick = Button6Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Demo #3'
      ImageIndex = 2
      object Label1: TLabel
        Left = 16
        Top = 472
        Width = 53
        Height = 13
        Caption = 'Rec #0000'
      end
      object DBGrid3: TDBGrid
        Left = 16
        Top = 24
        Width = 369
        Height = 393
        DataSource = DataSource3
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object DBGrid4: TDBGrid
        Left = 409
        Top = 24
        Width = 369
        Height = 393
        DataSource = DataSource4
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object Button7: TButton
        Left = 147
        Top = 431
        Width = 125
        Height = 25
        Caption = '.CopyDataSet'
        TabOrder = 2
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 278
        Top = 431
        Width = 125
        Height = 25
        Caption = '.Data'
        TabOrder = 3
        OnClick = Button8Click
      end
      object Button9: TButton
        Left = 16
        Top = 431
        Width = 125
        Height = 25
        Caption = 'Create'
        TabOrder = 4
        OnClick = Button9Click
      end
      object Button10: TButton
        Left = 653
        Top = 431
        Width = 125
        Height = 25
        Caption = 'Close DataSet'
        TabOrder = 5
        OnClick = Button10Click
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Demo #4'
      ImageIndex = 3
      object DBGrid5: TDBGrid
        Left = 16
        Top = 24
        Width = 369
        Height = 417
        DataSource = DataSource5
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object DBGrid6: TDBGrid
        Left = 409
        Top = 24
        Width = 369
        Height = 417
        DataSource = DataSource6
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object Button11: TButton
        Left = 16
        Top = 463
        Width = 125
        Height = 25
        Caption = 'Load'
        TabOrder = 2
        OnClick = Button11Click
      end
      object Button12: TButton
        Left = 147
        Top = 463
        Width = 125
        Height = 25
        Caption = '.CloneCursor'
        TabOrder = 3
        OnClick = Button12Click
      end
      object Button13: TButton
        Left = 278
        Top = 463
        Width = 125
        Height = 25
        Caption = '.Filter'
        TabOrder = 4
        OnClick = Button13Click
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Demo #5'
      ImageIndex = 4
      object Button14: TButton
        Left = 32
        Top = 455
        Width = 125
        Height = 25
        Caption = 'Create 1'
        TabOrder = 0
        OnClick = Button14Click
      end
      object DBGrid7: TDBGrid
        Left = 32
        Top = 24
        Width = 737
        Height = 417
        DataSource = DataSource7
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object Button15: TButton
        Left = 163
        Top = 455
        Width = 125
        Height = 25
        Caption = 'Create 2'
        TabOrder = 2
        OnClick = Button15Click
      end
      object Button16: TButton
        Left = 644
        Top = 455
        Width = 125
        Height = 25
        Caption = 'Close DataSet'
        TabOrder = 3
        OnClick = Button16Click
      end
    end
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 136
    Top = 64
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 64
    Top = 64
  end
  object FDMemTable2: TFDMemTable
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvPersistent, rvSilentMode]
    ResourceOptions.Persistent = True
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 64
    Top = 136
  end
  object DataSource2: TDataSource
    DataSet = FDMemTable2
    Left = 136
    Top = 136
  end
  object FDStanStorageXMLLink1: TFDStanStorageXMLLink
    Left = 232
    Top = 136
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 360
    Top = 136
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 488
    Top = 136
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 604
    Top = 136
  end
  object FileSaveDialog1: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 688
    Top = 136
  end
  object FDMemTable3: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 64
    Top = 216
  end
  object DataSource3: TDataSource
    DataSet = FDMemTable3
    Left = 136
    Top = 216
  end
  object FDMemTable4: TFDMemTable
    AfterPost = FDMemTable4AfterPost
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 216
    Top = 216
  end
  object DataSource4: TDataSource
    DataSet = FDMemTable4
    Left = 288
    Top = 216
  end
  object FDMemTable5: TFDMemTable
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 64
    Top = 288
  end
  object DataSource5: TDataSource
    DataSet = FDMemTable5
    Left = 136
    Top = 288
  end
  object FDMemTable6: TFDMemTable
    AfterPost = FDMemTable4AfterPost
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 216
    Top = 288
  end
  object DataSource6: TDataSource
    DataSet = FDMemTable6
    Left = 288
    Top = 288
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 640
    Top = 64
  end
  object FDMemTable7: TFDMemTable
    AfterPost = FDMemTable4AfterPost
    CachedUpdates = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 64
    Top = 352
  end
  object DataSource7: TDataSource
    DataSet = FDMemTable7
    Left = 136
    Top = 352
  end
end
