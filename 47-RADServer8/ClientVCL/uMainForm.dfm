object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Client VCL'
  ClientHeight = 460
  ClientWidth = 700
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
    Width = 153
    Height = 435
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 21
      Width = 22
      Height = 13
      Caption = 'User'
    end
    object Label2: TLabel
      Left = 16
      Top = 74
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object edtUser: TEdit
      Left = 16
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edtPass: TEdit
      Left = 16
      Top = 93
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object butLogin: TButton
      Left = 16
      Top = 136
      Width = 121
      Height = 25
      Caption = 'Login'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = butLoginClick
    end
    object butGetCountry: TButton
      Left = 16
      Top = 176
      Width = 121
      Height = 25
      Caption = 'Get Country'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = butGetCountryClick
    end
  end
  object DBGrid1: TDBGrid
    Left = 153
    Top = 0
    Width = 547
    Height = 435
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'COUNTRY'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CURRENCY'
        Width = 130
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 435
    Width = 700
    Height = 25
    DataSource = DataSource1
    Align = alBottom
    TabOrder = 2
  end
  object BackendEndpointGET: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Params = <>
    Resource = 'country'
    Response = RESTResponse1
    Left = 312
    Top = 64
  end
  object EMSProvider1: TEMSProvider
    ApiVersion = '1'
    ApplicationId = 'DelphiAcademyAppID'
    AppSecret = 'A020ED3F-5BB2-405C-BA0A-DE556055C49F'
    LoginResource = 'Users'
    URLHost = 'localhost'
    URLPort = 8080
    Left = 192
    Top = 64
  end
  object RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter
    Dataset = FDMemTable1
    FieldDefs = <>
    ResponseJSON = RESTResponse1
    Left = 312
    Top = 176
  end
  object BackendAuth1: TBackendAuth
    Provider = EMSProvider1
    LoginPrompt = False
    UserDetails = <>
    DefaultAuthentication = Application
    Left = 192
    Top = 120
  end
  object FDMemTable1: TFDMemTable
    BeforePost = FDMemTable1BeforePost
    AfterPost = FDMemTable1AfterPost
    AfterDelete = FDMemTable1AfterDelete
    FieldDefs = <>
    CachedUpdates = True
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 312
    Top = 232
    object FDMemTable1COUNTRY: TStringField
      FieldName = 'COUNTRY'
      Origin = 'COUNTRY'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      Size = 15
    end
    object FDMemTable1CURRENCY: TStringField
      FieldName = 'CURRENCY'
      Origin = 'CURRENCY'
      Required = True
      Size = 10
    end
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 312
    Top = 288
  end
  object RESTResponse1: TRESTResponse
    Left = 312
    Top = 120
  end
  object BackendEndpointACT: TBackendEndpoint
    Provider = EMSProvider1
    Auth = BackendAuth1
    Method = rmPOST
    Params = <>
    Resource = 'country'
    Left = 432
    Top = 64
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 616
    Top = 16
  end
end
