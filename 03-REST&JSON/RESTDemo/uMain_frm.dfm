object frm_Main: Tfrm_Main
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Embarcadero REST-Library - Demos'
  ClientHeight = 756
  ClientWidth = 894
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 454
    Width = 894
    Height = 10
    Cursor = crVSplit
    Align = alBottom
    MinSize = 200
    ExplicitTop = 333
    ExplicitWidth = 939
  end
  object Button3: TButton
    Left = 736
    Top = 204
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 0
  end
  object pc_Demos: TPageControl
    Left = 0
    Top = 89
    Width = 894
    Height = 365
    ActivePage = ts_Facebook
    Align = alClient
    TabOrder = 1
    TabWidth = 95
    object ts_DelphiPRAXiS: TTabSheet
      Caption = 'Delphi-PRAXiS'
      object btn_DelphiPRAXiS: TButton
        Left = 248
        Top = 175
        Width = 273
        Height = 49
        Caption = 'fetch list of forums'
        TabOrder = 0
        OnClick = btn_DelphiPRAXiSClick
      end
      object memo_Desc_DP: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'this demo connects to the REST-'
          'service from Delphi-PRAXiS.net and '
          'fetches a list of available forums.'
          ''
          'the client uses HTTP basic '
          'authentication for access to the '
          'service.')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object edt_DP_BaseURL: TLabeledEdit
        Left = 248
        Top = 32
        Width = 273
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Base-URL:'
        TabOrder = 2
        Text = 'http://www.delphipraxis.net/api/'
      end
      object edt_DP_ResourcePath: TLabeledEdit
        Left = 248
        Top = 80
        Width = 273
        Height = 21
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Resource-URI:'
        TabOrder = 3
        Text = 'forums'
      end
      object edt_DP_Username: TLabeledEdit
        Left = 248
        Top = 132
        Width = 121
        Height = 21
        EditLabel.Width = 52
        EditLabel.Height = 13
        EditLabel.Caption = 'Username:'
        TabOrder = 4
        Text = 'BAAS'
      end
      object edt_DP_Password: TLabeledEdit
        Left = 400
        Top = 132
        Width = 121
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Password:'
        TabOrder = 5
        Text = 'test'
      end
    end
    object ts_Twitter: TTabSheet
      Caption = 'Twitter'
      ImageIndex = 1
      object memo_Desc_Twitter: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'this demo connects to the REST-'
          'service from Twitter and posts a '
          'status-update ("tweet").'
          ''
          'the client uses OAuth 1.0a for access '
          'to service.'
          ''
          'Steps for your applications:'
          '* sign in at'
          'https://dev.twitter.com'
          '* create new app at '
          'https://dev.twitter.com/apps'
          ''
          'There you will find your'
          'comsumer-key and -secret. These'
          'values are unique to your app.'
          ''
          'There you will also find the urls'
          'you need to access the api.')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object edt_Twitter_BaseURL: TLabeledEdit
        Left = 248
        Top = 32
        Width = 273
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Base-URL:'
        TabOrder = 1
        Text = 'https://api.twitter.com'
      end
      object btn_Twitter: TButton
        Left = 248
        Top = 279
        Width = 273
        Height = 25
        Caption = '#3 Send a tweet'
        TabOrder = 2
        OnClick = btn_TwitterClick
      end
      object edt_Twitter_ResourceURI: TLabeledEdit
        Left = 248
        Top = 80
        Width = 273
        Height = 21
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Resource-URI:'
        TabOrder = 3
        Text = '1.1/statuses/update.json'
      end
      object edt_Twitter_Status: TLabeledEdit
        Left = 248
        Top = 132
        Width = 161
        Height = 21
        EditLabel.Width = 82
        EditLabel.Height = 13
        EditLabel.Caption = 'Status ("tweet"):'
        TabOrder = 4
      end
      object btn_CreateUniqueTweet: TButton
        Left = 424
        Top = 130
        Width = 97
        Height = 25
        Caption = 'create tweet'
        TabOrder = 5
        OnClick = btn_CreateUniqueTweetClick
      end
      object edt_Twitter_ConsumerKey: TLabeledEdit
        Left = 536
        Top = 32
        Width = 161
        Height = 21
        EditLabel.Width = 74
        EditLabel.Height = 13
        EditLabel.Caption = 'Consumer-Key:'
        TabOrder = 6
      end
      object edt_Twitter_ConsumerSecret: TLabeledEdit
        Left = 712
        Top = 32
        Width = 161
        Height = 21
        EditLabel.Width = 87
        EditLabel.Height = 13
        EditLabel.Caption = 'Consumer-Secret:'
        TabOrder = 7
      end
      object edt_Twitter_AccessToken: TLabeledEdit
        Left = 536
        Top = 132
        Width = 161
        Height = 21
        EditLabel.Width = 70
        EditLabel.Height = 13
        EditLabel.Caption = 'Access-Token:'
        TabOrder = 8
      end
      object edt_Twitter_AccessTokenSecret: TLabeledEdit
        Left = 712
        Top = 132
        Width = 161
        Height = 21
        EditLabel.Width = 105
        EditLabel.Height = 13
        EditLabel.Caption = 'Access-Token-Secret:'
        TabOrder = 9
      end
      object btn_Twitter_RequestAuthPIN: TButton
        Left = 248
        Top = 216
        Width = 273
        Height = 25
        Caption = '#1: Get Request-Token and Auth-Code'
        TabOrder = 10
        OnClick = btn_Twitter_RequestAuthPINClick
      end
      object btn_Twitter_RequestAccessToken: TButton
        Left = 248
        Top = 248
        Width = 273
        Height = 25
        Caption = '#2 Get Access-Token'
        TabOrder = 11
        OnClick = btn_Twitter_RequestAccessTokenClick
      end
      object edt_Twitter_AuthVerifier: TLabeledEdit
        Left = 536
        Top = 184
        Width = 161
        Height = 21
        EditLabel.Width = 101
        EditLabel.Height = 13
        EditLabel.Caption = 'Auth-Verifier ("PIN"):'
        TabOrder = 12
      end
      object edt_Twitter_RequestToken: TLabeledEdit
        Left = 536
        Top = 80
        Width = 161
        Height = 21
        EditLabel.Width = 77
        EditLabel.Height = 13
        EditLabel.Caption = 'Request-Token:'
        TabOrder = 13
      end
      object edt_Twitter_RequestTokenSecret: TLabeledEdit
        Left = 712
        Top = 80
        Width = 161
        Height = 21
        EditLabel.Width = 112
        EditLabel.Height = 13
        EditLabel.Caption = 'Request-Token-Secret:'
        TabOrder = 14
      end
    end
    object ts_GoogleTasks: TTabSheet
      Caption = 'Google (Tasks)'
      ImageIndex = 2
      object memo_Desc_GoogleTasks: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'this demo connects to the REST-'
          'service from Google and shows the '
          'task-list of the current-user.'
          ''
          'the client uses OAuth 2.0 for access '
          'to service.'
          ''
          'Steps for your applications:'
          '* sign in at'
          'https://code.google.com/apis/console'
          ''
          '* create a project and register'
          'for a service (e.g. "tasks api")'
          ''
          '* the menu "api access" will show'
          'your api-key and let'#39's you request'
          'the oauth 2.0 client-id and -secret'
          ''
          '*api-documentation (e.g. "tasks") at'
          'https://developers.google.com/'
          'google-apps/tasks/')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object btn_GoogleTasks_FetchAuthToken: TButton
        Left = 248
        Top = 132
        Width = 273
        Height = 49
        Caption = 'Step #1 fetch auch-code && access-token'
        TabOrder = 1
        OnClick = btn_GoogleTasks_FetchAuthTokenClick
      end
      object edt_GoogleTasks_BaseURL: TLabeledEdit
        Left = 248
        Top = 32
        Width = 273
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Base-URL:'
        TabOrder = 2
        Text = 'https://www.googleapis.com/tasks/v1/'
      end
      object edt_GoogleTasks_AuthCode: TLabeledEdit
        Left = 536
        Top = 132
        Width = 161
        Height = 21
        EditLabel.Width = 56
        EditLabel.Height = 13
        EditLabel.Caption = 'Auth-Code:'
        TabOrder = 3
      end
      object edt_GoogleTasks_AccessToken: TLabeledEdit
        Left = 536
        Top = 80
        Width = 161
        Height = 21
        EditLabel.Width = 70
        EditLabel.Height = 13
        EditLabel.Caption = 'Access-Token:'
        TabOrder = 4
      end
      object edt_GoogleTasks_ClientID: TLabeledEdit
        Left = 536
        Top = 32
        Width = 161
        Height = 21
        EditLabel.Width = 46
        EditLabel.Height = 13
        EditLabel.Caption = 'Client-ID:'
        TabOrder = 5
      end
      object edt_GoogleTasks_ClientSecret: TLabeledEdit
        Left = 712
        Top = 32
        Width = 161
        Height = 21
        EditLabel.Width = 66
        EditLabel.Height = 13
        EditLabel.Caption = 'Client-Secret:'
        TabOrder = 6
      end
      object edt_GoogleTasks_RefreshToken: TLabeledEdit
        Left = 712
        Top = 80
        Width = 161
        Height = 21
        EditLabel.Width = 75
        EditLabel.Height = 13
        EditLabel.Caption = 'Refresh-Token:'
        TabOrder = 7
      end
      object btn_GoogleTasks_FetchLists: TButton
        Left = 248
        Top = 195
        Width = 273
        Height = 49
        Caption = 'Step #2 fetch list of tasks'
        TabOrder = 8
        OnClick = btn_GoogleTasks_FetchListsClick
      end
      object edt_GoogleTasks_ResourceURI: TLabeledEdit
        Left = 248
        Top = 80
        Width = 273
        Height = 21
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Resource-URI:'
        TabOrder = 9
        Text = 'users/@me/lists'
      end
    end
    object ts_Facebook: TTabSheet
      Caption = 'Facebook'
      ImageIndex = 4
      ExplicitLeft = 8
      ExplicitTop = 22
      object memo_Desc_Facebook: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 326
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'this demo connects to the REST-'
          'service from Facebook and shows the '
          'first 10 friends of the current-user.'
          ''
          'the client uses OAuth 2.0 for access '
          'to service.'
          ''
          'Steps for your applications:'
          '* sign in at'
          'https://developers.facebook.com'
          ''
          '* register new app at'
          'https://developers.facebook.com/apps'
          ''
          ' - Valid OAuth redirect URIs:'
          '   * http://localhost:3000/auth/facebook/callback'
          '   * https://www.facebook.com/connect/login_success.html'
          ''
          '*the page "app settings" will'
          'show your client-id and -secret'
          '(called "app-id" and "app-secret")'
          ''
          '* use "OpenGraph API"'
          ''
          '* play around with the'
          '"Graph API Explorer" where you can'
          'visually create REST-requests')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object edt_Facebook_BaseURL: TLabeledEdit
        Left = 344
        Top = 32
        Width = 273
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Base-URL:'
        TabOrder = 1
        Text = 'https://graph.facebook.com/'
      end
      object btn_Facebook_FetchAuthToken: TButton
        Left = 344
        Top = 179
        Width = 273
        Height = 49
        Caption = 'Step #1 fetch auth code'
        TabOrder = 2
        OnClick = btn_Facebook_FetchAuthTokenClick
      end
      object edt_Facebook_AppID: TLabeledEdit
        Left = 664
        Top = 32
        Width = 209
        Height = 21
        EditLabel.Width = 46
        EditLabel.Height = 13
        EditLabel.Caption = 'Client-ID:'
        TabOrder = 3
      end
      object edt_Facebook_AppSecret: TLabeledEdit
        Left = 664
        Top = 80
        Width = 209
        Height = 21
        EditLabel.Width = 66
        EditLabel.Height = 13
        EditLabel.Caption = 'Client-Secret:'
        TabOrder = 4
      end
      object edt_FaceBook_AccessToken: TLabeledEdit
        Left = 664
        Top = 130
        Width = 209
        Height = 21
        EditLabel.Width = 70
        EditLabel.Height = 13
        EditLabel.Caption = 'Access-Token:'
        Enabled = False
        TabOrder = 5
      end
      object btn_Facebook_FetchData: TButton
        Left = 344
        Top = 243
        Width = 273
        Height = 49
        Caption = 'Step #2 fetch data'
        TabOrder = 6
        OnClick = btn_Facebook_FetchDataClick
      end
      object edt_Facebook_ResourceURI: TLabeledEdit
        Left = 344
        Top = 80
        Width = 273
        Height = 21
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Resource-URI:'
        TabOrder = 7
        Text = 'me?fields=name,birthday,friends.limit(10).fields(name)'
      end
    end
    object ts_DataSet: TTabSheet
      Caption = 'Fetch to DataSet'
      ImageIndex = 5
      object memo_Desc_DataSet: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'this demo connects to the REST-'
          'service from DevExperts and shows'
          'some data in a TDataSet.'
          ''
          'the client uses a TRESTAdapter to'
          'connect to a TDataSet')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object edt_DataSet_BaseURL: TLabeledEdit
        Left = 248
        Top = 32
        Width = 273
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Base-URL:'
        TabOrder = 1
        Text = 'http://www.developer-experts.net/hdmobile/api/'
      end
      object edt_DataSet_ResourceURI: TLabeledEdit
        Left = 248
        Top = 80
        Width = 273
        Height = 21
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Resource-URI:'
        TabOrder = 2
        Text = 'mandators'
      end
      object btn_DataSet_FetchData: TButton
        Left = 248
        Top = 131
        Width = 273
        Height = 49
        Caption = 'Step #1 fetch data'
        TabOrder = 3
        OnClick = btn_DataSet_FetchDataClick
      end
      object DBGrid1: TDBGrid
        Left = 250
        Top = 230
        Width = 633
        Height = 120
        DataSource = DataSource
        TabOrder = 4
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object tc_Twine: TTabSheet
      Caption = 'Twine'
      ImageIndex = 6
      object Lbl_Temperature: TLabel
        Left = 704
        Top = 27
        Width = 75
        Height = 48
        Caption = '0'#176' F'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -40
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object edt_twine_device: TLabeledEdit
        Left = 241
        Top = 123
        Width = 376
        Height = 21
        EditLabel.Width = 46
        EditLabel.Height = 13
        EditLabel.Caption = 'Device ID'
        TabOrder = 0
        Text = '000028ac4d62135d'
      end
      object edt_twine_url: TLabeledEdit
        Left = 241
        Top = 35
        Width = 376
        Height = 21
        EditLabel.Width = 80
        EditLabel.Height = 13
        EditLabel.Caption = 'Twine Base URL:'
        TabOrder = 1
        Text = 'https://twine.cc'
      end
      object edt_twine_resource_sensors: TLabeledEdit
        Left = 241
        Top = 80
        Width = 376
        Height = 21
        EditLabel.Width = 110
        EditLabel.Height = 13
        EditLabel.Caption = 'Sensor Resource Path:'
        TabOrder = 2
        Text = '{DeviceID}/rt?cached=1&access_key={AccessKey}'
      end
      object ButtonTwine: TButton
        Left = 241
        Top = 190
        Width = 273
        Height = 49
        Caption = 'Fetch temperature'
        TabOrder = 3
        OnClick = ButtonTwineClick
      end
      object Memo1: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'This demo connects to a Twine device '
          'which sits in Developer-Experts office.'
          ''
          'Twine is a small box, which has a '
          'couple of sensors, such as for '
          'temperature.'
          ''
          'See http://supermechanical.com/'
          ''
          'Twine doesn'#39't have an official REST '
          'API yet, so this demo is basically based '
          'on some reverse engineering.'
          ''
          'The response JSON contains the data '
          'for all sensors, we just parse it for '
          'sensor 0 - which is the temperature '
          'one.'
          ''
          'To find your own DeviceID and '
          'AccessKey go to twine.cc, log in to '
          'your account. Click on "Rules" on your '
          'dashboard and select "share". You will '
          'see a html snippet like the following '
          'one which contains both values:'
          ''
          '<iframe width="220" height="235" '
          'src="https://twine.cc/000028ac4d621'
          '35d/widget/?'
          'access_key=333d3cdbf31c07b35692b'
          '8b530da"></iframe>')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 4
      end
      object edt_twine_AccessKey: TLabeledEdit
        Left = 241
        Top = 163
        Width = 376
        Height = 21
        EditLabel.Width = 54
        EditLabel.Height = 13
        EditLabel.Caption = 'Access Key'
        TabOrder = 5
        Text = '333d3cdbf31c07b35692b8b530da'
      end
    end
    object tsFoursquare: TTabSheet
      Caption = 'Foursquare'
      ImageIndex = 3
      object memo_Desc_Foursquare: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'this demo connects to the REST-'
          'service from Foursquare and shows'
          'the last checkins of the current user.'
          ''
          'the client uses OAuth 2.0 for access '
          'to service.'
          ''
          'Steps for your applications:'
          '* sign in at'
          'https://foursquare.com/developers'
          '/apps'
          ''
          '*api-documentation at'
          'https://developer.foursquare.com/'
          'docs/')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object edt_Foursquare_BaseURL: TLabeledEdit
        Left = 248
        Top = 32
        Width = 273
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Base-URL:'
        TabOrder = 1
        Text = 'https://api.foursquare.com/v2/'
      end
      object edt_Foursquare_ClientID: TLabeledEdit
        Left = 536
        Top = 32
        Width = 161
        Height = 21
        EditLabel.Width = 46
        EditLabel.Height = 13
        EditLabel.Caption = 'Client-ID:'
        TabOrder = 2
      end
      object edt_Foursquare_ClientSecret: TLabeledEdit
        Left = 712
        Top = 32
        Width = 161
        Height = 21
        EditLabel.Width = 66
        EditLabel.Height = 13
        EditLabel.Caption = 'Client-Secret:'
        TabOrder = 3
      end
      object btn_Foursquare_FetchAuthToken: TButton
        Left = 248
        Top = 131
        Width = 273
        Height = 49
        Caption = 'Step #1 fetch auch-code && access-token'
        TabOrder = 4
        OnClick = btn_Foursquare_FetchAuthTokenClick
      end
      object edt_Foursquare_AuthCode: TLabeledEdit
        Left = 536
        Top = 132
        Width = 161
        Height = 21
        EditLabel.Width = 56
        EditLabel.Height = 13
        EditLabel.Caption = 'Auth-Code:'
        Enabled = False
        TabOrder = 5
      end
      object btn_Foursquare_Fetch: TButton
        Left = 248
        Top = 195
        Width = 273
        Height = 49
        Caption = 'Step #2 fetch list of places'
        TabOrder = 6
        OnClick = btn_Foursquare_FetchClick
      end
      object edt_Foursquare_ResourceURI: TLabeledEdit
        Left = 248
        Top = 80
        Width = 273
        Height = 21
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Resource-URI:'
        TabOrder = 7
        Text = 'users/{USER_ID}/checkins'
      end
      object edt_Foursquare_AccessToken: TLabeledEdit
        Left = 536
        Top = 80
        Width = 161
        Height = 21
        EditLabel.Width = 70
        EditLabel.Height = 13
        EditLabel.Caption = 'Access-Token:'
        Enabled = False
        TabOrder = 8
      end
    end
    object ts_DropBox: TTabSheet
      Caption = 'DropBox'
      ImageIndex = 8
      object memo_Desc_DropBox: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'this demo connects to the REST-'
          'service from DropBox and shows the '
          'task-list of the current-user.'
          ''
          'the client uses OAuth 2.0 for access '
          'to service.'
          ''
          'Steps for your applications:'
          '* sign in at'
          'https://www.dropbox.com'
          '/developers/apps'
          ''
          '*api-documentation at'
          'https://www.dropbox.com/'
          'developers/core/docs')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object edt_DropBox_ClientSecret: TLabeledEdit
        Left = 712
        Top = 32
        Width = 161
        Height = 21
        EditLabel.Width = 66
        EditLabel.Height = 13
        EditLabel.Caption = 'Client-Secret:'
        TabOrder = 1
      end
      object edt_DropBox_ClientID: TLabeledEdit
        Left = 536
        Top = 32
        Width = 161
        Height = 21
        EditLabel.Width = 46
        EditLabel.Height = 13
        EditLabel.Caption = 'Client-ID:'
        TabOrder = 2
      end
      object edt_DropBox_AuthCode: TLabeledEdit
        Left = 536
        Top = 132
        Width = 161
        Height = 21
        EditLabel.Width = 56
        EditLabel.Height = 13
        EditLabel.Caption = 'Auth-Code:'
        Enabled = False
        TabOrder = 3
      end
      object edt_DropBox_BaseURL: TLabeledEdit
        Left = 248
        Top = 32
        Width = 273
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Base-URL:'
        TabOrder = 4
        Text = 'https://api.dropbox.com/1/'
      end
      object edt_DropBox_ResourceURI: TLabeledEdit
        Left = 248
        Top = 80
        Width = 273
        Height = 21
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Resource-URI:'
        TabOrder = 5
        Text = 'metadata/{ROOT}/{PATH}'
      end
      object btn_DropBox_FetchAuthCode: TButton
        Left = 248
        Top = 131
        Width = 273
        Height = 49
        Caption = 'Step #1 fetch auch-code && access-token'
        TabOrder = 6
        OnClick = edt_DropBox_FetchAuthCodeClick
      end
      object btn_DropBox_FetchData: TButton
        Left = 248
        Top = 195
        Width = 273
        Height = 49
        Caption = 'Step #2 fetch list of metadata'
        TabOrder = 7
        OnClick = edt_DropBox_FetchDataClick
      end
      object edt_DropBox_AccessToken: TLabeledEdit
        Left = 536
        Top = 80
        Width = 161
        Height = 21
        EditLabel.Width = 70
        EditLabel.Height = 13
        EditLabel.Caption = 'Access-Token:'
        Enabled = False
        TabOrder = 8
      end
    end
    object FitBit: TTabSheet
      Caption = 'FitBit'
      ImageIndex = 8
      object btn_FitBit_RequestToken: TButton
        Left = 256
        Top = 225
        Width = 273
        Height = 25
        Caption = '#1: Get Request-Token'
        TabOrder = 0
        OnClick = btn_FitBit_RequestTokenClick
      end
      object edt_FitBit_AccessToken: TLabeledEdit
        Left = 544
        Top = 140
        Width = 161
        Height = 21
        EditLabel.Width = 70
        EditLabel.Height = 13
        EditLabel.Caption = 'Access-Token:'
        TabOrder = 1
      end
      object btn_FitBit_RequestAccessToken: TButton
        Left = 256
        Top = 256
        Width = 273
        Height = 25
        Caption = '#2 Get Access-Token'
        TabOrder = 2
        OnClick = btn_FitBit_RequestAccessTokenClick
      end
      object edt_Fitbit_RequestToken: TLabeledEdit
        Left = 544
        Top = 88
        Width = 161
        Height = 21
        EditLabel.Width = 77
        EditLabel.Height = 13
        EditLabel.Caption = 'Request-Token:'
        TabOrder = 3
      end
      object edt_FitBit_AuthVerifier: TLabeledEdit
        Left = 544
        Top = 192
        Width = 161
        Height = 21
        EditLabel.Width = 101
        EditLabel.Height = 13
        EditLabel.Caption = 'Auth-Verifier ("PIN"):'
        TabOrder = 4
      end
      object edt_FitBit_ConsumerSecret: TLabeledEdit
        Left = 720
        Top = 40
        Width = 161
        Height = 21
        EditLabel.Width = 87
        EditLabel.Height = 13
        EditLabel.Caption = 'Consumer-Secret:'
        TabOrder = 5
      end
      object Button4: TButton
        Left = 256
        Top = 287
        Width = 273
        Height = 25
        Caption = '#3 Browse Activities'
        TabOrder = 6
        OnClick = Button4Click
      end
      object edt_FitBit_BaseURL: TLabeledEdit
        Left = 256
        Top = 40
        Width = 273
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Base-URL:'
        TabOrder = 7
        Text = 'https://api.fitbit.com/'
      end
      object Memo2: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 331
        Align = alLeft
        Color = clInfoBk
        Lines.Strings = (
          'this demo connects to the REST-'
          'service from FitBit.'
          ''
          'the client uses OAuth 1.0a for access '
          'to service.'
          ''
          'Steps for your applications:'
          '* sign in at'
          'https://dev.fitbit.com'
          '* create new app at '
          'https://dev.fitbit.com/apps/new'
          ''
          'There you will find your'
          'comsumer-key and -secret. These'
          'values are unique to your app.'
          ''
          'There you will also find the urls'
          'you need to access the api.')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 8
      end
      object edt_FitBit_ResourceURI: TLabeledEdit
        Left = 256
        Top = 88
        Width = 273
        Height = 21
        EditLabel.Width = 71
        EditLabel.Height = 13
        EditLabel.Caption = 'Resource-URI:'
        TabOrder = 9
        Text = '1/activities.json'
      end
      object edt_FitBit_ConsumerKey: TLabeledEdit
        Left = 535
        Top = 40
        Width = 161
        Height = 21
        EditLabel.Width = 74
        EditLabel.Height = 13
        EditLabel.Caption = 'Consumer-Key:'
        TabOrder = 10
      end
      object edt_FitBit_RequestTokenSecret: TLabeledEdit
        Left = 720
        Top = 88
        Width = 161
        Height = 21
        EditLabel.Width = 112
        EditLabel.Height = 13
        EditLabel.Caption = 'Request-Token-Secret:'
        TabOrder = 11
      end
      object edt_FitBit_AccessTokenSecret: TLabeledEdit
        Left = 720
        Top = 140
        Width = 161
        Height = 21
        EditLabel.Width = 105
        EditLabel.Height = 13
        EditLabel.Caption = 'Access-Token-Secret:'
        TabOrder = 12
      end
    end
  end
  object pnl_Header: TPanel
    Left = 0
    Top = 0
    Width = 894
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    object Image1: TImage
      Left = -1
      Top = -21
      Width = 128
      Height = 128
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000800000
        00800806000000C33E61CB0000000774494D4507D8040F0E0C3781CC457D0000
        00097048597300000B1200000B1201D2DD7EFC0000000467414D410000B18F0B
        FC6105000064B14944415478DAEC7D07985455B6F5BAA972555757E74C939364
        044154548264504C33E3981D154154941184460151142418C73C8E091524898A
        095001C9393774D34DE7AE5C75F3BF4F15CC9BE028CE3C6178DF7FB4BEDB5D5D
        D43DF7EC75D65E7B9F7DCFE5700EB561332EEB6B4896D91DB2635DF279516D68
        742EB3D9D5FB1FB875E5B1B3DDB773B57167BB03A7DBAE1E3FECF701497E697F
        966271B7B6E3B2F305F40B5BB067B552159131B864CCB22D67BB8FE7623B2700
        50327F74DB6375B1CD6FD4D4D80C4D07541345DD3D683B281D0F78804D1FC62A
        55D5EC3579CCFF67825FDACE09003CFDDCC8D95BB8E8037FD95C07F027DF544C
        B4EF9782E25E3EDC6F01D67C18FBC10BB3CFD8B19FC867BBBFAC959480B75846
        A6DA6C3EF3FBFB82814558A49FED3EFD583B270030EBF9CBFF7230CDBCFE9555
        7E40FC9B3F2840BF5B6D286A92832BAB0D73C3D7F2C292FB578C3DCBDDE51E5F
        38F836AB8409E9D96253DDE4D05863544663E62A81E39EFBE3D8A55BCF72FFFE
        BEB367BB03A7D39E7C76D8335C1765DC84176A01EBDFFF8D37815EB714E0AAD6
        1A52BF118CA387B59BA7DEB7E28DB3D5D7C7E60D7AC6D5D432765B3AC71D0D29
        68EA6B408BDC02745435546C50B51327E41953C77D56729687F4AFED9C00C0E3
        0B878F2CEAA27F78FBB20A2E5CC7FF4DAF4D080260715A30E24E37AEC94A41D9
        E278CCEFD7FB4DB9E7936FCF743FA7CD1F711D97ADFF65B5B79EFA19861153D0
        D0604375831DCD3BDA70F3A03464AC55CCE3878DAB1F1EBBFC83B33DAEAC9D13
        00287971A823D3C297BFEF89FBBE5E5177D20D98E0451E164987C90BC8CEB260
        E06F7271530AF0D55BF172D3A6777DF8E64F6ACF541F1F98DDDFE97258F77ED7
        5E28282B0FC09465C42380A08651D76847A44143CB41F928E9C9E3E03BF26793
        EF5939E06C8F2B6BE70400587B72FEB085F2F9E2DD8FBC4842DF42C6173832BE
        09ABD5200058E8423874E86CC39D57C9C83F9A677EB54CF960EABDCBAE3E53FD
        2B997BE5ED9196EA0B5FE9B59C123610090AE09418B4988ADA46176201024027
        03336E1771EC8556C7EFBBF3CF05677B4C593B6700F0F8D3C33A67B732363D59
        7A8CDFBF47822009B05A7448140188225D8660875D8863C0F05CDCD25EC39EA5
        16E34419864CBA67C927BF76DF98E23752876C6FB8B4B6FDD1E38DA8AA77A3A6
        D1014109211AE1100EF1880480CC166E3C786726C477B9C67B6F7DCF77B6C794
        B5730600ACAF24067F08F53AD1F5B19774B85C4830809DD840923802040F2B01
        213FCF81FEFDD3D05F94B0F8B5E8FAA96397F702F317BF629BF2CCC87E7A3EF7
        E9265F25678BD5E0686D06E261154ACC4034642214E6110D72286CEFC1CC3136
        54BEE83CF8E0DD1FB63CDB039A18D4B3DD815FD2662CBCFAEE967DEA168C5F59
        C3458344FB92151E7B1C9C20C2C640200AF4BB811BAEF5A0D8EA43D98A98597E
        149DA6DDBB74C7AFD9AF494F0F59255F121BB0B7BA9142D348C2E707C805F843
        5668D12882C400F4365AF774E0A91B2DD8FE92B479D2984FBB9DEDF164ED9C02
        40C9CB037CE992EDD886E661D7E26551B89D80C71627FAB7921B10880D284A14
        35DC79AD088977C377CC866F57C9134AC67FF2D4AFD5A747670DE9686BAA6DD9
        5214E7AB2A7444C9FF874202C261201E55A0446562000E864C1AA5AF17538639
        B1EF3571E5E47B3F1A7CB6C793B5730A00AC4D9F3BE495AC81CE9BA7AEA8821B
        7ED82C02343869F64BF03963704A32065FEA826477A065C481B5EF6A2F4CBE6F
        E99DBF567F66CC1BF28C7E817DDC975521E81105A1888E48C424E3ABD0E23104
        83E4FFA3F44195C3B5BF55716B876C7CF717F1AD29E356FEEE6C8F256BE71C00
        662DBCB25D5A96BAFD40174358BCA496FCBF95E89F87C3A641140438AC1A7A74
        C94056BA8A9631016B1719AF968C5B7ECBAFD11726FEEC99430FF1C323C59B76
        36A02E6823FAB724588001204C6008874DA80A1259CBD1B7A7E3B63C011B1763
        D6E47B57FEF16C8F256BE71C00589BF9CCA0778B06D9AEF94656B0637D0DB180
        018377C14A3AC0EB52D1A97D2ABC293A0AAA396C5DC14D9B7AFFF2925FA31F4F
        CE1FD12CB5903BB0B150E3ABCA1B1121E157EBB7438D47E10F22E106A2110D86
        41C32C9BB8F5E1661811D5B0658D79C794714B5E3ADBE3C8DA390980C7E70C6B
        EE48D5B7A55F6B386B038D58B5D6848DD761B159E0B473E8D2560011035276A7
        E2C0468C9A7ADFC78B7F8D7E4C9F377278715775F13A29C2D59C08A321C0A336
        6081128F93083448FC01B1180B40689809042FCC4F47EA777673FF4EBDCF94F1
        4BCF78A6F2C7DA390900D61E9B37E0EEDC62EB02E93291AB209AADADA820F1C5
        43B2F0282CB0C325A9706D4A358F1E31DA96DCB36CDFAFD18769F387DCD6F642
        FB4BABA2411C2F8F93FF5769E69300A46388857F34FB35953E68004D8A44BC36
        D9831D6FDAE20AE74D9F70C39F23677B0C593B6701C0DA630B863D9291654EC9
        EB6D15EDB90D38D028930138988A09AF9B03FF75861E562329136EF8EC5719EC
        92F9836FEEDEC7F2CA3A298AEF3751DC2FC7511FE048F533DF4FAF28B926323E
        A9540CBFC28A0943D2F1D54BF8E691714B2F39DB6377AA9DD30060EDD105C37A
        733A7F5F8ACF18D4AE97C5E6CF69C0FA436164782578D63495AB34C15B72D3EB
        F15FE3DC33E78EEA5D7C9EBED6E8CE71CFBDDF88785C43E0E4CC0F860CC4E227
        F34F31E089477D68E37763EBD7C6D8A9E3972C38DBE376AA9DF30038D5263D35
        A848E4C5495959C20D5917C5AC5F35D6A2D3913CB3A1CCDEE2C1B1EF1DFE35CE
        59F2FE688BB55E3EDCEF766BFE13CBFD387C2484467243411283018A04349AF9
        2C07999561C58A193EAC7BD38829D568FAE0832BABCEF6789D6AFF670070AACD
        7A7A4847AB47F8CA7F617D6A7ED083BA0DD607FF3876F1EC5FEB7CD39E19725B
        B3B6FC8B29BD35EEE18575F093B309D0EC0F454D988C00C80D944CC8470FD162
        6E58253F5772FFF231677B8CFEB69D71003CF5C2D517294AE47A8ED7CEE30CA7
        3D6E44032684FD3C67F95E36B8CF668EFFE8C47F7A8E990B863F9EDE439978CC
        1686679D77F743772D6BFF6B5D0FCB05989E41EFF7B8CC3AAA3635CC95BC1C43
        69553C11F6413771C3D5A9B8BD87075FBD1FDE2D59CCDE13EF581D38B323FED3
        ED8C01A0FF03FD9D1E9FF9A7B6CD3DD77A5A44394FA11D3E97803C5248929FC7
        893298C70EA9467DB5FE2D27F1AF1922DE2FB96359F4DF39D7D4A747B62C6862
        EE5D9D77941F585F68569672173C3CE6E30DBFD6B5CD9933DA1E86BABCED50F5
        D25542398AD4148A0438F46DE1048E5ACD4D5FC6B6C1A20E9D7CE79715676ABC
        4FB79D29007057CCE8B3A4CE691DFAC39130878042EF70706458D0A49D1303FB
        A838BFD083F622C0476DD8BF43350F6C37AAE5B8B9C0EAE6163C74CBD2D02F3D
        E193CF8ED87CBC7D6397A61E11F135CE57268E5B7AEBAF798133E65F39A1756F
        FDC92BAB9721CF6BC5FD591D61DF94A5D656EACF1A0E4CFA77C1FC6BB7330280
        FBE7F5EFAAE5077F98B752E320FCCDCA2CFB514FBE320A45F4BF44458B36C5E8
        94CEA11981A16CAB616E5BAF1C5714DC376DFC2F2BA19A357FE49494F3E5692B
        EB6B31A03E275477446E5252F269C3CFFDBBD1EF8F16DA9F902F1245A3AF8513
        DB9137CFE5E0B2A97A3C686AE2C6382CAF4E1FBF78EFDFFE9BDB5FEC2A35D3F3
        767A46D5B5BA73E3662C6EDD0F7B964B1B1549BF7BDA3D4B379D8931FE77DB19
        01C0947903FAA675097FF1C7A50A17F51BFF74565E60953D80CDC241B458D0A1
        A30B57F496D13CCB8B229DC7B64F0DB3EC205E355383634A6EFAFAB442BA479F
        1DDABD654B7EC303FB8E72B37BE79B47D6F2E3268D5BF693E1D7F4F9C3875A45
        6E4E717B34AB76A95C59D0805FD361F36828C8B220C710CCF07A49ADABD5264F
        1ABBFCAFC2F2D1B9236EECDC5D7A7592B68EEB90E7C0851B3A46030D68F5D0D8
        8F8E9F89F1FD4FDA1901C0AC176F4FB1DB2A2B3E6DCA3B57BE5C0E58F9BFFE8D
        A31F6D647C87C384C3C6C14E2F8B8555FB0868D7C28EEE5DDC287459C1ED92CD
        6DEBE29F1AB9EEE125572F527EEE9C25A3475B7C43948AD5AEBAF4B64D79A46F
        48DDBDE10FD68EFFAA3EFFD1B9C36ECE2FE45E6A1CDC28CC7AD58FDA0DCC1FFD
        CFF0A4E508E831A4016D2E70A3EBF262F3D011F90F53C6AD786934F9FFEE5665
        4FF635FE26372CDB8A8F2EEE8D3D1F5B5F9D3C7EC9AFB200F5BFDDCE98089C39
        7FE85CF785F67BE76E0AE0C877B500CD768E4BCE7C8783A3230F8F4387D526C1
        ED30081422DC76F637177A76F5A1654610C1F536F3C026EDC1C9E3969DD6FAFE
        E30B072D352E90872E5C578579E7B532F7EED1064C1DB3F4F37FFCDCAC79430B
        3D29FC9E7DD757393F7CCE878A6D35D4BFBFFF8CDD66A2A8C0055F2B077A0CAD
        4493578A038ACAB78CC9DAAD3D2FB24E1F737C0397952EE06E7F1763E761ADC7
        8CFF72EA3FD5CE18001E7AE8F294F426D6CDF691F666CF7E701C7B37A9B0B819
        0068B6DB449AFD3C45057170921B5EA7025E72202F250CBBDD02A7C3863E1D44
        A4714EAC7935B27DF298959D4EE79C33E68F9CD0FC72F9C96BE657E0A587D250
        BFDCBDF48FF77C3CFC1F3F376BDEC8C7D307C61E5A91B19EDBF95C4B1C3AAAFF
        CF1D48A71A79AEFC263EF4E91A4265EF1DB8A5BCAF59B65A78D5EDE1AE1686D6
        B8C7CCAFC49FC6E5A3FEEDD4AD13EF59DEE54C8DEB7FDACE681E60FA8241DD3C
        4EEE4B6E98D3FDFCA25A1CDC1D829B54BA93E8DFE752132EC0103CC400409627
        8E54A70E3B6902C16A474EBA0B1D0A056C7F2B2EE7DAEDCEABAFFEF95BAD1E99
        3BAA77B7AEDCDA9BBE3BC89DDF51C4B59126DAA14AA9F5F47FC80C3EB960D8DE
        BD7F38DCDAB5C78277DF75A1B62EFACF236398B8E0420EBE1407728B03E03A18
        683AAFD8EC73ADC00DFC68074402ED075D7BE0BB2F94FB1EB977D9DC3339AEFF
        493BE389A0990B070DF1A6081F784689D6973F8D60D7E620D2BC061C769ED8C0
        4A542B245C809740E171F0A4071C487369487719685D68C58EF744CDACBBD435
        76ECD89FBD07F0895786B9BD2EBE76B9B3D2BAECCB083EBEABD8DCBE829F3B65
        DCD2FB4F7D66F6EC91991985A87871E016B1CBFE5CACDD68C38E2DC1BFBF058D
        8C9F9DABC3E3CE42F7AEC104731D6B538559C72FC473CA0EBC3E9BA8BF4443A7
        CD2DD5C6A342E1830F2EFAAF49F5FE5C3B2BA9E092E7868E48771A6FE78F74D8
        571FA9C7EAAF14380900696E1576F2FD56AB8DFC3F093732BC6871258E0C004D
        B281F5AF9A9109B7AE729DEEB9663D3B78977C59B0DDD4A9514C9C21217B4D56
        BDBCCF2C7EE8C9646E61DABC1197B5EA84CFC765EFE67EA789D877B835366C68
        40201881229B0440035959268A8B0D686A365C3E3B8ADAEDC161178F6BD3BDB8
        FDBE3020011F4FCEC1AE37EC5F4CBA77D9E567634CFFDD76D6D602A62D1C7299
        D36ABED76B149FB6438860D5572A4C4D23FF6FC266B340B238C90D28705859E5
        2F8F0C8F81FC141E9F3F6F39F6C7BB3E6A72BAE799B160C85F0A46C5AFBF6152
        03727B45F146B7F6E6776BF43BA6DEBBF84FECEF33E70FBCC3D1577BE159DB21
        742B6D8A9C824A84C30E1C3B1AC7A1C339044020D31786C521C1E97141480B80
        1749ACB6AE42D6EE22CC9D1343D3A10D78A56D1773ED17FA5D53EE5BFCC2D91A
        D37FA79DD5C5A0992F0E6DADCAC607E75F6C6BCBB7E3B8AFF7875051A190F893
        E1B2890997E0B24BE40E181B98240C81EDAF5B236A44E9FCE0B8E5074FEB1C0B
        864FEC7E051EEFF70A7DBC51C2A2273370F04DFB0F0F8F59D683FE6CCE98336C
        9C6D50F49937BDDBD1F1487384036E44422E346F710047CB9B50141283455450
        174E85C51384D543BFA7C461C9D2E0FB2E0B735E0EE0A1A75434FFAEB972B054
        2B7EF2A1A59567734C7F693BEBAB81B367F7774625E9E9EC3CFED6AE036C428D
        5543796D3DE2718194B8033E27B906A25BAF55C2EB07F7A0BB35078DAB1CC734
        01031FBEE3E72B7DA6CD193ABC7B1FEB923B7ED88BF2EF1DB8F29E3846D7B734
        77EFD52E7EECBE8FD7CE787AD08D457D85D7A6E6FD80565F5F88D4BC13A82CCF
        8527358070D0437E9F28DE26C3E439A4E55541D19D141058D0AE650CDF2CF4E0
        D3D26A7CF2583EB6BC6C5F3769DCD23E677B3C7F693B5B00E01E7966683749E3
        3B728245E2F968A301A195C119B7366D25E415B515393195832972B0F206E2F5
        2602D5268E6736E2A3B2E318E62B82FA8DB32EA418BF79E4EEE59FFDD4891E7B
        62649BE69DB0FB75AD8CFBF47DBADCE6017C7E572B6C780BAB278F5BDE6FC673
        C33A34CD13B7BD3A6C1D97B3AE29A2110FECCE084C4E435D4D365273AAA0C106
        9B2B065F7A083C2F422076EAE5F661D48D01E45E5A8FF77B77C3572BE21349FD
        3F7196C6F3DF37C4993EE1FBEF8F16F65486DFCA2E94AE492990388E17109365
        E851CE34433A1A493FCBAC928683668AA152C0125014719728580E78ED98601F
        10F4BDB0F728061765A26877AE5A55AECE2C779C98F1D21D9BD51F3BDFFCF9F3
        ADD6AC2FA2BB5B57F1F3E768892BBE7242185797B7370FEE57466A8DCB96D97D
        23B6B9EFAA3BEF835D0694ED2DD0E1BCDDD0740DC76BF2919AE90747AE278568
        9FE32CE40E2474CE53F1FECB12967D1DC6AD93655C72A48579641F3A4FB97FF1
        F6B36DD05FDACE380058CAB5B0BDF972FBCB75AE3266455D4845448D830E08C2
        8DEC1411AEA8004F79DC3CBAC3A894358C7DE4EEA51FB17F3B75FEE8F3333DCA
        17B51757B9DE585F8B8E45360CD38ACDBAEDE206C61E13C72CDBFD63E79CF3F2
        C84A6E586DCEF80762890C24328278EBA15CD4BEE3ADFC52A99F9A05F1920BF2
        52AE3F74652957B6D5027F7926AC7615A283959B5BD1A2F82878CE06CE4841BB
        1C0D6B3E11F0FC3B142A5274F2CE3C37CA5EF6953F78E7D2228EFBF97B1067BF
        D9DF29D7DA7D047C6D9B29F817DDB72876A66DF0B7ED8C03E09167067C32E066
        EB40DE15C7EE137654341838D1482088465059A7E24805893FBB80AE174AF87D
        6B2F8E2E53B4AA5A6DD0D4312B1329DC69CF8CB8A5651BFE4F1FA6EDE5BEDDAA
        C0EE317163BB5C646F4F8BD5D56396E20E3CF9770B4626B8D9CF8DDC9DFFBB60
        9BEBEEF2C396CDC395A1A378502D7E93576CEE2925C37206A008E890E9E6F4E2
        133854A74022E4A590E0D3636E5878178AD215C8210B5E7E4DC1F75BE8EBD9BD
        88CD1BF1ED2D1DB0EA2FCAEB25F7AEB8E9A7AE7BE6FCD16D753D32D7E913FA38
        520C1B67F250427C24D0A8EF1220AE1420BCFBE0B80F4F4BD89ED3009832EFF2
        ED43C6DA3BD4444DEC27BD5C1B5250D9C011004244B93A8E9409A8AD90019D83
        2F47C23793F3B1E4D5E08647EEF9B4E7A93E4F9F377871E7A1FCB0715B7672FE
        13E442883DCEEF2AE23A4BB119D9623940E6B9778B18FD9E8BE937EBA27E5B4A
        11DFBAE59020B76A430DC4CA3894B00657BE81DA2332621526857A3475AD229C
        BA088FD789111715A121330E7F8388D414720B3A8FD79EE371F4B096DCA286F4
        29340E9D470430BBA023BE5DA3DC3875DCD27FB92DCDF8B9839B5A046E3DFA4A
        1962413D32846A54475AC167B7A019F15EF4881BE143AA5E5BA97F2AD8F8FB4F
        47DC9EB300983A77D0BB3D460BD7445325EC3EAAA0BA319E98FDB57E1DE5553A
        76EFE16018A79894C38C9BD3915929E9551568F1C83DCB4AD9BB4FFC6954BE05
        FA0ED7558DA9E3DF2C072F0B88C778883E15D75DE2469F634DCD6F2A4211BD43
        DC69CB89A0F4EB6AEED8FE06C4D274A83E13BA9A0C29B35BF388D69A081D3728
        E42410181C74FA6FDA88EE78EF875A1C3924A0B84300971536C3ABDBFDA8DCE4
        44A48AAC2F1263C81CEE9E24A3FBDEE6E6D1234AF392F12B8EFCAB6B1E3BBBFF
        22F512FECA23360777E4F37264645AE1CB4E41AA5B408BDC08AC961464B838E4
        3798D8F5B91A8CC48D2B1FB96BC5EAFF930028993BB48BD7677E5730CA66AD8D
        6BD8B83B8C13F50A6AEB755456C45059C9FDB5577607509825E1B9EB73CCCDAB
        8C323D8AE972C0F26649C92265DABC11F774E926CC7BC3BD8BFB788901DEE0A1
        2964407AE59C1FC20D03D21139528F1FE21528AFA3091B484E5C567CA2536447
        1213AE2C1E761AF8FABD7AE24E22B6E154A7E20CF4E9998ED73EA40F0A1AB25A
        3562406A1E16ADDD0F4F8E1735521A0E7EE184DCA8E34FCF5B11F87366E50377
        2CCDC7BFD88360D6B3837B996E7CBDFE6251DAF06103A2358D70D0CCCF6F9E83
        564551B81C123252ACC8F29A708A3CDA49C47A7F966BA382BD75C9AD8B7EB680
        E59C03006BD39F1BF87B4F8AF072D108432C27B5BDF607133BB63680223C4422
        49008834181289B02C8F00A70B18DCD5898EBCD33CB19B2F95356E0ECF6B6FEB
        0AFF59BF9BA56E43566C44C30E7762C5CE5079983191049A829E5786D1B98980
        6F765720C2EAF469B6EBAC5A973EC7938B916C803B1D040003360F3180059834
        AC0BD61D6FC4860D1638BD0AD29BFA3124A7002F2DD90984C85D84C8555C948E
        D2B81B6F0E688A2D7F963E9E347ED9887FBCC6990B47A6998AF95BB71B535386
        9BA94F7CABE2F8FA13897C829DAECBE9F4A1F3792A2C36278A33C26896A521AE
        6723DFA3C0B2CB8EEFBF96EF7CECBEE5BF7A56F1ECA582E70D1DEEB4712FE75F
        24A41DCFD3B850A41CEBD60AF8E233011C2B1B2363746D63C56B7766A2A656C3
        8B5BABB0EDA08CA13D5CE8A0A79A813D627D30C8EDC8CA33FB8A436AB89B669F
        00E777C0D47840A5B9AE09895BB26D5DEBD1B37704FB77D5430D1B04009AD8EC
        664DB6C914C9F6D43C0E8D1B4CD888212E68938D8B6F4FC7AB8B150423F49E4B
        86373F88AB8B8BF0CC035B917D81159E2626E28DC4382979187869310E7E947E
        5C8E73439B0416ED806F786B9BC90F12440CF1A5713D2DC571CB666F05F7FA86
        1032951C54EE8F130008D836621FA70DAD5AD0AC7759D131B71A193E0BE27C73
        643A02681E70E1F3F785D9D3EE5BF4E0FF5900B0F6C4FC51F9B22E3F9D932F5D
        957E7E8C970B1BB0A1D2C0E15D3ABA65A7E2EAF66958FD5E5C69ACC69ABCA6FC
        A5DB8AAAF90FD70460F3EAB8A4830B9708D9F0EFB09A6D86F0DCC2E3DBB1E439
        F219A4AEA1894832329F0081BBB00EA2B7143C5BE3271761B07AA22AC2582987
        824C0991460D8D8775BCB2EA322C6F3C82A54B1C70D3ECB7381538B323B8B66B
        1E9EBF7E170AFA0A28E824214A518B52DA041D5BF7843F2762AAFB257F53255E
        975BC8370B6407F97D921FEB4E90FB3944119EA623AB4840377B3AD6AE96A011
        F85822C9EEB05378198360F5A07DCE09B46C26C1EB24804916A41CCAC4BACFE4
        07A7DEBBFC57BB9FE1BF0200A7DAA3F34674E50C6DBCDB2B8C68D25C7078334D
        AEB10EE69E1DD1E38AC1DF3AEDEE4F3E9B3E7FC475A9A9FAAB87CFAFB4BDBBAE
        915CB88E989518E14A3B6ABF6EC453575C82011FFD80AA65D9895DC49277E492
        2B9128A4BC280A234ABFF31A89BE08029F5443A6488309C194220A3BBD067E37
        BA03A48B152CF994C3B13283C24B3D01005B6A1803CF4FC5BA476A285A8920AF
        3B451DF5266AF72918D8EB2AA4E5A4E144970D082B063EDBD3881A7657834248
        7368F0B6F76362A756E82015E143ED043EFF380D659B2AE04AE16173BB9091A6
        C0E396706197188A0AC8759929E8694BC3776F4762F106AEC31FEF5B7AE89C00
        C0FCF9F75863CE7CCB83373F18E6589DD7BFD94AE65FE19174676F190DCD2529
        A5547736AEFEDB987EE673C386A5A70AEFEFEC71D8FAE63735687D818894A341
        ECB144D0D1958989DD3BE3E2397B60EC22C7CE6924F0EC78F0BAE1484D75202E
        8711DB2343AF52113364549FA8C0A1835B71227000CDDBB871FFFCAEF8ECD071
        BCF38E830CA4C2EA4C3280E88EA34307FABAB71DF86663192C14FF8BE45DDC19
        02A271058F5CFF0A5E5A3E0BA5FD4238BCDD09535061E60730AA8F0B7799E763
        FBA7669DA6999A7B48287BB3BD0E27B6C670FCA8159614275C1929282870203B
        2586F38811B2AA25ECFC2A1A0A45CD9BA6DEB3F2C35FDBF8FF360066BE3A3A43
        09C8234D5EEF6711F82E291E7B8628B92CD1F889703880431A67AC88C78D579E
        7CE8F3FFF595B1990B86DC545C687FF9AB4B76F2953BAB51951B279637507D44
        C1EFF39AA37D910FB7CCA9C7C519176170EFAE88C93144ABC3E04EC8304C0DE1
        7A198AAC52A8A9D3CB4020E047F7CB4FC0E8598B77DEB7A3AA5687D3A3110054
        12A10A04471C392DA3E8BAA118EF2DD90B471A074B26450F3E01528A8ED6693D
        70CF05CFE28E577AA3FC3207A25C0473BA77817D43A6B9758BF299CDA9DE2A87
        8C14ABC3F8BCC925668ED9AA06A5910C6227273C712732C20AAC358659B64F8F
        D7FBF5F734BB317DFA1D9FFC2AF732FEC70078F4E92B3AEB82785F4A2A7755CB
        76BCB5B09585F3BA7444C8DF1E356324AA74F80C1BA2870573F3B772381C36EE
        9A72CFAAB7FE373B7CFDBCFE5D8DB4F8F752874AE987D27288EDD9EE1B406345
        1CBF6FF247F4C9688746D94444251EF107102F8B4239422F9D195443B056811C
        53A16B2A54955E711D2D07D5A02EAB0A6FBE2D21D5A7838C9600006300CE1E87
        BDC08FABEBBAE099A736C1532C20BBB50505BDEC109C3206781EC2C5F937A0A2
        BA0C13378EC2D8115D50FA8E4DADAAD7A7A07ED9932525CC1101335E1C9083B8
        6DB224580608CEEA4C5ECD0A2B72F4A8AA61A70E618D1413563DFCF0E2FA3365
        F85F0480590B869F17D38D92EC3C737897DE36C19103EC090B58F55D18EBB737
        60DF610AAD685641E4E0C930D0B5138F71FD33105827EAE587F91755D3FE15A7
        051A6421AA3A851CDD303DBA6EC44D81B771A219E24390799E6FA0382CC20B5C
        A64096110D2E22D984366E41D7ECB2759F08DDCE5B0D09DB78F90F9EA1B14E3B
        D6EFE6EA5BABE4D639C4A232FE903B0DAD7DDD10941B1153C288CA51C498E18F
        C7A010552B7196DB57112601178F300650A12924E6E865736BD8927E02158DE4
        935D1A01805E7464B39F77C6612B08A2D9273958B3A20CCE0C11EE1622DAF677
        A1B62C8276393D31A1DFEB20C782EFB67E894F6A1699F69AE28810B7769C7BD7
        F3474E677C4FB7952CE8DF9AD3EC9709BCD1C9403C07BA690A82A7D234B4FD06
        8CEF8D3CFBE6D329993F6D00CC79F9165F3052FD4466067EDFB9AF248114F1F2
        3D1ABEFE2E82EF3669647472F712DBBCF9EFEFF6E178139E6C15370FF3A06F4A
        2AA4A03591A0D1D428043D8DFACD27041A3BBB90A8BEA519C985E977A25C3E85
        FEBD018D606111982F27D1C55593B89720506C58EA6BC4EA3DDB71203D08DEA0
        902CA26158CE4DE897792D1AE45AF2F551C4E93CD1A3743C168526D38C2723CB
        1185BE838C6AD310AA51108F290900E8BA469DD0C193C10FD8FC087A23647C62
        00FADD42C6176D2AA25F8451FF798C04210F6B0A87F4F6C4003D2CA83FAAC010
        63B8EA923B30ACF97D88C80DB8E7BDDEE85EFC07B36E6FF0ED67C7FCF9B7FFA9
        D1AF987F85B587EA1A200A91313905C2A5F92D2541A46181834F18CFA040436D
        00FC159A79FC88561B93B9A5AA2EBDFED8F88FBEC3696C90F92F01F0E88B837A
        D80CEE838E9758F26A9BF0DCB77BAAB1FE7B05E527087B711EAAA24396D92D7E
        3471CD7F388FC9C19BAE4117C99FA699B0D88DC45EBE12A973761388C5CAF2EF
        0C3C149793685245858E1A85EEC9DF1551A3F7C857133098A0D64ED6FFB20CF1
        C5593E1CFD328210018C2300664B0598D6ED65F8E53A44B408199B667D430CA1
        1D11A2779900A021AE100868D6B3357E88CCE8345F43048A283101081476C60A
        74B6A80E953E7F24B29722032E51A2A6549291E9F3BC9D83D54321658E80AC36
        56A4E68990299EB4A5497064C6312AEB31744E1D82D78EDF8CF2BDA669AFEFA0
        396BAD370AFCB1C3D0E37117D7B62EE85CD3E8D9970851E83ADDBC2373B85BB3
        1E7537986BEC82EE1624BE798A2E9667F0A6D5074E6D423DE86077F0BD9AB5B0
        A464B4D7B92F7004CF6C3D8CC26013A4C57C102C1436BA79647881820C0E2D7C
        36B8831CAAE9FC6507B4DD9A2E3C6F4896374BEE5E14FE450098316F707BBB13
        6BF3875BBDABAA1BB1E93B036A9854745C48183F4E7E331633FE35006876DBC9
        97FEFE4A1BAE6A99869A3A19016285323582AD9541EC2A5310A814A191BE27EE
        A74E1081B1EF2131679E4CD2306327BE9BD7138921F67661271D85463DF65BD9
        4DF8E4E7A90F13BBCC429EAB2942B120093E32BE1683BA9B667F380E25AA2604
        1F73016C7D5F27E1A7A964687A717CF277DD9409CCEC73646C628B584087BFB1
        12757595909C02381B41573489E878A434E191DEC242CC0052F07424B1C8C460
        FD41625DFACC9C6BBFA4F328786AED6FD02AB7179A06183B06612ABC29471DE4
        6EA274695222C50C6238AB980D89734113CBE93D0B67F01C7449834CE1AD62A5
        8941AEA98A8F605D5923BE3D1A8011B440B2E9F0883614879B41A0AFB2D12462
        DBE4B2E255569D9A9E12454E86076D7D6E382A61966DD3AAA38A3179F29865AF
        FED872F58F0260D682419FA65C61EBFFDA5E52D027A23460062C461481903531
        50F138516F8C3141F2F3027DB37E0A046C570CD202E3EF167053412196BE19DF
        6D72C1A5229796275AF4F3333285968E428D6FCC0D605BBC1E5FEC6B44ED7109
        729D4422D264ABB70920B0CB61C032589FE97F8DA8A0EFD52A6AB656A1810689
        EDC8EDE53231FDD257511FAE21E347C8F7535F0F92CFAFA317CD5E5549FA7E26
        F658C798BF67EFC9518D00956402935886E5071850E231E62AE8024C15A5A5FB
        88FEE9479B40034CE236CF0207857E92CB84C32B245C8112D6110D510F3523C1
        2CFDFA0EC16D173F8ABD077660DCB63B71F868139AEA06525C041C0F0F9793E5
        25388469EC1472897E3A1F611311C5846E63825343AA8B834B94E87D1D27EAA9
        6F0D646562508A22088CC47A0400874544DAB11690AC4662B36C56A5649192E9
        F3147B8498D305AB4583274540AFA636D8F658CD8ADDFAEBBB0A6DB72DFA87FB
        297E1400CFFD65B0BABC3026369E082118762466AA1AA74122C3736A1CC1084F
        C20B506249E324752E4720069A77D470D7D53E0CF166E0BD9743D55CDCDEEB8F
        E33FFCAB189A34E7863C27DF3088A2E991DE4CAE6F93D69CB5BAA89E5B2757E2
        B33D0DF09713C8EA6C242B881728AA609060D872D0000E1E10C5F7FBEA113AA1
        211A04AE6C7F0DFA36198E60BC3131FBD5600CC631022DD17B3CA424289EF979
        0D495F9F0083926483C4FB140918A69A080F759AB9B1088B0AD4447D405D7D05
        14622C810C66D881940C09762F0F39A025CAC0D9B5F29299003F5B64B2B9C8D6
        E496BAB5EA833FF4781AB3568CC7D260088183D6A42BD6B9E4E420C3203B066F
        A1821ECD1C689DEE441EE99BF4A8CB74471C0807788DC0280BBCC4A71718F67D
        6D4BB9A99F9441AFB1250A541808ACA615CED23C6671323CBB8D8E2D6E582092
        A0A2B7204902529D21E8BC95AE4D425A8613ED65DDACDE1D9CF8DC03DF3DF9B3
        0058F0C690F0F7DD3867E5B1467011B65A9742202094D2ACE27493065082CB1A
        4BECD59B936E209DE2E2A0CB8BC27C037D891A9B465D58F6B6EC8FCBDCC0293F
        B131C3D4D92333450B464BD06EC86D62E956D409FCFEB44A2CAA3E82EF0F84A1
        9E70C368B02698A079571E69D575281589DEAB8921C800F79D3F1DE9CE0C9A2D
        6184A344FF3BE2D048DC41521121451F0BAA27E99DA97EC600647C35696C76D4
        1291001D3525F139161EB2E8802D09C7A30104030D241AC9C0E4FF39A65D58DD
        00CFD61208941AB9056FD24DE98601270944C60C327DD7B4DFBE81C6BA06DC7F
        741E6AB7A7014541F89AC770D1790E74216A2E684C85ABDA63D6549875E188B6
        9990FE0371D1764DB6ED4D49CBAB1C7FD333FE9292D1162E3536A74B77EB5D39
        17C4B8D1DFAC47D91E09160ABB2D640FB3C24BAE5148CC7AB66D3EDB2C53E792
        20B0B21DD4E9251028EC3639F15495D645321C7B1C279EB9E3DBDC9FD700F307
        BFD2A4A7E5A6BD4D6D9C643B805DB5054821BFE5748561C453909372023E4B9C
        66692AB268B045C34B74EC443A8DCEB13DBAB9796DAC4CD685AB7EC1BDF1DCA3
        4F5FDBC9E443777853C4EBDB75B6B9ED6DA358143A80B78F1C415DA90D3DDA79
        50FB713542797A22E808379878BCFF0B90CD3889B13081228ED80EF2E76C633E
        EA2B9BD9A17A1270F43BDBB65567C6D6B404ED335F7F2A0248B805359917F82B
        004C267023490090E1252701C0C1B18004225B4E260A77E6F3C8EF2C20C3E340
        A6DB872FBE2A2501998C80EEBB6E3ADA1776C76C650C5AB7E3D149CD83E7A817
        15872037D4E9EBE80A3ED1615D6D3454EF2E29F95AFB5783F2E28B5DA513B1BC
        BF74E82E5CD5B997C0DDBAF33BAC2590AB6B9B245DAFC02518EA140838621289
        5849B232E38B647C85DC850E8380E070E8681F76C45FBC738DFD6701F0E013BD
        DD1E976FAED76D5E9F572CD99C3E8EE369B60B1409F2E42F2D64685DE512FE32
        12225FD6A099B5278C4838A4EFA5B868A5D5619D3BF18E45FFD65E38EC567225
        5A758B281963DA74B03539AF87855BAD96E2B3C0211CDAD188081947234D2BD7
        0998D8E3399AFD419AA1B588ECE711382A93E1E4C46CE7896AC30DA401624906
        60006054AF1243C8F1E47B9A9A74036A02045AE208EE245848E187028D44BBAC
        3A9D6D454F7F62C6A7C8832756F091204CBDC085CE37E6E0FAB4662820BFFBEE
        E77BF0DA9F77E0EE61D3D1B15D372C3E381D45662B3370B4F633127F6F684274
        45C9D84F82BF643C4A4A2E114DAFF3CD4E3DACD7F6E969E56E581EC2975F5627
        562493BB50994C84811759910BB9007A316070F472D2EC6722C19EEAC6B016E4
        CA76C6572C7C60DD909F05C0A9F6F8E383536392ED7C49525A9A865148036833
        D9A67770B20AD938A1FD384C4B25855E07D345E7FEFBFE170B1CD9166C5C9536
        5A32F5870ABA48EDBFCFDFC86DF25722504BBAE8988EE6F6748CBC6830D6EAAB
        B1A6AC0A171C1A843635CD108DC7089CE4AAAC497F1E0BAB0956D05834404CC0
        FC3CD302CCF86CE66BEAFF0081A045637A120026034A2831F39DE4FB0DA27C2D
        6A24B6B671E503594DADF84EED9014916DAB71C185C0DD1D8A71A19889EAE3AD
        5057DD1E4F7C300E7DBBDE6206AAFDBD9F19F3E2F7FFF65894B4B320B5C9AB2D
        DB89D777BDCCC6ED3223F8A1CA8AF2BA386A09082669153BC7F21F16C40C0FDC
        245E3348A476CD0DC1A6E7C151AE99353BE20723A632E4913F7CFE777587FF15
        AB813FD5AE9B7159169A72C7F98223E216FD28DADA33A1BB4D1CA66864EF0F02
        F43DD9304927A47AADB839B70B82C16892D20DA6EE9399BE782839E34FC5FE6A
        223248FA7DA60128FA4F503F6308E63A9806D0D9DF112165CE2611CB0190F0AD
        26059EC5C193C6A3CE96874A572612D525069FA830427E00F93DC3B8A34F1A7E
        EB698F0F3FD94F60EB67468E95CF95BC27269DEEEE263F0E020A92D287DD2198
        E63D99B97CEB8C2C706E52F9CC459902832E45119A2391DA263C20E837CD862A
        924641639D29F08B148BF1DE8FED53F45F0D8092276FCCB65AFD77D535512637
        E41EE4CA3D216CDA46FE7F970F5A992B117B833FB9E50C19627C8B9E14C34793
        06D692C664B39D2581648AB7D4932CC0C0A0B2C41085931ACD70A3319ED8E059
        97926291257E742889A4146F499EC2243625518DF4962282E51A82AD5B23E874
        9EA4E1938D252B98DA77C8B075A9C3CD035370634E53346EB299DB37AB1511D5
        98CD09F6577F2A31733AED892746E5C76C5A7BEA6A8129465239439060440C91
        F7AAA6A9D583B354A9AA79488C763B5C525262FCD477FDD70160EE6B23BC5A58
        18025EBF0E1EFDD2FD7915B6F5744D7B7792DA3E904AF1289FCC1F73FF705DA4
        C4C7B5EE8E407DF4AF3E9FF973260099E113C7186301024250476454238E7B04
        569A0DF29AF01E3761FBAA8104653C91233012DB7C1A8CF1938ADFCDC191CED6
        3A38C41A808ABCB6885A4F86783FD65865120347EB3A0CED67C5AD6D898AF7F8
        CC9D3FA8B5B1B8F09C24C75FFC6FD831F4BF0200AC0EC00AC7608ABF47A7F9B8
        FE8102BFE35BD472AB4B1B10DAE7A6292F91BD8564C651174E8EF9C901FEEB25
        9818DFA613EAEB88B6F9A44F0F372673010C002A51BDCAD47F8C84EBC5F5385C
        44EAB8C69A6070F61D26095B1F093FF3C55A68160A2555239183605F6F21E33B
        D3CD44C6CDC59683C9C71EB1B546BD69C7CFA6DB997BA0EF427A1C6D7ACBB8ED
        927474A8C9330F6CD263FE7A633149F63F29358BD69E5A35FCA7B1993DB83D44
        5C4E7D694D2A94E74DFB4ED165BC3FE9B6A5D5FFB50060CA35EC74B6E64CA39D
        24C49D568ED779D35B2B98DAD18016A9F035380C2E9FCF544CAE9F241A437DA9
        425FA148B16F15EAF059650DF6EFA141F55B9374CA0650E713EB0B09A3B31C31
        ABC860F44CE18E41FE8FA35027D3E2C270B92D42A158227D6C720AA27E25F9F4
        2E3999046229613DA4A36EEE41D46EC9A0998D046D9B66723F7FC1C32165C909
        4413377E244787ED13654B63492E9EC063267C6E7A33209653884391AC04F324
        30F0B7AE80FB91A399AC506259477BBB107E73790A2E4F4D8374D0631E39AC54
        CA71E313C5D037D227828229A41B1CD79942BABEB9F952714E5383F392EE6039
        886035877DDBD47820C0BDE2908547274C585CF35F03809292013ED12BDDC589
        DC2D5A2E8A249FC849A28A6C0A472C3127B4106786C3AAC91B2986E40809B154
        953B2C06F16D7D0336948560907A8753478A4B80CF4D2F125E5E9748BFB35857
        8395253E04030EC102915986A8D9CA625C0A4F2BBF4D07B73B0DB2C20A3E4808
        D131E267C9201248141A9ECAF2E9210DBE77EAB0759709A5DA96305E12003C31
        078F94864AC49E232660E93D2631A4E44E66EC2852F8C7EA0A59057146071776
        764B81C3AD228D5C49AA43A07E4989CCA0CC16CA148A976222FC04383216C22C
        280E107AFC74CE087DB94E31655E0C9D7B72B8A66B3ADA881ED848CCAA71BA26
        D2250E1F075BA68A6A169B708DF02B56F8650778D3862E5E03F201DEDCB8566E
        90A3E2C346E39297FF15839C110094BC7689CD12748DB5D8F80773BB73BE50BE
        C4ADD91E4279959C889FB3323538D332D03C5746AAB501DBCB62D85921C3742A
        68E573230B14E5C836B8E3363A3A4D83A59E63A229CB9A6EC42DD17243B697A5
        574ADF566CE658152F7B3EA0BF826393092A79D1FEEE6B1161215E389EA07B96
        F091A34A62DD9FE501120B40725218C6FD2A6E5CE4C54BE13D086C4D23F56C90
        E8237B90CF965A05907D8804DE3C02AC9BA5F10924A49B0D32A6C5C92596BD59
        7C6DA59F1DD91C46DD588C3183BBA1560E92024FA3F34BC4403A526D0504041E
        FB6BB722D3914DB6AE8149ACE59722A813E2A8A37E5486151CA850B0F74404E1
        06D22004AAF6CD05B4CB76C063E361174512A0368AEB1D89F4FBAE632E289A8E
        B685D5484DD71053737081DD81FDDFA8E6FE5DEA679CACDF5AF2D027BF785FC2
        FF1800339E19D68F178C854DBB892DD2CF97B9BFAC05D66F6A84C3C61EE122C2
        10ED70D0806566D08475A5A3202380DAEA7274F67991BA3343AFAF300F1A86F1
        8361727BC910A5447395E4322B83416BC3DC92E48D11BF7D76F00A6F37E58AAF
        B7ADE6780A7D9C34438224DA585226B05F876F6F3ABA5C702929FD58C2CF33E3
        3363B31050934F267A4E1E23140ADE31B6009FF7DB87BDEB6C641CFA1EA27F5B
        D300F29A7268F2940FCBDE3A066F130136024194184469A459E93EB96DA0C425
        18C19523C19AA621AB1D4DE8A0817E5D46A36FABEBC830E1C4636259A266E9D6
        B7CDE6B93DC24268EB44812594496670A6962DF0967CC922354DF5A12035CBB4
        C4B2625CA92580AF7687906693509026218B18C0EE4EC1F10A0B6A6A2308C6AD
        88903461390EAF33045F411A523345F4CF0CC35EEE63B7A7D76A067FC323772F
        5D754600C0841B54E1E9DC62EEA6E6FD24619BDFC0ABEF56A336A0C3EB66DBBC
        5034643748E3907A179CC8490FC3994AF13AC5D1695214996B9B28D555D68BA6
        DCFBEECF6EE27CFD824153B3CE774CFD76FD0A2EE2D412B5041CD17FFD6E52E9
        3112657B818C827CB46AD58966A14C468B271782D8EA9EAC244BBF48042A048C
        18BD5F906DC1C00FBC786D554362254FCC94D1A9B51DA3AB9BE37703BF809D5C
        8F85DC8EC3CB7C2E31045BFFB126D3BC162B7B4C61724D80A723AB11141C3C9C
        3E118FDEFE5CE26693280160D7AEBD08B8B69A19FE3CD95FDB3071C7B8950B17
        2537C6FD6BBBB76484372D4DEA421D1DE64B936EF30CF63BBED813240088484D
        15C1C5351C3A2E2122930026811A667518869E00726DC8064F8A15175FD8089B
        3D07036D4E7CBB38A6D6D51A53A68C59F9044EE34EE57F1B006C1B56DE34DEEE
        D0C7D2CCD181E7FEB4B411EFAD0CC2C57C17F9426BA2E88366AA5D85CB6524F2
        D43181FC25697DA78FC7C8D61C6AFE2CEC9C387659879F3BD7E3CF8F68024D7F
        C7D1DFE8F1F6176BB89A8608B9158E6625195FE320D1CF96833CE2311D6E571A
        DAB6E88018F9FAB09F627B1A3416FAB1954C952DF830311850E8A561F2E65678
        716D6542C35DD0D58ACB4BF370DD902FE1CCE0E14963CBBEC9546BA8DE841A36
        13B39F158924F2EC6CC58D02008E2D0EB1457672591EBAA62B065E8C0B0B6EA2
        BFE958B67B0EDE207ABFA68B0F174773CD435B8DD532B81BA7DDF3E35BC83CB2
        60E8C49EBDA499A539355C659887C3EA425D65235D3A8F107D7F7D84D5139809
        7DC1D84D26E046E36C6F05A0691B37DA76147003B1D281CF74F3D01EF5B54A47
        D51FFED59E09FF1100A6CE1BF287540F37A7DB9592FD90CEEED9AFC5B1239144
        E1848DBDAC6CBB573663043AEAB0130B0856112AE724C4CA1053EDB821CF8BCA
        55B1E7268D5B71F74F1AFFD911D7119B2C0CB48AF81E5D518FAEBDAB11AAA881
        1A60F7FA5160C412CFA4A76CFB013942E28B7CB6C323C16B2FA201F4408BCAF4
        3EBD543D910E560D99FEAD96880E4AD677C487BBD8DD210626B66B81A1CD5625
        AA7ED8FD81AE6C1EEE1CFACE28690056A9A62773EDF150D2155852F804BBB12D
        6F595106DBC1C4D3C640EFFE6D709967222AEB0FE199754FE308456EF14A727F
        5DE328E997656AEBDD35A1A871D3A47B96FDD303AD67CE1C9966CFD1CA07DE28
        D93FAAA82501EB40B031482E85B443D492A81BD0A8FFC1A83511E26AE427238A
        482E81453906525281F617A7E2E6D622E48D92B9E95B7969100DD7CDBDEFFB9F
        4CCF9F3600923B7BC4E6143615C6B41DCAF3AB774508B58D444DC65F77D5646B
        D1899D3F4F318093158A5A128B48AC0CCCE18CC3F4A5E06EBBC73CBE5DBB72F2
        B81F7F9C1B1B0CDEAD2ECC6ACE5DF361B48E5BBE524EDCBFDF724418299587C1
        51F4A51135C74E10A7D2C05848FA18346B0CA6BEA93B7A88A5672562841408BC
        2D11EBEB9A9E70070A09C2184DA929EBBA61FD713FDAE43B7168761D3EFAA094
        344B32E7EF2600A46403A106060002B58B2503F844895822F34BC01049D7D853
        89119836F0EAB8B47B7F0CE97D1D1A8F9A78F9DDA7B035338ACA13E9C91166C4
        6FD570D375222EAECBD32A8FE9B3147F78DA3FAE043E3A6FE827A36EB20F5CAF
        85B1FB18E9157F1875114FA22047D349E012900331BA1E534698220C267659E6
        92451B1AFD4D21217BD9F008AEEB5200CF769BB9EE9BF83221CF3EFAA70A454F
        0B006C61C6AC88FEB96D3769748B0B65EE890F15BCF771234D3DEE9FBE81F948
        B6EFAF8506CC6A67C913F267EE2854B6DF1AA9A0EEDD33D0632717AEF5AB793F
        B632F6F8B343FA3BACE29F8CF6D182C73EABE31A0E728994AFAB6D14378EF0A0
        727B15766C2AA3892F402543B0153A8BCA436CE46876335A341377E6B09A7F96
        0C64093D5D3792A138B90C3598A4F0C7B6F5C6E1FA082E70A762C2755FC3E6E4
        0924C9DBC958C8C79EFC2D13D31884285716730902E90213D1066213629FF4F6
        22B23BD0EC2E746048B38790C217C25F15A62840C6934BC6227C553ECABF4901
        AA6CC9CC259B2404C2F67D558C6F966BD66DB7AE8E41BBA1E4EEFFC9064E9E3F
        70C24597D99E949B89F8FE700CC7CA2334CB498C727ED4C7AC8992BC884CC25A
        9713A0881103B023ABD390E9738C15647285853DD3F0D4352AA41FBCE68675F2
        ABD3EE5BF12F9F95F0B30060491DC1E778AB6D0FDBD59EEE3C37E1992A6CDB1E
        4B6E94F0238D6DFD6E6742C922D2E4A741239760B36B899B22335BD8F050072F
        F62E51FFF2C8B8157F57315BF2EC252E8B993223BBD0B8FB6B4BA3F0E6BB113A
        878E3617E9B8B6730ADA3764997B76410D7575485B0E2EE262E40363D526C234
        E358C5925041E32BB3A21524D3B7F433136FBA41034300D0C97FB2F57A9D4031
        E4A602F47DA01031A2D2032FD563D9F2C38975F5784D329FC3519F25215992C6
        123F3C9F5C0760153DD674A27FD209799D39A417D0CF6A730CCE988C903F486E
        4E2500FAB1B36A837971070FEAD3CBB8BDD600566E0DE2C026FA820A67E2DE45
        6B93181E1B9E0EEB36CFF14844BFFEE1B12BD6B231983A6740735F86B86BC06F
        9DD62FEA757CF30301805C986E92DB9285C402569C5C821501323E8F868893C0
        A08163C52ECC25C42D4910444C343BDF82D937E620F299611ED98FBFEE8BF88B
        01306DDE9099AD3A49131DE7F3DC6D8F56A2BA5AF9FB6D54FFA99920C6270D90
        7C88A3C8441381A1CB4556DCDFDD8EBDEF89A18680D69366FF9E53FF62E68261
        BDADA2F1B2BBA3D16AC6F7D5DCB1D2382E192C62584E2A7C87D2D5EA0AF95395
        E35F3A04A1D69E2AAD71B7AD128B0A02DCFC4DA5C820FA17CC28C2BBC8471320
        A2F5C93B8B11634FECA6D91A47E299C2B7FFA11D2EEA5F003187C5D864045344
        031743C92D9BB08DED1CC2272B7F2446F74AE281E4109CEC16F2E4635F596238
        461A20BFA780A24E02C5E87C4294D99D0A7EDBE6593AAF9058743A72BC14A660
        33E3B5FE3956A3D4B0D88521790552EB785123B749A8C7A26F83A8DEE049B885
        9B6FB4A057758E5C7DDC9CA4FA97CE65C99C9205C32736698699DDAEB0705FD7
        2BD87C20808387680C11277DA21010F844214B63D4460250042B616035309C41
        914FA2F41EC9D096B44B9B4BAD78EF771EAC7A45F2A3416FF3636B0F3F0980E9
        CF0EEE959EC1ADC91868156E98729C6268EDE42E0B3FDDD8ED817CA25A8547CB
        2E4E8CB8DC87C156CEFC6EB11C886ADC958FDCB9EC4BF639F6AC9D98284FCBCA
        C5BD9BBD01E9F9B55518DCDB86C19E2C93DBE76C0844F457E20AFF7CC9F82547
        4F7DF79D73467D306A847DD42CED4BEE8BD9B9C4D1027AF73A8E0C5B0DAAB770
        683C644089B0F323B17E5F509082A75FBE08F31A77614D7D0579130B1CBC8E14
        D18E816905E82714E237033F23365028EC2351E762AAD280DB2D20A558822B83
        43E55699841EF9586290560345787325444960E6B85BE0AA4EE3108F88A403CA
        10A8E370E450C80C55735B038ADC6BC1D8056C3F63EED185D7B595CCD835A268
        5CD7A4B9D8ACB2A89A5B5AEAC717CB4D74EA21E2DEDC02B36217BF94E3C55B1E
        1EB3B861CABC2BA6E5E58B932E1969E3B7C92A3696CB283B508503657644550B
        24339CC80BE82AAB8E62092E33F173342E265D1EAB6B65BA83C0FFBBFB6DB8D9
        9A6DAEFB3C3EF5917B563CF68B0030F399A12B3A5DC30DBA76761582C11FD942
        FD9F27FFC9AA60A04B6F1E3D2FC9C195B922229B4C73FB6665A345177EFFD0BD
        4BF72798E59961FD9C167381A7BDD9727ECD512E8706FE4A7796A9EC751E0BC7
        3157E1C47F5A36658F6669A266EDEF7B075FDCEB952D307FC863057A89DBC107
        0F380ABDAA11959B8902EB74E876016E4878E6CD4B7163E967F0F25E70722ABD
        C321C51E87CE8910B838326D126E77B4C3B557AC4CC4FA569AF12289419E2D00
        11D5A71648683846D44BBE9FAD05B41D25250A549B382EC6F0167763D1CE7598
        F5E1E718F41B05575B3AA2FC50D3A8A6C87DA7DDF9E4C67F1C1EB6A66FCB18D9
        CBD4F5DBD2D2F82BADEDA38E8FEB6AB82F0E4431AD5B01B0CD753C1CD6E609F0
        54A85CC3D5924D1ADEABAF9573B434B1231EC0DA035E6CDF1A42C38946C435A6
        0548D3684662E5D2A0A3AA929B53F984EB3B650F4786156F4C28C6BEB76A563E
        72CFE7837F1100E6BE39A0E1AD403475F3D6E8BFA67DE3E48BA8C75B6045CF3E
        06065E28A293CD85E87687B96393AA5398524E86FA5E623ADD545591B7B7C92B
        B2F45A9F5ACB6F566B709DA5C8E476D92B220AFF98BCC9FF66C9EB3F5E38317D
        C1882BDB9E272DFABCF576EEF9A9497F7AEA32DC291C7AE7EC45C3C118EA8F90
        2B2041B860E1C598EADC0E979E06A3810462CC05B7254AE1940D69AE18523359
        6825A1ADD786D40DC00B2FEF805C9FCCF9DBD8432CF278701E720334B3E3B296
        28B4C8EF2D22AD40C6E056F762E526156F7DF54D52F98A1A16CDCCC681779CA5
        557AACCD82B19FFCE46EE66C9772C582DB3C2E73ACB3433CE3F5C632AEB5D781
        CB1C3E18713BF9FD389CB0C15FC123E0D7D0B58F845881053B4999EE2CD7B17B
        8B8CC3FB58E693C2614E4FEC7BC4E89F01E0EF5240BC8885139AC2BFD4FFC6E4
        F19FDCF88B0030FBC521FB96C9A1566B36871229D004031827673A5B95A39F7D
        79565C7C41147D2ED091E669816C0A9D1A7773E6D17DE621555743AE14A343BB
        9E16A15C254C87F4C457D42B061697556378573B9A6DCF8ED7D6F24F4ADEC893
        3FF58C5F367B04EFB00D97DF2A74BB64D1F790D7B1D9FF3F9B49B1F5F7761755
        C0FA05B9395270859929E8F2C77C2CA9AC81ED843D9134D174173CD630140280
        5392E120ADD0B4055B3B1030D2598C3FDEFF255C99022AB7A848C9249D50447A
        81A203CD4F80AA253D4191455A0703432F1E8EDE4D06E1FCE51380AF5A260531
        D170B39E0A9EEDD6D25CFFADF270C93DCB66E1345AE29678CEF2607626FFC086
        A20AEB4B7FA121603B5386F8C46654FD7B7A30ED8A2C7CB938665A2DBCD9A2AD
        C8F1C50217C82297A71CC33B2B5BE1F0763F1A993633FE61899C58E1BA11B9B8
        C08FBA70D8ECFDF09895077E1100A63F3B7C68568EF9D191CCA8F8DAA701D493
        71B33225E4E6DB905BE4468F8E1A9AA551381454D0B8D76E1EDB6786030175B9
        26D95FE78C6897FC026146D3FE063F7D793DF6EC5710A0B02AC6366AA0F93DF4
        7A60585551E4788D72E9B4FB966FC4CFB429F3860E3FAF8D65F1C7ADB6726FCD
        62BBC59FAC0B304E5E020922EFAD7BD07329873D5B429834A90716E61C82BDA1
        19EACB01A78D4251CD49119909ABA410A1B1421013ED5BC5607589E8ED4BC392
        79BB11710470EC3B72031481A4B5B3D004E2102DD510AC53D1E9B226B8A6FB83
        B06869E40622589D3E07D39654C1F8AA0981803A13373061A20DC55B32227E3F
        DF85FCF9819FBBAE536DE6C2912FA81DFD774C7D2590DCCC52D093BB56D144BB
        AA6F1AA60ECDC4B2D7430143B53C452AA7ABD763BB2CB35877D5A4099C51588B
        727F2DD66F9450759C8D318F6C9717FDF31C664EA3581A96F52B27DDBD72DB8F
        9DF767A38019F347DCE8CD309EEFDC97B3D9B3D88D1A1AFC8A0B5A0D87489986
        F2837AB4BE51FF9CE4D3A2C600BFE2999225FE690B86DF949B6FFEA9C3504978
        E0B50A1C2A4F0A15B644CA54AB423475C34D40E7DD8EE3F7DEB8B6103F5355C1
        4251C9E7DEDCF546BDC3156F6E87B9373DF907667C23A94A2D83F7E1F7BDB2A0
        BE1AC28675B578E04FBDF0A2BF0ACACE14042986B608148A92D96D129B29E4FF
        790356A26DAFCF4AB1BC1D5DBC0AF4C33558FDFD31546E641B4999095F6F2106
        880664D4D59106B97334BAA48E463C18833F568F88F36BF83B5560EAAAA3503F
        6D4ACC6326CAC13E78A000FB96706BB6DFE3B8F45F3DA4EA1FDB132F5CBE7A57
        AE7CD99F3F0E277738216DC36E8963AB8A691457BF323D1D99F52E7CF24E64A7
        29467AA1368315995C6251CC01BC95EBEDF6A08DCDE3643750735A2C68C4EAB9
        7DE40EDE8EE9DA829FAA443EAD44D0CCF9C3DBAA8679AF249A3DC1A94E121F35
        9AC96F117571B55465AE3EF5F005D6A6CE1F72BED7C37DDDFB37827DD647611C
        AB0E211443223F1F95190892F5935327586059E12ABFFF962F8A7E0E008FCE1B
        756BA72EFC4B0B527EE03E7FD103D26FC97B08292616D288CA87EDC3F4165D91
        53DDD17CEDF3259C2DA0A3CD5DF9D856CB63DF5A6BE28609871449F87E911590
        B0D94F036B1148F93B2DB8B05708A9820BB6FD417CB9F7100EAE269F4AAADF96
        6F22CF97878ECDFAA049C67968D1A22D6C2932366FDD8BE7620B71C7059DC17F
        9E6A3A8654E3FE8DBBB9C092E2C488B6205730BB433373E7467DC22363973E7D
        5A63FCFCF96B76F89C7DDEFDA221C1261C13B704283E6A49D441BC34DD01AFCD
        09EB7E87B96E75FC8D69E3FF7E675296A9ADA8C8235523DB038EACAA923B4A4E
        EB4195FFAB0521EC112D46DCD8DCEFF7628B17D6D5A1B2D68A38C54EE1781C81
        087BA03210A157612B13D3AEF7E1C02BF6ED13C72CFFC90740B12213578E6D77
        CE6FFDD9BF995306AEC195F4738A00B175239A0CA8C253EE7E38BE5A085706DC
        9CFBA643CED2C3F588FA78941EC944D92E9566BA0A37F9FE46399D0653819DFC
        BFA213300CF63C020E175C6620CF2A22FE591D76E3088E6F54D150A6233F2F07
        336F79239188D9BCF37B1C8BEF246DE080D5F1FFDAFB0EB028AEEEFD77CAF685
        65A98208A2D8317622F6965802962426F94CFBE297C4584030B106018D3D318A
        35261A4D4F348905EC2DB6D87BC1860D1083C0B2C0F69D99FF9D01BEAC3C340B
        E67B7ECFFF3CCFECCCCE4EB97BCF7BCF3DE7DE73CEA585DF947F50016A0DFAA5
        35158CF97C7183BEBC76E29D1354FA0F8112C2DF8D9623FC6A1DCBDD1C7BA7A4
        B88AC5AF2BCD59DE778339941A34FDF72C504A41D26F443738D626273A0A8F39
        1FBA11AB448DA65A77DCDA6AE1B36F08AFC4C7A63C761A9927EB11B4306A69C7
        3EF2917BACC5D4C5EB623F5BA2281A89CD6C10037A4D9C64A00E78D98EA1AA86
        D8B7DEBC2C69EC8E2A2784A625472EE9DE5739EA9DEB87A99BBB3DA4F10571A8
        97E97C0703BA283122A7B370F680E39C83568CF4F7E4F76E0B3FA6F0BAEF8B0C
        1F3B7288F837DD3588518B927E64B2BB4BC3842CE95BD57231949CB42A1D87B6
        3D3468A53323676B016ECB73E06E6C092FAE21BAB78C448139179F7F37052A0F
        2D22DABE2028393D672FBC9F56B74946D812FA2235CA3F14F7766A0E084E6787
        6E916AE5C7B947F1E72A229E654E7C39C907795BD4E758B3B5D3F8F1952BB852
        DD2DE93FBC755BE5CA77B6DDA40C269BE4A8CA5895A0C5B22A1804076BF0C66B
        9EA43BC842B8A22ED67F65CBD2A8D8B0B8773614FC4F006046F2A04EDE01D847
        77A1D89443F9D0B2E2F0A40C1C472A9FE30900ECC836F0C45A60F06267356EAF
        95DB8D06A6C3C7A3369DABF4994B0775AAE343EFBBD1FD163B6B8119B4E810CA
        0A90F749C7872D9A21EC7823FE72BA6505A5A23F62AD74DF96EDE9DF86197750
        1F78B5C4396F1B0A0F3B7123D38D30BCD4C19354AB4A6E835CF219243A9B5305
        9D8EC5B3CF29D1C3331FBDA8A9282CAC833B776EE24AFA259CA3CE204F791A1D
        4CEF108393719A8BAC7B783B3F93C9FFF490C23372B5BE35FFE62EAFEB549B5B
        CD85DBFEB954919711CDD4F5712C230DA98B755007DBF1DD4BC142DA5E6A557C
        4CCA7B55D59F180696CBF9EC0909577689599F89DC1C41F2A590B334EA071275
        9591A351A8063DBBD2085059E079552F1CD86D5D382D76F3B87F1C0043870E65
        5A77B31DEAF89AFDD9E5272D507244A112A750057160828183939186EF408340
        39067455227B272BDCBC648B4F88DD3EABB267C67D1EA1F2917B1F7FF64DA145
        1FA2F821530B9A286A3E7DEF60B6577798F7680B0C46E707F1B1A9BF88D7CF4E
        1EB84A33207F78CCCE344C7CB6312EFB5871E97710D091BE9DB54A51B262E611
        5ACA4A22FA15DA50E450A30EB168BAF53563B05E8B85C7CFE07A6E3ECC721636
        621914136B253AA829B43B5B1FE614FEA392DE9FF55F51BE68517F8519CC663A
        DCDCEBC7CCBB54C13D1A0E1303B393C29067B5C8F5C8C3A6CF356817C5E123CF
        06C2954BCEE14971296BAAAAC7995F0DF4639DFC81F021CA465F9FCEC1EE4316
        E8F532C8C5604F567252C780BE1A04F8E5A2A93218E77FE0AC56239A8F2FCDA1
        FC8F0160DAC2A8971AB560D6EDF3305199F728621693A2129B592EA67C117370
        3889C6DFC4033D9B6B907190172E9D74AC9C3A76D388AABC56A62747CEEDD44D
        357E4AC149EAD81639D8E0623CD3CF88C9C69E42FA619CA59C8AD726C6AE9346
        15452BC1BD8EEECEE9DE69FEDFADB56042BF00E4051561F372255140450BC021
        396E28882560E545A5CA069612506C57A14D7B1E2D239478DFC7031FA79FC6F6
        F34629DE440C40B5710226376B0AD53EAFC553A2B7C5942F63FF84E7A3B8E6D4
        C6EBC62C8A2BA624BF017174CE601230E6A5209C325DC7F6256ABC3D9645A71B
        FEA6FC5C67F7C963379FACAA2EE3570C6EA87170BB2286CA838F99EE513FA73A
        A5616D71AE9D62C499551EDD7B2B61E1E4E85DA8120E6C352F4888D9F2211E91
        9E0800662D8EDCA6E825EFBBEB5611DC4825D394139CA3C4F3B51EE90E3B8429
        51DFCD03B7F6DA85B4338E35698735EFAD5B57B9793463C9C06E7E3EF4EEEC1E
        596CC2E23C308D0A31B0A712432F850BE9D71CDF6A1CCA91AE7188734857D1A0
        317B7044E17ECA705183F103FC90E6E9813F961AE0103500DA091969F50CB100
        6C44F99389B9828892657350E83BC88E900641186056A04863C14BC7F6416E56
        4A6EDC7602E284E64D20DFE7F5D9A4B15BC7972F67C4F467DE726BCF7E73FCAA
        0D5A1B23E534100800CC4472B469E98E8F86EA307DDB75FCF9830233E3DD0890
        3C6E5B608FF878C4F6ECAAEA73CE8AA82087954B0DEF270BCB0B2AA0566D040A
        48F7A954F3B00B72B47A8685775D0A9D345EB8B8A6F8FCE431D57B56D52A0016
        AD7CFDFE99B07CEF42B3090E9B82B4200E1AB502AD9B98E1A3D3A1B94C815329
        56FE4E86308F31A47E5C950B7352D2600FA5177FB2F55BCE06FDBEBE0036D088
        E88E816879A8992DEB2FE7F8A963372C4139B3716EF290F974AF9C71E35333A5
        38AE1F468660AFF93EBE4E524BD94A188A97942971AA5A641247E4124B8B73E8
        2C5E1FC7A1BDA71702D3DC04F35FC4DC6A78903A72DA2EE51116FDA9125B12D3
        6F6FE3B9E363574D2A5FD681D3BA3FA708B36EDF535048D933645091A62A8691
        15108BE7B5487F7469A5409086C2B8755771E1108B95317EC8DDA9387AD5443F
        F7F5C4BF4DE70AEB61C1600F50F6D58D5AC807091D95D4DEB43C5CBF560C41E6
        86FA2144BA7AEBF1BC2F8F1B6B9C17278DDE18F68F0260D6A22117723BDB5BDC
        B110734B2583A7E8FCA901C2BDEDD0662B70709BDD6829E24626C66DF9A9BA67
        7DB260E0F75D072886FDE7D231EA369B8B192D5B923ED82F2FCFCCBD9A383675
        77F9EB45FB37D360BF76A0ED9590F5EB9C183E4C8DA196A6581D781B6B939C25
        C3C5A5FF9292F62559CD68888E1E1E1834C28A7F7BFB10D38A16EEDEE4CF3103
        725A4D3D761932A2C0DA093313DA8692961B346FC2D85F26967FF70789CFFB06
        842A33F6B7BD2F3FF08B85F08B878398A72D9BA9F1DECB01C49C17F3F988A372
        1C5E597C15F9F902BE79A5BE706B37B6DBDCCC43AA0B169586BFF59123B55ACC
        0EE9C6B865F97238785D065FA26CAAB54AB4B820176E9C73C427C46C9E8547A4
        270280998B238769FCF02DDF8763F46E06780A3EF0352A71E5888DBF73035BED
        6A217AC67BD52B2AD39207BDD3B409BB2A35E00AF573C60D24370A176CBB7437
        2C0C1F59D92A1AD317BFD839A489E3C0F06B67A88675E4984085392D4534ECC3
        2EB1E363DDE07454A466085272C93643EBA24D980C6F0515E2ECB79A229B591E
        553748D83B46B683B2DC56C14694D8491175E0F6876EE19498037115BD7FF6C2
        A8B58DFAAA5F3EEE7D83CA482F805A1988B60D55F0A4AD50898E299C0EEE4483
        F35058D179CE55283D787CD9B98970758FB0071AE6F59A8478CD581E5917363A
        DEDD837A4317226838154F3932052EF70EBE3C13A08E5E578375946B15001223
        161210A898D9EEDEF67AA6224591D1E0D80B5AB63871EC86DDD5DE4C24F3B485
        0387E9F5D40AAA6F8126E6F411AC08ED2218B76B2FDA58AE5FFCC8D4ACCA6E9D
        B7F8E5E50511191FCCDA9C8BA5BD4384A2B3EA4F3927D5D07390F1A529DF7330
        DC3257B812B84F1DD287BEE78DF77C94D89F9509AFAD75CE4D8C4E6DFDF9F2C1
        69A77A5F6AF2F3CE223809002676AF07FD1F7E4B27C5A48EA9E8FD492B7A78CB
        1CEA2DAD9F55B6F76B6FA6CE9B3430DA58A8C94B7C5436E8590E1E3202A66C1A
        A7B28C18BD231D8141C0C28826C2BD3F597381914F2372239F1614B7059ADAA5
        B2FF9532AE1247CEA4D5833D6416BA33E5E07CED3C753A296E43B5034C4F0D00
        520189364E764A34F7B1D73463E5B4A5037BD2763EB171334537599742EA8DFD
        4730A7796BD852F457058BB37B455E2CB3970DEAC3F24872289D2D942CB46BB4
        D7D9301F15DA5D083E21E3EC5D8C0CDD8056F2C70F0659355B7F2C2C89F37325
        2200864F72A2952F0DCB993A38EDBC4BEE0DDA30213A65C8ECE4A819CAEE0553
        C69DBA4C09050A44F7F242D081E6DF8E8FFEE9EDCAFE83E8D852CCDA3E54C885
        3141A1ACAF6F5D502A9D8CE09FE81B3607F2737821ED94C3C059E87BEA9EA666
        13526F53BEFEC48A68EB8E7A4457726389C4E058C8B254C29D8BCE0CBB931E39
        754CEA9627C99BA702809A9298FDB223C7BE4874A6989086B267FDC39DD40EC7
        2DACDA938757223CD1E670234BBE45D66EE27BBFA495BF77C6E21786FAD7617E
        3CDE3883FDE158015138496F2EB363B67B6BE1DE1DBC3E3D2E45D23346CF7F3E
        92D15A7FDC6F94B99DB96E2809CE24661DA562D0615800BA3436A12D41C21BE3
        73312E8685FFA17A4BC6476F884E4A7E35AC5188F3DC47EC1F54F6590DDEEEEB
        8EB0C3215BC68F4E79A1BAFF2535000FCF4E9CE0EC48836B2050829C16986286
        A2CEAB54C27A4331ADF2D7D3672E44DCF4DCB4C70C37A220BA135D498C81ACE3
        C922C85F83765A5F646EE11D8602EE95A9A3B66EF83F078069F35FE8C9C8A92F
        9AB69637F20CB3534789B6BE33AD0077C5E562881235A4831E0D4E04E404E864
        01AF94EBDB667DDDDF47C5CB2FFB0CE03DDF989A0529825354E8E4145A3E5780
        7FE537B84F731E111346FC2265DB1E39B75F93FBC5CCEC8C424724AF37C914C1
        3AB46AE38686AC113EB794F833F80696AFB423F12D1DD4077DA64C1C9B3A5BBC
        6FFEB221E7CFF64C0BFB76AD1543FA2BD1ED7893C371A337767A12FF7FF68241
        FB55FFBADF75F9B65CF8E9192868167FDD918173CA51C75F8EB61DF578C157C0
        89EFB9C2826275F88CD2B18EDAA2A70A804F1644F650EBB1A5F72B2AD5452E0F
        DBCF1620AF908383E361B6712834F350E9058C538509572E382312621F4C3137
        6BE1E0F8D0DEB6E9F32F6751270F2940734EC903A624CC9BC7FC680FD80EE8D6
        4E89D9F0AAEB7D91719175F575CCBD58D623AC09E3E363E7EF66C818A17B419F
        ACEE737F29C4EC819EA00E6B622745FF912C5E3F6F495402DDC930EDA33D37D1
        BD830A2F5D6B762BE6DD94902751079F7DD93FCB39282F60D39F85E04D4A9CFC
        939152BAD10C3121692211B40C26C7D9D1DA1A885D3F3A0F0905A61E5565127B
        5C7A6A0098BDEC053DCF33E7A2DE550466C18C2F36E7C3586403A3A4C1289C92
        4B95D526C0E07412DBBB112EEDA226278C7DD0AB66D6E241E703479C0B5BB9D5
        0DD7D335907166627609B09A696262D198FFAE1FE88BB431CD72CFA7BAB0A879
        C983E75A7ADD9D90B8EB2E3EED1C08DB31CDD8F831BB1789BFCDF93CAA456028
        7B3EDA7E88D2B30A4C36B72FCEDA61F0A9CC55ADA69494D4DF3DA0A9FAFEB5F0
        1BF24BD739EC4A9143AEE0C04A59BD649089F3FF0C8FC07A3486BFA187CF11A5
        70F13CF37EC298DF57D6165F9E1A009292072EE9D45B369A694663E5861C64E5
        DAC89F764ABE01A2EB13ADE0A594AC0E9AC7871141B8B096F92D2136F5E5B2FB
        67CD7ACB4B199293AD7BEDACECF0450F5CBD26A0D8AA839C2F4451B10206238F
        35EF07E3F266C18A3CCE23A61A9FBC598B2297697B1B47C61CB88A7911A170EC
        F59EFB71ECC6B2C11E6AC1B221E70E774A0B5B7FAC10ABFCDB0B57AEAA83677E
        F873C6E3D4C19C1543751E1EC2FD9C4EB765BB8FB03876C806B54A74A317036C
        5889F962548A20700809F7437C6715314FAD595641D1F471F30A55464F0500F1
        0B0736F1F1E6CFF57A5D295FBECF8463678D50294A06DC9D44397348398179B0
        723B946E14260DF0C399D5C2E929D1DBDB963D63D2C2A80EEE8D8A8FB25D1C54
        B195C19E8B2C01911B4275E9C833E9C16A81153DBCB0F54BEEC494319B3A5455
        9E694B227B077A2BB6EC6B775EFEED96228C88D4A1CDC1A645B9857CAFF8D2E4
        9673160D9966EF9C9D9070E40E96356D09E339ED7393E37E7BACC51C93929268
        F780B377EA0F2BAEFBE5EE3C1C3904520F62F28B925C7F3271414A4A9CF9A361
        E6D578EEA5000C2BB009270E3A261069F8596DF0E6A90020F1F381DFF51B2A7F
        235367C0846413F9D3349462524346902669C4F1539AB6C1C11320C8192CFB8F
        1F0E7CA1B83A71D46F4DCA9EF1DE277DFAF8B555EEF4E828830800131852F8B3
        3099CDC83235869FA70FA2B22CC2C993FCE8C4319B9657CA84E417C2742A7A9F
        6E4881E7F06DE7A0B0C96157DAB1A2572B146E70BB6D57F21DA60CDF7A3F6949
        54DB90FAF489E1F947A8A98D42A03E16387EC2D85F1F9B09890B23BF7CFE45F9
        7BC9E9466CDB6E825AF44096DB60E535A44E2CB0086A7869F2617268A0AAA7C3
        E7510C2EAC96679A8F1A1B3D6E175411D53A003E591C15A2F3A0D23ABD21578C
        5B5380ABD70BA111BDA80900642C0DB90404A79421DC4E747E953B832FDEF643
        CAD2A21353A377FFB725BF3BBB57AF9076F2DDFE1DD552266D31D3BF46918B5C
        8B07B2EC5A4492679EFCC97CD17ECAD4BEB28A12AD08CA4A1F0B7F95AE3FE4C4
        1E380D6C49AE298E82CAD7819F829FC3D114E7A684B8D4C1A4B552FA3A676EFE
        DEF44250884A8366271A6C9D387AFD80C7AD8F394BA25AB87B53A7B5BD54B2B1
        0B6F40AF754859478A3977521756F29FAC646F2392510613A741FB3E81783B9F
        134E1FE5DE491C57F9FAC48F4AB50E80A48551B3BB0F904D3AAB7162DC8C6CE8
        B402417A49DA7496283E7231B72D018238E529266AEC1EA1C29BC14494AFB32E
        9B16FB77F878F49CC856414DF9D34DFBAB2893B85E014793D622BAA85AD0402D
        C7AD5F79EBFD02BA6B65F989C9A5D48C85FD37F418A21A38227B3FAEEDF3000C
        AA92DC406AC284BA4644F6A5F0DAB567852BE7F937E3E3D6FFF0D9E2413F5FEB
        72E7D59D578D98A178C69995891D0E8E3B20A8A9D58F93A52B2939EAAB9E918A
        FF7C72F22E9576AD240E4D5C2B407456705715932E5106ABA0818648843AC14E
        4CECEC87B3ABA9A31F8F4DEDF8A8EFFC470010BDA8BFA2AE5C76B3FB08B5FFB8
        AF6EE3F071BBB44873898383D8E731A43BE02530C89992640FCBE23C7163BB52
        48BFE2EC9114BB697FD9B3E23E1FAAD2A96D597D47C8F50E27E92E04078C4E16
        5E4A27D253147CC64DEE83C4D8D4AF2A2B4BC2E2C8575B34637F3AA2CB250A9E
        B87C68C91882B40925710568988F0D1F06E1CA1A6D066EA2051B8243E7DADE69
        F9CD4A1E6DBB5268555F899E7A77149C901B0C45FCDB89315B531EA55E66CDEA
        EFC378D2E7DB0C53FABDB6F41EDCE55622D114A4180C189693126058890E20A6
        7C177D17860C9421645F5DFEC60DA1DD9318FE7D6A0020AD3FB2456BD9266707
        23F5AFD1C6D2051D4BB2BCC94A173850100920EEC5E9D77EDD3488EDEA898D2B
        ED67B8BC4DEDCA4F1B277E1E95DCB22B13DDAC3D4B89AD3F3753C0F53F9D8EFB
        7F39E2A78DDD3AAFAAB27CB238727FC48BCAAE7DA6DC2C8D6C9656DC79F0221B
        851EC3CC18EFD1583879D8B1BE616B7AC80727D229F3557769751188134BFE56
        CC1CE906FD813AE64283ACE3A4E875E71FA56E44279AFA0D98B58656F9F467BF
        16C14D5C9B4870835D50C2535B0C8B53038188453FB77C68EBCBF1BE7B00AE6F
        E73E9F3276D3233B7F5444B50B80E4A835FDFFC5BC3DE7C85F58BFE1C1A86251
        F7131BA124096462666B1A5F25F9C3F887205CBDC0BD322D2EF5D7F2CFFBF4D3
        E7351625FB194FD97B12C1CDF14EF9315AAE5E92306A5D955E36A2FB16E325BF
        7F546172FB766B7E69392AC8EB27E221C4823FFEDD0C477EB70BF7BADDA3162E
        28CD115F46A23BBABB15BF4CF0C3B5B5EA1DF131297D1FB97E16BEB034A2BB62
        E46AD37DEAF8D922E8D544B2414E5EC180A314443974404D2424AF95A35F9B7A
        50FF6EB97D54AF6AF838B37FE5A9D600209A3C5AFF33779BBFCBFABDF4D11D58
        8AB90ADF26C5F273023E18EE897F37D161FBF7F67D8221A5D7A3E6BDAB8848F7
        E1D926D49CBB26B388DA77DE2449A09244EBE2DA4442E98B4ADC4655FE020E7D
        D80417775A31DD7C05D776B895E424FE2F095264D398383DDADDD6F2B7EE0A6D
        A655E1D85A1589C034F0F4AE4E83349DA75FCCA62CF9B944FC33C8B1FA12C613
        BD8465E16494F0D1E5C23B380843D39DC295EB42F843ACC3502DD51E00E60D6E
        1DFA0C4E9D0FB65173E7107D4951C9AB082EEA876AB06C823FD27F34DB72F3A8
        F069B18F56A195D1EC05931A34689E7D7DF9F59BD4D12B66B0D2F4B0D8ED482B
        124969DF78D19D4BE0115C57855F3EAC8BB45D36C4E69C43D66ECF0700207A15
        89164C2F628D8CEAE085735BF839F163374E7ED4B2CD9C39D08F76E70F747C15
        8D261CCD476E8E0232054BBA8142D81815291F9104A47B10BCF598C8F0B8B09B
        9B9810B769DEA3BEAF3CD51A00481F37A2436FF9F28F8E18A94B470C95E61510
        C70296CED3C1FBAC4EB8709C9B9A189732F34997655EF2F846214DEF5F599991
        4E1D4BB3108697A69D151D1144E55B741B170140C47B70901A6BC678E3C07734
        2EB7BF87C5738A895880A49829E534346A1A2A2503BD3B87B9237D71FC4BFE52
        FC98CD2D1EA77CF12BFA3774E3B1A7E140B77A4B2F675006030D77371BB1743C
        C0D14A884E3616A52F3EF22500F8CDBC262176E73B8FF33E57AA35004C5F1499
        DC6CB03CE6CD45D9B01AED15BFC9C663D8E860FCCB4F86B3EBCDFB9D05A6DEB5
        31F1F1E9A71FF8FA35CBBAB7B9D844ED3A5D0C19C34BC9A15196E2A8F4439400
        618D6558F2B6177E5EE010BABEEB410D5F9485E2221BDCD48294F246B45CC41C
        8834CBE3AD17EB407D40E06F65D081B3E27ECF7E9C32CE9ADFBB31AD526F6B3B
        D012B2C45084BB996E440F50C1CBAD087575A251D214F13A050EAFB72C27D6C7
        A8275537B50680194BFBAEF51DA01DFA7ED2CD8AC5BF4D40AF41DE78B79707B2
        7F29FECBE29475881FBDF1B1C6DA2BA3B9AB9AB8E93D1A1BCE686CCCAFFB8D50
        10E6F16553C99420897F460C162192FE99961ACC7ED117BF2DCAB3356BA3939F
        D6DBA91FD71BE0A9734A09AF64C47C51AB38D215D088E848A34DB60E674FF083
        A78F4DD9F8D875B63CB22EEDC46F41EDE4E17FD63550794577A15358A4EC6AF5
        54CFC26B3FC3DFCEE0FB258ED9B4F349D54DED4980259D37D8DBEB07CDF8E26E
        49B8B32B1125AA5D1F3DDE7DD11DD44687EDAF02AA7FE2E84D7B6BAB2CA2E3E8
        3D1B77AFB089C97BC5967C0200014E5E0C0E2FF11513D72A94B30ED89C0C5AB6
        F642620F2D76AEB16D6619AE71EBE142A3E59B735164E2A5BE9F673450117396
        63E4681F968B36F6409CDFC14D4D88497D225D57DCD008A57767EF44BD171D2B
        6B6351D8BCCDF02956C270426BBF9B6D4F9A1EB765F693AC9B5A03C0BC25FD57
        E43464DEFF6CDD5DC9355B2251DD2602FEF9176518D0A31E3CF659F9DBE9DCA8
        C4D82D2B6AAB1C653477E990B3BADEFC3333D6DE835AC64B5136921490FA011E
        72DA01934381F02E9E98D05485ADBF142FA69D8A63C1CF50DFFED5CA42EDDD57
        0C778D9D6057274D6471441284D5CF460BA63E2EAEB57F9718B765F843D4A950
        CD77C47FF25203A51BF716F8A25602A3BA63B12BBE9C35FEB70B955D8F6A172E
        A8986A0D0073960C794D13C2FF18FD7BA6B8ACA8A4EDEB423468DDD71F933BE5
        217F874EB87ED13E33615CEAD427FCEA0AFFD38CC591EB42FBAB5E9AB2EE2601
        80380B29E6DD5548412C629E007130B0D822C70B51DE7895F4F3BB365B27FEF4
        C5D6E4B7C70CD8DDEE15743E6CCE414E0E0F83D35FCA071CEC990D25B1CF5BA8
        B5B8B44A7D606ADCA67ED594AB7489AF07BE57778C6AAEA90E488FBE78F4E392
        E81FA7F275DBA16B8C1E1734722A20B400011E5AB4B02B7076BB95CFBC23CC48
        8A4D49AA49212B292BF530BF257ED63F3E3C4A93109B9A0115C3492F15834315
        AC0D564E05256381D1A6C088B7FCF0CC4D87F0E71EEBD039F1DBB78C9BD8B771
        60A8FC60FD378BB447B36CC833B9434DCC325EE606ADB218A12A0DB256179C4A
        8CDBFF5C25952FE041E6BB1EF3E5AE2F0F1254F35DA8E0FE9A5C5B69C53D5192
        D61128727F87A19CAF6ADC35AD685A60F30CD673C4AA9D353D66F3E6EA9886CA
        995CD1EF555E33714E649F8E5DD98DCB3232A9BCBCD2CCA1D28F441A502CEC9C
        0C0E283179B80FA89D167EC7B6FBED7FFEE2F06D72091D97D4F3C5C6CDDC96B9
        0FCE6532CC6688B145365E8542A706A1164FDCFBD99C923469EB183CC858A1DC
        C697DBBB9E072A064A55FBCA982C5472BEA2E3A7E711B462C5FBB25D970DECBA
        05EB5CA76AA91AEE1FE6DA0AF7CF0FEAA01BFC729DAB27EAF1CA6317F3A060C4
        19451518CA26BAA24819441CEEFE98F7B23BCEAE2C36C44FD8D5C166949C8AC4
        816366E4F8AEAFB768EB31C9D24D41E76BB29065F04707FD4D085B03B8C33BEE
        7DF0D3EA9347F077CEB4322673E5CE712EE7E1722C54B055C6D09A32B9464078
        1A00A88C8935611C55C53DE57FA7CBDD4F97BF6FC6FCA89FB4CFF13DBF3F9A01
        9A9623DFA68387A290A8283C14B4154D5BCB31BE5928BE996F3C322F7E871877
        CF946ED2EC41D42B2DDBB70BAF3B5215620D3A6F50C333D776FBDE25C3576BBF
        3DB51F7F33DB597AECFA9D2F77AE8CE9AEC7AED201E58E2B637E55D2A13A10D4
        3A006AC2D0EA185BD14657725CD13570F98EE80F7B0C8918E4F6E9C2DB77298B
        4D0D3BCF48B390DEAA1C14DA55181C19881E790CBE5974F7C735CB0F6F2C65BE
        6B1DF10C03AA5EA84E677182FB2BDD985BCA604729231DA58CB3977E77BA6C1C
        1E04411938CA18559924A8890E51592BAFEAF75A0540552DB5A6CCA65DF6551D
        57F6BDFC33E0E9AB524F9AD62BE54C03DEEB746691943A461C0350B22698DD03
        B16A901CD7365B84E4E9A7E69F399E59B6B04359CB2C63627986961DDB4B0150
        B6395D8E39977365F9547997E33266F12EC7E57584F2BA82EB71650CAE568FA8
        0D0054C4ECF2E2F96118EFBA95B548C6E57B557BD7FB4431AE787D788717DAF6
        F71DB591BE451513295040BA015A66C7C8015A442AB5F8E2D3DC7B0B12F77C8D
        075B6619A3AD2EFB3286BA32DAF5BCCD85F18ED267D9F1A03E800A00509EE9D5
        4982AAC47FB5A6636D01A0326657C7FCF2ADBC224652A5CC7465348BBFFB6BD7
        7E5BFC4DCC1D2E4607AAC5639A86F6CD915DDE773455B6E55BDC05A565F06C68
        430C50CB90F2B389FB6ED9B9F5678E6566E04191ED2C659EC860532943CB5ABC
        2B206C2E0C2FEB06CABA853249E12AEECB33BDBC9550918550D3165E9DF5506B
        00A8290880EAFB70BA8AAD8CD1954900D74D0971C9F392CD836669DF4EFDC27A
        F7EA56B7554030ADB5D92921FD7251CEC15DE9874E1DCDBC89BF5BB5DD85F156
        1706DB2BD9CAEEABACCFAF8AE195B5F64769F1351958AA55003C2C101E561FA8
        4E27287F5E560A02B7D24D4736779448055969259531CE5ECA689B0B631D2EDF
        CBF7F3E57582F25A7E45ADBA3A65EF61FAF11AB5F4EA18F4B4A8B62C839A0048
        94046540103779E9C6965EE36A969557DC5C154057D3CEB54557C65CE0C933F7
        91985D1543FE497AAC011E540D9E8ACE572439CAAEA9AC3596672C1E63FFB0BF
        953FAE95CAFF5FA5EA8682AB3A7ED87355514D67DF1E65B2A7D6985B13FA5F07
        C0D3FA5FE5AF7F58A6FCA34C7C1CFABF0A80FF4F35A4FF07953ECF3348E5C24B
        0000000049454E44AE426082}
    end
    object Label1: TLabel
      Left = 133
      Top = 58
      Width = 30
      Height = 13
      Caption = '-- .. --'
    end
    object Label2: TLabel
      Left = 133
      Top = 33
      Width = 262
      Height = 19
      Caption = 'Embarcadero REST-Library :: Demos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object pc_Data: TPageControl
    Left = 0
    Top = 464
    Width = 894
    Height = 292
    ActivePage = ts_ResponseData
    Align = alBottom
    TabOrder = 3
    TabPosition = tpBottom
    TabWidth = 125
    object ts_ResponseData: TTabSheet
      Caption = 'ResponseData'
      object memo_ResponseData: TMemo
        Left = 0
        Top = 19
        Width = 886
        Height = 247
        Align = alClient
        BorderStyle = bsNone
        Lines.Strings = (
          'memo_ResponseData')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 886
        Height = 19
        Align = alTop
        ParentBackground = False
        TabOrder = 1
        DesignSize = (
          886
          19)
        object lbl_status: TLabel
          Left = 6
          Top = 2
          Width = 859
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
        end
      end
    end
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 744
    Top = 656
  end
  object RESTResponseDataSetAdapter: TRESTResponseDataSetAdapter
    Dataset = ClientDataSet
    FieldDefs = <>
    OnBeforeOpenDataSet = RESTResponseDataSetAdapterBeforeOpenDataSet
    Left = 192
    Top = 600
  end
  object RESTClient: TRESTClient
    Authenticator = OAuth1_FitBit
    Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Params = <>
    HandleRedirects = True
    Left = 40
    Top = 528
  end
  object RESTResponse: TRESTResponse
    Left = 192
    Top = 528
  end
  object OAuth2_GoogleTasks: TOAuth2Authenticator
    AccessTokenEndpoint = 'https://accounts.google.com/o/oauth2/token'
    AccessTokenExpiry = 41488.448189351900000000
    AuthorizationEndpoint = 'https://accounts.google.com/o/oauth2/auth'
    RedirectionEndpoint = 'urn:ietf:wg:oauth:2.0:oob'
    Scope = 'https://www.googleapis.com/auth/tasks'
    Left = 360
    Top = 504
    AccessTokenExpiryDate = 41488.4481893519d
  end
  object OAuth2_Facebook: TOAuth2Authenticator
    AuthorizationEndpoint = 'https://www.facebook.com/dialog/oauth'
    RedirectionEndpoint = 'https://www.facebook.com/connect/login_success.html'
    ResponseType = rtTOKEN
    Scope = 'user_about_me,user_birthday'
    Left = 360
    Top = 552
  end
  object OAuth2_Foursquare: TOAuth2Authenticator
    AccessTokenParamName = 'oauth_token'
    AuthorizationEndpoint = 'https://foursquare.com/oauth2/authenticate'
    ResponseType = rtTOKEN
    Left = 360
    Top = 600
  end
  object ClientDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 664
    Top = 656
  end
  object OAuth2_Dropbox: TOAuth2Authenticator
    AuthorizationEndpoint = 'https://www.dropbox.com/1/oauth2/authorize'
    ResponseType = rtTOKEN
    Left = 360
    Top = 656
  end
  object RESTRequest: TRESTRequest
    Client = RESTClient
    Params = <>
    Response = RESTResponse
    OnAfterExecute = RESTRequestAfterExecute
    SynchronizedEvents = False
    OnHTTPProtocolError = RESTRequestHTTPProtocolError
    Left = 112
    Top = 528
  end
  object HTTPBasic_DelphiPRAXiS: THTTPBasicAuthenticator
    Username = 'BAAS'
    Password = 'test'
    Left = 360
    Top = 448
  end
  object OAuth1_Twitter: TOAuth1Authenticator
    AccessTokenEndpoint = 'https://api.twitter.com/oauth/access_token'
    RequestTokenEndpoint = 'https://api.twitter.com/oauth/request_token'
    AuthenticationEndpoint = 'https://api.twitter.com/oauth/authenticate'
    CallbackEndpoint = 'oob'
    Left = 472
    Top = 512
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 20
    Top = 5
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = HTTPBasic_DelphiPRAXiS
      FieldName = 'UserName'
      Control = edt_DP_Username
      Track = True
    end
    object LinkControlToField2: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = HTTPBasic_DelphiPRAXiS
      FieldName = 'Password'
      Control = edt_DP_Password
      Track = True
    end
    object LinkControlToField3: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = OAuth1_Twitter
      FieldName = 'RequestToken'
      Control = edt_Twitter_RequestToken
      Track = True
    end
    object LinkControlToField5: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = OAuth1_Twitter
      FieldName = 'ConsumerSecret'
      Control = edt_Twitter_ConsumerSecret
      Track = True
    end
    object LinkControlToField6: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = OAuth1_Twitter
      FieldName = 'ConsumerKey'
      Control = edt_Twitter_ConsumerKey
      Track = True
    end
    object LinkControlToField7: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = OAuth1_Twitter
      FieldName = 'VerifierPIN'
      Control = edt_Twitter_AuthVerifier
      Track = True
    end
    object LinkControlToField8: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = OAuth1_Twitter
      FieldName = 'AccessToken'
      Control = edt_Twitter_AccessToken
      Track = True
    end
    object LinkControlToField4: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = OAuth1_Twitter
      FieldName = 'AccessTokenSecret'
      Control = edt_Twitter_AccessTokenSecret
      Track = True
    end
    object LinkControlToField9: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = OAuth1_Twitter
      FieldName = 'RequestTokenSecret'
      Control = edt_Twitter_RequestTokenSecret
      Track = True
    end
  end
  object OAuth1_FitBit: TOAuth1Authenticator
    AccessTokenEndpoint = 'https://api.fitbit.com/oauth/access_token'
    RequestTokenEndpoint = 'https://api.fitbit.com/oauth/request_token'
    AuthenticationEndpoint = 'https://www.fitbit.com/oauth/authorize'
    CallbackEndpoint = 'oob'
    Left = 472
    Top = 576
  end
end
