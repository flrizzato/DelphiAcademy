//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uMain_frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.StrUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DB,
  Vcl.ExtCtrls, Vcl.ComCtrls, System.DateUtils, Vcl.Grids, Vcl.DBGrids, System.IniFiles,
  Vcl.Imaging.pngimage, Datasnap.DBClient, Datasnap.Provider, System.Generics.Collections,
  REST.Utils,
  REST.Types,
  REST.Client,
  REST.Response.Adapter,
  REST.Authenticator.Simple,
  REST.Authenticator.Basic,
  REST.Authenticator.OAuth,
  Data.Bind.EngExt, Vcl.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.Components,
  Data.Bind.ObjectScope, IPPeerClient;

type
  Tfrm_Main = class(TForm)
    btn_DelphiPRAXiS: TButton;
    Button3: TButton;
    pc_Demos: TPageControl;
    ts_DelphiPRAXiS: TTabSheet;
    ts_Twitter: TTabSheet;
    pnl_Header: TPanel;
    Splitter1: TSplitter;
    memo_Desc_DP: TMemo;
    memo_Desc_Twitter: TMemo;
    edt_DP_BaseURL: TLabeledEdit;
    edt_DP_ResourcePath: TLabeledEdit;
    edt_DP_Username: TLabeledEdit;
    edt_DP_Password: TLabeledEdit;
    pc_Data: TPageControl;
    ts_ResponseData: TTabSheet;
    memo_ResponseData: TMemo;
    edt_Twitter_BaseURL: TLabeledEdit;
    btn_Twitter: TButton;
    edt_Twitter_ResourceURI: TLabeledEdit;
    edt_Twitter_Status: TLabeledEdit;
    btn_CreateUniqueTweet: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edt_Twitter_ConsumerKey: TLabeledEdit;
    edt_Twitter_ConsumerSecret: TLabeledEdit;
    edt_Twitter_AccessToken: TLabeledEdit;
    edt_Twitter_AccessTokenSecret: TLabeledEdit;
    ts_GoogleTasks: TTabSheet;
    memo_Desc_GoogleTasks: TMemo;
    btn_GoogleTasks_FetchAuthToken: TButton;
    edt_GoogleTasks_BaseURL: TLabeledEdit;
    edt_GoogleTasks_AuthCode: TLabeledEdit;
    edt_GoogleTasks_AccessToken: TLabeledEdit;
    edt_GoogleTasks_ClientID: TLabeledEdit;
    edt_GoogleTasks_ClientSecret: TLabeledEdit;
    tsFoursquare: TTabSheet;
    edt_GoogleTasks_RefreshToken: TLabeledEdit;
    btn_GoogleTasks_FetchLists: TButton;
    ts_Facebook: TTabSheet;
    memo_Desc_Facebook: TMemo;
    edt_Facebook_BaseURL: TLabeledEdit;
    btn_Facebook_FetchAuthToken: TButton;
    edt_Facebook_AppID: TLabeledEdit;
    edt_Facebook_AppSecret: TLabeledEdit;
    edt_FaceBook_AccessToken: TLabeledEdit;
    btn_Facebook_FetchData: TButton;
    Image1: TImage;
    edt_Facebook_ResourceURI: TLabeledEdit;
    edt_GoogleTasks_ResourceURI: TLabeledEdit;
    btn_Twitter_RequestAuthPIN: TButton;
    btn_Twitter_RequestAccessToken: TButton;
    edt_Twitter_AuthVerifier: TLabeledEdit;
    ts_DataSet: TTabSheet;
    memo_Desc_DataSet: TMemo;
    edt_DataSet_BaseURL: TLabeledEdit;
    edt_DataSet_ResourceURI: TLabeledEdit;
    btn_DataSet_FetchData: TButton;
    DBGrid1: TDBGrid;
    DataSource: TDataSource;
    RESTResponseDataSetAdapter: TRESTResponseDataSetAdapter;
    RESTClient: TRESTClient;
    RESTResponse: TRESTResponse;
    tc_Twine: TTabSheet;
    edt_twine_device: TLabeledEdit;
    edt_twine_url: TLabeledEdit;
    edt_twine_resource_sensors: TLabeledEdit;
    ButtonTwine: TButton;
    Memo1: TMemo;
    Lbl_Temperature: TLabel;
    memo_Desc_Foursquare: TMemo;
    edt_Foursquare_BaseURL: TLabeledEdit;
    edt_Foursquare_ClientID: TLabeledEdit;
    edt_Foursquare_ClientSecret: TLabeledEdit;
    btn_Foursquare_FetchAuthToken: TButton;
    edt_Foursquare_AuthCode: TLabeledEdit;
    btn_Foursquare_Fetch: TButton;
    ts_DropBox: TTabSheet;
    memo_Desc_DropBox: TMemo;
    edt_DropBox_ClientSecret: TLabeledEdit;
    edt_DropBox_ClientID: TLabeledEdit;
    edt_DropBox_AuthCode: TLabeledEdit;
    edt_DropBox_BaseURL: TLabeledEdit;
    edt_DropBox_ResourceURI: TLabeledEdit;
    btn_DropBox_FetchAuthCode: TButton;
    btn_DropBox_FetchData: TButton;
    edt_DropBox_AccessToken: TLabeledEdit;
    OAuth2_GoogleTasks: TOAuth2Authenticator;
    OAuth2_Facebook: TOAuth2Authenticator;
    OAuth2_Foursquare: TOAuth2Authenticator;
    ClientDataSet: TClientDataSet;
    edt_twine_AccessKey: TLabeledEdit;
    OAuth2_Dropbox: TOAuth2Authenticator;
    RESTRequest: TRESTRequest;
    HTTPBasic_DelphiPRAXiS: THTTPBasicAuthenticator;
    edt_Foursquare_ResourceURI: TLabeledEdit;
    edt_Foursquare_AccessToken: TLabeledEdit;
    Panel1: TPanel;
    lbl_status: TLabel;
    OAuth1_Twitter: TOAuth1Authenticator;
    edt_Twitter_RequestToken: TLabeledEdit;
    edt_Twitter_RequestTokenSecret: TLabeledEdit;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkControlToField6: TLinkControlToField;
    LinkControlToField7: TLinkControlToField;
    LinkControlToField8: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField9: TLinkControlToField;
    FitBit: TTabSheet;
    btn_FitBit_RequestToken: TButton;
    edt_FitBit_AccessToken: TLabeledEdit;
    btn_FitBit_RequestAccessToken: TButton;
    edt_Fitbit_RequestToken: TLabeledEdit;
    edt_FitBit_AuthVerifier: TLabeledEdit;
    edt_FitBit_ConsumerSecret: TLabeledEdit;
    Button4: TButton;
    edt_FitBit_BaseURL: TLabeledEdit;
    Memo2: TMemo;
    edt_FitBit_ResourceURI: TLabeledEdit;
    edt_FitBit_ConsumerKey: TLabeledEdit;
    OAuth1_FitBit: TOAuth1Authenticator;
    edt_FitBit_RequestTokenSecret: TLabeledEdit;
    edt_FitBit_AccessTokenSecret: TLabeledEdit;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure btn_DelphiPRAXiSClick(Sender: TObject);
    procedure btn_TwitterClick(Sender: TObject);
    procedure btn_CreateUniqueTweetClick(Sender: TObject);
    procedure btn_GoogleTasks_FetchAuthTokenClick(Sender: TObject);
    procedure btn_GoogleTasks_FetchListsClick(Sender: TObject);
    procedure btn_Facebook_FetchAuthTokenClick(Sender: TObject);
    procedure btn_Facebook_FetchDataClick(Sender: TObject);
    procedure btn_Twitter_RequestAuthPINClick(Sender: TObject);
    procedure btn_Twitter_RequestAccessTokenClick(Sender: TObject);
    procedure btn_DataSet_FetchDataClick(Sender: TObject);
    procedure ButtonTwineClick(Sender: TObject);
    procedure btn_Foursquare_FetchAuthTokenClick(Sender: TObject);
    procedure btn_Foursquare_FetchClick(Sender: TObject);
    procedure edt_DropBox_FetchAuthCodeClick(Sender: TObject);
    procedure edt_DropBox_FetchDataClick(Sender: TObject);
    procedure RESTRequestAfterExecute(Sender: TCustomRESTRequest);
    procedure RESTRequestHTTPProtocolError(Sender: TCustomRESTRequest);
    procedure RESTResponseDataSetAdapterBeforeOpenDataSet(Sender: TObject);
    procedure btn_FitBit_RequestTokenClick(Sender: TObject);
    procedure btn_FitBit_RequestAccessTokenClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    FCloseLevel: integer;
    FINIFilename: string;

    procedure LoadConnectionData(const AFilename: string);

    procedure ClearMemos;
    procedure ResetRESTComponentsToDefaults;

    function GetMonospaceFontName: string;

    procedure DataSetAdapterOnBeforeOpen(Sender: TObject);

    function ExtractTokenValue(const AToken, AContent: string): string;

    procedure OAuth2_Facebook_AccessTokenRedirect(const AURL: string; var DoCloseWebView: boolean);
    procedure OAuth2_Foursquare_AccessTokenRedirect(const AURL: string; var DoCloseWebView: boolean);
    procedure OAuth2_GoogleTasks_BrowserTitleChanged(const ATitle: string; var DoCloseWebView: boolean);
    procedure OAuth2_DropBox_AccessTokenRedirect(const AURL: string; var DoCloseWebView: boolean);
  public
    { Public declarations }
  end;

var
  frm_Main: Tfrm_Main;

implementation

uses
  System.UITypes, System.JSON, REST.Authenticator.OAuth.WebForm.Win, REST.Json;

{$R *.dfm}

procedure Tfrm_Main.btn_CreateUniqueTweetClick(Sender: TObject);
begin
  /// twitter does not allow the same status ("tweet")
  /// multiple times in a row. so we have to be creative...
  edt_Twitter_Status.Text := 'Test ' + IntToStr(Random(MAXINT));
end;

procedure Tfrm_Main.btn_DataSet_FetchDataClick(Sender: TObject);
begin
  ResetRESTComponentsToDefaults;

  RESTResponseDataSetAdapter.Response:= RESTResponse;
  RESTResponseDataSetAdapter.Dataset:= ClientDataSet;

  RESTClient.BaseURL := edt_DataSet_BaseURL.Text;
  RESTRequest.Resource := edt_DataSet_ResourceURI.Text;
  RESTRequest.Execute;
end;

procedure Tfrm_Main.btn_DelphiPRAXiSClick(Sender: TObject);
begin
  ResetRESTComponentsToDefaults;

  RESTClient.BaseURL := edt_DP_BaseURL.Text;
  RESTClient.Authenticator := HTTPBasic_DelphiPRAXiS;

  RESTRequest.Resource := edt_DP_ResourcePath.Text;

  HTTPBasic_DelphiPRAXiS.Username := edt_DP_Username.Text;
  HTTPBasic_DelphiPRAXiS.Password := edt_DP_Password.Text;

  RESTRequest.Execute;
end;

procedure Tfrm_Main.btn_Facebook_FetchAuthTokenClick(Sender: TObject);
var
  LURL: string;
  wv: Tfrm_OAuthWebForm;
begin
  edt_GoogleTasks_AuthCode.Text := '';
  edt_GoogleTasks_AccessToken.Text := '';

  LURL := 'https://www.facebook.com/dialog/oauth';
  LURL := LURL + '?client_id=' + URIEncode(edt_Facebook_AppID.Text);
  LURL := LURL + '&response_type=token';
  LURL := LURL + '&scope=' + URIEncode('user_about_me,user_birthday');
  LURL := LURL + '&redirect_uri=' + URIEncode('https://www.facebook.com/connect/login_success.html');

  wv := Tfrm_OAuthWebForm.Create(self);
  wv.OnAfterRedirect := OAuth2_Facebook_AccessTokenRedirect;
  wv.ShowModalWithURL(LURL);

  wv.Release;
end;

procedure Tfrm_Main.btn_Facebook_FetchDataClick(Sender: TObject);
begin
  ResetRESTComponentsToDefaults;

  RESTClient.BaseURL := edt_Facebook_BaseURL.Text;
  RESTClient.Authenticator := OAuth2_Facebook;

  RESTRequest.Resource := edt_Facebook_ResourceURI.Text;

  OAuth2_Facebook.AccessToken := edt_FaceBook_AccessToken.Text;

  RESTRequest.Execute;
end;

procedure Tfrm_Main.btn_FitBit_RequestTokenClick(Sender: TObject);
var
  wv: Tfrm_OAuthWebForm;
  LURL: string;
begin
  ResetRESTComponentsToDefaults;

  edt_FitBit_AccessToken.Text := '';
  edt_FitBit_AccessTokenSecret.Text := '';
  edt_FitBit_AuthVerifier.Text := '';
  edt_FitBit_RequestToken.Text := '';
  edt_FitBit_RequestTokenSecret.Text := '';
  edt_FitBit_AuthVerifier.Text := '';


  /// we need to transfer the data here manually
  OAuth1_FitBit.ConsumerKey := edt_FitBit_ConsumerKey.Text;
  OAuth1_FitBit.ConsumerSecret := edt_FitBit_ConsumerSecret.Text;

  OAuth1_FitBit.AccessToken := '';
  OAuth1_FitBit.AccessTokenSecret := '';
  OAuth1_FitBit.RequestToken := '';
  OAuth1_FitBit.RequestTokenSecret := '';
  OAuth1_FitBit.VerifierPIN := '';
  OAuth1_FitBit.CallbackEndpoint := '';

  /// a client-id is required
  if (OAuth1_FitBit.ConsumerKey = '') then
  begin
    TaskMessageDlg('Error', 'A Consumer-ID ("client-id" or "app-id") is required.', mtError, [mbOK], 0);
    EXIT;
  end;

  /// step #1, get request-token
  RESTClient.BaseURL := OAuth1_FitBit.RequestTokenEndpoint;
  RESTClient.Authenticator := OAuth1_FitBit;

  RESTRequest.Method := TRESTRequestMethod.rmPOST;

  RESTRequest.Execute;

  OAuth1_FitBit.RequestToken := ExtractTokenValue('oauth_token=', RESTResponse.Content);
  OAuth1_FitBit.RequestTokenSecret := ExtractTokenValue('oauth_token_secret=', RESTResponse.Content);

  edt_FitBit_RequestToken.Text := OAuth1_FitBit.RequestToken;
  edt_FitBit_RequestTokenSecret.Text := OAuth1_FitBit.RequestTokenSecret;

  /// step #2: get the auth-verifier (PIN must be entered by the user!)
  LURL := OAuth1_FitBit.AuthenticationEndpoint;
  LURL := LURL + '?oauth_token=' + OAuth1_FitBit.RequestToken;

  wv := Tfrm_OAuthWebForm.Create(self);
  try
    wv.ShowModalWithURL(LURL);
  finally
    wv.Release;
  end;
end;

procedure Tfrm_Main.btn_Foursquare_FetchAuthTokenClick(Sender: TObject);
var
  LURL: string;
  wv: Tfrm_OAuthWebForm;
begin
  /// step #1: get the auth-code
  LURL := 'https://foursquare.com/oauth2/authenticate';
  LURL := LURL + '?client_id=' + URIEncode(edt_Foursquare_ClientID.Text);
  LURL := LURL + '&response_type=' + URIEncode('token');
  LURL := LURL + '&redirect_uri=' + URIEncode('{ANY HTTP/HTTPS-SITE YOU OWN OR YOU TRUST}');
  // optional
  // LURL := LURL + '&login_hint=' + URIEncode('user@example.com');

  wv := Tfrm_OAuthWebForm.Create(self);
  try
    wv.OnBeforeRedirect := self.OAuth2_Foursquare_AccessTokenRedirect;
    wv.OnAfterRedirect := self.OAuth2_Foursquare_AccessTokenRedirect;
    wv.ShowModalWithURL(LURL);

    if (StartsText('Success code', wv.LastTitle)) then
      edt_GoogleTasks_AuthCode.Text := Copy(wv.LastTitle, 14, Length(wv.LastTitle));
  finally
    wv.Release;
  end;

end;

procedure Tfrm_Main.btn_Foursquare_FetchClick(Sender: TObject);
begin
  ResetRESTComponentsToDefaults;

  RESTClient.BaseURL := edt_Foursquare_BaseURL.Text;
  RESTClient.Authenticator := OAuth2_Foursquare;

  OAuth2_Foursquare.AccessToken := edt_Foursquare_AccessToken.Text;

  RESTRequest.Resource := edt_Foursquare_ResourceURI.Text;
  RESTRequest.Method := TRESTRequestMethod.rmGET;
  RESTRequest.Params.AddItem('USER_ID', 'self', TRESTRequestParameterKind.pkURLSEGMENT);
  RESTRequest.Params.AddItem('v', FormatDateTime('yyyymmdd', Now), TRESTRequestParameterKind.pkGETorPOST);

  RESTRequest.Execute;
end;

procedure Tfrm_Main.btn_GoogleTasks_FetchAuthTokenClick(Sender: TObject);
var
  LURL: string;
  wv: Tfrm_OAuthWebForm;
  LToken: string;
begin
  edt_GoogleTasks_AuthCode.Text := '';
  edt_GoogleTasks_AccessToken.Text := '';
  edt_GoogleTasks_RefreshToken.Text := '';

  /// step #1: get the auth-code
  LURL := 'https://accounts.google.com/o/oauth2/auth';
  LURL := LURL + '?response_type=' + URIEncode('code');
  LURL := LURL + '&client_id=' + URIEncode(edt_GoogleTasks_ClientID.Text);
  LURL := LURL + '&redirect_uri=' + URIEncode('urn:ietf:wg:oauth:2.0:oob');
  LURL := LURL + '&scope=' + URIEncode('https://www.googleapis.com/auth/tasks');
  // optional
  // LURL := LURL + '&login_hint=' + URIEncode('user@example.com');

  wv := Tfrm_OAuthWebForm.Create(self);
  try
    wv.OnTitleChanged := self.OAuth2_GoogleTasks_BrowserTitleChanged;
    wv.ShowModalWithURL(LURL);
  finally
    wv.Release;
  end;

  /// step #2: get the access-token

  ResetRESTComponentsToDefaults;

  RESTClient.BaseURL := 'https://accounts.google.com/';

  RESTRequest.Method := TRESTRequestMethod.rmPOST;
  RESTRequest.Resource := 'o/oauth2/token';
  RESTRequest.Params.AddItem('code', edt_GoogleTasks_AuthCode.Text, TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.Params.AddItem('client_id', edt_GoogleTasks_ClientID.Text, TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.Params.AddItem('client_secret', edt_GoogleTasks_ClientSecret.Text, TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.Params.AddItem('redirect_uri', 'urn:ietf:wg:oauth:2.0:oob', TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.Params.AddItem('grant_type', 'authorization_code', TRESTRequestParameterKind.pkGETorPOST);

  RESTRequest.Execute;

  if RESTRequest.Response.GetSimpleValue('access_token', LToken) then
    // edt_GoogleTasks_AccessToken.Text := LToken;
    OAuth2_GoogleTasks.AccessToken := LToken;
  if RESTRequest.Response.GetSimpleValue('refresh_token', LToken) then
    // edt_GoogleTasks_RefreshToken.Text := LToken;
    OAuth2_GoogleTasks.RefreshToken := LToken;

end;

procedure Tfrm_Main.btn_TwitterClick(Sender: TObject);
begin
  if (edt_Twitter_Status.Text = '') then
  begin
    TaskMessageDlg('Error', 'Status-Field (the "tweet") must not be empty.', mtError, [mbOK], 0);
    EXIT;
  end;

  ResetRESTComponentsToDefaults;

  RESTClient.BaseURL := edt_Twitter_BaseURL.Text;
  RESTClient.Authenticator := OAuth1_Twitter;

  // OAuth1_Twitter.CallbackEndpoint:= '';

  RESTRequest.Resource := edt_Twitter_ResourceURI.Text;

  RESTRequest.Method := TRESTRequestMethod.rmPOST;
  RESTRequest.Params.AddItem('status', edt_Twitter_Status.Text, TRESTRequestParameterKind.pkGETorPOST);

  RESTRequest.Execute;
end;

procedure Tfrm_Main.btn_Twitter_RequestAuthPINClick(Sender: TObject);
var
  LToken: string;
  wv: Tfrm_OAuthWebForm;
  LURL: string;
begin
  ResetRESTComponentsToDefaults;

  edt_Twitter_AccessToken.Text := '';
  edt_Twitter_AccessTokenSecret.Text := '';
  edt_Twitter_AuthVerifier.Text := '';
  edt_Twitter_RequestToken.Text := '';
  edt_Twitter_RequestTokenSecret.Text := '';
  edt_Twitter_AuthVerifier.Text := '';

  /// we need to transfer the data here manually
  OAuth1_Twitter.ConsumerKey := edt_Twitter_ConsumerKey.Text;
  OAuth1_Twitter.ConsumerSecret := edt_Twitter_ConsumerSecret.Text;

  OAuth1_Twitter.AccessToken := '';
  OAuth1_Twitter.AccessTokenSecret := '';
  OAuth1_Twitter.RequestToken := '';
  OAuth1_Twitter.RequestTokenSecret := '';
  OAuth1_Twitter.VerifierPIN := '';

  /// a client-id is required
  if (OAuth1_Twitter.ConsumerKey = '') then
  begin
    TaskMessageDlg('Error', 'A Consumer-ID ("client-id" or "app-id") is required.', mtError, [mbOK], 0);
    EXIT;
  end;

  /// step #1, get request-token
  RESTClient.BaseURL := OAuth1_Twitter.RequestTokenEndpoint;
  RESTClient.Authenticator := OAuth1_Twitter;

  RESTRequest.Method := TRESTRequestMethod.rmPOST;

  RESTRequest.Execute;

  if RESTResponse.GetSimpleValue('oauth_token', LToken) then
    OAuth1_Twitter.RequestToken := LToken;
  if RESTResponse.GetSimpleValue('oauth_token_secret', LToken) then
    OAuth1_Twitter.RequestTokenSecret := LToken;

  edt_Twitter_RequestToken.Text := OAuth1_Twitter.RequestToken;
  edt_Twitter_RequestTokenSecret.Text := OAuth1_Twitter.RequestTokenSecret;

  /// step #2: get the auth-verifier (PIN must be entered by the user!)
  LURL := OAuth1_Twitter.AuthenticationEndpoint;
  LURL := LURL + '?oauth_token=' + OAuth1_Twitter.RequestToken;

  wv := Tfrm_OAuthWebForm.Create(self);
  try
    wv.ShowModalWithURL(LURL);
  finally
    wv.Release;
  end;
end;

procedure Tfrm_Main.btn_Twitter_RequestAccessTokenClick(Sender: TObject);
var
  LToken: string;
begin
  ResetRESTComponentsToDefaults;

  /// grab the verifier from the edit-field
  OAuth1_Twitter.VerifierPIN := edt_Twitter_AuthVerifier.Text;

  /// here, we want to change the request-token and the verifier into an access-token
  if (OAuth1_Twitter.RequestToken = '') or (OAuth1_Twitter.VerifierPIN = '') then
  begin
    TaskMessageDlg('Error', 'Request-token and verifier are both required.', mtError, [mbOK], 0);
    EXIT;
  end;

  /// we want to request an access-token
  OAuth1_Twitter.AccessToken := '';
  OAuth1_Twitter.AccessTokenSecret := '';

  RESTClient.BaseURL := OAuth1_Twitter.AccessTokenEndpoint;
  RESTClient.Authenticator := OAuth1_Twitter;

  RESTRequest.Method := TRESTRequestMethod.rmPOST;
  RESTRequest.Params.AddItem('oauth_verifier', OAuth1_Twitter.VerifierPIN, TRESTRequestParameterKind.pkGETorPOST,
    [TRESTRequestParameterOption.poDoNotEncode]);

  RESTRequest.Execute;

  if RESTResponse.GetSimpleValue('oauth_token', LToken) then
    OAuth1_Twitter.AccessToken := LToken;
  if RESTResponse.GetSimpleValue('oauth_token_secret', LToken) then
    OAuth1_Twitter.AccessTokenSecret := LToken;

  /// now we should remove the request-token
  OAuth1_Twitter.RequestToken := '';
  OAuth1_Twitter.RequestTokenSecret := '';
  OAuth1_Twitter.VerifierPin := '';

  edt_Twitter_AccessToken.Text := OAuth1_Twitter.AccessToken;
  edt_Twitter_AccessTokenSecret.Text := OAuth1_Twitter.AccessTokenSecret;
  edt_Twitter_RequestToken.Text := OAuth1_Twitter.RequestToken;
  edt_Twitter_RequestTokenSecret.Text := OAuth1_Twitter.RequestTokenSecret;
end;

procedure Tfrm_Main.btn_GoogleTasks_FetchListsClick(Sender: TObject);
begin
  ResetRESTComponentsToDefaults;

  RESTClient.BaseURL := edt_GoogleTasks_BaseURL.Text;
  RESTClient.Authenticator := OAuth2_GoogleTasks;

  RESTRequest.Resource := edt_GoogleTasks_ResourceURI.Text;

  RESTRequest.Execute;
end;

procedure Tfrm_Main.btn_FitBit_RequestAccessTokenClick(Sender: TObject);
var
  LToken: string;
begin
  ResetRESTComponentsToDefaults;

  /// grab the verifier from the edit-field
  OAuth1_FitBit.VerifierPIN := edt_FitBit_AuthVerifier.Text;

  /// here, we want to change the request-token and the verifier into an access-token
  if (OAuth1_FitBit.RequestToken = '') or (OAuth1_FitBit.VerifierPIN = '') then
  begin
    TaskMessageDlg('Error', 'Request-token and verifier are both required.', mtError, [mbOK], 0);
    EXIT;
  end;

  /// we want to request an access-token using the RequestToken Signed with onsumerKey and RequestTokenSecret
  OAuth1_FitBit.AccessToken := OAuth1_FitBit.RequestToken;
  OAuth1_FitBit.AccessTokenSecret := OAuth1_FitBit.RequestTokenSecret;

  RESTClient.BaseURL := OAuth1_FitBit.AccessTokenEndpoint;
  RESTClient.Authenticator := OAuth1_FitBit;

  RESTRequest.Method := TRESTRequestMethod.rmPOST;
//  RESTRequest.Params.AddItem('oauth_verifier', OAuth1_FitBit.VerifierPIN, TRESTRequestParameterKind.pkGETorPOST,
//    [TRESTRequestParameterOption.poDoNotEncode]);

  RESTRequest.Execute;

  OAuth1_FitBit.AccessToken := ExtractTokenValue('oauth_token=', RESTResponse.Content);
  OAuth1_FitBit.AccessTokenSecret := ExtractTokenValue('oauth_token_secret=', RESTResponse.Content);

  /// now we should remove the request-token
  OAuth1_FitBit.RequestToken := '';
  OAuth1_FitBit.RequestTokenSecret := '';
  OAuth1_FitBit.VerifierPin := '';

  edt_FitBit_AccessToken.Text := OAuth1_FitBit.AccessToken;
  edt_FitBit_AccessTokenSecret.Text := OAuth1_FitBit.AccessTokenSecret;
  edt_FitBit_RequestToken.Text := OAuth1_FitBit.RequestToken;
  edt_FitBit_RequestTokenSecret.Text := OAuth1_FitBit.RequestTokenSecret;
end;

procedure Tfrm_Main.Button4Click(Sender: TObject);
begin
  ResetRESTComponentsToDefaults;

  RESTClient.BaseURL := edt_FitBit_BaseURL.Text;
  RESTClient.Authenticator := OAuth1_FitBit;

  RESTRequest.Resource := edt_FitBit_ResourceURI.Text;

  RESTRequest.Method := TRESTRequestMethod.rmGET;

  RESTRequest.Execute;
end;

procedure Tfrm_Main.ButtonTwineClick(Sender: TObject);
var
  LValues: TJSONArray;
  LJson: TJSONObject;
  LTempSensorName: string;
  LValue: TJsonValue;
  LTemp: double;
begin
  ClearMemos;
  RESTClient.BaseURL := edt_twine_url.Text;

  RESTRequest.Accept := '*/*';
  RESTRequest.Method := TRESTRequestMethod.rmGET;
  RESTRequest.Resource := edt_twine_resource_sensors.Text;
  RESTRequest.Params.AddItem('DeviceID', edt_twine_device.Text, TRESTRequestParameterKind.pkURLSEGMENT);
  RESTRequest.Params.AddItem('AccessKey', edt_twine_AccessKey.Text, TRESTRequestParameterKind.pkURLSEGMENT);
  RESTRequest.Execute;

  // Parse for sensor value
  LTempSensorName := edt_twine_device.Text + '01'; // Twine uses the device id in the sensore names.
  LJson := RESTRequest.Response.JSONValue as TJSONObject;
  LValues := LJson.Values['values'] as TJSONArray;
  for LValue in LValues do
  begin
    // the sensor values are not objects unfortunately, but arrays of strings
    if (LValue as TJSONArray).Items[0].Value = LTempSensorName then
    begin
      LTemp := StrToInt((LValue as TJSONArray).Items[1].Value) / 100;
      Lbl_Temperature.Caption := Format('%3.0f°F', [LTemp]);
      break;
    end;
  end;

  // Todo: check all sensor values to find the one that carries the temperature. Assuming id 0 might not be stable.

end;

procedure Tfrm_Main.ClearMemos;
begin
  memo_ResponseData.Clear;
end;

procedure Tfrm_Main.DataSetAdapterOnBeforeOpen(Sender: TObject);
begin
  ClientDataSet.CreateDataSet;
end;

procedure Tfrm_Main.edt_DropBox_FetchAuthCodeClick(Sender: TObject);
var
  LURL: string;
  wv: Tfrm_OAuthWebForm;
begin
  LURL := 'https://www.dropbox.com/1/oauth2/authorize';
  LURL := LURL + '?client_id=' + URIEncode(edt_DropBox_ClientID.Text);
  LURL := LURL + '&response_type=' + URIEncode('token');
  LURL := LURL + '&redirect_uri=' + URIEncode('{ANY HTTPS-SITE YOU OWN OR YOU TRUST}');

  wv := Tfrm_OAuthWebForm.Create(self);
  try
    wv.OnAfterRedirect := self.OAuth2_DropBox_AccessTokenRedirect;
    wv.OnBeforeRedirect := self.OAuth2_DropBox_AccessTokenRedirect;
    wv.ShowModalWithURL(LURL);

    if (StartsText('Success code', wv.LastTitle)) then
      edt_GoogleTasks_AuthCode.Text := Copy(wv.LastTitle, 14, Length(wv.LastTitle));
  finally
    wv.Release;
  end;
end;

procedure Tfrm_Main.edt_DropBox_FetchDataClick(Sender: TObject);
begin
  ResetRESTComponentsToDefaults;

  RESTClient.BaseURL := edt_DropBox_BaseURL.Text;
  RESTClient.Authenticator := OAuth2_Dropbox;

  OAuth2_Dropbox.AccessToken := edt_DropBox_AccessToken.Text;

  RESTRequest.Resource := edt_DropBox_ResourceURI.Text;

  RESTRequest.Params.AddItem('ROOT', 'dropbox', TRESTRequestParameterKind.pkURLSEGMENT);
  RESTRequest.Params.AddItem('PATH', '', TRESTRequestParameterKind.pkURLSEGMENT);

  RESTRequest.Execute;
end;

function Tfrm_Main.ExtractTokenValue(const AToken, AContent: string): string;
begin
  Result :='';
  if ContainsText(AContent, AToken) then
  begin
    Result := Copy(AContent, Pos(AToken, AContent) + Length(AToken), Length(AContent));
    if (Pos('&', Result) > 0) then
      Result := Copy(Result, 1, Pos('&', Result) - 1);
  end;
end;

procedure Tfrm_Main.FormCreate(Sender: TObject);
begin
  FCloseLevel := 0;
  FINIFilename := ChangeFileExt(Application.ExeName, '.ini');

  LoadConnectionData(FINIFilename);

  pc_Demos.ActivePage := ts_Twitter;
  pc_Data.ActivePage := ts_ResponseData;

  ClearMemos;

  memo_ResponseData.Font.Name := GetMonospaceFontName;
end;

procedure Tfrm_Main.FormKeyPress(Sender: TObject; var Key: Char);
begin
  /// pressing ESC twice will close the application
  if (Key = #27) then
  begin
    inc(FCloseLevel);
    if (FCloseLevel = 2) then
      Close;
  end
  else
    FCloseLevel := 0;
end;

procedure Tfrm_Main.OAuth2_Foursquare_AccessTokenRedirect(const AURL: string; var DoCloseWebView: boolean);
var
  LATPos: integer;
  LToken: string;
begin
  LATPos := Pos('access_token=', AURL);
  if (LATPos > 0) then
  begin
    LToken := Copy(AURL, LATPos + 13, Length(AURL));
    if (Pos('&', LToken) > 0) then
    begin
      LToken := Copy(LToken, 1, Pos('&', LToken) - 1);
    end;

    edt_Foursquare_AuthCode.Text := LToken;
    if (LToken <> '') then
      DoCloseWebView := TRUE;
  end;
end;

procedure Tfrm_Main.OAuth2_GoogleTasks_BrowserTitleChanged(const ATitle: string; var DoCloseWebView: boolean);
begin
  if (StartsText('Success code', ATitle)) then
  begin
    edt_GoogleTasks_AuthCode.Text := Copy(ATitle, 14, Length(ATitle));

    if (edt_GoogleTasks_AuthCode.Text <> '') then
      DoCloseWebView := TRUE;
  end;
end;

procedure Tfrm_Main.OAuth2_DropBox_AccessTokenRedirect(const AURL: string; var DoCloseWebView: boolean);
var
  LATPos: integer;
  LToken: string;
begin
  LATPos := Pos('access_token=', AURL);
  if (LATPos > 0) then
  begin
    LToken := Copy(AURL, LATPos + 13, Length(AURL));
    if (Pos('&', LToken) > 0) then
    begin
      LToken := Copy(LToken, 1, Pos('&', LToken) - 1);
    end;

    edt_DropBox_AccessToken.Text := LToken;
    if (LToken <> '') then
      DoCloseWebView := TRUE;

  end;
end;

procedure Tfrm_Main.OAuth2_Facebook_AccessTokenRedirect(const AURL: string; var DoCloseWebView: boolean);
var
  LATPos: integer;
  LToken: string;
begin
  LATPos := Pos('access_token=', AURL);
  if (LATPos > 0) then
  begin
    LToken := Copy(AURL, LATPos + 13, Length(AURL));
    if (Pos('&', LToken) > 0) then
    begin
      LToken := Copy(LToken, 1, Pos('&', LToken) - 1);
    end;

    edt_FaceBook_AccessToken.Text := LToken;
    if (LToken <> '') then
      DoCloseWebView := TRUE;
  end;
end;

function Tfrm_Main.GetMonospaceFontName: string;
begin
  if Screen.Fonts.IndexOf('Source Code Pro') > -1 then
    result := 'Source Code Pro'
  else if Screen.Fonts.IndexOf('Consolas') > -1 then
    result := 'Consolas'
  else if Screen.Fonts.IndexOf('Lucida Console') > -1 then
    result := 'Lucida Console'
  else if Screen.Fonts.IndexOf('Courier New') > -1 then
    result := 'Courier New'
  else if Screen.Fonts.IndexOf('Tahoma') > -1 then
    result := 'Tahoma'
  else
    result := 'MS Sans Serif';
end;

procedure Tfrm_Main.LoadConnectionData(const AFilename: string);
var
  LINIFile: TMemIniFile;
begin
  if not FileExists(AFilename) then
    EXIT;

  LINIFile := TMemIniFile.Create(AFilename);
  try
    /// load data for example "twitter"
    edt_Twitter_ConsumerKey.Text := LINIFile.ReadString('twitter', 'clientid', edt_Twitter_ConsumerKey.Text);
    edt_Twitter_ConsumerSecret.Text := LINIFile.ReadString('twitter', 'clientsecret', edt_Twitter_ConsumerSecret.Text);
    edt_Twitter_AccessToken.Text := LINIFile.ReadString('twitter', 'accesstoken', edt_Twitter_AccessToken.Text);
    edt_Twitter_AccessTokenSecret.Text := LINIFile.ReadString('twitter', 'accesstokensecret',
      edt_Twitter_AccessTokenSecret.Text);

    /// load data for example "googletasks"
    edt_GoogleTasks_ClientID.Text := LINIFile.ReadString('googletasks', 'clientid', edt_GoogleTasks_ClientID.Text);
    edt_GoogleTasks_ClientSecret.Text := LINIFile.ReadString('googletasks', 'clientsecret',
      edt_GoogleTasks_ClientSecret.Text);
    edt_GoogleTasks_AuthCode.Text := LINIFile.ReadString('googletasks', 'authcode', edt_GoogleTasks_AuthCode.Text);
    edt_GoogleTasks_AccessToken.Text := LINIFile.ReadString('googletasks', 'accesstoken',
      edt_GoogleTasks_AccessToken.Text);
    edt_GoogleTasks_RefreshToken.Text := LINIFile.ReadString('googletasks', 'refreshtoken',
      edt_GoogleTasks_RefreshToken.Text);

    /// load data for example "facebook"
    edt_Facebook_AppID.Text := LINIFile.ReadString('facebook', 'clientid', edt_Facebook_AppID.Text);
    edt_Facebook_AppSecret.Text := LINIFile.ReadString('facebook', 'clientsecret', edt_Facebook_AppSecret.Text);
    edt_FaceBook_AccessToken.Text := LINIFile.ReadString('facebook', 'accesstoken', edt_FaceBook_AccessToken.Text);

    /// load data for example "foursquare"
    edt_Foursquare_ClientID.Text := LINIFile.ReadString('foursquare', 'clientid', edt_Foursquare_ClientID.Text);
    edt_Foursquare_ClientSecret.Text := LINIFile.ReadString('foursquare', 'clientsecret',
      edt_Foursquare_ClientSecret.Text);
    edt_Foursquare_AuthCode.Text := LINIFile.ReadString('foursquare', 'authcode', edt_Foursquare_AuthCode.Text);
    edt_Foursquare_AccessToken.Text := LINIFile.ReadString('foursquare', 'accesstoken',
      edt_Foursquare_AccessToken.Text);

    /// load data for example "dropbox"
    edt_DropBox_ClientID.Text := LINIFile.ReadString('dropbox', 'clientid', edt_DropBox_ClientID.Text);
    edt_DropBox_ClientSecret.Text := LINIFile.ReadString('dropbox', 'clientsecret', edt_DropBox_ClientSecret.Text);
    edt_DropBox_AuthCode.Text := LINIFile.ReadString('dropbox', 'authcode', edt_DropBox_AuthCode.Text);
    edt_DropBox_AccessToken.Text := LINIFile.ReadString('dropbox', 'accesstoken', edt_DropBox_AccessToken.Text);

    LINIFile.UpdateFile;
  finally
    FreeAndNil(LINIFile);
  end;
end;

procedure Tfrm_Main.ResetRESTComponentsToDefaults;
begin
  /// reset all of the rest-components for a complete
  /// new request
  ///
  /// --> we do not clear the private data from the
  /// individual authenticators.
  ///
  RESTRequest.ResetToDefaults;
  RESTClient.ResetToDefaults;
  RESTResponse.ResetToDefaults;
  RESTResponseDataSetAdapter.ResetToDefaults;
end;

procedure Tfrm_Main.RESTRequestAfterExecute(Sender: TCustomRESTRequest);
begin
  ClearMemos;
  lbl_status.Caption := 'URI: ' + Sender.GetFullRequestURL + ' Execution time: ' +
    IntToStr(Sender.ExecutionPerformance.TotalExecutionTime) + 'ms';
  if assigned(RESTResponse.JSONValue) then
  begin
    memo_ResponseData.Lines.Text := TJson.Format(RESTResponse.JSONValue)
  end
  else
  begin
    memo_ResponseData.Lines.Add(RESTResponse.Content);
  end;
end;

procedure Tfrm_Main.RESTRequestHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  // show error
  memo_ResponseData.Lines.Add(Sender.Response.StatusText);
  memo_ResponseData.Lines.Add(Sender.Response.Content);
end;

procedure Tfrm_Main.RESTResponseDataSetAdapterBeforeOpenDataSet(Sender: TObject);
begin
  if ClientDataSet.FieldCount = 0 then
    ClientDataSet.CreateDataSet;
end;

end.
