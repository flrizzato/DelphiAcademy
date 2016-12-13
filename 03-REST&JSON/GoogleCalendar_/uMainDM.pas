unit uMainDM;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, REST.Client, REST.Authenticator.OAuth,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Response.Adapter,
  Data.Bind.Components, Data.Bind.ObjectScope, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer;

type
  TMainDM = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1: TFDMemTable;
    OAuth2Authenticator1: TOAuth2Authenticator;
    FDMemTable1kind: TWideStringField;
    FDMemTable1etag: TWideStringField;
    FDMemTable1id: TWideStringField;
    FDMemTable1status: TWideStringField;
    FDMemTable1htmlLink: TWideStringField;
    FDMemTable1created: TWideStringField;
    FDMemTable1updated: TWideStringField;
    FDMemTable1summary: TWideStringField;
    FDMemTable1location: TWideStringField;
    FDMemTable1creator: TWideStringField;
    FDMemTable1creatoremail: TWideStringField;
    FDMemTable1creatordisplayName: TWideStringField;
    FDMemTable1organizer: TWideStringField;
    FDMemTable1organizeremail: TWideStringField;
    FDMemTable1organizerdisplayName: TWideStringField;
    FDMemTable1start: TWideStringField;
    FDMemTable1startdateTime: TWideStringField;
    FDMemTable1end: TWideStringField;
    FDMemTable1enddateTime: TWideStringField;
    FDMemTable1iCalUID: TWideStringField;
    FDMemTable1sequence: TWideStringField;
    FDMemTable1attendees: TWideStringField;
    FDMemTable1reminders: TWideStringField;
    FDMemTable1remindersuseDefault: TWideStringField;
    IdHTTPServer1: TIdHTTPServer;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function BuildLoginURL: string;
    function ExtractAuthCode(aURL: string): string;
    function RequestAuthToken(aAuthCode: string): string;
  end;

var
  MainDM: TMainDM;

implementation

{ %CLASSGROUP 'FMX.Controls.TControl' }

{$R *.dfm}
{ TDataModule1 }

uses REST.Utils, REST.Types;

function TMainDM.BuildLoginURL: string;
begin
  Result := OAuth2Authenticator1.AuthorizationEndpoint +
    '?client_id=' + URIEncode(OAuth2Authenticator1.ClientID) +
    '&response_type=' + URIEncode('code') +
    '&redirect_uri=' + URIEncode(OAuth2Authenticator1.RedirectionEndpoint) +
    '&scope=' + URIEncode(OAuth2Authenticator1.Scope);
end;

procedure TMainDM.DataModuleCreate(Sender: TObject);
var
  aParam: TRESTRequestParameter;
begin
  RESTClient1.Params.Clear;
  aParam := RESTClient1.Params.AddItem;
  aParam.name := 'timeMin';
  aParam.Value := FormatDateTime('yyyy-mm-dd', Date) + 'T00:00:00-23:59:59';

  IdHTTPServer1.Active := True;
end;

function TMainDM.ExtractAuthCode(aURL: string): string;
var
  tokenPos: integer;
  token: String;
begin
  tokenPos := Pos('code=', aURL);
  if (tokenPos > 0) then
  begin
    token := Copy(aURL, tokenPos + 5, aURL.Length);
    if (Pos('&', token) > 0) then
    begin
      Delete(token, Pos('&', token), MaxInt);
    end;
  end;
  Result := token;
end;

function TMainDM.RequestAuthToken(aAuthCode: string): string;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LValue: string;
begin
  LClient := TRESTClient.Create(self);
  LRequest := TRESTRequest.Create(self);
  LRequest.Client := LClient;
  LRequest.Method := TRESTRequestMethod.rmPOST;

  TRY
    LClient.BaseURL := OAuth2Authenticator1.AccessTokenEndpoint;
    LRequest.AddParameter('code', aAuthCode);
    LRequest.AddParameter('client_id', OAuth2Authenticator1.ClientID);
    LRequest.AddParameter('client_secret', OAuth2Authenticator1.ClientSecret);
    LRequest.AddParameter('grant_type', 'authorization_code');
    LRequest.AddParameter('redirect_uri', OAuth2Authenticator1.RedirectionEndpoint);

    LRequest.Execute;
    if (LRequest.Response.StatusCode = 200) then
    begin
      Result := '';
      if LRequest.Response.GetSimpleValue('access_token', LValue) then
        Result := LValue;
    end;

  FINALLY
    FreeAndNIL(LRequest);
    FreeAndNIL(LClient);
  END;
end;

end.
