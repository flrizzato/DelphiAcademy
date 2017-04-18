unit uDMHTTP;

interface

uses
  System.SysUtils, System.Classes, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TDMHTTP = class(TDataModule)
    NetHTTPRequest1: TNetHTTPRequest;
    NetHTTPClient1: TNetHTTPClient;
  private
    { Private declarations }
  public
    procedure GetResponse(aURL: string; memstr: TMemoryStream);
    function GetRespString(aURL: string): string;
  end;

var
  DMHTTP: TDMHTTP;

implementation

{$R *.dfm}

{ TDMHTTP }

procedure TDMHTTP.GetResponse(aURL: string; memstr: TMemoryStream);
begin
  if (aURL <> '') and (memstr <> nil) then
  begin
    NetHTTPRequest1.Get(aURL, memstr);
  end;
end;

function TDMHTTP.GetRespString(aURL: string): string;
var strstr: TStringStream;
begin
  strstr := TStringStream.Create;
  try
    GetResponse(aURL, strstr);
    Result := strstr.DataString;
  finally
    strstr.Free;
  end;
end;

end.
