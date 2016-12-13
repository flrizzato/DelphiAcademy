unit ULaunchWebbrowser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
{$IFDEF IOS}
  iOSapi.Foundation, FMX.Helpers.iOS;
{$ENDIF IOS}
{$IFDEF WIN32}
  Winapi.ShellAPI, Winapi.Windows;
{$ENDIF WIN32}
{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  FMX.Helpers.Android, Androidapi.JNI.Net;
{$ENDIF ANDROID}

procedure LaunchWeb(Url:String);


implementation



procedure LaunchWeb(Url:String);
{$IFDEF ANDROID}
var
  Intent: JIntent;
{$ENDIF ANDROID}
begin
{$IFDEF ANDROID}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(URL));
  SharedActivity.startActivity(Intent);
{$ENDIF ANDROID}
{$IFDEF WIN32}
  ShellExecute(0, 'OPEN', PWideChar(URL), nil, nil, SW_SHOWNORMAL);
{$ENDIF WIN32}
{$IFDEF IOS}
  SharedApplication.OpenURL(StrToNSUrl(Url));
{$ENDIF IOS}
end;


end.
