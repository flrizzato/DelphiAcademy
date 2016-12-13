{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.base64coder;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type
// ===== Forward declarations =====

  JBase64Coder = interface;//biz.source_code.base64Coder.Base64Coder

// ===== Interface declarations =====

  JBase64CoderClass = interface(JObjectClass)
    ['{C89D9341-2006-4A7A-A00C-CEA03A90A317}']
    {class} function encode(P1: TJavaArray<Byte>): TJavaArray<Char>; cdecl;
    {class} function encodeString(P1: JString): JString; cdecl;
  end;

  [JavaSignature('biz/source_code/base64Coder/Base64Coder')]
  JBase64Coder = interface(JObject)
    ['{867ED649-EDE1-4C98-9E76-A8660B40D9CD}']
  end;
  TJBase64Coder = class(TJavaGenericImport<JBase64CoderClass, JBase64Coder>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.base64coder.JBase64Coder', TypeInfo(Androidapi.JNI.base64coder.JBase64Coder));
end;

initialization
  RegisterTypes;
end.


