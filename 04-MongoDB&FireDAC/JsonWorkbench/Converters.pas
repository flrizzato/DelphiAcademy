//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit Converters;

interface

uses System.SysUtils, System.Classes;

type
  TConverterOption = (ExtendedMode, Indented);
  TConverterOptions = set of TConverterOption;

  TConverters = class
  public
    // Json <-> Bson converter
    class function Bson2Json(const ABytes: TBytes; AOptions: TConverterOptions = []): string;
    class function Json2Bson(const AJson: string; AOptions: TConverterOptions = []): TBytes;
    class function BsonString2Json(const ABsonString: string; AOptions: TConverterOptions = []): string;
    class function Json2BsonString(const AJson: string; AOptions: TConverterOptions = []): string;

    // Byte <-> String convert utils
    class function Bytes2String(const ABytes: TBytes): string;
    class function String2Bytes(const AString: string): TBytes;

    class function JsonReformat(const AJson: string; Indented: Boolean): string;
    class function Json2DelphiCode(const AJson: string): string;
    class function Json2JsonWriterCode(const AJson: string; const AWriterName: string): string;
    class function Json2JsonBuilderCode(const AJson: string; const ABuilderName: string): string;
  end;

implementation

uses System.DateUtils, System.JSON.Readers, System.JSON.Types, System.JSON.Writers,  System.JSON.BSON, Writers;

{ TConverter }

class function TConverters.Bson2Json(const ABytes: TBytes; AOptions: TConverterOptions): string;
var
  JsonWriter: TJsonStringWriter;
  BsonReader: TBsonReader;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create(ABytes);
  BsonReader := TBsonReader.Create(Stream);
  JsonWriter := TJsonStringWriter.Create;

  if Indented in AOptions then
    JsonWriter.Formatting := TJsonFormatting.Indented;

  try
    JsonWriter.WriteToken(BsonReader);
    Result := JsonWriter.ToString;
  finally
    JsonWriter.Free;
    BsonReader.Free;
    Stream.Free;
  end;
end;

class function TConverters.Bytes2String(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(ABytes) to High(ABytes) do
  if I = 0 then
    Result := IntToHex(ABytes[I], 2)
  else
    Result := Result + '-' + IntToHex(ABytes[I], 2);
end;

class function TConverters.Json2Bson(const AJson: string; AOptions: TConverterOptions): TBytes;
var
  JsonReader: TJsonStringReader;
  BsonWriter: TBsonWriter;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create;
  BsonWriter := TBsonWriter.Create(Stream);
  JsonReader := TJsonStringReader.Create(AJson);
  try
    BsonWriter.WriteToken(JsonReader);
    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.Read(Result, Stream.Size);
  finally
    JsonReader.Free;
    BsonWriter.Free;
    Stream.Free;
  end;
end;

class function TConverters.Json2BsonString(const AJson: string; AOptions: TConverterOptions): string;
begin
  Result := Bytes2String(Json2Bson(AJson, AOptions));
end;

class function TConverters.JsonReformat(const AJson: string; Indented: Boolean): string;
var
  JsonWriter: TJsonStringWriter;
  JsonReader: TJsonStringReader;
begin
  JsonReader := TJsonStringReader.Create(AJson);
  JsonWriter := TJsonStringWriter.Create;

  if Indented then
    JsonWriter.Formatting := TJsonFormatting.Indented;

  try
    JsonWriter.WriteToken(JsonReader);
    Result := JsonWriter.ToString;
  finally
    JsonWriter.Free;
    JsonReader.Free;
  end;
end;

class function TConverters.Json2JsonBuilderCode(const AJson: string; const ABuilderName: string): string;
var
  JsonWriter: TJsonCodeWriter;
  JsonReader: TJsonStringReader;
begin
  JsonReader := TJsonStringReader.Create(AJson);
  JsonWriter := TJsonBuilderGenerator.Create(ABuilderName);
  try
    JsonWriter.WriteToken(JsonReader);
    Result := JsonWriter.ToString;
  finally
    JsonWriter.Free;
    JsonReader.Free;
  end;
end;

class function TConverters.Json2DelphiCode(const AJson: string): string;
var
  LJson: string;
begin
  LJson := JsonReformat(AJson, True);
  Result := ''
    + 'var' + SLineBreak
    + '  S: string;' + SLineBreak
    + 'begin' + SLineBreak
    + '  S := ''''' + SLineBreak
    + '  + '''
    + LJson.Replace(SLineBreak, ''' + SLineBreak' + SLineBreak + '  + ''')

    + ''';' + SLineBreak
    + 'end;' + SLineBreak;
end;

class function TConverters.Json2JsonWriterCode(const AJson: string; const AWriterName: string): string;
var
  JsonWriter: TJsonCodeWriter;
  JsonReader: TJsonStringReader;
begin
  JsonReader := TJsonStringReader.Create(AJson);
  JsonWriter := TJsonWriterGenerator.Create(AWriterName);
  try
    JsonWriter.WriteToken(JsonReader);
    Result := JsonWriter.ToString;
  finally
    JsonWriter.Free;
    JsonReader.Free;
  end;
end;

class function TConverters.BsonString2Json(const ABsonString: string; AOptions: TConverterOptions): string;
begin
  Result := Bson2Json(String2Bytes(ABsonString), AOptions)
end;

class function TConverters.String2Bytes(const AString: string): TBytes;
var
  CleanStr: string;
begin
  CleanStr := AString.Replace('-', '');
  SetLength(Result, Round(Length(CleanStr) / 2));
  HexToBin(PChar(CleanStr), 0, Result, 0, Length(Result));
end;


end.
