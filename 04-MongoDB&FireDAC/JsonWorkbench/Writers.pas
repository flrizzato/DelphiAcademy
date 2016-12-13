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
unit Writers;

interface

uses System.SysUtils, System.Classes, System.JSON.Writers, System.JSON.Readers, System.JSON.Types;

type
  TJsonStringWriter = class(TJsonTextWriter)
  private
    FStrinBuilder: TStringBuilder;
    FStringWriter: TStringWriter;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: string;
  end;

  TJsonStringReader = class(TJsonTextReader)
  private
    FStrinReader: TStringReader;
  public
    constructor Create(const AJson: string);
    destructor Destroy; override;
  end;

  TJsonCodeWriter = class(TJsonWriter)
  private
    FWriter: TStringWriter;
    FBuilder: TStringBuilder;
    FIndentation: Integer;
    FIndentationBase: Integer;
    FIndentChar: Char;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Rewind; override;
    function ToString: string;
    property IndentationBase: Integer read FIndentationBase write FIndentationBase;
    property Indentation: Integer read FIndentation write FIndentation;
    property IndentChar: Char read FIndentChar write FIndentChar;
  end;

  TJsonBuilderGenerator = class(TJsonCodeWriter)
  private
    FPropertyName: string;
    FBuilderName: string;
    FPreviousState: TJsonWriter.TState;
    procedure WriteIndent;
    procedure WriteValueAsFunction(const AValue: string; AsString: Boolean = False); overload;
    procedure WriteKeywordAsFunction(const AKeyword: string); overload;
    procedure WriteFunctionCall(const AFunction: string); overload;
    procedure WriteSymbolAsFunction(const ASymbol: string); overload;
  protected
    procedure WriteEnd(const Token: TJsonToken); override;
  public
    constructor Create(const ABuilderName: string);
    procedure WriteStartObject; override;
    procedure WriteStartArray; override;
    procedure WriteStartConstructor(const Name: string); override;
    procedure WritePropertyName(const Name: string); override;
    procedure WriteNull; override;
    procedure WriteRaw(const Json: string); override;
    procedure WriteRawValue(const Json: string); override;
    procedure WriteUndefined; override;
    procedure WriteValue(const Value: string); override;
    procedure WriteValue(Value: Integer); override;
    procedure WriteValue(Value: UInt32); override;
    procedure WriteValue(Value: Int64); override;
    procedure WriteValue(Value: UInt64); override;
    procedure WriteValue(Value: Single); override;
    procedure WriteValue(Value: Double); override;
    procedure WriteValue(Value: Extended); override;
    procedure WriteValue(Value: Boolean); override;
    procedure WriteValue(Value: Char); override;
    procedure WriteValue(Value: Byte); override;
    procedure WriteValue(Value: TDateTime); override;
    procedure WriteValue(const Value: TGUID); override;
    procedure WriteValue(const Value: TBytes; BinaryType: TJsonBinaryType = TJsonBinaryType.Generic); override;
    procedure WriteValue(const Value: TJsonOid); override;
    procedure WriteValue(const Value: TJsonRegEx); override;
    procedure WriteValue(const Value: TJsonDBRef); override;
    procedure WriteValue(const Value: TJsonCodeWScope); override;
    procedure WriteMinKey; override;
    procedure WriteMaxKey; override;
    procedure OnBeforeWriteToken(TokenBeginWritten: TJsonToken); override;
  end;

  TJsonWriterGenerator = class(TJsonCodeWriter)
  private
    FWriterName: string;
    procedure WriteValueAsFunctionCall(const AValue: string; AsString: Boolean = False);
    procedure WriteTokenAsFunctionCall(const ATokenName: string);
  protected
    procedure WriteEnd(const Token: TJsonToken); override;
  public
    constructor Create(const AWriterName: string);
    procedure WriteStartObject; override;
    procedure WriteStartArray; override;
    procedure WriteStartConstructor(const Name: string); override;
    procedure WritePropertyName(const Name: string); override;
    procedure WriteNull; override;
    procedure WriteRaw(const Json: string); override;
    procedure WriteRawValue(const Json: string); override;
    procedure WriteUndefined; override;
    procedure WriteValue(const Value: string); override;
    procedure WriteValue(Value: Integer); override;
    procedure WriteValue(Value: UInt32); override;
    procedure WriteValue(Value: Int64); override;
    procedure WriteValue(Value: UInt64); override;
    procedure WriteValue(Value: Single); override;
    procedure WriteValue(Value: Double); override;
    procedure WriteValue(Value: Extended); override;
    procedure WriteValue(Value: Boolean); override;
    procedure WriteValue(Value: Char); override;
    procedure WriteValue(Value: Byte); override;
    procedure WriteValue(Value: TDateTime); override;
    procedure WriteValue(const Value: TGUID); override;
    procedure WriteValue(const Value: TBytes; BinaryType: TJsonBinaryType = TJsonBinaryType.Generic); override;
    procedure WriteValue(const Value: TJsonOid); override;
    procedure WriteValue(const Value: TJsonRegEx); override;
    procedure WriteValue(const Value: TJsonDBRef); override;
    procedure WriteValue(const Value: TJsonCodeWScope); override;
    procedure WriteMinKey; override;
    procedure WriteMaxKey; override;
    procedure OnBeforeWriteToken(TokenBeginWritten: TJsonToken); override;
  end;


implementation

uses System.Math;

{ TJsonStringWriter }

constructor TJsonStringWriter.Create;
begin
  FStrinBuilder := TStringBuilder.Create;
  FStringWriter := TStringWriter.Create(FStrinBuilder);
  inherited Create(FStringWriter);
end;

destructor TJsonStringWriter.Destroy;
begin
  FStringWriter.Free;
  FStrinBuilder.Free;
  inherited Destroy;
end;

function TJsonStringWriter.ToString: string;
begin
  Result := FStrinBuilder.ToString;
end;

{ TJsonStringReader }

constructor TJsonStringReader.Create(const AJson: string);
begin
  FStrinReader := TStringReader.Create(AJson);
  inherited Create(FStrinReader);
end;

destructor TJsonStringReader.Destroy;
begin
  FStrinReader.Free;
  inherited Destroy;
end;

{ TJsonWriterGenerator }

constructor TJsonBuilderGenerator.Create(const ABuilderName: string);
begin
  inherited Create;
  FBuilderName := ABuilderName;
end;

procedure TJsonBuilderGenerator.OnBeforeWriteToken(TokenBeginWritten: TJsonToken);
begin
  FPreviousState := FCurrentState;

 if FCurrentState = TState.Start then
 begin
   FWriter.Write(FBuilderName);
   WriteIndent;
 end;

 if TokenBeginWritten = TJsonToken.PropertyName then
   Exit;

  case FCurrentState of
    TState.Property,
    TState.Array,
    TState.ArrayStart,
    TState.ObjectStart,
    TState.Object,
    TState.Constructor,
    TState.ConstructorStart:
      WriteIndent;
    TState.Start:
      if TokenBeginWritten = TJsonToken.PropertyName then
        WriteIndent;
  end;
end;

procedure TJsonBuilderGenerator.WriteSymbolAsFunction(const ASymbol: string);
begin
  WriteKeywordAsFunction('Add' + ASymbol);
end;

procedure TJsonBuilderGenerator.WriteValueAsFunction(const AValue: string; AsString: Boolean);
var
  LValue: string;
begin
  if AsString then
    LValue := '''' + AValue + ''''
  else
    LValue := AValue;
  if FPreviousState = TJsonWriter.TState.Property then
    WriteFunctionCall('Add(''' + FPropertyName + ''', ' + LValue + ')')
  else
    WriteFunctionCall('Add(' + LValue + ')')
end;

procedure TJsonBuilderGenerator.WriteEnd(const Token: TJsonToken);
begin
  case Token of
    TJsonToken.EndObject:
      WriteFunctionCall('EndObject');
    TJsonToken.EndArray:
      WriteFunctionCall('EndArray');
    TJsonToken.EndConstructor:
      // not implemented in builders
    ;
  end;
end;

procedure TJsonBuilderGenerator.WriteFunctionCall(const AFunction: string);
begin
  FWriter.Write('.' + AFunction);
end;

procedure TJsonBuilderGenerator.WriteIndent;
var
  CurrentIndentCount, WriteCount: Integer;
begin
  FWriter.WriteLine;
  CurrentIndentCount := Top * FIndentation + FIndentationBase;
  while CurrentIndentCount > 0 do
  begin
    WriteCount := Min(CurrentIndentCount, 10);
    FWriter.Write(string.Create(FIndentChar, WriteCount));
    Dec(CurrentIndentCount, WriteCount);
  end;
end;

procedure TJsonBuilderGenerator.WriteKeywordAsFunction(const AKeyword: string);
begin
  if FPreviousState = TJsonWriter.TState.Property then
    WriteFunctionCall(AKeyword + '(''' + FPropertyName + ''')')
  else
    WriteFunctionCall(AKeyword)
end;

procedure TJsonBuilderGenerator.WriteMaxKey;
begin
  inherited;
  WriteSymbolAsFunction('MaxKey');
end;

procedure TJsonBuilderGenerator.WriteMinKey;
begin
  inherited;
  WriteSymbolAsFunction('MinKey');
end;

procedure TJsonBuilderGenerator.WriteNull;
begin
  inherited;
  WriteSymbolAsFunction('Null');
end;

procedure TJsonBuilderGenerator.WritePropertyName(const Name: string);
begin
  inherited;
  FPropertyName := Name;
end;

procedure TJsonBuilderGenerator.WriteRaw(const Json: string);
begin
  inherited;
  // not implemented in builders
end;

procedure TJsonBuilderGenerator.WriteRawValue(const Json: string);
begin
  inherited;
  // not implemented in builders
end;

procedure TJsonBuilderGenerator.WriteStartArray;
begin
  inherited;
  WriteKeywordAsFunction('BeginArray');
end;

procedure TJsonBuilderGenerator.WriteStartConstructor(const Name: string);
begin
  inherited;
  // not implemented in builders
end;

procedure TJsonBuilderGenerator.WriteStartObject;
begin
  inherited;
  WriteKeywordAsFunction('BeginObject');
end;

procedure TJsonBuilderGenerator.WriteUndefined;
begin
  inherited;
  WriteSymbolAsFunction('Undefined');
end;

procedure TJsonBuilderGenerator.WriteValue(Value: Int64);
begin
  inherited;
  WriteValueAsFunction(IntToStr(Value));
end;

procedure TJsonBuilderGenerator.WriteValue(Value: UInt64);
begin
  inherited;
  WriteValueAsFunction(UIntToStr(Value));
end;

procedure TJsonBuilderGenerator.WriteValue(Value: Single);
begin
  inherited;
  WriteValueAsFunction(FloatToStr(Value, JSONFormatSettings));
end;

procedure TJsonBuilderGenerator.WriteValue(const Value: string);
begin
  inherited;
  WriteValueAsFunction(Value, True);
end;

procedure TJsonBuilderGenerator.WriteValue(Value: Integer);
begin
  inherited;
  WriteValueAsFunction(IntToStr(Value));
end;

procedure TJsonBuilderGenerator.WriteValue(Value: UInt32);
begin
  inherited;
  WriteValueAsFunction(UIntToStr(Value));
end;

procedure TJsonBuilderGenerator.WriteValue(Value: Double);
begin
  inherited;
  WriteValueAsFunction(FloatToStr(Value, JSONFormatSettings));
end;

procedure TJsonBuilderGenerator.WriteValue(const Value: TJsonOid);
begin
  raise ENotImplemented.Create('TJsonOid overload');
end;

procedure TJsonBuilderGenerator.WriteValue(const Value: TBytes; BinaryType: TJsonBinaryType);
begin
  raise ENotImplemented.Create('TJsonRegEx overload');
end;

procedure TJsonBuilderGenerator.WriteValue(const Value: TJsonRegEx);
begin
  raise ENotImplemented.Create('TJsonRegEx overload');
end;

procedure TJsonBuilderGenerator.WriteValue(const Value: TJsonCodeWScope);
begin
  raise ENotImplemented.Create('TJsonCodeWScope overload');
end;

procedure TJsonBuilderGenerator.WriteValue(const Value: TJsonDBRef);
begin
  raise ENotImplemented.Create('TJsonDBRef overload');
end;

procedure TJsonBuilderGenerator.WriteValue(const Value: TGUID);
begin
  //TODO
end;

procedure TJsonBuilderGenerator.WriteValue(Value: Boolean);
begin
  inherited;
  WriteValueAsFunction(BoolToStr(Value, True));
end;

procedure TJsonBuilderGenerator.WriteValue(Value: Extended);
begin
  inherited;
  WriteValueAsFunction(FloatToStr(Value, JSONFormatSettings));
end;

procedure TJsonBuilderGenerator.WriteValue(Value: Char);
begin
  inherited;
  WriteValueAsFunction(Value, True);
end;

procedure TJsonBuilderGenerator.WriteValue(Value: TDateTime);
begin
  //TODO
end;

procedure TJsonBuilderGenerator.WriteValue(Value: Byte);
begin
  WriteValueAsFunction(Value.ToString);
end;

{ TJsonWriterGenerator }

constructor TJsonWriterGenerator.Create(const AWriterName: string);
begin
  inherited Create;
  FWriterName := AWriterName;
end;

procedure TJsonWriterGenerator.OnBeforeWriteToken(TokenBeginWritten: TJsonToken);
begin
  FWriter.WriteLine;
end;

procedure TJsonWriterGenerator.WriteEnd(const Token: TJsonToken);
begin
  case Token of
    TJsonToken.EndObject:
      WriteTokenAsFunctionCall('WriteEndObject');
    TJsonToken.EndArray:
      WriteTokenAsFunctionCall('WriteEndArray');
    TJsonToken.EndConstructor:
      WriteTokenAsFunctionCall('WriteEndConstructor');
  end;
end;

procedure TJsonWriterGenerator.WriteTokenAsFunctionCall(const ATokenName: string);
begin
  FWriter.Write(FWriterName + '.' + ATokenName + ';');
end;

procedure TJsonWriterGenerator.WriteMaxKey;
begin
  inherited;

end;

procedure TJsonWriterGenerator.WriteMinKey;
begin
  inherited;

end;

procedure TJsonWriterGenerator.WriteNull;
begin
  inherited;
  WriteTokenAsFunctionCall('Null');
end;

procedure TJsonWriterGenerator.WritePropertyName(const Name: string);
begin
  inherited;
  FWriter.Write(FWriterName + '.PropertyName(''' + Name + ''');');
end;

procedure TJsonWriterGenerator.WriteRaw(const Json: string);
begin
  inherited;

end;

procedure TJsonWriterGenerator.WriteRawValue(const Json: string);
begin
  inherited;

end;

procedure TJsonWriterGenerator.WriteStartArray;
begin
  inherited;
  WriteTokenAsFunctionCall('StartArray');
end;

procedure TJsonWriterGenerator.WriteStartConstructor(const Name: string);
begin
  inherited;
  WriteValueAsFunctionCall(Name, True);
end;

procedure TJsonWriterGenerator.WriteStartObject;
begin
  inherited;
  WriteTokenAsFunctionCall('StartObject');
end;

procedure TJsonWriterGenerator.WriteUndefined;
begin
  inherited;
  WriteTokenAsFunctionCall('Undefined');
end;

procedure TJsonWriterGenerator.WriteValue(Value: UInt64);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString);
end;

procedure TJsonWriterGenerator.WriteValue(Value: Single);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString);
end;

procedure TJsonWriterGenerator.WriteValue(Value: Double);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString);
end;

procedure TJsonWriterGenerator.WriteValue(Value: Int64);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString);
end;

procedure TJsonWriterGenerator.WriteValue(const Value: string);
begin
  inherited;
  WriteValueAsFunctionCall(Value, True);
end;

procedure TJsonWriterGenerator.WriteValue(Value: Integer);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString);
end;

procedure TJsonWriterGenerator.WriteValue(Value: UInt32);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString);
end;

procedure TJsonWriterGenerator.WriteValue(const Value: TJsonOid);
begin
  raise ENotImplemented.Create('TJsonOid overload');
end;

procedure TJsonWriterGenerator.WriteValue(const Value: TBytes; BinaryType: TJsonBinaryType);
begin
  raise ENotImplemented.Create('TBytes overload');
end;

procedure TJsonWriterGenerator.WriteValue(const Value: TJsonRegEx);
begin
  raise ENotImplemented.Create('TJsonRegEx overload');
end;

procedure TJsonWriterGenerator.WriteValue(const Value: TJsonCodeWScope);
begin
  raise ENotImplemented.Create('TJsonCodeWScope overload');
end;

procedure TJsonWriterGenerator.WriteValue(const Value: TJsonDBRef);
begin
  raise ENotImplemented.Create('TJsonDBRef overload');
end;

procedure TJsonWriterGenerator.WriteValue(const Value: TGUID);
begin
  raise ENotImplemented.Create('TGUID overload');
end;

procedure TJsonWriterGenerator.WriteValue(Value: Boolean);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString(True));
end;

procedure TJsonWriterGenerator.WriteValue(Value: Extended);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString);
end;

procedure TJsonWriterGenerator.WriteValue(Value: Char);
begin
  inherited;
  WriteValueAsFunctionCall(Value, True);
end;

procedure TJsonWriterGenerator.WriteValue(Value: TDateTime);
begin

end;

procedure TJsonWriterGenerator.WriteValue(Value: Byte);
begin
  inherited;
  WriteValueAsFunctionCall(Value.ToString);
end;

procedure TJsonWriterGenerator.WriteValueAsFunctionCall(const AValue: string; AsString: Boolean);
var
  LValue: string;
begin
  if AsString then
    LValue := '''' + AValue + ''''
  else
    LValue := AValue;
  FWriter.Write(FWriterName + '.WriteValue(' + LValue + ');');
end;

{ TJsonCodeWriter }

constructor TJsonCodeWriter.Create;
begin
  inherited Create;
  FIndentation := 2;
  FIndentChar := ' ';
  FBuilder := TStringBuilder.Create;
  FWriter := TStringWriter.Create(FBuilder);
end;

destructor TJsonCodeWriter.Destroy;
begin
  FWriter.Free;
  FBuilder.Free;
  inherited Destroy;
end;

procedure TJsonCodeWriter.Rewind;
begin
  inherited Rewind;
  FBuilder.Clear;
end;

function TJsonCodeWriter.ToString: string;
begin
  Result := FBuilder.ToString;
end;

end.
