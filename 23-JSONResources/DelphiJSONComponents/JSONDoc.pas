unit JSONDoc;

// ***********************************************************************
//
//   JSON Document Component
//
//   pawel.glowacki@embarcadero.com
//
//   July 2010 - version 1.0
//   February 2016 - version 1.1
//
// ***********************************************************************

interface

uses
  System.Classes, System.JSON;

type
  TJSONDocument = class(TComponent)
  private
    FRootValue: TJSONValue;
    FJsonText: string;
    FOnChange: TNotifyEvent;
    procedure SetJsonText(const Value: string);
  protected
    procedure FreeRootValue;
    procedure DoOnChange; virtual;
    procedure ProcessJsonText;
  public
    class function StripNonJson(s: string): string; inline;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsActive: boolean;
    function EstimatedByteSize: integer;
    property RootValue: TJSONValue read FRootValue;
  published
    property JsonText: string read FJsonText write SetJsonText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  System.SysUtils, System.Character;

{ TJSONDocument }

constructor TJSONDocument.Create(AOwner: TComponent);
begin
  inherited;
  FRootValue := nil;
  FJsonText := '';
end;

destructor TJSONDocument.Destroy;
begin
  FreeRootValue;
  inherited;
end;

procedure TJSONDocument.FreeRootValue;
begin
  if Assigned(FRootValue) then
    FreeAndNil(FRootValue);
end;

procedure TJSONDocument.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

function TJSONDocument.EstimatedByteSize: integer;
begin
  if IsActive then
    Result := FRootValue.EstimatedByteSize
  else
    Result := 0;
end;

function TJSONDocument.IsActive: boolean;
begin
  Result := RootValue <> nil;
end;

procedure TJSONDocument.SetJsonText(const Value: string);
begin
  if FJsonText <> Value then
  begin
    FreeRootValue;
    FJsonText := Value;
    if FJsonText <> '' then
      ProcessJsonText;
    if not IsActive then
      FJsonText := '';
  end;
end;

procedure TJSONDocument.ProcessJsonText;
var s: string;
begin
  FreeRootValue;
  s := StripNonJson(JsonText);
  FRootValue := TJSONObject.ParseJSONValue(BytesOf(s),0);
  DoOnChange;
end;

class function TJSONDocument.StripNonJson(s: string): string;
var ch: char; inString: boolean;
begin
  Result := '';
  inString := false;
  for ch in s do
  begin
    if ch = '"' then
      inString := not inString;

    if ch.IsWhiteSpace and not inString then
      continue;

    Result := Result + ch;
  end;
end;

end.

