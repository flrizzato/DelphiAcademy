{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2016-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit EMSManagementSetUp.QueryU;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils, System.Generics.Collections, System.JSON, System.Classes, System.Rtti;

type

  TJSONOpNames = record
  public const
    OpLike = '$like';
    OpNlike = '$nlike';
    OpOr = '$or';
    OpAnd = '$and';
    OpEq = '$e';
    OpNeq = '$ne';
    OpGt = '$gt';
    OpGte = '$gte';
    OpLt = '$lt';
    OpLte = '$lte';
    OpIn = '$in';
    OpNin = '$nin';
    OpExists = '$exists';
  end;

  TFilterItem = class  abstract
  public
    function GetAttributes: TArray<string>; virtual;
  end;

  TFilters = class;

  TFilterItemGroup = class(TFilterItem)
  private
    FFilters: TFilters;
  public
    constructor Create;
    destructor Destroy; override;
    property Filters: TFilters read FFilters;
  end;

  TFilters = class
  private
    FList: TList<TFilterItem>;
    function GetCount: Integer;
    function GetItem(I: Integer): TFilterItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AFilter: TFilterItem);
    procedure Remove(const AFilter: TFilterItem);
    procedure Delete(I: Integer);
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[I: Integer]: TFilterItem read GetItem;
  end;

  TQuery = class
  public type
    TDirection = (Ascending, Descending);

    TOrderByItem = TPair<string, TDirection>;

    TOrderBy = class
    private
      FList: TList<TOrderByItem>;
      function GetCount: Integer;
      function GetItem(I: Integer): TOrderByItem;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(const AFieldName: string; ADirection: TDirection);
      procedure Remove(const AItemIndex: Integer);
      property Count: Integer read GetCount;
      procedure Clear;
      property Items[I: Integer]: TOrderByItem read GetItem;
    end;
  private
    FFilters: TFilters;
    FOrderBy: TOrderBy;
    FLimit: Integer;
    FSkip: Integer;
    function GetHasFilters: Boolean;
    function GetHasMultipleFilters: Boolean;
    function GetHasLimit: Boolean;
    function GetHasOrderBy: Boolean;
    function GetHasSkip: Boolean;
    function GetFilters: TFilters;
    function GetOrderBy: TOrderBy;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property HasFilters: Boolean read GetHasFilters;
    property HasMultipleFilters: Boolean read GetHasMultipleFilters;
    property HasOrderBy: Boolean read GetHasOrderBy;
    property HasLimit: Boolean read GetHasLimit;
    property HasSkip: Boolean read GetHasSkip;
    property Filters: TFilters read GetFilters;
    property OrderBy: TOrderBy read GetOrderBy;
    property Limit: Integer read FLimit write FLimit;
    property Skip: Integer read FSkip write FSkip;
  end;

  TQueryWriter = class abstract
  public type
    TParameter = TPair<string, TValue>;
    TParameters = TArray<TParameter>;
  private
    FQuery: TQuery;
  protected
    property Query: TQuery read FQuery;
  public
    constructor Create(const AQuery: TQuery);
    procedure Validate; virtual; abstract;
    procedure Write(const AStrings: TStrings; out AParameters: TParameters); overload;
    procedure Write(const AStrings: TStringWriter; out AParameters: TParameters); overload; virtual; abstract;
  end;

  EQueryException = class(Exception);

  TCommonFilterItem = class(TFilterItem)
  private
    FAttribute: string;
  public
    property Attribute: string read FAttribute write FAttribute;
    function GetAttributes: TArray<string>; override;
  end;

  TSimpleFilter = class(TCommonFilterItem)
  public type
    TOperator = (lt, lte, gt, gte, eq, neq, like, nlike);
  private
    FOp: TOperator;
    FValue: TValue;
  public
    property Op: TOperator read FOp write FOp;
    property Value: TValue read FValue write FValue;
  end;

  TExistsFilter = class(TCommonFilterItem)
  private
    FExists: Boolean;
  public
    property Exists: Boolean read FExists write FExists;
  end;

  TInFilter = class(TCommonFilterItem)
  public type
    TOperator = (inop, ninop);
  private
    FOp: TOperator;
    FValues: TArray<TValue>;
  public
    property Op: TOperator read FOp write FOp;
    property Values: TArray<TValue> read FValues write FValues;
  end;

  TRangeFilter = class(TCommonFilterItem)
  public type
    TOperator = (lt, lte, gt, gte);
  private
    FOp1: TOperator;
    FValue1: TValue;
    FOp2: TOperator;
    FValue2: TValue;
  public
    property Op1: TOperator read FOp1 write FOp1;
    property Op2: TOperator read FOp2 write FOp2;
    property Value1: TValue read FValue1 write FValue1;
    property Value2: TValue read FValue2 write FValue2;
  end;

  TOrFilter = class(TFilterItemGroup)
  end;

  TAndFilter = class(TFilterItemGroup)
  end;

  TQueryToJSON = class
  private
    class procedure WriteFilter(const AFilter: TFilterItem; const AJSON: TJSONObject); static;
    class procedure WriteExistsFilter(const AFilter: TExistsFilter;
      const AJSON: TJSONObject); static;
    class procedure WriteOrFilter(const AFilter: TOrFilter;
      const AJSON: TJSONObject); static;
    class procedure WriteAndFilter(const AFilter: TAndFilter;
      const AJSON: TJSONObject); static;
    class procedure WriteRangeFilter(const AFilter: TRangeFilter;
      const AJSON: TJSONObject); static;
    class procedure WriteSimpleFilter(const AFilter: TSimpleFilter;
      const AJSON: TJSONObject); static;
    class procedure WriteInFilter(const AFilter: TInFilter;
      const AJSON: TJSONObject); static;
    class function ValueToJSON(const AValue: TValue): TJSONValue; static;
  public
    class procedure WriteFilters(const AFilters: TFilters; const AJSON: TJSONObject); static;
  end;

  TJSONFilterParser = class
  protected const
    ISO8601_datetime = 'yyyy/MM/dd hh:nn:ss';
    ISO8601_date = 'yyyy/MM/dd';
    USDate = 'MM/dd/yyyy';
  private
    class var FUtcOffset: Integer;
  public
    // Examples
    // {"score":{"$gte":1000,"$lte":3000}}
    // {"score":{"$in":[1,3,5,7,9]}}
    // {"name": {"$nin":["Jonathan Walsh","Dario Wunsch","Shawn Simon"]}}
    // {"playerName":"Sean Plott"}
    // {"playerName":{"$ne", "Sean Plott"}}
    // {"score":{"$exists":true}}
    // {"$or":[{"wins":{"$gt":150}},{"wins":{"$lt":5}}]}
    // {"$or":[{"wins":{"$gt":150}},{"playerName":"Sean Plott","cheatMode":false}]}
    // {"playerName":"Sean Plott","cheatMode":false}
    // {"$and":[{"username":"thomasm"},{"lname":"Blas"}]}
    // Internal methods
    class procedure Parse(const AJSONObject: TJSONObject; const AFilters: TFilters; const AUtcOffset: Integer); static;
  end;

  EFilterParserException = class(Exception);

implementation

uses
  System.StrUtils, System.DateUtils, System.TimeSpan;

{ TFilters }

procedure TFilters.Add(const AFilter: TFilterItem);
begin
  FList.Add(AFilter);
end;

procedure TFilters.Clear;
begin
  FList.Clear;
end;

constructor TFilters.Create;
begin
  FList := TObjectList<TFilterItem>.Create;  // Owns objects
end;

procedure TFilters.Remove(const AFilter: TFilterItem);
begin
  FList.Remove(AFilter);
end;

procedure TFilters.Delete(I: Integer);
begin
  FList.Delete(I);
end;

destructor TFilters.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFilters.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFilters.GetItem(I: Integer): TFilterItem;
begin
  Result := FList[I];
end;

{ TQuery.TOrderBy }

procedure TQuery.TOrderBy.Add(const AFieldName: string; ADirection: TDirection);
begin
  Assert(AFieldName <> '');
  if AFieldName <> '' then
    FList.Add(TOrderByItem.Create(AFieldName, ADirection));
end;

procedure TQuery.TOrderBy.Remove(const AItemIndex: Integer);
begin
  FList.delete(AItemIndex);
end;

procedure TQuery.TOrderBy.Clear;
begin
  FList.Clear;
end;

constructor TQuery.TOrderBy.Create;
begin
  FList := TList<TOrderByItem>.Create;
end;

destructor TQuery.TOrderBy.Destroy;
begin
  FList.Free;
  inherited;
end;

function TQuery.TOrderBy.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TQuery.TOrderBy.GetItem(I: Integer): TOrderByItem;
begin
  Result := FList[I];
end;

{ TQuery }

procedure TQuery.Clear;
begin
  FLimit := 0;
  FSkip := 0;
  FreeAndNil(FOrderBy);
  FreeAndNil(FFilters);
end;

constructor TQuery.Create;
begin
  //
end;

destructor TQuery.Destroy;
begin
  FFilters.Free;
  FOrderBy.Free;
  inherited;
end;

function TQuery.GetFilters: TFilters;
begin
  if FFilters = nil then
    FFilters := TFilters.Create;
  Result := FFilters;
end;

function TQuery.GetHasFilters: Boolean;
begin
  Result := (FFilters <> nil) and (FFilters.Count > 0);
end;

function TQuery.GetHasMultipleFilters: Boolean;
begin
  Result := (FFilters <> nil) and (FFilters.Count > 1);
end;

function TQuery.GetHasLimit: Boolean;
begin
  Result := FLimit <> 0;
end;

function TQuery.GetHasOrderBy: Boolean;
begin
  Result := (FOrderBy <> nil) and (FOrderBy.Count > 0);
end;

function TQuery.GetHasSkip: Boolean;
begin
  Result := FSkip <> 0;
end;

function TQuery.GetOrderBy: TOrderBy;
begin
  if FOrderBy = nil then
    FOrderBy := TOrderBy.Create;
  Result := FOrderBy;
end;

{ TQueryWriter }

constructor TQueryWriter.Create(const AQuery: TQuery);
begin
  FQuery := AQuery;
end;

procedure TQueryWriter.Write(const AStrings: TStrings; out AParameters: TArray<TParameter>);
var
  LWriter: TStringWriter;
begin
  LWriter := TStringWriter.Create;
  try
    Write(LWriter, AParameters);
    AStrings.Text := LWriter.ToString;
  finally
    LWriter.Free;
  end;
end;

{ TFilterItemGroup }

constructor TFilterItemGroup.Create;
begin
  FFilters := TFilters.Create;
end;

destructor TFilterItemGroup.Destroy;
begin
  FFilters.Free;
  inherited;
end;

{ TFilterItem }

function TFilterItem.GetAttributes: TArray<string>;
begin
  Result := nil;
end;


{ TCommonFilterItem }

function TCommonFilterItem.GetAttributes: TArray<string>;
begin
  Result := TArray<string>.Create(Attribute);
end;


{ TQueryToJSON }

class procedure TQueryToJSON.WriteFilters(const AFilters: TFilters;
  const AJSON: TJSONObject);
var
  LFilter: TFilterItem;
  I: Integer;
begin
  for I := 0 to AFilters.Count - 1 do
  begin
    LFilter := AFilters.Items[I];
    WriteFilter(LFilter, AJSON);
  end;
end;

class function TQueryToJSON.ValueToJSON(const AValue: TValue): TJSONValue;
begin
  if AValue.IsEmpty then
    Result := TJSONNull.Create
  else
    case AValue.Kind of
      tkInteger:
        Result := TJSONNumber.Create(AValue.AsInteger);
      tkFloat:
        Result := TJSONNumber.Create(AValue.AsExtended);
      tkUString:
        Result := TJSONString.Create(AValue.AsString);
      tkInt64:
        Result := TJSONNumber.Create(AValue.AsInt64);
      tkEnumeration:
        if AValue.AsBoolean then
          Result := TJSONTrue.Create
        else
          Result := TJSONFalse.Create;
    else
      raise Exception.CreateFmt('Unknown type: %s', [AValue.TypeInfo.Name]);
    end;
end;

class procedure TQueryToJSON.WriteSimpleFilter(const AFilter: TSimpleFilter; const AJSON: TJSONObject);
var
  LOp: string;
begin
  case AFilter.Op of
    TSimpleFilter.TOperator.lt:
      LOp := TJSONOpNames.OpLt;
    TSimpleFilter.TOperator.lte:
      LOp := TJSONOpNames.OpLte;
    TSimpleFilter.TOperator.gt:
      LOp := TJSONOpNames.OpGt;
    TSimpleFilter.TOperator.gte:
      LOp := TJSONOpNames.OpGte;
    TSimpleFilter.TOperator.eq:;
    TSimpleFilter.TOperator.neq:
      LOp := TJSONOpNames.OpNeq;
    TSimpleFilter.TOperator.like:
      LOp := TJSONOpNames.OpLike;
    TSimpleFilter.TOperator.nlike:
      LOp := TJSONOpNames.OpNlike;
  end;
  if LOp <> '' then
    AJSON.AddPair(AFilter.Attribute, TJSONObject.Create(TJSONPair.Create(LOp, ValueToJSON(AFilter.Value))))
  else
    AJSON.AddPair(AFilter.Attribute, ValueToJSON(AFilter.Value));
end;

class procedure TQueryToJSON.WriteExistsFilter(const AFilter: TExistsFilter; const AJSON: TJSONObject);
begin
  if AFilter.Exists then
    AJSON.AddPair(AFilter.Attribute, TJSONObject.Create(TJSONPair.Create(TJSONOpNames.OpExists, TJSONTrue.Create)))
  else
    AJSON.AddPair(AFilter.Attribute, TJSONObject.Create(TJSONPair.Create(TJSONOpNames.OpExists, TJSONFalse.Create)))
end;

class procedure TQueryToJSON.WriteRangeFilter(const AFilter: TRangeFilter; const AJSON: TJSONObject);

  procedure WriteOne(const AJSON: TJSONObject; AOp: TRangeFilter.TOperator; const AValue: TValue);
  var
    LOp: string;
  begin
    case AOp of
      TRangeFilter.TOperator.lt:
        LOp := TJSONOpNames.OpLt;
      TRangeFilter.TOperator.lte:
        LOp := TJSONOpNames.OpLte;
      TRangeFilter.TOperator.gt:
        LOp := TJSONOpNames.OpGt;
      TRangeFilter.TOperator.gte:
        LOp := TJSONOpNames.OpGte;
    else
      Assert(False);
    end;
    AJSON.AddPair(LOp, ValueToJSON(AValue))
  end;
var
  LRange: TJSONObject;
begin
  LRange := TJSONObject.Create;
  try
    WriteOne(LRange, AFilter.Op1, AFilter.Value1);
    WriteOne(LRange, AFilter.Op2, AFilter.Value2);
    AJSON.AddPair(AFilter.Attribute, LRange)
  except
    LRange.Free;
    raise;
  end;
end;

class procedure TQueryToJSON.WriteOrFilter(const AFilter: TOrFilter; const AJSON: TJSONObject);
var
  LArray: TJSONArray;
  LJSON: TJSONObject;
  I: Integer;
begin
  LArray := TJSONArray.Create;
  try
    for I := 0 to AFilter.Filters.Count - 1 do
    begin
      LJSON := TJSONObject.Create;
      try
        WriteFilter(AFilter.Filters.Items[I], LJSON);
        LArray.Add(LJSON);
      except
        LJSON.Free;
        raise;
      end;
    end;
    AJSON.AddPair(TJSONPair.Create(TJSONOpNames.OpOr, LArray));
  except
    LArray.Free;
    raise;
  end;
end;

class procedure TQueryToJSON.WriteAndFilter(const AFilter: TAndFilter; const AJSON: TJSONObject);
var
  LArray: TJSONArray;
  LJSON: TJSONObject;
  I: Integer;
begin
  LArray := TJSONArray.Create;
  try
    for I := 0 to AFilter.Filters.Count - 1 do
    begin
      LJSON := TJSONObject.Create;
      try
        WriteFilter(AFilter.Filters.Items[I], LJSON);
        LArray.Add(LJSON);
      except
        LJSON.Free;
        raise;
      end;
    end;
    AJSON.AddPair(TJSONPair.Create(TJSONOpNames.OpAnd, LArray));
  except
    LArray.Free;
    raise;
  end;
end;

class procedure TQueryToJSON.WriteInFilter(const AFilter: TInFilter; const AJSON: TJSONObject);
var
  LArray: TJSONArray;
  LValue: TValue;
  LOp: string;
begin
  LArray := TJSONArray.Create;
  try
    for LValue in AFilter.Values do
      LArray.AddElement(ValueToJSON(LValue));
    case AFilter.Op of
      TInFilter.TOperator.inop:
        LOp := TJSONOpNames.OpIn;
      TInFilter.TOperator.ninop:
        LOp := TJSONOpNames.OpNin;
    else
      Assert(False);
    end;
    Assert(LOp <> '');
    AJSON.AddPair(AFilter.Attribute, TJSONObject.Create(TJSONPair.Create(LOp, LArray)));
  except
    LArray.Free;
    raise;
  end;
end;

class procedure TQueryToJSON.WriteFilter(const AFilter: TFilterItem;
  const AJSON: TJSONObject);
begin
  if AFilter is TSimpleFilter then
    WriteSimpleFilter(TSimpleFilter(AFilter), AJSON)
  else if AFilter is TExistsFilter then
    WriteExistsFilter(TExistsFilter(AFilter), AJSON)
  else if AFilter is TInFilter then
    WriteInFilter(TInFilter(AFilter), AJSON)
  else if AFilter is TRangeFilter then
    WriteRangeFilter(TRangeFilter(AFilter), AJSON)
  else if AFilter is TOrFilter then
    WriteOrFilter(TOrFilter(AFilter), AJSON)
  else if AFilter is TAndFilter then
    WriteAndFilter(TAndFilter(AFilter), AJSON)
  else
    raise Exception.Create('Unknown filter');
end;

procedure ParseJSON(const AJSONObject: TJSONObject; const AFilters: TFilters); forward;

{ TJSONFilterParser }

class procedure TJSONFilterParser.Parse(const AJSONObject: TJSONObject;
  const AFilters: TFilters; const AUtcOffset: Integer);
begin
  try
    AFilters.Clear;
    FUtcOffset := AUtcOffset;
    ParseJSON(AJSONObject, AFilters);
  except
    AFilters.Clear;
    raise;
  end;
end;

// Internal methods

procedure ParseOr(const AJSONArray: TJSONArray; const AFilter: TOrFilter); forward;
procedure ParseAnd(const AJSONArray: TJSONArray; const AFilter: TAndFilter); forward;
procedure ParseNameValue(const AName: string; AJSONValue: TJSONValue; const AFilters: TFilters); forward;
procedure ParseSimpleFilter(const AName: string; AOp: TSimpleFilter.TOperator; const AValue: TJSONValue; const AFilters: TFilters); forward;
procedure ParseOneOperator(const AName, AOp: string; const AJSONValue: TJSONValue; const AFilters: TFilters); forward;
function ParseValue(const AJSONValue: TJSONValue): TValue; forward;
procedure ParseTwoOperators(const AName, AOp1: string; const AJSONValue1: TJSONValue;
  const AOp2: string; const AJSONValue2: TJSONValue; const AFilters: TFilters); forward;

procedure RaiseUnexpectedJSONArgument(const AMessage: string);
begin
  raise EFilterParserException.Create(AMessage);
end;

procedure RaiseUnexpectedJSON(const AJSONValue: TJSONValue);
begin
  raise EFilterParserException.Create('Unexpected JSON: ' + AJSONValue.ToString);
end;

procedure RaiseUnexpectedOperator(const AOperator: string);
begin
  raise EFilterParserException.Create('Unexpected operator: ' + AOperator);
end;

function CheckObject(const AJSON: TJSONValue): TJSONObject;
begin
  if not (AJSON is TJSONObject) then
    RaiseUnexpectedJSON(AJSON);
  Result := TJSONObject(AJSON);
end;


function CheckArray(const AJSON: TJSONValue): TJSONArray;
begin
  if not (AJSON is TJSONArray) then
    RaiseUnexpectedJSON(AJSON);
  Result := TJSONArray(AJSON);
end;

procedure ParseOr(const AJSONArray: TJSONArray; const AFilter: TOrFilter);
var
  LJSONValue: TJSONValue;
begin
  for LJSONValue in AJSONArray do
    ParseJSON(CheckObject(LJSONValue), AFilter.Filters);
end;

procedure ParseAnd(const AJSONArray: TJSONArray; const AFilter: TAndFilter);
var
  LJSONValue: TJSONValue;
begin
  for LJSONValue in AJSONArray do
    ParseJSON(CheckObject(LJSONValue), AFilter.Filters);
end;

var
  FSimpleFilterLookup: TDictionary<string, TSimpleFilter.TOperator>;
  FExistsOperator: string;
  FInFilterLookup: TDictionary<string, TInFilter.TOperator>;
  FRangeFilterLookup: TDictionary<string, TRangeFilter.TOperator>;

procedure ParseExistsFilter(const AName: string; const AValue: TJSONValue; const AFilters: TFilters);
var
  LExistsFilter: TExistsFilter;
begin
  LExistsFilter := TExistsFilter.Create;
  AFilters.Add(LExistsFilter);
  LExistsFilter.Attribute := AName;
  if AValue is TJSONTrue then
    LExistsFilter.Exists := True
  else if AValue is TJSONFalse then
    LExistsFilter.Exists := False
  else
    RaiseUnexpectedJSONArgument('Boolean argment expected');
end;

procedure ParseSimpleFilter(const AName: string; AOp: TSimpleFilter.TOperator; const AValue: TJSONValue; const AFilters: TFilters);
var
  LSimpleFilter: TSimpleFilter;
begin
  LSimpleFilter := TSimpleFilter.Create;
  AFilters.Add(LSimpleFilter);
  LSimpleFilter.Attribute := AName;
  LSimpleFilter.Op := AOp;
  LSimpleFilter.Value := ParseValue(AValue);
end;

procedure ParseInFilter(const AName: string; AOp: TInFilter.TOperator; const AValue: TJSONValue; const AFilters: TFilters);
var
  LInFilter: TInFilter;
  LValues: TList<TValue>;
  LJSONValue: TJSONValue;
begin
  LInFilter := TInFilter.Create;
  AFilters.Add(LInFilter);
  LInFilter.Attribute := AName;
  LInFilter.Op := AOp;
  LValues := TList<TValue>.Create;
  try
    if AValue is TJSONArray then
      for LJSONValue in TJSONArray(AValue) do
        LValues.Add(ParseValue(LJSONValue))
    else
      RaiseUnexpectedJSON(AValue);
    LInFilter.Values := LValues.ToArray;
  finally
    LValues.Free;
  end;
end;

procedure ParseRangeFilter(const AName: string; AOp1: TRangeFilter.TOperator; const AValue1: TJSONValue;
  AOp2: TRangeFilter.TOperator; const AValue2: TJSONValue;
  const AFilters: TFilters);
var
  LRangeFilter: TRangeFilter;
begin
  LRangeFilter := TRangeFilter.Create;
  AFilters.Add(LRangeFilter);
  LRangeFilter.Attribute := AName;
  LRangeFilter.Op1 := AOp1;
  LRangeFilter.Op2 := AOp2;
  LRangeFilter.Value1 := ParseValue(AValue1);
  LRangeFilter.Value2 := ParseValue(AValue2);
end;

procedure ParseOneOperator(const AName, AOp: string; const AJSONValue: TJSONValue; const AFilters: TFilters);
var
  LSimpleOperator: TSimpleFilter.TOperator;
  LInOperator: TInFilter.TOperator;
begin
  if AOp = FExistsOperator then
    ParseExistsFilter(AName, AJSONValue, AFilters)
  else if FSimpleFilterLookup.TryGetValue(AOp, LSimpleOperator) then
    ParseSimpleFilter(AName, LSimpleOperator, AJSONValue, AFilters)
  else if FInFilterLookup.TryGetValue(AOp, LInOperator) then
    ParseInFilter(AName, LInOperator, AJSONValue, AFilters)
  else
    RaiseUnexpectedOperator(AOp);
end;

procedure ParseTwoOperators(const AName, AOp1: string; const AJSONValue1: TJSONValue;
  const AOp2: string; const AJSONValue2: TJSONValue; const AFilters: TFilters);
var
  LOp1: TRangeFilter.TOperator;
  LOp2: TRangeFilter.TOperator;
begin
  if not FRangeFilterLookup.TryGetValue(AOp1, LOp1) then
    RaiseUnexpectedOperator(AOp1);

  if not FRangeFilterLookup.TryGetValue(AOp2, LOp2) then
    RaiseUnexpectedOperator(AOp2);

  ParseRangeFilter(AName, LOp1, AJSONValue1, LOp2, AJSONValue2, AFilters);

end;

function ParseValue(const AJSONValue: TJSONValue): TValue;

  function IsValidDateString(const ADateStr: string; out ADateTime: TDateTime): Boolean;
  var
    FSettings: TFormatSettings;
  begin
    Result := TryStrToDate(ADateStr,ADateTime);
    if Result then
      exit;
    FSettings := TFormatSettings.Create;
    FSettings.ShortDateFormat := TJSONFilterParser.USDate;
    Result := TryStrToDateTime(ADateStr,ADateTime,FSettings);
    if Result then
      exit;
    FSettings.ShortDateFormat := TJSONFilterParser.ISO8601_date;  //ISO8601
    Result := TryStrToDateTime(ADateStr,ADateTime,FSettings);
  end;

  function ParseDateToObject(ADateTime: TDateTime): Tvalue;
  var
    LTimeDiff, LServerTimeZone: integer;
  begin
    LServerTimeZone := Trunc(TTimeZone.Local.UtcOffset.Hours * 60 + TTimeZone.Local.UtcOffset.Minutes);
    LTimeDiff := LServerTimeZone + TJSONFilterParser.FUtcOffset;
    ADateTime := IncMinute(ADateTime,LTimeDiff);
    Result := TValue.From<variant>(ADateTime);
  end;


var
  LInteger: Int64;
  LDateTime: TDateTime;
begin
  if AJSONValue is TJSONNumber then
  begin
    if AJSONValue.TryGetValue<Int64>('', LInteger) then
      Result := LInteger
    else
      Result := TJSONNumber(AJSONValue).AsDouble;
  end
  else if AJSONValue is TJSONTrue then
    Result := True
  else if AJSONValue is TJSONFalse then
    Result := False
  else if AJSONValue is TJSONNull then
    Result := TValue.Empty
  else if AJSONValue is TJSONString then
  begin
    if IsValidDateString(TJSONString(AJSONValue).Value, LDateTime) then
      Result := ParseDateToObject(LDateTime)
    else
      Result := TJSONString(AJSONValue).Value;
  end
  else
    RaiseUnexpectedJSON(AJSONValue);
end;



procedure ParseCompare(const AName: string; const AJSONValue: TJSONValue; const AFilters: TFilters);
var
  LJSONObject: TJSONObject;
begin
  if AJSONValue is TJSONObject then
  begin
    LJSONObject := TJSONObject(AJSONValue);
    if LJSONObject.Count = 1 then
    begin
      ParseOneOperator(AName, LJSONObject.Pairs[0].JsonString.Value, LJSONObject.Pairs[0].JsonValue, AFilters);
    end
    else if LJSONObject.Count = 2 then
      ParseTwoOperators(AName, LJSONObject.Pairs[0].JsonString.Value, LJSONObject.Pairs[0].JsonValue,
        LJSONObject.Pairs[1].JsonString.Value, LJSONObject.Pairs[1].JsonValue,
        AFilters)
    else
      RaiseUnexpectedJSON(LJSONObject);
  end
  else
  begin
    // {"playerName":"Sean Plott"}
    ParseSimpleFilter(AName, TSimpleFilter.TOperator.eq, AJSONValue, AFilters);
  end
end;

procedure ParseNameValue(const AName: string; AJSONValue: TJSONValue; const AFilters: TFilters);
var
  LOrFilter: TOrFilter;
  LAndFilter: TAndFilter;
begin
  if AName = TJSONOpNames.OpOr then
  begin
    LOrFilter := TOrFilter.Create;
    AFilters.Add(LOrFilter);
    ParseOr(CheckArray(AJSONValue), LOrFilter);
  end
  else if AName = TJSONOpNames.OpAnd then
  begin
    LAndFilter := TAndFilter.Create;
    AFilters.Add(LAndFilter);
    ParseAnd(CheckArray(AJSONValue), LAndFilter);
  end
  else
    ParseCompare(AName, AJSONValue, AFilters);
end;

procedure ParseJSON(const AJSONObject: TJSONObject; const AFilters: TFilters);
var
  LPair: TJSONPair;
begin
  for LPair in AJSONObject do
    ParseNameValue(LPair.JSONString.Value, LPair.JSONValue, AFilters)
end;

function CreateSimpleFilterLookup: TDictionary<string, TSimpleFilter.TOperator>;
begin
  Result := TDictionary<string, TSimpleFilter.TOperator>.Create;
  Result.Add(TJSONOpNames.OpLike, TSimpleFilter.TOperator.like);
  Result.Add(TJSONOpNames.OpNlike, TSimpleFilter.TOperator.nlike);
  Result.Add(TJSONOpNames.OpEq, TSimpleFilter.TOperator.eq);
  Result.Add(TJSONOpNames.OpNEq, TSimpleFilter.TOperator.neq);
  Result.Add(TJSONOpNames.OpGt, TSimpleFilter.TOperator.gt);
  Result.Add(TJSONOpNames.OpGte, TSimpleFilter.TOperator.gte);
  Result.Add(TJSONOpNames.OpLt, TSimpleFilter.TOperator.lt);
  Result.Add(TJSONOpNames.OpLte, TSimpleFilter.TOperator.lte);
end;

function CreateInFilterLookup: TDictionary<string, TInFilter.TOperator>;
begin
  Result := TDictionary<string, TInFilter.TOperator>.Create;
  Result.Add(TJSONOpNames.OpIn, TInFilter.TOperator.inop);
  Result.Add(TJSONOpNames.OpNin, TInFilter.TOperator.ninop);
end;

function CreateRangeFilterLookup: TDictionary<string, TRangeFilter.TOperator>;
begin
  Result := TDictionary<string, TRangeFilter.TOperator>.Create;
  Result.Add(TJSONOpNames.OpLt, TRangeFilter.TOperator.lt);
  Result.Add(TJSONOpNames.OpLte, TRangeFilter.TOperator.lte);
  Result.Add(TJSONOpNames.OpGt, TRangeFilter.TOperator.Gt);
  Result.Add(TJSONOpNames.OpGte, TRangeFilter.TOperator.Gte);
end;

initialization
  FExistsOperator := TJSONOpNames.OpExists;;
  FSimpleFilterLookup := CreateSimpleFilterLookup;
  FInFilterLookup := CreateInFilterLookup;
  FRangeFilterLookup := CreateRangeFilterLookup;
finalization
  FSimpleFilterLookup.Free;
  FInFilterLookup.Free;
  FRangeFilterLookup.Free;
end.
