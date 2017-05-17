{*******************************************************}
{                                                       }
{               Delphi DataSnap Framework                }
{                                                       }
{ Copyright(c) 2004-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
unit Data.FireDACJSONReflect;

interface

uses System.Classes, System.SysUtils, System.Json, Data.DBXJSONReflect, System.Generics.Collections,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Stan.Error, FireDAC.Dats, FireDAC.Stan.Intf, FireDAC.Stan.Option;

type

{$REGION 'Dataset lists'}
  TFDJSONDataSetsBase = class;

  /// <summary>Helper to convert and revert objects for marshalling dataset
  /// lists.</summary>
  TFDJSONInterceptor = class(TJSONInterceptor)
  public type
    TItemPair = TPair<string, TFDAdaptedDataSet>;
    TItemList = TList<TItemPair>;
  private
    FFreeObjects: TObjectList<TObject>;
  public
    // Conversion utility methods
    class function ItemListToJSONObject(const AList: TItemList; const AJSONObject: TJSONObject): Boolean; static;
    class function DataSetsToJSONObject(const ADataSets: TFDJSONDataSetsBase; const AJSONObject: TJSONObject): Boolean; static;
    class function JSONObjectToItemList(const AJSONObject: TJSONObject; const AList: TItemList): Boolean; static;
    class function JSONObjectToDataSets(const AJSONObject: TJSONObject; const ADataSets: TFDJSONDataSetsBase): Boolean; static;
    destructor Destroy; override;
    // Reverter/Converter overrides
    function TypeObjectConverter(AData: TObject): TObject; override;
    function TypeObjectReverter(AData: TObject): TObject; override;
  end;

  /// <summary> List of FireDAC datasets that can be marshaled as JSON
  /// </summary>
  TFDJSONDataSetsBase = class
  public type
    TItemPair = TPair<string, TFDAdaptedDataSet>;
    TItemList = class(TList<TItemPair>);
  private
    [JSONReflect(ctTypeObject, rtTypeObject, TFDJSONInterceptor,nil,true)]
    [JSONOwned(false)] // Prevent unmarshal from freeing FList after construction
    FDataSets: TItemList;
    [JSONMarshalled(false)]
    FOwnsDataSets: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>Dataset list payload class.</summary>
  TFDJSONDataSets = class sealed(TFDJSONDataSetsBase)
  end;

  /// <summary>Delta list payload class.</summary>
  TFDJSONDeltas = class sealed(TFDJSONDataSetsBase)
  end;
{$ENDREGION}

{$REGION 'Reader and Writer interfaces'}
  TFDJSONErrors = class;

  /// <summary>Dataset list payload writer interface.</summary>
  IFDJSONDataSetsWriter = interface
    ['{1DD9F6B1-D841-4475-98B6-5AA985D05DFC}']
    procedure Add(const AName: string; const ADataSet: TFDAdaptedDataSet); overload;
    procedure Add(const ADataSet: TFDAdaptedDataSet); overload;
    function GetDataSetList: TFDJSONDataSets;
    property DataSetList: TFDJSONDataSets read GetDataSetList;
  end;

  /// <summary>Delta list payload writer interface.</summary>
  IFDJSONDeltasWriter = interface
    ['{2C9646B0-DCFC-43A3-838D-1E9592095D71}']
    procedure Add(const AName: string; const AFDMemTable: TFDMemTable); overload;
    procedure Add(const AFDMemTable: TFDMemTable); overload;
    function GetDeltaList: TFDJSONDeltas;
    property DeltaList: TFDJSONDeltas read GetDeltaList;
  end;

  /// <summary>Dataset list payload reader interface.</summary>
  IFDJSONDataSetsReader = interface
    ['{00C69185-9E6C-4FEA-A8F8-2344972B4C68}']
    function GetDataSetList: TFDJSONDataSets;
    property DataSetList: TFDJSONDataSets read GetDataSetList;

    function GetCount: Integer;
    function GetValue(I: Integer): TFDAdaptedDataSet;
    function GetValueByName(const AName: string): TFDAdaptedDataSet;
    function GetKey(I: Integer): string;
    function GetItem(I: Integer): TPair<string, TFDAdaptedDataSet>;
    function ExtractItem(I: Integer): TPair<string, TFDAdaptedDataSet>;
    property Count: Integer read GetCount;
    property Values[I: Integer]: TFDAdaptedDataSet read GetValue;
    property Keys[I: Integer]: string read GetKey;
    property Items[I: Integer]: TPair<string, TFDAdaptedDataSet> read GetItem;
    property ValueByName[const AName: string]: TFDAdaptedDataSet read GetValueByName;
  end;

  /// <summary>Delta list payload reader interface.</summary>
  IFDJSONDeltasReader = interface
    ['{45073B83-F37A-40C9-9749-56C96F7B7EEA}']
    function GetDeltaList: TFDJSONDeltas;
    property DeltaList: TFDJSONDeltas read GetDeltaList;

    function GetCount: Integer;
    function GetValue(I: Integer): TFDMemTable;
    function GetValueByName(const AName: string): TFDMemTable;
    function GetKey(I: Integer): string;
    function GetItem(I: Integer): TPair<string, TFDMemTable>;
    function ExtractItem(I: Integer): TPair<string, TFDMemTable>;
    property Count: Integer read GetCount;
    property Values[I: Integer]: TFDMemTable read GetValue;
    property Keys[I: Integer]: string read GetKey;
    property Items[I: Integer]: TPair<string, TFDMemTable> read GetItem;
    property ValueByName[const AName: string]: TFDMemTable read GetValueByName;
  end;

  /// <summary>Delta list payload interface to apply updates.</summary>
  IFDJSONDeltasApplyUpdates = interface(IFDJSONDeltasReader)
    ['{58213D3C-BFE8-4BE3-8197-4236FFC215C8}']
    function ApplyUpdates(const AKey: string; const ASelectCommand: TFDCustomCommand): Integer; overload;
    function ApplyUpdates(const Index: Integer; const ASelectCommand: TFDCustomCommand): Integer; overload;
    function ApplyUpdates(const AKey: string; const AAdapter: TFDTableAdapter): Integer; overload;
    function ApplyUpdates(const Index: Integer; const AAdapter: TFDTableAdapter): Integer; overload;
    function GetErrors: TFDJSONErrors;
    property Errors: TFDJSONErrors read GetErrors;
  end;

  /// <summary>Delta list payload interface to apply updates that allows
  /// defining a maximum number of errors that is allowed.</summary>
  IFDJSONDeltasApplyUpdatesMax = interface(IFDJSONDeltasReader)
    ['{58213D3C-BFE8-4BE3-8197-4236FFC215C8}']
    function ApplyUpdates(const AKey: string; const ASelectCommand: TFDCustomCommand; AMaxErrors: Integer = -1): Integer; overload;
    function ApplyUpdates(const Index: Integer; const ASelectCommand: TFDCustomCommand; AMaxErrors: Integer = -1): Integer; overload;
    function ApplyUpdates(const AKey: string; const AAdapter: TFDTableAdapter; AMaxErrors: Integer = -1): Integer; overload;
    function ApplyUpdates(const Index: Integer; const AAdapter: TFDTableAdapter; AMaxErrors: Integer = -1): Integer; overload;
    function GetErrors: TFDJSONErrors;
    property Errors: TFDJSONErrors read GetErrors;
  end;

{$ENDREGION}

{$REGION 'Reader and Writer base classes'}
  /// <summary>Base class for writers.</summary>
  TFDJSONDataSetsWriterBase = class(TInterfacedObject)
  public
    type
      TItemPair = TPair<string, TFDAdaptedDataSet>;
      TPairList = TList<TItemPair>;
  protected
    class procedure CreateList(const ADataList: TFDJSONDataSetsBase); virtual; abstract;
    class function GetPairList(const AList: TFDJSONDataSetsBase): TPairList;
    class procedure ListAddT<T: TFDADaptedDataSet>(const ADataList: TFDJSONDataSetsBase; const AName: string; const ADataSet: T); overload;
    class procedure ListAddT<T: TFDADaptedDataSet>(const ADataList: TFDJSONDataSetsBase; const ADataSet: T); overload;
  end;

  /// <summary>Base class for readers.</summary>
  TFDJSONDataSetsReaderBase = class(TInterfacedObject)
  public
    type
      TItemPair = TPair<string, TFDAdaptedDataSet>;
      TPairList = TList<TItemPair>;
  private
    class function GetPairList(const AList: TFDJSONDataSetsBase): TPairList;
  protected
    class function GetListCountT(const ADataList: TFDJSONDataSetsBase): Integer;
    class function GetListValueT<T: TFDADaptedDataSet>(const ADataList: TFDJSONDataSetsBase; I: Integer): T;
    class function GetListValueByNameT<T: TFDADaptedDataSet>(const ADataList: TFDJSONDataSetsBase; const AName: string): T;
    class function GetListKeyT<T: TFDADaptedDataSet>(const ADataList: TFDJSONDataSetsBase; I: Integer): string;
    class function GetListItemT<T: TFDADaptedDataSet>(const ADataList: TFDJSONDataSetsBase; I: Integer): TPair<string, T>;
    class function ExtractListItemT<T: TFDADaptedDataSet>(const ADataList: TFDJSONDataSetsBase; I: Integer): TPair<string, T>;
  end;
{$ENDREGION}

{$REGION 'Reader and Writer classes'}
  /// <summary>Dataset list payload writer.</summary>
  TFDJSONDataSetsWriter = class(TFDJSONDataSetsWriterBase, IFDJSONDataSetsWriter)
  private
    FDataSetList: TFDJSONDataSets;
  protected
    class procedure CreateList(const ADataList: TFDJSONDataSetsBase); override;
    function GetList: TFDJSONDataSetsBase;
    { IFDJSONDataSetsWriter }
    procedure Add(const AName: string; const ADataSet: TFDAdaptedDataSet); overload;
    procedure Add(const ADataSet: TFDAdaptedDataSet); overload;
    function GetDataSetList: TFDJSONDataSets;
  public
    { public class methods }
    class procedure ListAdd(const ADataList: TFDJSONDataSets; const AName: string; const ADataSet: TFDAdaptedDataSet); overload;
    class procedure ListAdd(const ADataList: TFDJSONDataSets; const ADataSet: TFDAdaptedDataSet); overload;
  public
    constructor Create(const ADataSetList: TFDJSONDataSets);
  end;

  /// <summary>Dataset list payload reader.</summary>
  TFDJSONDataSetsReader = class(TFDJSONDataSetsReaderBase, IFDJSONDataSetsReader)
  public type
    TItemPair = TPair<string, TFDAdaptedDataSet>;
  private
    FDataSetList: TFDJSONDataSets;
  protected
    class function GetListReader(const ADataList: TFDJSONDataSets): IFDJSONDataSetsReader; static;
    { IFDJSONDataSetsReader }
    function GetCount: Integer;
    function GetValue(I: Integer): TFDAdaptedDataSet;
    function GetValueByName(const AName: string): TFDAdaptedDataSet;
    function GetKey(I: Integer): string;
    function GetItem(I: Integer): TItemPair;
    function ExtractItem(I: Integer): TItemPair; overload;
    function GetDataSetList: TFDJSONDataSets;
  public
    { public class methods }
    class function GetListCount(const ADataList: TFDJSONDataSets): Integer; static;
    class function GetListValue(const ADataList: TFDJSONDataSets; I: Integer): TFDAdaptedDataSet; static;
    class function GetListValueByName(const ADataList: TFDJSONDataSets;const AName: string): TFDAdaptedDataSet; static;
    class function GetListKey(const ADataList: TFDJSONDataSets; I: Integer): string; static;
    class function GetListItem(const ADataList: TFDJSONDataSets;I: Integer): TItemPair; static;
    class function ExtractListItem(const ADataList: TFDJSONDataSets; I: Integer): TItemPair; static;
  public
    constructor Create(const ADataSetList: TFDJSONDataSets); virtual;
  end;

  /// <summary>Delta list payload writer.</summary>
  TFDJSONDeltasWriter = class(TFDJSONDataSetsWriterBase, IFDJSONDeltasWriter)
  private
    FDeltaList: TFDJSONDeltas;
  protected
    class procedure CreateList(const ADataList: TFDJSONDataSetsBase); override;
    function CreateDelta(const AFDMemTable: TFDMemTable): TFDMemTable;
    { IFDJSONDataSetsWriter }
    procedure Add(const AName: string; const AFDMemTable: TFDMemTable); overload;
    procedure Add(const AFDMemTable: TFDMemTable); overload;
    function GetDeltaList: TFDJSONDeltas;
  public
    { public class methods }
    class procedure ListAdd(const ADeltaList: TFDJSONDeltas; const AName: string; const AFDMemTable: TFDMemTable); overload; static;
    class procedure ListAdd(const ADeltaList: TFDJSONDeltas; const AFDMemTable: TFDMemTable); overload; static;
  public
    constructor Create(const ADeltaList: TFDJSONDeltas); virtual;
  end;

  /// <summary>Delta list payload reader.</summary>
  TFDJSONDeltasReader = class(TFDJSONDataSetsReaderBase, IFDJSONDeltasReader)
  public type
    TItemPair = TPair<string, TFDMemTable>;
  private
    FDeltaList: TFDJSONDeltas;
  protected
    { IFDJSONDeltasWriter }
    function GetDeltaList: TFDJSONDeltas;
    function GetCount: Integer;
    function GetValue(I: Integer): TFDMemTable;
    function GetValueByName(const AName: string): TFDMemTable;
    function GetKey(I: Integer): string;
    function GetItem(I: Integer): TItemPair;
    function ExtractItem(I: Integer): TItemPair;
  public
    { public class methods }
    class function GetListCount(const ADataList: TFDJSONDeltas): Integer; static;
    class function GetListValue(const ADataList: TFDJSONDeltas; I: Integer): TFDMemTable; static;
    class function GetListValueByName(const ADataList: TFDJSONDeltas;const AName: string): TFDMemTable; static;
    class function GetListKey(const ADataList: TFDJSONDeltas; I: Integer): string; static;
    class function GetListItem(const ADataList: TFDJSONDeltas;I: Integer): TItemPair; static;
    class function ExtractListItem(const ADataList: TFDJSONDeltas; I: Integer): TItemPair; static;
  public
    constructor Create(const ADeltaList: TFDJSONDeltas); virtual;
  end;

  /// <summary>Delta list payload to apply updates.</summary>
  TFDJSONDeltasApplyUpdates = class(TFDJSONDeltasReader, IFDJSONDeltasApplyUpdates)
  private
    FErrors: TFDJSONErrors;
  protected
    class function ApplyUpdates(const ADelta: TFDMemTable;
      const AAdapter: TFDTableAdapter; const AErrors: TFDJSONErrors): Integer; overload;
    class function ApplyUpdates(const ADelta: TFDMemTable;
      const ACommand: TFDCustomCommand; const AErrors: TFDJSONErrors): Integer; overload;
    { IFDJSONDeltasApplyUpdates }
    function ApplyUpdates(const AKey: string; const ASelectCommand: TFDCustomCommand): Integer; overload;
    function ApplyUpdates(const Index: Integer; const ASelectCommand: TFDCustomCommand): Integer; overload;
    function ApplyUpdates(const AKey: string; const AAdapter: TFDTableAdapter): Integer; overload;
    function ApplyUpdates(const Index: Integer; const AAdapter: TFDTableAdapter): Integer; overload;
    function GetErrors: TFDJSONErrors;
   public
    { public class methods }
    class function ListApplyUpdates(const ADeltaList: TFDJSONDeltas; const AKey: string; const ASelectCommand: TFDCustomCommand; const AErrors: TFDJSONErrors = nil): Integer; overload; static;
    class function ListApplyUpdates(const ADeltaList: TFDJSONDeltas; const Index: Integer; const ASelectCommand: TFDCustomCommand; const AErrors: TFDJSONErrors = nil): Integer; overload; static;
    class function ListApplyUpdates(const ADeltaList: TFDJSONDeltas; const AKey: string; const AAdapter: TFDTableAdapter; const AErrors: TFDJSONErrors = nil): Integer; overload; static;
    class function ListApplyUpdates(const ADeltaList: TFDJSONDeltas; const Index: Integer; const AAdapter: TFDTableAdapter; const AErrors: TFDJSONErrors = nil): Integer; overload; static;
  public
    constructor Create(const ADeltaList: TFDJSONDeltas); override;
    destructor Destroy; override;
  end;

  TFDJSONDeltasApplyUpdatesMax = class(TFDJSONDeltasReader, IFDJSONDeltasApplyUpdatesMax)
  private
    FErrors: TFDJSONErrors;
  protected
    class function ApplyUpdates(const ADelta: TFDMemTable;
      const AAdapter: TFDTableAdapter; AMaxErrors: Integer; const AErrors: TFDJSONErrors): Integer; overload;
    class function ApplyUpdates(const ADelta: TFDMemTable;
      const ACommand: TFDCustomCommand; AMaxErrors: Integer; const AErrors: TFDJSONErrors): Integer; overload;
    { IFDJSONDeltasApplyUpdatesMax }
    function ApplyUpdates(const AKey: string; const ASelectCommand: TFDCustomCommand; AMaxErrors: Integer = -1): Integer; overload;
    function ApplyUpdates(const Index: Integer; const ASelectCommand: TFDCustomCommand; AMaxErrors: Integer = -1): Integer; overload;
    function ApplyUpdates(const AKey: string; const AAdapter: TFDTableAdapter; AMaxErrors: Integer = -1): Integer; overload;
    function ApplyUpdates(const Index: Integer; const AAdapter: TFDTableAdapter; AMaxErrors: Integer = -1): Integer; overload;
    function GetErrors: TFDJSONErrors;
   public
    { public class methods }
    class function ListApplyUpdates(const ADeltaList: TFDJSONDeltas; const AKey: string; const ASelectCommand: TFDCustomCommand; AMaxErrors: Integer = -1; const AErrors: TFDJSONErrors = nil): Integer; overload; static;
    class function ListApplyUpdates(const ADeltaList: TFDJSONDeltas; const Index: Integer; const ASelectCommand: TFDCustomCommand; AMaxErrors: Integer = -1;  const AErrors: TFDJSONErrors = nil): Integer; overload; static;
    class function ListApplyUpdates(const ADeltaList: TFDJSONDeltas; const AKey: string; const AAdapter: TFDTableAdapter; AMaxErrors: Integer = -1;  const AErrors: TFDJSONErrors = nil): Integer; overload; static;
    class function ListApplyUpdates(const ADeltaList: TFDJSONDeltas; const Index: Integer; const AAdapter: TFDTableAdapter; AMaxErrors: Integer = -1;  const AErrors: TFDJSONErrors = nil): Integer; overload; static;
  public
    constructor Create(const ADeltaList: TFDJSONDeltas); override;
    destructor Destroy; override;
  end;

{$ENDREGION}

{$REGION 'Error types'}
  /// <summary> Handle ApplyUpdates errors
  /// </summary>
  TFDJSONErrors = class(TObject)
  private
    FErrors: TStrings;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Override the following methods to set event handlers for custom error handling
    procedure DoBeforeApplyUpdates(ASender: TFDMemTable); virtual;
    procedure DoAfterApplyUpdates(ASender: TFDMemTable); virtual;
    procedure UpdateError(ASender: TDataSet; AException: EFDException;
     ARow: TFDDatSRow; ARequest: TFDUpdateRequest; var AAction: TFDErrorAction); virtual;
    procedure Clear;
    property Strings: TStrings read FErrors;
    property Count: Integer read GetCount;
  end;

  EFDDataListError = class(Exception);
{$ENDREGION}

implementation

uses
  System.RTLConsts, Data.DBXPlatform, Data.FireDACJSONConsts,
  System.NetEncoding, FireDAC.DApt.Intf, System.ZLib;

procedure CopyDataSet(const ASource, ADest: TFDAdaptedDataSet);
var
  LStream: TStream;
begin
  LStream := TMemoryStream.Create;
  try
    ASource.SaveToStream(LStream, TFDStorageFormat.sfBinary);
    LStream.Seek(0, TSeekOrigin.soBeginning);
    ADest.LoadFromStream(LStream, TFDStorageFormat.sfBinary);
  finally
    LStream.Free;
  end;
end;

function DataSetToString(const ADataSet: TFDAdaptedDataSet): string;
var
  LBinary64: string;
  LMemoryStream: TMemoryStream;
  LStringStream: TStringStream;
  LDstStream: TMemoryStream;
  Zipper: TZCompressionStream;
begin
  LDstStream := TMemoryStream.Create;
  try
    LMemoryStream := TMemoryStream.Create;
    try
      ADataSet.SaveToStream(LMemoryStream, TFDStorageFormat.sfBinary);
      LMemoryStream.Seek(0, TSeekOrigin.soBeginning);
      Zipper := TZCompressionStream.Create(clDefault, LDstStream);
      try
        Zipper.CopyFrom(LMemoryStream, LMemoryStream.Size);
      finally
        Zipper.Free;
      end;
    finally
      LMemoryStream.Free;
    end;
    LDstStream.Seek(0, TSeekOrigin.soBeginning);

    LStringStream := TStringStream.Create;
    try
      TNetEncoding.Base64.Encode(LDstStream, LStringStream);
      LBinary64 := LStringStream.DataString;
    finally
      LStringStream.Free;
    end;
  finally
    LDstStream.Free;
  end;
  Result := LBinary64;
end;

function MemTableFromString(const AValue: string): TFDMemTable;
var
  LMemTable: TFDMemTable;
  LStringStream: TStringStream;
  LMemoryStream: TMemoryStream;
  LOrgStream: TMemoryStream;
  Unzipper: TZDecompressionStream;
begin
  LMemTable := TFDMemTable.Create(nil);
  LMemoryStream := TMemoryStream.Create;
  try
    LStringStream := TStringStream.Create(AValue);
    try
      LOrgStream := TMemoryStream.Create;
      try
        TNetEncoding.Base64.Decode(LStringStream,LOrgStream);
        LOrgStream.Seek(0, TSeekOrigin.soBeginning);
        Unzipper := TZDecompressionStream.Create(LOrgStream);
        try
          LMemoryStream.CopyFrom(Unzipper, Unzipper.Size);
        finally
          Unzipper.Free;
        end;
        LMemoryStream.Seek(Longint(0), soFromBeginning);
      finally
        LOrgStream.Free;
      end;
    finally
      LStringStream.Free;
    end;
    LMemTable.LoadFromStream(LMemoryStream, TFDStorageFormat.sfBinary);
  finally
    LMemoryStream.Free;
  end;
  Result := LMemTable;
end;

/// <summary> Creates the JSON value representing a dataset.
/// </summary>
function DataSetToJSONValue(const ADataSet: TFDAdaptedDataSet): TJSONValue;
var
  S: string;
begin
  S := DataSetToString(ADataSet);
  Result := TJSONString.Create(S);
end;

// Converts dataset list into json
class function TFDJSONInterceptor.DataSetsToJSONObject(
  const ADataSets: TFDJSONDataSetsBase;
  const AJSONObject: TJSONObject): Boolean;
begin
  Result := ItemListToJSONObject(ADataSets.FDataSets, AJSONObject);
end;

destructor TFDJSONInterceptor.Destroy;
begin
  FFreeObjects.Free;
  inherited;
end;

function InternalCreateDelta(const AFDMemTable: TFDMemTable): TFDMemTable;
var
  LStoreItems: TFDStoreItems;
begin
  Result := TFDMemTable.Create(nil);
  Result.ResourceOptions.StoreItems := [siMeta, siDelta];
  LStoreItems := AFDMemTable.ResourceOptions.StoreItems;
  try
    AFDMemTable.ResourceOptions.StoreItems := [siMeta, siDelta];
    CopyDataSet(AFDMemTable, Result);
  finally
    AFDMemTable.ResourceOptions.StoreItems := LStoreItems;
  end;
end;

class function TFDJSONInterceptor.ItemListToJSONObject(
  const AList: TItemList; const AJSONObject: TJSONObject): Boolean;
var
  LPair: TFDJSONDataSets.TItemPair;
  LActive: Boolean;
  LJSONDataSet: TJSONValue;
  LDataSet: TFDAdaptedDataSet;
begin
  Result := False;
  if AList <> nil then
  begin
    for LPair in  TFDJSONDataSets.TItemList(AList) do
    begin
      Result := True;
      LDataSet := LPair.Value;
      LActive := LDataSet.Active;
      if not LActive then
        LDataSet.Active := True;
      try
        LJSONDataSet := DataSetToJSONValue(LDataSet);
        // Use AddPair overload that will accept blank key
        AJSONObject.AddPair(TJSONPair.Create(LPair.Key, LJSONDataSet))
      finally
        if not LActive then
          LDataSet.Active := False;
      end;
    end;
  end
end;

class function TFDJSONInterceptor.JSONObjectToDataSets(
  const AJSONObject: TJSONObject;
  const ADataSets: TFDJSONDataSetsBase): Boolean;
var
  LList: TFDJSONDataSetsBase.TItemList;
begin
  LList := TFDJSONDataSetsBase.TItemList.Create;
  Result := JSONObjectToItemList(
    AJSONObject, LList);
  Assert(ADAtaSets.FDataSets = nil);
  ADataSets.FDataSets := LList;
  ADataSets.FOwnsDataSets := True;
end;

class function TFDJSONInterceptor.JSONObjectToItemList(
  const AJSONObject: TJSONObject; const AList: TItemList): Boolean;
var
  LMemTable: TFDMemTable;
  LJSONPair: TJSONPair;
  LValues: TList<TFDAdaptedDataSet>;
  LKeys: TList<string>;
  I: Integer;
begin
  Result := False;
  LValues := nil;
  LKeys := nil;
  try
    LValues := TList<TFDAdaptedDataSet>.Create;
    LKeys := TList<string>.Create;
    for LJSONPair in  AJSONObject do
    begin
      Assert(LJSONPair.JsonValue is TJSONString);
      if LJSONPair.JsonValue is TJSONString then
      begin
        LMemTable := MemTableFromString(TJSONString(LJSONPair.JSONValue).Value);
        LValues.Add(LMemTable);
        try
          LKeys.Add(LJSONPair.JsonString.Value);
        except
          LMemTable.Free;
          raise;
        end;
      end;
    end;
    Assert(LValues.Count = LKeys.Count);
    for I := 0 to LValues.Count - 1 do
    begin
      AList.Add(TFDJSONDataSets.TItemPair.Create(LKeys[I], LValues[I]));
      LValues[I] := nil;
    end;
    Result := True;
  finally
{$IFNDEF NEXTGEN}
    for I := 0 to AList.Count - 1 do
      LValues[I].Free; // LValues[I] will be nil unless error
{$ENDIF}
    LKeys.Free;
    LValues.Free;
  end;

end;

function TFDJSONInterceptor.TypeObjectConverter(AData: TObject): TObject;
var
  LJSONResult: TJSONObject;
begin
  if (AData <> nil) and (AData is TFDJSONDataSets.TItemList) then
  begin
    LJSONResult := TJSONObject.Create;
    try
      Result := LJSONResult;
      ItemListToJSONObject(TFDJSONDataSets.TItemList(AData), LJSONResult);
    except
      LJSONResult.Free;
      raise;
    end;
  end
  else
  begin
    Assert(False);
    Result := TJSONNull.Create;
  end;
  if FFreeObjects = nil then
    FFreeObjects := TObjectList<TObject>.Create;
  FFreeObjects.Add(Result);

end;

// Creates a dataset list from json
function TFDJSONInterceptor.TypeObjectReverter(AData: TObject): TObject;
var
  LDataSetList: TFDJSONDataSets.TItemList;
begin
  LDataSetList := TFDJSONDataSets.TItemList.Create;
  Result := LDataSetList;
  try
    Assert(AData is TJSONObject);
    JSONObjectToItemList(TJSONObject(AData), LDataSetList);
  finally
    if FFreeObjects = nil then
      FFreeObjects := TObjectList<TObject>.Create;
    FFreeObjects.Add(AData);
  end;

end;

{ TFDJSONErrors }

procedure TFDJSONErrors.Clear;
begin
  FErrors.Clear;
end;

constructor TFDJSONErrors.Create;
begin
  FErrors := TStringList.Create;
end;

destructor TFDJSONErrors.Destroy;
begin
  FErrors.Free;
  inherited;
end;

procedure TFDJSONErrors.DoBeforeApplyUpdates(ASender: TFDMemTable);
begin
  if not Assigned(ASender.OnUpdateError) then
    ASender.OnUpdateError := UpdateError;
end;

procedure TFDJSONErrors.DoAfterApplyUpdates(ASender: TFDMemTable);
var
  E1, E2: TFDUpdateErrorEvent;
begin
  E1 := ASender.OnUpdateError;
  E2 := UpdateError;
  if @E1 = @E2 then
    ASender.OnUpdateError := nil;
end;

function TFDJSONErrors.GetCount: Integer;
begin
  Result := FErrors.Count;
end;

procedure TFDJSONErrors.UpdateError(ASender: TDataSet; AException: EFDException;
    ARow: TFDDatSRow; ARequest: TFDUpdateRequest; var AAction: TFDErrorAction);
begin
  FErrors.Add(Format('%s %s', [AException.ClassName, AException.Message]));
end;

{ TFDJSONDataSetsWriter }

class procedure TFDJSONDataSetsWriter.CreateList(const ADataList: TFDJSONDataSetsBase);
begin
  if ADataList = nil then
    raise EFDDataListError.Create(sCantWriteToNilList);
  if ADataList.FDataSets = nil then
    ADataList.FDataSets := TFDJSONDataSets.TItemList.Create;
  ADataList.FOwnsDataSets := False;
end;

procedure TFDJSONDataSetsWriter.Add(const AName: string;
  const ADataSet: TFDAdaptedDataSet);
begin
  ListAdd(FDataSetList, AName, ADataSet);
end;

procedure TFDJSONDataSetsWriter.Add(
  const ADataSet: TFDAdaptedDataSet);
begin
  ListAdd(FDataSetList, ADataSet);
end;

constructor TFDJSONDataSetsWriter.Create(const ADataSetList: TFDJSONDataSets);
begin
  FDataSetList := ADataSetList;
end;

function TFDJSONDataSetsWriter.GetDataSetList: TFDJSONDataSets;
begin
  Result := FDataSetList;
end;

function TFDJSONDataSetsWriter.GetList: TFDJSONDataSetsBase;
begin
  Result := FDataSetList;
end;

class procedure TFDJSONDataSetsWriter.ListAdd(const ADataList: TFDJSONDataSets;
  const AName: string; const ADataSet: TFDAdaptedDataSet);
begin
  ListAddT<TFDAdaptedDataSet>(ADataList, AName, ADataSet);
end;

class procedure TFDJSONDataSetsWriter.ListAdd(const ADataList: TFDJSONDataSets;
  const ADataSet: TFDAdaptedDataSet);
begin
  ListAddT<TFDAdaptedDataSet>(ADataList, ADataSet);
end;

{ TFDJSONDataSetsReader }

constructor TFDJSONDataSetsReader.Create(const ADataSetList: TFDJSONDataSets);
begin
  FDataSetList := ADataSetList;
end;

function TFDJSONDataSetsReader.ExtractItem(I: Integer): TItemPair;
begin
  Result := ExtractListItemT<TFDADaptedDataSet>(FDataSetList, I);
end;

class function TFDJSONDataSetsReader.ExtractListItem(
  const ADataList: TFDJSONDataSets; I: Integer): TItemPair;
begin
  Result := ExtractListItemT<TFDADaptedDataSet>(ADataList, I);
end;

function TFDJSONDataSetsReader.GetCount: Integer;
begin
  Result := GetListCountT(FDataSetList);
end;

function TFDJSONDataSetsReader.GetDataSetList: TFDJSONDataSets;
begin
  Result := FDataSetList;
end;

function TFDJSONDataSetsReader.GetItem(I: Integer): TItemPair;
begin
  Result := GetListItemT<TFDADaptedDataSet>(FDataSetList, I);
end;

function TFDJSONDataSetsReader.GetKey(I: Integer): string;
begin
  Result := GetListKeyT<TFDADaptedDataSet>(FDataSetList, I);
end;

class function TFDJSONDataSetsReader.GetListCount(
  const ADataList: TFDJSONDataSets): Integer;
begin
  Result := GetListCountT(ADataList);
end;

class function TFDJSONDataSetsReader.GetListItem(const ADataList: TFDJSONDataSets;
  I: Integer): TItemPair;
begin
  Result := GetListItemT<TFDADaptedDataSet>(ADataList, I);
end;

class function TFDJSONDataSetsReader.GetListKey(const ADataList: TFDJSONDataSets;
  I: Integer): string;
begin
  Result := GetListKeyT<TFDADaptedDataSet>(ADataList, I);
end;

class function TFDJSONDataSetsReader.GetListReader(
  const ADataList: TFDJSONDataSets): IFDJSONDataSetsReader;
begin
  Result := TFDJSONDataSetsReader.Create(ADataList);
end;

class function TFDJSONDataSetsReader.GetListValue(
  const ADataList: TFDJSONDataSets; I: Integer): TFDAdaptedDataSet;
begin
  Result := GetListValueT<TFDADaptedDataSet>(ADataList, I);
end;

class function TFDJSONDataSetsReader.GetListValueByName(
  const ADataList: TFDJSONDataSets; const AName: string): TFDAdaptedDataSet;
begin
  Result := GetListValueByNameT<TFDADaptedDataSet>(ADataList, AName);
end;

function TFDJSONDataSetsReader.GetValue(I: Integer): TFDAdaptedDataSet;
begin
  Result := GetListValueT<TFDADaptedDataSet>(FDataSetList, I);
end;

function TFDJSONDataSetsReader.GetValueByName(
  const AName: string): TFDAdaptedDataSet;
begin
  Result := GetListValueByNameT<TFDADaptedDataSet>(FDataSetList, AName);
end;

{ TFDJSONDeltasWriter }

class procedure TFDJSONDeltasWriter.CreateList(const ADataList: TFDJSONDataSetsBase);
begin
  if ADataList = nil then
    raise EFDDataListError.Create(sCantWriteToNilList);
  if ADataList.FDataSets = nil then
    ADataList.FDataSets := TFDJSONDeltas.TItemList.Create;
  ADataList.FOwnsDataSets := True; // Free results of CreateDelta
end;

procedure TFDJSONDeltasWriter.Add(const AName: string; const AFDMemTable: TFDMemTable);
begin
  ListAddT(FDeltaList, AName, CreateDelta(AFDMemTable));
end;

procedure TFDJSONDeltasWriter.Add(const AFDMemTable: TFDMemTable);
begin
  ListAddT(FDeltaList, AFDMemTable.Name, CreateDelta(AFDMemTable));
end;

constructor TFDJSONDeltasWriter.Create(const ADeltaList: TFDJSONDeltas);
begin
  FDeltaList := ADeltaList;
end;

function TFDJSONDeltasWriter.GetDeltaList: TFDJSONDeltas;
begin
  Result := FDeltaList;
end;

class procedure TFDJSONDeltasWriter.ListAdd(const ADeltaList: TFDJSONDeltas; const AName: string;
  const AFDMemTable: TFDMemTable);
begin
  ListAddT<TFDMemTable>(ADeltaList, AName, InternalCreateDelta(AFDMemTable));
end;

class procedure TFDJSONDeltasWriter.ListAdd(const ADeltaList: TFDJSONDeltas; const AFDMemTable: TFDMemTable);
begin
  ListAddT<TFDMemTable>(ADeltaList, InternalCreateDelta(AFDMemTable));
end;

function TFDJSONDeltasWriter.CreateDelta(const AFDMemTable: TFDMemTable): TFDMemTable;
begin
  Result := InternalCreateDelta(AFDMemTable);
end;

{ TFDJSONDeltasReader }

constructor TFDJSONDeltasReader.Create(const ADeltaList: TFDJSONDeltas);
begin
  FDeltaList := ADeltaList;
end;

function TFDJSONDeltasReader.ExtractItem(I: Integer): TItemPair;
begin
  Result := ExtractListItemT<TFDMemTable>(FDeltaList, I);
end;

class function TFDJSONDeltasReader.ExtractListItem(const ADataList: TFDJSONDeltas;
  I: Integer): TItemPair;
begin
  Result := ExtractListItemT<TFDMemTable>(ADataList, I);
end;

function TFDJSONDeltasReader.GetCount: Integer;
begin
  Result := GetListCountT(FDeltaList);
end;

function TFDJSONDeltasReader.GetDeltaList: TFDJSONDeltas;
begin
  Result := FDeltaList;
end;

function TFDJSONDeltasReader.GetItem(I: Integer): TItemPair;
begin
  Result := GetListItemT<TFDMemTable>(FDeltaList, I);
end;

function TFDJSONDeltasReader.GetKey(I: Integer): string;
begin
  Result := GetListKeyT<TFDMemTable>(FDeltaList, I);
end;

class function TFDJSONDeltasReader.GetListCount(
  const ADataList: TFDJSONDeltas): Integer;
begin
  Result := GetListCountT(ADataList);
end;

class function TFDJSONDeltasReader.GetListItem(const ADataList: TFDJSONDeltas;
  I: Integer): TItemPair;
begin
  Result := GetListItemT<TFDMemTable>(ADataList, I);
end;

class function TFDJSONDeltasReader.GetListKey(const ADataList: TFDJSONDeltas;
  I: Integer): string;
begin
  Result := GetListKeyT<TFDMemTable>(ADataList, I);
end;

class function TFDJSONDeltasReader.GetListValue(const ADataList: TFDJSONDeltas;
  I: Integer): TFDMemTable;
begin
  Result := GetListValueT<TFDMemTable>(ADataList, I);
end;

class function TFDJSONDeltasReader.GetListValueByName(
  const ADataList: TFDJSONDeltas; const AName: string): TFDMemTable;
begin
  Result := GetListValueByNameT<TFDMemTable>(ADataList, AName);
end;

function TFDJSONDeltasReader.GetValue(I: Integer): TFDMemTable;
begin
  Result := GetListValueT<TFDMemTable>(FDeltaList, I);
end;

function TFDJSONDeltasReader.GetValueByName(const AName: string): TFDMemTable;
begin
  Result := GetListValueByNameT<TFDMemTable>(FDeltaList, AName);
end;

{ TFDJSONDataSetsReaderBase }

class function TFDJSONDataSetsReaderBase.ExtractListItemT<T>(
  const ADataList: TFDJSONDataSetsBase; I: Integer): TPair<string, T>;
begin
  if GetPairList(ADataList) <> nil then
  begin
    Result := TPair<string, T>.Create(GetPairList(ADataList)[I].Key, T(GetPairList(ADataList)[I].Value));
    GetPairList(ADataList).Delete(I);
  end
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

class function TFDJSONDataSetsReaderBase.GetListCountT(
  const ADataList: TFDJSONDataSetsBase): Integer;
begin
  if GetPairList(ADataList) <> nil then
    Result := GetPairList(ADataList).Count
  else
    Result := 0;
end;

class function TFDJSONDataSetsReaderBase.GetListItemT<T>(const ADataList: TFDJSONDataSetsBase;
  I: Integer): TPair<string, T>;
begin
  if GetPairList(ADataList) <> nil then
    Result :=
      TPair<string, T>.Create(GetPairList(ADataList)[I].Key, T(GetPairList(ADataList)[I].Value))
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

class function TFDJSONDataSetsReaderBase.GetListValueT<T>(const ADataList: TFDJSONDataSetsBase;
  I: Integer): T;
begin
  if GetPairList(ADataList) <> nil then
    Result := T(GetPairList(ADataList)[I].Value)
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

class function TFDJSONDataSetsReaderBase.GetPairList(
  const AList: TFDJSONDataSetsBase): TPairList;
begin
  if (AList = nil) or (AList.FDataSets = nil) then
    raise EFDDataListError.Create(sCantReadNilList);
  Result := AList.FDataSets;

end;

class function TFDJSONDataSetsReaderBase.GetListValueByNameT<T>(
  const ADataList: TFDJSONDataSetsBase; const AName: string): T;
var
  I: Integer;
begin
  Result := nil;
  if GetPairList(ADataList) <> nil then
    for I := 0 to GetPairList(ADataList).Count - 1 do
      if SameText(AName, GetPairList(ADataList)[I].Key) then
      begin
        Result := T(GetPairList(ADataList)[I].Value);
        break;
      end;
  if Result = nil then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

class function TFDJSONDataSetsReaderBase.GetListKeyT<T>(const ADataList: TFDJSONDataSetsBase;
  I: Integer): string;
begin
  if GetPairList(ADataList) <> nil then
    Result := GetPairList(ADataList)[I].Key
  else
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

function InternalApplyUpdates(const ADelta: TFDMemTable; const AAdapter: TFDTableAdapter;
  AMaxErrors: Integer; const AErrors: TFDJSONErrors): Integer;
var
  LFDMemTable: TFDMemTable;
  LFDAdapter: TFDTableAdapter;
  LStoreItems: TFDStoreItems;
begin
  Assert(AAdapter <> nil);
  LFDMemTable := TFDMemTable.Create(nil);
  LStoreItems := LFDMemTable.ResourceOptions.StoreItems;
  LFDAdapter := TFDTableAdapter.Create(nil);
  try
    if Assigned(AAdapter.SelectCommand) then
      LFDAdapter.SelectCommand := AAdapter.SelectCommand;
    if Assigned(AAdapter.InsertCommand) then
      LFDAdapter.InsertCommand := AAdapter.InsertCommand;
    if Assigned(AAdapter.UpdateCommand) then
      LFDAdapter.UpdateCommand := AAdapter.UpdateCommand;
    if Assigned(AAdapter.DeleteCommand) then
      LFDAdapter.DeleteCommand := AAdapter.DeleteCommand;

    LFDAdapter.UpdateTableName := AAdapter.UpdateTableName;
    if LFDAdapter.UpdateTableName = '' then
      LFDAdapter.UpdateTableName := LFDAdapter.SelectCommand.UpdateOptions.UpdateTableName;

    LStoreItems := LFDMemTable.ResourceOptions.StoreItems;
    LFDMemTable.ResourceOptions.StoreItems := [siMeta, siDelta];
    LFDMemTable.CachedUpdates := true;
    LFDMemTable.Adapter := LFDAdapter;
    CopyDataSet(ADelta, LFDMemTable);
    if AErrors <> nil then
      AErrors.DoBeforeApplyUpdates(LFDMemTable);
    try
      Result := LFDMemTable.ApplyUpdates(AMaxErrors);
    finally
      if AErrors <> nil then
        AErrors.DoAfterApplyUpdates(LFDMemTable);
    end;
  finally
    LFDMemTable.ResourceOptions.StoreItems := LStoreItems;
    LFDMemTable.Free;
    LFDAdapter.Free;
  end;
end;

{ TFDJSONDeltasApplyUpdates }

class function TFDJSONDeltasApplyUpdates.ApplyUpdates(const ADelta: TFDMemTable; const AAdapter: TFDTableAdapter;
  const AErrors: TFDJSONErrors): Integer;
begin
  Result := InternalApplyUpdates(ADelta, AAdapter, -1, AErrors);
end;

class function TFDJSONDeltasApplyUpdates.ApplyUpdates(const ADelta: TFDMemTable; const ACommand: TFDCustomCommand;
  const AErrors: TFDJSONErrors): Integer;
var
  LFDAdapter: TFDTableAdapter;
begin
  LFDAdapter := nil;
  try
    LFDAdapter := TFDTableAdapter.Create(nil);
    Assert(ACommand <> nil);
    LFDAdapter.SelectCommand := ACommand;
    Result := InternalApplyUpdates(ADelta, LFDAdapter, -1, AErrors);
  finally
    LFDAdapter.Free;
  end;
end;

function TFDJSONDeltasApplyUpdates.ApplyUpdates(const AKey: string; const ASelectCommand: TFDCustomCommand): Integer;
var
  LMemTable: TFDMemTable;
begin
  LMemTable := GetValueByName(AKey);
  Result := ApplyUpdates(LMemTable, ASelectCommand, FErrors);
end;

function TFDJSONDeltasApplyUpdates.ApplyUpdates(const Index: Integer; const ASelectCommand: TFDCustomCommand): Integer;
var
  LMemTable: TFDMemTable;
begin
  LMemTable := GetValue(Index);
  Result := ApplyUpdates(LMemTable, ASelectCommand, FErrors);
end;

function TFDJSONDeltasApplyUpdates.ApplyUpdates(const AKey: string; const AAdapter: TFDTableAdapter): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetValueByName(AKey);
  Result := ApplyUpdates(LDelta, AAdapter, FErrors);
end;

function TFDJSONDeltasApplyUpdates.ApplyUpdates(const Index: Integer; const AAdapter: TFDTableAdapter): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetValue(Index);
  Result := ApplyUpdates(LDelta, AAdapter, FErrors);
end;

constructor TFDJSONDeltasApplyUpdates.Create(const ADeltaList: TFDJSONDeltas);
begin
  inherited;
  FErrors := TFDJSONErrors.Create;
end;

destructor TFDJSONDeltasApplyUpdates.Destroy;
begin
  FErrors.Free;
  inherited;
end;

function TFDJSONDeltasApplyUpdates.GetErrors: TFDJSONErrors;
begin
  Result := FErrors;
end;

class function TFDJSONDeltasApplyUpdates.ListApplyUpdates(
  const ADeltaList: TFDJSONDeltas; const Index: Integer;
  const ASelectCommand: TFDCustomCommand;
  const AErrors: TFDJSONErrors): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetListValue(ADeltaList, Index);
  Result := ApplyUpdates(LDelta, ASelectCommand, AErrors);
end;

class function TFDJSONDeltasApplyUpdates.ListApplyUpdates(
  const ADeltaList: TFDJSONDeltas; const AKey: string;
  const ASelectCommand: TFDCustomCommand;
  const AErrors: TFDJSONErrors): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetListValueByName(ADeltaList, AKey);
  Result := ApplyUpdates(LDelta, ASelectCommand, AErrors);
end;

class function TFDJSONDeltasApplyUpdates.ListApplyUpdates(
  const ADeltaList: TFDJSONDeltas; const Index: Integer;
  const AAdapter: TFDTableAdapter; const AErrors: TFDJSONErrors): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetListValue(ADeltaList, Index);
  Result := ApplyUpdates(LDelta, AAdapter, AErrors);
end;

class function TFDJSONDeltasApplyUpdates.ListApplyUpdates(
  const ADeltaList: TFDJSONDeltas; const AKey: string;
  const AAdapter: TFDTableAdapter; const AErrors: TFDJSONErrors): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetListValueByName(ADeltaList, AKey);
  Result := ApplyUpdates(LDelta, AAdapter, AErrors);
end;

{ TFDJSONDeltasApplyUpdatesMax }

constructor TFDJSONDeltasApplyUpdatesMax.Create(const ADeltaList: TFDJSONDeltas);
begin
  inherited;
  FErrors := TFDJSONErrors.Create;
end;

destructor TFDJSONDeltasApplyUpdatesMax.Destroy;
begin
  FErrors.Free;
  inherited;
end;

function TFDJSONDeltasApplyUpdatesMax.GetErrors: TFDJSONErrors;
begin
  Result := FErrors;
end;

class function TFDJSONDeltasApplyUpdatesMax.ApplyUpdates(const ADelta: TFDMemTable; const AAdapter: TFDTableAdapter;
  AMaxErrors: Integer; const AErrors: TFDJSONErrors): Integer;
begin
  Result := InternalApplyUpdates(ADelta, AAdapter, AMaxErrors, AErrors);
end;

class function TFDJSONDeltasApplyUpdatesMax.ApplyUpdates(const ADelta: TFDMemTable; const ACommand: TFDCustomCommand;
  AMaxErrors: Integer; const AErrors: TFDJSONErrors): Integer;
var
  LFDAdapter: TFDTableAdapter;
begin
  LFDAdapter := nil;
  try
    LFDAdapter := TFDTableAdapter.Create(nil);
    Assert(ACommand <> nil);
    LFDAdapter.SelectCommand := ACommand;
    Result := InternalApplyUpdates(ADelta, LFDAdapter, AMaxErrors, AErrors);
  finally
    LFDAdapter.Free;
  end;
end;

function TFDJSONDeltasApplyUpdatesMax.ApplyUpdates(const AKey: string; const ASelectCommand: TFDCustomCommand;
  AMaxErrors: Integer): Integer;
var
  LMemTable: TFDMemTable;
begin
  LMemTable := GetValueByName(AKey);
  Result := ApplyUpdates(LMemTable, ASelectCommand, AMaxErrors, FErrors);
end;

function TFDJSONDeltasApplyUpdatesMax.ApplyUpdates(const Index: Integer; const ASelectCommand: TFDCustomCommand;
   AMaxErrors: Integer): Integer;
var
  LMemTable: TFDMemTable;
begin
  LMemTable := GetValue(Index);
  Result := ApplyUpdates(LMemTable, ASelectCommand, AMaxErrors, FErrors);
end;

function TFDJSONDeltasApplyUpdatesMax.ApplyUpdates(const AKey: string; const AAdapter: TFDTableAdapter;
  AMaxErrors: Integer): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetValueByName(AKey);
  Result := ApplyUpdates(LDelta, AAdapter, AMaxErrors, FErrors);
end;

function TFDJSONDeltasApplyUpdatesMax.ApplyUpdates(const Index: Integer; const AAdapter: TFDTableAdapter; AMaxErrors: Integer): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetValue(Index);
  Result := ApplyUpdates(LDelta, AAdapter, AMaxErrors, FErrors);
end;

class function TFDJSONDeltasApplyUpdatesMax.ListApplyUpdates(
  const ADeltaList: TFDJSONDeltas; const Index: Integer;
  const ASelectCommand: TFDCustomCommand;
  AMaxErrors: Integer;
  const AErrors: TFDJSONErrors): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetListValue(ADeltaList, Index);
  Result := ApplyUpdates(LDelta, ASelectCommand, AMaxErrors, AErrors);
end;

class function TFDJSONDeltasApplyUpdatesMax.ListApplyUpdates(
  const ADeltaList: TFDJSONDeltas; const AKey: string;
  const ASelectCommand: TFDCustomCommand;
  AMaxErrors: Integer;
  const AErrors: TFDJSONErrors): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetListValueByName(ADeltaList, AKey);
  Result := ApplyUpdates(LDelta, ASelectCommand, AMaxErrors, AErrors);
end;

class function TFDJSONDeltasApplyUpdatesMax.ListApplyUpdates(
  const ADeltaList: TFDJSONDeltas; const Index: Integer;
  const AAdapter: TFDTableAdapter;
  AMaxErrors: Integer;
  const AErrors: TFDJSONErrors): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetListValue(ADeltaList, Index);
  Result := ApplyUpdates(LDelta, AAdapter, AMaxErrors, AErrors);
end;

class function TFDJSONDeltasApplyUpdatesMax.ListApplyUpdates(
  const ADeltaList: TFDJSONDeltas; const AKey: string;
  const AAdapter: TFDTableAdapter;
  AMaxErrors: Integer;
  const AErrors: TFDJSONErrors): Integer;
var
  LDelta: TFDMemTable;
begin
  LDelta := GetListValueByName(ADeltaList, AKey);
  Result := ApplyUpdates(LDelta, AAdapter, AMaxErrors, AErrors);
end;


{ TFDJSONDataSetsBase }

constructor TFDJSONDataSetsBase.Create;
begin
  FOwnsDataSets := True;
end;

destructor TFDJSONDataSetsBase.Destroy;
begin
  if FOwnsDataSets and (FDataSets <> nil) then
    while FDataSets.Count > 0 do
    begin
{$IFNDEF NEXTGEN}
      FDataSets[0].Value.Free;
{$ENDIF}
      FDataSets.Delete(0);
    end;
  FDataSets.Free;
  inherited;
end;

class function TFDJSONDataSetsWriterBase.GetPairList(
  const AList: TFDJSONDataSetsBase): TPairList;
begin
  if (AList = nil) or (AList.FDataSets = nil) then
    raise EFDDataListError.Create(sCantReadNilList);
  Result := AList.FDataSets;
end;

class procedure TFDJSONDataSetsWriterBase.ListAddT<T>(
  const ADataList: TFDJSONDataSetsBase; const ADataSet: T);
begin
  ListAddT<T>(ADataList, ADataSet.Name, ADataSet);
end;

class procedure TFDJSONDataSetsWriterBase.ListAddT<T>(
  const ADataList: TFDJSONDataSetsBase; const AName: string; const ADataSet: T);
begin
  CreateList(ADataList);
  GetPairList(ADataList).Add(TPair<string, TFDAdaptedDataSet>.Create(AName,
    ADataSet));
end;

end.
