{*******************************************************}
{                                                       }
{               Delphi DataSnap Framework               }
{                                                       }
{ Copyright(c) 1995-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Datasnap.Provider;

{$T-,H+,X+}

interface

uses
  System.SysUtils, System.Variants, System.Classes, Datasnap.DBClient, Data.DB,
  Datasnap.DSIntf, Datasnap.Midas, Data.SqlTimSt, System.Types, System.Generics.Collections
{$IF DEFINED(MSWINDOWS) and NOT DEFINED(NEXTGEN)}
  , Winapi.ActiveX
{$ENDIF}
  ;

var
  InformixLob: Boolean;

type

{ EDSWriter }

  EDSWriter = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(ErrMsg: string; Status: Integer; Dummy: Integer = 0);
    property ErrorCode: Integer read FErrorCode;
  end;

{ TCustomPacketWriter }

  TCustomPacketWriter = class(TObject)
  strict private
    FIDSWriter: IDSWriter;
    procedure InternalCheck(Status: Integer);
  protected
    FBuffer: TBlobByteData;
    procedure AddAttribute(Area: TPcktAttrArea; const ParamName: string;
      const Value: OleVariant; IncludeInDelta: Boolean); virtual;
    procedure Check(Status: Integer); inline;
    procedure SetDSWriter(const Value: IDSWriter);
    property DSWriter: IDSWriter read FIDSWriter;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

{ TDataPacketWriter }

type

{ Forward declarations }


  TGetRecordOption = (grMetaData, grReset, grXML, grXMLUTF8);
  TGetRecordOptions = set of TGetRecordOption;

  TDataRequestEvent = function(Sender: TObject; Input: OleVariant): OleVariant of object;

  TProviderOption = (poFetchBlobsOnDemand, poFetchDetailsOnDemand,
    poIncFieldProps, poCascadeDeletes, poCascadeUpdates, poReadOnly,
    poAllowMultiRecordUpdates, poDisableInserts, poDisableEdits,
    poDisableDeletes, poNoReset, poAutoRefresh, poPropogateChanges,
    poAllowCommandText, poRetainServerOrder, poUseQuoteChar );
  TProviderOptions = set of TProviderOption;

  PPutFieldInfo = ^TPutFieldInfo;
  TPutFieldProc = procedure(Info: PPutFieldInfo) of object;
  TPutFieldInfo = record
    FieldNo: Integer;
    Field: TField;
    DataSet: TDataSet;
    Size: Integer;
    IsDetail: Boolean;
    Opened: Boolean;
    PutProc: TPutFieldProc;
    LocalFieldIndex: Integer;
    FieldInfos: Pointer;
  end;

  TInfoArray = array of TPutFieldInfo;

  TGetParamsEvent = procedure(DataSet: TDataSet; Params: TPacketAttributeList) of object;
  TOnValidate = procedure(const Delta: OleVariant) of object;

  TDataPacketWriter = class(TCustomPacketWriter)
  strict private
    FConstraints: Boolean;
    FPutFieldInfo: TInfoArray;
    FOptions: TProviderOptions;
    FPacketOptions: TGetRecordOptions;
    FOnGetParams: TGetParamsEvent;
  protected
    procedure AddColumn(const Info: TPutFieldInfo); virtual;
    procedure AddConstraints(DataSet: TDataSet); virtual;
    procedure AddDataSetAttributes(DataSet: TDataSet); virtual;
    procedure AddExtraFieldProps(Field: TField);
    procedure AddFieldLinks(const Info: TInfoArray); virtual;
    procedure AddIndexDefs(DataSet: TDataSet; const Info: TInfoArray); virtual;
    procedure FreeInfoRecords(var Info: TInfoArray);
    function GetFieldIdx(const FieldName: string; const Info: TInfoArray): Integer;
    function InitPutProcs(ADataSet: TDataSet; var GlobalIdx: Integer): TInfoArray;
    procedure RefreshPutProcs(ADataSet: TDataSet; var Info: TInfoArray);
    procedure PutADTField(Info: PPutFieldInfo);
    procedure PutArrayField(Info: PPutFieldInfo);
    procedure PutBlobField(Info: PPutFieldInfo);
    procedure PutDataSetField(Info: PPutFieldInfo);
    procedure PutField(Info: PPutFieldInfo);
    procedure PutStringField(Info: PPutFieldInfo);
    procedure PutWideStringField(Info: PPutFieldInfo);
    procedure PutVarBytesField(Info: PPutFieldInfo);
    procedure WriteMetaData(DataSet: TDataSet; const Info: TInfoArray;
      IsReference: Boolean = False); virtual;
    function WriteDataSet(DataSet: TDataSet; var Info: TInfoArray;
      RecsOut: Integer): Integer; virtual;
  public
    destructor Destroy; override;
    procedure GetDataPacket(DataSet: TDataSet; var RecsOut: Integer;
      out Data: OleVariant); virtual;
    procedure Reset;
    property Constraints: Boolean read FConstraints write FConstraints;
    property PacketOptions: TGetRecordOptions read FPacketOptions write FPacketOptions;
    property Options: TProviderOptions read FOptions write FOptions;
    property OnGetParams: TGetParamsEvent read FOnGetParams write FOnGetParams;
  end;

{ TPacketDataSet }

  TPacketDataSet = class(TCustomClientDataSet)
  strict private
    FOldRecBuf: TRecBuf;
    FCurRecBuf: TRecBuf;
    FCurValues: TRecBuf;
    FUseCurValues: Boolean;
    FWritingCurValues: Boolean;
    FNewValuesModified: Boolean;
    function GetStreamMetaData: Boolean;
    procedure SetStreamMetaData(Value: Boolean);
    procedure SetWritingCurValues(const Value: Boolean);
  protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
    procedure InternalClose; override;
    procedure InternalOpen; override;
    procedure InternalInitRecord(Buffer: TRecBuf); overload; override;
{$IFNDEF NEXTGEN}
    procedure InternalInitRecord(Buffer: TRecordBuffer); overload; override; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
{$IFNDEF NEXTGEN}
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; override; deprecated;
{$ENDIF !NEXTGEN}
    procedure SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); override;
    property WritingCurValues: Boolean read FWritingCurValues write SetWritingCurValues;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignCurValues(Source: TDataSet); overload;
    procedure AssignCurValues(const CurValues: Variant); overload;
    procedure CreateFromDelta(Source: TPacketDataSet);
    function HasCurValues: Boolean;
    function HasMergeConflicts: Boolean;
    procedure InitAltRecBuffers(CheckModified: Boolean = True);
    function UpdateKind: TUpdateKind;
    property NewValuesModified: Boolean read FNewValuesModified;
    property StreamMetaData: Boolean read GetStreamMetaData write SetStreamMetaData;
    property UseCurValues: Boolean read FUseCurValues write FUseCurValues;
  end;

{ TCustomProvider }

  TCustomProvider = class(TComponent)
  strict private
    FExported: Boolean;
    FOwnerData: OleVariant;
    FActiveUpdateAbortException: Exception;
    FOnDataRequest: TDataRequestEvent;
    FOnValidate: TOnValidate;
    FBeforeApplyUpdates: TRemoteEvent;
    FAfterApplyUpdates: TRemoteEvent;
    FBeforeGetRecords: TRemoteEvent;
    FAfterGetRecords: TRemoteEvent;
    FBeforeRowRequest: TRemoteEvent;
    FAfterRowRequest: TRemoteEvent;
    FBeforeExecute: TRemoteEvent;
    FAfterExecute: TRemoteEvent;
    FBeforeGetParams: TRemoteEvent;
    FAfterGetParams: TRemoteEvent;
    function GetData: OleVariant;
  protected
  { Internal Provider Methods }
    function DoApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant; virtual;
    function DoGetRecords(Count: Integer; out RecsOut: Integer; Options: Integer;
      const CommandText: OleStr; var Params, OwnerData: OleVariant): OleVariant; virtual;
    function InternalGetRecords(Count: Integer; out RecsOut: Integer;
      Options: TGetRecordOptions; const CommandText: OleStr;
      var Params: OleVariant): OleVariant; virtual;
    procedure InternalExecute(const CommandText: OleStr; var Params: OleVariant); virtual;

  { Event overrides }
    procedure DoBeforeExecute(const CommandText: OleStr; var Params,
      OwnerData: OleVariant); virtual;
    procedure DoBeforeGetRecords(Count: Integer; Options: Integer;
      const CommandText: OleStr; var Params, OwnerData: OleVariant); virtual;

  { Event overrides }
    function InternalApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer): OleVariant; virtual; abstract;
    function InternalRowRequest(const Row: OleVariant; RequestType: TFetchOptions): OleVariant; virtual;
    function InternalGetParams(Types: TParamTypes = AllParamTypes): OleVariant; virtual;

  { Event overrides }
    procedure DoAfterApplyUpdates(var OwnerData: OleVariant); virtual;
    procedure DoBeforeApplyUpdates(var OwnerData: OleVariant); virtual;
    procedure DoAfterExecute(var OwnerData: OleVariant); virtual;
    procedure DoAfterGetParams(var OwnerData: OleVariant); virtual;
    procedure DoBeforeGetParams(var OwnerData: OleVariant); virtual;
    procedure DoAfterGetRecords(var OwnerData: OleVariant); virtual;
    procedure DoAfterRowRequest(var OwnerData: OleVariant); virtual;
    procedure DoBeforeRowRequest(var OwnerData: OleVariant); virtual;

  { Events }
    property OnDataRequest: TDataRequestEvent read FOnDataRequest write FOnDataRequest;
    property OnValidate: TOnValidate read FOnValidate write FOnValidate;
    property BeforeApplyUpdates: TRemoteEvent read FBeforeApplyUpdates write FBeforeApplyUpdates;
    property AfterApplyUpdates: TRemoteEvent read FAfterApplyUpdates write FAfterApplyUpdates;
    property BeforeGetRecords: TRemoteEvent read FBeforeGetRecords write FBeforeGetRecords;
    property AfterGetRecords: TRemoteEvent read FAfterGetRecords write FAfterGetRecords;
    property BeforeRowRequest: TRemoteEvent read FBeforeRowRequest write FBeforeRowRequest;
    property AfterRowRequest: TRemoteEvent read FAfterRowRequest write FAfterRowRequest;
    property BeforeExecute: TRemoteEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TRemoteEvent read FAfterExecute write FAfterExecute;
    property BeforeGetParams: TRemoteEvent read FBeforeGetParams write FBeforeGetParams;
    property AfterGetParams: TRemoteEvent read FAfterGetParams write FAfterGetParams;
  protected
  { Data member access }
    procedure SetActiveUpdateException(E: Exception);
    property ActiveUpdateException: Exception read FActiveUpdateAbortException;
    property OwnerData: OleVariant read FOwnerData write FOwnerData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer): OleVariant; overload;
    function ApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant; overload;
    function GetRecords(Count: Integer; out RecsOut: Integer;
      Options: Integer): OleVariant; overload;
    function GetRecords(Count: Integer; out RecsOut: Integer; Options: Integer;
      const CommandText: string; var Params,
      OwnerData: OleVariant): OleVariant; overload;
    function RowRequest(const Row: OleVariant; RequestType: Integer;
      var OwnerData: OleVariant): OleVariant; virtual;
    procedure Execute(const CommandText: OleStr; var Params,
      OwnerData: OleVariant); virtual;
    function GetParams(var OwnerData: OleVariant): OleVariant; virtual;
    function DataRequest(Input: OleVariant): OleVariant; virtual;

    property Data: OleVariant read GetData;
    [Default(True)]
    property Exported: Boolean read FExported write FExported default True;
  end;

const
  ResetOption: Integer = 1 shl ord(grReset);
  MetaDataOption: Integer = 1 shl ord(grMetaData);
  XMLOption: Integer = 1 shl ord(grXML);
  XMLUTF8Option: Integer = 1 shl ord(grXMLUTF8);

{ TBaseProvider }

type

  TUpdateTree = class;

  TCustomResolver = class;
  TResolverResponse = (rrSkip, rrAbort, rrMerge, rrApply, rrIgnore);
  TProviderDataEvent = procedure(Sender: TObject; DataSet: TCustomClientDataSet) of object;
  TBeforeUpdateRecordEvent = procedure(Sender: TObject; SourceDS: TDataSet;
    DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind; var Applied: Boolean) of object;
  TAfterUpdateRecordEvent = procedure(Sender: TObject; SourceDS: TDataSet;
    DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind) of object;
  TResolverErrorEvent = procedure(Sender: TObject; DataSet: TCustomClientDataSet;
    E: EUpdateError; UpdateKind: TUpdateKind; var Response: TResolverResponse) of object;
  TBeforeCommitEvent = procedure(Sender:TObject; DeltaDS: TCustomClientDataSet;
    ErrorCount, MaxErrors: Integer; const ResultVar: OleVariant) of object;


  TBaseProvider = class(TCustomProvider)
  strict private
    [Weak]FDataDS: TPacketDataSet;
    FUpdateMode: TUpdateMode;
    FResolver: TCustomResolver;
    FOnGetData: TProviderDataEvent;
    FOnUpdateData: TProviderDataEvent;
    FOnUpdateError: TResolverErrorEvent;
    FBeforeUpdateRecord: TBeforeUpdateRecordEvent;
    FAfterUpdateRecord: TAfterUpdateRecordEvent;
    FBeforeCommit: TBeforeCommitEvent;
    FProviderOptions: TProviderOptions;
  protected
    procedure CheckResolver;
    function CreateResolver: TCustomResolver; virtual;
    procedure FreeResolver;
    procedure CreateDataPacket(PacketOpts: TGetRecordOptions;
      ProvOpts: TProviderOptions; var RecsOut: Integer; var Data: OleVariant); virtual;
    procedure DoBeforeCommit(Delta: TPacketDataSet; ErrorCount, MaxErrors: Integer;
      const ReturnVar: OleVariant); virtual;
    procedure DoOnGetData(var Data: OleVariant); virtual;
    procedure DoOnUpdateData(Delta: TPacketDataSet); virtual;
    procedure DoBeforeUpdateRecord(SourceDS: TDataSet; DeltaDS: TCustomClientDataSet;
      UpdateKind: TUpdateKind; var Applied: Boolean); virtual;
    procedure DoAfterUpdateRecord(SourceDS: TDataSet; DeltaDS: TCustomClientDataSet;
      UpdateKind: TUpdateKind); virtual;
    procedure LocateRecord(Source, Delta: TDataSet); virtual;
    procedure UpdateRecord(Source, Delta: TDataSet; BlobsOnly, KeyOnly: Boolean); virtual;
    procedure FetchDetails(Source, Delta: TDataSet); virtual;
    function InternalRowRequest(const Row: OleVariant;
      RequestType: TFetchOptions): OleVariant; override;
    function InternalApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer): OleVariant; override;
    function InternalGetRecords(Count: Integer; out RecsOut: Integer;
      Options: TGetRecordOptions; const CommandText: OleStr;
      var Params: OleVariant): OleVariant; override;
    property PacketDataSet: TPacketDataSet read FDataDS write FDataDS;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Resolver: TCustomResolver read FResolver;
    property Options: TProviderOptions read FProviderOptions
      write FProviderOptions default [poUseQuoteChar];
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode default upWhereAll;
    property OnDataRequest;
    property OnGetData: TProviderDataEvent read FOnGetData write FOnGetData;
    property OnUpdateData: TProviderDataEvent read FOnUpdateData write FOnUpdateData;
    property OnUpdateError: TResolverErrorEvent read FOnUpdateError write FOnUpdateError;
    property BeforeUpdateRecord: TBeforeUpdateRecordEvent read FBeforeUpdateRecord
      write FBeforeUpdateRecord;
    property AfterUpdateRecord: TAfterUpdateRecordEvent read FAfterUpdateRecord
      write FAfterUpdateRecord;
  end;

{ TDataSetProvider }

  TGetTableNameEvent = procedure(Sender: TObject; DataSet: TDataSet; var TableName: string) of object;
  TGetDSProps = procedure(Sender: TObject; DataSet: TDataSet;
    out Properties: OleVariant) of object;

  TDataSetProvider = class(TBaseProvider)
  strict private
    [Weak]FDataSet: TDataSet;
    FDataSetOpened: Boolean;
    FDetailDataSetOpened: Boolean;
    FDSWriter: TDataPacketWriter;
    FGetDSProps: TGetDSProps;
    FParams: TParams;
    FResolveToDataSet: Boolean;
    FRecordsSent: Integer;
    FConstraints: Boolean;
    FTransactionStarted: Boolean;
    FGetTableName: TGetTableNameEvent;
    procedure SetDataSet(ADataSet: TDataSet);
    procedure SetResolveToDataSet(Value: Boolean);
  protected
    { SQL Resolver support methods }
    procedure DoGetTableName(DataSet: TDataSet; var TableName: string); virtual;
    { Event overrides }
    procedure DoBeforeGetRecords(Count: Integer; Options: Integer;
      const CommandText: OleStr; var Params, OwnerData: OleVariant); override;
    procedure DoBeforeExecute(const CommandText: OleStr; var Params,
      OwnerData: OleVariant); override;
  protected
    procedure CheckDataSet;
    procedure DoGetProviderAttributes(DataSet: TDataSet; List: TPacketAttributeList); virtual;
    function CreateResolver: TCustomResolver; override;
    procedure CreateDataPacket(PacketOpts: TGetRecordOptions;
      ProvOpts: TProviderOptions; var RecsOut: Integer; var Data: OleVariant); override;
    function GetDataSetFromDelta(ATree: TUpdateTree; Source, Delta: TDataSet;
      Mode: TUpdateMode): TDataSet; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure LocateRecord(Source, Delta: TDataSet); override;
    procedure UpdateRecord(Source, Delta: TDataSet; BlobsOnly, KeyOnly: Boolean); override;
    procedure FetchDetails(Source, Delta: TDataSet); override;
    function InternalRowRequest(const Row: OleVariant; Options: TFetchOptions): OleVariant; override;
    function InternalGetParams(Types: TParamTypes = AllParamTypes): OleVariant; override;
    procedure InternalExecute(const CommandText: OleStr; var Params: OleVariant); override;
    function InternalGetRecords(Count: Integer; out RecsOut: Integer;
      Options: TGetRecordOptions; const CommandText: OleStr;
      var Params: OleVariant): OleVariant; override;
    procedure SetCommandText(const CommandText: OleStr); virtual;
    function InternalApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer): OleVariant; override;
    function FindRecord(Source, Delta: TDataSet; UpdateMode: TUpdateMode): Boolean; virtual;
    procedure Reset;
    procedure SetParams(Values: OleVariant); virtual;
    property TransactionStarted: Boolean read FTransactionStarted write FTransactionStarted;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ActiveUpdateException;
    property OwnerData;
    property Params: TParams read FParams;
  published
    property DataSet: TDataSet read FDataSet write SetDataSet;
    [Default(True)]
    property Constraints: Boolean read FConstraints write FConstraints default True;
    [Default(False)]
    property ResolveToDataSet: Boolean read FResolveToDataSet write SetResolveToDataSet default False;
    property Exported;
    property Options;
    property UpdateMode;
    property OnDataRequest;
    property OnGetData;
    property OnUpdateData;
    property OnUpdateError;
    property AfterUpdateRecord;
    property BeforeUpdateRecord;
    property OnValidate;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property BeforeRowRequest;
    property AfterRowRequest;
    property BeforeExecute;
    property AfterExecute;
    property BeforeGetParams;
    property AfterGetParams;
    property OnGetTableName: TGetTableNameEvent read FGetTableName write FGetTableName;
    property OnGetDataSetProperties: TGetDSProps read FGetDSProps write FGetDSProps;
  end;

{ TProvider - deprecated }
{$IFNDEF NEXTGEN}
  TProvider = class(TDataSetProvider)
  end deprecated;
{$ENDIF !NEXTGEN}

{ TUpdateTree }

  TUpdateTree = class(TObject)
  strict private
    FDeltaDS: TPacketDataSet;
    FErrorDS: TPacketDataSet;
    FOpened: Boolean;
    [Weak]FSourceDS: TDataSet;
    [Weak]FParent: TUpdateTree;
    FDetails: TList<TUpdateTree>;
    FData: Pointer;
    [Weak]FResolver: TCustomResolver;
    FName: string;
    function GetDetailCount: Integer;
    function GetDetail(Index: Integer): TUpdateTree;
    function GetErrorDS: TPacketDataSet;
    function GetHasErrors: Boolean;
    function GetIsNested: Boolean;
    function GetTree(const AName: string): TUpdateTree;
  protected
    property Opened: Boolean read FOpened write FOpened;
    property DetailList: TList<TUpdateTree> read FDetails;
  public
    constructor Create(AParent: TUpdateTree; AResolver: TCustomResolver);
    destructor Destroy; override;
    procedure Clear; virtual;
    function DoUpdates: Boolean; virtual;
    procedure InitData(ASource: TDataSet); virtual;
    procedure InitDelta(ADelta: TPacketDataSet); overload; virtual;
    procedure InitDelta(const ADelta: OleVariant); overload; virtual;
    procedure InitErrorPacket(E: EUpdateError; Response: TResolverResponse); virtual;
    procedure RefreshData(Options: TFetchOptions); virtual;

    property Data: Pointer read FData write FData;
    property Delta: TPacketDataSet read FDeltaDS write FDeltaDS;
    property DetailCount: Integer read GetDetailCount;
    property Details[Index: Integer]: TUpdateTree read GetDetail;
    property ErrorDS: TPacketDataSet read GetErrorDS write FErrorDS;
    property HasErrors: Boolean read GetHasErrors;
    property IsNested: Boolean read GetIsNested;
    property Name: string read FName write FName;
    property Parent: TUpdateTree read FParent;
    property Resolver: TCustomResolver read FResolver;
    property Source: TDataSet read FSourceDS write FSourceDS;
  end;

{ TCustomResolver }

  TCustomResolver = class(TComponent)
  strict private
    [Weak]FProvider: TBaseProvider;
    FPrevResponse: TResolverResponse;
    FErrorCount: Integer;
    FMaxErrors: Integer;
    FUpdateTree: TUpdateTree;
  protected
    function HandleUpdateError(Tree: TUpdateTree; E: EUpdateError;
      var MaxErrors, ErrorCount: Integer): Boolean; virtual;
    procedure LogUpdateRecord(Tree: TUpdateTree); virtual;
    procedure LogUpdateError(Tree: TUpdateTree; E: EUpdateError;
      Response: TResolverResponse); virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
  protected
    { Abstract Methods }
    procedure DoUpdate(Tree: TUpdateTree); virtual; abstract;
    procedure DoDelete(Tree: TUpdateTree); virtual; abstract;
    procedure DoInsert(Tree: TUpdateTree); virtual; abstract;
    procedure InitializeConflictBuffer(Tree: TUpdateTree); virtual; abstract;
  public
    constructor Create(AProvider: TBaseProvider); reintroduce;
    destructor Destroy; override;
    function ApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
      out ErrorCount: Integer): OleVariant; virtual;
    procedure FreeTreeData(Tree: TUpdateTree); virtual;
    procedure InitKeyFields(Tree: TUpdateTree; ADelta: TPacketDataSet); virtual;
    procedure InitTreeData(Tree: TUpdateTree); virtual;
    procedure InternalBeforeResolve(Tree: TUpdateTree); virtual;
    function InternalUpdateRecord(Tree: TUpdateTree): Boolean; virtual;
    function RowRequest(Row: OleVariant; Options: TFetchOptions): OleVariant; virtual;
    property Provider: TBaseProvider read FProvider;
    property UpdateTree: TUpdateTree read FUpdateTree;
  end;

{ TDataSetResolver }

  TDataSetResolver = class(TCustomResolver)
  strict private
    FBookmark: TBookmark;
    FOpened: Boolean;
    function GetProvider: TDataSetProvider;
    procedure PutRecord(Tree: TUpdateTree);
  protected
    property Provider: TDataSetProvider read GetProvider;
    procedure BeginUpdate; override;
    procedure DoUpdate(Tree: TUpdateTree); override;
    procedure DoDelete(Tree: TUpdateTree); override;
    procedure DoInsert(Tree: TUpdateTree); override;
    procedure EndUpdate; override;
    procedure InitializeConflictBuffer(Tree: TUpdateTree); override;
  public
    constructor Create(AProvider: TDataSetProvider); reintroduce;
    procedure InternalBeforeResolve(Tree: TUpdateTree); override;
  end;

{ TSQLResolver }

  TSQLResolver = class(TCustomResolver)
  strict private
    FSQL: TStringList;
    FParams: TParams;
  protected
    function GetProvider: TDataSetProvider; virtual;
    procedure GenWhereSQL(Tree: TUpdateTree; SQL: TStrings; Params: TParams;
      GenUpdateMode: TUpdateMode; Alias: string); virtual;
    procedure GenInsertSQL(Tree: TUpdateTree; SQL: TStrings; Params: TParams); virtual;
    procedure GenDeleteSQL(Tree: TUpdateTree; SQL: TStrings; Params: TParams;
      Alias: string); virtual;
    procedure GenUpdateSQL(Tree: TUpdateTree; SQL: TStrings; Params: TParams;
      Alias: string); virtual;
    procedure GenSelectSQL(Tree: TUpdateTree; SQL: TStrings; Params: TParams;
      Alias: string; Mode: TUpdateMode = upWhereKeyOnly); virtual;
    function UseFieldInUpdate(Field: TField): Boolean; virtual;
    function UseFieldInWhere(Field: TField; Mode: TUpdateMode): Boolean; virtual;
    procedure InternalDoUpdate(Tree: TUpdateTree; UpdateKind: TUpdateKind); virtual;
  protected
    procedure DoExecSQL(SQL: TStringList; Params: TParams); virtual;
    procedure DoGetValues(SQL: TStringList; Params: TParams;
      DataSet: TDataSet); virtual;
    procedure DoUpdate(Tree: TUpdateTree); override;
    procedure DoDelete(Tree: TUpdateTree); override;
    procedure DoInsert(Tree: TUpdateTree); override;
    procedure InitializeConflictBuffer(Tree: TUpdateTree); override;
    property Provider: TDataSetProvider read GetProvider;
  public
    constructor Create(AProvider: TDataSetProvider); reintroduce;
    destructor Destroy; override;
    procedure FreeTreeData(Tree: TUpdateTree); override;
    procedure InitTreeData(Tree: TUpdateTree); override;
  end;

{ TLocalAppServer }

  TLocalAppServer = class(TInterfacedObject, IAppServer{$IFDEF MSWINDOWS}{$IFNDEF NEXTGEN}, ISupportErrorInfo{$ENDIF}{$ENDIF})
  strict private
    FProvider: TCustomProvider;
    FProviderCreated: Boolean;
  protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    { IAppServer }
    function AS_ApplyUpdates(const ProviderName: OleStr; Delta: OleVariant; MaxErrors: Integer;
                             out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant; safecall;
    function AS_GetRecords(const ProviderName: OleStr; Count: Integer; out RecsOut: Integer;
                           Options: Integer; const CommandText: OleStr;
                           var Params: OleVariant; var OwnerData: OleVariant): OleVariant; safecall;
    function AS_DataRequest(const ProviderName: OleStr; Data: OleVariant): OleVariant; safecall;
    function AS_GetProviderNames: OleVariant; safecall;
    function AS_GetParams(const ProviderName: OleStr; var OwnerData: OleVariant): OleVariant; safecall;
    function AS_RowRequest(const ProviderName: OleStr; Row: OleVariant; RequestType: Integer;
                           var OwnerData: OleVariant): OleVariant; safecall;
    procedure AS_Execute(const ProviderName: OleStr;  const CommandText: OleStr;
                         var Params, OwnerData: OleVariant); safecall;
    { ISupportErrorInfo }
    function InterfaceSupportsErrorInfo(const iid: TGUID): HResult; stdcall;
  public
    constructor Create(AProvider: TCustomProvider); overload;
    constructor Create(ADataset: TDataset); overload;
    destructor Destroy; override;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; override;
  end;

  IProviderContainer = interface
  ['{EEE9FFD4-752F-11D4-80DD-00C04F6BB88C}']
    procedure RegisterProvider(Prov: TCustomProvider);
    procedure UnRegisterProvider(Prov: TCustomProvider);
  end;

{ Utility functions }

function GetObjectProperty(Instance: TPersistent; const PropName: string): TObject;
function GetStringProperty(Instance: TPersistent; const PropName: string): string;
function VarArrayFromStrings(Strings: TStrings): Variant;

implementation

uses
  System.Rtti, System.VarUtils, Datasnap.MidConst, Data.DBConsts, Data.DBCommon, System.TypInfo,
  Datasnap.DataBkr, Data.FmtBcd
{$IF DEFINED(MSWINDOWS) and not DEFINED(NEXTGEN)}
  , System.Win.ComObj
{$ENDIF MSWINDOWS and not NEXTGEN}
{$IFDEF MACOS}
  , Macapi.CoreFoundation
{$ENDIF MACOS}
;

const
  DEFBUFSIZE = 8192;  { Default size for field data buffer }
  DefAlias   = 'A';
  NestAlias  = 'B';
  tagSERVERCALC = 1;

  PacketTypeMap: array [TFieldType] of Integer =
    (dsfldUNKNOWN, dsfldZSTRING, dsfldINT, dsfldINT, dsfldINT, dsfldBOOL, // 0..5
     dsfldFLOATIEEE, dsfldFLOATIEEE, dsfldBCD, dsfldDATE, dsfldTIME, // 6..10
     dsfldTIMESTAMP, dsfldBYTES, dsfldBYTES, dsfldINT, dsfldBYTES, dsfldBYTES, //11..16
     dsfldBYTES, dsfldBYTES, dsfldBYTES, dsfldBYTES, dsfldBYTES, dsfldUNKNOWN, // 17..22
     dsfldZSTRING, dsfldUNICODE, dsfldINT, dsfldADT, dsfldARRAY, dsfldEMBEDDEDTBL, // 23..28
     dsfldEMBEDDEDTBL, dsfldBYTES, dsfldBYTES, dsfldUNKNOWN, dsfldUNKNOWN, // 29..33
     dsfldUNKNOWN, dsfldZSTRING, dsfldDATETIME, dsFLDFMTBCD, //34..37
     dsfldUNICODE, dsfldBYTES, dsfldDATETIME, dsfldZSTRING, // 38..41
     dsfldUINT, dsfldINT, dsfldUINT, dsfldFLOATIEEE, dsfldUNKNOWN, dsfldUNKNOWN, dsfldUNKNOWN, //42..48
     dsfldDATETIMEOFFSET, dsfldUNKNOWN, dsfldSINGLE); // 49..51

  ExtraFieldProps: array [0..10] of string = ('Alignment', 'DisplayLabel',
    'DisplayWidth', 'Visible', 'EditMask', 'DisplayFormat', 'EditFormat',
    'MinValue', 'MaxValue', 'currency', 'DisplayValues');

{ Utility functions }

function GetObjectProperty(Instance: TPersistent; const PropName: string): TObject;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  PropInfo := System.TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo <> nil) and (PropInfo.PropType^.Kind = tkClass) then
    Result := TObject(GetOrdProp(Instance, PropInfo));
end;

function GetStringProperty(Instance: TPersistent; const PropName: string): string;
var
  PropInfo: PPropInfo;
begin
  Result := '';
  PropInfo := System.TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo <> nil) then
  begin
    if (PropInfo.PropType^.Kind in [tkString, tkLString]) then
      Result := GetStrProp(Instance, PropInfo)
{$IFNDEF NEXTGEN}
    else if (PropInfo.PropType^.Kind = tkWString) then
      Result := GetWideStrProp(Instance, PropInfo);
{$ENDIF !NEXTGEN}
  end;
end;

function VarArrayFromStrings(Strings: TStrings): Variant;
var
  I: Integer;
begin
  Result := Null;
  if Strings.Count > 0 then
  begin
{$IFNDEF NEXTGEN}
    Result := VarArrayCreate([0, Strings.Count - 1], varOleStr);
    for I := 0 to Strings.Count - 1 do Result[I] := WideString(Strings[I]);
{$ELSE}
    Result := VarArrayCreate([0, Strings.Count - 1], varUString);
    for I := 0 to Strings.Count - 1 do Result[I] := string(Strings[I]);
{$ENDIF !NEXTGEN}
  end;
end;

{ EDSWriter }

constructor EDSWriter.Create(ErrMsg: string; Status: Integer; Dummy: Integer);
begin
  FErrorCode := Status;
  inherited Create(ErrMsg);
end;

{ TCustomPacketWriter }

constructor TCustomPacketWriter.Create;
begin
  SetLength(FBuffer, DEFBUFSIZE);
  TRttiContext.KeepContext;
end;

destructor TCustomPacketWriter.Destroy;
begin
  FIDSWriter := nil;
  FBuffer := nil;
  TRttiContext.DropContext;
  inherited Destroy;
end;

procedure TCustomPacketWriter.Check(Status: Integer);
begin
  if Status <> 0 then InternalCheck(Status);
end;

procedure TCustomPacketWriter.InternalCheck(Status: Integer);
var
  ErrMsg: array of Byte;
begin
  if Status <> 0 then
  begin
    SetLength(ErrMsg, 2049);
    DSWriter.GetErrorString(Status, Pointer(@ErrMsg[0]));
    raise EDSWriter.Create(TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(ErrMsg)).Trim, Status);
  end;
end;

procedure TCustomPacketWriter.AddAttribute(Area: TPcktAttrArea; const ParamName: string;
  const Value: OleVariant; IncludeInDelta: Boolean);
const
  ParamTypeMap: array[varSmallInt..varByte] of Integer =
    ( dsfldINT, dsfldINT, dsfldFLOATIEEE, dsfldFLOATIEEE, dsfldBCD,
      dsfldFLOATIEEE, dsfldZSTRING, 0, 0, dsfldBOOL, 0, 0, 0, 0, 0, dsfldINT);
  ParamTypeSize: array[varSmallInt..varByte] of Integer =
    ( SizeOf(SmallInt), SizeOf(Integer), SizeOf(Single), SizeOf(Double),
      SizeOf(Currency), SizeOf(TDateTime), 0, 0, 0, SizeOf(WordBool), 0, 0, 0,
      0, 0, SizeOf(Byte));
var
  ParamType, ParamLen, ElemSize, ElemCount: LongWord;
  P: Pointer;
  DateRec: TDateTimeRec;
  TimeStamp: TTimeStamp;
  M: TMarshaller;
  Count: Integer;
  LValuePtr: Pointer;
begin
  if ((VarType(Value) and varTypeMask) in [varSmallInt, varInteger, varSingle,
      varDouble, varCurrency, varDate, varOleStr, varBoolean, varByte, varNull]) then
  begin
    ParamType := ParamTypeMap[VarType(Value) and varTypeMask];
    ParamLen := ParamTypeSize[VarType(Value) and varTypeMask];
    if ParamType = dsfldZSTRING then
    begin
      ParamType := (dsfldZSTRING shl dsSizeBitsLen) or dsVaryingFldType or SizeOf(Word);
      Count := Length(TEncoding.UTF8.GetBytes(string(Value)));
      ParamLen := Count + 1;
      PWord(FBuffer)^ := ParamLen;
      Inc(ParamLen, SizeOf(Word));
      if Count > (Length(FBuffer) - SizeOf(Word)) then
        Count := Length(FBuffer) - SizeOf(Word);
      LValuePtr := M.AsAnsi(string(Value)).ToPointer;
      Move(LValuePtr^, FBuffer[SizeOf(Word)], Count);
      FBuffer[ParamLen-1] := 0; //Ensure null termination
    end
    else if ParamType = dsfldTIMESTAMP then
    begin
      TimeStamp := DateTimeToTimeStamp(Value);
      DateRec.DateTime := TimeStampToMSecs(TimeStamp);
      Move(DateRec, FBuffer[0], ParamLen);
      ParamType := ParamType shl dsSizeBitsLen or SizeOf(TDateTimeRec);
    end
    else if ParamType = dsfldDATETIME then
    begin
      P := @TVarData(Value).VPointer;
      Move(P^, FBuffer[0], ParamLen);
      ParamType := (ParamType shl dsSizeBitsLen) or SizeOf(TSQLTimeStamp);
    end
    else if ParamType = dsfldDATETIMEOFFSET then
    begin
      P := @TVarData(Value).VPointer;
      Move(P^, FBuffer[0], ParamLen);
      ParamType := (ParamType shl dsSizeBitsLen) or SizeOf(TSQLTimeStampOffset);
    end
    else if VarIsArray(Value) then
    begin
      if ParamLen = 0 then
        raise EDSWriter.Create(SInvalidOptParamType, 0);
      ElemCount := VarArrayHighBound(Value, 1) + 1;
      ElemSize := ParamLen;
      if ParamType in [dsfldINT, dsfldUINT] then
        ParamType := (dsfldUINT shl dsSizeBitsLen) or dsArrayFldType or ElemSize
      else
        ParamType := (dsfldBYTES shl dsSizeBitsLen) or dsArrayFldType or ElemSize;
      PInteger(FBuffer)^ := ElemCount;
      ParamLen := ElemCount * ElemSize;
      P := VarArrayLock(Value);
      try
        Move(P^, FBuffer[SizeOf(Integer)], ParamLen);
        Inc(ParamLen, SizeOf(Integer));
      finally
        VarArrayUnlock(Value);
      end;
    end
    else
    begin
      if (VarType(Value) and varByRef) = varByRef then
        P := TVarData(Value).VPointer
      else
        P := @TVarData(Value).VPointer;
      Move(P^, FBuffer[0], ParamLen);
      ParamType := ParamType shl dsSizeBitsLen or ParamLen;
    end;
    if IncludeInDelta then
      ParamType := ParamType or dsIncInDelta;
    Check(DSWriter.AddAttribute(Area, M.AsUTF8(ParamName).ToPointer, ParamType,
      ParamLen, PByte(FBuffer)));
  end
  else
    raise EDSWriter.Create(SInvalidOptParamType, 0);
end;

procedure TCustomPacketWriter.SetDSWriter(const Value: IDSWriter);
begin
  FIDSWriter := Value;
end;

{ TDataPacketWriter }

destructor TDataPacketWriter.Destroy;
begin
  FreeInfoRecords(FPutFieldInfo);
  FPutFieldInfo := nil;
  inherited Destroy;
end;

procedure TDataPacketWriter.FreeInfoRecords(var Info: TInfoArray);
var
  i: Integer;
begin
  for i := 0 to High(Info) do
    if Info[i].FieldInfos <> nil then
    begin
      FreeInfoRecords(TInfoArray(Info[i].FieldInfos));
      TInfoArray(Info[i].FieldInfos) := nil;
    end;
end;

{ Writing data }

procedure TDataPacketWriter.PutBlobField(Info: PPutFieldInfo);
begin
  if not (poFetchBlobsOnDemand in Options) then
  begin
    Info.Size := Info.DataSet.GetBlobFieldData(Info.FieldNo, FBuffer);
    if Info.Size <> 0 then
    begin
      if Length(FBuffer) <= Info.Size then
        SetLength(FBuffer, Info.Size + 1);
      FBuffer[Info.Size] := 0;
{$IFNDEF NEXTGEN}
      if TBlobField(Info.Field).Transliterate then
        Info.Size := Info.DataSet.Translate(PAnsiChar(FBuffer), PAnsiChar(FBuffer), False);
{$ENDIF !NEXTGEN}
      Check(DSWriter.PutField(fldIsChanged, Info.Size, FBuffer))
    end else
      Check(DSWriter.PutField(fldIsNull, 0, nil));
  end else
    Check(DSWriter.PutField(fldIsChanged, dsDELAYEDBIT or 1, @Info.Size));
end;

procedure TDataPacketWriter.PutField(Info: PPutFieldInfo);
begin
  if Length(FBuffer) <= Info.Size then
    SetLength(FBuffer, Info.Size);
  if Info.DataSet.GetFieldData(Info.FieldNo, FBuffer) then
    Check(DSWriter.PutField(fldIsChanged, Info.Size, PByte(FBuffer)))
  else
    Check(DSWriter.PutField(fldIsNull, 0, nil));
end;

procedure TDataPacketWriter.PutStringField(Info: PPutFieldInfo);
begin
  if Length(FBuffer) <= Info.Size then
    SetLength(FBuffer, Info.Size + 1);
  if Info.DataSet.GetFieldData(Info.FieldNo, FBuffer) then
  begin
{$IFNDEF NEXTGEN}
    if TStringField(Info.Field).Transliterate then
      Info.Size := Info.DataSet.Translate(PAnsiChar(FBuffer), PAnsiChar(FBuffer), False)
    else
      Info.Size := StrLen(PAnsiChar(FBuffer));
{$ELSE}
    Info.Size := TMarshal.ReadStringAsAnsi(TPtrWrapper.Create(FBuffer)).Length;
{$ENDIF !NEXTGEN}
    Check(DSWriter.PutField(fldIsChanged, Info.Size, PByte(FBuffer)));
  end
  else
    Check(DSWriter.PutField(fldIsNull, 0, nil));
end;

procedure TDataPacketWriter.PutWideStringField(Info: PPutFieldInfo);
begin
  if Length(FBuffer) <= Info.Size then
    SetLength(FBuffer, (Info.Size + 1) * 2);
  if Info.DataSet.GetFieldData(Info.Field, FBuffer, False) then
  begin
    Info.Size := StrLen(PChar(FBuffer)) * 2;
    Check(DSWriter.PutField(fldIsChanged, Info.Size, PByte(FBuffer)));
  end else
    Check(DSWriter.PutField(fldIsNull, 0, nil));
end;

procedure TDataPacketWriter.PutVarBytesField(Info: PPutFieldInfo);
begin
  //Two byte length prefix
  if Length(FBuffer) < (Info.Size + 2) then
    SetLength(FBuffer, Info.Size + 2);
  if Info.DataSet.GetFieldData(Info.FieldNo, FBuffer) then
    Check(DSWriter.PutField(fldIsChanged, PWord(FBuffer)^, @FBuffer[SizeOf(Word)]))
  else
    Check(DSWriter.PutField(fldIsNull, 0, nil));
end;

procedure TDataPacketWriter.PutADTField(Info: PPutFieldInfo);
var
  I: Integer;
begin
  if Info.Field.IsNull then
    Check(DSWriter.PutField(fldIsNull, 0, nil))
  else
    Check(DSWriter.PutField(fldIsChanged, 0, nil));
  for I := 0 to High(TInfoArray(Info.FieldInfos)) do
    TInfoArray(Info.FieldInfos)[I].PutProc(@TInfoArray(Info.FieldInfos)[I]);
end;

procedure TDataPacketWriter.PutArrayField(Info: PPutFieldInfo);

  procedure RefreshInfos(Src: TField; Dest: PPutFieldInfo);
  var
    I: Integer;
  begin
    Dest.Field := Src;
    Dest.FieldNo := Src.FieldNo;
    if (Dest.FieldInfos <> nil) then { Must be an ADT }
    begin
      if not (Src is TADTField) then
        raise EDSWriter.CreateFmt(SArrayElementError,[Src.ClassName]);
      for I := 0 to (Src as TADTField).FieldCount - 1 do
        RefreshInfos((Src as TADTField).Fields[I], @TInfoArray(Dest.FieldInfos)[I]);
    end;
  end;

var
  I: Integer;
begin
  if Info.Field.IsNull then
    Check(DSWriter.PutField(fldIsNull, 0, nil))
  else
    Check(DSWriter.PutField(fldIsChanged, 0, nil));
  for I := 0 to TArrayField(Info.Field).FieldCount - 1 do
  begin
    RefreshInfos(TArrayField(Info.Field).Fields[I], @TInfoArray(Info.FieldInfos)[0]);
    TInfoArray(Info.FieldInfos)[0].PutProc(@TInfoArray(Info.FieldInfos)[0]);
  end;
end;

procedure TDataPacketWriter.PutDataSetField(Info: PPutFieldInfo);
var
  Count: Cardinal;
  DataSet: TDataSet;
begin
  if Info.Field <> nil then
  begin
    if Info.Field.IsNull then
    begin
      Check(DSWriter.PutField(fldIsNull, 0, nil));
      Exit;
    end;
    DataSet := TDataSetField(Info.Field).NestedDataSet;
  end else
    DataSet := Info.DataSet;
  if (poFetchDetailsOnDemand in Options) then
    Count := dsDELAYEDBIT else
    Count := Cardinal(-1);
  Check(DSWriter.PutField(fldIsChanged, SizeOf(Count), @Count));
  if (not (poFetchDetailsOnDemand in Options)) and (Count = Cardinal(-1)) then
  begin
    DataSet.UpdateCursorPos;
    DataSet.First;
    DataSet.BlockReadSize := MaxInt;
    try
      WriteDataSet(DataSet, TInfoArray(Info.FieldInfos), -1);
      DSWriter.EndOfNestedRows;
    finally
      DataSet.BlockReadSize := 0;
    end;
  end;
end;

function TDataPacketWriter.WriteDataSet(DataSet: TDataSet; var Info: TInfoArray;
  RecsOut: Integer): Integer;
const
  B: Byte = 0;
var
  i: Integer;
  ChildOpened: Boolean;

  function OpenCloseDetails(Info: TInfoArray; ActiveState: Boolean): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to High(Info) do
    begin
      if Info[I].IsDetail and (Info[I].DataSet.Active <> ActiveState) then
      begin
        Info[I].DataSet.Active := ActiveState;
        Info[I].Opened := ActiveState;
        Result := True;
      end;
    end;
  end;

begin
  Result := 0;
  if RecsOut = AllRecords then
    RecsOut := High(Integer);
  if lcAutomatic in DataSet.Fields.LifeCycles then
    RefreshPutProcs(DataSet, Info);
  ChildOpened := not(poFetchDetailsOnDemand in Options);
  ChildOpened := OpenCloseDetails(Info, ChildOpened) and ChildOpened;
  while (not DataSet.EOF) and (Result < RecsOut) do
  begin
    Check(DSWriter.PutField(fldIsChanged, 1, @B));
    for i := 0 to High(Info) do
      Info[i].PutProc(@Info[i]);
    Inc(Result);
    if Result < RecsOut then
      DataSet.Next;
  end;
  if ChildOpened then
    OpenCloseDetails(Info, False);
end;

{ Writing meta data }

procedure TDataPacketWriter.AddDataSetAttributes(DataSet: TDataSet);
var
  I: Integer;
  List: TPacketAttributeList;
begin
  if Assigned(FOnGetParams) then
  begin
    List := TPacketAttributeList.Create;
    try
      FOnGetParams(DataSet, List);
      for I := 0 to List.Count - 1 do
      begin
{$IFDEF NEXTGEN}
        AddAttribute(pcktAttrArea, List[I].Name, List[I].Value, List[I].IncludeInDelta);
{$ELSE}
        AddAttribute(pcktAttrArea, PPacketAttribute(List[I]).Name, PPacketAttribute(List[I]).Value, PPacketAttribute(List[I]).IncludeInDelta);
        Dispose(PPacketAttribute(List[I]));
{$ENDIF NEXTGEN}
      end;
    finally
      List.Free;
    end;
  end;
end;

function TDataPacketWriter.GetFieldIdx(const FieldName: string; const Info: TInfoArray): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Info) do
    if (Info[I].Field <> nil) and (Info[I].Field.FieldName = FieldName) then
    begin
      Result := Info[I].LocalFieldIndex;
      break;
    end;
end;

type
  TPropWriter = class(TWriter);

procedure TDataPacketWriter.AddExtraFieldProps(Field: TField);

  procedure WriteProp(Instance: TPersistent; const PropName: string;
    Writer: TPropWriter);
  var
    PropInfo: PPropInfo;
  begin
    PropInfo := System.TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
    if (PropInfo <> nil) and IsStoredProp(Instance, PropInfo) then
      Writer.WriteProperty(Instance, PropInfo);
  end;

var
  Writer: TPropWriter;
  Stream: TMemoryStream;
  i: Integer;
  Attr: Cardinal;
begin
  Stream := TMemoryStream.Create;
  try
    Writer := TPropWriter.Create(Stream, 1024);
    try
      Writer.WriteListBegin;
      for i := 0 to High(ExtraFieldProps) do
        WriteProp(Field, ExtraFieldProps[i], Writer);
      Writer.WriteListEnd;
      Writer.FlushBuffer;
      if Stream.Size > 2 then
      begin
        Attr := (dsfldBYTES shl dsSizeBitsLen) or dsArrayFldType or SizeOf(Byte) or dsIncInDelta;
        PInteger(FBuffer)^ := Stream.Size;
        Move(Stream.Memory^, FBuffer[SizeOf(Integer)], Stream.Size);
        Check(DSWriter.AddAttribute(fldAttrArea, szFIELDPROPS, Attr,
          Stream.Size + SizeOf(Integer), FBuffer));
      end;
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TDataPacketWriter.AddColumn(const Info: TPutFieldInfo);

  procedure AddFieldDesc(const FldName: string; FldType, Attributes: Integer);
  var
    FldDesc: TDSDataPacketFldDesc;
    LFldNamePtr: Pointer;
    Count: Integer;
    M: TMarshaller;
  begin
    FillChar(FldDesc, SizeOf(FldDesc), 0);
    Count := Length(TEncoding.UTF8.GetBytes(FldName));
    if Count > SizeOf(FldDesc.szFieldName) then
      Count := SizeOf(FldDesc.szFieldName);
    LFldNamePtr := M.AsUTF8(FldName).ToPointer;
    Move(LFldNamePtr^, FldDesc.szFieldName[0], Count);
    FldDesc.szFieldName[SizeOf(FldDesc.szFieldName)-1] := {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF}; //Ensure null termination
    FldDesc.iFieldType := FldType;
    FldDesc.iAttributes := Attributes;
    Check(DSWriter.AddColumnDesc(FldDesc));
  end;

  function ComputeInfoCount(Info: TInfoArray): Integer;
  var
    I: Integer;
  begin
    Result := Length(Info);
    for I := 0 to High(Info) do
      if Info[I].FieldInfos <> nil then
        Inc(Result, ComputeInfoCount(TInfoArray(Info[I].FieldInfos)));
  end;

  procedure AddMinMax(AField: TField);
  begin
    case AField.DataType of
      ftInteger, ftSmallInt, ftShortInt, ftByte:
        if (TIntegerField(AField).MinValue <> 0) or
           (TIntegerField(AField).MaxValue <> 0)  then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                           TIntegerField(AField).MinValue, False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                          TIntegerField(AField).MaxValue, False);
           end;
      TFieldType.ftSingle:
        if (TSingleField(AField).MinValue <> 0 ) or
           (TSingleField(AField).MaxValue <> 0 ) then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                           TSingleField(AField).MinValue, False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                          TSingleField(AField).MaxValue, False);
           end;
      ftCurrency, ftFloat:
        if (TFloatField(AField).MinValue <> 0 ) or
           (TFloatField(AField).MaxValue <> 0 ) then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                           TFloatField(AField).MinValue, False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                          TFloatField(AField).MaxValue, False);
           end;
      ftBCD:
        if (TBCDField(AField).MinValue <> 0 ) or
           (TIntegerField(AField).MaxValue <> 0 ) then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                           TBCDField(AField).MinValue, False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                          TBCDField(AField).MaxValue, False);
           end;
      ftFMTBcd:
        if (TFMTBcdField(AField).MaxValue <> '') or
           (TFMTBcdField(AField).MinValue <> '') then
           begin
             AddAttribute(fldAttrArea, szMINVALUE,
                    VarFMTBcdCreate(TFMTBCDField(AField).MinValue, TFMTBCDField(AField).Precision, TFMTBCDField(AField).Size), False);
             AddAttribute(fldAttrArea, szMAXVALUE,
                    VarFMTBcdCreate(TFMTBCDField(AField).MaxValue, TFMTBCDField(AField).Precision, TFMTBCDField(AField).Size), False);
           end;
    end;
  end;

var
  FldType, Prec, Attr, i, Width: Integer;
  TempStr: string;
begin
  if Info.IsDetail and (Info.Field = nil) then
  begin
    FldType := (dsfldEMBEDDEDTBL shl dsSizeBitsLen) or
      ComputeInfoCount(TInfoArray(Info.FieldInfos)) or dsPseudoFldType;
    AddFieldDesc(Info.DataSet.Name, FldType, 0);
    WriteMetaData(Info.DataSet, TInfoArray(Info.FieldInfos));
  end else
  begin
    Width := 0;
    Attr := 0;
    if Info.Field.ReadOnly or (Info.Field.FieldKind <> fkData) then Attr := Attr or fldAttrREADONLY;
    if Info.Field.Required and (Info.Field.DataType <> ftAutoInc) then Attr := Attr or fldAttrREQUIRED;
    if (pfHidden in Info.Field.ProviderFlags) then Attr := Attr or fldAttrHIDDEN or fldAttrREADONLY;
    FldType := PacketTypeMap[Info.Field.DataType];
    case Info.Field.DataType of
      ftTimeStamp:
        FldType := (FldType shl dsSizeBitsLen) or sizeof(TSQLTimeStamp);
      ftTimeStampOffset:
        FldType := (FldType shl dsSizeBitsLen) or sizeof(TSQLTimeStampOffset);
      ftString, ftFixedChar, ftVarBytes, ftGUID, ftWideString, ftFixedWideChar:
      begin
        FldType := FldType shl dsSizeBitsLen or dsVaryingFldType;
        if Info.Size < 255 then
          FldType := FldType or SizeOf(Byte) else
          FldType := FldType or SizeOf(Word);
        Width := Info.Size;
      end;
      ftBCD:
      begin
        if TBCDField(Info.Field).Precision = 0 then
          Width := 32 else
          Width := TBCDField(Info.Field).Precision;
        Prec := Width shr 1;
        Inc(Prec, Prec and 1);  { Make an even number }
        FldType := (FldType shl dsSizeBitsLen) or (Prec + 2);
      end;
      ftFMTBcd:
      begin
        if TFMTBCDField(Info.Field).Precision = 0 then
          Width := 32 else
          Width := TFMTBCDField(Info.Field).Precision;
        Prec := Width shr 1;
        Inc(Prec, Prec and 1);  { Make an even number }
        FldType := (FldType shl dsSizeBitsLen) or (Prec + 2);
      end;
      ftArray:
        FldType := (FldType shl dsSizeBitsLen) or dsPseudoFldType or
          dsCompArrayFldType or TObjectField(Info.Field).Size;
      ftADT:
        FldType := (FldType shl dsSizeBitsLen) or dsPseudoFldType or
          TObjectField(Info.Field).FieldCount;
      ftDataSet, ftReference:
        FldType := (FldType shl dsSizeBitsLen) or dsPseudoFldType or
          dsEmbeddedFldType or ComputeInfoCount(TInfoArray(Info.FieldInfos));
    else
      if Info.Field.IsBlob then
      begin
        FldType := (FldType shl dsSizeBitsLen) or dsVaryingFldType or SizeOf(Integer);
        Width := Info.Field.Size;
      end else
        FldType := (FldType shl dsSizeBitsLen) or Info.Size;
    end;
    AddFieldDesc(Info.Field.FieldName, FldType, Attr);
{$IFNDEF NEXTGEN}
    if Length(MetaDataFromUnicode(Info.Field.FieldName)) >= SizeOf(MIDASNAME) then
{$ELSE}
    if Length(TEncoding.UTF8.GetBytes(Info.Field.FieldName)) >= SizeOf(MIDASNAME) then
{$ENDIF !NEXTGEN}
      AddAttribute(fldAttrArea, szFIELDNAME, Info.Field.FieldName, True);
    if Info.Field.FieldKind <> fkData then
      AddAttribute(fldAttrArea, szSERVERCALC, True, True);
    if Info.Field.ProviderFlags <> [pfInWhere, pfInUpdate] then
      AddAttribute(fldAttrArea, szPROVFLAGS, Byte(Info.Field.ProviderFlags), True);
    if Info.Field.Origin <> '' then
      AddAttribute(fldAttrArea, szORIGIN, Info.Field.Origin, True);
    if Width > 0 then
      AddAttribute(fldAttrArea, szWIDTH, Width, False);
    if Info.Field is TBCDField then
    begin
      if TBCDField(Info.Field).Size <> 0 then
        AddAttribute(fldAttrArea, szDECIMALS, TBCDField(Info.Field).Size, False);
    end
    else if Info.Field is TFMTBCDField then
    begin
      if TFMTBCDField(Info.Field).Size <> 0 then
        AddAttribute(fldAttrArea, szDECIMALS, TFMTBCDField(Info.Field).Size, False);
    end;
    AddMinMax(Info.Field);
    case Info.Field.DataType of
      ftCurrency: TempStr := szstMONEY;
      ftAutoInc: TempStr := szstAUTOINC;
      ftVarBytes, ftBlob: TempStr := szstBINARY;
      ftMemo: TempStr := szstMEMO;
      ftWideMemo: TempStr := szstWIDEMEMO;
      ftFmtMemo: TempStr := szstFMTMEMO;
      ftParadoxOle: TempStr := szstOLEOBJ;
      ftGraphic: TempStr := szstGRAPHIC;
      ftDBaseOle: TempStr := szstDBSOLEOBJ;
      ftTypedBinary: TempStr := szstTYPEDBINARY;
      ftADT:
        if (Info.Field.ParentField <> nil) and
           (Info.Field.ParentField.DataType in [ftDataSet, ftReference]) then
          TempStr := szstADTNESTEDTABLE;
      ftReference: TempStr := szstREFNESTEDTABLE;
      ftString:
        if (Info.Field is TStringField) and (TStringField(Info.Field).FixedChar) then
          TempStr := szstFIXEDCHAR else
          TempStr := '';
      ftWideString:
        if (Info.Field is TWideStringField) and (TWideStringField(Info.Field).FixedChar) then
          TempStr := szstFIXEDCHAR else
//          TempStr := szstFIXEDWIDECHAR else
          TempStr := '';
      ftGUID: TempStr := szstGUID;
      ftOraClob: TempStr := szstHMEMO;
      ftOraBlob: TempStr := szstHBINARY;
    else
        TempStr := '';
    end;
    if TempStr <> '' then
      AddAttribute(fldAttrArea, szSUBTYPE, TempStr, False);
    if Info.Field is TObjectField then
      AddAttribute(fldAttrArea, szTYPENAME, TObjectField(Info.Field).ObjectType, False);
    if poIncFieldProps in Options then
      AddExtraFieldProps(Info.Field);
    case Info.Field.DataType of
      ftADT, ftArray: { Array will only have 1 child field }
        for i := 0 to High(TInfoArray(Info.FieldInfos)) do
          AddColumn(TInfoArray(Info.FieldInfos)[i]);
      ftDataSet, ftReference:
        if Info.Field is TDataSetField then
          WriteMetaData(TDataSetField(Info.Field).NestedDataSet, TInfoArray(Info.FieldInfos),
            Info.Field.DataType = ftReference);
    end;
  end;
end;

procedure TDataPacketWriter.AddConstraints(DataSet: TDataSet);
type
  TConstraintType = (ctField, ctRecord, ctDefault);

  procedure AddSQLExprAttr(ExprParser: TExprParser; const ExprText, ExprErrMsg,
    FieldName: string; FieldIndex: Integer; ConstraintType: TConstraintType;
    Required: Boolean);
  type
    PSQLExprInfo = ^TSQLExprInfo;
    TSQLExprInfo = {$IFDEF CPUX86}packed{$ENDIF} record
      iErrStrLen: Integer;
      iFldNum: Integer;
      bReqExpr: BYTE;
    end;
  const
    TypeStr: array[TConstraintType] of string = (szBDEDOMX, szBDERECX, szBDEDEFX);
    Attr: Integer = dsVaryingFldType or SizeOf(Integer) or (dsfldBYTES shl dsSizeBitsLen);
  var
    ErrorStr: string;
    Len, AttrSize: Integer;
    SQLExprInfo: PSQLExprInfo;
    Options: TParserOptions;
    M: TMarshaller;
  begin
    if ExprText = '' then Exit;
    if (ConstraintType <> ctDefault) and (ExprErrMsg = '') then
    begin
      if (ConstraintType = ctField) and (FieldName <> '') then
        ErrorStr := string.Format('%s %s: %s %s',[SConstraintFailed, SField, FieldName, ExprText])
      else
        ErrorStr := string.Format('%s %s',[SConstraintFailed, ExprText]);
    end
    else
      ErrorStr := ExprErrMsg;
    Len := ErrorStr.Length;
    if (Len > 0) then Inc(Len);
    SQLExprInfo := @FBuffer[SizeOf(Integer)];
    SQLExprInfo.iErrStrLen := Len;
    SQLExprInfo.iFldNum := FieldIndex;
    SQLExprInfo.bReqExpr := Ord(Required);
    Options := [poExtSyntax];
    if ConstraintType = ctDefault then Include(Options, poDefaultExpr);
    if ConstraintType = ctRecord then Include(Options, poUseOrigNames);
    if FieldName <> '' then Include(Options, poFieldNameGiven);
    ExprParser.SetExprParams(ExprText, [], Options, FieldName);
    Move(ExprParser.FilterData[0], FBuffer[SizeOf(TSQLExprInfo) + Len + SizeOf(Integer)], ExprParser.DataSize);
    AttrSize := ExprParser.DataSize + SizeOf(TSQLExprInfo) + Len;
    PInteger(FBuffer)^ := AttrSize;
    if Len > 0 then
      StrLCopy(@FBuffer[SizeOf(TSQLExprInfo) + SizeOf(Integer)], PChar(ErrorStr), Length(FBuffer) - SizeOf(TSQLExprInfo) - SizeOf(Integer) - 1);
    Check(DSWriter.AddAttribute(pcktAttrArea, M.AsAnsi(TypeStr[ConstraintType]).ToPointer, Attr, AttrSize + SizeOf(Integer), PByte(FBuffer)));
  end;

var
  I: Integer;
  ExprParser: TExprParser;
  Constraints: TCheckConstraints;
  Obj: TObject;
  ErrMsg: string;
begin
  ExprParser := TExprParser.Create(DataSet, '', [], [], '', nil, FieldTypeMap, True);
  try
    Obj := GetObjectProperty(DataSet, 'Constraints'); { Do not localize }
    if (Obj <> nil) and (Obj is TCheckConstraints) then
    begin
      Constraints := Obj as TCheckConstraints;
      try
        for I := 0 to Constraints.Count - 1 do
        begin
          AddSQLExprAttr(ExprParser, Constraints[I].ImportedConstraint, Constraints[I].ErrorMessage, '', 0,
            ctRecord, False);
          AddSQLExprAttr(ExprParser, Constraints[I].CustomConstraint, Constraints[I].ErrorMessage, '', 0,
            ctRecord, False);
        end;
      except
        if DataSet.Name <> '' then
          ErrMsg := Format('%s: %s',[DataSet.Name, SRecConstFail])
        else
          ErrMsg := SRecConstFail;
        if ExceptObject is Exception then
          raise EDSWriter.CreateFmt(ErrMsg, [Exception(ExceptObject).Message])
        else
          raise EDSWriter.CreateFmt(ErrMsg, ['']);
      end;
    end;
    for I := 0 to DataSet.FieldList.Count - 1 do
    begin
      try
        AddSQLExprAttr(ExprParser, DataSet.FieldList[I].DefaultExpression, '', DataSet.FieldList[I].FullName, I + 1,
          ctDefault, False);
      except
        if DataSet.FieldList[I].Name <> '' then
          ErrMsg := Format('%s: %s',[DataSet.FieldList[I].Name, SDefExprFail])
        else if DataSet.FieldList[I].DataSet.Name <> '' then
          ErrMsg := Format('%s.%s: %s',[DataSet.FieldList[I].DataSet.Name, DataSet.FieldList[I].FullName, SDefExprFail])
        else
          ErrMsg := Format('%s: %s', [DataSet.FieldList[I].FullName, SDefExprFail]);
        if ExceptObject is Exception then
          raise EDSWriter.CreateFmt(ErrMsg, [Exception(ExceptObject).Message])
        else
          raise EDSWriter.CreateFmt(ErrMsg, ['']);
      end;
      try
        AddSQLExprAttr(ExprParser, DataSet.FieldList[I].ImportedConstraint, DataSet.FieldList[I].ConstraintErrorMessage,
          DataSet.FieldList[I].FullName, I + 1, ctField, False);
        AddSQLExprAttr(ExprParser, DataSet.FieldList[I].CustomConstraint, DataSet.FieldList[I].ConstraintErrorMessage,
          DataSet.FieldList[I].FullName, I + 1, ctField, False);
      except
        if DataSet.FieldList[I].Name <> '' then
          ErrMsg := Format('%s: %s',[DataSet.FieldList[I].Name, SFieldConstFail])
        else if DataSet.FieldList[I].DataSet.Name <> '' then
          ErrMsg := Format('%s.%s: %s',[DataSet.FieldList[I].DataSet.Name, DataSet.FieldList[I].FullName, SFieldConstFail])
        else
          ErrMsg := Format('%s: %s', [DataSet.FieldList[I].FullName, SFieldConstFail]);
        if ExceptObject is Exception then
          raise EDSWriter.CreateFmt(ErrMsg, [Exception(ExceptObject).Message])
        else
          raise EDSWriter.CreateFmt(ErrMsg, ['']);
      end;
    end;
  finally
    ExprParser.Free;
  end;
end;

procedure TDataPacketWriter.AddIndexDefs(DataSet: TDataSet; const Info: TInfoArray);
var
  FieldList, CaseList, DescList: TList<TField>;

  function GetKeyData(Index: TIndexDef): OleVariant;
  var
    I: Integer;
    x: Integer;
  begin
    FieldList.Clear;
    CaseList.Clear;
    DescList.Clear;
    DataSet.GetFieldList(FieldList, Index.Fields);
    DataSet.GetFieldList(CaseList, Index.CaseInsFields);
    DataSet.GetFieldList(DescList, Index.DescFields);
    Result := VarArrayCreate([0, FieldList.Count - 1], varInteger);
    for I := 0 to FieldList.Count - 1 do
    begin
      x := GetFieldIdx(TField(FieldList[I]).FieldName, Info);
      if (CaseList.IndexOf(FieldList[I]) <> -1) or
         ((I = 0) and (FieldList.Count = 1) and (ixCaseInSensitive in Index.Options)) then
        x := x or dskeyCASEINSENSITIVE;
      if (DescList.IndexOf(FieldList[I]) <> -1) or
         ((I = 0) and (FieldList.Count = 1) and (ixDescending in Index.Options)) then
        x := x or dskeyDESCENDING;
      Result[I] := x;
    end;
  end;

var
  I: Integer;
  DefIdx, KeyIndex: TIndexDef;
  IndexDefs: TIndexDefs;
  KeyList: OleVariant;
  KeyFields: string;
begin
  FieldList := TList<TField>.Create;
  try
    CaseList := TList<TField>.Create;
    try
      DescList := TList<TField>.Create;
      try
        { Get the DEFAULT_ORDER }
        if not (poRetainServerOrder in Options) then
          DefIdx := (DataSet as IProviderSupportNG).PSGetDefaultOrder
        else
          DefIdx := nil;
        if Assigned(DefIdx) then
        try
          KeyList := GetKeyData(DefIdx);
          AddAttribute(pcktAttrArea, szDEFAULT_ORDER, KeyList, False);
        finally
          DefIdx.Free;
        end;
        KeyFields := (DataSet as IProviderSupportNG).PSGetKeyFields;
        IndexDefs := (DataSet as IProviderSupportNG).PSGetIndexDefs([ixUnique]);
        try
          if KeyFields <> '' then
          begin
            { PRIMARY_KEY is used to define the keyfields }
            KeyList := NULL;
            if Assigned(IndexDefs) then
            begin
              KeyIndex := IndexDefs.GetIndexForFields(KeyFields, False);
              if Assigned(KeyIndex) then
              begin
                KeyList := GetKeyData(KeyIndex);
                KeyIndex.Free;{ KeyIndex is already used, remove it from the list }
              end;
            end;
            if VarIsNull(KeyList) then
            begin
              FieldList.Clear;
              DataSet.GetFieldList(FieldList, KeyFields);
              KeyList := VarArrayCreate([0, FieldList.Count - 1], varSmallInt);
              for I := 0 to FieldList.Count - 1 do
                KeyList[I] := GetFieldIdx(TField(FieldList[I]).FieldName, Info);
            end;
            if not VarIsNull(KeyList) then
              AddAttribute(pcktAttrArea, szPRIMARY_KEY, KeyList, False);
          end;
          if Assigned(IndexDefs) then
            for I := 0 to IndexDefs.Count - 1 do
            begin
              KeyList := GetKeyData(IndexDefs[I]);
              AddAttribute(pcktAttrArea, szUNIQUE_KEY, KeyList, False);
            end;
        finally
          IndexDefs.Free;
        end;
      finally
        DescList.Free;
      end;
    finally
      CaseList.Free;
    end;
  finally
    FieldList.Free;
  end;
end;

procedure TDataPacketWriter.AddFieldLinks(const Info: TInfoArray);
var
  MasterFields, DetailFields: TList<TField>;
  i, j: Integer;
  LinkFields: Variant;
begin
  MasterFields := TList<TField>.Create;
  try
    DetailFields := TList<TField>.Create;
    try
      for i := 0 to High(Info) do
        if Info[i].IsDetail and (Info[i].Field = nil) then
        begin
          Info[i].DataSet.GetDetailLinkFields(MasterFields, DetailFields);
          if (MasterFields.Count > 0) and (MasterFields.Count <= DetailFields.Count) then
          begin
            LinkFields := VarArrayCreate([0, MasterFields.Count * 2], varSmallInt);
            LinkFields[0] := Info[i].LocalFieldIndex;
            for j := 0 to MasterFields.Count - 1 do
              LinkFields[j + 1] := GetFieldIdx(TField(MasterFields[j]).FieldName,
                Info);
            for j := 0 to MasterFields.Count - 1 do
              LinkFields[j + MasterFields.Count + 1] :=
                GetFieldIdx(TField(DetailFields[j]).FieldName, TInfoArray(Info[i].FieldInfos));
            AddAttribute(pcktAttrArea, szMD_FIELDLINKS, LinkFields, False);
          end;
        end;
    finally
      DetailFields.Free;
    end;
  finally
    MasterFields.Free;
  end;
end;

procedure TDataPacketWriter.WriteMetaData(DataSet: TDataSet; const Info: TInfoArray;
  IsReference: Boolean);
var
  i, MDOptions: Integer;
begin
  for i := 0 to High(Info) do
    AddColumn(Info[i]);
  if (poReadOnly in Options) or IsReference then
    AddAttribute(pcktAttrArea, szREADONLY, True, False);
  if (poDisableEdits in Options) then
    AddAttribute(pcktAttrArea, szDISABLE_EDITS, True, False);
  if (poDisableInserts in Options) then
    AddAttribute(pcktAttrArea, szDISABLE_INSERTS, True, False);
  if (poDisableDeletes in Options) then
    AddAttribute(pcktAttrArea, szDISABLE_DELETES, True, False);
  if (poNoReset in Options) then
    AddAttribute(pcktAttrArea, szNO_RESET_CALL, True, False);
  if Constraints then
    AddConstraints(DataSet);
  AddIndexDefs(DataSet, Info);
  AddFieldLinks(Info);
  MDOptions := 0;
  if poCascadeDeletes in Options then MDOptions := dsCASCADEDELETES;
  if poCascadeUpdates in Options then MDOptions := MDOptions or dsCASCADEUPDATES;
  if MDOptions <> 0 then
    AddAttribute(pcktAttrArea, szMD_SEMANTICS, MDOptions, True);
  AddDataSetAttributes(DataSet);
  if Info <> FPutFieldInfo then
    Check(DSWriter.AddAttribute(pcktAttrArea, nil, 0, 0, nil));
end;

procedure TDataPacketWriter.RefreshPutProcs(ADataSet: TDataSet; var Info: TInfoArray);

  procedure RefreshInfo(ADataSet: TDataSet; AField: TField; var Info: TPutFieldInfo);
  var
    J: Integer;
  begin
    Info.Field := AField;
    if AField = nil then
      Info.DataSet := ADataSet
    else
    begin
      Info.DataSet := AField.DataSet;
      if AField.DataType = ftADT then
      begin
        for J := 0 to TADTField(AField).FieldCount - 1 do
          RefreshInfo(ADataSet, TADTField(AField).Fields[J], TInfoArray(Info.FieldInfos)[J]);
      end;
    end;
  end;

var
  I: Integer;
  List: TList<TDataSet>;
begin
  List := TList<TDataSet>.Create;
  try
    ADataSet.GetDetailDataSets(List);
    for I := 0 to ADataSet.FieldCount - 1 do
      RefreshInfo(ADataSet, ADataSet.Fields[i], Info[i]);
    for I := 0 to List.Count - 1 do
      RefreshInfo(List[I], nil, Info[ADataSet.FieldCount + i]);
  finally
    List.Free;
  end;
end;

function TDataPacketWriter.InitPutProcs(ADataSet: TDataSet;
  var GlobalIdx: Integer): TInfoArray;

  procedure InitInfoStruct(var Info: TPutFieldInfo; AField: TField;
    var GlobalIdx, LocalIdx: Integer);
  begin
    FillChar(Info, SizeOf(Info), 0);
    Info.IsDetail := AField = nil;
    Info.Field := AField;
    Inc(GlobalIdx);
    Info.LocalFieldIndex := LocalIdx;
    Inc(LocalIdx);
    if Info.Field <> nil then
    begin
      Info.FieldNo := Info.Field.FieldNo;
      Info.Size := Info.Field.DataSize;
      Info.DataSet := Info.Field.DataSet;
    end;
  end;

  procedure InitFieldProc(ADataSet: TDataSet; AField: TField;
    var Info: TPutFieldInfo; var GlobalIdx, LocalIdx: Integer);
  var
    I, NestedIdx: Integer;
  begin
    InitInfoStruct(Info, AField, GlobalIdx, LocalIdx);
    if AField = nil then { Linked dataset }
    begin
      Info.Opened := not ADataSet.Active;
      if Info.Opened then ADataSet.Open;
      Info.DataSet := ADataSet;
      Info.PutProc := PutDataSetField;
      TInfoArray(Info.FieldInfos) := InitPutProcs(Info.DataSet, GlobalIdx);
    end
    else
      case Info.Field.DataType of
        ftString, ftFixedChar, ftGUID:
        begin
          Info.PutProc := PutStringField;
          Dec(Info.Size);  { Don't count the null terminator }
        end;
        ftWideString:
        begin
          Info.PutProc := PutWideStringField;
          Info.Size := AField.Size * 2;
        end;
        ftVarBytes:
        begin
          Info.PutProc := PutVarBytesField;
          Dec(Info.Size, 2); { Don't write size bytes }
        end;
        ftADT:
        begin
          Info.PutProc := PutADTField;
          SetLength(TInfoArray(Info.FieldInfos), TADTField(Info.Field).FieldCount);
          for I := 0 to TADTField(Info.Field).FieldCount - 1 do
            InitFieldProc(ADataSet, TADTField(Info.Field).Fields[I], TInfoArray(Info.FieldInfos)[I],
              GlobalIdx, LocalIdx);
        end;
        ftArray:
        begin
          Info.PutProc := PutArrayField;
          SetLength(TInfoArray(Info.FieldInfos), 1);
          NestedIdx := LocalIdx;
          InitFieldProc(ADataSet, TArrayField(Info.Field).Fields[0], TInfoArray(Info.FieldInfos)[0],
              GlobalIdx, LocalIdx);
          LocalIdx := (LocalIdx - NestedIdx) * (TArrayField(Info.Field).FieldCount - 1) + LocalIdx;
        end;
        ftDataSet, ftReference:
        begin
          Info.PutProc := PutDataSetField;
          NestedIdx := 1;
          SetLength(TInfoArray(Info.FieldInfos), TDataSetField(Info.Field).NestedDataSet.FieldCount);
          for I := 0 to TDataSetField(Info.Field).NestedDataSet.FieldCount - 1 do
            InitFieldProc(TDataSetField(Info.Field).NestedDataSet, TDataSetField(Info.Field).NestedDataSet.Fields[I],
              TInfoArray(Info.FieldInfos)[I], GlobalIdx, NestedIdx);
        end;
        ftSmallint, ftShortint, ftByte, ftInteger, ftWord, ftLongWord, ftBoolean, ftFloat,
        ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, ftAutoInc, ftLargeint,
        ftBytes, ftTimeStamp, ftTimeStampOffset, ftFMTBcd, TFieldType.ftSingle, TFieldType.ftExtended:
          Info.PutProc := PutField;
        ftBlob, ftMemo, ftWideMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle,
        ftTypedBinary, ftOraBlob, ftOraClob:
          Info.PutProc := PutBlobField;
      else
        DatabaseErrorFmt(SUnknownFieldType, [Info.Field.FieldName]);
      end;
  end;

var
  I, LocalIdx: Integer;
  List: TList<TDataSet>;
begin
  LocalIdx := 1;
  List := TList<TDataSet>.Create;
  try
    ADataSet.GetDetailDataSets(List);
    SetLength(Result, ADataSet.FieldCount + List.Count);
    for I := 0 to ADataSet.FieldCount - 1 do
      InitFieldProc(ADataSet, ADataSet.Fields[I], Result[I], GlobalIdx, LocalIdx);
    for I := 0 to List.Count - 1 do
      InitFieldProc(List[I], nil, Result[ADataSet.FieldCount + I],
        GlobalIdx, LocalIdx);
  finally
    List.Free;
  end;
end;

procedure TDataPacketWriter.GetDataPacket(DataSet: TDataSet;
  var RecsOut: Integer; out Data: OleVariant);

  procedure CheckMetaData(DataSet: TDataSet);
  var
    Idx: Integer;
    TempPacket: TDataPacket;
    Version: Integer;
    NewWriter: IDSWriter;
  begin
    Idx := 1;
    if (FPutFieldInfo = nil) or (grMetaData in PacketOptions) then
    begin
      CreateDBClientObject(CLSID_DSWriter, IDSWriter, NewWriter);
      SetDSWriter(NewWriter);
      if FPutFieldInfo <> nil then
      begin
        FreeInfoRecords(FPutFieldInfo);
        FPutFieldInfo := nil;
      end;
      FPutFieldInfo := InitPutProcs(DataSet, Idx);
      if poFetchBlobsOnDemand in Options then
        Version := PACKETVERSION_3 else
        Version := PACKETVERSION_1;
      if grXMLUTF8 in PacketOptions then
        Check(DSWriter.SetXMLMode(xmlUTF8))
      else if grXML in PacketOptions then
        Check(DSWriter.SetXMLMode(xmlON))
      else
        Check(DSWriter.SetXMLMode(0));
      Check(DSWriter.Init_Sequential(Version, Idx - 1));
      WriteMetaData(DataSet, FPutFieldInfo);
      if not (grMetaData in PacketOptions) then
      begin
        Check(DSWriter.GetDataPacket(TempPacket));
        SafeArrayCheck(SafeArrayDestroy(TempPacket));
        TempPacket := nil;
      end;
    end;
    if not (grMetaData in PacketOptions) then
      Check(DSWriter.Reset);
  end;

var
  DataPacket: TDataPacket;
begin
  CheckMetaData(DataSet);
  RecsOut := WriteDataSet(DataSet, FPutFieldInfo, RecsOut);
  Check(DSWriter.GetDataPacket(DataPacket));
  DataPacketToVariant(DataPacket, Data);
end;

procedure TDataPacketWriter.Reset;

  procedure CloseDetailDatasets(const Info: TInfoArray);
  var
    i: Integer;
  begin
    for i := 0 to High(Info) do
      if Info[i].IsDetail and (Info[i].Opened or Info[i].Dataset.Active) then
      begin
        Info[i].DataSet.Close;
        Info[i].Opened := False;
        CloseDetailDatasets(TInfoArray(Info[i].FieldInfos));
      end;
  end;

begin
  CloseDetailDatasets(FPutFieldInfo);
end;

{ TPacketDataSet }

constructor TPacketDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FetchOnDemand := False;
end;

procedure TPacketDataSet.CreateFromDelta(Source: TPacketDataSet);
var
  TempBase: IDSBase;
begin
  Source.Check(Source.DSBase.Clone(2, True, False, TempBase));
  DSBase := TempBase;
  Open;
end;

procedure TPacketDataSet.InternalInitRecord(Buffer: TRecBuf);
var
  I: Integer;
begin
  inherited InternalInitRecord(Buffer);
  { Initialize new records in the error result dataset to unchanged values }
  for I := 1 to FieldCount do
    Check(DSBase.PutBlank(Pointer(Buffer), 0, I, BLANK_NOTCHANGED));
end;

{$IFNDEF NEXTGEN}
procedure TPacketDataSet.InternalInitRecord(Buffer: TRecordBuffer);
var
  I: Integer;
begin
  inherited InternalInitRecord(Buffer);
  { Initialize new records in the error result dataset to unchanged values }
  for I := 1 to FieldCount do
    Check(DSBase.PutBlank(Pointer(Buffer), 0, I, BLANK_NOTCHANGED));
end;
{$ENDIF !NEXTGEN}

procedure TPacketDataSet.InternalOpen;
var
  MDSem: NativeUInt;
begin
  MDSem := 0;
  inherited InternalOpen;
{$IFDEF NEXTGEN}
  FOldRecBuf := AllocRecBuf;
  FCurRecBuf := AllocRecBuf;
{$ELSE}
  FOldRecBuf := NativeInt(AllocRecordBuffer);
  FCurRecBuf := NativeInt(AllocRecordBuffer);
{$ENDIF NEXTGEN}
  Check(DSBase.GetProp(dspropMD_SEMANTICS, @MDSem));
  MDSem := MDSem and mdCASCADEMOD;
  Check(DSBase.SetProp(dspropMD_SEMANTICS, MDSem));
end;

procedure TPacketDataSet.InternalClose;
begin
  inherited InternalClose;
{$IFDEF NEXTGEN}
  FreeRecBuf(FOldRecBuf);
  FreeRecBuf(FCurRecBuf);
{$ELSE}
  FreeRecordBuffer(TRecordBuffer(FOldRecBuf));
  FreeRecordBuffer(TRecordBuffer(FCurRecBuf));
{$ENDIF NEXTGEN}
end;

function TPacketDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
begin
  { When reading an OldValue, return the CurValue instead if we have one }
  if FUseCurValues and (State = dsOldValue) and HasCurValues then
  begin
    Result := inherited GetStateFieldValue(dsCurValue, Field);
    if not VarIsClear(Result) then Exit;
  end;
  Result := inherited GetStateFieldValue(State, Field);
end;

function TPacketDataSet.GetStreamMetaData: Boolean;
var
  Value: Integer;
begin
  Check(DSBase.GetProp(DSProp(dspropDONTINCLMETADATA), @Value));
  Result := Value <> 0;
end;

procedure TPacketDataSet.SetStreamMetaData(Value: Boolean);
begin
  Check(DSBase.SetProp(DSProp(dspropDONTINCLMETADATA), NativeUInt(not Value)));
end;

function TPacketDataSet.UpdateKind: TUpdateKind;
begin
  case UpdateStatus of
    usInserted: Result := ukInsert;
    usDeleted: Result := ukDelete;
  else
    Result := ukModify;
  end;
end;

procedure TPacketDataSet.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  if Event in [deDataSetScroll, deDataSetChange] then
  begin
    FNewValuesModified := False;
    FCurValues := 0;
  end;
  inherited DataEvent(Event, Info);
end;

function TPacketDataSet.HasCurValues: Boolean;
begin
  Result := FCurValues <> 0;
end;

procedure TPacketDataSet.InitAltRecBuffers(CheckModified: Boolean);
var
  No: Integer;
begin
  if UpdateStatus in [usUnmodified, usDeleted] then
    GetCurrentRecord(FOldRecBuf);
  if CheckModified and (UpdateStatus = usUnmodified) then
  begin
    No := RecNo;
    Next;
    if UpdateStatus <> usModified then
      RecNo := No;
  end;
  if UpdateStatus = usInserted then
    SetAltRecBuffers(ActiveBuffer, ActiveBuffer, FCurRecBuf) else
    SetAltRecBuffers(FOldRecBuf, ActiveBuffer, FCurRecBuf);
end;

procedure TPacketDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  { Set a flag when any of the field's NewValue properties are modified }
  if State = dsNewValue then
    FNewValuesModified := True;
  if FWritingCurValues then
    Check(DSCursor.PutField(Pointer(FCurRecBuf), Field.FieldNo, Buffer))
  else
    inherited SetFieldData(Field, Buffer);
end;

{$IFNDEF NEXTGEN}
procedure TPacketDataSet.SetFieldData(Field: TField; Buffer: Pointer);
begin
  { Set a flag when any of the field's NewValue properties are modified }
  if State = dsNewValue then
    FNewValuesModified := True;
  if FWritingCurValues then
    Check(DSCursor.PutField(Pointer(FCurRecBuf), Field.FieldNo, Buffer))
  else
    inherited SetFieldData(Field, Buffer);
end;
{$ENDIF !NEXTGEN}

procedure TPacketDataSet.SetWritingCurValues(const Value: Boolean);
begin
  if Value then
  begin
    FCurValues := FCurRecBuf;
    InitRecord(FCurValues);
  end else
    InitAltRecBuffers;
  FWritingCurValues := Value;
end;

procedure TPacketDataSet.AssignCurValues(Source: TDataSet);
var
  I: Integer;
  NewValue: Variant;
  Field, SourceField: TField;
begin
  WritingCurValues := True;
  try
    for I := 0 to FieldCount - 1 do
    begin
      Field := Fields[I];
      SourceField := Source.FindField(Field.FieldName);
      if (SourceField <> nil) and not Field.IsBlob and
         not (Field.DataType in [ftBytes, ftVarBytes]) and
         (Field.OldValue <> SourceField.Value) then
      begin
        NewValue := Field.NewValue;
        if VarIsClear(Field.NewValue) or
           (NewValue <> SourceField.Value) then
          Field.Assign(SourceField);
      end;
    end;
  finally
    WritingCurValues := False;
  end;
end;

procedure TPacketDataSet.AssignCurValues(const CurValues: Variant);
var
  I: Integer;
  Field: TField;
  CurValue: Variant;
begin
  WritingCurValues := True;
  try
    if VarIsNull(CurValues) then
      FCurValues := 0
    else
      for I := VarArrayLowBound(CurValues, 1) to VarArrayHighBound(CurValues, 1) do
      begin
        if VarIsArray(CurValues[I]) then
        begin
          CurValue := CurValues[I][1];
          Field := FieldByName(CurValues[I][0])
        end else
        begin
          CurValue := CurValues[I];
          Field := Fields[I];
        end;
        if not VarIsClear(CurValue) then
          if (Field.OldValue <> CurValue) then
            Fields[I].Value := CurValue;
      end;
  finally
    WritingCurValues := False;
  end;
end;

function TPacketDataSet.HasMergeConflicts: Boolean;
var
  I: Integer;
  CurVal, NewVal: Variant;
begin
  Result := False;
  for I := 0 to FieldCount - 1 do
  begin
    CurVal := Fields[I].CurValue;
    if VarIsClear(CurVal) then Continue;
    NewVal := Fields[I].NewValue;
    if VarIsClear(NewVal) then Continue;
    if CurVal = NewVal then Continue;
    Result := True;
    Break;
  end;
end;

procedure TPacketDataSet.SetStateFieldValue(State: TDataSetState;
  Field: TField; const Value: Variant);
begin
  if (State = dsNewValue) and VarIsClear(Value) then
    Check(DSBase.PutBlank(Pointer(ActiveBuffer), 0, Field.FieldNo, BLANK_NOTCHANGED))
  else
    inherited;
end;

{ TCustomProvider }

constructor TCustomProvider.Create(AOwner: TComponent);
var
  ProvContainer: IProviderContainer;
begin
  inherited Create(AOwner);
  FExported := True;
  if AOwner is TProviderDataModule then
    TProviderDataModule(AOwner).RegisterProvider(Self)
  else if AOwner is TCRemoteDataModule then
    TCRemoteDataModule(AOwner).RegisterProvider(Self)
  else if Assigned(AOwner) then
    if AOwner.GetInterface(IProviderContainer, ProvContainer) then
      ProvContainer.RegisterProvider(Self);
end;

destructor TCustomProvider.Destroy;
var
  ProvContainer: IProviderContainer;
begin
  if Owner is TProviderDataModule then
    TProviderDataModule(Owner).UnRegisterProvider(Self)
  else if Owner is TCRemoteDataModule then
    TCRemoteDataModule(Owner).UnRegisterProvider(Self)
  else if Assigned(Owner) then
    if Owner.GetInterface(IProviderContainer, ProvContainer) then
      ProvContainer.UnRegisterProvider(Self);
  inherited Destroy;
end;

function TCustomProvider.GetData: OleVariant;
var
  Recs: Integer;
  Options: TGetRecordOptions;
begin
  Options := [grMetaData];
  Result := GetRecords(-1, Recs, Byte(Options));
end;

procedure TCustomProvider.DoAfterApplyUpdates(var OwnerData: OleVariant);
begin
  if Assigned(FAfterApplyUpdates) then FAfterApplyUpdates(Self, OwnerData);
end;

procedure TCustomProvider.DoBeforeApplyUpdates(var OwnerData: OleVariant);
begin
  if Assigned(FBeforeApplyUpdates) then FBeforeApplyUpdates(Self, OwnerData);
end;

function TCustomProvider.ApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer): OleVariant;
var
  OwnerData: OleVariant;
begin
  Result := DoApplyUpdates(Delta, MaxErrors, ErrorCount, OwnerData);
end;

function TCustomProvider.ApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant;
begin
  Result := DoApplyUpdates(Delta, MaxErrors, ErrorCount, OwnerData);
end;

function TCustomProvider.DoApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer; var OwnerData: OleVariant): OleVariant;
begin
  SetActiveUpdateException(nil);
  try
    try
      if Assigned(FOnValidate) then
        FOnValidate(Delta);
      DoBeforeApplyUpdates(OwnerData);
      Self.OwnerData := OwnerData;
      try
        Result := InternalApplyUpdates(Delta, MaxErrors, ErrorCount);
      finally
        OwnerData := Self.OwnerData;
        Self.OwnerData := unassigned;
      end;
    except
      on E: Exception do
      begin
        SetActiveUpdateException(E);
        raise;
      end;
    end;
  finally
    try
      DoAfterApplyUpdates(OwnerData);
    finally
      SetActiveUpdateException(nil);
    end;
  end;
end;

procedure TCustomProvider.DoAfterGetRecords(var OwnerData: OleVariant);
begin
  if Assigned(FAfterGetRecords) then FAfterGetRecords(Self, OwnerData);
end;

procedure TCustomProvider.DoBeforeGetRecords(Count: Integer; Options: Integer;
  const CommandText: OleStr; var Params, OwnerData: OleVariant);
begin
  if Assigned(FBeforeGetRecords) then FBeforeGetRecords(Self, OwnerData);
end;

function TCustomProvider.GetRecords(Count: Integer; out RecsOut: Integer; Options: Integer): OleVariant;
var
  Params, OwnerData: OleVariant;
begin
  Result := DoGetRecords(Count, RecsOut, Options, '', Params, OwnerData);
end;

function TCustomProvider.GetRecords(Count: Integer; out RecsOut: Integer; Options: Integer;
  const CommandText: string; var Params, OwnerData: OleVariant): OleVariant;
begin
  Result := DoGetRecords(Count, RecsOut, Options, CommandText, Params, OwnerData);
end;

function TCustomProvider.DoGetRecords(Count: Integer; out RecsOut: Integer; Options: Integer;
  const CommandText: OleStr; var Params, OwnerData: OleVariant): OleVariant;
var
  ParamsPassedIn: Boolean;
begin
  ParamsPassedIn := VarIsArray(Params);
  DoBeforeGetRecords(Count, Options, CommandText, Params, OwnerData);
  Result := InternalGetRecords(Count, RecsOut, TGetRecordOptions(Byte(Options)),
    CommandText, Params);
  DoAfterGetRecords(OwnerData);
  { When packet records > 0 then we need to retrieve the input params
    if params are set on ClientDataset.  But when the params are
    set on underlying DataSet, we don't want to retrieve input params }
  if ParamsPassedIn then
  begin
    if Count > 0 then
      Params :=  InternalGetParams([ptOutput, ptInput, ptInputOutput, ptResult])
    else
      Params :=  InternalGetParams([ptOutput, ptInputOutput]);
  end;
end;

procedure TCustomProvider.DoAfterRowRequest(var OwnerData: OleVariant);
begin
  if Assigned(FAfterRowRequest) then FAfterRowRequest(Self, OwnerData);
end;

procedure TCustomProvider.DoBeforeRowRequest(var OwnerData: OleVariant);
begin
  if Assigned(FBeforeRowRequest) then FBeforeRowRequest(Self, OwnerData);
end;

function TCustomProvider.RowRequest(const Row: OleVariant; RequestType: Integer;
  var OwnerData: OleVariant): OleVariant;
begin
  DoBeforeRowRequest(OwnerData);
  Result := InternalRowRequest(Row, TFetchOptions(Byte(RequestType)));
  DoAfterRowRequest(OwnerData);
end;

procedure TCustomProvider.DoAfterExecute(var OwnerData: OleVariant);
begin
  if Assigned(FAfterExecute) then FAfterExecute(Self, OwnerData);
end;

procedure TCustomProvider.DoBeforeExecute(const CommandText: OleStr; var Params,
      OwnerData: OleVariant);
begin
  if Assigned(FBeforeExecute) then FBeforeExecute(Self, OwnerData);
end;

procedure TCustomProvider.Execute(const CommandText: OleStr; var Params,
      OwnerData: OleVariant);
begin
  DoBeforeExecute(CommandText, Params, OwnerData);
  InternalExecute(CommandText, Params);
  DoAfterExecute(OwnerData);
  Params := InternalGetParams([ptOutput, ptInputOutput]);
end;

procedure TCustomProvider.DoAfterGetParams(var OwnerData: OleVariant);
begin
  if Assigned(FAfterGetParams) then FAfterGetParams(Self, OwnerData);
end;

procedure TCustomProvider.DoBeforeGetParams(var OwnerData: OleVariant);
begin
  if Assigned(FBeforeGetParams) then FBeforeGetParams(Self, OwnerData);
end;

function TCustomProvider.GetParams(var OwnerData: OleVariant): OleVariant;
begin
  DoBeforeGetParams(OwnerData);
  Result := InternalGetParams;
  DoAfterGetParams(OwnerData);
end;

function TCustomProvider.InternalGetParams(Types: TParamTypes = AllParamTypes): OleVariant;
begin
  Result := NULL;
end;

procedure TCustomProvider.InternalExecute(const CommandText: OleStr; var Params: OleVariant);
begin
end;

function TCustomProvider.InternalGetRecords(Count: Integer; out RecsOut: Integer;
  Options: TGetRecordOptions; const CommandText: OleStr;
  var Params: OleVariant): OleVariant;
begin
  Result := NULL;
end;

function TCustomProvider.InternalRowRequest(const Row: OleVariant; RequestType: TFetchOptions): OleVariant;
begin
  Result := NULL;
end;

function TCustomProvider.DataRequest(Input: OleVariant): OleVariant;
begin
  if Assigned(FOnDataRequest) then
    Result := FOnDataRequest(Self, Input) else
    Result := NULL;
end;

procedure TCustomProvider.SetActiveUpdateException(E: Exception);
begin
  FActiveUpdateAbortException := E;
end;

{ TBaseProvider }

constructor TBaseProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProviderOptions := [poUseQuoteChar];
end;

destructor TBaseProvider.Destroy;
begin
  FreeAndNil(FResolver);
  inherited Destroy;
end;

procedure TBaseProvider.LocateRecord(Source, Delta: TDataSet);
begin
end;

procedure TBaseProvider.UpdateRecord(Source, Delta: TDataSet; BlobsOnly, KeyOnly: Boolean);
begin
end;

procedure TBaseProvider.FetchDetails(Source, Delta: TDataSet);
begin
end;

procedure TBaseProvider.CheckResolver;
begin
  if not Assigned(FResolver) then
    FResolver := CreateResolver;
end;

procedure TBaseProvider.FreeResolver;
begin
  FResolver.Free;
  FResolver := nil;
end;

function TBaseProvider.InternalApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer): OleVariant;
begin
  if poReadOnly in Options then DatabaseError(SReadOnlyProvider);
  CheckResolver;
  Result := Resolver.ApplyUpdates(Delta, MaxErrors, ErrorCount);
end;

function TBaseProvider.InternalRowRequest(const Row: OleVariant; RequestType: TFetchOptions): OleVariant;
begin
  CheckResolver;
  Result := Resolver.RowRequest(Row, RequestType);
end;

function TBaseProvider.InternalGetRecords(Count: Integer; out RecsOut: Integer;
  Options: TGetRecordOptions; const CommandText: OleStr;
  var Params: OleVariant): OleVariant;
begin
  if (Count = 0) or (Assigned(FOnGetData) and not Assigned(FDataDS)) then
    Include(Options, grMetaData);
  RecsOut := Count;
  CreateDataPacket(Options, Self.Options, RecsOut, Result);
  DoOnGetData(Result);
end;

procedure TBaseProvider.DoOnGetData(var Data: OleVariant);
begin
  if Assigned(OnGetData) then
  begin
    if not Assigned(FDataDS) then
      FDataDS := TPacketDataSet.Create(Self) else
      FDataDS.StreamMetaData := False;
    FDataDS.AppendData(Data, False);
    OnGetData(Self, FDataDS);
    if FDataDS.UpdatesPending then
    begin
      FDataDS.MergeChangeLog;
      Data := FDataDS.Data;
    end;
    FDataDS.EmptyDataSet;
  end;
end;

procedure TBaseProvider.DoOnUpdateData(Delta: TPacketDataSet);
begin
  if Assigned(FOnUpdateData) then
  begin
    Delta.LogChanges := False;
    FOnUpdateData(Self, Delta);
  end;
end;

function TBaseProvider.CreateResolver: TCustomResolver;
begin
  Result := nil;
end;

procedure TBaseProvider.CreateDataPacket(PacketOpts: TGetRecordOptions;
  ProvOpts: TProviderOptions; var RecsOut: Integer; var Data: OleVariant);
begin
  RecsOut := 0;
  Data := NULL;
end;

procedure TBaseProvider.DoBeforeCommit(Delta: TPacketDataSet; ErrorCount, MaxErrors: Integer;
  const ReturnVar: OleVariant);
begin
  if Assigned(FBeforeCommit) then
  begin
    Delta.LogChanges := False;
    FBeforeCommit(Self, Delta, ErrorCount, MaxErrors, ReturnVar);
  end;
end;

procedure TBaseProvider.DoAfterUpdateRecord(SourceDS: TDataSet;
  DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
begin
  if Assigned(FAfterUpdateRecord) then
    FAfterUpdateRecord(Self, SourceDS, DeltaDS, UpdateKind);
end;

procedure TBaseProvider.DoBeforeUpdateRecord(SourceDS: TDataSet;
  DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind;
  var Applied: Boolean);
begin
  if Assigned(FBeforeUpdateRecord) then
    FBeforeUpdateRecord(Self, SourceDS, DeltaDS, UpdateKind, Applied);
end;

{ TDataSetProvider }

type
  PSQLInfo = ^TSQLInfo;
  TSQLInfo = record
    IsSQLBased: Boolean;
    QuoteChar: string;
    QuotedTable: string;
    QuotedTableDot: string;
    Opened: Boolean;
    HasObjects: Boolean;
  end;

constructor TDataSetProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResolveToDataSet := False;
  UpdateMode := upWhereAll;
  FDSWriter := nil;
  FConstraints := True;
  FRecordsSent := 0;
end;

destructor TDataSetProvider.Destroy;
begin
  FreeAndNil(FDSWriter);
  if Assigned(FParams) then
    FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TDataSetProvider.LocateRecord(Source, Delta: TDataSet);
begin
  if not Source.Active then
  begin
    FDataSetOpened := True;
    Source.Open;
  end;
  if not FindRecord(Source, Delta, UpdateMode) then
    DatabaseError(SRecordChanged);
end;

function TDataSetProvider.FindRecord(Source, Delta: TDataSet;
  UpdateMode: TUpdateMode): Boolean;

  procedure GetFieldList(DataSet: TDataSet; UpdateMode: TUpdateMode; List: TList<TField>);
  var
    I: Integer;
  begin
    for I := 0 to DataSet.FieldCount - 1 do
    begin
      if (DataSet.Fields[I].DataType in [ftBytes, ftVarBytes]) or DataSet.Fields[I].IsBlob or
         (DataSet.Fields[I].DataSet.Fields[I] is TObjectField) then Continue;
      case UpdateMode of
        upWhereKeyOnly:
          if pfInKey in DataSet.Fields[I].ProviderFlags then List.Add(DataSet.Fields[I]);
        upWhereAll:
          if pfInWhere in DataSet.Fields[I].ProviderFlags then List.Add(DataSet.Fields[I]);
        upWhereChanged:
          if (pfInKey in DataSet.Fields[I].ProviderFlags) or (not VarIsClear(DataSet.Fields[I].NewValue)) then
            List.Add(DataSet.Fields[I]);
      end;
    end;
  end;

var
  I: Integer;
  KeyValues: Variant;
  Fields: string;
  FieldList: TList<TField>;
  IsDelta: LongBool;
begin
  Result := False;
  TPacketDataSet(Delta).Check(TPacketDataSet(Delta).DSBase.GetProp(dspropISDELTA, @IsDelta));
  FieldList := TList<TField>.Create;
  try
    GetFieldList(Delta, UpdateMode, FieldList);
    if FieldList.Count > 1 then
    begin
      KeyValues := VarArrayCreate([0, FieldList.Count - 1], varVariant);
      Fields := '';
      for I := 0 to FieldList.Count - 1 do
      begin
        if IsDelta then
          KeyValues[I] := FieldList[I].OldValue
        else
          KeyValues[I] := FieldList[I].Value;
        if Fields <> '' then Fields := Fields + ';';
        Fields := Fields + FieldList[I].FieldName;
      end;
      Result := Source.Locate(Fields, KeyValues, []);
    end
    else if FieldList.Count = 1 then
    begin
      if IsDelta then
        Result := Source.Locate(FieldList[0].FieldName, FieldList[0].OldValue, [])
      else
        Result := Source.Locate(FieldList[0].FieldName, FieldList[0].Value, []);
    end
    else
      DatabaseError(SNoKeySpecified);
  finally
    FieldList.Free;
  end;
end;

procedure TDataSetProvider.FetchDetails(Source, Delta: TDataSet);
var
  i: Integer;
  Field: TField;
begin
  FDetailDataSetOpened := not Source.Active;
  if FDetailDataSetOpened then Source.Open;
  Source.First;
  while not Source.EOF do
  begin
    Delta.Insert;
    for i := 0 to Delta.FieldCount - 1 do
    begin
      Field := Source.FindField(Delta.Fields[i].FieldName);
      if Field <> nil then
        Delta.Fields[i].Assign(Field);
    end;
    Delta.Post;
    Source.Next;
  end;
end;

function TDataSetProvider.GetDataSetFromDelta(ATree: TUpdateTree; Source, Delta: TDataSet; Mode: TUpdateMode): TDataSet;
var
  Alias: string;
  FSQL: TStringList;
  FParams: TParams;
begin
  Result := nil;
  FSQL := TStringList.Create;
  FParams := TParams.Create;
  try
    CheckResolver;
    if PSQLInfo(Resolver.UpdateTree.Data).HasObjects then Alias := DefAlias else Alias := '';
    TSQLResolver(Resolver).GenSelectSQL(ATree, FSQL, FParams, Alias, Mode);
    IProviderSupportNG(Source).PSExecuteStatement(FSQL.Text, FParams, Result);
    if Result.EOF then
      DatabaseError(SRecordChanged);
  finally
    FSQL.Free;
    FParams.Free;
  end;
end;

procedure TDataSetProvider.UpdateRecord(Source, Delta: TDataSet; BlobsOnly, KeyOnly: Boolean);
var
  Field: TField;
  I: Integer;
  UseUpMode: TUpdateMode;
  DS: TDataSet;
  UseFindRecord: Boolean;
begin
  if KeyOnly then
    UseUpMode := upWhereKeyOnly
  else
    UseUpMode := UpdateMode;
  UseFindRecord := ResolveToDataSet and not Source.IsUniDirectional;
  if not UseFindRecord then
    DS := TDataSetProvider(Resolver.Provider).GetDataSetFromDelta(
      Resolver.UpdateTree, Source, Delta, UseUpMode)
  else
  begin
    if not FindRecord(Source, Delta, UseUpMode) then
      DatabaseError(SRecordChanged);
    DS := Source;
  end;
  try
    Delta.Edit;
    for I := 0 to Delta.FieldCount - 1 do
    begin
      Field := DS.FindField(Delta.Fields[I].FieldName);
      if (Field <> nil) and (not (Field.Lookup or Field.Calculated)) and
         (not BlobsOnly or (Field.IsBlob and VarIsNull(Delta.Fields[I].NewValue))) then
        Delta.Fields[I].Assign(Field);
    end;
    Delta.Post;
  finally
    if not UseFindRecord then
      DS.Free;
  end;
end;

procedure TDataSetProvider.DoBeforeExecute(const CommandText: OleStr; var Params,
      OwnerData: OleVariant);
begin
  SetCommandText(CommandText);
  SetParams(Params);
  inherited DoBeforeExecute(CommandText, Params, OwnerData);
end;

procedure TDataSetProvider.InternalExecute(const CommandText: OleStr;
  var Params: OleVariant);
begin
  CheckDataSet;
  (DataSet as IProviderSupportNG).PSExecute;
end;

procedure TDataSetProvider.DoGetTableName(DataSet: TDataSet; var TableName: string);
begin
  if Assigned(OnGetTableName) then
    OnGetTableName(Self, DataSet, TableName);
end;

procedure TDataSetProvider.Reset;
begin
  CheckDataSet;
  if FDetailDataSetOpened then
  begin
    FDSWriter.Reset;
    FDetailDataSetOpened := False;
  end;
  if FDataSetOpened then
  begin
    DataSet.Close;
    FDataSetOpened := False;
  end;
  (DataSet as IProviderSupportNG).PSReset;
  if DataSet.Active then
    DataSet.First;
  FRecordsSent := 0;
end;

procedure TDataSetProvider.SetCommandText(const CommandText: OleStr);
begin
  if CommandText = '' then Exit;
  if not (poAllowCommandText in Options) then
    DatabaseError(SCannotChangeCommandText);
  CheckDataSet;

  (DataSet as IProviderSupportNG).PSSetCommandText(CommandText);
end;

procedure TDataSetProvider.SetParams(Values: OleVariant);
var
  DataSetParams: TParams;
begin
  if VarIsClear(Values) then Exit;
  CheckDataSet;
  DataSetParams := (DataSet as IProviderSupportNG).PSGetParams;
  if not Assigned(FParams) then
    FParams := TParams.Create;
  FParams.Clear;
  UnpackParams(Values, FParams);
//  TODO: Should we call PSSetParams if FParams.Count = 0?
  if not (Assigned(DataSetParams) and DataSetParams.IsEqual(FParams)) then
    (DataSet as IProviderSupportNG).PSSetParams(FParams);
end;

function TDataSetProvider.InternalGetParams(Types: TParamTypes = AllParamTypes): OleVariant;
var
  Params: TParams;
begin
  CheckDataSet;
  Params := (DataSet as IProviderSupportNG).PSGetParams;
  if (Params = nil) or (Params.Count = 0) then
    Result := NULL else
    Result := PackageParams(Params, Types);
end;

function TDataSetProvider.InternalRowRequest(const Row: OleVariant; Options: TFetchOptions): OleVariant;
var
  LocalDataSetOpened: Boolean;
begin
  CheckResolver;
  CheckDataSet;
  Resolver.UpdateTree.InitData(DataSet);
  LocalDataSetOpened := False;
  try
    if not DataSet.Active then
    begin
      DataSet.Open;
      LocalDataSetOpened := True;
    end;
    Result := inherited InternalRowRequest(Row, Options);
  finally
    Resolver.UpdateTree.InitData(nil);
    if FDetailDataSetOpened then
    begin
      if Assigned(FDSWriter) then
        FDSWriter.Reset;
      FDetailDataSetOpened := False;
    end;
    if LocalDataSetOpened then
      DataSet.Close;
  end;
end;

function TDataSetProvider.InternalApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer): OleVariant;
var
  Commit: Boolean;
begin
  CheckDataSet;
  FTransactionStarted := not (DataSet as IProviderSupportNG).PSInTransaction;
  Commit := False;
  if FTransactionStarted then
    (DataSet as IProviderSupportNG).PSStartTransaction;
  try
    CheckResolver;
    Resolver.UpdateTree.InitData(DataSet);
    try
      Result := inherited InternalApplyUpdates(Delta, MaxErrors, ErrorCount);
      Commit := (ErrorCount <= MaxErrors) or (MaxErrors = -1);
    finally
      Resolver.UpdateTree.InitData(nil);
    end;
  finally
    if FTransactionStarted then
      (DataSet as IProviderSupportNG).PSEndTransaction(Commit);
  end;
end;

procedure TDataSetProvider.SetDataSet(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
  if Assigned(FDataSet) then
    FDataSet.FreeNotification(Self);
end;

procedure TDataSetProvider.SetResolveToDataSet(Value: Boolean);
begin
  if (Value <> FResolveToDataSet) and Assigned(Resolver) then
    FreeResolver;
  FResolveToDataSet := Value;
end;

function TDataSetProvider.CreateResolver: TCustomResolver;
begin
  if ResolveToDataSet then
    Result := TDataSetResolver.Create(Self) else
    Result := TSQLResolver.Create(Self);
end;

procedure TDataSetProvider.CheckDataSet;
begin
  if not Assigned(DataSet) then DatabaseError(SMissingDataSet);
end;

procedure TDataSetProvider.DoBeforeGetRecords(Count: Integer; Options: Integer;
  const CommandText: OleStr; var Params, OwnerData: OleVariant);
begin
  if (not (grReset in TGetRecordOptions(Byte(Options)))) or (Count <> 0) then
  begin
    SetCommandText(CommandText);
    SetParams(Params);
  end;
  inherited DoBeforeGetRecords(Count, Options, CommandText, Params, OwnerData);
end;

function TDataSetProvider.InternalGetRecords(Count: Integer; out RecsOut: Integer;
  Options: TGetRecordOptions; const CommandText: OleStr; var Params: OleVariant): OleVariant;
begin
  try
    if grReset in Options then
    begin
      Reset;
      { When doing only a reset and not getting more data then exit }
      if Count = 0 then Exit;
    end;
    if not DataSet.Active then
    begin
      DataSet.Open;
      FDataSetOpened := True;
    end;
    if (Count = 0) or (grMetaData in Options) then
    begin
{$IFNDEF NEXTGEN}
      PacketDataSet.Free;
{$ENDIF !NEXTGEN}
      PacketDataSet := nil;
      FRecordsSent := 0;
    end;
    DataSet.CheckBrowseMode;
    DataSet.BlockReadSize := Count;
    try
      Result := inherited InternalGetRecords(Count, RecsOut, Options,
        CommandText, Params);
      Inc(FRecordsSent, RecsOut);
      if (RecsOut <> Count) then Reset;
    finally
      if DataSet.Active then
      begin
        DataSet.BlockReadSize := 0;
        if (Count <> 0) and (RecsOut = Count) then
          DataSet.Next;
      end;
    end;
  except
    Reset;
    raise;
  end;
end;

procedure TDataSetProvider.DoGetProviderAttributes(DataSet: TDataSet; List: TPacketAttributeList);
var
  CustParams: OleVariant;
{$IFDEF NEXTGEN}
  Attr: TPacketAttribute;
{$ELSE}
  Attr: PPacketAttribute;
{$ENDIF NEXTGEN}
  I, J: Integer;
begin
  (DataSet as IProviderSupport).PSGetAttributes(List);
  if Assigned(FGetDSProps) then
  begin
    FGetDSProps(Self, DataSet, CustParams);
    if VarIsArray(CustParams) then
    begin
      for I := VarArrayLowBound(CustParams, 1) to VarArrayHighBound(CustParams, 1) do
      begin
        if VarIsArray(CustParams[I]) and
          (VarArrayHighBound(CustParams[I], 1) - VarArrayLowBound(CustParams[I], 1) = 2) then
        begin
          J := VarArrayLowBound(CustParams[I], 1);
{$IFNDEF NEXTGEN}
          New(Attr);
{$ENDIF !NEXTGEN}
          List.Add(Attr);
          Attr.Name := CustParams[I][J];
          Attr.Value := CustParams[I][J + 1];
          Attr.IncludeInDelta := CustParams[I][J + 2];
        end;
      end;
    end;
  end;
end;

procedure TDataSetProvider.CreateDataPacket(PacketOpts: TGetRecordOptions;
  ProvOpts: TProviderOptions; var RecsOut: Integer; var Data: OleVariant);
begin
  if not Assigned(FDSWriter) then
    FDSWriter := TDataPacketWriter.Create;
  FDSWriter.Constraints := Constraints;
  FDSWriter.OnGetParams := DoGetProviderAttributes;
  FDSWriter.PacketOptions := PacketOpts;
  FDSWriter.Options := ProvOpts;
  FDSWriter.GetDataPacket(DataSet, RecsOut, Data);
end;

procedure TDataSetProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataSet <> nil) and
    (AComponent = FDataSet) then FDataSet := nil;
end;

{ TUpdateTree }

constructor TUpdateTree.Create(AParent: TUpdateTree; AResolver: TCustomResolver);
begin
  inherited Create;
  FResolver := AResolver;
  FParent := AParent;
  FDeltaDS := TPacketDataSet.Create(nil);
  FDeltaDS.ObjectView := True;
  FDeltaDS.FieldDefs.HiddenFields := True;
  FDetails := TObjectList<TUpdateTree>.Create;
  FName := '';
end;

destructor TUpdateTree.Destroy;
begin
  if Assigned(FResolver) then
    FResolver.FreeTreeData(Self);
  Clear;
  FDetails.Free;
  if not Assigned(Parent) then
    FDeltaDS.Free;
  inherited Destroy;
end;

function TUpdateTree.GetIsNested: Boolean;
begin
  Result := Assigned(Source) and Assigned(Source.DataSetField);
end;

procedure TUpdateTree.Clear;
begin
  FDetails.Clear;
  FDeltaDS.Data := NULL;
  if not Assigned(Parent) then
  begin
    FErrorDS.Free;
    FErrorDS := nil;
  end;
end;

function TUpdateTree.GetTree(const AName: string): TUpdateTree;
var
  I: Integer;
begin
  for I := 0 to DetailCount - 1 do
    if string.Compare(Details[I].Name, AName, True) = 0 then
    begin
      Result := Details[I];
      Exit;
    end;
  Result := TUpdateTree.Create(Self, FResolver);
  Result.Name := AName;
  FDetails.Add(Result);
end;

procedure TUpdateTree.InitData(ASource: TDataSet);
var
  I: Integer;
  Tree: TUpdateTree;
  List: TList<TDataSet>;
begin
  if ASource = nil then
  begin
    for I := 0 to FDetails.Count - 1 do
      FDetails[I].InitData(nil);
    if FOpened then FSourceDS.Close;
    FOpened := False;
  end else
  begin
    FSourceDS := ASource;
    FOpened := (FSourceDS.FieldCount = 0) and FSourceDS.ObjectView and
            (not FSourceDS.IsUniDirectional);
    if FOpened then FSourceDS.Open;
    if FSourceDS.ObjectView then
      for I := 0 to FSourceDS.FieldCount - 1 do
        if FSourceDS.Fields[I].DataType in [ftDataSet] then
        begin
          Tree := GetTree(FSourceDS.Fields[I].FieldName);
          Tree.InitData(TDataSetField(FSourceDS.Fields[I]).NestedDataSet);
        end;
    List := TList<TDataSet>.Create;
    try
      FSourceDS.GetDetailDataSets(List);
      for I := 0 to List.Count - 1 do
      begin
        Tree := GetTree(List[I].Name);
        Tree.InitData(List[I]);
      end;
    finally
      List.Free;
    end;
  end;
end;

type
  TPropReader = class(TReader);

procedure TUpdateTree.InitDelta(ADelta: TPacketDataSet);
var
  I: Integer;
  Attr: Variant;
  KeySet: Boolean;
  Tree: TUpdateTree;
  FieldInfo: TFieldInfo;
  P: Pointer;
  Stream: TMemoryStream;
  Reader: TPropReader;
begin
  if (FDeltaDS <> nil) and (FDeltaDS <> ADelta) then
    FDeltaDS.Free;
  FDeltaDS := ADelta;
  FDeltaDS.LogChanges := False;
  KeySet := False;
  Stream := TMemoryStream.Create;
  try
    for I := 0 to FDeltaDS.FieldCount - 1 do
    begin
      Attr := FDeltaDS.InternalGetOptionalParam(szPROVFLAGS, FDeltaDS.Fields[I].FieldNo);
       if not (VarIsNull(Attr) or VarIsClear(Attr)) then
        FDeltaDS.Fields[I].ProviderFlags := TProviderFlags(Byte(Attr));
      Attr := FDeltaDS.InternalGetOptionalParam(szORIGIN, FDeltaDS.Fields[I].FieldNo);
      if not (VarIsNull(Attr) or VarIsClear(Attr)) then
        FDeltaDS.Fields[I].Origin := Attr;
      Attr := FDeltaDS.InternalGetOptionalParam(szSERVERCALC, FDeltaDS.Fields[I].FieldNo);
      if not (VarIsClear(Attr) or VarIsNull(Attr)) and
        (VarType(Attr) = varBoolean) and Boolean(Attr) then
        FDeltaDS.Fields[I].Tag := tagSERVERCALC;
      {Setup included field properties}
      Attr := FDeltaDS.InternalGetOptionalParam(szFIELDPROPS, FDeltaDS.Fields[I].FieldNo);
      if not (VarIsNull(Attr) or VarIsClear(Attr) or not VarIsArray(Attr)) then
      begin
        Stream.Size := VarArrayHighBound(Attr, 1);
        P := VarArrayLock(Attr);
        try
          Stream.Position := 0;
          Stream.Write(P^, Stream.Size);
          Stream.Position := 0;
        finally
          VarArrayUnlock(Attr);
        end;
        Attr := NULL;
        Reader := TPropReader.Create(Stream, 1024);
        try
          Reader.ReadListBegin;
          while not Reader.EndOfList do
            Reader.ReadProperty(FDeltaDS.Fields[I]);
        finally
          Reader.Free;
        end;
      end;
      if GetFieldInfo(FDeltaDS.Fields[I].Origin, FieldInfo) then
        FDeltaDS.Fields[I].Origin := FieldInfo.OriginalFieldName else
        FDeltaDS.Fields[I].Origin := FDeltaDS.Fields[I].FieldName;
      if pfInKey in FDeltaDS.Fields[I].ProviderFlags then
        KeySet := True;
      if Delta.Fields[I].DataType = ftDataSet then
      begin
        Tree := GetTree(Delta.Fields[I].FieldName);
        Tree.InitDelta(TPacketDataSet(TDataSetField(Delta.Fields[I]).NestedDataSet));
      end;
    end;
  finally
    Stream.Free;
  end;
  FResolver.InitTreeData(Self);
  if not KeySet then
    FResolver.InitKeyFields(Self, FDeltaDS);
end;

procedure TUpdateTree.InitDelta(const ADelta: OleVariant);
begin
  if FDeltaDS.Active then Clear;
  FDeltaDS.Data := ADelta;
  InitDelta(FDeltaDS);
end;

function TUpdateTree.GetDetailCount: Integer;
begin
  Result := FDetails.Count;
end;

function TUpdateTree.GetDetail(Index: Integer): TUpdateTree;
begin
  Result := FDetails[Index];
end;

procedure TUpdateTree.RefreshData(Options: TFetchOptions);

  function NeedsUpdate(DataSet: TDataSet): Boolean;
  var
    i: Integer;
    Field: TField;
  begin
    Result := False;
    if DataSet.RecordCount = 0 then Exit;
    for i := 0 to DataSet.FieldCount - 1 do
    begin
      Field := DataSet.Fields[i];
      Result := (Field is TDataSetField) and
                (VarIsNull(Field.NewValue) or
                 NeedsUpdate(TDataSetField(Field).NestedDataSet));
      if Result then Exit;
    end;
  end;

var
  i: Integer;
  Tree: TUpdateTree;
  Field: TField;
  Updated: Boolean;
  SourceBookmark: TBookMark;
begin
  Updated := False;
  if (foRecord in Options) and (Delta.RecordCount > 0) then
  begin
    Updated := True;
    FResolver.Provider.UpdateRecord(Source, Delta, False, True);
  end;
  SourceBookmark := Source.GetBookmark;
  for i := 0 to Delta.FieldCount - 1 do
  begin
    Field := Delta.Fields[i];
    if (not Updated) and (foBlobs in Options) and Field.IsBlob and
       VarIsNull(Field.NewValue) then
    begin
      Updated := True;
      FResolver.Provider.UpdateRecord(Source, Delta, True, False);
    end;
    if (Field is TDataSetField) then
    begin
      if not Updated then
        FResolver.Provider.LocateRecord(Source, Delta);
      Tree := GetTree(Field.FieldName);
      if Assigned(Tree) then
      begin
        if not VarIsNull(Field.NewValue) then
        begin
          if Tree.Delta.RecordCount > 0 then
            Tree.RefreshData(Options);
        end else
          FResolver.Provider.FetchDetails(Tree.Source, Tree.Delta);
      end;
    end;
  end;
  Source.GotoBookmark(SourceBookmark);
end;

function TUpdateTree.DoUpdates: Boolean;
var
  i: Integer;
begin
  Result := True;
  Delta.First;
  while not Delta.EOF do
  begin
    Delta.InitAltRecBuffers(False);
    FResolver.InternalBeforeResolve(Self);
    if (Delta.UpdateStatus = usInserted) then
    begin
      Result := FResolver.InternalUpdateRecord(Self);
      if not Result then Exit;
    end;
    for i := 0 to DetailCount - 1 do
    begin
      Result := Details[i].DoUpdates;
      if not Result then Exit;
    end;
    if Delta.UpdateStatus = usUnmodified then
      Delta.InitAltRecBuffers(True);
    if (Delta.UpdateStatus = usModified) then
      Result := FResolver.InternalUpdateRecord(Self);
    if (Delta.UpdateStatus = usDeleted) then
      Result := FResolver.InternalUpdateRecord(Self);
    if not Result then Exit;
    Delta.Next;
  end;
end;

function TUpdateTree.GetErrorDS: TPacketDataSet;
var
  Field: TField;
begin
  if not Assigned(FErrorDS) then
  begin
    if not Assigned(Parent) then
    begin
      FErrorDS := TPacketDataSet.Create(nil);
      FErrorDS.ObjectView := True;
      FErrorDS.CreateFromDelta(Delta);
    end else
    begin
      Field := Parent.ErrorDS.FieldByName(Delta.DataSetField.FieldName);
      FErrorDS := (Field as TDataSetField).NestedDataSet as TPacketDataSet;
    end;
    FErrorDS.LogChanges := False;
    FErrorDS.DSBase.SetProp(DSProp(dspropAUTOINC_DISABLED), NativeUInt(True));
  end;
  Result := FErrorDS;
end;

function TUpdateTree.GetHasErrors: Boolean;
begin
  Result := Assigned(FErrorDS);
end;

procedure TUpdateTree.InitErrorPacket(E: EUpdateError; Response: TResolverResponse);
var
  TrueRecNo: Cardinal;
begin
  if Assigned(Parent) then Parent.InitErrorPacket(nil, rrSkip);
  Delta.UpdateCursorPos;
  Delta.DSCursor.GetRecordNumber(TrueRecNo);
  if not ErrorDS.Locate('ERROR_RECORDNO', Integer(TrueRecNo), []) then
    ErrorDS.Append
  else
    ErrorDS.Edit;
  if not Assigned(E) then
  begin
    if Response = rrSkip then
    begin
      ErrorDS.SetFields([TrueRecNo]);
      ErrorDS.Post;
    end
    else
      ErrorDS.SetFields([TrueRecNo, 0, '', '', 0, 0]);
  end
  else
    ErrorDS.SetFields([TrueRecNo, Ord(Response)+1, E.Message, '', 1, Variant(Cardinal(E.ErrorCode))]);
end;

{ TCustomResolver }

constructor TCustomResolver.Create(AProvider: TBaseProvider);
begin
  FProvider := AProvider;
  FUpdateTree := TUpdateTree.Create(nil, Self);
end;

destructor TCustomResolver.Destroy;
begin
  FreeAndNil(FUpdateTree);
  inherited Destroy;
end;

{ Updates }

procedure TCustomResolver.BeginUpdate;
begin
end;

procedure TCustomResolver.EndUpdate;
begin
end;

procedure TCustomResolver.InitKeyFields(Tree: TUpdateTree; ADelta: TPacketDataSet);
var
  I, Pos: Integer;
  FieldName, KeyFields: string;
begin
  KeyFields := (Tree.Source as IProviderSupportNG).PSGetKeyFields;
  Pos := 1;
  while Pos <= KeyFields.Length do
  begin
    FieldName := ExtractFieldName(KeyFields, Pos);
    for I := 0 to ADelta.FieldCount - 1 do
      if SameText(FieldName, ADelta.Fields[I].Origin) then
      begin
        ADelta.Fields[I].ProviderFlags := ADelta.Fields[I].ProviderFlags + [pfInKey];
        Break;
      end;
  end;
end;

procedure TCustomResolver.InitTreeData(Tree: TUpdateTree);
begin
end;

procedure TCustomResolver.FreeTreeData(Tree: TUpdateTree);
begin
end;

procedure TCustomResolver.InternalBeforeResolve(Tree: TUpdateTree);
begin
end;

function TCustomResolver.InternalUpdateRecord(Tree: TUpdateTree): Boolean;
var
  RecNoSave: Integer;
  Applied: Boolean;
  UpdateKind: TUpdateKind;
  E: Exception;
  PrevErr, Err: EUpdateError;
begin
  PrevErr := nil;
  Err := nil;
  Tree.Delta.UseCurValues := False;
  while True do
  try
    UpdateKind := Tree.Delta.UpdateKind;
    if ((UpdateKind = ukInsert) and (FPrevResponse in [rrMerge, rrApply])) or
       ((FPrevResponse = rrMerge) and Tree.Delta.HasMergeConflicts) then
      DatabaseError(SInvalidResponse);
    Applied := False;
    RecNoSave := Tree.Delta.RecNo;
    try
      Provider.DoBeforeUpdateRecord(Tree.Source, Tree.Delta, UpdateKind, Applied);
    finally
      if Tree.Delta.RecNo <> RecNoSave then
        Tree.Delta.RecNo := RecNoSave;
    end;
    if not Applied then
      case UpdateKind of
        ukModify:
        begin
          if poDisableEdits in Provider.Options then
            raise Exception.CreateRes(@SNoEditsAllowed);
          DoUpdate(Tree);
        end;
        ukDelete:
        begin
          if poDisableDeletes in Provider.Options then
            raise Exception.CreateRes(@SNoDeletesAllowed);
          DoDelete(Tree);
        end;
        ukInsert:
        begin
          if poDisableInserts in Provider.Options then
            raise Exception.CreateRes(@SNoInsertsAllowed);
          DoInsert(Tree);
        end;
      end;
    Provider.DoAfterUpdateRecord(Tree.Source, Tree.Delta, UpdateKind);
    if (poPropogateChanges in Provider.Options) and Tree.Delta.NewValuesModified then
      LogUpdateRecord(Tree);
    Break;
  except
      if ExceptObject is Exception then
      begin
        E := Exception(AcquireExceptionObject);
        PrevErr.Free;
        PrevErr := Err;
        Err := (Tree.Source as IProviderSupportNG).PSGetUpdateException(E, PrevErr);
        if HandleUpdateError(Tree, Err, FMaxErrors, FErrorCount) then
        begin
          Tree.Delta.UseCurValues := True;
          Continue;
        end else
          break;
      end else
        raise;
  end;
  PrevErr.Free;
  Err.Free;
  FPrevResponse := rrSkip;
  Result := FErrorCount <= FMaxErrors;
end;

function TCustomResolver.RowRequest(Row: OleVariant; Options: TFetchOptions): OleVariant;
begin
  BeginUpdate;
  try
    FUpdateTree.InitDelta(Row);
    try
      FUpdateTree.RefreshData(Options);
      Result := FUpdateTree.Delta.Data;
    finally
      FUpdateTree.Clear;
    end;
  finally
    EndUpdate;
  end;
end;

function TCustomResolver.ApplyUpdates(const Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer): OleVariant;
var
  XmlMode: Cardinal;
  Status: Integer;
  DataPacket: TDataPacket;
begin
  BeginUpdate;
  try
    FUpdateTree.InitDelta(Delta);
    try
      Provider.DoOnUpdateData(FUpdateTree.Delta);
      FPrevResponse := rrSkip;
      FMaxErrors := MaxErrors;
      if FMaxErrors = -1 then FMaxErrors := MaxInt;
      FErrorCount := 0;
      FUpdateTree.DoUpdates;
      ErrorCount := FErrorCount;
      if FUpdateTree.HasErrors then
      begin
        Status := FUpdateTree.ErrorDS.DSBase.GetProp(dspropXML_StreamMode, @XMLMode);
        if (Status <> 0) or (XMLMode = 0) then
          Result := FUpdateTree.ErrorDS.Data
        else
        begin
          FUpdateTree.ErrorDS.Check(FUpdateTree.ErrorDS.DSBase.StreamDS(DataPacket));
          DataPacketToVariant(DataPacket, Result);
        end;
      end else
        Result := Null;
      Provider.DoBeforeCommit(FUpdateTree.Delta, ErrorCount, MaxErrors, Result);
    finally
      FUpdateTree.Clear;
    end;
  finally
    EndUpdate;
  end;
end;

{ Update error handling }

function TCustomResolver.HandleUpdateError(Tree: TUpdateTree;
  E: EUpdateError; var MaxErrors, ErrorCount: Integer): Boolean;
var
  Response: TResolverResponse;
  UpdateKind: TUpdateKind;
begin
  UpdateKind := Tree.Delta.UpdateKind;
  if ErrorCount < MaxErrors then
    Response := rrSkip else
    Response := rrAbort;
  try
    InitializeConflictBuffer(Tree);
  except
    { Ignore errors that occur when initializing the conflict buffer }
  end;
  if Assigned(Provider.OnUpdateError) then
    Provider.OnUpdateError(Provider, Tree.Delta, E, UpdateKind, Response);
  if Response in [rrSkip, rrAbort] then
  begin
    Inc(ErrorCount);
    if ErrorCount > MaxErrors then
      Response := rrAbort;
    if (Response = rrAbort) then
      MaxErrors := ErrorCount - 1;
    if Response in [rrSkip, rrAbort] then
      LogUpdateError(Tree, E, Response);
  end;
  FPrevResponse := Response;
  Result := Response in [rrMerge, rrApply];
end;

procedure TCustomResolver.LogUpdateRecord(Tree: TUpdateTree);
var
  I: Integer;
  CurVal: Variant;
begin
  Tree.InitErrorPacket(nil, rrApply);
  for I := 0 to Tree.Delta.FieldCount - 1 do
  begin
    { Blobs, Bytes and VarBytes are not included in result packet }
    if (Tree.Delta.Fields[I].IsBlob) or
       (Tree.Delta.Fields[I].DataType in [ftBytes, ftVarBytes]) then
      continue;
    CurVal := Tree.Delta.Fields[I].NewValue;
    if not VarIsClear(CurVal) then
      Tree.ErrorDS.FieldByName(Tree.Delta.Fields[I].FieldName).Value := CurVal;
  end;
  Tree.ErrorDS.Post;
end;

procedure TCustomResolver.LogUpdateError(Tree: TUpdateTree;
  E: EUpdateError; Response: TResolverResponse);
var
  I: Integer;
  CurVal: Variant;
begin
  Tree.InitErrorPacket(E, Response);
  if Tree.Delta.HasCurValues then
    for I := 0 to Tree.Delta.FieldCount - 1 do
    begin
      { Blobs, Bytes and VarBytes are not included in result packet }
      if (Tree.Delta.Fields[I].IsBlob) or
         (Tree.Delta.Fields[I].DataType in [ftBytes, ftVarBytes]) then
        continue;
      CurVal := Tree.Delta.Fields[I].CurValue;
      if not VarIsClear(CurVal) then
        Tree.ErrorDS.FieldByName(Tree.Delta.Fields[I].FieldName).Value := CurVal;
    end;
  Tree.ErrorDS.Post;
end;

{ TDataSetResolver }

constructor TDataSetResolver.Create(AProvider: TDataSetProvider);
begin
  inherited Create(AProvider);
  FOpened := False;
end;

function TDataSetResolver.GetProvider: TDataSetProvider;
begin
  Result := TDataSetProvider(inherited Provider);
end;

procedure TDataSetResolver.BeginUpdate;
begin
  FOpened := not Provider.DataSet.Active;
  if FOpened then
  begin
    Provider.DataSet.Open;
    FBookmark := nil;
  end else
    FBookmark := Provider.DataSet.Bookmark;
end;

procedure TDataSetResolver.EndUpdate;
begin
  if FOpened then
  begin
    Provider.DataSet.Close;
    FOpened := False;
  end else
  begin
    if (Length(FBookmark) > 0) and Provider.DataSet.BookmarkValid(FBookmark) then
      Provider.DataSet.Bookmark := FBookmark;
  end;
end;

procedure TDataSetResolver.InitializeConflictBuffer(Tree: TUpdateTree);
begin
  { Set the conflict buffer to the current values of the data }
  if Provider.FindRecord(Tree.Source, Tree.Delta, upWhereKeyOnly) then
    Tree.Delta.AssignCurValues(Tree.Source);
end;

procedure TDataSetResolver.InternalBeforeResolve(Tree: TUpdateTree);
begin
  Provider.FindRecord(Tree.Source, Tree.Delta, Provider.UpdateMode);
end;

procedure TDataSetResolver.PutRecord(Tree: TUpdateTree);

  procedure PutField(Src, Dest: TField); forward;

  procedure PutObjectField(Src, Dest: TObjectField);
  var
    i: Integer;
  begin
    if VarIsNull(Src.NewValue) then
      Dest.Clear else
      for i := 0 to Src.FieldCount - 1 do
        if (not VarIsClear(Src.Fields[i].NewValue)) and
           (pfInUpdate in Src.Fields[i].ProviderFlags) then
          PutField(Src.Fields[i], Dest.Fields[i]);
  end;

  procedure PutField(Src, Dest: TField);
  begin
    if (Src.DataType in [ftArray, ftADT]) then
      PutObjectField(TObjectField(Src), TObjectField(Dest)) else
    if (Src.DataType in [ftDataSet, ftReference]) then
      raise Exception.CreateRes(@SNoDataSets) else
    if (not VarIsClear(Src.NewValue)) and
       (pfInUpdate in Src.ProviderFlags) then
      Dest.Assign(Src);
  end;

var
  I: Integer;
  Field: TField;
begin
  try
    for I := 0 to Tree.Delta.FieldCount - 1 do
    begin
      Field := Tree.Source.FindField(Tree.Delta.Fields[I].FieldName);
      if (Field <> nil) and (Tree.Delta.Fields[I].DataType <> ftDataSet) then
        PutField(Tree.Delta.Fields[I], Field);
    end;
    Tree.Source.Post;
  except
    Tree.Source.Cancel;
    raise;
  end;
end;

procedure TDataSetResolver.DoUpdate(Tree: TUpdateTree);
begin
  if not Provider.FindRecord(Tree.Source, Tree.Delta, Provider.UpdateMode) then
    DatabaseError(SRecordChanged);
  Tree.Source.Edit;
  PutRecord(Tree);
end;

procedure TDataSetResolver.DoDelete(Tree: TUpdateTree);
begin
  if Provider.FindRecord(Tree.Source, Tree.Delta, Provider.UpdateMode) then
    Tree.Source.Delete
  else
    DatabaseError(SRecordChanged);
end;

procedure TDataSetResolver.DoInsert(Tree: TUpdateTree);
begin
  Tree.Source.Append;
  PutRecord(Tree);
end;

{ TSQLResolver }

constructor TSQLResolver.Create(AProvider: TDataSetProvider);
begin
  inherited Create(AProvider);
  FSQL := TStringList.Create;
  FParams := TParams.Create(nil);
end;

destructor TSQLResolver.Destroy;
begin
  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TSQLResolver.GetProvider: TDataSetProvider;
begin
  Result := TDataSetProvider(inherited Provider);
end;

procedure TSQLResolver.InitTreeData(Tree: TUpdateTree);

  function GetQuotedTableName(SQLBased: Boolean;
    const QuoteChar, TableName: string): string;
  var
    DotPos, DotPos2, TableNameLen: Integer;
  begin
    Result := '';
    TableNameLen := TableName.Length;
    if TableNameLen > 0 then
    begin
      if (TableName.Chars[0] = '''') or (TableName.Chars[0] = '"') or (TableName.Chars[0] = '`') or
        (TableName.Chars[TableNameLen-1] = '''') or (TableName.Chars[TableNameLen-1] = '"')
        or (TableName.Chars[TableNameLen-1] = '`') then
        Result := TableName
      else
      begin
        if SQLBased then
        begin
          Result := TableName;
          DotPos := Result.IndexOf('.');
          DotPos2 := string(PChar(Result) + DotPos + 1).IndexOf('.');
          if DotPos2 <> -1 then
            DotPos2 := DotPos2 + DotPos + 1;
          if (DotPos <> -1) and (DotPos2 <> -1 ) then
          begin
            Result := Result.Insert(DotPos2, QuoteChar);
            Result := Result.Insert(DotPos2 + 2, QuoteChar);
            Result := Result.Insert(DotPos + 1, QuoteChar);
            Result := Result.Insert(DotPos, QuoteChar);
          end
          else if DotPos <> -1 then
          begin
            Result := Result.Insert(DotPos + 1, QuoteChar);
            Result := Result.Insert(DotPos, QuoteChar);
          end;
          Result := QuoteChar + Result + QuoteChar;
        end
        else
          Result := QuoteChar + TableName + QuoteChar;
      end;
    end;
  end;

var
  Info: PSQLInfo;
  i: Integer;
  TableName: string;
begin
  if Tree.Data <> nil then
    Dispose(PSQLInfo(Tree.Data));
  New(Info);
  Tree.Data := Info;

  Info.IsSQLBased := (Tree.Source as IProviderSupportNG).PSIsSQLBased;
  if (poUseQuoteChar in Provider.Options) then
    Info.QuoteChar := (Tree.Source as IProviderSupportNG).PSGetQuoteChar
  else
    Info.QuoteChar := '';
  TableName := VarToStr(Tree.Delta.GetOptionalParam(szTABLE_NAME));   // Wide
  if TableName = '' then
    TableName := (Tree.Source as IProviderSupportNG).PSGetTableName;

  Provider.DoGetTableName(Tree.Source, TableName);
  if TableName <> '' then
    Info.QuotedTable := GetQuotedTableName(Info.IsSQLBased, Info.QuoteChar, TableName);
  if Info.IsSQLBased then
    Info.QuotedTableDot := '' else
    Info.QuotedTableDot := Info.QuotedTable + '.';
  Info.HasObjects := False;
  for i := 0 to Tree.Delta.FieldCount - 1 do
    if (Tree.Delta.Fields[i] is TObjectField) and
       (TObjectField(Tree.Delta.Fields[i]).ObjectType <> '') then
    begin
      Info.HasObjects := True;
      break;
    end;
end;

procedure TSQLResolver.FreeTreeData(Tree: TUpdateTree);
begin
  Dispose(PSQLInfo(Tree.Data));
  Tree.Data := nil;
end;

procedure TSQLResolver.DoExecSQL(SQL: TStringList; Params: TParams);
var
  RowsAffected: Integer;
begin
  RowsAffected := (Provider.DataSet as IProviderSupportNG).PSExecuteStatement(SQL.Text, Params);
  if not (poAllowMultiRecordUpdates in Provider.Options) and (RowsAffected > 1) then
  begin
    (Provider.DataSet as IProviderSupportNG).PSEndTransaction(False);
    Provider.TransactionStarted := False;
    DatabaseError(STooManyRecordsModified);
  end;
  if RowsAffected < 1 then
    DatabaseError(SRecordChanged);
end;

procedure TSQLResolver.DoGetValues(SQL: TStringList; Params: TParams;
  DataSet: TDataSet);
var
  DS: TDataSet;
begin
  DS := nil;
  (Provider.DataSet as IProviderSupportNG).PSExecuteStatement(SQL.Text, Params, DS);
  if Assigned(DS) then
  try
    TPacketDataSet(DataSet).AssignCurValues(DS)
  finally
    DS.Free;
  end;
end;

procedure TSQLResolver.InitializeConflictBuffer(Tree: TUpdateTree);
var
  Alias: string;

  function GenConflictSelectSQL(Tree: TUpdateTree; SQL: TStrings): Boolean;
  var
    I: Integer;
    Temp: string;
  begin
    Result := False;
    SQL.Add('select');
    for I := 0 to Tree.Delta.FieldCount - 1 do
      if UseFieldInWhere(Tree.Delta.Fields[I], Provider.UpdateMode) then
      begin
        SQL.Add(string.Format(' %s%s%s%1:s,',
          [PSQLInfo(Tree.Data).QuotedTableDot, PSQLInfo(Tree.Data).QuoteChar, Tree.Delta.Fields[I].Origin]));
        Result := True;
      end;
    if Result then
    begin
      { Remove last ',' }
      Temp := SQL[SQL.Count-1];
      SQL[SQL.Count-1] := Temp.Substring(0, Temp.Length - 1);
      SQL.Add(string.Format(' from %s %s',[PSQLInfo(Tree.Data).QuotedTable, Alias]));     { Do not localize }
    end;
  end;

begin
  if PSQLInfo(Tree.Data).HasObjects then Alias := DefAlias else Alias := '';
  FSQL.Clear;
  FParams.Clear;
  if GenConflictSelectSQL(Tree, FSQL) then
  begin
    GenWhereSQL(Tree, FSQL, FParams, upWhereKeyOnly, Alias);
    DoGetValues(FSQL, FParams, Tree.Delta);
  end;
end;

procedure TSQLResolver.InternalDoUpdate(Tree: TUpdateTree; UpdateKind: TUpdateKind);
var
  Alias: string;
begin
  if (not (Tree.Source as IProviderSupportNG).PSUpdateRecord(UpdateKind, Tree.Delta)) then
  begin
    if (PSQLInfo(Tree.Data).QuotedTable = '') and not Tree.IsNested then
      DatabaseError(SNoTableName);
    if PSQLInfo(Tree.Data).HasObjects then Alias := DefAlias else Alias := '';
    FSQL.Clear;
    FParams.Clear;
    case UpdateKind of
      ukModify: GenUpdateSQL(Tree, FSQL, FParams, Alias);
      ukInsert: GenInsertSQL(Tree, FSQL, FParams);
      ukDelete: GenDeleteSQL(Tree, FSQL, FParams, Alias);
    end;
    if FSQL.Text <> '' then
      DoExecSQL(FSQL, FParams);
  end;
end;

procedure TSQLResolver.DoUpdate(Tree: TUpdateTree);
begin
  InternalDoUpdate(Tree, ukModify);
end;

procedure TSQLResolver.DoDelete(Tree: TUpdateTree);
begin
  InternalDoUpdate(Tree, ukDelete);
end;

procedure TSQLResolver.DoInsert(Tree: TUpdateTree);
begin
  InternalDoUpdate(Tree, ukInsert);
end;

{ SQL generation }

function QuoteFullName(const FullName, QuoteChar: string): string;
var
  i: Integer;
  p: PChar;
begin
  if (FullName.Length > 1) and ((FullName.Chars[0] = #0) or (FullName.Chars[0] = #1)) then
    p := @FullName[Low(FullName)+1]
  else
    p := PChar(FullName);
  Result := string.Format('%s%s%0:s',[QuoteChar, p]);
  for i := Result.Length-1 downto 0 do
    if Result.Chars[i] = '.' then
    begin
      Result.Insert(i + 1, QuoteChar);
      Result.Insert(i, QuoteChar);
    end;
end;

function TSQLResolver.UseFieldInUpdate(Field: TField): Boolean;
const
  ExcludedTypes = [ftAutoInc, ftDataSet, ftADT, ftArray, ftReference, ftCursor, ftUnknown];
begin
  Result := (pfInUpdate in Field.ProviderFlags) and not (Field.DataType in ExcludedTypes) and
    not Field.ReadOnly and (Field.FieldKind = fkData) and not (pfHidden in Field.ProviderFlags) and
    not VarIsClear(Field.NewValue) and (Field.Tag <> tagSERVERCALC);
end;

function TSQLResolver.UseFieldInWhere(Field: TField; Mode: TUpdateMode): Boolean;
const
  ExcludedTypes = [ftDataSet, ftADT, ftArray, ftReference, ftCursor, ftUnknown];
begin
  Result := not (Field.DataType in ExcludedTypes) and not Field.IsBlob and
    (Field.FieldKind = fkData) and (Field.Tag <> tagSERVERCALC);
  if Result then
    case Mode of
      upWhereAll:
        Result := pfInWhere in Field.ProviderFlags;
      upWhereChanged:
        Result := ((pfInWhere in Field.ProviderFlags) and not VarIsClear(Field.NewValue)) or
          (pfInKey in Field.ProviderFlags);
      upWhereKeyOnly:
        Result := pfInKey in Field.ProviderFlags;
    end;
end;

procedure TSQLResolver.GenWhereSQL(Tree: TUpdateTree; SQL: TStrings; Params: TParams;
  GenUpdateMode: TUpdateMode; Alias: string);

  function AddField(Field: TField; InObject: Boolean): Boolean;
  var
    I: Integer;
    BindText: string;
  begin
    Result := False;
    if Field.DataType = ftADT then
    begin
      for I := 0 to TObjectField(Field).FieldCount - 1 do
        if AddField(TObjectField(Field).Fields[I], True) then
          Result := True;
    end
    else if UseFieldInWhere(Field, GenUpdateMode) and (Field.DataSize < dsMaxStringSize) then
    begin
      Result := True;
      if InObject then
      begin
        if VarIsNull(Field.OldValue) then
          BindText := string.Format(' %s.%s is null and', [Alias,   { Do not localize }
            QuoteFullName(Field.FullName, PSQLInfo(Tree.Data).QuoteChar)])
        else
        begin
          BindText := string.Format(' %s.%s = ? and',[Alias,        { Do not localize }
            QuoteFullName(Field.FullName, PSQLInfo(Tree.Data).QuoteChar)]);
          TParam(Params.Add).AssignFieldValue(Field, Field.OldValue);
        end;
      end
      else
      begin
        if VarIsNull(Field.OldValue) or (not PSQLInfo(Tree.Data).IsSQLBased and
           (Field.DataType in [ftString, ftWideString]) and (Length(Field.OldValue) = 0)) then
          BindText := string.Format(' %s%s%s%1:s is null and',       { Do not localize }
            [PSQLInfo(Tree.Data).QuotedTableDot, PSQLInfo(Tree.Data).QuoteChar, Field.Origin])
        else
        begin
          BindText := string.Format(' %s%s%s%1:s = ? and',           { Do not localize }
            [PSQLInfo(Tree.Data).QuotedTableDot, PSQLInfo(Tree.Data).QuoteChar, Field.Origin]);
          TParam(Params.Add).AssignFieldValue(Field, Field.OldValue);
        end;
      end;
      SQL.Add(BindText);
    end;
  end;

var
  I: Integer;
  TempStr: string;
  Added: Boolean;
begin
  SQL.Add('where');
  Added := False;
  for I := 0 to Tree.Delta.FieldCount - 1 do
    if AddField(Tree.Delta.Fields[I], Alias = NestAlias) then
      Added := True;
  if not Added then
    DatabaseError(SNoKeySpecified);
  { Remove last ' and'}
  TempStr := SQL[SQL.Count-1];
  SQL[SQL.Count-1] := TempStr.Substring(0, TempStr.Length - 4);
end;

procedure TSQLResolver.GenInsertSQL(Tree: TUpdateTree; SQL: TStrings;
  Params: TParams);

  procedure AddField(Field: TField; var FieldLine, ParamLine: string);
  var
    I: Integer;
    TempStr: string;
    Value: Variant;
  begin
    if Field.DataType in [ftADT, ftArray] then
    begin
      FieldLine := string.Format('%s%s%s%s%2:s, ', [FieldLine, PSQLInfo(Tree.Data).QuotedTableDot,
        PSQLInfo(Tree.Data).QuoteChar, Field.Origin]);
      ParamLine := string.Format('%s%s(', [ParamLine, TObjectField(Field).ObjectType]);
      for I := 0 to TObjectField(Field).FieldCount - 1 do
        AddField(TObjectField(Field).Fields[I], TempStr, ParamLine);
      ParamLine := ParamLine.Substring(0, ParamLine.Length - 2) + '), ';
    end
    else if (Field.DataType = ftDataSet) and (TObjectField(Field).ObjectType <> '') then
    begin
      FieldLine := string.Format('%s%s%s%s%2:s, ', [FieldLine, PSQLInfo(Tree.Data).QuotedTableDot,
        PSQLInfo(Tree.Data).QuoteChar, Field.Origin]);
      ParamLine := string.Format('%s%s(), ', [ParamLine, TDataSetField(Field).ObjectType]);
    end
    else if (UseFieldInUpdate(Field)) or ((Field.ParentField <> nil) and (Field.ParentField.DataType in [ftADT, ftArray, ftReference]) and VarIsNull(Field.Value)) then
    begin
      if (Field.DataType = ftOraBlob) and (not InformixLob) then
      begin
        FieldLine := string.Format('%s%s%s%s%2:s, ', [FieldLine, PSQLInfo(Tree.Data).QuotedTableDot,
                  PSQLInfo(Tree.Data).QuoteChar, Field.Origin]);
        ParamLine := ParamLine + 'EMPTY_BLOB(), '    { Do not localize }
      end
      else if (Field.DataType = ftOraClob) and (not InformixLob) then
      begin
        FieldLine := string.Format('%s%s%s%s%2:s, ', [FieldLine, PSQLInfo(Tree.Data).QuotedTableDot,
                  PSQLInfo(Tree.Data).QuoteChar, Field.Origin]);
        ParamLine := ParamLine + 'EMPTY_CLOB(), '    { Do not localize }
      end
      else if (Field.ParentField <> nil) and VarIsNull(Field.Value) then
      begin
        FieldLine := string.Format('%s%s%s%s%2:s, ', [FieldLine, PSQLInfo(Tree.Data).QuotedTableDot,
           PSQLInfo(Tree.Data).QuoteChar, Field.Origin]);
        ParamLine := ParamLine + 'null, ';
      end
      else
      begin
        FieldLine := string.Format('%s%s%s%s%2:s, ', [FieldLine, PSQLInfo(Tree.Data).QuotedTableDot,
        PSQLInfo(Tree.Data).QuoteChar, Field.Origin]);
        ParamLine := ParamLine + '?, ';
        Value := Field.NewValue;
        if VarIsClear(Value) then Value := Field.OldValue;
        TParam(Params.Add).AssignFieldValue(Field, Value);
      end;
    end;
  end;

var
  I, J, OraLobs: Integer;
  FieldLine, ParamLine: string;
  Value: Variant;
begin
  OraLobs := 0;
  SQL.Clear;
  if Tree.IsNested then
  begin
    SQL.Add(string.Format('insert into the (select %s FROM %s %s',[QuoteFullName(Tree.Name, PSQLInfo(Tree.Data).QuoteChar),
      PSQLInfo(Tree.Parent.Data).QuotedTable, DefAlias]));   { Do not localize }
    GenWhereSQL(Tree.Parent, SQL, Params, upWhereKeyOnly, DefAlias);
    SQL.Add(')');
  end
  else
    SQL.Add(string.Format('insert into %s', [PSQLInfo(Tree.Data).QuotedTable]));        { Do not localize }
  FieldLine := '  (';
  ParamLine := FieldLine;
  for I := 0 to Tree.Delta.FieldCount - 1 do
  begin
    AddField(Tree.Delta.Fields[I], FieldLine, ParamLine);
    if Tree.Delta.Fields[I].DataType in [ftOraClob, ftOraBlob] then
      if (not InformixLob) then
        Inc(OraLobs);
  end;
  if not Tree.IsNested then
    SQL.Add(FieldLine.Substring(0, FieldLine.Length - 2) + ')');
  SQL.Add('values');
  SQL.Add(ParamLine.Substring(0, ParamLine.Length - 2) + ')');

  if OraLobs > 0 then
  begin
    SQL.Add(' RETURNING ');              { Do not localize }
    J := OraLobs;
    for I := 0 to Tree.Delta.FieldCount - 1 do
      if (Tree.Delta.Fields[I].DataType in [ftOraClob, ftOraBlob])
         and UseFieldInUpdate(Tree.Delta.Fields[I]) then
         begin
           Dec(J);
           SQL.Add(string.Format('%s ', [Tree.Delta.Fields[I].FullName]));
           if J > 0 then SQL.Add(', ');
           Value := Tree.Delta.Fields[I].NewValue;
           if VarIsClear(Value) then Value := Tree.Delta.Fields[I].OldValue;
           TParam(Params.Add).AssignFieldValue(Tree.Delta.Fields[I], Value)
         end;
    SQL.Add('INTO ');                    { Do not localize }
    while OraLobs > 0 do
    begin
      SQL.Add('? ');
      Dec(OraLobs);
      if OraLobs > 0 then SQL.Add(', ');
    end;
  end;
end;

procedure TSQLResolver.GenDeleteSQL(Tree: TUpdateTree; SQL: TStrings;
  Params: TParams; Alias: string);
begin
  SQL.Clear;
  if Tree.IsNested then
  begin
    Alias := NestAlias;
    SQL.Add(Format('delete the (select %s FROM %s %s',[QuoteFullName(Tree.Name, PSQLInfo(Tree.Data).QuoteChar),
      PSQLInfo(Tree.Parent.Data).QuotedTable, DefAlias]));       { Do not localize }
    GenWhereSQL(Tree.Parent, SQL, Params, upWhereKeyOnly, DefAlias);
    SQL.Add(Format(') %s',[Alias]));
  end
  else
    SQL.Add(Format('delete from %s %s', [PSQLInfo(Tree.Data).QuotedTable, Alias]));  { Do not localize }
  GenWhereSQL(Tree, SQL, Params, Provider.UpdateMode, Alias);
end;

procedure TSQLResolver.GenUpdateSQL(Tree: TUpdateTree; SQL: TStrings;
  Params: TParams; Alias: string);

  function AddField(Field: TField; InObject, InArray: Boolean): Boolean;
  var
    I: Integer;
    TempStr: string;
    Value: Variant;
    NoParam: Boolean;
  begin
    Result := False;
    NoParam := False;
    if Field.DataType = ftADT then
    begin
      if InArray then
        SQL.Add(Format(' %s(',[TObjectField(Field).ObjectType]));
      for I := 0 to TObjectField(Field).FieldCount - 1 do
        Result := Result or
          AddField(TObjectField(Field).Fields[I], True, InArray);
      if InArray then
      begin
        TempStr := SQL[SQL.Count-1];
        SQL[SQL.Count-1] := TempStr.Substring(0, TempStr.Length - 1);
        SQL.Add('),');
      end;
    end
    else if Field.DataType = ftArray then
    begin
      SQL.Add(string.Format('%s = %s(',[Field.FullName, TObjectField(Field).ObjectType]));
      for I := 0 to TObjectField(Field).FieldCount - 1 do
        Result := Result or
          AddField(TObjectField(Field).Fields[I], InObject, True);
      TempStr := SQL[SQL.Count-1];
      SQL[SQL.Count-1] := TempStr.Substring(0, TempStr.Length - 1);
      SQL.Add('),');
    end
    else if InArray then
    begin
      SQL.Add(' ?,');
      Value := Field.NewValue;
      if VarIsClear(Value) then Value := Field.OldValue;
      TParam(Params.Add).AssignFieldValue(Field, Value);
      Result := True;
    end
    else if UseFieldInUpdate(Field) then
    begin
      Result := True;
      if (Field.DataType = ftOraClob) and (not InformixLob) then
      begin
        NoParam := True;
        if InObject then
          SQL.Add(string.Format(' %s.%s = EMPTY_CLOB(),', [Alias, QuoteFullName(Field.FullName, PSQLInfo(Tree.Data).QuoteChar),   { Do not localize }
            Field.FullName]))
        else
          SQL.Add(string.Format(' %s%s%s%1:s = EMPTY_CLOB(),', [PSQLInfo(Tree.Data).QuotedTableDot,          { Do not localize }
             PSQLInfo(Tree.Data).QuoteChar, Field.Origin]));
      end
      else if (Field.DataType = ftOraBlob) and (not InformixLob) then
      begin
        NoParam := True;
        if InObject then
          SQL.Add(string.Format(' %s.%s = EMPTY_BLOB(),', [Alias, QuoteFullName(Field.FullName, PSQLInfo(Tree.Data).QuoteChar),   { Do not localize }
             Field.FullName]))
        else
          SQL.Add(string.Format(' %s%s%s%1:s = EMPTY_BLOB(),', [PSQLInfo(Tree.Data).QuotedTableDot,          { Do not localize }
             PSQLInfo(Tree.Data).QuoteChar, Field.Origin]));
      end
      else if InObject then
        SQL.Add(string.Format(' %s.%s = ?,', [Alias, QuoteFullName(Field.FullName, PSQLInfo(Tree.Data).QuoteChar),
          Field.FullName]))
      else
        SQL.Add(string.Format(' %s%s%s%1:s = ?,', [PSQLInfo(Tree.Data).QuotedTableDot,
          PSQLInfo(Tree.Data).QuoteChar, Field.Origin]));
      if not NoParam then
      begin
        Value := Field.NewValue;
        if VarIsClear(Value) then Value := Field.OldValue;
        TParam(Params.Add).AssignFieldValue(Field, Value);
      end;
    end;
  end;

var
  I, J, OraLobs: Integer;
  TempStr: string;
  Value: Variant;
  UpdateReqd: Boolean;
begin
  OraLobs := 0;
  if Tree.IsNested then
  begin
    Alias := NestAlias;
    SQL.Add(string.Format('update the (select %s FROM %s %s',[QuoteFullName(Tree.Name, PSQLInfo(Tree.Data).QuoteChar),   { Do not localize }
      PSQLInfo(Tree.Parent.Data).QuotedTable, DefAlias]));
    GenWhereSQL(Tree.Parent, SQL, Params, upWhereKeyOnly, DefAlias);
    SQL.Add(string.Format(') %s set',[Alias]));                                                      { Do not localize }
  end
  else
    SQL.Add(string.Format('update %s %s set', [PSQLInfo(Tree.Data).QuotedTable, Alias]));                                { Do not localize }

  UpdateReqd := False;
  for I := 0 to Tree.Delta.FieldCount - 1 do
  begin
    if (Tree.Delta.Fields[I].DataType in [ftOraClob, ftOraBlob]) and
        UseFieldInUpdate(Tree.Delta.Fields[I]) then
        if (not InformixLob) then
          Inc(OraLobs);
    if AddField(Tree.Delta.Fields[I], Alias = NestAlias, False) then
      UpdateReqd := True;
  end;

  if not UpdateReqd then
  begin
    SQL.Clear;
    Exit;
  end;

  { Remove last ',' }
  TempStr := SQL[SQL.Count-1];
  SQL[SQL.Count-1] := TempStr.Substring(0, TempStr.Length - 1);

  GenWhereSQL(Tree, SQL, Params, Provider.UpdateMode, Alias);

  if OraLobs > 0 then
  begin
    SQL.Add(' RETURNING ');       { Do not localize }
    J := OraLobs;
    for I := 0 to Tree.Delta.FieldCount - 1 do
      if (Tree.Delta.Fields[I].DataType in [ftOraClob, ftOraBlob])
         and UseFieldInUpdate(Tree.Delta.Fields[I])  then
         begin
           Dec(J);
           SQL.Add(string.Format('%s ', [Tree.Delta.Fields[I].FullName]));
           if J > 0 then SQL.Add(', ');
           Value := Tree.Delta.Fields[I].NewValue;
           if VarIsClear(Value) then Value := Tree.Delta.Fields[I].OldValue;
           TParam(Params.Add).AssignFieldValue(Tree.Delta.Fields[I], Value)
         end;
    SQL.Add('INTO ');         { Do not localize }
    while OraLobs > 0 do
    begin
      SQL.Add('? ');
      Dec(OraLobs);
      if OraLobs > 0 then SQL.Add(', ');
    end;
  end;
end;

procedure TSQLResolver.GenSelectSQL(Tree: TUpdateTree; SQL: TStrings;
  Params: TParams; Alias: string; Mode: TUpdateMode = upWhereKeyOnly);
var
  I: Integer;
  Temp: string;
begin
  SQL.Add('select');
  for I := 0 to Tree.Delta.FieldCount - 1 do
    if not (Tree.Delta.Fields[I].DataType in [ftDataSet, ftReference]) and (Tree.Delta.Fields[I].FieldKind = fkData)
       and (pfInUpdate in Tree.Delta.Fields[I].ProviderFlags) then
      SQL.Add(string.Format(' %s%s%s%1:s,',[PSQLInfo(Tree.Data).QuotedTableDot, PSQLInfo(Tree.Data).QuoteChar, Tree.Delta.Fields[I].Origin]));
  { Remove last ',' }
  Temp := SQL[SQL.Count-1];
  SQL[SQL.Count-1] := Temp.Substring(0, Temp.Length - 1);
  SQL.Add(string.Format(' from %s %s',[PSQLInfo(Tree.Data).QuotedTable, Alias]));     { Do not localize }
  GenWhereSQL(Tree, SQL, Params, Mode, Alias);
end;

{ TLocalAppServer }

constructor TLocalAppServer.Create(AProvider: TCustomProvider);
begin
  inherited Create;
  FProvider := AProvider;
end;

destructor TLocalAppServer.Destroy;
begin
  if FProviderCreated then
    FreeAndNil(FProvider)
  else
    FProvider := nil;
  inherited Destroy;
end;

constructor TLocalAppServer.Create(ADataset: TDataset);
begin
  inherited Create;
  FProvider := TDatasetProvider.Create(nil);
  TDatasetProvider(FProvider).Dataset := ADataset;
  FProviderCreated := True;
end;

function TLocalAppServer.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TLocalAppServer.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TLocalAppServer.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TLocalAppServer.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TLocalAppServer.AS_ApplyUpdates(const ProviderName: OleStr;
  Delta: OleVariant; MaxErrors: Integer; out ErrorCount: Integer;
  var OwnerData: OleVariant): OleVariant;
begin
  Result := FProvider.ApplyUpdates(Delta, MaxErrors, ErrorCount, OwnerData);
end;

function TLocalAppServer.AS_GetRecords(const ProviderName: OleStr; Count: Integer;
  out RecsOut: Integer; Options: Integer; const CommandText: OleStr;
  var Params, OwnerData: OleVariant): OleVariant;
begin
  Result := FProvider.GetRecords(Count, RecsOut, Options, CommandText, Params,
    OwnerData);
end;

function TLocalAppServer.AS_GetProviderNames: OleVariant;
begin
  Result := NULL;
end;

function TLocalAppServer.AS_DataRequest(const ProviderName: OleStr;
  Data: OleVariant): OleVariant;
begin
  Result := FProvider.DataRequest(Data);
end;

function TLocalAppServer.AS_GetParams(const ProviderName: OleStr;
  var OwnerData: OleVariant): OleVariant;
begin
  Result := FProvider.GetParams(OwnerData);
end;

function TLocalAppServer.AS_RowRequest(const ProviderName: OleStr;
  Row: OleVariant; RequestType: Integer; var OwnerData: OleVariant): OleVariant;
begin
  Result := FProvider.RowRequest(Row, RequestType, OwnerData);
end;

procedure TLocalAppServer.AS_Execute(const ProviderName: OleStr;
   const CommandText: OleStr; var Params, OwnerData: OleVariant);
begin
  FProvider.Execute(CommandText, Params, OwnerData);
end;

function TLocalAppServer.InterfaceSupportsErrorInfo(const iid: TGUID): HResult;
begin
  if IsEqualGUID(IAppServer, iid) then
    Result := S_OK else
    Result := S_FALSE;
end;

function TLocalAppServer.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IAppServer, '', '');
end;

end.
