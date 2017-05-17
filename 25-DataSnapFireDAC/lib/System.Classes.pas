{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Classes;

{$R-,T-,X+,H+,B-}
{$WARN WIDECHAR_REDUCED OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}
// TODO -oUnassigned -cNEXTGEN : Fix all immutable string use
{$WARN IMMUTABLE_STRINGS ON}
{$WARN UNSAFE_VOID_POINTER OFF}

{$IF Defined(MSWINDOWS) and not Defined(NEXTGEN)}
{ ACTIVEX.HPP is not required by CLASSES.HPP }
(*$NOINCLUDE Winapi.ActiveX*)
{$HPPEMIT LEGACYHPP}
{$ENDIF}

{$IF Defined(CPUX86) and Defined(ASSEMBLER) and not Defined(PUREPASCAL)}
  {$DEFINE X86ASM}
{$ELSEIF Defined(CPUX64) and Defined(ASSEMBLER) and not Defined(PUREPASCAL)}
  {$DEFINE X64ASM}
{$ENDIF}

{$IF SizeOf(Extended) >= 10}
  {$DEFINE EXTENDEDHAS10BYTES}
{$ENDIF}
{$IF SizeOf(Extended) = 10}
  {$DEFINE EXTENDEDIS10BYTES}
{$ENDIF}

{$IF SizeOf(LongInt) = 8}
  {$DEFINE LONGINT64}
{$ENDIF}

{$IF defined(ANDROID)}
  {$DEFINE USE_LIBICU}
{$ENDIF}

// TODO -oAntonioT -cLINUX64: Remove this when iconv and locales will be finished.
{$IFDEF LINUX64}
//  {$DEFINE USE_LIBICU}
{$ENDIF LINUX64}

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
{$IFNDEF NEXTGEN}
  Winapi.ActiveX,
{$ENDIF !NEXTGEN}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.SysTypes,
  Posix.UniStd,
  Posix.Signal,
{$IFDEF MACOS}
  Macapi.CoreServices,
{$ENDIF MACOS}
{$IFDEF ANDROID}
  Posix.Fcntl,
{$ENDIF ANDROID}
{$IFDEF LINUX}
  System.SyncObjs,
{$ENDIF LINUX}
{$ENDIF POSIX}
  System.Generics.Defaults, System.Generics.Collections,
  System.Types, System.TypInfo, System.Rtti,
  System.SysUtils;

const
  MaxListSize = MaxInt div 16 deprecated;

{ TStream seek origins }

  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;

type
{ TStream seek origins }
  TSeekOrigin = (soBeginning, soCurrent, soEnd);

  TPlatformIds = UInt32;

const
{ TFileStream create mode }

  fmCreate = $FF00;

{ TParser special tokens }

  toEOF     = Char(0);
  toSymbol  = Char(1);
  toString  = Char(2);
  toInteger = Char(3);
  toFloat   = Char(4);
  toWString = Char(5);

  {+ ! Moved here from menus.pas !!}
  { TShortCut special values }
  scCommand = $1000;
  scShift = $2000;
  scCtrl = $4000;
  scAlt = $8000;
  scNone = 0;

  { Platform identifiers }
  pidWin32        = $0001;
  pidWin64        = $0002;
  pidOSX32        = $0004;
  pidiOSSimulator = $0008;
  pidAndroid      = $0010;
  pidLinux32      = $0020;
  pidiOSDevice32  = $0040;
  pidiOSDevice    = pidiOSDevice32;// deprecated 'Use pidiOSDevice32';
  pidLinux64      = $0080;

  pidWinNX32      = $0100;
  pidWinIoT32     = $0200; // Embedded IoT (Internet of Things) Windows w/ Intel Galileo
  pidiOSDevice64  = $0400;
  pidWinARM       = $0800;
  pidOSX64        = $1000;
  pidOSXNX64      = pidOSX64 deprecated 'Use pidOSX64';
  pidLinux32Arm   = $2000;
  pidLinux64Arm   = $4000;
  pidAndroid64    = $8000;

  dupIgnore  = System.Types.TDuplicates.dupIgnore{ deprecated 'Use dupIgnore from System.Types.TDuplicates'};
  dupAccept  = System.Types.TDuplicates.dupAccept{ deprecated 'Use dupAccept from System.Types.TDuplicates'};
  dupError   = System.Types.TDuplicates.dupError{ deprecated 'Use dupError from System.Types.TDuplicates'};

type

{ Text alignment types }
{ To be moved to System.UITypes in a future release }

  TAlignment = (taLeftJustify, taRightJustify, taCenter);
  TLeftRight = TAlignment.taLeftJustify..TAlignment.taRightJustify;
  TBiDiMode = (bdLeftToRight, bdRightToLeft, bdRightToLeftNoAlign,
    bdRightToLeftReadingOnly);
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  TTopBottom = TVerticalAlignment.taAlignTop..TVerticalAlignment.taAlignBottom;

{ Types used by standard events }
{ To be moved to System.UITypes in a future release }

  // Controls.TCMMouseWheel relies on TShiftState not exceeding 2 bytes in size
  TShiftState = set of (ssShift, ssAlt, ssCtrl,
    ssLeft, ssRight, ssMiddle, ssDouble, ssTouch, ssPen, ssCommand, ssHorizontal);

  THelpContext = -MaxInt..MaxInt;
  THelpType = (htKeyword, htContext);

  {+ ! Moved here from menus.pas !!}
  { To be moved to System.UITypes in a future release }
  TShortCut = Low(Word)..High(Word);

{ Standard events }
type
  TNotifyEvent = procedure(Sender: TObject) of object;
  TGetStrProc = procedure(const S: string) of object;

{ Exception classes }

  EStreamError = class(Exception);
  EFileStreamError = class(EStreamError)
    constructor Create(ResStringRec: PResStringRec; const FileName: string);
  end;
  EFCreateError = class(EFileStreamError);
  EFOpenError = class(EFileStreamError);
  EFilerError = class(EStreamError);
  EReadError = class(EFilerError);
  EWriteError = class(EFilerError);
  EClassNotFound = class(EFilerError);
  EMethodNotFound = class(EFilerError);
  EInvalidImage = class(EFilerError);
  EResNotFound = class(Exception);
  EListError = System.SysUtils.EListError;
  EBitsError = class(Exception);
  EStringListError = class(Exception);
  EComponentError = class(Exception);
  EParserError = class(Exception);
  EOutOfResources = class(EOutOfMemory);
  EInvalidOperation = class(Exception);

{ Duplicate management }

  TDuplicates = System.Types.TDuplicates;

{ Forward class declarations }

  TStream = class;
  TFiler = class;
  TReader = class;
  TWriter = class;
  TComponent = class;

{ TList class }

  PPointerList = ^TPointerList;
  TPointerList = array of Pointer;
  TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TListSortCompareFunc = reference to function (Item1, Item2: Pointer): Integer;
  TListNotification = (lnAdded, lnExtracted, lnDeleted);

  // these operators are used in Assign and go beyond simply copying
  //   laCopy = dest becomes a copy of the source
  //   laAnd  = intersection of the two lists
  //   laOr   = union of the two lists
  //   laXor  = only those not in both lists
  // the last two operators can actually be thought of as binary operators but
  // their implementation has been optimized over their binary equivalent.
  //   laSrcUnique  = only those unique to source (same as laAnd followed by laXor)
  //   laDestUnique = only those unique to dest   (same as laOr followed by laXor)
  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);

  TList = class;

  TListEnumerator = class
  private
    FIndex: Integer;
    FList: TList;
  public
    constructor Create(AList: TList);
    function GetCurrent: Pointer; inline;
    function MoveNext: Boolean;
    property Current: Pointer read GetCurrent;
  end;

  TList = class(TObject)
  private
    FList: TPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    type
      TDirection = System.Types.TDirection;

    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: NativeInt); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TList;
    function Extract(Item: Pointer): Pointer; inline;
    function ExtractItem(Item: Pointer; Direction: TDirection): Pointer;
    function First: Pointer; inline;
    function GetEnumerator: TListEnumerator;
    function IndexOf(Item: Pointer): Integer;
    function IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Pointer): Integer; inline;
    function RemoveItem(Item: Pointer; Direction: TDirection): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure SortList(const Compare: TListSortCompareFunc);
    procedure Assign(ListA: TList; AOperator: TListAssignOp = laCopy; ListB: TList = nil);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: TPointerList read FList;
  end {$IFDEF AUTOREFCOUNT} deprecated {$ENDIF};

{ TThreadList class }

  TThreadList = class
  private
    FList: TList;
    FLock: TObject;
    FDuplicates: TDuplicates;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer);
    procedure Clear;
    function LockList: TList;
    procedure Remove(Item: Pointer); inline;
    procedure RemoveItem(Item: Pointer; Direction: TList.TDirection);
    procedure UnlockList; inline;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

{ IInterfaceList interface }

  IInterfaceList = interface
  ['{285DEA8A-B865-11D1-AAA7-00C04FB17A72}']
    function Get(Index: Integer): IInterface;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure Put(Index: Integer; const Item: IInterface);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);

    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: IInterface;
    function IndexOf(const Item: IInterface): Integer;
    function Add(const Item: IInterface): Integer;
    procedure Insert(Index: Integer; const Item: IInterface);
    function Last: IInterface;
    function Remove(const Item: IInterface): Integer;
    procedure Lock;
    procedure Unlock;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: IInterface read Get write Put; default;
  end;

{ EXTERNALSYM IInterfaceList}

{ IInterfaceListEx interface }

  TInterfaceListEnumerator = class;

  IInterfaceListEx = interface(IInterfaceList)
    ['{FDB39D70-65B9-4995-9436-6084ACA05DB3}']
    function GetEnumerator: TInterfaceListEnumerator;
  end;

  { EXTERNALSYM IInterfaceListEx}

{ TInterfaceList class }

  TInterfaceList = class;

  TInterfaceListEnumerator = class
  private
    FIndex: Integer;
    FInterfaceList: TInterfaceList;
  public
    constructor Create(AInterfaceList: TInterfaceList);
    function GetCurrent: IInterface; inline;
    function MoveNext: Boolean;
    property Current: IInterface read GetCurrent;
  end;

  TInterfaceList = class(TInterfacedObject, IInterfaceList, IInterfaceListEx)
  private
    FList: TThreadList<IInterface>;
  protected
    { IInterfaceList }
    function Get(Index: Integer): IInterface;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure Put(Index: Integer; const Item: IInterface);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TInterfaceList;
    function First: IInterface;
    function IndexOf(const Item: IInterface): Integer;
    function IndexOfItem(const Item: IInterface; Direction: TList.TDirection): Integer;
    function Add(const Item: IInterface): Integer;
    procedure Insert(Index: Integer; const Item: IInterface);
    function Last: IInterface;
    function Remove(const Item: IInterface): Integer;
    function RemoveItem(const Item: IInterface; Direction: TList.TDirection): Integer;
    procedure Lock;
    procedure Unlock;
    { IInterfaceListEx }
    function GetEnumerator: TInterfaceListEnumerator;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: IInterface read Get write Put; default;
  end;

{ EXTERNALSYM TInterfaceList}

{ TBits class }

  TBits = class
  private
    FSize: Integer;
    FBits: Pointer;
    procedure Error;
    procedure SetSize(Value: Integer);
    procedure SetBit(Index: Integer; Value: Boolean);
    function GetBit(Index: Integer): Boolean;
  public
    destructor Destroy; override;
    function OpenBit: Integer;
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    property Size: Integer read FSize write SetSize;
  end;

{ TPersistent abstract class }

{$M+}

  TPersistent = class(TObject)
  private
    procedure AssignError(Source: TPersistent);
  protected
    procedure AssignTo(Dest: TPersistent); virtual;
    procedure DefineProperties(Filer: TFiler); virtual;
    function GetOwner: TPersistent; dynamic;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); virtual;
    function GetNamePath: string; dynamic;
  end;

{$M-}

{ TPersistent class reference type }

  TPersistentClass = class of TPersistent;

{ TInterfaced Persistent }

  TInterfacedPersistent = class(TPersistent, IInterface)
  private
    FOwnerInterface: IInterface;
  protected
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure AfterConstruction; override;
  end;

{ TRecall class }

  TRecall = class(TObject)
  private
    FStorage, FReference: TPersistent;
  public
    constructor Create(AStorage, AReference: TPersistent);
    destructor Destroy; override;
    procedure Store;
    procedure Forget;
    property Reference: TPersistent read FReference;
  end;

{ TCollection class }

  TCollection = class;

  TCollectionItem = class(TPersistent)
  private
    [Unsafe] FCollection: TCollection;
    FID: Integer;
    function GetIndex: Integer;
  protected
    procedure Changed(AllItems: Boolean);
    function GetOwner: TPersistent; override;
    function GetDisplayName: string; virtual;
    procedure SetCollection(Value: TCollection); virtual;
    procedure SetIndex(Value: Integer); virtual;
    procedure SetDisplayName(const Value: string); virtual;
  public
    constructor Create(Collection: TCollection); virtual;
    destructor Destroy; override;
    procedure Release; virtual;
    function GetNamePath: string; override;
    property Collection: TCollection read FCollection write SetCollection;
    property ID: Integer read FID;
    property Index: Integer read GetIndex write SetIndex;
    property DisplayName: string read GetDisplayName write SetDisplayName;
  end;

  TCollectionItemClass = class of TCollectionItem;
  TCollectionNotification = (cnAdded, cnExtracting, cnDeleting);

  TCollectionEnumerator = class
  private
    FIndex: Integer;
    FCollection: TCollection;
  public
    constructor Create(ACollection: TCollection);
    function GetCurrent: TCollectionItem; inline;
    function MoveNext: Boolean;
    property Current: TCollectionItem read GetCurrent;
  end;

  TCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    FItems: TList<TCollectionItem>;
    FUpdateCount: Integer;
    FNextID: Integer;
    FPropName: string;
    function GetCapacity: Integer;
    function GetCount: Integer; inline;
    function GetPropName: string;
    procedure InsertItem(Item: TCollectionItem);
    procedure RemoveItem(Item: TCollectionItem);
    procedure SetCapacity(Value: Integer);
  protected
    procedure Added(var Item: TCollectionItem); virtual; deprecated;
    procedure Deleting(Item: TCollectionItem); virtual; deprecated;
    property NextID: Integer read FNextID;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); virtual;
    { Design-time editor support }
    function GetAttrCount: Integer; dynamic;
    function GetAttr(Index: Integer): string; dynamic;
    function GetItemAttr(Index, ItemIndex: Integer): string; dynamic;
    procedure Changed;
    function GetItem(Index: Integer): TCollectionItem;
    procedure SetItem(Index: Integer; Value: TCollectionItem);
    procedure SetItemName(Item: TCollectionItem); virtual;
    procedure Update(Item: TCollectionItem); virtual;
    property PropName: string read GetPropName write FPropName;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Owner: TPersistent;
    function Add: TCollectionItem;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure Clear;
    procedure ClearAndResetID;
    procedure Delete(Index: Integer);
    procedure EndUpdate; virtual;
    function FindItemID(ID: Integer): TCollectionItem;
    function GetEnumerator: TCollectionEnumerator;
    function GetNamePath: string; override;
    function Insert(Index: Integer): TCollectionItem;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property ItemClass: TCollectionItemClass read FItemClass;
    property Items[Index: Integer]: TCollectionItem read GetItem write SetItem;
  end;

{ Collection class that maintains an "Owner" in order to obtain property
  path information at design-time }

  TOwnedCollection = class(TCollection)
  private
    [Weak] FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
  end;

  TStrings = class;

{ TGetModuleProc }
{ Used in the TFormDesigner class to allow component/property editors access
  to project specific information }

  TGetModuleProc = procedure(const FileName, UnitName, FormName,
    DesignClass: string; CoClasses: TStrings) of object;

{ IStringsAdapter interface }
{ Maintains link between TStrings and IStrings implementations }

  IStringsAdapter = interface
    ['{739C2F34-52EC-11D0-9EA6-0020AF3D82DA}']
    procedure ReferenceStrings(S: TStrings);
    procedure ReleaseStrings;
  end;

{ TStrings class }

  TStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator,
    sdLineBreak, sdStrictDelimiter);

  TStringsOption = (soStrictDelimiter, soWriteBOM, soTrailingLineBreak,
    soUseLocale);
  TStringsOptions = set of TStringsOption;

  TStringsEnumerator = class
  private
    FIndex: Integer;
    FStrings: TStrings;
  public
    constructor Create(AStrings: TStrings);
    function GetCurrent: string; inline;
    function MoveNext: Boolean;
    property Current: string read GetCurrent;
  end;

  TStrings = class(TPersistent)
  private
    FEncoding: TEncoding;
    FDefaultEncoding: TEncoding;
    FLineBreak: string;
    FAdapter: IStringsAdapter;
    FUpdateCount: Integer;
    FDelimiter: Char;
    FQuoteChar: Char;
    FNameValueSeparator: Char;
    FOptions: TStringsOptions;
    function GetCommaText: string;
    function GetDelimitedText: string;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: string);
    procedure SetDelimitedText(const Value: string);
    procedure SetStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: string);
    procedure WriteData(Writer: TWriter);
    function GetStrictDelimiter: Boolean; inline;
    procedure SetStrictDelimiter(const Value: Boolean);
    function GetValueFromIndex(Index: Integer): string;
    procedure SetValueFromIndex(Index: Integer; const Value: string);
    procedure SetDefaultEncoding(const Value: TEncoding);
    function GetTrailingLineBreak: Boolean; inline;
    procedure SetTrailingLineBreak(const Value: Boolean);
    function GetUseLocale: Boolean; inline;
    procedure SetUseLocale(const Value: Boolean);
    function GetWriteBOM: Boolean; inline;
    procedure SetWriteBOM(const Value: Boolean);
    function GetUpdating: Boolean; inline;
    function GetKeyName(Index: Integer): string;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: string): string; overload; inline;
    function ExtractName(const S: string; AllNames: Boolean): string; overload;
    function Get(Index: Integer): string; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetEncoding(const Value: TEncoding); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    function CompareStrings(const S1, S2: string): Integer; virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): Integer; virtual;
    /// <summary>
    ///    Adds Name=Value list item using current NameValueSeparator. Method
    ///    returns reference to this string list, allowing to populate list
    ///    using fluent coding style.
    /// </summary>
    function AddPair(const Name, Value: string): TStrings; overload;
    /// <summary>
    ///    Adds Name=Value list item and corresponding AObject using current
    ///    NameValueSeparator. Method returns reference to this string list,
    ///    allowing to populate list using fluent coding style.
    /// </summary>
    function AddPair(const Name, Value: string; AObject: TObject): TStrings; overload;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;
    procedure Append(const S: string);
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure AddStrings(const Strings: TArray<string>); overload;
    procedure AddStrings(const Strings: TArray<string>; const Objects: TArray<TObject>); overload;
    procedure Assign(Source: TPersistent); override;
    /// <summary>
    ///    Assigns the strings from another TStrings object to this list.
    ///    Before assignment this list will be erased. The main difference
    ///    between Assign and SetStrings methods is that SetStrings method
    ///    preserves other properties, like QuoteChar or Delimiter.
    /// </summary>
    procedure SetStrings(Source: TStrings);
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TStrings): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TStringsEnumerator;
    function GetText: PChar; virtual;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: string); overload; virtual;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure LoadFromStream(Stream: TStream); overload; virtual;
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); overload; virtual;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure SaveToStream(Stream: TStream); overload; virtual;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload; virtual;
    procedure SetText(Text: PChar); virtual;
    function ToStringArray: TArray<string>;
    function ToObjectArray: TArray<TObject>;
    /// <summary>
    ///    Returns True, when UpdateCount is greater than zero. It is greater
    ///    than zero inside of BeginUpdate / EndUpdate calls.
    /// </summary>
    property Updating: Boolean read GetUpdating;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property DefaultEncoding: TEncoding read FDefaultEncoding write SetDefaultEncoding;
    property Delimiter: Char read FDelimiter write FDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Encoding: TEncoding read FEncoding;
    property LineBreak: string read FLineBreak write FLineBreak;
    property Names[Index: Integer]: string read GetName;
    /// <summary>
    ///    When the list of strings includes strings that are name-value pairs or just names,
    ///    read Keys to access the name part of a string. If the string is not a name-value
    ///    pair, Keys returns full string. Assigning Keys will write new name for name-value
    ///    pair. This is in contrast to Names property.
    /// </summary>
    property KeyNames[Index: Integer]: string read GetKeyName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property Values[const Name: string]: string read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read FNameValueSeparator write FNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
    property WriteBOM: Boolean read GetWriteBOM write SetWriteBOM;
    /// <summary>
    ///    When TrailingLineBreak property is True (default value) then Text property
    ///    will contain line break after last line. When it is False, then Text value
    ///    will not contain line break after last line. This also may be controlled
    ///    by soTrailingLineBreak option.
    /// </summary>
    property TrailingLineBreak: Boolean read GetTrailingLineBreak write SetTrailingLineBreak;
    /// <summary>
    ///    When UseLocale property is True (default value) then string list will use
    ///    AnsiCompareStr/AnsiCompareText functions to compare strings. When it is False
    ///    then CompareStr/CompareText functions will be used. This also may be controlled
    ///    by soUseLocale option.
    /// </summary>
    property UseLocale: Boolean read GetUseLocale write SetUseLocale;
    /// <summary>
    ///    Options property controls different aspects of string list. Each option
    ///    corresponds to one of the Boolean string list properties, eg soUseLocale
    ///    to UseLocale property.
    /// </summary>
    property Options: TStringsOptions read FOptions write FOptions;
  end;

{ TStringList class }

  TStringList = class;

  PStringItem = ^TStringItem;
  TStringItem = record
    FString: string;
    FObject: TObject;
  end;

  PStringItemList = ^TStringItemList;
  TStringItemList = array of TStringItem;
  TStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer;

  TStringList = class(TStrings)
  private
    FList: TStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure ExchangeItems(Index1, Index2: Integer);
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: string): Integer; override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); virtual;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    /// <summary>
    ///    This constructor creates new string list with specified QuoteChar
    ///    and Delimiter property values.
    /// </summary>
    constructor Create(QuoteChar, Delimiter: Char); overload;
    /// <summary>
    ///    This constructor creates new string list with specified QuoteChar,
    ///    Delimiter and Options property values.
    /// </summary>
    constructor Create(QuoteChar, Delimiter: Char; Options: TStringsOptions); overload;
    /// <summary>
    ///    This constructor creates new string list with specified Duplicates,
    ///    Sorted and CaseSensitive property values.
    /// </summary>
    constructor Create(Duplicates: TDuplicates; Sorted: Boolean; CaseSensitive: Boolean); overload;
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

{ TStream abstract class }

  TStream = class(TObject)
  private
    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    procedure SetSize64(const NewSize: Int64);
    function Skip(Amount: Int64): Int64;
  protected
    function GetSize: Int64; virtual;
    procedure SetSize(NewSize: Longint); overload; virtual; deprecated;
    procedure SetSize(const NewSize: Int64); overload; virtual;
  public

    function Read(var Buffer; Count: Longint): Longint; overload; virtual;
    function Write(const Buffer; Count: Longint): Longint; overload; virtual;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; overload; virtual;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; overload; virtual;

    function Read(var Buffer: TBytes; Count: Longint): Longint; overload;
    function Write(const Buffer: TBytes; Count: Longint): Longint; overload;

    function Read64(Buffer: TBytes; Offset, Count: Int64): Int64; virtual;
    function Write64(const Buffer: TBytes; Offset, Count: Int64): Int64; virtual;

    function ReadData(      Buffer: Pointer; Count: NativeInt): NativeInt; overload;
    function ReadData(const Buffer: TBytes; Count: NativeInt): NativeInt; overload;

    function ReadData(var Buffer: Boolean): NativeInt; overload;
    function ReadData(var Buffer: Boolean; Count: NativeInt): NativeInt; overload;
{$IFDEF NEXTGEN}
    function ReadData(var Buffer: System.UTF8Char): NativeInt; overload;
    function ReadData(var Buffer: System.UTF8Char; Count: NativeInt): NativeInt; overload;
{$ELSE !NEXTGEN}
    function ReadData(var Buffer: AnsiChar): NativeInt; overload;
    function ReadData(var Buffer: AnsiChar; Count: NativeInt): NativeInt; overload;
{$ENDIF NEXTGEN}
    function ReadData(var Buffer: Char): NativeInt; overload;
    function ReadData(var Buffer: Char; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Int8): NativeInt; overload;
    function ReadData(var Buffer: Int8; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: UInt8): NativeInt; overload;
    function ReadData(var Buffer: UInt8; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Int16): NativeInt; overload;
    function ReadData(var Buffer: Int16; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: UInt16): NativeInt; overload;
    function ReadData(var Buffer: UInt16; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Int32): NativeInt; overload;
    function ReadData(var Buffer: Int32; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: UInt32): NativeInt; overload;
    function ReadData(var Buffer: UInt32; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Int64): NativeInt; overload;
    function ReadData(var Buffer: Int64; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: UInt64): NativeInt; overload;
    function ReadData(var Buffer: UInt64; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Single): NativeInt; overload;
    function ReadData(var Buffer: Single; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Double): NativeInt; overload;
    function ReadData(var Buffer: Double; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: Extended): NativeInt; overload;
    function ReadData(var Buffer: Extended; Count: NativeInt): NativeInt; overload;
    function ReadData(var Buffer: TExtended80Rec): NativeInt; overload;
    function ReadData(var Buffer: TExtended80Rec; Count: NativeInt): NativeInt; overload;

    function ReadData<T>(var Buffer: T): NativeInt; overload;
    function ReadData<T>(var Buffer: T; Count: NativeInt): NativeInt; overload;

    function WriteData(const Buffer: TBytes; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Pointer; Count: NativeInt): NativeInt; overload;

    function WriteData(const Buffer: Boolean): NativeInt; overload;
    function WriteData(const Buffer: Boolean; Count: NativeInt): NativeInt; overload;
{$IFDEF NEXTGEN}
    function WriteData(const Buffer: UTF8Char): NativeInt; overload;
    function WriteData(const Buffer: UTF8Char; Count: NativeInt): NativeInt; overload;
{$ELSE !NEXTGEN}
    function WriteData(const Buffer: AnsiChar): NativeInt; overload;
    function WriteData(const Buffer: AnsiChar; Count: NativeInt): NativeInt; overload;
{$ENDIF NEXTGEN}
    function WriteData(const Buffer: Char): NativeInt; overload;
    function WriteData(const Buffer: Char; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Int8): NativeInt; overload;
    function WriteData(const Buffer: Int8; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: UInt8): NativeInt; overload;
    function WriteData(const Buffer: UInt8; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Int16): NativeInt; overload;
    function WriteData(const Buffer: Int16; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: UInt16): NativeInt; overload;
    function WriteData(const Buffer: UInt16; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Int32): NativeInt; overload;
    function WriteData(const Buffer: Int32; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: UInt32): NativeInt; overload;
    function WriteData(const Buffer: UInt32; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Int64): NativeInt; overload;
    function WriteData(const Buffer: Int64; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: UInt64): NativeInt; overload;
    function WriteData(const Buffer: UInt64; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Single): NativeInt; overload;
    function WriteData(const Buffer: Single; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Double): NativeInt; overload;
    function WriteData(const Buffer: Double; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: Extended): NativeInt; overload;
    function WriteData(const Buffer: Extended; Count: NativeInt): NativeInt; overload;
    function WriteData(const Buffer: TExtended80Rec): NativeInt; overload;
    function WriteData(const Buffer: TExtended80Rec; Count: NativeInt): NativeInt; overload;

    function WriteData<T>(const Buffer: T): NativeInt; overload;
    function WriteData<T>(const Buffer: T; Count: NativeInt): NativeInt; overload;

    function Seek32(const Offset: Integer; Origin: TSeekOrigin): Int64; overload; inline;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; virtual;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; virtual;
    function Seek(const Offset: Int64; Origin: Word): Int64; overload; deprecated; inline;

    procedure ReadBuffer(var Buffer; Count: NativeInt); overload;
    procedure ReadBuffer(var Buffer: TBytes; Count: NativeInt); overload;
    procedure ReadBuffer(var Buffer: TBytes; Offset, Count: NativeInt); overload;

    procedure ReadBufferData(var Buffer: Boolean); overload;
    procedure ReadBufferData(var Buffer: Boolean; Count: NativeInt); overload;
{$IFDEF NEXTGEN}
    procedure ReadBufferData(var Buffer: UTF8Char); overload;
    procedure ReadBufferData(var Buffer: UTF8Char; Count: NativeInt); overload;
{$ELSE !NEXTGEN}
    procedure ReadBufferData(var Buffer: AnsiChar); overload;
    procedure ReadBufferData(var Buffer: AnsiChar; Count: NativeInt); overload;
{$ENDIF NEXTGEN}
    procedure ReadBufferData(var Buffer: Char); overload;
    procedure ReadBufferData(var Buffer: Char; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Int8); overload;
    procedure ReadBufferData(var Buffer: Int8; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: UInt8); overload;
    procedure ReadBufferData(var Buffer: UInt8; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Int16); overload;
    procedure ReadBufferData(var Buffer: Int16; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: UInt16); overload;
    procedure ReadBufferData(var Buffer: UInt16; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Int32); overload;
    procedure ReadBufferData(var Buffer: Int32; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: UInt32); overload;
    procedure ReadBufferData(var Buffer: UInt32; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Int64); overload;
    procedure ReadBufferData(var Buffer: Int64; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: UInt64); overload;
    procedure ReadBufferData(var Buffer: UInt64; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Single); overload;
    procedure ReadBufferData(var Buffer: Single; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Double); overload;
    procedure ReadBufferData(var Buffer: Double; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: Extended); overload;
    procedure ReadBufferData(var Buffer: Extended; Count: NativeInt); overload;
    procedure ReadBufferData(var Buffer: TExtended80Rec); overload;
    procedure ReadBufferData(var Buffer: TExtended80Rec; Count: NativeInt); overload;

    procedure WriteBuffer(const Buffer; Count: NativeInt); overload;
    procedure WriteBuffer(const Buffer: TBytes; Count: NativeInt); overload;
    procedure WriteBuffer(const Buffer: TBytes; Offset, Count: NativeInt); overload;

    procedure WriteBufferData(var Buffer: Integer; Count: NativeInt); overload;

    function CopyFrom(const Source: TStream; Count: Int64): Int64;
    function ReadComponent(const Instance: TComponent): TComponent;
    function ReadComponentRes(const Instance: TComponent): TComponent;
    procedure WriteComponent(const Instance: TComponent);
    procedure WriteComponentRes(const ResName: string; const Instance: TComponent);
    procedure WriteDescendent(const Instance, Ancestor: TComponent);
    procedure WriteDescendentRes(const ResName: string; const Instance, Ancestor: TComponent);
    procedure WriteResourceHeader(const ResName: string; out FixupInfo: Integer);
    procedure FixupResourceHeader(FixupInfo: Integer);
    procedure ReadResHeader;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize64;
  end;

  IStreamPersist = interface
    ['{B8CD12A3-267A-11D4-83DA-00C04F60B2DD}']
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

{ THandleStream class }

  THandleStream = class(TStream)
  protected
    FHandle: THandle;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AHandle: THandle);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;


    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: THandle read FHandle;
  end;

{ TFileStream class }

  TFileStream = class(THandleStream)
  strict private
    FFileName: string;
  public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

{ TBufferedFileStream class }

  /// <summary>
  ///    TBufferedFileStream adds buffering to the TFileStream. This optimizes
  ///    multiple consecutive small writes or reads. TBufferedFileStream will
  ///    not give performance gain, when there are random position reads or
  ///    writes, or large reads or writes. TBufferedFileStream may be used
  ///    as a drop in replacement for TFileStream.
  /// </summary>
  TBufferedFileStream = class(TFileStream)
  private
    FFilePos, FBufStartPos, FBufEndPos: Int64;
    FBuffer: PByte;
    FBufferSize: Integer;
    FModified: Boolean;
    FBuffered: Boolean;
  protected
    procedure SetSize(const NewSize: Int64); override;
    /// <summary>
    ///    SyncBuffer writes buffered and not yet written data to the file.
    ///    When ReRead is True, then buffer will be repopulated from the file.
    ///    When ReRead is False, then buffer will be emptied, so next read or
    ///    write operation will repopulate buffer.
    /// </summary>
    procedure SyncBuffer(ReRead: Boolean);
  public
    constructor Create(const AFileName: string; Mode: Word; BufferSize: Integer = 32768); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal; BufferSize: Integer = 32768); overload;
    destructor Destroy; override;
    /// <summary>
    ///    FlushBuffer writes buffered and not yet written data to the file.
    /// </summary>
    procedure FlushBuffer; inline;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ TCustomMemoryStream abstract class }

  TCustomMemoryStream = class(TStream)
  private
    FMemory: Pointer;
    FSize, FPosition: NativeInt;
  protected
    procedure SetPointer(Ptr: Pointer; const Size: NativeInt);
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string);
    property Memory: Pointer read FMemory;
  end; // deprecated 'Use TBytesStream';

{ TMemoryStream }

  TMemoryStream = class(TCustomMemoryStream)
  private
    FCapacity: NativeInt;
  protected
    procedure SetCapacity(NewCapacity: NativeInt); virtual;
    function Realloc(var NewCapacity: Longint): Pointer; virtual;
    property Capacity: NativeInt read FCapacity write SetCapacity;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
  end; // deprecated 'Use TBytesStream';

{ TBytesStream }

  TBytesStream = class(TMemoryStream)
  private
    FBytes: TBytes;
  protected
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    constructor Create(const ABytes: TBytes); overload;
    property Bytes: TBytes read FBytes;
  end;

{ TStringStream }

  TStringStream = class(TBytesStream)
  private
    FEncoding: TEncoding;
    FOwnsEncoding: Boolean;
    function GetDataString: string;
  public
    constructor Create; overload;
    constructor Create(const AString: string); overload;
{$IFNDEF NEXTGEN}
    constructor Create(const AString: RawByteString); overload;
{$ENDIF !NEXTGEN}
    constructor Create(const AString: string; AEncoding: TEncoding; AOwnsEncoding: Boolean = True); overload;
    constructor Create(const AString: string; ACodePage: Integer); overload;
    constructor Create(const ABytes: TBytes); overload;
    destructor Destroy; override;
    function ReadString(Count: Integer): string;
    procedure WriteString(const AString: string);
    property DataString: string read GetDataString;
    property Encoding: TEncoding read FEncoding;
  end;

{ TResourceStream }

  TResourceStream = class(TCustomMemoryStream)
  private
    HResInfo: THandle;
    HGlobal: THandle;
    procedure Initialize(Instance: THandle; Name, ResType: PChar; FromID: Boolean);
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
    constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override; final;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override; final;
  end;

{ TStreamAdapter }
{ Implements OLE IStream on VCL TStream }

  TStreamOwnership = (soReference, soOwned);

  TStreamAdapter = class(TInterfacedObject, IStream)
  private
    FStream: TStream;
    FOwnership: TStreamOwnership;
  public
    constructor Create(Stream: TStream; Ownership: TStreamOwnership = soReference);
    destructor Destroy; override;
    function Read(pv: Pointer; cb: FixedUInt; pcbRead: PFixedUInt): HResult; virtual; stdcall;
    function Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult; virtual; stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: DWORD; out libNewPosition: LargeUInt): HResult; virtual; stdcall;
    function SetSize(libNewSize: LargeUInt): HResult; virtual; stdcall;
    function CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt; out cbWritten: LargeUInt): HResult; virtual; stdcall;
    function Commit(grfCommitFlags: DWORD): HResult; virtual; stdcall;
    function Revert: HResult; virtual; stdcall;
    function LockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: DWORD): HResult; virtual; stdcall;
    function UnlockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: DWORD): HResult; virtual; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult; virtual; stdcall;
    function Clone(out stm: IStream): HResult; virtual; stdcall;
    property Stream: TStream read FStream;
    property StreamOwnership: TStreamOwnership read FOwnership write FOwnership;
  end;

{ TClassFinder }

  TGetClass = procedure (AClass: TPersistentClass) of object;

  TClassFinder = class
  private
    FGroups: TList<TObject>;
    FClass: TPersistentClass;
  public
    constructor Create(AClass: TPersistentClass = nil; AIncludeActiveGroups: Boolean = False);
{$IFNDEF AUTOREFCOUNT}
    destructor Destroy; override;
{$ENDIF}
    function GetClass(const AClassName: string): TPersistentClass;
    procedure GetClasses(Proc: TGetClass);

    property FinderClass: TPersistentClass read FClass;
  end;

{ TFiler }

  TValueType = (vaNull, vaList, vaInt8, vaInt16, vaInt32, vaExtended,
    vaString, vaIdent, vaFalse, vaTrue, vaBinary, vaSet, vaLString,
    vaNil, vaCollection, vaSingle, vaCurrency, vaDate, vaWString,
    vaInt64, vaUTF8String, vaDouble);

  TFilerFlag = (ffInherited, ffChildPos, ffInline);
  TFilerFlags = set of TFilerFlag;

  TReaderProc = procedure(Reader: TReader) of object;
  TWriterProc = procedure(Writer: TWriter) of object;
  TStreamProc = procedure(Stream: TStream) of object;

  IInterfaceComponentReference = interface
    ['{E28B1858-EC86-4559-8FCD-6B4F824151ED}']
    function GetComponent: TComponent;
  end;

  TFiler = class(TObject)
  private
    FStream: TStream;
    FBuffer: TBytes;
    FBufSize: NativeInt;
    FBufPos: NativeInt;
    FBufEnd: NativeInt;
    FRoot: TComponent;
    FLookupRoot: TComponent;
    FAncestor: TPersistent;
    FIgnoreChildren: Boolean;
  protected
    procedure SetRoot(Value: TComponent); virtual;
  public
    constructor Create(Stream: TStream; BufSize: Integer);
    destructor Destroy; override;
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); virtual; abstract;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); virtual; abstract;
    procedure FlushBuffer; virtual; abstract;
    property Root: TComponent read FRoot write SetRoot;
    property LookupRoot: TComponent read FLookupRoot;
    property Ancestor: TPersistent read FAncestor write FAncestor;
    property IgnoreChildren: Boolean read FIgnoreChildren write FIgnoreChildren;
  end;

{ TComponent class reference type }

  TComponentClass = class of TComponent;

{ Custom variant stream support }

  IVarStreamable = interface
    ['{D60BA026-5E42-4C2A-BB01-3F1C8F30A28E}']
    procedure StreamIn(var Dest: TVarData; const Stream: TStream);
    procedure StreamOut(const Source: TVarData; const Stream: TStream);
  end;

{ TReader }

  TFindMethodEvent = procedure (Reader: TReader; const MethodName: string;
    var Address: Pointer; var Error: Boolean) of object;
  TSetNameEvent = procedure (Reader: TReader; Component: TComponent;
    var Name: string) of object;
  TReferenceNameEvent = procedure (Reader: TReader; var Name: string) of object;
  TAncestorNotFoundEvent = procedure (Reader: TReader; const ComponentName: string;
    ComponentClass: TPersistentClass; var Component: TComponent) of object;
  TReadComponentsProc = procedure (Component: TComponent) of object;
  TReaderError = procedure (Reader: TReader; const Message: string; var Handled: Boolean) of object;
  TFindComponentClassEvent = procedure (Reader: TReader; const ClassName: string;
    var ComponentClass: TComponentClass) of object;
  TCreateComponentEvent = procedure (Reader: TReader;
    ComponentClass: TComponentClass; var Component: TComponent) of object;
  TFindMethodInstanceEvent = procedure (Reader: TReader; const MethodName: string;
    var AMethod: TMethod; var Error: Boolean) of object;
  TFindComponentInstanceEvent = procedure (Reader: TReader; const Name: string;
    var Instance: Pointer) of object;

  TReader = class(TFiler)
  private
    FOwner: TComponent;
    FParent: TComponent;
    FFixups: TList<TObject>;
    FLoaded: TList<TComponent>;
    FOnFindMethod: TFindMethodEvent;
    FOnFindMethodInstance: TFindMethodInstanceEvent;
    FOnSetName: TSetNameEvent;
    FOnReferenceName: TReferenceNameEvent;
    FOnAncestorNotFound: TAncestorNotFoundEvent;
    FOnError: TReaderError;
    FOnFindComponentClass: TFindComponentClassEvent;
    FOnCreateComponent: TCreateComponentEvent;
    FOnFindComponentInstance: TFindComponentInstanceEvent;
    FPropName: string;
    FFinder: TClassFinder;
    FCanHandleExcepts: Boolean;
    procedure DoFixupReferences;
    procedure EnsureAtLeast(NumBytes: Integer);
    procedure FreeFixups;
    function GetFieldClass(const Instance: TObject; const ClassName: string): TPersistentClass;
    function GetPosition: NativeInt;
    procedure ReadBuffer(Keeping: Integer = 0; MoveBuffer: Boolean = True);
    procedure ReadDataInner(const Instance: TComponent);
    function FindComponentClass(const ClassName: string): TComponentClass;
  protected
    function Error(const Message: string): Boolean; virtual;
    function FindAncestorComponent(const Name: string;
      ComponentClass: TPersistentClass): TComponent; virtual;
    function FindMethodInstance(Root: TComponent; const MethodName: string): TMethod; virtual;
    function FindMethod(Root: TComponent; const MethodName: string): Pointer; virtual;
    procedure SetName(Component: TComponent; var Name: string); virtual;
    procedure ReadProperty(AInstance: TPersistent);
    procedure ReadPropValue(const Instance: TPersistent; PropInfo: Pointer);
    procedure ReferenceName(var Name: string); virtual;
    procedure PropertyError(const Name: string);
    procedure ReadData(const Instance: TComponent);
    function ReadSet(SetType: Pointer): Integer;
    procedure SetPosition(Value: NativeInt);
    procedure SkipBytes(Count: Integer);
    procedure SkipSetBody;
    procedure SkipProperty;
    procedure SkipComponent(SkipHeader: Boolean);
    property PropName: string read FPropName;
    property CanHandleExceptions: Boolean read FCanHandleExcepts;
  public
    destructor Destroy; override;
    procedure BeginReferences;
    procedure CheckValue(Value: TValueType);
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
    function EndOfList: Boolean;
    procedure EndReferences;
    procedure FixupReferences;
    procedure FlushBuffer; override;
    function NextValue: TValueType;

    procedure Read(var Buffer; Count: NativeInt); overload;

    procedure Read(Buffer: TBytes; Offset, Count: NativeInt); overload;
    procedure Read(Buffer: TBytes; Count: NativeInt); overload;

{$IFNDEF NEXTGEN}
//    procedure Read(Buffer: pointer; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: AnsiChar; Count: NativeInt); overload;
{$ENDIF !NEXTGEN}
    procedure ReadVar(var Buffer: Char; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: Int8; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: UInt8; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: Int16; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: UInt16; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: Int32; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: UInt32; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: Int64; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: UInt64; Count: NativeInt); overload;

    procedure ReadVar(var Buffer: Single; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: Double; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: Extended; Count: NativeInt); overload;
    procedure ReadVar(var Buffer: TExtended80Rec; Count: NativeInt); overload;

    function ReadBoolean: Boolean;
    function ReadChar: Char;
{$IFNDEF NEXTGEN}
    function ReadWideChar: WideChar; deprecated 'Use ReadChar';
{$ENDIF !NEXTGEN}
    procedure ReadCollection(const Collection: TCollection);
    function ReadComponent(Component: TComponent): TComponent;
    procedure ReadComponents(const AOwner, AParent: TComponent;
      Proc: TReadComponentsProc);
    function ReadFloat: Extended;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadCurrency: Currency;
    function ReadDate: TDateTime;
    function ReadIdent: string;
    function ReadInteger: Integer;
    function ReadInt64: Int64;
    procedure ReadListBegin; inline;
    procedure ReadListEnd; inline;
    procedure ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer); virtual;
    function ReadRootComponent(const Root: TComponent): TComponent;
    procedure ReadSignature;
    function ReadStr: string;
    function ReadString: string;
{$IFNDEF NEXTGEN}
    function ReadWideString: string; deprecated 'Use ReadString';
{$ENDIF !NEXTGEN}
    function ReadValue: TValueType;
    function ReadVariant: Variant;
    procedure CopyValue(const Writer: TWriter);
    procedure SkipValue;
    property Owner: TComponent read FOwner write FOwner;
    property Parent: TComponent read FParent write FParent;
    property Position: NativeInt read GetPosition write SetPosition;
    property OnError: TReaderError read FOnError write FOnError;
    property OnFindMethod: TFindMethodEvent read FOnFindMethod write FOnFindMethod;
    property OnFindMethodInstance: TFindMethodInstanceEvent read FOnFindMethodInstance write FOnFindMethodInstance;
    property OnSetName: TSetNameEvent read FOnSetName write FOnSetName;
    property OnReferenceName: TReferenceNameEvent read FOnReferenceName write FOnReferenceName;
    property OnAncestorNotFound: TAncestorNotFoundEvent read FOnAncestorNotFound write FOnAncestorNotFound;
    property OnCreateComponent: TCreateComponentEvent read FOnCreateComponent write FOnCreateComponent;
    property OnFindComponentClass: TFindComponentClassEvent read FOnFindComponentClass write FOnFindComponentClass;
    property OnFindComponentInstance: TFindComponentInstanceEvent read FOnFindComponentInstance write FOnFindComponentInstance;
  end;

{ TWriter }

  TFindAncestorEvent = procedure (Writer: TWriter; Component: TComponent;
    const Name: string; var Ancestor, RootAncestor: TComponent) of object;
  TFindMethodNameEvent = procedure (Writer: TWriter; AMethod: TMethod;
    var MethodName: string) of object;
  TGetLookupInfoEvent = procedure(var Ancestor: TPersistent;
    var Root, LookupRoot, RootAncestor: TComponent) of object;

  TWriter = class(TFiler)
  private
    FRootAncestor: TComponent;
    FPropPath: string;
    FAncestorList: TList<TComponent>;
    FAncestorPos: Integer;
    FChildPos: Integer;
    FOnFindAncestor: TFindAncestorEvent;
    FOnFindMethodName: TFindMethodNameEvent;
    FUseQualifiedNames: Boolean;
    procedure AddAncestor(Component: TComponent);
    procedure EnsureAtLeast(Amount: Integer);
    function GetPosition: NativeInt;
    procedure SetPosition(Value: NativeInt);
    procedure WriteBuffer;
    procedure WriteData(Instance: TComponent); virtual; // linker optimization
    procedure GetLookupInfo(var Ancestor: TPersistent;
      var Root, LookupRoot, RootAncestor: TComponent);
  protected
    function FindMethodName(AMethod: TMethod): string; virtual;
    procedure SetRoot(Value: TComponent); override;
    procedure WriteBinary(WriteData: TStreamProc);
    procedure WritePrefix(Flags: TFilerFlags; AChildPos: Integer);
    procedure WriteProperty(const Instance: TPersistent; PropInfo: PPropInfo);
    procedure WritePropName(const PropName: string);
    procedure WriteValue(Value: TValueType);
  public
    destructor Destroy; override;
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
    procedure FlushBuffer; override;

    procedure Write(const Buffer; Count: NativeInt); overload;

    procedure Write(Buffer: TBytes; Offset, Count: NativeInt); overload;
    procedure Write(Buffer: TBytes; Count: NativeInt); overload;

{$IFNDEF NEXTGEN}
//    procedure Write(const Buffer: Pointer; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: AnsiChar; Count: NativeInt); overload;
{$ENDIF !NEXTGEN}
    procedure WriteVar(const Buffer: Char; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: Int8; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: UInt8; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: Int16; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: UInt16; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: Int32; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: UInt32; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: Int64; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: UInt64; Count: NativeInt); overload;

    procedure WriteVar(const Buffer: Single; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: Double; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: Extended; Count: NativeInt); overload;
    procedure WriteVar(const Buffer: TExtended80Rec; Count: NativeInt); overload;

    procedure WriteBoolean(Value: Boolean);
    procedure WriteCollection(const Value: TCollection);
    procedure WriteComponent(Component: TComponent);
    procedure WriteChar(Value: Char);
{$IFNDEF NEXTGEN}
    procedure WriteWideChar(Value: WideChar); deprecated 'Use WriteChar';
{$ENDIF !NEXTGEN}
    procedure WriteDescendent(const Root: TComponent; const AAncestor: TComponent);
    procedure WriteFloat(const Value: Extended);
    procedure WriteSingle(const Value: Single);
    procedure WriteDouble(const Value: Double);
    procedure WriteCurrency(const Value: Currency);
    procedure WriteDate(const Value: TDateTime);
    procedure WriteIdent(const Ident: string);
// TODO -cELBRUS_LONGINT64 : Verify TWriter.WriteInteger (change LongInt to Integer)
    procedure WriteInteger(Value: Integer); overload;
    procedure WriteInteger(Value: Int64); overload;
    procedure WriteListBegin; inline;
    procedure WriteListEnd; inline;
    procedure WriteProperties(const Instance: TPersistent);
    procedure WriteRootComponent(const Root: TComponent);
    procedure WriteSignature;
{$IFDEF NEXTGEN}
    procedure WriteStr(const Value: string); deprecated 'Use WriteUTF8Str';
{$ELSE !NEXTGEN}
    procedure WriteStr(const Value: AnsiString);
{$ENDIF NEXTGEN}
    procedure WriteUTF8Str(const Value: string); inline;
    procedure WriteString(const Value: string);
{$IFNDEF NEXTGEN}
    procedure WriteWideString(const Value: string); deprecated 'Use WriteString';
{$ENDIF !NEXTGEN}
    procedure WriteVariant(const Value: Variant);
    property Position: NativeInt read GetPosition write SetPosition;
    property RootAncestor: TComponent read FRootAncestor write FRootAncestor;
    property OnFindAncestor: TFindAncestorEvent read FOnFindAncestor write FOnFindAncestor;
    property OnFindMethodName: TFindMethodNameEvent read FOnFindMethodName write FOnFindMethodName;
    property UseQualifiedNames: Boolean read FUseQualifiedNames write FUseQualifiedNames;
  end;

{ TParser }

  TParserErrorEvent = procedure (Sender: TObject; const Message: string; var Handled: Boolean) of object;

  TParser = class(TObject)
  private type
    TCharType = (ctOther, ctLetterStart, ctLetterNumber, ctNumber, ctHash, ctQuote, ctDollar, ctDash);
  private
    FStream: TStream;
    FOrigin: NativeInt;
    FBuffer: TBytes;
    FBufPtr: NativeInt;
    FBufEnd: NativeInt;
    FSourcePtr: NativeInt;
    FSourceEnd: NativeInt;
    FTokenPtr: NativeInt;
    FStringPtr: NativeInt;
    FSourceLine: Integer;
    FSaveChar: Byte;
    FToken: Char;
    FFloatType: Char;
    FWideStr: TCharArray;
    FOnError: TParserErrorEvent;
    FEncoding: TEncoding;
    FFormatSettings: TFormatSettings;
    procedure ReadBuffer;
    procedure SkipBlanks;
    function CharType(var ABufPos: NativeInt): TCharType;
  protected
    function GetLinePos: NativeInt;
  public
    constructor Create(Stream: TStream; AOnError: TParserErrorEvent = nil); overload;
    constructor Create(Stream: TStream; const FormatSettings: TFormatSettings; AOnError: TParserErrorEvent = nil); overload;
    destructor Destroy; override;
    procedure CheckToken(T: Char);
    procedure CheckTokenSymbol(const S: string);
    procedure Error(const Ident: string);
    procedure ErrorFmt(const Ident: string; const Args: array of const);
    procedure ErrorStr(const Message: string);
    procedure HexToBinary(Stream: TStream);
    function NextToken: Char;
    function SourcePos: NativeInt;
    function TokenComponentIdent: string;
    function TokenFloat: Extended;
    function TokenInt: Int64;
    function TokenString: string;
    function TokenWideString: UnicodeString;
    function TokenSymbolIs(const S: string): Boolean;
    property FloatType: Char read FFloatType;
    property SourceLine: Integer read FSourceLine;
    property LinePos: NativeInt read GetLinePos;
    property Token: Char read FToken;
    property OnError: TParserErrorEvent read FOnError write FOnError;
  end;

{ TThread }

  EThread = class(Exception);
  EThreadExternalException = class(EThread);

  TThreadMethod = procedure of object;
  TThreadProcedure = reference to procedure;

{$IFDEF MSWINDOWS}
  TThreadPriority = (tpIdle, tpLowest, tpLower, tpNormal, tpHigher, tpHighest,
    tpTimeCritical) platform;
{$ENDIF MSWINDOWS}

(*$HPPEMIT '#if defined(Yield)'*)
(*$HPPEMIT '#undef Yield'*)
(*$HPPEMIT '#endif'*)

  TThread = class
  private type
    PSynchronizeRecord = ^TSynchronizeRecord;
    TSynchronizeRecord = record
      FThread: TObject;
      FMethod: TThreadMethod;
      FProcedure: TThreadProcedure;
      FSynchronizeException: TObject;
    end;
  private class var
    FProcessorCount: Integer;
  private
    FThreadID: TThreadID;
{$IF Defined(MSWINDOWS)}
    FHandle: THandle platform;
{$ELSEIF Defined(POSIX)}
    FCreateSuspendedMutex: pthread_mutex_t;
    FInitialSuspendDone: Boolean;
{$ENDIF POSIX}
    FStarted: Boolean;
    FCreateSuspended: Boolean;
    FTerminated: Boolean;
    FSuspended: Boolean;
    FFreeOnTerminate: Boolean;
    FFinished: Boolean;
    FReturnValue: Integer;
    FOnTerminate: TNotifyEvent;
    FFatalException: TObject;
    FExternalThread: Boolean;
    class constructor Create;
    class destructor Destroy;
    procedure CallOnTerminate;
    class procedure Synchronize(ASyncRec: PSynchronizeRecord; QueueEvent: Boolean = False;
      ForceQueue: Boolean = False); overload;
    class function GetCurrentThread: TThread; static;
    class function GetIsSingleProcessor: Boolean; static; inline;
    procedure InternalStart(Force: Boolean);
{$IF Defined(MSWINDOWS)}
    function GetPriority: TThreadPriority; platform;
    procedure SetPriority(Value: TThreadPriority); platform;
{$ELSEIF Defined(POSIX)}
    function GetPriority: Integer; platform;
    procedure SetPriority(Value: Integer); platform;
    function GetPolicy: Integer; platform;
    procedure SetPolicy(Value: Integer); platform;
{$ENDIF POSIX}
    procedure SetSuspended(Value: Boolean);
  private class threadvar
    [Unsafe] FCurrentThread: TThread;
  protected
    procedure CheckThreadError(ErrCode: Integer); overload;
    procedure CheckThreadError(Success: Boolean); overload;
    procedure DoTerminate; virtual;
    procedure TerminatedSet; virtual;
    procedure Execute; virtual; abstract;
    procedure Queue(AMethod: TThreadMethod); overload; inline;
    procedure Synchronize(AMethod: TThreadMethod); overload; inline;
    procedure Queue(AThreadProc: TThreadProcedure); overload; inline;
    procedure Synchronize(AThreadProc: TThreadProcedure); overload; inline;
    procedure SetFreeOnTerminate(Value: Boolean);
    property ReturnValue: Integer read FReturnValue write FReturnValue;
    property Terminated: Boolean read FTerminated;
  public type
    TSystemTimes = record
      IdleTime, UserTime, KernelTime, NiceTime: UInt64;
    end;
  public
    constructor Create; overload;
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; override;
    // CreateAnonymousThread will create an instance of an internally derived TThread that simply will call the
    // anonymous method of type TProc. This thread is created as suspended, so you should call the Start method
    // to make the thread run. The thread is also marked as FreeOnTerminate, so you should not touch the returned
    // instance after calling Start as it could have run and is then freed before another external calls or
    // operations on the instance are attempted.
    class function CreateAnonymousThread(const ThreadProc: TProc): TThread; static;
    procedure AfterConstruction; override;
    // This function is not intended to be used for thread synchronization.
    procedure Resume; deprecated;
    // Use Start after creating a suspended thread.
    procedure Start;
    // This function is not intended to be used for thread synchronization.
    procedure Suspend; deprecated;
    procedure Terminate;
    function WaitFor: LongWord;
    // NOTE: You can only call CheckTerminated and SetReturnValue on an internally created thread.
    // Calling this from an externally created thread will raise an exception
    // Use TThread.CheckTerminated to check if the Terminated flag has been set on the current thread
    class function CheckTerminated: Boolean; static;
    // Use TThread.SetReturnValue to set the current thread's return value from code that doesn't have
    // direct access to the current thread
    class procedure SetReturnValue(Value: Integer); static;
    class procedure Queue(const AThread: TThread; AMethod: TThreadMethod); overload; static;
    class procedure Queue(const AThread: TThread; AThreadProc: TThreadProcedure); overload; static;
    class procedure RemoveQueuedEvents(const AThread: TThread; AMethod: TThreadMethod); overload; static;
    class procedure StaticQueue(const AThread: TThread; AMethod: TThreadMethod); static; deprecated 'From C++ just use Queue now that it is just a static method';
    class procedure Synchronize(const AThread: TThread; AMethod: TThreadMethod); overload; static;
    class procedure Synchronize(const AThread: TThread; AThreadProc: TThreadProcedure); overload; static;
    class procedure StaticSynchronize(const AThread: TThread; AMethod: TThreadMethod); static; deprecated 'From C++ just use Synchronize now that it is just a static method';
    /// <summary>
    ///    Queue the method to delay its  synchronous execution. Unlike the Queue method, this will queue it even
    ///    if the caller is in the main thread.
    /// </summary>
    class procedure ForceQueue(const AThread: TThread; const AMethod: TThreadMethod); overload; static;
    /// <summary>
    ///    Queue the procedure to delay its synchronous execution. Unlike the Queue method, this will queue it even
    ///    if the caller is in the main thread.
    /// </summary>
    class procedure ForceQueue(const AThread: TThread; const AThreadProc: TThreadProcedure); overload; static;
    class procedure RemoveQueuedEvents(const AThread: TThread); overload; static;
    class procedure RemoveQueuedEvents(AMethod: TThreadMethod); overload; static; inline;
{$IFNDEF NEXTGEN}
    class procedure NameThreadForDebugging(AThreadName: AnsiString; AThreadID: TThreadID = TThreadID(-1)); overload; static; //deprecated 'Use without AnsiString cast';
{$ENDIF !NEXTGEN}
    class procedure NameThreadForDebugging(AThreadName: string; AThreadID: TThreadID = TThreadID(-1)); overload; static;
    class procedure SpinWait(Iterations: Integer); static;
    class procedure Sleep(Timeout: Integer); static;
    class procedure Yield; static;
    // Call GetSystemTimes to get the current CPU ticks representing the amount of time the system has
    // spent Idle, in User's code, in Kernel or System code and Nice. For many systems, such as Windows,
    // the NiceTime is 0. NOTE: The KernelTime field also include the amount of time the system has been Idle.
    class function GetSystemTimes(out SystemTimes: TSystemTimes): Boolean; static;
    // Using the previously acquired SystemTimes structure, calculate the average time that the CPU has been
    // executing user and kernel code. This is the current CPU load the system is experiencing. The return value
    // is expressed as a percentage ranging from 0 to 100. NOTE: The passed in PrevSystemTimes record is updated
    // with the current system time values.
    class function GetCPUUsage(var PrevSystemTimes: TSystemTimes): Integer; static;
    // Returns current value in milliseconds of an internal system counter
    class function GetTickCount: Cardinal; static;
    property ExternalThread: Boolean read FExternalThread;
    property FatalException: TObject read FFatalException;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write SetFreeOnTerminate;
    property Finished: Boolean read FFinished;
{$IF Defined(MSWINDOWS)}
    property Handle: THandle read FHandle;
    property Priority: TThreadPriority read GetPriority write SetPriority;
{$ELSEIF Defined(POSIX)}
    // ** Priority is an Integer **
    property Priority: Integer read GetPriority write SetPriority;
    property Policy: Integer read GetPolicy write SetPolicy;
{$ENDIF POSIX}
    // Started is set to true once the thread has actually started running after the initial suspend.
    property Started: Boolean read FStarted;
    property Suspended: Boolean read FSuspended write SetSuspended;
    property ThreadID: TThreadID read FThreadID;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
    /// <summary>
    ///    The currently executing thread. This is the same as TThread.CurrentThread.
    /// </summary>
    class property Current: TThread read GetCurrentThread;
    /// <summary>
    ///    The currently executing thread. This is the same as TThread.Current.
    ///    Please use TThread.Current, which is more clear and less redundant.
    /// </summary>
    class property CurrentThread: TThread read GetCurrentThread;
    /// <summary>
    ///    The number of processor cores on which this application is running. This will include virtual
    ///    "Hyper-threading" cores on many modern Intel CPUs. It is ultimately based on what the underlying
    ///    operating system reports.
    /// </summary>
    class property ProcessorCount: Integer read FProcessorCount;
    /// <summary>
    ///    Simple Boolean property to quickly determine wether running on a single CPU based system.
    /// </summary>
    class property IsSingleProcessor: Boolean read GetIsSingleProcessor;
  end;

{ TComponentEnumerator }

  TComponentEnumerator = class
  private
    FIndex: Integer;
    FComponent: TComponent;
  public
    constructor Create(AComponent: TComponent);
    function GetCurrent: TComponent; inline;
    function MoveNext: Boolean;
    property Current: TComponent read GetCurrent;
  end;

{ TComponent class }

  TOperation = (opInsert, opRemove);
  TComponentState = set of (csLoading, csReading, csWriting, csDestroying,
    csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,
    csInline, csDesignInstance);
  TComponentStyle = set of (csInheritable, csCheckPropAvail, csSubComponent,
    csTransient);
  TGetChildProc = procedure (Child: TComponent) of object;
  TGetStreamProc = procedure (const S: TStream) of object;
  TGetDeltaStreamsEvent = procedure (Sender: TObject; Proc: TGetStreamProc; var Handled: Boolean) of object;

  TComponentName = type string;

  IVCLComObject = interface
    ['{E07892A0-F52F-11CF-BD2F-0020AF0E5B81}']
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult;
    procedure FreeOnRelease;
  end;

  IDesignerNotify = interface
    ['{B971E807-E3A6-11D1-AAB1-00C04FB16FBC}']
    procedure Modified;
    procedure Notification(AnObject: TPersistent; Operation: TOperation);
    procedure CanInsertComponent(AComponent: TComponent);
  end;

  TBasicAction = class;

  ComponentPlatformsAttribute = class(TCustomAttribute)
  private
    FPlatforms: TPlatformIds;
  public
    constructor Create(const Platforms: TPlatformIds);
    property Platforms: TPlatformIds read FPlatforms write FPlatforms;
  end;

  IObserver = interface;
  TObserverToggleEvent = reference to procedure(const AObserver: IObserver; const Value: Boolean);
  IObserver = interface
    ['{B03253D8-7720-4B68-B10A-E3E79B91ECD3}']
    procedure Removed;
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetOnObserverToggle: TObserverToggleEvent;
    procedure SetOnObserverToggle(AEvent: TObserverToggleEvent);
    property OnObserverToggle: TObserverToggleEvent read GetOnObserverToggle write SetOnObserverToggle;
    property Active: Boolean read GetActive write SetActive;
  end;

  ISingleCastObserver = interface(IObserver)
    ['{D0395F17-52AA-4515-93A5-5B292F03AA7B}']
  end;

  IMultiCastObserver = interface(IObserver)
    ['{C19CB01E-1233-4405-8A30-7987DF2C3690}']
  end;

  IEditLinkObserver = interface(ISingleCastObserver)
    ['{E88C2705-7C5A-4E66-9B81-447D05D5E640}']
    procedure Update;
    function Edit: Boolean;
    procedure Reset;
    procedure Modified;
    function IsModified: Boolean;
    function IsValidChar(AKey: Char): Boolean;
    function IsRequired: Boolean;
    function GetIsReadOnly: Boolean;
    procedure SetIsReadOnly(Value: Boolean);
    property IsReadOnly: Boolean read GetIsReadOnly write SetIsReadOnly;
    function GetIsEditing: Boolean;
    property IsEditing: Boolean read GetIsEditing;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetUpdating: Boolean;
    property Updating: Boolean read GetUpdating;
  end;

  TObserverGetCurrentEvent = reference to function: TVarRec;
  IEditGridLinkObserver = interface(IEditLinkObserver)
    ['{A911B648-E1E5-4EEC-9FEE-D8E62FFA0E71}']
    function GetCurrent: TVarRec;
    property Current: TVarRec read GetCurrent;
    function GetOnObserverCurrent: TObserverGetCurrentEvent;
    procedure SetOnObserverCurrent(AEvent: TObserverGetCurrentEvent);
    property OnObserverCurrent: TObserverGetCurrentEvent read GetOnObserverCurrent write SetOnObserverCurrent;
  end;

  IPositionLinkObserver170 = interface
    ['{FA45CF0C-E8DB-4F9E-B53F-E072C94659F6}']
    procedure PosChanged;
  end;

  IPositionLinkObserver = interface(IPositionLinkObserver170)
    ['{E78B0035-6802-447C-A80A-0AEC04AD851F}']
    procedure PosChanging;
  end;

  IControlValueObserver = interface
    ['{61DAC12C-B950-43CA-86B5-43D8E78012E8}']
    procedure ValueModified;
    procedure ValueUpdate;
  end;

  // May be implemented by EditLink or ControlValue observer
  IObserverTrack = interface
    ['{8B9F22C3-FDA3-45FD-99E1-5A88481A9F95}']
    function GetTrack: Boolean;
    property Track: Boolean read GetTrack;
  end;

  TObservers = class
  public type
    TCanObserveEvent = reference to function(const ID: Integer): Boolean;
    TObserverAddedEvent = reference to procedure(const ID: Integer; const Observer: IObserver);
  private
    FObservers: TDictionary<Integer, IInterfaceList>;
    FCanObserve: TCanObserveEvent;
    FObserverAdded: TObserverAddedEvent;
  public
    constructor Create;
    destructor Destroy; override;
    property OnCanObserve: TCanObserveEvent read FCanObserve write FCanObserve;
    property OnObserverAdded: TObserverAddedEvent read FObserverAdded write FObserverAdded;

    function CanObserve(const ID: Integer): Boolean; overload; virtual;
    procedure AddObserver(const ID: Integer; const AIntf: IInterface); overload; virtual;
    procedure AddObserver(const IDs: Array of Integer; const AIntf: IInterface); overload; virtual;
    procedure RemoveObserver(const ID: Integer; const AIntf: IInterface); overload; virtual;
    procedure RemoveObserver(const IDs: Array of Integer; const AIntf: IInterface); overload; virtual;
    function IsObserving(const ID: Integer): Boolean; overload; virtual;
    function TryIsObserving(const ID: Integer; out AIntf: IInterface): Boolean; virtual;
    function GetSingleCastObserver(const ID: Integer): IInterface; virtual;
    function GetMultiCastObserver(const ID: Integer): IInterfaceList; virtual;
  end;

  TLinkObservers = class
  public
    class function GetEditGridLink(const AObservers: TObservers): IEditGridLinkObserver; static;
    class function GetEditLink(const AObservers: TObservers): IEditLinkObserver; static;
    class procedure EditLinkUpdate(const AObservers: TObservers); static; inline;
    class function EditLinkTrackUpdate(const AObservers: TObservers): Boolean; static;
    class procedure EditLinkReset(const AObservers: TObservers); static; inline;
    class procedure EditLinkModified(AObservers: TObservers); static; inline;
    class function EditLinkIsModified(const AObservers: TObservers): Boolean; static; inline;
    class function EditLinkIsValidChar(const AObservers: TObservers; AKey: Char): Boolean; static; inline;
    class function EditLinkIsEditing(const AObservers: TObservers): Boolean; static; inline;
    class function EditLinkEdit(const AObservers: TObservers): Boolean; static; inline;
    class procedure EditLinkSetIsReadOnly(const AObservers: TObservers; AValue: Boolean); static; inline;
    class function EditLinkIsReadOnly(const AObservers: TObservers): Boolean; static; inline;

    class procedure EditGridLinkUpdate(const AObservers: TObservers); static; inline;
    class procedure EditGridLinkReset(const AObservers: TObservers); static; inline;
    class procedure EditGridLinkModified(const AObservers: TObservers); static; inline;
    class function EditGridLinkIsModified(const AObservers: TObservers): Boolean; static; inline;
    class function EditGridLinkIsValidChar(const AObservers: TObservers; AKey: Char): Boolean; static; inline;
    class function EditGridLinkIsEditing(const AObservers: TObservers): Boolean; static; inline;
    class function EditGridLinkEdit(const AObservers: TObservers): Boolean; static; inline;
    class function EditGridLinkIsReadOnly(const AObservers: TObservers): Boolean; static; inline;
    class procedure EditGridLinkSetIsReadOnly(const AObservers: TObservers; AValue: Boolean); static; inline;

    class procedure PositionLinkPosChanged(const AObservers: TObservers); static;
    class procedure PositionLinkPosChanging(const AObservers: TObservers); static;
    class procedure ListSelectionChanged(const AObservers: TObservers); static;
    class procedure ControlValueUpdate(AObservers: TObservers); static;
    class procedure ControlValueModified(AObservers: TObservers); static;
    class function ControlValueTrackUpdate(const AObservers: TObservers): Boolean; static;
    class function AllowControlChange(const AControl: TComponent): Boolean; static;
    class procedure ControlChanged(const AControl: TComponent); static;

  end;

  TObserverMapping = class
  public const
    EditLinkID = 1;
    EditGridLinkID = 2;
    PositionLinkID = 3;
    ControlValueID = 4;
    MappedID = 100;
  private
    FMappings: TStringList;
    class var
      FInstance: TObserverMapping;
  protected
    class function Instance: TObserverMapping;
  public
    constructor Create;
    destructor Destroy; override;
    class destructor Destroy;
    class function GetObserverID(const Key: string): Integer;
  end;

  EObserverException = class(Exception);

  TDefaultAttributeBase = class(TCustomAttribute)
  strict protected
    FValue: Variant;
  public
    property Value: Variant read FValue;
  end;

  DefaultAttribute = class(TDefaultAttributeBase)
  public
    constructor Create(const DefaultValue: Boolean); overload;
{$IFNDEF NEXTGEN}
    constructor Create(const DefaultValue: AnsiChar); overload;
{$ENDIF !NEXTGEN}
    constructor Create(const DefaultValue: Char); overload;
    constructor Create(const DefaultValue: Integer); overload;
    constructor Create(const DefaultValue: Cardinal); overload;
    constructor Create(const DefaultValue: Int64); overload;
    constructor Create(const DefaultValue: UInt64); overload;
    constructor Create(const DefaultValue: String); overload;
    constructor Create(const DefaultValue: Extended); overload;
  end;

  NoDefaultAttribute = class(TDefaultAttributeBase)
  public
    constructor Create;
  end;

  StoredAttribute = System.StoredAttribute;

  ObservableMemberAttribute = class(TCustomAttribute)
  strict protected
    FMemberName: String;
  public
    constructor Create(const AMemberName: string); overload;
    property MemberName: String read FMemberName;
  end;

  TDesignInfo = Int32;

(*$HPPEMIT '#pragma option -w-8022  // "TBaseAsyncResult::Dispatch" hides virtual function "TObject::Dispatch"'*)
{ TBaseAsyncResult }
  /// <summary>
  ///    Base class used for implementing all asynchronous procedure calls. Never pass this instance around as
  ///    an instance reference. The intent is that this object is only ever referenced through the IAsyncResult
  ///    interface. Failure to heed this warning will result in unpredictable behavior. See the information about the
  ///    Invoke method.
  /// </summary>
  TBaseAsyncResult = class abstract(TInterfacedObject, IAsyncResult)
  private type
    TAsyncFlag = (Completed, Synchronous, Invoked, Cancelled, ForceSize = SizeOf(Integer) * 8 - 1); // make the size the same as Integer.
    TAsyncFlags = set of TAsyncFlag;
  private
    class constructor Create;
    class destructor Destroy;
  private
    FContext: TObject;
    FAsyncFlags: TAsyncFlags;
    FInvokingThread: TThreadID;
    FAsyncHandle: TMultiWaitEvent;
    procedure SetFlagsAtomic(Value, Mask: TAsyncFlags);
    function GetAsyncContext: TObject;
    function GetAsyncWaitEvent: TMultiWaitEvent;
    function GetCompletedSynchronously: Boolean;
    function GetIsCompleted: Boolean;
    function GetIsCancelled: Boolean;

    property AsyncWaitEvent: TMultiWaitEvent read GetAsyncWaitEvent;
  protected
    /// <summary>
    ///    This field will hold the acquired exception instance raised from the execution of the async method call.
    ///    It will be re-raised in the context of the invoking thread when the corresponding EndXXXX method is called.
    /// </summary>
    FInvokingException: TObject;
    /// <summary>
    ///    Override this method to dispatch the actual asynchronous procedure call. Descendants will use whatever context
    ///    or other state information contained in the instance to pass along to the procedure or function.
    /// </summary>
    procedure AsyncDispatch; virtual; abstract;
    /// <summary>
    ///    Override this method to perform any extra state or signaling required by the descendant. The descendant must
    ///    call this inherited method in order to properly set the completion and possibly signal the FAsyncHandle if
    ///    previously created. Failure to call this method can result in a dead lock or hang.
    /// </summary>
    procedure Complete; virtual;
    /// <summary>
    ///    Calls the actual target asynchronous method within the context of however it is scheduled. This could be
    ///    in the context of the main or GUI thread, or within a background thread. This depends on the implementation
    ///    of a specific asynchronous BeginXXXX method call.
    /// </summary>
    procedure DoAsyncDispatch;
    /// <summary>
    ///    Override this method to schedule the asynchronous procedure call in the manner specific to
    ///    a given instance, component or class. By default, this will schedule the async procedure onto
    ///    the main thread or execute the procedure synchronously if already on the main thread.
    ///    Other classes may schedule the procedure call into a separate thread or thread pool.
    /// </summary>
    procedure Schedule; virtual;
    /// <summary>
    ///    This constructor must be called from a descendent protected constructor.
    /// </summary>
    constructor Create(const AContext: TObject); overload;
    /// <summary>
    ///    Opaque user-supplied context. This context is available via the IAsyncResult.GetAsyncContext and descendents
    ///    if this class.
    /// </summary>
    property Context: TObject read FContext;
    ///  <summary>
    ///    Returns true if the operation can be cancelled. When cancelling the async operation, do any additional processing.
    ///  </summary>
    ///  <remarks>
    ///    By default, all Async cannnot be cancelled. If descendants support cancelling asynchronous tasks,
    ///  they must override this behaviour and do the required processing;
    ///  </remarks>
    function DoCancel: Boolean; virtual;
  public
    /// <summary>
    ///    This constructor should never be called directly. Only descendents should be constructed using the
    ///    protected Create constructor above. Calling this constructor will raise an exception.
    /// </summary>
    constructor Create; overload;
    destructor Destroy; override;

    ///  <summary>
    ///    Cancels the async operation. Returns True when the asynchronous operation can be cancelled.
    ///  </summary>
    function Cancel: Boolean;
    /// <summary>
    ///    This method must be called prior in order to return itself as an IAsyncResult and actually schedule/invoke the
    ///    async call.
    /// </summary>
    function Invoke: IAsyncResult;
    /// <summary>
    ///    As long as the rules for only ever accessing this object through the IAsynsResult interface, this method
    ///    should only ever be called by a given "EndInvoke" method by casting the IAsyncResult interface instance
    ///    back to a specific descendant instance of this class. Never call this method directly outside the context
    ///    of an "EndInvoke" style method.
    /// </summary>
    procedure WaitForCompletion;

    /// <summary>
    ///    This method is called from VCL.TControl (and possibly other similar) descendants in order to call the
    ///    asynchronous procedure/function as a result of a posted Window message.
    /// </summary>
    class procedure Dispatch(const AsyncResult: TBaseAsyncResult); reintroduce; static; inline;

    ///  <summary>
    ///    Set to True when the asynchronous call has been cancelled.
    ///  </summary>
    property IsCancelled: Boolean read GetIsCancelled;
  end;

  TAsyncProcedureEvent = procedure (const ASyncResult: IAsyncResult) of object;
  TAsyncFunctionEvent = procedure (const ASyncResult: IAsyncResult; out Result: TObject) of object;
  TAsyncConstArrayProcedureEvent = procedure (const ASyncResult: IAsyncResult; const Params: array of const) of object;
  TAsyncConstArrayFunctionEvent = procedure (const ASyncResult: IAsyncResult; out Result: TObject; const Params: array of const) of object;
  TAsyncConstArrayProc = reference to procedure (const Params: array of const);
  TAsyncConstArrayFunc<TResult> = reference to function (const Params: array of const): TResult;

  TAsyncCallback = reference to procedure (const ASyncResult: IAsyncResult);
  TAsyncCallbackEvent = TAsyncProcedureEvent;

  TComponent = class(TPersistent, IInterface, IInterfaceComponentReference)
  protected type
    TComponentAsyncResult = class(TBaseAsyncResult)
    private
      FComponent: TComponent;
    protected
      procedure Schedule; override;
      constructor Create(const AContext: TObject; const AComponent: TComponent);
    end;
    TAsyncConstArrayResult = class(TComponentASyncResult)
    protected
      FParams: TArray<TValue>;
      constructor Create(const AContext: TObject; const AComponent: TComponent; const Params: array of const);
    end;
    TAsyncConstArrayProcResult = class sealed(TAsyncConstArrayResult)
    private
      FAsyncProcedure: TAsyncConstArrayProc;
    protected
      procedure AsyncDispatch; override;
      constructor Create(const AAsyncProcedure: TAsyncConstArrayProc; const AContext: TObject; const AComponent: TComponent; const Params: array of const);
    end;
    TAsyncConstArrayFuncResult<TResult> = class sealed(TAsyncConstArrayResult)
    private
      FRetVal: TResult;
      FAsyncFunction: TAsyncConstArrayFunc<TResult>;
    protected
      constructor Create(const AAsyncFunction: TAsyncConstArrayFunc<TResult>; const AContext: TObject; const AComponent: TComponent; const Params: array of const);
      procedure AsyncDispatch; override;
      function GetRetVal: TResult;
    end;
    TAsyncConstArrayProcedureResult = class sealed(TAsyncConstArrayResult)
    private
      FAsyncProcedure: TAsyncConstArrayProcedureEvent;
    protected
      procedure AsyncDispatch; override;
      constructor Create(const AAsyncProcedure: TAsyncConstArrayProcedureEvent; const AContext: TObject; const AComponent: TComponent; const Params: array of const);
    end;
    TAsyncConstArrayFunctionResult = class sealed(TAsyncConstArrayResult)
    private
      FRetVal: TObject;
      FAsyncFunction: TAsyncConstArrayFunctionEvent;
    protected
      constructor Create(const AAsyncFunction: TAsyncConstArrayFunctionEvent; const AContext: TObject; const AComponent: TComponent; const Params: array of const);
      procedure AsyncDispatch; override;
      function GetRetVal: TObject;
    end;
    TAsyncProcedureResult = class sealed(TComponentAsyncResult)
    private
      FAsyncProcedure: TProc;
    protected
      constructor Create(const AAsyncProcedure: TProc; const AContext: TObject; const AComponent: TComponent);// overload;
      procedure AsyncDispatch; override;
    end;
    TAsyncFunctionResult<TResult> = class sealed(TComponentAsyncResult)
    private
      FRetVal: TResult;
      FAsyncFunction: TFunc<TResult>;
    protected
      constructor Create(const AAsyncFunction: TFunc<TResult>; const AContext: TObject; const AComponent: TComponent);// overload;
      procedure AsyncDispatch; override;
      function GetRetVal: TResult;
    end;
    TAsyncProcedureResultEvent = class sealed(TComponentAsyncResult)
    private
      FAsyncProcedure: TAsyncProcedureEvent;
    protected
      constructor Create(const AAsyncProcedure: TAsyncProcedureEvent; const AContext: TObject; const AComponent: TComponent);// overload;
      procedure AsyncDispatch; override;
    end;
    TAsyncFunctionResultEvent = class sealed(TComponentAsyncResult)
    private
      FRetVal: TObject;
      FAsyncFunction: TAsyncFunctionEvent;
    protected
      constructor Create(const AAsyncFunction: TAsyncFunctionEvent; const AContext: TObject; const AComponent: TComponent);// overload;
      procedure AsyncDispatch; override;
      function GetRetVal: TObject;
    end;
  private
    [Unsafe] FOwner: TComponent;
    FName: TComponentName;
    FTag: NativeInt;
    FComponents: TList<TComponent>;
    FFreeNotifies: TList<TComponent>;
// TODO -cELBRUS_LONGINT64 : Verify TComponent.FDesignInfo (change LongInt to Integer)
    FDesignInfo: TDesignInfo;
    FComponentState: TComponentState;
    FVCLComObject: Pointer;
    FObservers: TObservers;
    FOnGetDeltaStreams: TGetDeltaStreamsEvent;
    function GetComObject: IUnknown;
    function GetComponent(AIndex: Integer): TComponent;
    function GetComponentCount: Integer;
    function GetComponentIndex: Integer;
    procedure Insert(AComponent: TComponent);
    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure Remove(AComponent: TComponent);
    procedure RemoveNotification(const AComponent: TComponent);
    procedure SetComponentIndex(Value: Integer);
    procedure SetReference(Enable: Boolean);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);
    { IInterfaceComponentReference }
    function IInterfaceComponentReference.GetComponent = IntfGetComponent;
    function IntfGetComponent: TComponent;
    procedure DoGetDeltaStreams(Proc: TGetStreamProc; var Handled: Boolean);
    procedure ReadDeltaStream(const S: TStream);
    procedure ReadDeltaState;
  protected
    FComponentStyle: TComponentStyle;
  private
    FSortedComponents: TList<TComponent>;
    function FindSortedComponent(const AName: string; var Index: Integer): TComponent;
    procedure AddSortedComponent(const AComponent: TComponent);
    procedure RemoveSortedComponent(const AComponent: TComponent); inline;
  private class var
    FComparer: IComparer<TComponent>;
    class constructor Create;
  protected
    /// <summary>
    ///    Override AsyncSchedule in descendant components in order to modify the manner in which an async method
    ///    call should be scheduled. By default, this will queue the method call with the main thread using
    ///    TThread.Queue.
    /// </summary>
    procedure AsyncSchedule(const ASyncResult: TBaseAsyncResult); virtual;
    procedure ChangeName(const NewName: TComponentName);
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); dynamic;
    function GetChildOwner: TComponent; dynamic;
    function GetChildParent: TComponent; dynamic;
    function GetOwner: TPersistent; override;
    procedure Loaded; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    procedure GetDeltaStreams(Proc: TGetStreamProc); dynamic;
    procedure PaletteCreated; dynamic;
    procedure ReadState(Reader: TReader); virtual;
    function CanObserve(const ID: Integer): Boolean; virtual;
    procedure ObserverAdded(const ID: Integer; const Observer: IObserver); virtual;
    function GetObservers: TObservers; virtual;
    procedure SetAncestor(Value: Boolean);
    procedure SetDesigning(Value: Boolean; SetChildren: Boolean = True);
    procedure SetInline(Value: Boolean);
    procedure SetDesignInstance(Value: Boolean);
    procedure SetName(const NewName: TComponentName); virtual;
    procedure SetChildOrder(Child: TComponent; Order: Integer); dynamic;
    procedure SetParentComponent(Value: TComponent); dynamic;
    procedure Updating; dynamic;
    procedure Updated; dynamic;
    class procedure UpdateRegistry(Register: Boolean; const ClassID, ProgID: string); virtual;
    procedure ValidateRename(AComponent: TComponent; const CurName, NewName: string); virtual;
    procedure ValidateContainer(AComponent: TComponent); dynamic;
    procedure ValidateInsert(AComponent: TComponent); dynamic;
    procedure WriteState(Writer: TWriter); virtual;
    procedure RemoveFreeNotifications;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    property OnGetDeltaStreams: TGetDeltaStreamsEvent read FOnGetDeltaStreams write FOnGetDeltaStreams;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    /// <summary>
    ///    Start an asynchronous procedure which will be execute in the context of the main thread or in the case of
    ///    a VCL TControl descendant, in the context of the thread on which the closest window handle was created
    ///    (following the parent chain). This will most likely still be on the main thread.
    /// </summary>
    function BeginInvoke(const AProc: TProc; const AContext: TObject = nil): IAsyncResult; overload;
    function BeginInvoke(const AProc: TASyncProcedureEvent; const AContext: TObject = nil): IAsyncResult; overload;
    function BeginInvoke<TResult>(const AFunc: TFunc<TResult>; const AContext: TObject = nil): IAsyncResult; overload;
    function BeginInvoke(const AProc: TAsyncConstArrayProc; const Params: array of const; const AContext: TObject = nil): IAsyncResult; overload;
    function BeginInvoke<TResult>(const AFunc: TAsyncConstArrayFunc<TResult>; const Params: array of const; const AContext: TObject = nil): IAsyncResult; overload;
    function BeginInvoke(const AProc: TAsyncConstArrayProcedureEvent; const Params: array of const; const AContext: TObject = nil): IAsyncResult; overload;
    function BeginInvoke(const AFunc: TAsyncConstArrayFunctionEvent; const Params: array of const; const AContext: TObject = nil): IAsyncResult; overload;
    function BeginInvoke(const AFunc: TAsyncFunctionEvent; const AContext: TObject = nil): IAsyncResult; overload;
    /// <summary>
    ///    Block the caller until the given IAsyncResult completes. This function will return immediately if the
    ///    IAsyncResult has already finished. This function will also raise any exception that may have happened while
    ///    the asynchronous procedure executed.
    /// </summary>
    procedure EndInvoke(const ASyncResult: IAsyncResult); overload;
    /// <summary>
    ///    Block the caller until the given IAsyncResult completes. Returns the result from the asynchronously executed
    ///    function. This function will return immediately if the IAsyncResult has already finished. This function will
    ///    also raise any exception that may have happened while the asynchronous procedure executed.
    /// </summary>
    function EndInvoke<TResult>(const AsyncResult: IAsyncResult): TResult; overload;
    /// <summary>
    ///    Block the caller until the given IAsyncResult completes. Returns the result from the asynchronously executed
    ///    function. This function will return immediately if the IAsyncResult has already finished. This function will
    ///    also raise any exception that may have happened while the asynchronous procedure executed.
    /// </summary>
    function EndFunctionInvoke(const AsyncResult: IAsyncResult): TObject;
    procedure DestroyComponents;
    procedure Destroying;
    function ExecuteAction(Action: TBasicAction): Boolean; dynamic;
    function FindComponent(const AName: string): TComponent;
    procedure FreeNotification(AComponent: TComponent);
    procedure RemoveFreeNotification(AComponent: TComponent);
    procedure FreeOnRelease;
    function GetEnumerator: TComponentEnumerator;
    function GetParentComponent: TComponent; dynamic;
    function GetNamePath: string; override;
    function HasParent: Boolean; dynamic;
    procedure InsertComponent(const AComponent: TComponent);
    procedure RemoveComponent(const AComponent: TComponent);
    procedure SetSubComponent(IsSubComponent: Boolean);
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
    function UpdateAction(Action: TBasicAction): Boolean; virtual;
    function IsImplementorOf(const I: IInterface): Boolean;
    function ReferenceInterface(const I: IInterface; Operation: TOperation): Boolean;
    property ComObject: IUnknown read GetComObject;
    property Components[Index: Integer]: TComponent read GetComponent;
    property ComponentCount: Integer read GetComponentCount;
    property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
    property ComponentState: TComponentState read FComponentState;
    property ComponentStyle: TComponentStyle read FComponentStyle;
    property DesignInfo: TDesignInfo read FDesignInfo write FDesignInfo;
    property Owner: TComponent read FOwner;
    property VCLComObject: Pointer read FVCLComObject write FVCLComObject;
    property Observers: TObservers read GetObservers;
  published
    property Name: TComponentName read FName write SetName stored False;
    property Tag: NativeInt read FTag write FTag default 0;
  end;

{ TBasicActionLink }

  TBasicActionLink = class(TObject)
  private
    FOnChange: TNotifyEvent;
    [Unsafe] FAction: TBasicAction;
  protected
    procedure AssignClient(AClient: TObject); virtual;
    procedure Change; virtual;
    function IsOnExecuteLinked: Boolean; virtual;
    procedure SetAction(Value: TBasicAction); virtual;
    procedure SetOnExecute(Value: TNotifyEvent); virtual;
  public
    constructor Create(AClient: TObject); virtual;
    destructor Destroy; override;
    function Execute(AComponent: TComponent = nil): Boolean; virtual;
    function Update: Boolean; virtual;
    property Action: TBasicAction read FAction write SetAction;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TBasicActionLinkClass = class of TBasicActionLink;
  {+ ! Moved here from Vcl.ActnList.pas !!}
  TActionEvent = procedure(Action: TBasicAction; var Handled: Boolean) of object;
  THintEvent = procedure(var HintStr: string; var CanShow: Boolean) of object;

{ TBasicAction }

  TBasicAction = class(TComponent)
  private
    FClients: TList<TBasicActionLink>;
    [Unsafe] FActionComponent: TComponent;
    FOnChange: TNotifyEvent;
    FOnExecute: TNotifyEvent;
    FOnUpdate: TNotifyEvent;
    function GetClientCount: Integer;
    function GetClient(Index: Integer): TBasicActionLink;
    procedure SetActionComponent(const Value: TComponent);
  protected
    procedure Change; virtual;
    procedure SetOnExecute(Value: TNotifyEvent); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property ClientCount: Integer read GetClientCount;
    property Clients[Index: Integer]: TBasicActionLink read GetClient;
    procedure RegisterChanges(const Value: TBasicActionLink);
    procedure UnRegisterChanges(const Value: TBasicActionLink);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Suspended: Boolean; virtual;
    function HandlesTarget(Target: TObject): Boolean; virtual;
    procedure UpdateTarget(Target: TObject); virtual;
    procedure ExecuteTarget(Target: TObject); virtual;
    function Execute: Boolean; dynamic;
    function Update: Boolean; virtual;
    property ActionComponent: TComponent read FActionComponent write SetActionComponent;
    property OnExecute: TNotifyEvent read FOnExecute write SetOnExecute;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  end;

{ TBasicAction class reference type }

  TBasicActionClass = class of TBasicAction;

{ TDataModule }

  TDataModule = class(TComponent)
  private
    FDesignSize: TPoint;
    FDesignOffset: TPoint;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOldCreateOrder: Boolean;
    procedure ReadHeight(Reader: TReader);
    procedure ReadHorizontalOffset(Reader: TReader);
    procedure ReadVerticalOffset(Reader: TReader);
    procedure ReadWidth(Reader: TReader);
    procedure WriteWidth(Writer: TWriter);
    procedure WriteHorizontalOffset(Writer: TWriter);
    procedure WriteVerticalOffset(Writer: TWriter);
    procedure WriteHeight(Writer: TWriter);
  protected
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function HandleCreateException: Boolean; dynamic;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property DesignOffset: TPoint read FDesignOffset write FDesignOffset;
    property DesignSize: TPoint read FDesignSize write FDesignSize;
  published
    property OldCreateOrder: Boolean read FOldCreateOrder write FOldCreateOrder;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

var
  AddDataModule: procedure (DataModule: TDataModule) of object = nil;
  RemoveDataModule: procedure (DataModule: TDataModule) of object = nil;
  ApplicationHandleException: procedure (Sender: TObject) of object = nil;
  ApplicationShowException: procedure (E: Exception) of object = nil;

{ Component registration handlers }

  RegisterComponentsProc: procedure(const Page: string;
    const ComponentClasses: array of TComponentClass) = nil;
  RegisterNoIconProc: procedure(const ComponentClasses: array of TComponentClass) = nil;
  CurrentGroup: Integer = -1; { Current design group }

{$IFDEF MSWINDOWS}
type
  TActiveXRegType = (axrComponentOnly, axrIncludeDescendants);

var
  RegisterNonActiveXProc: procedure(const ComponentClasses: array of TComponentClass;
    AxRegType: TActiveXRegType) = nil;
{$ENDIF MSWINDOWS}

  CreateVCLComObjectProc: procedure(Component: TComponent) = nil;

{ Point and rectangle constructors }

function Point(AX, AY: Integer): TPoint; inline;
function SmallPoint(AX, AY: SmallInt): TSmallPoint; inline;
function PointsEqual(const P1, P2: TPoint): Boolean; overload;
function PointsEqual(const P1, P2: TSmallPoint): Boolean; overload;
function InvalidPoint(X, Y: Integer): Boolean; overload;
function InvalidPoint(const At: TPoint): Boolean; overload;
function InvalidPoint(const At: TSmallPoint): Boolean; overload;

function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect; overload;
function Rect(const ATopLeft, ABottomRight: TPoint): TRect; overload;
function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;

{ Class registration routines }

procedure RegisterClass(AClass: TPersistentClass);
procedure RegisterClasses(const AClasses: array of TPersistentClass);
procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
procedure UnRegisterClass(AClass: TPersistentClass);
procedure UnRegisterClasses(const AClasses: array of TPersistentClass);
procedure UnRegisterModuleClasses(Module: HMODULE);
function FindClass(const ClassName: string): TPersistentClass;
function GetClass(const AClassName: string): TPersistentClass;
procedure StartClassGroup(AClass: TPersistentClass);
procedure GroupDescendentsWith(AClass, AClassGroup: TPersistentClass);
function ActivateClassGroup(AClass: TPersistentClass): TPersistentClass;
function ActiveClassGroup: TPersistentClass;
function ClassGroupOf(AClass: TPersistentClass): TPersistentClass; overload;
function ClassGroupOf(Instance: TPersistent): TPersistentClass; overload;

procedure ReportClassGroups(Report: TStrings);

{ Component registration routines }

procedure RegisterComponents(const Page: string;
  ComponentClasses: array of TComponentClass);
//  const ComponentClasses: array of TComponentClass);
procedure RegisterNoIcon(const ComponentClasses: array of TComponentClass);
{$IFDEF MSWINDOWS}
procedure RegisterNonActiveX(const ComponentClasses: array of TComponentClass;
  AxRegType: TActiveXRegType);
{$ENDIF MSWINDOWS}

var
  GlobalNameSpace: IReadWriteSync;

{ Object filing routines }

type
  TIdentMapEntry = record
    Value: Integer;
    Name: String;
  end;

// TODO -cELBRUS_LONGINT64 : Verify TIdentToInt/TIntToIdent (LongInt to Integer)
  TIdentToInt = function(const Ident: string; var Int: Integer): Boolean;
  TIntToIdent = function(Int: Integer; var Ident: string): Boolean;
  TFindGlobalComponent = function(const Name: string): TComponent;
  TIsUniqueGlobalComponentName = function(const Name: string): Boolean;

var
  IsUniqueGlobalComponentNameProc: TIsUniqueGlobalComponentName;

procedure RegisterIntegerConsts(AIntegerType: Pointer; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
procedure UnregisterIntegerConsts(AIntegerType: Pointer; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
procedure RegisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
procedure UnregisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
function FindGlobalComponent(const Name: string): TComponent;
function IsUniqueGlobalComponentName(const Name: string): Boolean;
function IdentToInt(const Ident: string; var Int: Integer; const Map: array of TIdentMapEntry): Boolean;
function IntToIdent(Int: Integer; var Ident: string; const Map: array of TIdentMapEntry): Boolean;
function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;

function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
function ReadComponentDeltaRes(Instance: TComponent; const DeltaCandidates: array of string; const Proc: TGetStreamProc): TComponent;
function InitComponentRes(const ResName: string; Instance: TComponent): Boolean;
function ReadComponentRes(const ResName: string; Instance: TComponent): TComponent;
function ReadComponentResEx(HInstance: THandle; const ResName: string): TComponent;
function ReadComponentResFile(const FileName: string; const Instance: TComponent): TComponent;
procedure WriteComponentResFile(const FileName: string; const Instance: TComponent);

procedure GlobalFixupReferences;
procedure GetFixupReferenceNames(const Root: TComponent; const Names: TStrings);
procedure GetFixupInstanceNames(const Root: TComponent;
  const ReferenceRootName: string; Names: TStrings);
procedure RedirectFixupReferences(const Root: TComponent; const OldRootName,
  NewRootName: string);
procedure RemoveFixupReferences(const Root: TComponent; const RootName: string);
procedure RemoveFixups(const Instance: TPersistent);
function FindNestedComponent(const Root: TComponent; const NamePath: string): TComponent;

procedure BeginGlobalLoading;
procedure NotifyGlobalLoading;
procedure EndGlobalLoading;

function CollectionsEqual(const C1, C2: TCollection; const Owner1, Owner2: TComponent): Boolean;

{ find the ultimate owner of a collection or item (or persistent for that matter) }
function GetUltimateOwner(const ACollectionItem: TCollectionItem): TPersistent; overload;
function GetUltimateOwner(const ACollection: TCollection): TPersistent; overload;
function GetUltimateOwner(const APersistent: TPersistent): TPersistent; overload;

{ Object conversion routines }

type
  TStreamOriginalFormat = (sofUnknown, sofBinary, sofText, sofUTF8Text);

procedure ObjectBinaryToText(const Input, Output: TStream); overload;
procedure ObjectBinaryToText(const Input, Output: TStream;
  var OriginalFormat: TStreamOriginalFormat); overload;
procedure ObjectTextToBinary(const Input, Output: TStream); overload;
procedure ObjectTextToBinary(const Input, Output: TStream;
  var OriginalFormat: TStreamOriginalFormat); overload;

procedure ObjectResourceToText(const Input, Output: TStream); overload;
procedure ObjectResourceToText(const Input, Output: TStream;
  var OriginalFormat: TStreamOriginalFormat); overload;
procedure ObjectTextToResource(const Input, Output: TStream; const Name: string = ''); overload;
procedure ObjectTextToResource(const Input, Output: TStream;
  var OriginalFormat: TStreamOriginalFormat; const Name: string = ''); overload;

function TestStreamFormat(const Stream: TStream): TStreamOriginalFormat;

{ Windows resource header writer routines.  Used by ObjectTextToResource. }

function GetResourceName(const ObjStream: TStream; var AName: string): Boolean;
procedure WriteObjectResourceHeader(const ObjStream, Output: TStream; const Name: string = '');
procedure Write16bitResourceHeader(const AName: TBytes; DataSize: Integer; const Output: TStream);
procedure Write32bitResourceHeader(const AName: TBytes; DataSize: Integer; const Output: TStream);

{ Utility routines }

function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar;
  Strings: TStrings): Integer;

function FindRootDesigner(Obj: TPersistent): IDesignerNotify;

{$IFNDEF NEXTGEN}
function LineStart(Buffer, BufPos: PAnsiChar): PAnsiChar; overload;

procedure BinToHex(Buffer: PAnsiChar; Text: PWideChar; BufSize: Integer); overload;
procedure BinToHex(Buffer: PAnsiChar; Text: PAnsiChar; BufSize: Integer); overload;
procedure BinToHex(var Buffer; Text: PWideChar; BufSize: Integer); overload; inline;
procedure BinToHex(var Buffer; Text: PAnsiChar; BufSize: Integer); overload; inline;
procedure BinToHex(Buffer: Pointer; Text: PWideChar; BufSize: Integer); overload; inline;
procedure BinToHex(Buffer: Pointer; Text: PAnsiChar; BufSize: Integer); overload; inline;

function HexToBin(Text: PWideChar; Buffer: PAnsiChar; BufSize: Integer): Integer; overload;
function HexToBin(Text: PAnsiChar; Buffer: PAnsiChar; BufSize: Integer): Integer; overload;
function HexToBin(Text: PWideChar; var Buffer; BufSize: Integer): Integer; overload; inline;
function HexToBin(Text: PAnsiChar; var Buffer; BufSize: Integer): Integer; overload; inline;
function HexToBin(Text: PWideChar; Buffer: Pointer; BufSize: Integer): Integer; overload; inline;
function HexToBin(Text: PAnsiChar; Buffer: Pointer; BufSize: Integer): Integer; overload; inline;
{$ENDIF !NEXTGEN}

function LineStart(const Buffer: TBytes; BufPos: NativeInt): NativeInt; overload;
function LineStart(Buffer, BufPos: PChar): PChar; overload;

procedure BinToHex(const Buffer: TBytes; BufOffset: Integer;
  var Text: TBytes; TextOffset: Integer; Count: Integer); overload;

function HexToBin(const Text: PChar; TextOffset: Integer;
  var Buffer: TBytes; BufOffset: Integer; Count: Integer): Integer; overload;

function HexToBin(const Text: TBytes; TextOffset: Integer;
  var Buffer: TBytes; BufOffset: Integer; Count: Integer): Integer; overload;


{ CountGenerations:  Use this helper function to calculate the distance
  between two related classes.  Returns -1 if Descendent is not a descendent of
  Ancestor. }

function CountGenerations(Ancestor, Descendent: TClass): Integer;

{  Call CheckSynchronize periodically within the main thread in order for
   background threads to synchronize execution with the main thread.  This
   is mainly for applications that have an event driven UI such as Windows
   or XWindows (Qt/CLX).  The best place this can be called is during Idle
   processing.  This guarantees that the main thread is in a known "good"
   state so that method calls can be safely made.  Returns True if a method
   was synchronized.  Returns False if there was nothing done.
}
function CheckSynchronize(Timeout: Integer = 0): Boolean;

{ Assign a method to WakeMainThread in order to properly force an event into
  the GUI thread's queue.  This will make sure that non-GUI threads can quickly
  synchronize with the GUI thread even if no events are being processed due to
  an idle state }
var
  WakeMainThread: TNotifyEvent = nil;
{$IF Defined(MSWINDOWS)}
{ SyncEvent is an Event handle that is signaled every time a thread wishes to
  synchronize with the main thread or is terminating.  This handle us suitable
  for use with WaitForMultipleObjects.  When this object is signaled,
  CheckSynchronize *must* be called in order to reset the event.  Do not call
  ResetEvent on this handle, or background threads may hang waiting for
  Synchronize to return.
}
  SyncEvent: THandle;
{$ELSEIF Defined(POSIX)}
{ SyncEvent is a set of file descriptors representing a pipe.  The ReadDes field
  is suitable for use within a select or poll call.  When this file descriptor
  is signaled, a background thread wishes to synchronize with the main thread
  or is terminating.  When the ReadDes file descriptor is signaled,
  CheckSynchronize *must* be called to reset the file descriptor.  Do *not*
  actually call __read (or any other "read" function) with this file descriptor
  as that may cause a background thread to hang waiting for Synchronize to return.
}
  SyncEvent: TPipeDescriptors;
{$ENDIF POSIX}


type
{$IFDEF MSWINDOWS}
  TWndMethod = procedure(var Message: TMessage) of object;
{$ENDIF MSWINDOWS}

  TTextReader = class
  public
    procedure Close; virtual; abstract;
    function Peek: Integer; virtual; abstract;
    function Read: Integer; overload; virtual; abstract;
    function Read(var Buffer: TCharArray; Index, Count: Integer): Integer; overload; virtual; abstract;
    function ReadBlock(var Buffer: TCharArray; Index, Count: Integer): Integer; virtual; abstract;
    function ReadLine: string; virtual; abstract;
    function ReadToEnd: string; virtual; abstract;
  end;

  TTextWriter = class
  public
    procedure Close; virtual; abstract;
    procedure Flush; virtual; abstract;
    procedure Write(Value: Boolean); overload; virtual; abstract;
    procedure Write(Value: Char); overload; virtual; abstract;
    procedure Write(const Value: TCharArray); overload; virtual; abstract;
    procedure Write(Value: Double); overload; virtual; abstract;
    procedure Write(Value: Integer); overload; virtual; abstract;
    procedure Write(Value: Int64); overload; virtual; abstract;
    procedure Write(Value: TObject); overload; virtual; abstract;
    procedure Write(Value: Single); overload; virtual; abstract;
    procedure Write(const Value: string); overload; virtual; abstract;
    procedure Write(Value: Cardinal); overload; virtual; abstract;
    procedure Write(Value: UInt64); overload; virtual; abstract;
    procedure Write(const Format: string; Args: array of const); overload; virtual; abstract;
    procedure Write(const Value: TCharArray; Index, Count: Integer); overload; virtual; abstract;
    procedure WriteLine; overload; virtual; abstract;
    procedure WriteLine(Value: Boolean); overload; virtual; abstract;
    procedure WriteLine(Value: Char); overload; virtual; abstract;
    procedure WriteLine(const Value: TCharArray); overload; virtual; abstract;
    procedure WriteLine(Value: Double); overload; virtual; abstract;
    procedure WriteLine(Value: Integer); overload; virtual; abstract;
    procedure WriteLine(Value: Int64); overload; virtual; abstract;
    procedure WriteLine(Value: TObject); overload; virtual; abstract;
    procedure WriteLine(Value: Single); overload; virtual; abstract;
    procedure WriteLine(const Value: string); overload; virtual; abstract;
    procedure WriteLine(Value: Cardinal); overload; virtual; abstract;
    procedure WriteLine(Value: UInt64); overload; virtual; abstract;
    procedure WriteLine(const Format: string; Args: array of const); overload; virtual; abstract;
    procedure WriteLine(const Value: TCharArray; Index, Count: Integer); overload; virtual; abstract;
  end;

  TBinaryReader = class
  strict private
    FStream: TStream;
    FEncoding: TEncoding;
    FOwnsStream: Boolean;
    FTwoBytesPerChar: Boolean;
    FCharBytes: TBytes;
    FOneChar: TCharArray;
    FMaxCharsSize: Integer;
    function InternalReadChar: Integer;
    function InternalReadChars(const Chars: TCharArray; Index, Count: Integer): Integer;
  protected
    function GetBaseStream: TStream; virtual;
    function Read7BitEncodedInt: Integer; virtual;
  public
    constructor Create(Stream: TStream; AEncoding: TEncoding = nil; AOwnsStream: Boolean = False); overload;
    constructor Create(const Filename: string; Encoding: TEncoding = nil); overload;
    destructor Destroy; override;
    procedure Close; virtual;
    function PeekChar: Integer; virtual;
    function Read: Integer; overload; virtual;
    function Read(var Buffer: TCharArray; Index, Count: Integer): Integer; overload; virtual;
    function Read(const Buffer: TBytes; Index, Count: Integer): Integer; overload; virtual;
    function ReadBoolean: Boolean; virtual;
    function ReadByte: Byte; virtual;
    function ReadBytes(Count: Integer): TBytes; virtual;
    function ReadChar: Char; virtual;
    function ReadChars(Count: Integer): TCharArray; virtual;
    function ReadDouble: Double; virtual;
    function ReadSByte: ShortInt; inline;
    function ReadShortInt: ShortInt; virtual;
    function ReadSmallInt: SmallInt; virtual;
    function ReadInt16: SmallInt; inline;
    function ReadInteger: Integer; virtual;
    function ReadInt32: Integer; inline;
    function ReadInt64: Int64; virtual;
    function ReadSingle: Single; virtual;
    function ReadString: string; virtual;
    function ReadWord: Word; virtual;
    function ReadUInt16: Word; inline;
    function ReadCardinal: Cardinal; virtual;
    function ReadUInt32: Cardinal; inline;
    function ReadUInt64: UInt64; virtual;
    property BaseStream: TStream read GetBaseStream;
  end;

  TBinaryWriter = class
  strict private
    FStream: TStream;
    FOwnsStream: Boolean;
    FEncoding: TEncoding;
    class var FNull: TBinaryWriter;
    class destructor Destroy;
    class function GetNull: TBinaryWriter; static;
  protected
    function GetBaseStream: TStream; virtual;
    procedure Write7BitEncodedInt(Value: Integer); virtual;
    constructor Create; overload;
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Stream: TStream; Encoding: TEncoding); overload;
    constructor Create(Stream: TStream; Encoding: TEncoding; AOwnsStream: Boolean); overload;
    constructor Create(const Filename: string; Append: Boolean = False); overload;
    constructor Create(const Filename: string; Append: Boolean; Encoding: TEncoding); overload;
    destructor Destroy; override;
    procedure Close; virtual;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; virtual;
    procedure Write(Value: Byte); overload; virtual;
    procedure Write(Value: Boolean); overload; virtual;
    procedure Write(Value: Char); overload; virtual;
    procedure Write(const Value: TCharArray); overload; virtual;
    procedure Write(const Value: TBytes); overload; virtual;
    procedure Write(Value: Double); overload; virtual;
    procedure Write(Value: Integer); overload; virtual;
    procedure Write(Value: SmallInt); overload; virtual;
    procedure Write(Value: ShortInt); overload; virtual;
    procedure Write(Value: Word); overload; virtual;
    procedure Write(Value: Cardinal); overload; virtual;
    procedure Write(Value: Int64); overload; virtual;
    procedure Write(Value: Single); overload; virtual;
    procedure Write(const Value: string); overload; virtual;
    procedure Write(Value: UInt64); overload; virtual;
    procedure Write(const Value: TCharArray; Index, Count: Integer); overload; virtual;
    procedure Write(const Value: TBytes; Index, Count: Integer); overload; virtual;
    property BaseStream: TStream read GetBaseStream;
    class property Null: TBinaryWriter read GetNull;
  end;

  TStringReader = class(TTextReader)
  private
    FData: string;   //String Data being read
    FIndex: Integer; //Next character index to be read
  public
    constructor Create(S: string);
    procedure Close; override;
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(var Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(var Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
  end;

  TStringWriter = class(TTextWriter)
  private
    FBuilder: TStringBuilder;
    FOwnsBuilder: Boolean;
  public
    constructor Create; overload;
    constructor Create(Builder: TStringBuilder); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure Flush; override;
    procedure Write(Value: Boolean); override;
    procedure Write(Value: Char); override;
    procedure Write(const Value: TCharArray); override;
    procedure Write(Value: Double); override;
    procedure Write(Value: Integer); override;
    procedure Write(Value: Int64); override;
    procedure Write(Value: TObject); override;
    procedure Write(Value: Single); override;
    procedure Write(const Value: string); override;
    procedure Write(Value: Cardinal); override;
    procedure Write(Value: UInt64); override;
    procedure Write(const Format: string; Args: array of const); override;
    procedure Write(const Value: TCharArray; Index, Count: Integer); override;
    procedure WriteLine; override;
    procedure WriteLine(Value: Boolean); override;
    procedure WriteLine(Value: Char); override;
    procedure WriteLine(const Value: TCharArray); override;
    procedure WriteLine(Value: Double); override;
    procedure WriteLine(Value: Integer); override;
    procedure WriteLine(Value: Int64); override;
    procedure WriteLine(Value: TObject); override;
    procedure WriteLine(Value: Single); override;
    procedure WriteLine(const Value: string); override;
    procedure WriteLine(Value: Cardinal); override;
    procedure WriteLine(Value: UInt64); override;
    procedure WriteLine(const Format: string; Args: array of const); override;
    procedure WriteLine(const Value: TCharArray; Index, Count: Integer); override;
    function ToString: string; override;
  end;

  TStreamWriter = class(TTextWriter)
  private
    FStream: TStream;
    FEncoding: TEncoding;
    FNewLine: string;
    FAutoFlush: Boolean;
    FOwnsStream: Boolean;
    FBufferIndex: Integer;
    FBuffer: TBytes;
    procedure WriteBytes(Bytes: TBytes);
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Stream: TStream; Encoding: TEncoding; BufferSize: Integer = 4096); overload;
    constructor Create(const Filename: string; Append: Boolean = False); overload;
    constructor Create(const Filename: string; Append: Boolean; Encoding: TEncoding; BufferSize: Integer = 4096); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure Flush; override;
    procedure OwnStream; inline;
    procedure Write(Value: Boolean); override;
    procedure Write(Value: Char); override;
    procedure Write(const Value: TCharArray); override;
    procedure Write(Value: Double); override;
    procedure Write(Value: Integer); override;
    procedure Write(Value: Int64); override;
    procedure Write(Value: TObject); override;
    procedure Write(Value: Single); override;
    procedure Write(const Value: string); override;
    procedure Write(Value: Cardinal); override;
    procedure Write(Value: UInt64); override;
    procedure Write(const Format: string; Args: array of const); override;
    procedure Write(const Value: TCharArray; Index, Count: Integer); override;
    procedure WriteLine; override;
    procedure WriteLine(Value: Boolean); override;
    procedure WriteLine(Value: Char); override;
    procedure WriteLine(const Value: TCharArray); override;
    procedure WriteLine(Value: Double); override;
    procedure WriteLine(Value: Integer); override;
    procedure WriteLine(Value: Int64); override;
    procedure WriteLine(Value: TObject); override;
    procedure WriteLine(Value: Single); override;
    procedure WriteLine(const Value: string); override;
    procedure WriteLine(Value: Cardinal); override;
    procedure WriteLine(Value: UInt64); override;
    procedure WriteLine(const Format: string; Args: array of const); override;
    procedure WriteLine(const Value: TCharArray; Index, Count: Integer); override;
    property AutoFlush: Boolean read FAutoFlush write FAutoFlush;
    property NewLine: string read FNewLine write FNewLine;
    property Encoding: TEncoding read FEncoding;
    property BaseStream: TStream read FStream;
  end;

  TStreamReader = class(TTextReader)
  private
    FBufferedData: TStringBuilder;
    FBufferSize: Integer;
    FDetectBOM: Boolean;
    FEncoding: TEncoding;
    FNoDataInStream: Boolean;
    FOwnsStream: Boolean;
    FSkipPreamble: Boolean;
    FStream: TStream;
    function DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
    procedure FillBuffer(var Encoding: TEncoding);
    function GetEndOfStream: Boolean;
    function SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Stream: TStream; DetectBOM: Boolean); overload;
    constructor Create(Stream: TStream; Encoding: TEncoding;
      DetectBOM: Boolean = False; BufferSize: Integer = 4096); overload;
    constructor Create(const Filename: string); overload;
    constructor Create(const Filename: string; DetectBOM: Boolean); overload;
    constructor Create(const Filename: string; Encoding: TEncoding;
      DetectBOM: Boolean = False; BufferSize: Integer = 4096); overload;
    destructor Destroy; override;
    procedure Close; override;
    procedure DiscardBufferedData;
    procedure OwnStream; inline;
    function Peek: Integer; override;
    function Read: Integer; overload; override;
    function Read(var Buffer: TCharArray; Index, Count: Integer): Integer; overload; override;
    function ReadBlock(var Buffer: TCharArray; Index, Count: Integer): Integer; override;
    function ReadLine: string; override;
    function ReadToEnd: string; override;
    property BaseStream: TStream read FStream;
    property CurrentEncoding: TEncoding read FEncoding;
    property EndOfStream: Boolean read GetEndOfStream;
  end;

  ELoginCredentialError = class(Exception);

  TLoginCredentialService = class sealed
  public const
    Default = '';
    DefaultUsrPw = 'DefaultUsrPw'; // do not localize
    DefaultUsrPwDm = 'DefaultUsrPwDm'; //do not localize
  public type
    TLoginFunc = reference to function (const Username, Password, Domain: string): Boolean;
    TLoginEvent = procedure (Sender: TObject; const Username, Password, Domain: string; var Handled: Boolean) of object;
    TLoginCredentialEvent = procedure (Sender: TObject; Callback: TLoginEvent; var Success: Boolean) of object;
  strict private type
    TLoginCredentialEventObject = class
      Handler: TLoginCredentialEvent;
      constructor Create(const NewHandler: TLoginCredentialEvent);
    end;
    TLoginFuncProxy = class
    private
      FLoginFunc: TLoginFunc;
      procedure LoginEvent(Sender: TObject; const Username, Password, Domain: string; var Handled: Boolean);
    public
      constructor Create(const ALoginFunc: TLoginFunc);
    end;
  strict private class var
    FLoginHandlers: TStringList;
  strict private
    class constructor Create;
    class destructor Destroy;
    class function IndexOfHandler(const Context: TLoginCredentialEvent): Integer;
  public
    class procedure RegisterLoginHandler(const Context: string; const HandlerEvent: TLoginCredentialEvent); static;
    class procedure UnregisterLoginHandler(const Context: string; const HandlerEvent: TLoginCredentialEvent); static;

    class function GetLoginCredentialEvent(const Context: string): TLoginCredentialEvent; static;
    class function GetLoginCredentials(const Context: string; Sender: TObject; const Callback: TLoginEvent): Boolean; overload; static;
    class function GetLoginCredentials(const Context: string; const Callback: TLoginFunc): Boolean; overload; static;
    class function GetLoginCredentials(const Context: string; var Username, Password: string): Boolean; overload; static;
    class function GetLoginCredentials(const Context: string; var Username, Password, Domain: string): Boolean; overload; static;
  end;

  TPostFoundCycleProc = reference to procedure (const ClassName: string; Reference: IntPtr; const Stack: TStack<IntPtr>);

{$IFDEF MSWINDOWS}

function MakeObjectInstance(const AMethod: TWndMethod): Pointer;
procedure FreeObjectInstance(ObjectInstance: Pointer);

function AllocateHWnd(const AMethod: TWndMethod): HWND;
procedure DeallocateHWnd(Wnd: HWND);

{$ENDIF MSWINDOWS}

function AncestorIsValid(const Ancestor: TPersistent; const Root,
  RootAncestor: TComponent): Boolean;
function IsDefaultPropertyValue(const Instance: TObject; PropInfo: PPropInfo;
  OnGetLookupInfo: TGetLookupInfoEvent; Writer: TWriter = nil;
  OnFindMethodName: TFindMethodNameEvent = nil): Boolean;

{ Utility functions useful for tracking down reference cycles }

procedure CheckForCycles(const Obj: TObject; const PostFoundCycle: TPostFoundCycleProc); overload;
procedure CheckForCycles(const Intf: IInterface; const PostFoundCycle: TPostFoundCycleProc); overload;

implementation

uses
{$IF Defined(POSIX)}
  Posix.Base, Posix.Pthread, Posix.Sched, Posix.Stdlib, Posix.StrOpts,
  Posix.SysSelect, Posix.SysTime, Posix.Time,
{$IF Defined(LINUX)}
  Posix.Fcntl,
  Linuxapi.KernelIoctl,
{$ELSEIF Defined(MACOS)}
  Macapi.Mach,
  Macapi.ObjCRuntime,
{$ENDIF MACOS}
{$IFDEF ANDROID}
  Androidapi.NativeActivity,
  Androidapi.Jni,
{$ENDIF ANDROID}
{$ENDIF POSIX}
{$IFDEF USE_LIBICU}
  System.Internal.ICU,
{$ENDIF USE_LIBICU}
  System.Variants, System.Character, System.RTLConsts, System.SysConst;

const
// TODO -cELBRUS_LONGINT64 : Verify FilerSignature (change LongInt to UInt32)
  FilerSignature: UInt32 = $30465054; // ($54, $50, $46, $30) 'TPF0'

  H2BValidSet = ['0'..'9','A'..'F','a'..'f'];
  H2BConvert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);

{$IFNDEF MSWINDOWS}
type
  EThreadNameException = class(Exception);
{$ENDIF  MSWINDOWS}

{ Point and rectangle constructors }

function Point(AX, AY: Integer): TPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function SmallPoint(AX, AY: SmallInt): TSmallPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function PointsEqual(const P1, P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

function PointsEqual(const P1, P2: TSmallPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function Rect(const ATopLeft, ABottomRight: TPoint): TRect;
begin
  Result.Left := ATopLeft.X;
  Result.Top := ATopLeft.Y;
  Result.Right := ABottomRight.X;
  Result.Bottom := ABottomRight.Y;
end;

function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  Result := System.Types.Bounds(ALeft, ATop, AWidth, AHeight);
end;

function InvalidPoint(X, Y: Integer): Boolean;
begin
  Result := (X = -1) and (Y = -1);
end;

function InvalidPoint(const At: TPoint): Boolean;
begin
  Result := (At.X = -1) and (At.Y = -1);
end;

function InvalidPoint(const At: TSmallPoint): Boolean;
begin
  Result := (At.X = -1) and (At.Y = -1);
end;

function NextChar(P: PChar): PChar;
begin
  Result := P;
  if (Result <> nil) and (Result^ <> #0) then
  begin
    Inc(Result);
    if Result^.IsLowSurrogate then
      Inc(Result);
    while Result^.GetUnicodeCategory = TUnicodeCategory.ucNonSpacingMark do
      Inc(Result);
  end;
end;

{ Class registration groups }

type
  TPersistentClassDictionary = TDictionary<string, TPersistentClass>;
  TPersistentClassDictionaryElem = TPair<string, TPersistentClass>;

  TRegGroup = class
  private
  var
    FClassList: TPersistentClassDictionary;
    FAliasList: TPersistentClassDictionary;
    FGroupClasses: TList<TPersistentClass>;
    FActive: Boolean;
    function BestClass(AClass: TPersistentClass): TPersistentClass;
  public
    constructor Create(AClass: TPersistentClass);
{$IFNDEF AUTOREFCOUNT}
    destructor Destroy; override;
{$ENDIF}
    class function BestGroup(Group1, Group2: TRegGroup; AClass: TPersistentClass): TRegGroup;
    procedure AddClass(AClass: TPersistentClass);
    function GetClass(const AClassName: string): TPersistentClass;
    procedure GetClasses(Proc: TGetClass);
    function InGroup(AClass: TPersistentClass): Boolean;
    procedure RegisterClass(AClass: TPersistentClass);
    procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
    function Registered(AClass: TPersistentClass): Boolean;
    procedure UnregisterClass(AClass: TPersistentClass);
    procedure UnregisterModuleClasses(Module: HMODULE);
    property Active: Boolean read FActive write FActive;
  end;

  TRegGroups = class
  private
    FGroups: TObjectList<TRegGroup>;
    FActiveClass: TPersistentClass;
    function FindGroup(AClass: TPersistentClass): TRegGroup;
  public
    constructor Create;
{$IFNDEF AUTOREFCOUNT}
    destructor Destroy; override;
{$ENDIF}
    procedure Activate(AClass: TPersistentClass);
    procedure AddClass(ID: Integer; AClass: TPersistentClass);
    function GetClass(const AClassName: string): TPersistentClass;
    function GroupedWith(AClass: TPersistentClass): TPersistentClass;
    procedure GroupWith(AClass, AGroupClass: TPersistentClass);
    procedure Lock; inline;
    procedure RegisterClass(AClass: TPersistentClass);
    procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
    function Registered(AClass: TPersistentClass): Boolean;
    procedure StartGroup(AClass: TPersistentClass);
    procedure Unlock; inline;
    procedure UnregisterClass(AClass: TPersistentClass);
    procedure UnregisterModuleClasses(Module: HMODULE);
    property ActiveClass: TPersistentClass read FActiveClass;
  end;

var
  RegGroups: TRegGroups;
  DictComparer: TStringComparer;

{ TRegGroup }

procedure TRegGroup.AddClass(AClass: TPersistentClass);
begin
  FGroupClasses.Add(AClass);
end;

function TRegGroup.BestClass(AClass: TPersistentClass): TPersistentClass;
var
  I: Integer;
  Current: TPersistentClass;
begin
  Result := nil;
  for I := 0 to FGroupClasses.Count - 1 do
  begin
    Current := FGroupClasses[I];
    if AClass.InheritsFrom(Current) then
      if (Result = nil) or Current.InheritsFrom(Result) then
        Result := Current;
  end;
end;

class function TRegGroup.BestGroup(Group1, Group2: TRegGroup;
  AClass: TPersistentClass): TRegGroup;
var
  Group1Class: TPersistentClass;
  Group2Class: TPersistentClass;
begin
  if Group1 <> nil then
    Group1Class := Group1.BestClass(AClass) else
    Group1Class := nil;
  if Group2 <> nil then
    Group2Class := Group2.BestClass(AClass) else
    Group2Class := nil;
  if Group1Class = nil then
    if Group2Class = nil then
      // AClass is not in either group, no best group
      Result := nil
    else
      // AClass is in Group2 but not Group1, Group2 is best
      Result := Group2
  else
    if Group2Class = nil then
      // AClass is in Group1 but not Group2, Group1 is best
      Result := Group1
    else
      // AClass is in both groups, select the group with the closest ancestor
      if Group1Class.InheritsFrom(Group2Class) then
        Result := Group1
      else
        Result := Group2;
end;

constructor TRegGroup.Create(AClass: TPersistentClass);
begin
  inherited Create;
  FGroupClasses := TList<TPersistentClass>.Create;
  FGroupClasses.Add(AClass);
end;

{$IFNDEF AUTOREFCOUNT}
destructor TRegGroup.Destroy;
begin
  inherited Destroy;
  FClassList.Free;
  FAliasList.Free;
  FGroupClasses.Free;
end;
{$ENDIF}

function TRegGroup.GetClass(const AClassName: string): TPersistentClass;
begin
  Result := nil;
  if FClassList <> nil then
    FClassList.TryGetValue(AClassName, Result);
  if (Result = nil) and (FAliasList <> nil) then
    FAliasList.TryGetValue(AClassName, Result);
end;

procedure TRegGroup.GetClasses(Proc: TGetClass);
var
  p: TPersistentClass;
begin
  if FClassList <> nil then
    for P in FClassList.Values do
      Proc(P);
end;

function TRegGroup.InGroup(AClass: TPersistentClass): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FGroupClasses.Count - 1 do
    if AClass.InheritsFrom(TPersistentClass(FGroupClasses[I])) then Exit;
  Result := False;
end;

procedure TRegGroup.RegisterClass(AClass: TPersistentClass);
var
  LClassName: string;
  LClass: TPersistentClass;
begin
  LClassName := AClass.ClassName;
  LClass := GetClass(LClassName);

  if LClass = nil then
  begin
    if FClassList = nil then
      FClassList := TPersistentClassDictionary.Create(DictComparer);
    FClassList.Add(LClassName, AClass);
  end
  else
    if LClass <> AClass then
      raise EFilerError.CreateResFmt(@SDuplicateClass, [LClassName]);
end;

procedure TRegGroup.RegisterClassAlias(AClass: TPersistentClass;
  const Alias: string);
begin
  RegisterClass(AClass);
  if FAliasList = nil then
    FAliasList := TPersistentClassDictionary.Create(DictComparer);
  FAliasList.Add(Alias, AClass);
end;

function TRegGroup.Registered(AClass: TPersistentClass): Boolean;
begin
  Result := (FClassList <> nil) and FClassList.ContainsValue(AClass);
end;

procedure TRegGroup.UnregisterClass(AClass: TPersistentClass);
var
  pair: TPersistentClassDictionaryElem;
begin
  if FClassList <> nil then
  begin
    for pair in FClassList do
    begin
      if pair.Value = AClass then
      begin
        FClassList.Remove(pair.Key);
        break;
      end;
    end;
  end;
  if FAliasList <> nil then
  begin
    while FAliasList.ContainsValue(AClass) do
    begin
      for pair in FAliasList do
      begin
        if pair.Value = AClass then
        begin
          FAliasList.Remove(pair.Key);
          break;
        end;
      end;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
function PointerInModule(Ptr: Pointer; ModuleStartAddr, ModuleStopAddr: Pointer): Boolean; inline;
begin
  Result := (UIntPtr(Ptr) >= UIntPtr(ModuleStartAddr)) and (UIntPtr(Ptr) < UIntPtr(ModuleStopAddr));
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
function PointerInModule(Ptr: Pointer; Module: HMODULE): Boolean;
begin
  Result := (Module = 0) or (FindHInstance(Ptr) = Module);
end;
{$ENDIF POSIX}

procedure TRegGroup.UnregisterModuleClasses(Module: HMODULE);
var
  I: Integer;
  pair: TPersistentClassDictionaryElem;
  NewList: TPersistentClassDictionary;
  {$IFDEF MSWINDOWS}
  Deleted: Boolean;
  ModuleStartAddr, ModuleStopAddr: Pointer;
  MemInfo: TMemoryBasicInformation;
  CurProcess: THandle;
  {$ENDIF MSWINDOWS}
begin
  // Even though the group criterion changes we do not need to recalculate the
  // groups because the groups are based on ancestry. If an ancestor of a class
  // is removed because its module was unloaded we can safely assume that all
  // descendents have also been unloaded or are being unloaded as well. This
  // means that any classes left in the registry are not descendents of the
  // classes being removed and, therefore, will not be affected by the change
  // to the FGroupClasses list.

  if Module <> 0 then
  begin
    {$IFDEF MSWINDOWS}
    { Find all allocated memory pages for the module. This is faster than
      calling VirtualQuery for every class in the list. }
    ModuleStartAddr := Pointer(Module);
    ModuleStopAddr := ModuleStartAddr;
    CurProcess := GetCurrentProcess;
    while (VirtualQueryEx(CurProcess, ModuleStopAddr, MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo)) and
          (MemInfo.AllocationBase = ModuleStartAddr) do
        ModuleStopAddr := Pointer(Cardinal(ModuleStopAddr) + MemInfo.RegionSize);

    { FGroupClasses }
    Deleted := False;
    for I := FGroupClasses.Count - 1 downto 0 do
    begin
      if PointerInModule(FGroupClasses[I], ModuleStartAddr, ModuleStopAddr) then
      begin
        FGroupClasses[I] := nil;
        Deleted := True;
      end;
    end;
    if Deleted then
      FGroupClasses.Pack;
    if FClassList <> nil then
    begin
      NewList := TPersistentClassDictionary.Create(DictComparer);
      for pair in FClassList do
        if not PointerInModule(pair.Value, ModuleStartAddr, ModuleStopAddr) then
          NewList.Add(pair.Key, pair.Value);
      FreeAndNil(FClassList);
      FClassList := NewList;
    end;
    if FAliasList <> nil then
    begin
      NewList := TPersistentClassDictionary.Create(DictComparer);
      for pair in FAliasList do
        if not PointerInModule(pair.Value, ModuleStartAddr, ModuleStopAddr) then
          NewList.Add(pair.Key, pair.Value);
      FreeAndNil(FAliasList);
      FAliasList := NewList;
    end;
    {$ENDIF MSWINDOWS}
    {$IFDEF POSIX}
    for I := FGroupClasses.Count - 1 downto 0 do
      if PointerInModule(FGroupClasses[I], Module) then
        FGroupClasses.Delete(I);
    if FClassList <> nil then
    begin
      NewList := TPersistentClassDictionary.Create(DictComparer);
      for pair in FClassList do
        if not PointerInModule(pair.Value, Module) then
          NewList.Add(pair.Key, pair.Value);
      FreeAndNil(FClassList);
      FClassList := NewList;
    end;
    if FAliasList <> nil then
    begin
      NewList := TPersistentClassDictionary.Create(DictComparer);
      for pair in FAliasList do
        if not PointerInModule(pair.Value, Module) then
          NewList.Add(pair.Key, pair.Value);
      FreeAndNil(FAliasList);
      FAliasList := NewList;
    end;
    {$ENDIF POSIX}
  end
  else
  begin
    FGroupClasses.Clear;
    if FClassList <> nil then
      FClassList.Clear;
    if FAliasList <> nil then
      FAliasList.Clear;
  end;
end;

{ TRegGroups }

procedure TRegGroups.Activate(AClass: TPersistentClass);
var
  I: Integer;
  LRegGroup: TRegGroup;
begin
  if FActiveClass <> AClass then
  begin
    FActiveClass := AClass;
    for I := 0 to FGroups.Count - 1 do
    begin
      LRegGroup := FGroups[I];
      LRegGroup.Active := LRegGroup.InGroup(AClass);
    end;
  end;
end;

procedure TRegGroups.AddClass(ID: Integer; AClass: TPersistentClass);
begin
  FGroups[ID].AddClass(AClass);
end;

constructor TRegGroups.Create;
var
  Group: TRegGroup;
begin
  inherited Create;
  FGroups := TObjectList<TRegGroup>.Create;
  // Initialize default group
  Group := TRegGroup.Create(TPersistent);
  FGroups.Add(Group);
  Group.Active := True;
end;

{$IFNDEF AUTOREFCOUNT}
destructor TRegGroups.Destroy;
begin
  FGroups.Free;
  inherited;
end;
{$ENDIF}

function TRegGroups.FindGroup(AClass: TPersistentClass): TRegGroup;
var
  I: Integer;
  Current: TRegGroup;
begin
  Result := nil;
  for I := 0 to FGroups.Count - 1 do
  begin
    Current := FGroups[I];
    Result := TRegGroup.BestGroup(Current, Result, AClass);
  end;
end;

function TRegGroups.GetClass(const AClassName: string): TPersistentClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FGroups.Count - 1 do
  begin
    if FGroups[I].Active then Result := FGroups[I].GetClass(AClassName);
    if Result <> nil then Exit;
  end;
end;

function TRegGroups.GroupedWith(AClass: TPersistentClass): TPersistentClass;
var
  Group: TRegGroup;
begin
  Result := nil;
  Group := FindGroup(AClass);
  if Group <> nil then
    Result := TPersistentClass(Group.FGroupClasses[0]);
end;

procedure TRegGroups.GroupWith(AClass, AGroupClass: TPersistentClass);

  procedure Error;
  begin
    raise EFilerError.CreateFmt(SUnknownGroup, [AGroupClass.ClassName]);
  end;

var
  Group: TRegGroup;
  CurrentGroup: TRegGroup;
  I: Integer;
  NewList: TPersistentClassDictionary;
  Pair: TPersistentClassDictionaryElem;
begin
  Group := FindGroup(AGroupClass);
  if Group = nil then Error;
  Group.AddClass(AClass);

  // The group criterion has changed. We need to recalculate which groups the
  // classes that have already been registered belong to. We can skip
  // Group since we would just be moving a class to a group it already belongs
  // to. We also only need to find the new group of classes that descend from
  // AClass since that is the only criterion being changed. In other words,
  // we only need to move classes that descend from AClass to Group if they
  // are in another group.
  for I := 0 to FGroups.Count - 1 do
  begin
    CurrentGroup := FGroups[I];
    if CurrentGroup <> Group then
    begin
      if CurrentGroup.FClassList <> nil then
      begin
        NewList := TPersistentClassDictionary.Create(DictComparer);
        for Pair in CurrentGroup.FClassList do
        begin
          // Check CurrentClass should be put into Group based on the new
          // criterion. Their might be a descendent of AClass registered that
          // overrides Group's criterion.
          if Pair.Value.InheritsFrom(AClass) and (FindGroup(Pair.Value) = Group) then
            Group.RegisterClass(pair.Value)
          else
            NewList.Add(Pair.Key, Pair.Value);
        end;
        FreeAndNil(CurrentGroup.FClassList);
        CurrentGroup.FClassList := NewList;
      end;
    end;
  end;
end;

procedure TRegGroups.Lock;
begin
  MonitorEnter(Self);
end;

procedure TRegGroups.RegisterClass(AClass: TPersistentClass);
var
  Group: TRegGroup;
begin
  Group := FindGroup(AClass);
  if Group <> nil then Group.RegisterClass(AClass);
end;

procedure TRegGroups.RegisterClassAlias(AClass: TPersistentClass;
  const Alias: string);
var
  Group: TRegGroup;
begin
  Group := FindGroup(AClass);
  if Group <> nil then Group.RegisterClassAlias(AClass, Alias);
end;

function TRegGroups.Registered(AClass: TPersistentClass): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FGroups.Count - 1 do
    if FGroups[I].Registered(AClass) then Exit;
  Result := False;
end;

procedure TRegGroups.StartGroup(AClass: TPersistentClass);
var
  I: Integer;
begin
  // Do not start a group that already exists
  for I := 0 to FGroups.Count - 1 do
    if FGroups[I].FGroupClasses.IndexOf(AClass) >= 0 then
      Exit;
  // Create the group
  FGroups.Add(TRegGroup.Create(AClass));
end;

procedure TRegGroups.Unlock;
begin
  MonitorExit(Self);
end;

procedure TRegGroups.UnregisterClass(AClass: TPersistentClass);
var
  I: Integer;
begin
  for I := 0 to FGroups.Count - 1 do
    FGroups[I].UnregisterClass(AClass);
end;

procedure TRegGroups.UnregisterModuleClasses(Module: HMODULE);
var
  I: Integer;
  Group: TRegGroup;
begin
  for I := FGroups.Count - 1 downto 0 do
  begin
    Group := FGroups[I];
    Group.UnregisterModuleClasses(Module);
    if Group.FGroupClasses.Count = 0 then
      FGroups.Delete(I);
  end;
end;

{ TClassFinder }

constructor TClassFinder.Create(AClass: TPersistentClass; AIncludeActiveGroups: Boolean);
var
  I: Integer;
  Group: TRegGroup;
begin
  inherited Create;
  FGroups := TList<TObject>.Create;
  RegGroups.Lock;
  try
    if AClass = nil then AClass := RegGroups.ActiveClass;
    for I := 0 to RegGroups.FGroups.Count - 1 do
    begin
      Group := RegGroups.FGroups[I];
      if Group.InGroup(AClass) then
        FGroups.Add(Group);
    end;
    if AIncludeActiveGroups then
      for I := 0 to RegGroups.FGroups.Count - 1 do
      begin
        Group := RegGroups.FGroups[I];
        if Group.Active then
          FGroups.Add(Group);
      end;
    FClass := AClass;
  finally
    RegGroups.Unlock;
  end;
end;

{$IFNDEF AUTOREFCOUNT}
destructor TClassFinder.Destroy;
begin
  FGroups.Free;
  inherited;
end;
{$ENDIF}

function TClassFinder.GetClass(const AClassName: string): TPersistentClass;
var
  I: Integer;
begin
  Result := nil;
  RegGroups.Lock;
  try
    for I := 0 to FGroups.Count - 1 do
    begin
      Result := TRegGroup(FGroups[I]).GetClass(AClassName);
      if Result <> nil then Exit;
    end;
  finally
    RegGroups.Unlock;
  end;
end;

procedure TClassFinder.GetClasses(Proc: TGetClass);
var
  I: Integer;
begin
  RegGroups.Lock;
  try
    for I := 0 to FGroups.Count - 1 do
      TRegGroup(FGroups[I]).GetClasses(Proc);
  finally
    RegGroups.Unlock;
  end;
end;

{ Class registration routines }

type
  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = packed record
    Count: Smallint;
    Classes: array[0..8191] of ^TPersistentClass;
  end;

function GetFieldClassTable(AClass: TClass): PFieldClassTable;
{$IFNDEF X86ASM}
var
  LFieldTablePtr: IntPtr;
begin
  LFieldTablePtr := IntPtr(PPointer(PByte(AClass) + vmtFieldTable)^);
  if LFieldTablePtr <> 0 then
    Result := PPointer(LFieldTablePtr + 2)^
  else
    Result := nil;
end;
{$ELSE X86ASM}
asm
        MOV     EAX,[EAX].vmtFieldTable
        OR      EAX,EAX
        JE      @@1
        MOV     EAX,[EAX+2].Integer
@@1:
end;
{$ENDIF X86ASM}

procedure ClassNotFound(const ClassName: string);
begin
  raise EClassNotFound.CreateFmt(SClassNotFound, [ClassName]);
end;

function GetClass(const AClassName: string): TPersistentClass;
begin
  RegGroups.Lock;
  try
    Result := RegGroups.GetClass(AClassName);
  finally
    RegGroups.Unlock;
  end;
end;

function FindClass(const ClassName: string): TPersistentClass;
begin
  Result := GetClass(ClassName);
  if Result = nil then ClassNotFound(ClassName);
end;

procedure RegisterClass(AClass: TPersistentClass);
begin
  RegGroups.Lock;
  try
    while not RegGroups.Registered(AClass) do
    begin
      RegGroups.RegisterClass(AClass);
      if AClass = TPersistent then Break;
      AClass := TPersistentClass(AClass.ClassParent);
    end;
  finally
    RegGroups.Unlock;
  end;
end;

procedure RegisterClasses(const AClasses: array of TPersistentClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do RegisterClass(AClasses[I]);
end;

procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);
begin
  RegGroups.Lock;
  try
    RegGroups.RegisterClassAlias(AClass, Alias);
  finally
    RegGroups.Unlock;
  end;
end;

procedure UnRegisterClass(AClass: TPersistentClass);
begin
  RegGroups.Lock;
  try
    RegGroups.UnregisterClass(AClass);
  finally
    RegGroups.Unlock;
  end;
end;

procedure UnRegisterClasses(const AClasses: array of TPersistentClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do UnRegisterClass(AClasses[I]);
end;

procedure UnRegisterModuleClasses(Module: HMODULE);
begin
  RegGroups.Lock;
  try
    RegGroups.UnregisterModuleClasses(Module);
  finally
    RegGroups.Unlock;
  end;
end;

procedure StartClassGroup(AClass: TPersistentClass);
begin
  RegGroups.Lock;
  try
    RegGroups.StartGroup(AClass);
  finally
    RegGroups.Unlock;
  end;
end;

procedure GroupDescendentsWith(AClass, AClassGroup: TPersistentClass);
begin
  RegGroups.Lock;
  try
    RegGroups.GroupWith(AClass, AClassGroup);
  finally
    RegGroups.Unlock;
  end;
end;

function ActivateClassGroup(AClass: TPersistentClass): TPersistentClass;
begin
  RegGroups.Lock;
  try
    Result := RegGroups.ActiveClass;
    RegGroups.Activate(AClass);
  finally
    RegGroups.Unlock;
  end;
end;

function ActiveClassGroup: TPersistentClass;
begin
  RegGroups.Lock;
  try
    Result := RegGroups.ActiveClass;
  finally
    RegGroups.Unlock;
  end;
end;

function ClassGroupOf(AClass: TPersistentClass): TPersistentClass;
begin
  RegGroups.Lock;
  try
    Result := RegGroups.GroupedWith(AClass);
  finally
    RegGroups.Unlock;
  end;
end;

function ClassGroupOf(Instance: TPersistent): TPersistentClass;
begin
  RegGroups.Lock;
  try
    Result := nil;
    while Instance <> nil do
    begin
      Result := RegGroups.GroupedWith(TPersistentClass(Instance.ClassType));
      if Result <> nil then
        Exit
      else
        Instance := Instance.GetOwner;
    end;
  finally
    RegGroups.Unlock;
  end;
end;

procedure ReportClassGroups(Report: TStrings);
var
  I, J: Integer;
  pair: TPersistentClassDictionaryElem;
  Group: TRegGroup;
  ClassGroupHeader, GroupClassesHeader, ClassListHeader, ClassAliasesHeader: string;
begin
  Report.BeginUpdate;
  try
    Report.Clear;
    ClassGroupHeader := sClassGroupHeader;
    GroupClassesHeader := sGroupClassesHeader;
    ClassListHeader := sClassListHeader;
    ClassAliasesHeader := sClassAliasesHeader;
    for I := 0 to RegGroups.FGroups.Count - 1 do
    begin
      Group := TRegGroup(RegGroups.FGroups[I]);
      Report.Add(Format(ClassGroupHeader, [I, BoolToStr(Group.Active, True)]));
      Report.Add(GroupClassesHeader);
      for J := 0 to Group.FGroupClasses.Count - 1 do
        Report.Add(Format('    %s', [TPersistentClass(Group.FGroupClasses[J]).QualifiedClassName])); // do not localize
      Report.Add(ClassListHeader);
      for pair in Group.FClassList do
        Report.Add(Format('    %s = %s', [pair.Key, pair.Value.QualifiedClassName])); // do not localilze
      if (Group.FAliasList <> nil) and (Group.FAliasList.Count > 0) then
      begin
        Report.Add(ClassAliasesHeader);
        for pair in Group.FAliasList do
          Report.Add(Format('    %s = %s', [pair.Key, pair.Value.QualifiedClassName])); // do not localilze
      end;
    end;
  finally
    Report.EndUpdate;
  end;
end;

{ Component registration routines }

procedure RegisterComponents(const Page: string;
  ComponentClasses: array of TComponentClass);
//  const ComponentClasses: array of TComponentClass);
begin
  if Assigned(RegisterComponentsProc) then
    RegisterComponentsProc(Page, ComponentClasses)
  else
    raise EComponentError.CreateRes(@SRegisterError);
end;

procedure RegisterNoIcon(const ComponentClasses: array of TComponentClass);
begin
  if Assigned(RegisterNoIconProc) then
    RegisterNoIconProc(ComponentClasses)
  else
    raise EComponentError.CreateRes(@SRegisterError);
end;

{$IFDEF MSWINDOWS}
procedure RegisterNonActiveX(const ComponentClasses: array of TComponentClass;
  AxRegType: TActiveXRegType);
begin
  if not Assigned(RegisterNonActiveXProc) then
    raise EComponentError.CreateRes(@SRegisterError);
  RegisterNonActiveXProc(ComponentClasses, AxRegType)
end;
{$ENDIF MSWINDOWS}

{ Component filing }

type
  TIntConst = class
    IntegerType: PTypeInfo;
    IdentToInt: TIdentToInt;
    IntToIdent: TIntToIdent;
    constructor Create(AIntegerType: PTypeInfo; AIdentToInt: TIdentToInt;
      AIntToIdent: TIntToIdent);
  end;

var
  IntConstList: TThreadList<TIntConst>;

constructor TIntConst.Create(AIntegerType: PTypeInfo; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
begin
  IntegerType := AIntegerType;
  IdentToInt := AIdentToInt;
  IntToIdent := AIntToIdent;
end;

procedure RegisterIntegerConsts(AIntegerType: Pointer; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
begin
  IntConstList.Add(TIntConst.Create(AIntegerType, AIdentToInt, AIntToIdent));
end;

procedure UnregisterIntegerConsts(AIntegerType: Pointer; AIdentToInt: TIdentToInt;
  AIntToIdent: TIntToIdent);
var
  I: Integer;
  aLockList: TList<TIntConst>;
  aIntConst: TIntConst;
begin
  aLockList := IntConstList.LockList;
  try
    for I := aLockList.Count-1 downto 0 do
    begin
      aIntConst := aLockList.Items[I];
      if (aIntConst.IntegerType = AIntegerType) and
         (@aIntConst.IntToIdent = @AIntToIdent) and
         (@aIntConst.IdentToInt = @AIdentToInt) then
      begin
        TIntConst(aLockList.Items[I]).DisposeOf;
        aLockList.Delete(I);
      end;
    end;
  finally
    IntConstList.UnlockList;
  end;
end;

function FindIntToIdent(AIntegerType: Pointer): TIntToIdent;
var
  I: Integer;
  aLockList: TList<TIntConst>;
  aIntConst: TIntConst;
begin
  Result := nil;
  aLockList := IntConstList.LockList;
  try
    for I := aLockList.Count - 1 downto 0 do
    begin
      aIntConst := aLockList.Items[I];
      if AIntegerType = aIntConst.IntegerType then
      begin
        Result := @aIntConst.IntToIdent;
        Exit;
      end;
    end;
  finally
    IntConstList.UnlockList;
  end;
end;

function FindIdentToInt(AIntegerType: Pointer): TIdentToInt;
var
  I: Integer;
  aLockList: TList<TIntConst>;
  aIntConst: TIntConst;
begin
  Result := nil;
  aLockList := IntConstList.LockList;
  try
    for I := aLockList.Count - 1 downto 0 do
    begin
      aIntConst := aLockList.Items[I];
      begin
        if AIntegerType = aIntConst.IntegerType then
        begin
          Result := @aIntConst.IdentToInt;
          Exit;
        end;
      end;
    end;
  finally
    IntConstList.UnlockList;
  end;
end;

function IdentToInt(const Ident: string; var Int: Integer; const Map: array of TIdentMapEntry): Boolean;
var
  I: Integer;
begin
  for I := Low(Map) to High(Map) do
    if SameText(Map[I].Name, Ident) then
    begin
      Result := True;
      Int := Map[I].Value;
      Exit;
    end;
  Result := False;
end;

function IntToIdent(Int: Integer; var Ident: string; const Map: array of TIdentMapEntry): Boolean;
var
  I: Integer;
begin
  for I := Low(Map) to High(Map) do
    if Map[I].Value = Int then
    begin
      Result := True;
      Ident := Map[I].Name;
      Exit;
    end;
  Result := False;
end;

var
  FindGlobalComponentProcs: TList<TFindGlobalComponent>;

procedure RegisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
begin
  if FindGlobalComponentProcs = nil then
    FindGlobalComponentProcs := TList<TFindGlobalComponent>.Create;
  if FindGlobalComponentProcs.IndexOf(AFindGlobalComponent) < 0 then
    FindGlobalComponentProcs.Add(AFindGlobalComponent);
end;

procedure UnregisterFindGlobalComponentProc(AFindGlobalComponent: TFindGlobalComponent);
begin
  if FindGlobalComponentProcs <> nil then
    FindGlobalComponentProcs.Remove(AFindGlobalComponent);
end;

function FindGlobalComponent(const Name: string): TComponent;
var
  I: Integer;
begin
  Result := nil;
  if FindGlobalComponentProcs <> nil then
  begin
    for I := FindGlobalComponentProcs.Count - 1 downto 0 do
    begin
      Result := FindGlobalComponentProcs[I](Name);
      if Result <> nil then Exit;
    end;
  end;
end;

function IsUniqueGlobalComponentName(const Name: string): Boolean;
begin
  if Assigned(IsUniqueGlobalComponentNameProc) then
    Result := IsUniqueGlobalComponentNameProc(Name)
  else Result := FindGlobalComponent(Name) = nil;
end;

// TODO -odtotoliciu -cTest : WRITE TEST: function InternalReadComponentRes(const ResName: UnicodeString; HInst: THandle; var Instance: TComponent): Boolean; overload;
function InternalReadComponentRes(const ResName: string; HInst: THandle; var Instance: TComponent): Boolean; overload;
var
  HRsrc: THandle;
  aResourceStream: TResourceStream;
begin
  if HInst = 0 then HInst := HInstance;
  HRsrc := FindResource(HInst, PChar(ResName), PChar(RT_RCDATA));
  Result := HRsrc <> 0;
  if not Result then Exit;
  aResourceStream := TResourceStream.Create(HInst, ResName, RT_RCDATA);
  try
    Instance := aResourceStream.ReadComponent(Instance);
  finally
    aResourceStream.Free;
  end;
  Result := True;
end;

threadvar
  GlobalLoaded: TList<TComponent>;
  GlobalLists: TList<TList<TComponent>>;

procedure BeginGlobalLoading;
var
  G: TList<TList<TComponent>>;
begin
  TRttiContext.KeepContext; // Make sure the Rtti context is created and stays while loading
  G := GlobalLists;
  if G = nil then
  begin
    G := TList<TList<TComponent>>.Create;
    GlobalLists := G;
  end;
  G.Add(GlobalLoaded);
  GlobalLoaded := TList<TComponent>.Create;
end;

procedure NotifyGlobalLoading;
var
  I: Integer;
  G: TList<TComponent>;
begin
  G := GlobalLoaded;  // performance:  eliminate repeated trips through TLS lookup
  for I := 0 to G.Count - 1 do
    TComponent(G[I]).Loaded;
end;

procedure EndGlobalLoading;
var
  G: TList<TList<TComponent>>;
begin
  GlobalLoaded.Free;
  G := GlobalLists;
  GlobalLoaded := G.Last;
  G.Delete(G.Count - 1);
  if G.Count = 0 then
  begin
    GlobalLists := nil;
    G.Free;
  end;
  TRttiContext.DropContext; // finally drop the Rtti context.
end;

function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  begin
    Result := False;
    if (ClassType = TComponent) or (ClassType = RootAncestor) then Exit;
    Result := InitComponent(ClassType.ClassParent);
    Result := InternalReadComponentRes(ClassType.ClassName,
      FindResourceHInstance(FindClassHInstance(ClassType)), Instance) or Result;
  end;

var
  LocalizeLoading: Boolean;
begin
  GlobalNameSpace.BeginWrite;  // hold lock across all ancestor loads (performance)
  try
    LocalizeLoading := (Instance.ComponentState * [csInline, csLoading]) = [];
    if LocalizeLoading then BeginGlobalLoading;  // push new loadlist onto stack
    try
      Result := InitComponent(Instance.ClassType);
      if Result then Instance.ReadDeltaState;
      if LocalizeLoading then NotifyGlobalLoading;  // call Loaded
    finally
      if LocalizeLoading then EndGlobalLoading;  // pop loadlist off stack
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

function ReadComponentDeltaRes(Instance: TComponent; const DeltaCandidates: array of string; const Proc: TGetStreamProc): TComponent;
var
  HInstance: THandle;
  HRsrc: THandle;
  RootName, Delta, ResName: string;
  S: TStream;
begin
  if (Instance <> nil) and Assigned(Proc) then
    HInstance := FindResourceHInstance(FindClassHInstance(Instance.ClassType))
  else raise EArgumentNilException.CreateRes(@SArgumentNil);
  Result := Instance;
  RootName := Instance.ClassType.ClassName;
  for Delta in DeltaCandidates do
  begin
    ResName := string.Join('_', [RootName, Delta]);
    HRsrc := FindResource(HInstance, PChar(ResName), PChar(RT_RCDATA));
    if HRsrc <> 0 then
    begin
      S := TResourceStream.Create(HInstance, ResName, PChar(RT_RCDATA));
      try
        Proc(S);
        Exit;
      finally
        S.Free;
      end;
    end;
  end;
end;

function InitComponentRes(const ResName: string; Instance: TComponent): Boolean;
begin
  Result := InternalReadComponentRes(ResName, FindResourceHInstance(
    FindClassHInstance(Instance.ClassType)), Instance);
end;

function ReadComponentRes(const ResName: string; Instance: TComponent): TComponent;
var
  HInstance: THandle;
begin
  if Instance <> nil then
    HInstance := FindResourceHInstance(FindClassHInstance(Instance.ClassType))
  else HInstance := 0;
  if InternalReadComponentRes(ResName, HInstance, Instance) then
    Result := Instance else
    raise EResNotFound.CreateFmt(SResNotFound, [ResName]);
end;

function ReadComponentResEx(HInstance: THandle; const ResName: string): TComponent;
var
  Instance: TComponent;
begin
  Instance := nil;
  if InternalReadComponentRes(ResName, HInstance, Instance) then
    Result := Instance else
    raise EResNotFound.CreateFmt(SResNotFound, [ResName]);
end;

function ReadComponentResFile(const FileName: string; const Instance: TComponent): TComponent;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := Stream.ReadComponentRes(Instance);
  finally
    Stream.Free;
  end;
end;

procedure WriteComponentResFile(const FileName: string; const Instance: TComponent);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteComponentRes(Instance.ClassName, Instance);
  finally
    Stream.Free;
  end;
end;

function CollectionsEqual(const C1, C2: TCollection; const Owner1, Owner2: TComponent): Boolean;
var
  S1, S2: TMemoryStream;

  procedure WriteCollection(const Stream: TStream; const Collection: TCollection; const CollectionOwner: TComponent);
  var
    Writer: TWriter;
  begin
    Writer := TWriter.Create(Stream, 1024);
    Writer.Root := CollectionOwner;
    Writer.FLookupRoot := CollectionOwner;
    try
      Writer.WriteCollection(Collection);
    finally
      Writer.Free;
    end;
  end;

begin
  Result := False;
  if C1.ClassType <> C2.ClassType then Exit;
  if C1.Count <> C2.Count then Exit;
  S1 := TMemoryStream.Create;
  try
    WriteCollection(S1, C1, Owner1);
    S2 := TMemoryStream.Create;
    try
      WriteCollection(S2, C2, Owner2);
      Result := (S1.Size = S2.Size) and CompareMem(S1.Memory, S2.Memory, S1.Size);
    finally
      S2.Free;
    end;
  finally
    S1.Free;
  end;
end;

{ Utility routines }
function LineStart(const Buffer: TBytes; BufPos: NativeInt): NativeInt;
begin
  while (BufPos > 0) and (Buffer[BufPos] <> 10) do
    Dec(BufPos);

  if Buffer[BufPos] = 10 then
    Inc(BufPos);

  Result := BufPos;
end;

{$IFNDEF NEXTGEN}
function LineStart(Buffer, BufPos: PAnsiChar): PAnsiChar;
{$IFNDEF X86ASM}
var
  C: NativeInt;
begin
  Result := Buffer;
  C := (BufPos - Buffer) - 1;
  while (C > 0) and (Buffer[C] <> #10) do
    Dec(C);

  if C > 0 then
    Result := @Buffer[C + 1];
end;
{$ELSE X86ASM}
asm // StackAlignSafe
        PUSH    EDI
        MOV     EDI,EDX
        MOV     ECX,EDX
        SUB     ECX,EAX
        SUB     ECX,1
        JBE     @@1
        MOV     EDX,EAX
        DEC     EDI
        MOV     AL,0AH
        STD
        REPNE   SCASB
        CLD
        MOV     EAX,EDX
        JNE     @@1
        LEA     EAX,[EDI+2]
@@1:    POP     EDI
end;
{$ENDIF X86ASM}
{$ENDIF !NEXTGEN}

function LineStart(Buffer, BufPos: PChar): PChar;
{$IFNDEF X86ASM}
var
  C: NativeInt;
begin
  Result := Buffer;
  C := (BufPos - Buffer) - 1;
  while (C > 0) and (Buffer[C] <> #10) do
    Dec(C);

  if C > 0 then
    Result := @Buffer[C + 1];
end;
{$ELSE X86ASM}
asm // StackAlignSafe
        PUSH    EDI
        MOV     EDI,EDX
        MOV     ECX,EDX
        SUB     ECX,EAX
        SHR     ECX,1
        SUB     ECX,2
        JBE     @@1
        MOV     EDX,EAX
        DEC     EDI
        DEC     EDI
        MOV     AX,0AH
        STD
        REPNE   SCASW
        CLD
        MOV     EAX,EDX
        JNE     @@1
        LEA     EAX,[EDI+4]  // TODO: Determine if this should be +2 or +4
@@1:    POP     EDI
end;
{$ENDIF X86ASM}

function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar;
  Strings: TStrings): Integer;
var
  Head, Tail: PChar;
  EOS, InQuote: Boolean;
  QuoteChar: Char;
  Item: string;
begin
  Result := 0;
  if (Content = nil) or (Content^ = #0) or (Strings = nil) then
    Exit;
  Tail := Content;
  InQuote := False;
  QuoteChar := #0;
  Strings.BeginUpdate;
  try
    Include(WhiteSpace, #13);
    Include(WhiteSpace, #10);

    Include(Separators, #0);
    Include(Separators, #13);
    Include(Separators, #10);
    Include(Separators, '''');
    Include(Separators, '"');
    repeat
      while (Tail^ in WhiteSpace) do
        Inc(Tail);
      Head := Tail;
      while True do
      begin
        while (InQuote and not ((Tail^ = #0) or (Tail^ = QuoteChar))) or
          not (Tail^ in Separators) do
            Inc(Tail);
        if (Tail^ in ['''', '"']) then
        begin
          if (QuoteChar <> #0) and (QuoteChar = Tail^) then
            QuoteChar := #0
          else if QuoteChar = #0 then
            QuoteChar := Tail^;
          InQuote := QuoteChar <> #0;
          Inc(Tail);
        end else Break;
      end;
      EOS := Tail^ = #0;
      if (Head <> Tail) and (Head^ <> #0) then
      begin
        if Strings <> nil then
        begin
          SetString(Item, Head, Tail - Head);
          Strings.Add(Item);
        end;
        Inc(Result);
      end;
      Inc(Tail);
    until EOS;
  finally
    Strings.EndUpdate;
  end;
end;

{ TListEnumerator }

constructor TListEnumerator.Create(AList: TList);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TListEnumerator.GetCurrent: Pointer;
begin
  Result := FList[FIndex];
end;

function TListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TList }

destructor TList.Destroy;
begin
  Clear;
end;

function TList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
  if (Item <> nil) and (ClassType <> TList) then
    Notify(Item, lnAdded);
end;

procedure TList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := FList[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(Pointer));
  if (Temp <> nil) and (ClassType <> TList) then
    Notify(Temp, lnDeleted);
end;

class procedure TList.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress;
end;

class procedure TList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;

procedure TList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

function TList.Expand: TList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TList.First: Pointer;
begin
  Result := Get(0);
end;

function TList.Get(Index: Integer): Pointer;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index];
end;

function TList.GetEnumerator: TListEnumerator;
begin
  Result := TListEnumerator.Create(Self);
end;

procedure TList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TList.IndexOf(Item: Pointer): Integer;
var
  P: PPointer;
begin
  P := Pointer(FList);
  for Result := 0 to FCount - 1 do
  begin
    if P^ = Item then
      Exit;
    Inc(P);
  end;
  Result := -1;
end;

function TList.IndexOfItem(Item: Pointer; Direction: TDirection): Integer;
var
  P: PPointer;
begin
  if Direction = FromBeginning then
    Result := IndexOf(Item)
  else
  begin
    if FCount > 0 then
    begin
      P := Pointer(@List[FCount - 1]);
      for Result := FCount - 1 downto 0 do
      begin
        if P^ = Item then
          Exit;
        Dec(P);
      end;
    end;
    Result := -1;
  end;
end;

procedure TList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList[Index] := Item;
  Inc(FCount);
  if (Item <> nil) and (ClassType <> TList) then
    Notify(Item, lnAdded);
end;

function TList.Last: Pointer;
begin
  if FCount > 0 then
    Result := FList[Count - 1]
  else
  begin
    Error(@SListIndexError, 0);
    Result := nil;
  end;
end;

procedure TList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList[NewIndex] := Item;
  end;
end;

procedure TList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  if Item <> FList[Index] then
  begin
    Temp := FList[Index];
    FList[Index] := Item;
    if ClassType <> TList then
    begin
      if Temp <> nil then
        Notify(Temp, lnDeleted);
      if Item <> nil then
        Notify(Item, lnAdded);
    end;
  end;
end;

function TList.Remove(Item: Pointer): Integer;
begin
  Result := RemoveItem(Item, TList.TDirection.FromBeginning);
end;

function TList.RemoveItem(Item: Pointer; Direction: TDirection): Integer;
begin
  Result := IndexOfItem(Item, Direction);
  if Result >= 0 then
    Delete(Result);
end;

procedure TList.Pack;
var
  PackedCount : Integer;
  StartIndex : Integer;
  EndIndex : Integer;
begin
  if FCount = 0 then
    Exit;

  PackedCount := 0;
  StartIndex := 0;
  repeat
    // Locate the first/next non-nil element in the list
    while (StartIndex < FCount) and (FList[StartIndex] = nil) do
      Inc(StartIndex);

    if StartIndex < FCount then // There is nothing more to do
    begin
      // Locate the next nil pointer
      EndIndex := StartIndex;
      while (EndIndex < FCount) and (FList[EndIndex] <> nil) do
        Inc(EndIndex);
      Dec(EndIndex);

      // Move this block of non-null items to the index recorded in PackedToCount:
      // If this is a contiguous non-nil block at the start of the list then
      // StartIndex and PackedToCount will be equal (and 0) so don't bother with the move.
      if StartIndex > PackedCount then
        System.Move(FList[StartIndex],
                    FList[PackedCount],
                    (EndIndex - StartIndex + 1) * SizeOf(Pointer));

      // Set the PackedToCount to reflect the number of items in the list
      // that have now been packed.
      Inc(PackedCount, EndIndex - StartIndex + 1);

      // Reset StartIndex to the element following EndIndex
      StartIndex := EndIndex + 1;
    end;
  until StartIndex >= FCount;

  // Set Count so that the 'free' item
  FCount := PackedCount;
end;

procedure TList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TList.SetCount(NewCount: Integer);
var
  I: Integer;
  Temp: Pointer;
begin
  if NewCount < 0 then
    Error(@SListCountError, NewCount);
  if NewCount <> FCount then
  begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
    else
    if ClassType <> TList then
    begin
      for I := FCount - 1 downto NewCount do
      begin
        Dec(FCount);
        Temp := List[I];
        if Temp <> nil then
          Notify(Temp, lnDeleted);
      end;
    end;
    FCount := NewCount;
  end;
end;

procedure QuickSort(SortList: TPointerList; L, R: Integer;
  SCompare: TListSortCompareFunc);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList[(L + R) shr 1];
    repeat
      while SCompare(SortList[I], P) < 0 do
        Inc(I);
      while SCompare(SortList[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := SortList[I];
          SortList[I] := SortList[J];
          SortList[J] := T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TList.Sort(Compare: TListSortCompare);
begin
  if Count > 1 then
    QuickSort(FList, 0, Count - 1,
      function(Item1, Item2: Pointer): Integer
      begin
        Result := Compare(Item1, Item2);
      end);
end;

procedure TList.SortList(const Compare: TListSortCompareFunc);
begin
  if Count > 1 then
    QuickSort(FList, 0, Count - 1, Compare);
end;

function TList.Extract(Item: Pointer): Pointer;
begin
  Result := ExtractItem(Item, TDirection.FromBeginning);
end;

function TList.ExtractItem(Item: Pointer; Direction: TDirection): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOfItem(Item, Direction);
  if I >= 0 then
  begin
    Result := Item;
    FList[I] := nil;
    Delete(I);
    if ClassType <> TList then
      Notify(Result, lnExtracted);
  end;
end;

procedure TList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

procedure TList.Assign(ListA: TList; AOperator: TListAssignOp; ListB: TList);
var
  I: Integer;
  LTemp, LSource: TList;
begin
  // ListB given?
  if ListB <> nil then
  begin
    LSource := ListB;
    Assign(ListA);
  end
  else
    LSource := ListA;

  // on with the show
  case AOperator of

    // 12345, 346 = 346 : only those in the new list
    laCopy:
      begin
        Clear;
        Capacity := LSource.Capacity;
        for I := 0 to LSource.Count - 1 do
          Add(LSource[I]);
      end;

    // 12345, 346 = 34 : intersection of the two lists
    laAnd:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) = -1 then
          Delete(I);

    // 12345, 346 = 123456 : union of the two lists
    laOr:
      for I := 0 to LSource.Count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);

    // 12345, 346 = 1256 : only those not in both lists
    laXor:
      begin
        LTemp := TList.Create; // Temp holder of 4 byte values
        try
          LTemp.Capacity := LSource.Count;
          for I := 0 to LSource.Count - 1 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          for I := Count - 1 downto 0 do
            if LSource.IndexOf(Items[I]) <> -1 then
              Delete(I);
          I := Count + LTemp.Count;
          if Capacity < I then
            Capacity := I;
          for I := 0 to LTemp.Count - 1 do
            Add(LTemp[I]);
        finally
          LTemp.Free;
        end;
      end;

    // 12345, 346 = 125 : only those unique to source
    laSrcUnique:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6 : only those unique to dest
    laDestUnique:
      begin
        LTemp := TList.Create;
        try
          LTemp.Capacity := LSource.Count;
          for I := LSource.Count - 1 downto 0 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          Assign(LTemp);
        finally
          LTemp.Free;
        end;
      end;
  end;
end;

{ TThreadList }

constructor TThreadList.Create;
begin
  inherited Create;
  FLock := TObject.Create;
  FList := TList.Create;
  FDuplicates := dupIgnore;
end;

destructor TThreadList.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

procedure TThreadList.Add(Item: Pointer);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or
       (FList.IndexOf(Item) = -1) then
      FList.Add(Item)
    else if Duplicates = dupError then
      FList.Error(@SDuplicateItem, IntPtr(Item));
  finally
    UnlockList;
  end;
end;

procedure TThreadList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function  TThreadList.LockList: TList;
begin
  TMonitor.Enter(FLock);
  Result := FList;
end;

procedure TThreadList.Remove(Item: Pointer);
begin
  RemoveItem(Item, TList.TDirection.FromBeginning);
end;

procedure TThreadList.RemoveItem(Item: Pointer; Direction: TList.TDirection);
begin
  LockList;
  try
    FList.RemoveItem(Item, Direction);
  finally
    UnlockList;
  end;
end;

procedure TThreadList.UnlockList;
begin
  TMonitor.Exit(FLock);
end;

{ TInterfaceListEnumerator }

constructor TInterfaceListEnumerator.Create(AInterfaceList: TInterfaceList);
begin
  inherited Create;
  FIndex := -1;
  FInterfaceList := AInterfaceList;
end;

function TInterfaceListEnumerator.GetCurrent: IInterface;
begin
  Result := FInterfaceList[FIndex];
end;

function TInterfaceListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FInterfaceList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TInterfaceList }

constructor TInterfaceList.Create;
begin
  inherited Create;
  FList := TThreadList<IInterface>.Create;
end;

destructor TInterfaceList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TInterfaceList.Clear;
var
  I: Integer;
  aList: TList<IInterface>;
begin
  if FList <> nil then
  begin
    aList := FList.LockList;
    try
      for I := 0 to aList.Count - 1 do
        aList.List[I] := nil;
      aList.Clear;
    finally
      Self.FList.UnlockList;
    end;
  end;
end;

procedure TInterfaceList.Delete(Index: Integer);
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    Self.Put(Index, nil);
    aList.Delete(Index);
  finally
    Self.FList.UnlockList;
  end;
end;

function TInterfaceList.Expand: TInterfaceList;
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    aList.Expand;
    Result := Self;
  finally
    Self.FList.Unlocklist;
  end;
end;

function TInterfaceList.First: IInterface;
begin
  Result := Get(0);
end;

function TInterfaceList.Get(Index: Integer): IInterface;
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    if (Index < 0) or (Index >= aList.Count) then
      aList.Error(SListIndexError, Index);
    Result := aList.List[Index];
  finally
    Self.FList.UnlockList;
  end;
end;

function TInterfaceList.GetCapacity: Integer;
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    Result := aList.Capacity;
  finally
    Self.FList.UnlockList;
  end;
end;

function TInterfaceList.GetCount: Integer;
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    Result := aList.Count;
  finally
    Self.FList.UnlockList;
  end;
end;

function TInterfaceList.GetEnumerator: TInterfaceListEnumerator;
begin
  Result := TInterfaceListEnumerator.Create(Self);
end;

function TInterfaceList.IndexOf(const Item: IInterface): Integer;
begin
  Result := IndexOfItem(Item, TList.TDirection.FromBeginning);
end;

function TInterfaceList.IndexOfItem(const Item: IInterface; Direction: TList.TDirection): Integer;
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    Result := aList.IndexOfItem(Item, Direction);
  finally
    Self.FList.UnlockList;
  end;
end;

function TInterfaceList.Add(const Item: IInterface): Integer;
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    Result := aList.Add(nil);
    aList.List[Result] := Item;
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TInterfaceList.Insert(Index: Integer; const Item: IInterface);
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    aList.Insert(Index, nil);
    aList.List[Index] := Item;
  finally
    Self.FList.UnlockList;
  end;
end;

function TInterfaceList.Last: IInterface;
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    Result := Self.Get(aList.Count - 1);
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TInterfaceList.Put(Index: Integer; const Item: IInterface);
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    if (Index < 0) or (Index >= aList.Count) then aList.Error(SListIndexError, Index);
    aList.List[Index] := Item;
  finally
    Self.FList.UnlockList;
  end;
end;

function TInterfaceList.Remove(const Item: IInterface): Integer;
begin
  Result := RemoveItem(Item, TList.TDirection.FromBeginning);
end;

function TInterfaceList.RemoveItem(const Item: IInterface; Direction: TList.TDirection): Integer;
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    Result := aList.IndexOfItem(Item, Direction);
    if Result > -1 then
    begin
      aList.List[Result] := nil;
      aList.Delete(Result);
    end;
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TInterfaceList.SetCapacity(NewCapacity: Integer);
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    aList.Capacity := NewCapacity;
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TInterfaceList.SetCount(NewCount: Integer);
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    aList.Count := NewCount;
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TInterfaceList.Exchange(Index1, Index2: Integer);
var
  aList: TList<IInterface>;
begin
  aList := FList.LockList;
  try
    aList.Exchange(Index1, Index2);
  finally
    Self.FList.UnlockList;
  end;
end;

procedure TInterfaceList.Lock;
begin
  FList.LockList;
end;

procedure TInterfaceList.Unlock;
begin
  FList.UnlockList;
end;

{ TBits }

const
  BitsPerInt = SizeOf(Integer) * 8;

type
  TBitEnum = 0..BitsPerInt - 1;
  TBitSet = set of TBitEnum;
  PBitArray = ^TBitArray;
  TBitArray = array[0..4096] of TBitSet;

destructor TBits.Destroy;
begin
  SetSize(0);
  inherited Destroy;
end;

procedure TBits.Error;
begin
  raise EBitsError.CreateRes(@SBitsIndexError);
end;

procedure TBits.SetSize(Value: Integer);
var
  NewMem: Pointer;
  NewMemSize: Integer;
  OldMemSize: Integer;

  function Min(X, Y: Integer): Integer;
  begin
    Result := X;
    if X > Y then Result := Y;
  end;

begin
  if Value <> Size then
  begin
    if Value < 0 then Error;
    NewMemSize := ((Value div BitsPerInt) + (((Value mod BitsPerInt) + BitsPerInt - 1) div bitsPerInt)) * SizeOf(Integer);
    OldMemSize := ((Size div BitsPerInt) + (((Size mod BitsPerInt) + BitsPerInt - 1) div bitsPerInt)) * SizeOf(Integer);

    if NewMemSize <> OldMemSize then
    begin
      NewMem := nil;
      if NewMemSize <> 0 then
      begin
        GetMem(NewMem, NewMemSize);
        FillChar(NewMem^, NewMemSize, 0);
      end;
      if OldMemSize <> 0 then
      begin
        if NewMem <> nil then
          Move(FBits^, NewMem^, Min(OldMemSize, NewMemSize));
        FreeMem(FBits, OldMemSize);
      end;
      FBits := NewMem;
    end;
    FSize := Value;
  end;
end;

procedure TBits.SetBit(Index: Integer; Value: Boolean);
{$IFNDEF X86ASM}
var
  LRelInt: PInteger;
  LMask: Integer;
begin
  if Index < 0 then
    Error;

  if Index >= FSize then
    SetSize(Index + 1);

  { Calculate the address of the related integer }
  LRelInt := FBits;
  Inc(LRelInt, Index div BitsPerInt);

  { Generate the mask }
  LMask := (1 shl (Index mod BitsPerInt));

  { Update the integer }
  if Value then
    LRelInt^ := LRelInt^ or LMask
  else
    LRelInt^ := LRelInt^ and not LMask;
end;
{$ELSE X86ASM}
asm
        CMP     Index,[EAX].FSize
        JAE     @@Size

@@1:    MOV     EAX,[EAX].FBits
        OR      Value,Value
        JZ      @@2
        BTS     [EAX],Index
        RET

@@2:    BTR     [EAX],Index
        RET

@@Size: CMP     Index,0
        JL      TBits.Error
        PUSH    Self
        PUSH    Index
        PUSH    ECX {Value}
        INC     Index
        CALL    TBits.SetSize
        POP     ECX {Value}
        POP     Index
        POP     Self
        JMP     @@1
end;
{$ENDIF X86ASM}

function TBits.GetBit(Index: Integer): Boolean;
{$IFNDEF X86ASM}
var
  LRelInt: PInteger;
  LMask: Integer;
begin
  if (Index >= FSize) or (Index < 0) then
    Error;

  { Calculate the address of the related integer }
  LRelInt := FBits;
  Inc(LRelInt, Index div BitsPerInt);

  { Generate the mask }
  LMask := (1 shl (Index mod BitsPerInt));
  Result := (LRelInt^ and LMask) <> 0;
end;
{$ELSE X86ASM}
asm
        CMP     Index,[EAX].FSize
        JAE     TBits.Error
        MOV     EAX,[EAX].FBits
        BT      [EAX],Index
        SBB     EAX,EAX
        AND     EAX,1
end;
{$ENDIF X86ASM}

function TBits.OpenBit: Integer;
var
  I: Integer;
  B: TBitSet;
  J: TBitEnum;
  E: Integer;
begin
  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
    if PBitArray(FBits)^[I] <> [0..BitsPerInt - 1] then
    begin
      B := PBitArray(FBits)^[I];
      for J := Low(J) to High(J) do
      begin
        if not (J in B) then
        begin
          Result := I * BitsPerInt + J;
          if Result >= Size then Result := Size;
          Exit;
        end;
      end;
    end;
  Result := Size;
end;

{ TPersistent }

destructor TPersistent.Destroy;
begin
  RemoveFixups(Self);
  inherited Destroy;
end;

procedure TPersistent.Assign(Source: TPersistent);
begin
  if Source <> nil then Source.AssignTo(Self) else AssignError(nil);
end;

procedure TPersistent.AssignError(Source: TPersistent);
var
  SourceName: string;
begin
  if Source <> nil then
    SourceName := Source.ClassName else
    SourceName := 'nil';
  raise EConvertError.CreateResFmt(@SAssignError, [SourceName, ClassName]);
end;

procedure TPersistent.AssignTo(Dest: TPersistent);
begin
  Dest.AssignError(Self);
end;

procedure TPersistent.DefineProperties(Filer: TFiler);
begin
end;

function TPersistent.GetNamePath: string;
var
  S: string;
begin
  Result := ClassName;
  if (GetOwner <> nil) then
  begin
    S := GetOwner.GetNamePath;
    if S <> '' then
      Result := S + '.' + Result;
  end;
end;

function TPersistent.GetOwner: TPersistent;
begin
  Result := nil;
end;

{ TInterfacedPersistent }

procedure TInterfacedPersistent.AfterConstruction;
begin
  inherited;
  if GetOwner <> nil then
    GetOwner.GetInterface(IInterface, FOwnerInterface);
end;

function TInterfacedPersistent._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef else
    Result := -1;
end;

function TInterfacedPersistent._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release else
    Result := -1;
end;

function TInterfacedPersistent.QueryInterface(const IID: TGUID;
  out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

// Out param is more code efficient for interfaces than function result
procedure GetDesigner(const Obj: TPersistent; out Result: IDesignerNotify);
var
  Temp: TPersistent;
begin
  Result := nil;
  if Obj = nil then Exit;
  Temp := Obj.GetOwner;
  if Temp = nil then
  begin
    if (Obj is TComponent) and (csDesigning in TComponent(Obj).ComponentState) then
      TComponent(Obj).QueryInterface(IDesignerNotify, Result);
  end else
  begin
    if (Obj is TComponent) and
      not (csDesigning in TComponent(Obj).ComponentState) then Exit;
    GetDesigner(Temp, Result);
  end;
end;

function FindRootDesigner(Obj: TPersistent): IDesignerNotify;
begin
  GetDesigner(Obj, Result);
end;

procedure GetComponentDesigner(const Component: TComponent; out Result: IDesignerNotify);
begin
  Result := nil;
  if (Component <> nil) and (csDesigning in Component.ComponentState) then
    Component.QueryInterface(IDesignerNotify, Result);
end;

function CountGenerations(Ancestor, Descendent: TClass): Integer;
var
  R: Integer;
begin
  R := 0;
  while Ancestor <> Descendent do
  begin
    if Descendent = nil then
    begin
      // Descendent wasn't a descendent of Ancestor.
      Result := -1;
      Exit;
    end;
    Descendent := Descendent.ClassParent;
    Inc(R);
  end;
  Result := R;
end;

procedure NotifyDesigner(Self, Item: TPersistent; Operation: TOperation);
var
  Designer: IDesignerNotify;
begin
  GetDesigner(Self, Designer);
  if Designer <> nil then
    Designer.Notification(Item, Operation);
end;

{ TRecall }

constructor TRecall.Create(AStorage, AReference: TPersistent);
begin
  inherited Create;
  FStorage := AStorage;
  FReference := AReference;
  Store;
end;

destructor TRecall.Destroy;
begin
  if Assigned(FReference) then
    FReference.Assign(FStorage);
  Forget;
  inherited;
end;

procedure TRecall.Forget;
begin
  FReference := nil;
  FreeAndNil(FStorage);
end;

procedure TRecall.Store;
begin
  if Assigned(FReference) then
    FStorage.Assign(FReference);
end;

{ TCollectionItem }

constructor TCollectionItem.Create(Collection: TCollection);
begin
  SetCollection(Collection);
end;

destructor TCollectionItem.Destroy;
begin

  if FCollection <> nil then
    Release;
  inherited Destroy;
end;

procedure TCollectionItem.Release;
begin
  SetCollection(nil);
end;

procedure TCollectionItem.Changed(AllItems: Boolean);
var
  Item: TCollectionItem;
begin
  if (FCollection <> nil) and (FCollection.FUpdateCount = 0) then
  begin
    if AllItems then Item := nil else Item := Self;
    FCollection.Update(Item);
  end;
end;

function TCollectionItem.GetIndex: Integer;
begin
  if FCollection <> nil then
    Result := FCollection.FItems.IndexOf(Self) else
    Result := -1;
end;

function TCollectionItem.GetDisplayName: string;
begin
  Result := ClassName;
end;

function TCollectionItem.GetNamePath: string;
begin
  if FCollection <> nil then
    Result := Format('%s[%d]',[FCollection.GetNamePath, Index])
  else
    Result := ClassName;
end;

function TCollectionItem.GetOwner: TPersistent;
begin
  Result := FCollection;
end;

procedure TCollectionItem.SetCollection(Value: TCollection);
begin
  if FCollection <> Value then
  begin
    if FCollection <> nil then FCollection.RemoveItem(Self);
    if Value <> nil then Value.InsertItem(Self);
  end;
end;

procedure TCollectionItem.SetDisplayName(const Value: string);
begin
  Changed(False);
end;

procedure TCollectionItem.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
  begin
    FCollection.FItems.Move(CurIndex, Value);
    Changed(True);
  end;
end;

{ TCollectionEnumerator }

constructor TCollectionEnumerator.Create(ACollection: TCollection);
begin
  inherited Create;
  FIndex := -1;
  FCollection := ACollection;
end;

function TCollectionEnumerator.GetCurrent: TCollectionItem;
begin
  Result := FCollection.Items[FIndex];
end;

function TCollectionEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TCollection }

constructor TCollection.Create(ItemClass: TCollectionItemClass);
begin
  FItemClass := ItemClass;
  FItems := TList<TCollectionItem>.Create;
  NotifyDesigner(Self, Self, opInsert);
end;

destructor TCollection.Destroy;
begin
  FUpdateCount := 1;
  if FItems <> nil then
    Clear;
  NotifyDesigner(Self, Self, opRemove);
  FItems.Free;
  inherited Destroy;
end;

function TCollection.Add: TCollectionItem;
begin
  Result := FItemClass.Create(Self);
  Added(Result);
end;

procedure TCollection.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TCollection then
  begin
    BeginUpdate;
    try
      // Replaces call to Clear to avoid BeginUpdate/try/finally/EndUpdate block
      while FItems.Count > 0 do
        TCollectionItem(FItems.List[FItems.Count - 1]).DisposeOf;

      for I := 0 to TCollection(Source).FItems.Count - 1 do
        Add.Assign(TCollection(Source).FItems[I]);
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TCollection.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCollection.Changed;
begin
  if FUpdateCount = 0 then Update(nil);
end;

procedure TCollection.Clear;
begin
  if FItems.Count > 0 then
  begin
    BeginUpdate;
    try
      while FItems.Count > 0 do
        TCollectionItem(FItems.List[FItems.Count - 1]).DisposeOf;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCollection.ClearAndResetID;
begin
  Clear;
  FNextID := 0;
end;

procedure TCollection.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

function TCollection.FindItemID(ID: Integer): TCollectionItem;
var
  I: Integer;
begin
  for I := 0 to FItems.Count-1 do
  begin
    Result := TCollectionItem(FItems[I]);
    if Result.ID = ID then Exit;
  end;
  Result := nil;
end;

function TCollection.GetAttrCount: Integer;
begin
  Result := 0;
end;

function TCollection.GetAttr(Index: Integer): string;
begin
  Result := '';
end;

function TCollection.GetEnumerator: TCollectionEnumerator;
begin
  Result := TCollectionEnumerator.Create(Self);
end;

function TCollection.GetCapacity: Integer;
begin
  Result := FItems.Capacity;
end;

function TCollection.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  Result := Items[ItemIndex].DisplayName;
end;

function TCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TCollection.GetItem(Index: Integer): TCollectionItem;
begin
  Result := FItems[Index];
end;

function TCollection.GetNamePath: string;
var
  S, P: string;
begin
  Result := ClassName;
  if GetOwner = nil then Exit;
  S := GetOwner.GetNamePath;
  if S = '' then Exit;
  P := PropName;
  if P = '' then Exit;
  Result := S + '.' + P;
end;

function TCollection.GetPropName: string;
var
  I: Integer;
  Props: PPropList;
  TypeData: PTypeData;
  Owner: TPersistent;
begin
  Result := FPropName;
  Owner := GetOwner;
  if (Result <> '') or (Owner = nil) or (Owner.ClassInfo = nil) then Exit;
  TypeData := GetTypeData(Owner.ClassInfo);
  if (TypeData = nil) or (TypeData^.PropCount = 0) then Exit;
  GetMem(Props, TypeData^.PropCount * sizeof(Pointer));
  try
    GetPropInfos(Owner.ClassInfo, Props);
    for I := 0 to TypeData^.PropCount-1 do
    begin
      begin
        if (Props^[I]^.PropType^^.Kind = tkClass) and
          (GetOrdProp(Owner, Props^[I]) = IntPtr(Self)) then
          FPropName := System.TypInfo.GetPropName(Props^[I]);
      end;
    end;
  finally
    Freemem(Props);
  end;
  Result := FPropName;
end;

function TCollection.Insert(Index: Integer): TCollectionItem;
begin
  Result := Add;
  Result.Index := Index;
end;

procedure TCollection.InsertItem(Item: TCollectionItem);
begin
  if not (Item is FItemClass) then TList.Error(@SInvalidProperty, 0);
  FItems.Add(Item);
  Item.FCollection := Self;
  Item.FID := FNextID;
  Inc(FNextID);
  SetItemName(Item);
  Notify(Item, cnAdded);
  Changed;
  NotifyDesigner(Self, Item, opInsert);
end;

procedure TCollection.RemoveItem(Item: TCollectionItem);
begin
  Notify(Item, cnExtracting);
  if Item = FItems.Last then
    FItems.Delete(FItems.Count - 1)
  else
    FItems.Remove(Item);
  Item.FCollection := nil;
  NotifyDesigner(Self, Item, opRemove);
  Changed;
end;

procedure TCollection.SetCapacity(Value: Integer);
begin
  if (Value <> FItems.Capacity) then
    FItems.Capacity := Value;
end;

procedure TCollection.SetItem(Index: Integer; Value: TCollectionItem);
begin
  TCollectionItem(FItems[Index]).Assign(Value);
end;

procedure TCollection.SetItemName(Item: TCollectionItem);
begin
end;

procedure TCollection.Update(Item: TCollectionItem);
begin
end;

procedure TCollection.Delete(Index: Integer);
begin
  Notify(TCollectionItem(FItems[Index]), cnDeleting);
  TCollectionItem(FItems[Index]).DisposeOf;
end;

function TCollection.Owner: TPersistent;
begin
  Result := GetOwner;
end;

procedure TCollection.Added(var Item: TCollectionItem);
begin
end;

procedure TCollection.Deleting(Item: TCollectionItem);
begin
end;

procedure TCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded: Added(Item);
    cnDeleting: Deleting(Item);
  end;
end;

{ TOwnedCollection }

constructor TOwnedCollection.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  FOwner := AOwner;
  inherited Create(ItemClass);
end;

function TOwnedCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ find the ultimate owner of a collection or item (or persistent for that matter) }

function GetUltimateOwner(const ACollectionItem: TCollectionItem): TPersistent;
begin
  Result := ACollectionItem.GetOwner;
  if Result <> nil then
    Result := GetUltimateOwner(TCollection(Result));
end;

function GetUltimateOwner(const ACollection: TCollection): TPersistent;
begin
  Result := ACollection.GetOwner;
  if Result <> nil then
    Result := GetUltimateOwner(Result);
end;

function GetUltimateOwner(const APersistent: TPersistent): TPersistent;
begin
  Result := APersistent.GetOwner;
end;

{ TStringsEnumerator }

constructor TStringsEnumerator.Create(AStrings: TStrings);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

function TStringsEnumerator.GetCurrent: string;
begin
  Result := FStrings[FIndex];
end;

function TStringsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TStrings }

constructor TStrings.Create;
begin
  inherited Create;
  FDefaultEncoding := TEncoding.Default;
  FLineBreak := sLineBreak;
  FDelimiter := ',';
  FQuoteChar := '"';
  FNameValueSeparator := '=';
  FOptions := [soWriteBOM, soTrailingLineBreak, soUseLocale];
end;

destructor TStrings.Destroy;
begin
  if (FEncoding <> nil) and not TEncoding.IsStandardEncoding(FEncoding) then
    FreeAndNil(FEncoding);
  if not TEncoding.IsStandardEncoding(FDefaultEncoding) then
    FreeAndNil(FDefaultEncoding);
  StringsAdapter := nil;
  inherited Destroy;
end;

function TStrings.Add(const S: string): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TStrings.AddPair(const Name, Value: string): TStrings;
begin
  Add(Name + NameValueSeparator + Value);
  Result := Self;
end;

function TStrings.AddPair(const Name, Value: string; AObject: TObject): TStrings;
begin
  AddObject(Name + NameValueSeparator + Value, AObject);
  Result := Self;
end;

function TStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TStrings.Append(const S: string);
begin
  Add(S);
end;

procedure TStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TStrings.AddStrings(const Strings: TArray<string>);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      Add(Strings[I]);
  finally
    EndUpdate;
  end;
end;

procedure TStrings.AddStrings(const Strings: TArray<string>; const Objects: TArray<TObject>);
var
  I: Integer;
begin
  if Length(Strings) <> Length(Objects) then
    raise EArgumentOutOfRangeException.CreateRes(@System.RTLConsts.sInvalidStringAndObjectArrays);
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      AddObject(Strings[I], Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TStrings.Assign(Source: TPersistent);
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      // Must use property setter for DefaultEncoding
      DefaultEncoding := TStrings(Source).FDefaultEncoding;
      // Must use internal property setter for Encoding
      SetEncoding(TStrings(Source).FEncoding);
      FLineBreak := TStrings(Source).FLineBreak;
      FDelimiter := TStrings(Source).FDelimiter;
      FQuoteChar := TStrings(Source).FQuoteChar;
      FNameValueSeparator := TStrings(Source).FNameValueSeparator;
      FOptions := TStrings(Source).FOptions;
      AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TStrings.SetStrings(Source: TStrings);
begin
  BeginUpdate;
  try
    Clear;
    AddStrings(Source);
  finally
    EndUpdate;
  end;
end;

procedure TStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TStrings then
        Result := not Equals(TStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TStrings.GetUpdating: Boolean;
begin
  Result := UpdateCount > 0;
end;

function TStrings.Equals(Strings: TStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

{$IFOPT O+}
  // Turn off optimizations to force creating a EBP stack frame and
  // place params on the stack.
  {$DEFINE OPTIMIZATIONSON}
  {$O-}
{$ENDIF O+}
procedure TStrings.Error(const Msg: string; Data: Integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;

procedure TStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  raise EStringListError.CreateFmt(LoadResString(Msg), [Data]) at
    PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(Pointer))^;
end;
{$IFDEF OPTIMIZATIONSON}
  {$UNDEF OPTIMIZATIONSON}
  {$O+}
{$ENDIF OPTIMIZATIONSON}

procedure TStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TStrings.ExtractName(const S: string): string;
begin
  Result := ExtractName(S, False);
end;

function TStrings.ExtractName(const S: string; AllNames: Boolean): string;
var
  P: Integer;
begin
  Result := S;
  P := AnsiPos(NameValueSeparator, Result);
  if P <> 0 then
    SetLength(Result, P - 1)
  else if not AllNames then
    SetLength(Result, 0);
end;

function TStrings.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

function TStrings.GetCommaText: string;
var
  LOldDelimiter: Char;
  LOldQuoteChar: Char;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

function TStrings.GetDelimitedText: string;
var
  S: string;
  P: PChar;
  I, Count: Integer;
  LDelimiters: set of Char;
  SB: TStringBuilder;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    if QuoteChar = #0 then
      Result := ''
    else
      Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    if QuoteChar <> #0 then
    begin
      LDelimiters := [Char(#0), Char(QuoteChar), Char(Delimiter)];
      if not StrictDelimiter then
        LDelimiters := LDelimiters + [Char(#1)..Char(' ')];
    end;
    SB := TStringBuilder.Create;
    try
      for I := 0 to Count - 1 do
      begin
        S := Get(I);
        if QuoteChar <> #0 then
        begin
          P := PChar(S);
          while not (P^ in LDelimiters) do
            P := NextChar(P);
          if (P^ <> #0) then S := AnsiQuotedStr(S, QuoteChar);
        end;
        SB.Append(S);
        SB.Append(Delimiter);
      end;
      if SB.Length > 0 then
        Result := SB.ToString(0, SB.Length - 1);
    finally
      SB.Free;
    end;
  end;
end;

function TStrings.GetEnumerator: TStringsEnumerator;
begin
  Result := TStringsEnumerator.Create(Self);
end;

function TStrings.GetName(Index: Integer): string;
begin
  Result := ExtractName(Get(Index), False);
end;

function TStrings.GetKeyName(Index: Integer): string;
begin
  Result := ExtractName(Get(Index), True);
end;

function TStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TStrings.GetText: PChar;
begin
  Result := StrNew(PChar(GetTextStr));
end;

function TStrings.GetTextStr: string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S, LB: string;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  if not TrailingLineBreak then
    Dec(Size, Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
    if TrailingLineBreak or (I < Count - 1) then
    begin
      L := Length(LB);
      if L <> 0 then
      begin
        System.Move(Pointer(LB)^, P^, L * SizeOf(Char));
        Inc(P, L);
      end;
    end;
  end;
end;

function TStrings.GetValue(const Name: string): string;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TStrings.IndexOf(const S: string): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TStrings.IndexOfName(const Name: string): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TStrings.InsertObject(Index: Integer; const S: string; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TStrings.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TStrings.LoadFromFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TStrings.LoadFromStream(Stream: TStream);
begin
  LoadFromStream(Stream, nil);
end;

procedure TStrings.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer, 0, Size);
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, FDefaultEncoding);
    SetEncoding(Encoding); // Keep Encoding in case the stream is saved
    SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
  finally
    EndUpdate;
  end;
end;

procedure TStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      PutObject(CurIndex, nil);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TStrings.Put(Index: Integer; const S: string);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do Add(Reader.ReadString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TStrings.SaveToFile(const FileName: string);
begin
  SaveToFile(FileName, FEncoding);
end;

procedure TStrings.SaveToFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TStrings.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, FEncoding);
end;

procedure TStrings.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := FDefaultEncoding;
  Buffer := Encoding.GetBytes(GetTextStr);
  if WriteBOM then
  begin
    Preamble := Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

procedure TStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

procedure TStrings.SetCommaText(const Value: string);
var
  LOldDelimiter: Char;
  LOldQuoteChar: Char;
begin
  LOldDelimiter := Delimiter;
  LOldQuoteChar := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    SetDelimitedText(Value);
  finally
    Delimiter := LOldDelimiter;
    QuoteChar := LOldQuoteChar;
  end;
end;

procedure TStrings.SetStringsAdapter(const Value: IStringsAdapter);
begin
  if FAdapter <> nil then
    FAdapter.ReleaseStrings;
  FAdapter := Value;
  if FAdapter <> nil then
    FAdapter.ReferenceStrings(Self);
end;

procedure TStrings.SetText(Text: PChar);
begin
  SetTextStr(Text);
end;

procedure TStrings.SetTextStr(const Value: string);
var
  P, Start, LB: PChar;
  S: string;
  LineBreakLen: Integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      if CompareStr(LineBreak, sLineBreak) = 0 then
      begin
        // This is a lot faster than using StrPos/AnsiStrPos when
        // LineBreak is the default (#13#10)
        while P^ <> #0 do
        begin
          Start := P;
          while not (P^ in [#0, #10, #13]) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P^ = #13 then Inc(P);
          if P^ = #10 then Inc(P);
        end;
      end
      else
      begin
        LineBreakLen := Length(LineBreak);
        while P^ <> #0 do
        begin
          Start := P;
          LB := AnsiStrPos(P, PChar(LineBreak));
          while (P^ <> #0) and (P <> LB) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P = LB then
            Inc(P, LineBreakLen);
        end;
      end;
  finally
    EndUpdate;
  end;
end;

procedure TStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TStrings.SetValue(const Name, Value: string);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(Get(I));
  Writer.WriteListEnd;
end;

procedure TStrings.SetDelimitedText(const Value: string);
var
  P, P1: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    if not StrictDelimiter then
      while (P^ in [#1..' ']) do
        P := NextChar(P);
    while P^ <> #0 do
    begin
      if (P^ = QuoteChar) and (QuoteChar <> #0) then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not StrictDelimiter and (P^ > ' ')) or
              (StrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
          P := NextChar(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not StrictDelimiter then
        while (P^ in [#1..' ']) do
          P := NextChar(P);

      if P^ = Delimiter then
      begin
        P1 := P;
        if NextChar(P1)^ = #0 then
          Add('');
        repeat
          P := NextChar(P);
        until not (not StrictDelimiter and (P^ in [#1..' ']));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TStrings.GetTrailingLineBreak: Boolean;
begin
  Result := soTrailingLineBreak in Options;
end;

function TStrings.GetStrictDelimiter: Boolean;
begin
  Result := soStrictDelimiter in Options;
end;

procedure TStrings.SetDefaultEncoding(const Value: TEncoding);
begin
  if not TEncoding.IsStandardEncoding(FDefaultEncoding) then
    FDefaultEncoding.Free;
  if TEncoding.IsStandardEncoding(Value) then
    FDefaultEncoding := Value
  else if Value <> nil then
    FDefaultEncoding := Value.Clone
  else
    FDefaultEncoding := TEncoding.Default;
end;

procedure TStrings.SetEncoding(const Value: TEncoding);
begin
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
  if TEncoding.IsStandardEncoding(Value) then
    FEncoding := Value
  else if Value <> nil then
    FEncoding := Value.Clone
  else
    FEncoding := TEncoding.Default;
end;

procedure TStrings.SetTrailingLineBreak(const Value: Boolean);
begin
  if Value then
    Include(FOptions, soTrailingLineBreak)
  else
    Exclude(FOptions, soTrailingLineBreak);
end;

procedure TStrings.SetStrictDelimiter(const Value: Boolean);
begin
  if Value then
    Include(FOptions, soStrictDelimiter)
  else
    Exclude(FOptions, soStrictDelimiter);
end;

function TStrings.CompareStrings(const S1, S2: string): Integer;
begin
  if UseLocale then
    Result := AnsiCompareText(S1, S2)
  else
    Result := CompareText(S1, S2);
end;

function TStrings.GetUseLocale: Boolean;
begin
  Result := soUseLocale in Options;
end;

procedure TStrings.SetUseLocale(const Value: Boolean);
begin
  if Value then
    Include(FOptions, soUseLocale)
  else
    Exclude(FOptions, soUseLocale);
end;

function TStrings.GetWriteBOM: Boolean;
begin
  Result := soWriteBOM in Options;
end;

procedure TStrings.SetWriteBOM(const Value: Boolean);
begin
  if Value then
    Include(FOptions, soWriteBOM)
  else
    Exclude(FOptions, soWriteBOM);
end;

function TStrings.GetValueFromIndex(Index: Integer): string;
var
  SepPos: Integer;
begin
  if Index >= 0 then
  begin
    Result := Get(Index);
    SepPos := AnsiPos(NameValueSeparator, Result);
    if (SepPos > 0) then
      System.Delete(Result, 1, SepPos)
    else
      Result := '';
  end
  else
    Result := '';
end;

procedure TStrings.SetValueFromIndex(Index: Integer; const Value: string);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

function TStrings.ToStringArray: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;

function TStrings.ToObjectArray: TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Objects[I];
end;

{ TStringList }

destructor TStringList.Destroy;
var
  I: Integer;
  Temp: TArray<TObject>;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      Temp[I].DisposeOf;
end;

function TStringList.Add(const S: string): Integer;
begin
  Result := AddObject(S, nil);
end;

function TStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TStringList.Assign(Source: TPersistent);
begin
 if Source is TStringList then
 begin
   FCaseSensitive := TStringList(Source).FCaseSensitive;
   FDuplicates := TStringList(Source).FDuplicates;
   FSorted := TStringList(Source).FSorted;
 end;
 inherited Assign(Source);
end;

procedure TStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TStringList.Clear;
var
  I: Integer;
  Temp: TArray<TObject>;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        Temp[I].Free;

    Changed;
  end;
end;

procedure TStringList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(TStringItem));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FString)^ := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    Obj.Free;
  Changed;
end;

procedure TStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := Pointer(Item1^.FObject);
  Pointer(Item1^.FObject) := Pointer(Item2^.FObject);
  Pointer(Item2^.FObject) := Temp;
end;

function TStringList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TStringList.Get(Index: Integer): string;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FString;
end;

function TStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index].FObject;
end;

procedure TStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TStringList.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TStringList.Insert(Index: Integer; const S: string);
begin
  InsertObject(Index, S, nil);
end;

procedure TStringList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TStringList.InsertItem(Index: Integer; const S: string; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  Pointer(FList[Index].FString) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FString := S;
  Inc(FCount);
  Changed;
end;

procedure TStringList.Put(Index: Integer; const S: string);
begin
  if Sorted then Error(@SSortedListError, 0);
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Changing;
  FList[Index].FString := S;
  Changed;
end;

procedure TStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

procedure TStringList.QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
          ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListCompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FString,
                                List.FList[Index2].FString);
end;

procedure TStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TStringList.CustomSort(Compare: TStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if UseLocale then
    if CaseSensitive then
      Result := AnsiCompareStr(S1, S2)
    else
      Result := AnsiCompareText(S1, S2)
  else
    if CaseSensitive then
      Result := CompareStr(S1, S2)
    else
      Result := CompareText(S1, S2);
end;

constructor TStringList.Create;
begin
  inherited Create;
end;

constructor TStringList.Create(OwnsObjects: Boolean);
begin
  Create;
  FOwnsObject := OwnsObjects;
end;

constructor TStringList.Create(QuoteChar, Delimiter: Char);
begin
  Create;
  FQuoteChar := QuoteChar;
  FDelimiter := Delimiter;
end;

constructor TStringList.Create(QuoteChar, Delimiter: Char;
  Options: TStringsOptions);
begin
  Create;
  FQuoteChar := QuoteChar;
  FDelimiter := Delimiter;
  FOptions := Options;
end;

constructor TStringList.Create(Duplicates: TDuplicates; Sorted,
  CaseSensitive: Boolean);
begin
  Create;
  FDuplicates := Duplicates;
  FSorted := Sorted;
  FCaseSensitive := CaseSensitive;
end;

procedure TStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

const
  Dummy32bitResHeader: array[0..31] of Byte = (
    $00, $00, $00, $00, $20, $00, $00, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);

{ TStream }

function TStream.GetPosition: Int64;
begin
  Result := Seek(0, soCurrent);
end;

procedure TStream.SetPosition(const Pos: Int64);
begin
  Seek(Pos, soBeginning);
end;

function TStream.GetSize: Int64;
var
  Pos: Int64;
begin
  Pos := Seek(0, soCurrent);
  Result := Seek(0, soEnd);
  Seek(Pos, soBeginning);
end;

procedure TStream.SetSize(NewSize: Longint);
begin
  // default = do nothing  (read-only streams, etc)
  // descendents should implement this method to call the Int64 sibling
end;

procedure TStream.SetSize64(const NewSize: Int64);
begin
  SetSize(NewSize);
end;

procedure TStream.SetSize(const NewSize: Int64);
begin
{ For compatibility with old stream implementations, this new 64 bit SetSize
  calls the old 32 bit SetSize.  Descendent classes that override this
  64 bit SetSize MUST NOT call inherited. Descendent classes that implement
  64 bit SetSize should reimplement their 32 bit SetSize to call their 64 bit
  version.}
  if (NewSize < Low(Integer)) or (NewSize > High(Integer)) then
    raise ERangeError.CreateRes(@SRangeError);
  SetSize(LongInt(NewSize));
end;

function TStream.Seek32(const Offset: Integer; Origin: TSeekOrigin): Int64;
begin
  Result := Seek(Int64(Offset), Origin);
end;

function TStream.Seek(Offset: Longint; Origin: Word): Longint;

  procedure RaiseException;
  begin
    raise EStreamError.CreateResFmt(@sSeekNotImplemented, [Classname]);
  end;

type
  TSeek64 = function (const Offset: Int64; Origin: TSeekOrigin): Int64 of object;
var
  Impl: TSeek64;
  Base: TSeek64;
  ClassTStream: TClass;
begin
{ Deflect 32 seek requests to the 64 bit seek, if 64 bit is implemented.
  No existing TStream classes should call this method, since it was originally
  abstract.  Descendent classes MUST implement at least one of either
  the 32 bit or the 64 bit version, and must not call the inherited
  default implementation. }
  Impl := Seek;
  ClassTStream := Self.ClassType;
  while (ClassTStream <> nil) and (ClassTStream <> TStream) do
    ClassTStream := ClassTStream.ClassParent;
  if ClassTStream = nil then RaiseException;
  Base := TStream(@ClassTStream).Seek;
  if TMethod(Impl).Code = TMethod(Base).Code then
    RaiseException;
  Result := Seek(Int64(Offset), TSeekOrigin(Origin));
end;

function TStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
{ Default implementation of 64 bit seek is to deflect to existing 32 bit seek.
  Descendents that override 64 bit seek must not call this default implementation. }
  if (Offset < Low(Integer)) or (Offset > High(Integer)) then
    raise ERangeError.CreateRes(@SRangeError);
  Result := Seek(LongInt(Offset), Ord(Origin));
end;

function TStream.Seek(const Offset: Int64; Origin: Word): Int64;
begin
  Result := Seek(Offset, TSeekOrigin(Origin));
end;

function TStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Read(Buffer[Offset], Count);
end;

function TStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Write(Buffer[Offset], Count);
end;

function TStream.Read64(Buffer: TBytes; Offset, Count: Int64): Int64;
const
  BUKETSIZE = $20000000; // 512M
begin
  Result := 0;
  while Count >= BUKETSIZE do
  begin
    Result := Result + Read(Buffer[Offset], BUKETSIZE);
    Inc(Offset, BUKETSIZE);
    Dec(Count, BUKETSIZE);
  end;
  if Count > 0 then
    Result := Result + Read(Buffer[Offset], Count);
end;

function TStream.Write64(const Buffer: TBytes; Offset, Count: Int64): Int64;
const
  BUKETSIZE = $20000000; // 512M
begin
  Result := 0;
  while Count >= BUKETSIZE do
  begin
    Result := Result + Write(Buffer[Offset], BUKETSIZE);
    Inc(Offset, BUKETSIZE);
    Dec(Count, BUKETSIZE);
  end;
  if Count > 0 then
    Result := Result + Write(Buffer[Offset], Count);
end;

function TStream.Read(var Buffer: TBytes; Count: LongInt): LongInt;
begin
  Result := Read(Buffer, 0, Count);
end;

function TStream.Write(const Buffer: TBytes; Count: LongInt): LongInt;
begin
  Result := Write(Buffer, 0, Count);
end;

function TStream.ReadData(Buffer: Pointer; Count: NativeInt): NativeInt;
begin
  Result := Read(Buffer^, Count);
end;

function TStream.ReadData(const Buffer: TBytes; Count: NativeInt): NativeInt;
begin
  Result := Read(Buffer, 0, Count);
end;

function TStream.ReadData(var Buffer: Boolean): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Boolean; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;

{$IFDEF NEXTGEN}
function TStream.ReadData(var Buffer: UTF8Char): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: UTF8Char; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;
{$ELSE !NEXTGEN}
function TStream.ReadData(var Buffer: AnsiChar): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: AnsiChar; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;
{$ENDIF NEXTGEN}

function TStream.ReadData(var Buffer: Char): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Char; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: Int8): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Int8; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: UInt8): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: UInt8; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: Int16): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Int16; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
    Result := Read(Buffer, BufSize) + Skip(Count - BufSize)
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: UInt16): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: UInt16; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
    Result := Read(Buffer, BufSize) + Skip(Count - BufSize)
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: Int32): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Int32; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: UInt32): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: UInt32; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: Int64): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Int64; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: UInt64): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: UInt64; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Read(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Read(Buffer, Count)
end;

function TStream.ReadData(var Buffer: Single): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Single; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count <> BufSize then
  begin
    Buffer := 0;
    Result := Skip(Count);
  end
  else
    Result := Read(Buffer, BufSize);
end;

function TStream.ReadData(var Buffer: Double): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Double; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count <> BufSize then
  begin
    Buffer := 0;
    Result := Skip(Count);
  end
  else
    Result := Read(Buffer, BufSize);
end;

function TStream.ReadData(var Buffer: Extended): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: Extended; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count <> BufSize then
  begin
    Buffer := 0;
    Result := Skip(Count);
  end
  else
    Result := Read(Buffer, BufSize);
end;

function TStream.ReadData(var Buffer: TExtended80Rec): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData(var Buffer: TExtended80Rec; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count <> BufSize then
  begin
    Buffer.BuildUp(False, 0, -$3FFF); // 0.0
    Result := Skip(Count);
  end
  else
    Result := Read(Buffer, BufSize);
end;

function TStream.ReadData<T>(var Buffer: T): NativeInt;
begin
  Result := Read(Buffer, SizeOf(Buffer));
end;

function TStream.ReadData<T>(var Buffer: T; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count <> BufSize then
  begin
    Buffer := Default(T);
    Result := Skip(Count);
  end
  else
    Result := Read(Buffer, BufSize);
end;


function TStream.WriteData(const Buffer: TBytes; Count: NativeInt): NativeInt;
begin
  Result := Write(Buffer, 0, Count);
end;

function TStream.WriteData(const Buffer: Pointer; Count: NativeInt): NativeInt;
begin
  Result := Write(Buffer^, Count);
end;

function TStream.WriteData(const Buffer: Boolean): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: Boolean; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

{$IFDEF NEXTGEN}
function TStream.WriteData(const Buffer: UTF8Char): NAtiveInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: UTF8Char; Count: NAtiveInt): NAtiveInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;
{$ELSE !NEXTGEN}
function TStream.WriteData(const Buffer: AnsiChar): NAtiveInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: AnsiChar; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;
{$ENDIF NEXTGEN}

function TStream.WriteData(const Buffer: Char): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: Char; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: Int8): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: Int8; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: UInt8): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: UInt8; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: Int16): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: Int16; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: UInt16): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: UInt16; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;


function TStream.WriteData(const Buffer: Int32): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: Int32; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: UInt32): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: UInt32; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: Int64): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;


function TStream.WriteData(const Buffer: Int64; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: UInt64): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: UInt64; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: Single): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: Single; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: Double): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: Double; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: Extended): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: Extended; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData(const Buffer: TExtended80Rec): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData(const Buffer: TExtended80Rec; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

function TStream.WriteData<T>(const Buffer: T): NativeInt;
begin
  Result := Write(Buffer, SizeOf(Buffer));
end;

function TStream.WriteData<T>(const Buffer: T; Count: NativeInt): NativeInt;
const
  BufSize = SizeOf(Buffer);
begin
  if Count > BufSize then
  begin
    Result := Write(Buffer, BufSize);
    Result := Result + Skip(Count - BufSize);
  end
  else
    Result := Write(Buffer, Count)
end;

procedure TStream.ReadBuffer(var Buffer: TBytes; Count: NativeInt);
begin
  ReadBuffer(Buffer, 0, Count);
end;

procedure TStream.ReadBuffer(var Buffer: TBytes; Offset, Count: NativeInt);
var
  LTotalCount,
  LReadCount: NativeInt;
begin
  { Perform a read directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := Read(Buffer, Offset, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    raise EReadError.CreateRes(@SReadError);

  while (LTotalCount < Count) do
  begin
    { Try to read a contiguous block of <Count> size }
    LReadCount := Read(Buffer, Offset + LTotalCount, (Count - LTotalCount));

    { Check if we read something and decrease the number of bytes left to read }
    if LReadCount <= 0 then
      raise EReadError.CreateRes(@SReadError)
    else
      Inc(LTotalCount, LReadCount);
  end
end;

procedure TStream.ReadBuffer(var Buffer; Count: NativeInt);
var
  LTotalCount,
  LReadCount: NativeInt;
begin
  { Perform a read directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := Read(Buffer, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    raise EReadError.CreateRes(@SReadError);

  while (LTotalCount < Count) do
  begin
    { Try to read a contiguous block of <Count> size }
    LReadCount := Read(PByte(PByte(@Buffer) + LTotalCount)^,
      (Count - LTotalCount));

    { Check if we read something and decrease the number of bytes left to read }
    if LReadCount <= 0 then
      raise EReadError.CreateRes(@SReadError)
    else
      Inc(LTotalCount, LReadCount);
  end
end;

procedure TStream.ReadBufferData(var Buffer: Boolean);
begin
  if ReadData(Buffer) <> SizeOf(Boolean) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Boolean; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

{$IFDEF NEXTGEN}
procedure TStream.ReadBufferData(var Buffer: UTF8Char);
begin
  if ReadData(Buffer) <> SizeOf(UTF8Char) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UTF8Char; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;
{$ELSE !NEXTGEN}
procedure TStream.ReadBufferData(var Buffer: AnsiChar);
begin
  if ReadData(Buffer) <> SizeOf(AnsiChar) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: AnsiChar; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;
{$ENDIF NEXTGEN}

procedure TStream.ReadBufferData(var Buffer: Char);
begin
  if ReadData(Buffer) <> SizeOf(Char) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Char; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int8);
begin
  if ReadData(Buffer) <> SizeOf(Int8) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int8; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt8);
begin
  if ReadData(Buffer) <> SizeOf(UInt8) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt8; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int16);
begin
  if ReadData(Buffer) <> SizeOf(Int16) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int16; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt16);
begin
  if ReadData(Buffer) <> SizeOf(UInt16) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt16; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int32);
begin
  if ReadData(Buffer) <> SizeOf(Int32) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int32; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt32);
begin
  if ReadData(Buffer) <> SizeOf(UInt32) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt32; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int64);
begin
  if ReadData(Buffer) <> SizeOf(Int64) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Int64; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt64);
begin
  if ReadData(Buffer) <> SizeOf(UInt64) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: UInt64; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Single);
begin
  if ReadData(Buffer) <> SizeOf(Single) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Single; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Double);
begin
  if ReadData(Buffer) <> SizeOf(Double) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Double; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Extended);
begin
  if ReadData(Buffer) <> SizeOf(Extended) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: Extended; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: TExtended80Rec);
begin
  if ReadData(Buffer) <> SizeOf(TExtended80Rec) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.ReadBufferData(var Buffer: TExtended80Rec; Count: NativeInt);
begin
  if (Count <> 0) and (ReadData(Buffer, Count) <> Count) then
    raise EReadError.Create(SReadError);
end;

procedure TStream.WriteBuffer(const Buffer; Count: NativeInt);
var
  LTotalCount,
  LWrittenCount: NativeInt;
begin
  { Perform a write directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := Write(Buffer, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    raise EWriteError.CreateRes(@SWriteError);

  while (LTotalCount < Count) do
  begin
    { Try to write a contiguous block of <Count> size }
    LWrittenCount := Write(PByte(PByte(@Buffer) + LTotalCount)^,
      (Count - LTotalCount));

    { Check if we written something and decrease the number of bytes left to write }
    if LWrittenCount <= 0 then
      raise EWriteError.CreateRes(@SWriteError)
    else
      Inc(LTotalCount, LWrittenCount);
  end
end;

procedure TStream.WriteBuffer(const Buffer: TBytes; Count: NativeInt);
begin
  WriteBuffer(Buffer, 0, Count);
end;

procedure TStream.WriteBuffer(const Buffer: TBytes; Offset, Count: NativeInt);
var
  LTotalCount,
  LWrittenCount: NativeInt;
begin
  { Perform a write directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := Write(Buffer, Offset, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    raise EWriteError.CreateRes(@SWriteError);

  while (LTotalCount < Count) do
  begin
    { Try to write a contiguous block of <Count> size }
    LWrittenCount := Write(Buffer, Offset + LTotalCount, (Count - LTotalCount));

    { Check if we written something and decrease the number of bytes left to write}
    if LWrittenCount <= 0 then
      raise EWriteError.CreateRes(@SWriteError)
    else
      Inc(LTotalCount, LWrittenCount);
  end;
end;

procedure TStream.WriteBufferData(var Buffer: Integer; Count: NativeInt);
var
  C: NativeInt;
begin
  C := Count;
  if C > 4 then
    C := 4;
  Write(Buffer, C);
  if C < Count then
    Skip(Count - C);
end;

function TStream.CopyFrom(const Source: TStream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: TBytes;
begin
  if Count <= 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  SetLength(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer, N);
      WriteBuffer(Buffer, N);
      Dec(Count, N);
    end;
  finally
    SetLength(Buffer, 0);
  end;
end;

function TStream.ReadComponent(const Instance: TComponent): TComponent;
var
  Reader: TReader;
begin
  Reader := TReader.Create(Self, 4096);
  try
    Result := Reader.ReadRootComponent(Instance);
  finally
    Reader.Free;
  end;
end;

procedure TStream.WriteComponent(const Instance: TComponent);
begin
  WriteDescendent(Instance, nil);
end;

procedure TStream.WriteDescendent(const Instance, Ancestor: TComponent);
var
  Writer: TWriter;
begin
  Writer := TWriter.Create(Self, 4096);
  try
    Writer.WriteDescendent(Instance, Ancestor);
  finally
    Writer.Free;
  end;
end;

function TStream.ReadComponentRes(const Instance: TComponent): TComponent;
begin
  ReadResHeader;
  Result := ReadComponent(Instance);
end;

procedure TStream.WriteComponentRes(const ResName: string; const Instance: TComponent);
begin
  WriteDescendentRes(ResName, Instance, nil);
end;

procedure TStream.WriteResourceHeader(const ResName: string; out FixupInfo: Integer);
var
  L, HeaderSize: Integer;
  NameBytes: TBytes;
  Header: TBytes; // array[0..255] of Byte;
begin
  NameBytes := TEncoding.UTF8.GetBytes(UpperCase(ResName));
  SetLength(Header, 255);
  if Length(NameBytes) > Length(ResName) then
  begin
    NameBytes := TEncoding.Unicode.GetBytes(UpperCase(ResName));
    L := Length(NameBytes);
    if L div 2 > 63 then
      L := 63 * 2;
    SetLength(NameBytes, L + 2);
    PWord(@NameBytes[L])^ := 0;
    WriteBuffer(Dummy32bitResHeader, Length(Dummy32bitResHeader));

    FixupInfo := -(Position + 4);
    PInteger(@Header[0])^ := 0;
    PInteger(@Header[4])^ := 12 + L + 2 + 16;
    PInteger(@Header[8])^ := $000AFFFF;
    L := 12 + Length(NameBytes);
    Move(NameBytes[0], Header[12], Length(NameBytes));
    PInteger(@Header[L])^      := 0; // Data Version
    PWord(@Header[L + 4])^     := 0; // MemoryFlags
    PWord(@Header[L + 6])^     := $0409; // LangID - for now just use US as the language ID
    PInteger(@Header[L + 8])^  := 0; // Version
    PInteger(@Header[L + 12])^ := 0; // Characteristics
    WriteBuffer(Header, L + 16);
  end else
  begin
    Header[0] := $FF;
    Word((@Header[1])^) := 10;
    L := Length(NameBytes);
    if L > 63 then
      L := 64;
    SetLength(NameBytes, L + 1);
    NameBytes[L] := 0;
    Move(NameBytes[0], Header[3], Length(NameBytes));
    HeaderSize := Length(NameBytes) + 9;
    Word((@Header[HeaderSize - 6])^) := $1030;
    Integer((@Header[HeaderSize - 4])^) := 0;
    WriteBuffer(Header, HeaderSize);
    FixupInfo := Position;
  end;
end;

procedure TStream.FixupResourceHeader(FixupInfo: Integer);
// TODO -cELBRUS_LONGINT64 : Verify  TStream.FixupResourceHeader (Integer/LongInt to UInt32)
var
  ImageSize, HeaderSize: Int32;
begin
  if FixupInfo < 0 then
  begin
    ImageSize := Position - (-FixupInfo);
    Position := -FixupInfo;
    ReadBuffer(HeaderSize, SizeOf(HeaderSize));
    ImageSize := ImageSize - HeaderSize + 4;
    Position := -FixupInfo - 4;
    WriteBuffer(ImageSize, SizeOf(ImageSize));
    Position := -FixupInfo + ImageSize + HeaderSize - 4;
  end else
  begin
    ImageSize := Position - FixupInfo;
    Position := FixupInfo - 4;
    WriteBuffer(ImageSize, SizeOf(ImageSize));
    Position := FixupInfo + ImageSize;
  end;
end;

procedure TStream.WriteDescendentRes(const ResName: string; const Instance,
  Ancestor: TComponent);
var
  FixupInfo: Integer;
begin
  WriteResourceHeader(ResName, FixupInfo);
  WriteDescendent(Instance, Ancestor);
  FixupResourceHeader(FixupInfo);
end;

type
  TUInt32Helper = record helper for UInt32
  protected
    function GetBytes(Index: Cardinal): UInt8;
    function GetWords(Index: Cardinal): UInt16;
    procedure SetBytes(Index: Cardinal; const Value: UInt8);
    procedure SetWords(Index: Cardinal; const Value: UInt16);

  public
    function ToBytes: TBytes;
    function FromBytes(T: TBytes; Offset: Integer = 0): UInt32;

    property Bytes[Index: Cardinal]: UInt8 read GetBytes write SetBytes;
    property Words[Index: Cardinal]: UInt16 read GetWords write SetWords;
  end;

function TUInt32Helper.GetBytes(Index: Cardinal): UInt8;
begin
  case Index of
    0: Result :=  Self         and $FF;
    1: Result := (Self shr  8) and $FF;
    2: Result := (Self shr 16) and $FF;
    3: Result := (Self shr 24) and $FF;
  else
    raise ERangeError.CreateRes(@SRangeError);
  end;
end;

function TUInt32Helper.GetWords(Index: Cardinal): UInt16;
begin
  case Index of
    0: Result :=  Self         and $FFFF;
    1: Result := (Self shr 16) and $FFFF;
  else
    raise ERangeError.CreateRes(@SRangeError);
  end;
end;

procedure TUInt32Helper.SetBytes(Index: Cardinal; const Value: UInt8);
begin
  case Index of
    0: Self := (Self and $FFFFFF00) or  Value;
    1: Self := (Self and $FFFF00FF) or (Value shl  8);
    2: Self := (Self and $FF00FFFF) or (Value shl 16);
    3: Self := (Self and $00FFFFFF) or (Value shl 24);
  else
    raise ERangeError.CreateRes(@SRangeError);
  end;
end;

procedure TUInt32Helper.SetWords(Index: Cardinal; const Value: UInt16);
begin
  case Index of
    0: Self := (Self and $FFFF0000) or  Value;
    1: Self := (Self and $0000FFFF) or (Value shl 16);
  else
    raise ERangeError.CreateRes(@SRangeError);
  end;
end;


function TUInt32Helper.ToBytes: TBytes;
begin
  SetLength(Result, 4);
  Result[0] :=  Self         and $FF;
  Result[1] := (Self shr  8) and $FF;
  Result[2] := (Self shr 16) and $FF;
  Result[3] := (Self shr 24) and $FF;
end;

function TUInt32Helper.FromBytes(T: TBytes; Offset: Integer): UInt32;
begin
  Self := 0;
  if Length(T) > Offset     then Self := Self or  T[Offset];
  if Length(T) > Offset + 1 then Self := Self or (T[Offset + 1] shl  8);
  if Length(T) > Offset + 2 then Self := Self or (T[Offset + 2] shl 16);
  if Length(T) > Offset + 3 then Self := Self or (T[Offset + 3] shl 24);
  Result := Self;
end;

procedure TStream.ReadResHeader;
var
  C, ReadCount: Cardinal;
  Header: TBytes;
begin
  SetLength(Header, 256);
  FillChar(Header[0], Length(Header), 0);
  ReadCount := Read(Header, 0, Length(Header) - 1);
  if (Integer(ReadCount) > Length(Dummy32bitResHeader)) and
    CompareMem(@Dummy32bitResHeader, @Header[0], Length(Dummy32bitResHeader)) then
  begin
    Seek(Length(Dummy32bitResHeader), soBeginning);
    ReadCount := Read(Header, 0, Length(Header) - 1);
    C := 0;
    if C.FromBytes(Header, 8) = $000AFFFF then
    begin
      C.FromBytes(Header, 4);
      Seek(Int64(C) - ReadCount, soCurrent);
    end
    else
      raise EInvalidImage.CreateRes(@SInvalidImage);
  end
  else if (Header[0] = $FF) and (Header[1] = 10) and (Header[2] = 0) then
  begin
    C := 3;
    while Header[C] <> 0 do Inc(C);
    Seek(Int64(C) - 3 + 10 - ReadCount, soCurrent)
  end
  else
    raise EInvalidImage.CreateRes(@SInvalidImage);
end;

function TStream.Skip(Amount: Int64): Int64;
var
  P: Int64;
begin
  P := Position;
  Result := Seek(Amount, soCurrent) - P;
end;

{ THandleStream }

constructor THandleStream.Create(AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

function THandleStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Offset, Count);
  if Result = -1 then Result := 0;
end;

//{$IFNDEF NEXTGEN}
function THandleStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function THandleStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;
//{$ENDIF !NEXTGEN}

function THandleStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := FileWrite(FHandle, Buffer, Offset, Count);
  if Result = -1 then Result := 0;
end;

function THandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FileSeek(FHandle, Offset, Ord(Origin));
end;

procedure THandleStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure THandleStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
{$IF Defined(MSWINDOWS)}
  Win32Check(SetEndOfFile(FHandle));
{$ELSEIF Defined(POSIX)}
  if ftruncate(FHandle, Position) = -1 then
    raise EStreamError(sStreamSetSize);
{$ENDIF POSIX}
end;

{ TFileStream }

constructor TFileStream.Create(const AFileName: string; Mode: Word);
begin
{$IF Defined(MSWINDOWS)}
  Create(AFilename, Mode, 0);
{$ELSEIF Defined(POSIX)}
  Create(AFilename, Mode, FileAccessRights);
{$ENDIF POSIX}
end;

constructor TFileStream.Create(const AFileName: string; Mode: Word; Rights: Cardinal);
var
  LShareMode: Word;
begin
  if (Mode and fmCreate = fmCreate) then
  begin
    LShareMode := Mode and $FF;
    if LShareMode = $FF then
      LShareMode := fmShareExclusive; // For compat in case $FFFF passed as Mode
    inherited Create(FileCreate(AFileName, LShareMode, Rights));
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFCreateError.CreateResFmt(@SFCreateErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
{$IFDEF MSWINDOWS}
    inherited Create(FileOpen(AFileName, Mode));
{$ELSE !MSWINDOWS}
// TODO -oUnAssigned : Review file Rights for MAC when opening existing files.
    inherited Create(FileOpen(AFileName, Mode));
{$ENDIF MSWINDOWS}
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(AFileName), SysErrorMessage(GetLastError)]);
  end;
  FFileName := AFileName;
end;

destructor TFileStream.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    FileClose(FHandle);
  inherited Destroy;
end;

{ TBufferedFileStream }

constructor TBufferedFileStream.Create(const AFileName: string; Mode: Word;
  BufferSize: Integer);
begin
{$IF Defined(MSWINDOWS)}
  Create(AFilename, Mode, 0, BufferSize);
{$ELSEIF Defined(POSIX)}
  Create(AFilename, Mode, FileAccessRights, BufferSize);
{$ENDIF POSIX}
end;

constructor TBufferedFileStream.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal; BufferSize: Integer);
begin
  inherited Create(AFileName, Mode, Rights);
  FBufferSize := BufferSize;
  GetMem(FBuffer, FBufferSize);
  FBuffered := True;
  SyncBuffer(True);
end;

destructor TBufferedFileStream.Destroy;
begin
  SyncBuffer(False);
  FreeMem(FBuffer, FBufferSize);
  inherited Destroy;
end;

procedure TBufferedFileStream.SyncBuffer(ReRead: boolean);
begin
  if FModified then
  begin
    inherited Seek(FBufStartPos, soBeginning);
    inherited Write(FBuffer^, NativeInt(FBufEndPos - FBufStartPos));
    FModified := False;
  end;
  if ReRead then
  begin
    FBufStartPos := inherited Seek(FFilePos, soBeginning);
    FBufEndPos := FBufStartPos + inherited Read(FBuffer^, FBufferSize);
  end
  else
  begin
    inherited Seek(FFilePos, soBeginning);
    FBufEndPos := FBufStartPos;
  end;
end;

procedure TBufferedFileStream.FlushBuffer;
begin
  SyncBuffer(False);
end;

function TBufferedFileStream.Read(var Buffer; Count: Longint): Longint;
var
  PSrc: PByte;
begin
  if Count >= FBufferSize then
  begin
    SyncBuffer(False);
    Result := inherited Read(Buffer, Count)
  end
  else
  begin
    if (FBufStartPos > FFilePos) or (FFilePos + Count > FBufEndPos) then
      SyncBuffer(True);
    if Count < FBufEndPos - FFilePos then
      Result := Count
    else
      Result := FBufEndPos - FFilePos;
    PSrc := FBuffer + (FFilePos - FBufStartPos);
{$IF DEFINED(CPUARM32)}
    Move(PSrc^, Buffer, Result);
{$ELSE}
    case Result of
      SizeOf(Byte):
        PByte(@Buffer)^ := PByte(PSrc)^;
      SizeOf(Word):
        PWord(@Buffer)^ := PWord(PSrc)^;
      SizeOf(Cardinal):
        PCardinal(@Buffer)^ := PCardinal(PSrc)^;
      SizeOf(UInt64):
        PUInt64(@Buffer)^ := PUInt64(PSrc)^;
    else
      Move(PSrc^, Buffer, Result);
    end;
{$ENDIF}
  end;
  FFilePos := FFilePos + Result;
end;

function TBufferedFileStream.Write(const Buffer; Count: Longint): Longint;
var
  PDest: PByte;
begin
  if Count >= FBufferSize then
  begin
    SyncBuffer(False);
    Result := inherited Write(Buffer, Count);
    FFilePos := FFilePos + Result;
  end
  else
  begin
    if (FBufStartPos > FFilePos) or (FFilePos + Count > FBufStartPos + FBufferSize) then
      SyncBuffer(True);
    Result := Count;
    PDest := FBuffer + (FFilePos - FBufStartPos);
{$IF DEFINED(CPUARM32)}
    Move(Buffer, PDest^, Result);
{$ELSE}
    case Result of
      SizeOf(Byte):
        PByte(PDest)^ := PByte(@Buffer)^;
      SizeOf(Word):
        PWord(PDest)^ := PWord(@Buffer)^;
      SizeOf(Cardinal):
        PCardinal(PDest)^ := PCardinal(@Buffer)^;
      SizeOf(UInt64):
        PUInt64(PDest)^ := PUInt64(@Buffer)^;
    else
      Move(Buffer, PDest^, Result);
    end;
{$ENDIF}
    FModified := True;
    FFilePos := FFilePos + Result;
    if FFilePos > FBufEndPos then
      FBufEndPos := FFilePos;
  end;
end;

function TBufferedFileStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Read(Buffer[Offset], Count);
end;

function TBufferedFileStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Write(Buffer[Offset], Count);
end;

function TBufferedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if not FBuffered then
    FFilePos := inherited Seek(Offset, Origin)
  else
    case Origin of
      soBeginning:
        begin
          if (Offset < FBufStartPos) or (Offset > FBufEndPos) then
            SyncBuffer(False);
          FFilePos := Offset;
        end;
      soCurrent:
        begin
          if (FFilePos + Offset < FBufStartPos) or (FFilePos + Offset > FBufEndPos) then
            SyncBuffer(False);
          FFilePos := FFilePos + Offset;
        end;
      soEnd:
        begin
          SyncBuffer(False);
          FFilePos := inherited Seek(Offset, soEnd);
        end;
    end;
  Result := FFilePos;
end;

procedure TBufferedFileStream.SetSize(const NewSize: Int64);
begin
  if NewSize < FBufEndPos then
    SyncBuffer(False);
  FBuffered := False;
  try
    inherited SetSize(NewSize);
  finally
    FBuffered := True;
  end;
end;

{ TCustomMemoryStream }

procedure TCustomMemoryStream.SetPointer(Ptr: Pointer; const Size: NativeInt);
begin
  FMemory := Ptr;
  FSize := Size;
end;

function TCustomMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    if FSize - FPosition> 0 then
    begin
      if FSize > Count + FPosition then Result := Count
      else Result := FSize - FPosition;
      Move((PByte(FMemory) + FPosition)^, Buffer, Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TCustomMemoryStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    if FSize > FPosition then
    begin
      if FSize > Count + FPosition then Result := Count
      else Result := FSize - FPosition;
      //TODO -oAntonioT -cNEXTGEN: Check Buffer Overrun (Offset+count>Length)
      Move((PByte(FMemory) + FPosition)^, Buffer[Offset], Result);
      Inc(FPosition, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TCustomMemoryStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TCustomMemoryStream.SaveToStream(Stream: TStream);
begin
  if FSize <> 0 then Stream.WriteBuffer(FMemory^, FSize);
end;

procedure TCustomMemoryStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TMemoryStream }

const
  MemoryDelta = $2000; { Must be a power of 2 }

destructor TMemoryStream.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMemoryStream.Clear;
begin
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TMemoryStream.LoadFromStream(Stream: TStream);
var
  Count: Int64;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if Count <> 0 then Stream.ReadBuffer(FMemory^, Count);
end;

procedure TMemoryStream.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMemoryStream.SetCapacity(NewCapacity: NativeInt);
{$IF SizeOf(LongInt) = SizeOf(NativeInt)}
begin
  SetPointer(Realloc(LongInt(NewCapacity)), FSize);
  FCapacity := NewCapacity;
end;
{$ELSEIF SizeOf(LongInt) < SizeOf(NativeInt)} // LP64 / Win64 platform
var
  NewCapacityLongInt: LongInt;
begin
  NewCapacityLongInt := NewCapacity;
  SetPointer(Realloc(NewCapacityLongInt), FSize);
  NewCapacity := NewCapacityLongInt;
  FCapacity := NewCapacity;
end;
{$ENDIF}

procedure TMemoryStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

procedure TMemoryStream.SetSize(const NewSize: Int64);
var
  OldPosition: NativeInt;
begin
  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then Seek(0, soEnd);
end;

function TMemoryStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  if (NewCapacity > 0) and (NewCapacity <> FSize) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Memory;
  if NewCapacity <> FCapacity then
  begin
    if NewCapacity = 0 then
    begin
      FreeMem(Memory);
      Result := nil;
    end else
    begin
      if Capacity = 0 then
        GetMem(Result, NewCapacity)
      else
        ReallocMem(Result, NewCapacity);
      if Result = nil then raise EStreamError.CreateRes(@SMemoryStreamError);
    end;
  end;
end;

function TMemoryStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Int64;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
      begin
        if Pos > FCapacity then
          SetCapacity(Pos);
        FSize := Pos;
      end;
      System.Move(Buffer, (PByte(FMemory) + FPosition)^, Count);
      FPosition := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

function TMemoryStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
var
  Pos: Int64;
begin
  if (FPosition >= 0) and (Count >= 0) then
  begin
    Pos := FPosition + Count;
    if Pos > 0 then
    begin
      if Pos > FSize then
      begin
        if Pos > FCapacity then
          SetCapacity(Pos);
        FSize := Pos;
      end;
      System.Move(Buffer[Offset], (PByte(FMemory) + FPosition)^, Count);
      FPosition := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

{ TBytesStream }

constructor TBytesStream.Create(const ABytes: TBytes);
begin
  inherited Create;
  FBytes := ABytes;
  SetPointer(Pointer(FBytes), Length(FBytes));
  FMemory := FBytes;
  FCapacity := FSize;
end;

function TBytesStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  if (NewCapacity > 0) and (NewCapacity <> FSize) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Pointer(FBytes);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FBytes, NewCapacity);
    Result := Pointer(FBytes);
    if NewCapacity = 0 then
      Exit;
    if Result = nil then raise EStreamError.CreateRes(@SMemoryStreamError);
  end;
end;

{ TStringStream }

constructor TStringStream.Create(const AString: string);
begin
  Create(AString, TEncoding.Default, False);
end;

constructor TStringStream.Create(const AString: string; AEncoding: TEncoding;
  AOwnsEncoding: Boolean);
begin
  FEncoding := AEncoding;
  FOwnsEncoding := AOwnsEncoding and not TEncoding.IsStandardEncoding(AEncoding);
  inherited Create(FEncoding.GetBytes(AString));
end;

constructor TStringStream.Create(const AString: string; ACodePage: Integer);
begin
  Create(AString, TEncoding.GetEncoding(ACodePage));
end;

constructor TStringStream.Create(const ABytes: TBytes);
begin
  inherited;

  FEncoding := nil;
  TEncoding.GetBufferEncoding(ABytes, FEncoding);
end;

constructor TStringStream.Create;
begin
  Create('', TEncoding.Default, False);
end;

{$IFNDEF NEXTGEN}
constructor TStringStream.Create(const AString: RawByteString);
var
  LCodePage: Cardinal;
begin
  LCodePage := StringCodePage(AString);
  if (LCodePage = CP_ACP) or (LCodePage = TEncoding.Default.CodePage) then
    FEncoding := TEncoding.Default
  else
  begin
    FEncoding := TEncoding.GetEncoding(LCodePage);
    FOwnsEncoding := True;
  end;
  inherited Create(BytesOf(AString));
end;
{$ENDIF !NEXTGEN}

destructor TStringStream.Destroy;
begin
  if FOwnsEncoding then
    FEncoding.Free;
  inherited;
end;

function TStringStream.GetDataString: string;
begin
  Result := FEncoding.GetString(Bytes, 0, Size);
end;

function TStringStream.ReadString(Count: Integer): string;
begin
  if Count > Size - Position then
    Count := Size - Position;
  Result := FEncoding.GetString(Bytes, Position, Count);
  Position := Position + Count;
end;

procedure TStringStream.WriteString(const AString: string);
var
  LBytes: TBytes;
begin
  LBytes := FEncoding.GetBytes(AString);
  Write(LBytes, 0, Length(LBytes));
end;

{ TResourceStream }

constructor TResourceStream.Create(Instance: THandle; const ResName: string;
  ResType: PChar);
begin
  inherited Create;
  Initialize(Instance, PChar(ResName), ResType, False);
end;

constructor TResourceStream.CreateFromID(Instance: THandle; ResID: Integer;
  ResType: PChar);
begin
  inherited Create;
  Initialize(Instance, PChar(ResID), ResType, True);
end;

procedure TResourceStream.Initialize(Instance: THandle; Name, ResType: PChar;
  FromID: Boolean);

  procedure Error;
  var
    S: string;
  begin
    if FromID then
      S := IntToStr(IntPtr(Name))
    else
      S := Name;
    raise EResNotFound.CreateFmt(SResNotFound, [S]);
  end;

begin
  HResInfo := FindResource(Instance, Name, ResType);
  if HResInfo = 0 then Error;
  HGlobal := LoadResource(Instance, HResInfo);
  if HGlobal = 0 then Error;
  SetPointer(LockResource(HGlobal), SizeOfResource(Instance, HResInfo));
end;

destructor TResourceStream.Destroy;
begin
  UnlockResource(HGlobal);
  FreeResource(HGlobal);
  inherited Destroy;
end;

function TResourceStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamError.CreateRes(@SCantWriteResourceStreamError);
end;

function TResourceStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  raise EStreamError.CreateRes(@SCantWriteResourceStreamError);
end;

{ TFiler }

constructor TFiler.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create;
  FStream := Stream;
  FBufSize := BufSize;
  SetLength(FBuffer, BufSize);
end;

destructor TFiler.Destroy;
begin
  SetLength(FBuffer, 0);
end;

procedure TFiler.SetRoot(Value: TComponent);
begin
  FRoot := Value;
end;

{ TPropFixup }

type
  TPropFixup = class
    FInstance: TPersistent;
    FInstanceRoot: TComponent;
    FPropInfo: PPropInfo;
    FRootName: string;
    FName: string;
    constructor Create(Instance: TPersistent; InstanceRoot: TComponent;
      PropInfo: PPropInfo; const RootName, Name: string);
    function MakeGlobalReference: Boolean;
    procedure ResolveReference(Reference: Pointer); virtual;
  end;

  TPropIntfFixup = class(TPropFixup)
    procedure ResolveReference(Reference: Pointer); override;
  end;

var
  GlobalFixupList: TThreadList<TPropFixup>;

constructor TPropFixup.Create(Instance: TPersistent; InstanceRoot: TComponent;
  PropInfo: PPropInfo; const RootName, Name: string);
begin
  FInstance := Instance;
  FInstanceRoot := InstanceRoot;
  FPropInfo := PropInfo;
  FRootName := RootName;
  FName := Name;
end;

function TPropFixup.MakeGlobalReference: Boolean;
var
  S: PChar;
  P: PChar;
begin
  Result := False;
  S := PChar(FName);
  P := S;
  while not (P^ in ['.', #0]) do Inc(P);
  if P^ = #0 then Exit;
  SetString(FRootName, S, P - S);
  Delete(FName, 1, P - S + 1);
  Result := True;
end;

procedure TPropFixup.ResolveReference(Reference: Pointer);
begin
  SetOrdProp(FInstance, FPropInfo, IntPtr(Reference));
end;

procedure TPropIntfFixup.ResolveReference(Reference: Pointer);
var
  Intf: IInterface;
  LGuid: TGuid;
begin
  Intf := nil;
  if Reference <> nil then
  begin
    // TODO: Remove LGuid variable after compiler issue is resolved.  See RS-69126.
    LGuid := GetTypeData(FPropInfo^.PropType^)^.Guid;
    if not Supports(TObject(Reference), LGuid, Intf) then
      Intf := nil;
  end;
  SetInterfaceProp(FInstance, FPropInfo, Intf);
end;

function FindNestedComponent(const Root: TComponent; const NamePath: string): TComponent;
var
  Current, Found: TComponent;
  S, P: PChar;
  Name: string;
begin
  Result := nil;
  if NamePath = '' then Exit;
  Current := Root;
  P := PChar(NamePath);
  while P^ <> #0 do
  begin
    S := P;
    while not (P^ in ['.', '-', #0]) do Inc(P);
    SetString(Name, S, P - S);
    Found := Current.FindComponent(Name);
    if (Found = nil) and SameText(Name, 'Owner') then                           { Do not translate }
      Found := Current;
    if Found = nil then Exit;
    if P^ = '.' then Inc(P);
    if P^ = '-' then Inc(P);
    if P^ = '>' then Inc(P);
    Current := Found;
  end;
  Result := Current;
end;

procedure GlobalFixupReferences;
var
  FinishedList: TList<TPersistent>;
  NotFinishedList: TList<TPersistent>;
  GlobalList: TList<TPropFixup>;
  I: Integer;
  Root: TComponent;
  Instance: TPersistent;
  Reference: Pointer;

  procedure AddFinished(Instance: TPersistent);
  begin
    if (FinishedList.IndexOf(Instance) < 0) and
      (NotFinishedList.IndexOf(Instance) >= 0) then
      FinishedList.Add(Instance);
  end;

  procedure AddNotFinished(Instance: TPersistent);
  var
    Index: Integer;
  begin
    Index := FinishedList.IndexOf(Instance);
    if Index <> -1 then FinishedList.Delete(Index);
    if NotFinishedList.IndexOf(Instance) < 0 then
      NotFinishedList.Add(Instance);
  end;

var
  aPropFixup: TPropFixup;
begin
  // Fixup resolution requires a stable component / name space
  // Block construction and destruction of forms / datamodules during fixups
  GlobalNameSpace.BeginWrite;
  try
    GlobalList := GlobalFixupList.LockList;
    try
      if GlobalList.Count > 0 then
      begin
        FinishedList := TList<TPersistent>.Create;
        try
          NotFinishedList := TList<TPersistent>.Create;
          try
            I := 0;
            while I < GlobalList.Count do
            begin
              aPropFixup := GlobalList[I];
              begin
                Root := FindGlobalComponent(aPropFixup.FRootName);
                if (Root <> nil) or (GetOrdProp(aPropFixup.FInstance, aPropFixup.FPropInfo) <> 0) then
                begin
                  if Root <> nil then
                    if not (csReading in Root.ComponentState) then
                    begin
                      Reference := FindNestedComponent(Root, aPropFixup.FName);
                      aPropFixup.ResolveReference(Reference);
                    end
                    else
                    begin
                      AddNotFinished(aPropFixup.FInstance);
                      Inc(I);
                      Continue;
                    end;
                  AddFinished(aPropFixup.FInstance);
                  GlobalList.Delete(I);
                  aPropFixup.Free;
                end else
                begin
                  AddNotFinished(aPropFixup.FInstance);
                  Inc(I);
                end;
              end;
            end;
          finally
            NotFinishedList.Free;
          end;
          for I := 0 to FinishedList.Count - 1 do
          begin
            Instance := FinishedList[I];
            if Instance is TComponent then
              Exclude(TComponent(Instance).FComponentState, csFixups);
          end;
        finally
          FinishedList.Free;
        end;
      end;
    finally
      GlobalFixupList.UnlockList;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

function NameInStrings(Strings: TStrings; const Name: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Strings.Count - 1 do
    if SameText(Name, Strings[I]) then Exit;
  Result := False;
end;

procedure GetFixupReferenceNames(const Root: TComponent; const Names: TStrings);
var
  I: Integer;
  aList: TList<TPropFixup>;
  Fixup: TPropFixup;
begin
  aList := GlobalFixupList.LockList;
  try
    for I := 0 to aList.Count - 1 do
    begin
      Fixup := aList.Items[I];
      if ((Root = nil) or (Fixup.FInstanceRoot = Root)) and
        not NameInStrings(Names, Fixup.FRootName) then
        Names.Add(Fixup.FRootName);
    end;
  finally
    GlobalFixupList.UnlockList;
  end;
end;

procedure RedirectFixupReferences(const Root: TComponent; const OldRootName,
  NewRootName: string);
var
  I: Integer;
  aList: TList<TPropFixup>;
  Fixup: TPropFixup;
begin
  aList := GlobalFixupList.LockList;
  try
    for I := 0 to aList.Count - 1 do
    begin
      Fixup := aList.Items[I];
      if ((Root = nil) or (Fixup.FInstanceRoot = Root)) and
        SameText(OldRootName, Fixup.FRootName) then
        Fixup.FRootName := NewRootName;
    end;
    GlobalFixupReferences;
  finally
    GlobalFixupList.Unlocklist;
  end;
end;

procedure RemoveFixupReferences(const Root: TComponent; const RootName: string);
var
  I: Integer;
  aList: TList<TPropFixup>;
  Fixup: TPropFixup;
begin
  if GlobalFixupList = nil then Exit;
  aList := GlobalFixupList.LockList;
  try
    for I := aList.Count - 1 downto 0 do
    begin
      Fixup := aList.Items[I];
      if ((Root = nil) or (Fixup.FInstanceRoot = Root)) and
        ((RootName = '') or SameText(RootName, Fixup.FRootName)) then
      begin
        aList.Delete(I);
        Fixup.Free;
      end;
    end;
  finally
    GlobalFixupList.UnlockList;
  end;
end;

procedure RemoveFixups(const Instance: TPersistent);
var
  I: Integer;
  aList: TList<TPropFixup>;
  Fixup: TPropFixup;
begin
  if GlobalFixupList = nil then Exit;
  aList := GlobalFixupList.LockList;
  try
    for I := aList.Count - 1 downto 0 do
    begin
      Fixup := aList.Items[I];
      if (Fixup.FInstance = Instance) then
      begin
        aList.Delete(I);
        Fixup.Free;
      end;
    end;
  finally
    GlobalFixupList.UnlockList;
  end;
end;

procedure GetFixupInstanceNames(const Root: TComponent; const ReferenceRootName: string; Names: TStrings);
var
  I: Integer;
  aList: TList<TPropFixup>;
  Fixup: TPropFixup;
begin
  aList := GlobalFixupList.LockList;
  try
    for I := 0 to aList.Count - 1 do
    begin
      Fixup := aList.Items[I];
      if (Fixup.FInstanceRoot = Root) and
        SameText(ReferenceRootName, Fixup.FRootName) and
        not NameInStrings(Names, Fixup.FName) then
        Names.Add(Fixup.FName);
    end;
  finally
    GlobalFixupList.UnlockList;
  end;
end;

{ TReader }

procedure ReadError(Ident: PResStringRec);
begin
  raise EReadError.CreateRes(Ident);
end;

procedure PropValueError;
begin
  ReadError(@SInvalidPropertyValue);
end;

procedure PropertyNotFound(const Name: string);
begin
  raise EReadError.CreateResFmt(@SUnknownProperty, [Name]);
end;

function EnumValue(EnumType: PTypeInfo; const EnumName: string): Integer;
begin
  Result := GetEnumValue(EnumType, EnumName);
  if Result = -1 then PropValueError;
end;

function SetElementValue(EnumType: PTypeInfo; const EnumName: string): Integer;
begin
  Result := GetSetElementValue(EnumType, EnumName);
  if Result = -1 then PropValueError;
end;

destructor TReader.Destroy;
begin
  FStream.Seek(FBufPos - FBufEnd, TSeekOrigin.soCurrent);
  inherited Destroy;
end;

procedure TReader.BeginReferences;
begin
  FLoaded := TList<TComponent>.Create;
  try
    FFixups := TList<TObject>.Create;
  except
    FLoaded.Free;
    raise;
  end;
end;

procedure TReader.CheckValue(Value: TValueType);
begin
  if ReadValue <> Value then
  begin
    Dec(FBufPos);
    SkipValue;
    PropValueError;
  end;
end;

procedure TReader.DefineProperty(const Name: string;
  ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
begin
  if SameText(Name, FPropName) and Assigned(ReadData) then
  begin
    ReadData(Self);
    FPropName := '';
  end;
end;

procedure TReader.DefineBinaryProperty(const Name: string;
  ReadData, WriteData: TStreamProc; HasData: Boolean);
var
  Stream: TMemoryStream;
  Count: UInt32;
begin
  if SameText(Name, FPropName) and Assigned(ReadData) then
  begin
    if ReadValue <> vaBinary then
    begin
      Dec(FBufPos);
      SkipValue;
      FCanHandleExcepts := True;
      PropValueError;
    end;
    Stream := TMemoryStream.Create;
    try
      Read(Count, SizeOf(Count));
      Stream.Size := Count;
      Read(Stream.Memory^, Count);
      FCanHandleExcepts := True;
      ReadData(Stream);
    finally
      Stream.Free;
    end;
    FPropName := '';
  end;
end;

function TReader.EndOfList: Boolean;
begin
  Result := ReadValue = vaNull;
  Dec(FBufPos);
end;

procedure TReader.EndReferences;
begin
  FreeFixups;
  FLoaded.Free;
  FLoaded := nil;
end;

procedure TReader.EnsureAtLeast(NumBytes: Integer);
var
  C: NativeInt;
begin
  C := FBufEnd - FBufPos;
  if C < NumBytes then
  begin
    ReadBuffer(C);
    if Length(FBuffer) < NumBytes then
    begin
      // Grow the buffer if the buffer is to small for what we are reading
      SetLength(FBuffer, NumBytes);
      ReadBuffer(FBufEnd, False);
    end;
    if FBufEnd - FBufPos < NumBytes then
      raise EReadError.Create(SReadError);
  end;
end;

function TReader.Error(const Message: string): Boolean;
begin
  Result := False;
  if Assigned(FOnError) then FOnError(Self, Message, Result);
end;

function TReader.FindMethodInstance(Root: TComponent; const MethodName: string): TMethod;
var
  Error: Boolean;
begin
  if Assigned(FOnFindMethodInstance) then
  begin
    Result.Code := Root.MethodAddress(MethodName);
    Result.Data := Root;
    Error := Result.Code = nil;
    FOnFindMethodInstance(Self, MethodName, Result, Error);
  end else
    Error := True;
  if Error then
  begin
    Result.Data := Root;
    Result.Code := FindMethod(Root, MethodName);
  end;
end;

function TReader.FindMethod(Root: TComponent; const MethodName: string): Pointer;
var
  Error: Boolean;
begin
  Result := Root.MethodAddress(MethodName);
  Error := Result = nil;
  if Assigned(FOnFindMethod) then FOnFindMethod(Self, MethodName, Result, Error);
  if Error then PropValueError;
end;

procedure RemoveGlobalFixup(const Fixup: TPropFixup);
var
  I: Integer;
  FixupList: TList<TPropFixup>;
  aPropFixup: TPropFixup;
begin
  FixupList := GlobalFixupList.LockList;
  try
    for I := FixupList.Count - 1 downto 0 do
    begin
      aPropFixup := FixupList.Items[I];
      if (aPropFixup.FInstance = Fixup.FInstance) and (aPropFixup.FPropInfo = Fixup.FPropInfo) then
      begin
        aPropFixup.Free;
        FixupList.Delete(I);
      end;
    end;
  finally
    GlobalFixupList.UnlockList;
  end;
end;

procedure TReader.DoFixupReferences;
var
  I: Integer;
  aPropFixup: TPropFixup;
  CompName: string;
  Reference: Pointer;
begin
  if FFixups <> nil then
    try
      for I := 0 to FFixups.Count - 1 do
      begin
        aPropFixup := TPropFixup(FFixups[I]);
        CompName := aPropFixup.FName;
        ReferenceName(CompName);
        Reference := FindNestedComponent(aPropFixup.FInstanceRoot, CompName);
        if (Reference = nil) and Assigned(FOnFindComponentInstance) then
          FOnFindComponentInstance(Self, CompName, Reference);
        { Free any preexisting global fixups for this instance/property.
          Last fixup added is the only one that counts.
          In particular, fixups created when streaming inherited forms/frames
          must be destroyed when overriding references are found later
          in the stream.  }
        RemoveGlobalFixup(TPropFixup(FFixups[I]));
        if (Reference = nil) and aPropFixup.MakeGlobalReference then
        begin
          GlobalFixupList.Add(TPropFixup(FFixups[I]));
          FFixups[I] := nil;
        end
        else
          aPropFixup.ResolveReference(Reference);
      end;
    finally
      FreeFixups;
    end;
end;

procedure TReader.FixupReferences;
var
  I: Integer;
begin
  DoFixupReferences;
  GlobalFixupReferences;
  for I := 0 to FLoaded.Count - 1 do TComponent(FLoaded[I]).Loaded;
end;

procedure TReader.FlushBuffer;
begin
  FStream.Position := Position;
  FBufPos := 0;
  FBufEnd := 0;
end;

procedure TReader.FreeFixups;
var
  I: Integer;
begin
  if FFixups <> nil then
  begin
    for I := 0 to FFixups.Count - 1 do
      TPropFixup(FFixups[I]).DisposeOf;
    FFixups.Free;
    FFixups := nil;
  end;
end;

function TReader.GetFieldClass(const Instance: TObject; const ClassName: string): TPersistentClass;
var
  I: Integer;
  ClassTable: PFieldClassTable;
  ClassType: TClass;
begin
  ClassType := Instance.ClassType;
  while ClassType <> TPersistent do
  begin
    ClassTable := GetFieldClassTable(ClassType);
    if ClassTable <> nil then
      for I := 0 to ClassTable^.Count - 1 do
      begin
        Result := ClassTable^.Classes[I]^;
        if Result.ClassNameIs(ClassName) then Exit;
      end;
    ClassType := ClassType.ClassParent;
  end;
  if FFinder <> nil then
    Result := FFinder.GetClass(ClassName)
  else
    Result := GetClass(ClassName);
end;

function TReader.GetPosition: NativeInt;
begin
  Result := FStream.Position - (FBufEnd - FBufPos);
end;

function TReader.NextValue: TValueType;
begin
  Result := ReadValue;
  Dec(FBufPos);
end;

procedure TReader.PropertyError(const Name: string);
begin
  SkipValue;
  PropertyNotFound(Name);
end;

procedure TReader.Read(var Buffer; Count: NativeInt);
var
  LShouldRead,
  BufOffset: NativeInt;
begin
  BufOffset := 0;
  while Count > 0 do
  begin
    { Calculate the amount of data left in the internal buffer. Read more if necessary }
    LShouldRead := FBufEnd - FBufPos;
    if LShouldRead = 0 then
    begin
      ReadBuffer();
      LShouldRead := FBufEnd;
    end;

    { Check how much we can read in one go }
    if (Count <= LShouldRead) then
      LShouldRead := Count;

    { Copy the data and update internal pointers. }
    Move(Pointer(PByte(FBuffer) + FBufPos)^, Pointer(PByte(@Buffer) + BufOffset)^, LShouldRead);
    Inc(FBufPos, LShouldRead);
    Inc(BufOffset, LShouldRead);

    { Update the count to reflect the read data }
    Dec(Count, LShouldRead);
  end;
end;

procedure TReader.Read(Buffer: TBytes; Offset, Count: NativeInt);
var
  LShouldRead: NativeInt;
begin
  while Count > 0 do
  begin
    { Calculate the amount of data left in the internal buffer. Read more if necessary }
    LShouldRead := FBufEnd - FBufPos;
    if LShouldRead = 0 then
    begin
      ReadBuffer();
      LShouldRead := FBufEnd;
    end;

    { Check how much we can read in one go }
    if (Count <= LShouldRead) then
      LShouldRead := Count;

    { Copy the data and update internal pointers. }
    Move(Pointer(PByte(@FBuffer[FBufPos]))^, Pointer(PByte(@Buffer[Offset]))^, LShouldRead);
    Inc(FBufPos, LShouldRead);
    Inc(Offset, LShouldRead);

    { Update the count to reflect the read data }
    Dec(Count, LShouldRead);
  end;
end;

procedure TReader.Read(Buffer: TBytes; Count: NativeInt);
begin
  Read(Buffer, 0, Count);
end;

procedure TReader.ReadBuffer(Keeping: Integer; MoveBuffer: Boolean);
begin
  if MoveBuffer and (Keeping > 0) then
    Move(Pointer(PByte(@FBuffer[0]) + Length(FBuffer) - Keeping)^,
         Pointer(PByte(@FBuffer[0]))^, Keeping);
  FBufEnd := FStream.Read(FBuffer, Keeping, Length(FBuffer) - Keeping);
  if FBufEnd = 0 then
    raise EReadError.Create(SReadError);
  Inc(FBufEnd, Keeping);
  FBufPos := 0;
end;

{$IFNDEF NEXTGEN}
procedure TReader.ReadVar(var Buffer: AnsiChar; Count: NativeInt);
begin
  Buffer := #0;
  EnsureAtLeast(Count);
  if Count >= 1 then
    Buffer := AnsiChar(FBuffer[FBufPos]);
  Inc(FBufPos, Count);
end;
{$ENDIF !NEXTGEN}

procedure TReader.ReadVar(var Buffer: Char; Count: NativeInt);
begin
  Buffer := #0;
  EnsureAtLeast(Count);
  if Count = 1 then
    Buffer := Char(FBuffer[FBufPos])
  else if Count >= 2 then
    Buffer := Char(FBuffer[FBufPos] or (FBuffer[FBufPos + 1] shl 8));
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: Int8; Count: NativeInt);
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count >= 1 then
    Buffer := FBuffer[FBufPos];
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: UInt8; Count: NativeInt);
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count >= 1 then
    Buffer := FBuffer[FBufPos];
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: Int16; Count: NativeInt);
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count > 0 then
  begin
    Buffer := Int16(FBuffer[FBufPos]);
    if Count > 1 then
      Buffer := Buffer or (Int16(FBuffer[FBufPos + 1]) shl 8);
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: UInt16; Count: NativeInt);
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count > 0 then
  begin
    Buffer := UInt16(FBuffer[FBufPos]);
    if Count > 1 then
      Buffer := Buffer or (UInt16(FBuffer[FBufPos + 1]) shl 8);
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: Int32; Count: NativeInt);
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count > 0 then
  begin
    Buffer := Int32(FBuffer[FBufPos]);
    if Count > 1 then
    begin
      Buffer := Buffer or (Int32(FBuffer[FBufPos + 1]) shl 8);
      if Count > 2 then
      begin
        Buffer := Buffer or (Int32(FBuffer[FBufPos + 2]) shl 16);
        if Count > 3 then
          Buffer := Buffer or (Int32(FBuffer[FBufPos + 3]) shl 24);
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: UInt32; Count: NativeInt);
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count > 0 then
  begin
    Buffer := UInt32(FBuffer[FBufPos]);
    if Count > 1 then
    begin
      Buffer := Buffer or (UInt32(FBuffer[FBufPos + 1]) shl 8);
      if Count > 2 then
      begin
        Buffer := Buffer or (UInt32(FBuffer[FBufPos + 2]) shl 16);
        if Count > 3 then
          Buffer := Buffer or (UInt32(FBuffer[FBufPos + 3]) shl 24);
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: Int64; Count: NativeInt);
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count > 0 then
  begin
    Buffer := Int64(FBuffer[FBufPos]);
    if Count > 1 then
    begin
      Buffer := Buffer or (Int64(FBuffer[FBufPos + 1]) shl 8);
      if Count > 2 then
      begin
        Buffer := Buffer or (Int64(FBuffer[FBufPos + 2]) shl 16);
        if Count > 3 then
        begin
          Buffer := Buffer or (Int64(FBuffer[FBufPos + 3]) shl 24);
          if Count > 4 then
          begin
            Buffer := Buffer or (Int64(FBuffer[FBufPos + 4]) shl 32);
            if Count > 5 then
            begin
              Buffer := Buffer or (Int64(FBuffer[FBufPos + 5]) shl 40);
              if Count > 6 then
              begin
                Buffer := Buffer or (Int64(FBuffer[FBufPos + 6]) shl 48);
                if Count > 7 then
                  Buffer := Buffer or (Int64(FBuffer[FBufPos + 7]) shl 56);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: UInt64; Count: NativeInt);
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count > 0 then
  begin
    Buffer := UInt64(FBuffer[FBufPos]);
    if Count > 1 then
    begin
      Buffer := Buffer or (UInt64(FBuffer[FBufPos + 1]) shl 8);
      if Count > 2 then
      begin
        Buffer := Buffer or (UInt64(FBuffer[FBufPos + 2]) shl 16);
        if Count > 3 then
        begin
          Buffer := Buffer or (UInt64(FBuffer[FBufPos + 3]) shl 24);
          if Count > 4 then
          begin
            Buffer := Buffer or (UInt64(FBuffer[FBufPos + 4]) shl 32);
            if Count > 5 then
            begin
              Buffer := Buffer or (UInt64(FBuffer[FBufPos + 5]) shl 40);
              if Count > 6 then
              begin
                Buffer := Buffer or (UInt64(FBuffer[FBufPos + 6]) shl 48);
                if Count > 7 then
                  Buffer := Buffer or (UInt64(FBuffer[FBufPos + 7]) shl 56);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: Single; Count: NativeInt);
var
  ind: Integer;
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count >= 4 then
  begin
    for ind := 0 to 3 do
      Buffer.Bytes[ind] := FBuffer[FBufPos + ind];
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: Double; Count: NativeInt);
var
  ind: Integer;
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count >= 8 then
  begin
    for ind := 0 to 7 do
      Buffer.Bytes[ind] := FBuffer[FBufPos + ind];
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: Extended; Count: NativeInt);
const
  BufSize = SizeOf(Extended);
var
  ind: Integer;
begin
  Buffer := 0;
  EnsureAtLeast(Count);
  if Count >= BufSize then
  begin
    for ind := 0 to BufSize - 1 do
      Buffer.Bytes[ind] := FBuffer[FBufPos + ind];
  end;
  Inc(FBufPos, Count);
end;

procedure TReader.ReadVar(var Buffer: TExtended80Rec; Count: NativeInt);
var
  ind: Integer;
begin
  Buffer.BuildUp(False, 0, -$3FFF); // 0.0
  EnsureAtLeast(Count);
  if Count >= 10 then
  begin
    for ind := 0 to 9 do
      Buffer.Bytes[ind] := FBuffer[FBufPos + ind];
  end;
  Inc(FBufPos, Count);
end;

function TReader.ReadBoolean: Boolean;
var
  Value: TValueType;
begin
  Value := ReadValue;
  if Value in [vaTrue, vaFalse] then
    Result := Value = vaTrue
  else
  begin
    ReadError(@SInvalidPropertyValue);
    Result := False;
  end;
end;

function TReader.ReadChar: Char;
var
  Temp: string;
begin
  Temp := ReadString;
  if Temp.Length > 1 then
    PropValueError;
  Result := Temp[Low(string)];
end;

{$IFNDEF NEXTGEN}
function TReader.ReadWideChar: WideChar;
begin
  Result := ReadChar;
end;
{$ENDIF !NEXTGEN}

procedure TReader.ReadCollection(const Collection: TCollection);
var
  Item: TPersistent;
begin
  Collection.BeginUpdate;
  try
    if not EndOfList then Collection.Clear;
    while not EndOfList do
    begin
      if NextValue in [vaInt8, vaInt16, vaInt32] then ReadInteger;
      Item := Collection.Add;
      ReadListBegin;
      while not EndOfList do ReadProperty(Item);
      ReadListEnd;
    end;
    ReadListEnd;
  finally
    Collection.EndUpdate;
  end;
end;

function TReader.ReadComponent(Component: TComponent): TComponent;
var
  CompClass, CompName: string;
  Flags: TFilerFlags;
  Position: Integer;
  OldParent, OldLookupRoot: TComponent;
  SubComponents: array of TComponent;

  procedure AddSubComponentsToLoaded(const Component: TComponent);
  var
    I: Integer;
  begin
    for I := 0 to Length(SubComponents) - 1 do
      FLoaded.Add(SubComponents[I]);
  end;

  procedure CheckSubComponents(const Component: TComponent);
  var
    I: Integer;
  begin
    for I := 0 to Component.ComponentCount - 1 do
      if csSubComponent in Component.Components[I].FComponentStyle then
      begin
        SetLength(SubComponents, Length(SubComponents) + 1);
        SubComponents[Length(SubComponents) - 1] := Component.Components[I];
      end;
  end;

  procedure SetSubComponentState(State: TComponentState; Add: Boolean = True);
  var
    I: Integer;
  begin
    if Add then
      for I := 0 to Length(SubComponents) - 1 do
        SubComponents[I].FComponentState := SubComponents[I].FComponentState + State
    else
      for I := 0 to Length(SubComponents) - 1 do
        SubComponents[I].FComponentState := SubComponents[I].FComponentState - State;
  end;

  function ComponentCreated: Boolean;
  begin
    Result := not (ffInherited in Flags) and (Component = nil);
  end;

  function Recover(var Component: TComponent): Boolean;
  begin
    Result := False;
    if not (ExceptObject is Exception) then Exit;
    if ComponentCreated then Component.Free;
    Component := nil;
    SkipComponent(False);
    Result := Error(Exception(ExceptObject).Message);
  end;

  procedure CreateComponent;
  var
    ComponentClass: TComponentClass;
  begin
    try
      ComponentClass := FindComponentClass(CompClass);
      Result := nil;
      if Assigned(FOnCreateComponent) then
        FOnCreateComponent(Self, ComponentClass, Result);
      if Result = nil then
      begin
        Result := TComponent(ComponentClass.NewInstance);
        if ffInline in Flags then
        begin
          Include(Result.FComponentState, csLoading);
          Include(Result.FComponentState, csInline);
        end;
        try
          Result.Create(Owner);
        except
          Result := nil;
          raise;
        end;
      end;
      Include(Result.FComponentState, csLoading);
    except
      if not Recover(Result) then raise;
    end;
  end;

  procedure SetCompName;
  begin
    try
      Result.SetParentComponent(Parent);
      SetName(Result, CompName);
      if (csDesigning in Result.ComponentState) and (FindGlobalComponent(CompName) = Result) then
        Include(Result.FComponentState, csInline);
    except
      if not Recover(Result) then raise;
    end;
  end;

  procedure FindExistingComponent;
  begin
    try
      Result := FindAncestorComponent(CompName, FindComponentClass(CompClass));
      Parent := Result.GetParentComponent;
      if Parent = nil then Parent := Root;
    except
      if not Recover(Result) then raise;
    end;
  end;

begin
  ReadPrefix(Flags, Position);
  CompClass := ReadStr;
  CompName := ReadStr;
  OldParent := Parent;
  OldLookupRoot := FLookupRoot;
  try
    Result := Component;
    if Result = nil then
      if ffInherited in Flags then
        FindExistingComponent else
        CreateComponent;
    if Result <> nil then
      try
        CheckSubComponents(Result);
        Include(Result.FComponentState, csLoading);
        SetSubComponentState([csLoading]);
        if not (ffInherited in Flags) then SetCompName;
        if Result = nil then Exit;
        if csInline in Result.ComponentState then
          FLookupRoot := Result;
        Include(Result.FComponentState, csReading);
        SetSubComponentState([csReading]);
        Result.ReadState(Self);
        Exclude(Result.FComponentState, csReading);
        SetSubComponentState([csReading], False);
        if ffChildPos in Flags then Parent.SetChildOrder(Result, Position);
        if (ffInherited in Flags) or (csInline in Result.ComponentState) then
        begin
          if FLoaded.IndexOf(Result) < 0 then
          begin
            AddSubComponentsToLoaded(Result);
            FLoaded.Add(Result);
          end;
        end
        else
        begin
          AddSubComponentsToLoaded(Result);
          FLoaded.Add(Result);
        end;
      except
        if ComponentCreated then Result.Free;
        raise;
      end;
  finally
    Parent := OldParent;
    FLookupRoot := OldLookupRoot;
  end;
end;

procedure TReader.ReadData(const Instance: TComponent);
begin
  if FFixups = nil then
  begin
    FFixups := TList<TObject>.Create;
    try
      ReadDataInner(Instance);
      DoFixupReferences;
    finally
      FreeFixups;
    end;
  end else
    ReadDataInner(Instance);
end;

procedure TReader.ReadDataInner(const Instance: TComponent);
var
  OldParent, OldOwner: TComponent;
begin
  while not EndOfList do ReadProperty(Instance);
  ReadListEnd;
  OldParent := Parent;
  OldOwner := Owner;
  Parent := Instance.GetChildParent;
  try
    Owner := Instance.GetChildOwner;
    if not Assigned(Owner) then Owner := Root;
    while not EndOfList do ReadComponent(nil);
    ReadListEnd;
  finally
    Parent := OldParent;
    Owner := OldOwner;
  end;
end;

function TReader.ReadFloat: Extended;
{$IFDEF EXTENDEDIS10BYTES}
begin
  if ReadValue = vaExtended then
    Read(Result, SizeOf(Result))
  else
  begin
    Dec(FBufPos);
    Result := ReadDouble;
  end;
end;
{$ELSE !EXTENDEDIS10BYTES}
var
  E: TExtended80Rec;
begin
  if ReadValue = vaExtended then
  begin
    Read(E, SizeOf(E));
    Result := Extended(E);
  end
  else
  begin
    Dec(FBufPos);
    Result := ReadDouble;
  end;
end;
{$ENDIF !EXTENDEDIS10BYTES}

function TReader.ReadDouble: Double;
begin
  if ReadValue = vaDouble then
    Read(Result, SizeOf(Result))
  else
  begin
    Dec(FBufPos);
    Result := ReadInt64;
  end;
end;

function TReader.ReadSingle: Single;
begin
  if ReadValue = vaSingle then Read(Result, SizeOf(Result)) else
  begin
    Dec(FBufPos);
    Result := ReadInt64;
  end;
end;

function TReader.ReadCurrency: Currency;
type
  PCurrency = ^Currency;
var
  Buf: UInt64;
begin
  if ReadValue = vaCurrency then
  begin
    Read(Buf, 8);
    Result := PCurrency(@Buf)^;
  end
  else
  begin
    Dec(FBufPos);
    Result := ReadInt64;
  end;
end;

function TReader.ReadDate: TDateTime;
var
  D: Double;
begin
  if ReadValue = vaDate then
  begin
    Read(D, SizeOf(Double));
    Result := D;
  end
  else
  begin
    Dec(FBufPos);
    Result := ReadInt64;
  end;
end;

function TReader.ReadIdent: string;
var
  L: Byte;
  Buf: TBytes;
begin
  case ReadValue of
    vaIdent:
      begin
        Read(L, SizeOf(Byte));
        SetLength(Buf, L);
        Read(Buf, L);
        Result := TEncoding.UTF8.GetString(Buf);
      end;
    vaFalse:
      Result := 'False';
    vaTrue:
      Result := 'True';
    vaNil:
      Result := 'nil';
    vaNull:
      Result := 'Null';
  else
    PropValueError;
  end;
end;

function TReader.ReadInteger: Integer;
var
  S: Shortint;
  I: Smallint;
begin
  case ReadValue of
    vaInt8:
      begin
        Read(S, SizeOf(S));
        Result := S;
      end;
    vaInt16:
      begin
        Read(I, SizeOf(I));
        Result := I;
      end;
    vaInt32:
      Read(Result, SizeOf(Result));
  else
    PropValueError;
  end;
end;

function TReader.ReadInt64: Int64;
begin
  if NextValue = vaInt64 then
  begin
    ReadValue;
    Read(Result, SizeOf(Result));
  end
  else
    Result := ReadInteger;
end;

procedure TReader.ReadListBegin;
begin
  CheckValue(vaList);
end;

procedure TReader.ReadListEnd;
begin
  CheckValue(vaNull);
end;

procedure TReader.ReadPrefix(var Flags: TFilerFlags; var AChildPos: Integer);
var
  Prefix: Byte;
begin
  Flags := [];
  if Byte(NextValue) and $F0 = $F0 then
  begin
    Prefix := Byte(ReadValue);
    Byte(Flags) := Prefix and $0F;
    if ffChildPos in Flags then AChildPos := ReadInteger;
  end;
end;

procedure TReader.ReadProperty(AInstance: TPersistent);
var
  I, J, L: Integer;
  Instance: TPersistent;
  PropInfo: PPropInfo;
  PropValue: TObject;
  PropPath: string;

  procedure HandleException(E: Exception);
  var
    Name: string;
  begin
    Name := '';
    if AInstance is TComponent then
      Name := TComponent(AInstance).Name;
    if Name = '' then Name := AInstance.ClassName;
    raise EReadError.CreateResFmt(@SPropertyException, [Name, DotSep, PropPath, E.Message]);
  end;

  procedure PropPathError;
  begin
    SkipValue;
    ReadError(@SInvalidPropertyPath);
  end;

begin
  try
    PropPath := ReadStr;
    try
      I := Low(string);
      L := High(PropPath);
      Instance := AInstance;
      FCanHandleExcepts := True;
      while True do
      begin
        J := I;
        while (I <= L) and (PropPath[I] <> '.') do Inc(I);
        FPropName := PropPath.SubString(J - Low(string), I - J);
        if I > L then Break;
        PropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
        if PropInfo = nil then
        begin
          // Call DefineProperties with the entire PropPath
          // to allow defining properties such as "Prop.SubProp"
          FPropName := PropPath;
          { Cannot reliably recover from an error in a defined property }
          FCanHandleExcepts := False;
          Instance.DefineProperties(Self);
          FCanHandleExcepts := True;
          if FPropName <> '' then
            PropertyError(FPropName);
          Exit;
        end;
        PropValue := nil;
        if PropInfo^.PropType^.Kind = tkClass then
          PropValue := TObject(GetOrdProp(Instance, PropInfo));
        if not (PropValue is TPersistent) then PropPathError;
        Instance := TPersistent(PropValue);
        Inc(I);
      end;
      PropInfo := GetPropInfo(Instance.ClassInfo, FPropName);
      if PropInfo <> nil then
        ReadPropValue(Instance, PropInfo)
      else
      begin
        { Cannot reliably recover from an error in a defined property }
        FCanHandleExcepts := False;
        Instance.DefineProperties(Self);
        FCanHandleExcepts := True;
        if FPropName <> '' then
          PropertyError(FPropName);
      end;
    except
      on E: Exception do HandleException(E);
    end;
  except
    on E: Exception do
      if not FCanHandleExcepts or not Error(E.Message) then raise;
  end;
end;

procedure TReader.ReadPropValue(const Instance: TPersistent; PropInfo: Pointer);
const
  NilMethod: TMethod = (Code: nil; Data: nil);
var
  PropType: PTypeInfo;
  LMethod: TMethod;
  EnumValue: Integer;

  procedure SetIntIdent(const Instance: TPersistent; PropInfo: Pointer;
    const Ident: string);
  var
    V: FixedInt;
    IdentToInt: TIdentToInt;
  begin
    IdentToInt := FindIdentToInt(PPropInfo(PropInfo)^.PropType^);
    if Assigned(IdentToInt) and IdentToInt(Ident, V) then
      SetOrdProp(Instance, PropInfo, V)
    else
      PropValueError;
  end;

  procedure SetObjectIdent(const Instance: TPersistent; PropInfo: Pointer;
    const Ident: string);
  begin
    FFixups.Add(TPropFixup.Create(Instance, Root, PropInfo, '', Ident));
  end;

  // This is isolated into a local to help reduce transient VarClears
  procedure SetVariantReference;
  begin
    SetVariantProp(Instance, PropInfo, ReadVariant);
  end;

  procedure SetInterfaceReference;
  var
    Intf: IInterface;
  begin
    if NextValue = vaNil then
    begin
      ReadValue;
      Intf := nil;
      SetInterfaceProp(Instance, PropInfo, Intf);
    end
    else
      FFixups.Add(TPropIntfFixup.Create(Instance, Root, PropInfo, '', ReadIdent));
  end;

begin
  if PPropInfo(PropInfo)^.SetProc = nil then
    if not ((PPropInfo(PropInfo)^.PropType^.Kind = tkClass) and
       (TObject(GetOrdProp(Instance, PropInfo)) is TComponent) and
       (csSubComponent in TComponent(GetOrdProp(Instance, PropInfo)).ComponentStyle)) then
      ReadError(@SReadOnlyProperty);
  PropType := PPropInfo(PropInfo)^.PropType^;
  case PropType^.Kind of
    tkInteger:
      if NextValue = vaIdent then
        SetIntIdent(Instance, PropInfo, ReadIdent)
      else
        SetOrdProp(Instance, PropInfo, ReadInteger);
    tkChar, tkWChar:
      SetOrdProp(Instance, PropInfo, Ord(ReadChar));
    tkEnumeration:
      begin
        EnumValue := GetEnumValue(PropType, ReadIdent);

        if EnumValue <> -1 then
          SetOrdProp(Instance, PropInfo, EnumValue)
        else
          ReadError(@SInvalidPropertyValue);
      end;
    tkFloat:
      SetFloatProp(Instance, PropInfo, ReadFloat);
    tkString, tkLString:
      SetStrProp(Instance, PropInfo, ReadString);
    tkWString:
      SetStrProp(Instance, PropInfo, ReadString);
    tkUString:
      SetStrProp(Instance, PropInfo, ReadString);
    tkSet:
      SetOrdProp(Instance, PropInfo, ReadSet(PropType));
    tkClass:
      case NextValue of
        vaNil:
          begin
            ReadValue;
            SetOrdProp(Instance, PropInfo, 0);
          end;
        vaCollection:
          begin
            ReadValue;
            ReadCollection(TCollection(GetOrdProp(Instance, PropInfo)));
          end
      else
        SetObjectIdent(Instance, PropInfo, ReadIdent);
      end;
    tkMethod:
      if NextValue = vaNil then
      begin
        ReadValue;
        SetMethodProp(Instance, PropInfo, NilMethod);
      end
      else
      begin
        LMethod := FindMethodInstance(Root, ReadIdent);
        if LMethod.Code <> nil then SetMethodProp(Instance, PropInfo, LMethod);
      end;
    tkVariant:
      SetVariantReference;
    tkInt64:
      SetInt64Prop(Instance, PropInfo, ReadInt64);
    tkInterface:
      SetInterfaceReference;
  end;
end;

function TReader.ReadRootComponent(const Root: TComponent): TComponent;

  function FindUniqueName(const Name: string): string;
  var
    I: Integer;
  begin
    I := 0;
    Result := Name;
    while not IsUniqueGlobalComponentName(Result) do
    begin
      Inc(I);
      Result := Format('%s_%d', [Name, I]);
    end;
  end;

var
  I: Integer;
  Flags: TFilerFlags;
  G: TList<TComponent>;
begin
  ReadSignature;
  Result := nil;
  GlobalNameSpace.BeginWrite;  // Loading from stream adds to name space
  try
    try
      ReadPrefix(Flags, I);
      if Root = nil then
      begin
        Result := TComponentClass(FindClass(ReadStr)).Create(nil);
        Result.Name := ReadStr;
      end else
      begin
        Result := Root;
        ReadStr; { Ignore class name }
        if csDesigning in Result.ComponentState then
          ReadStr else
        begin
          Include(Result.FComponentState, csLoading);
          Include(Result.FComponentState, csReading);
          Result.Name := FindUniqueName(ReadStr);
        end;
      end;
      FRoot := Result;
      FFinder := TClassFinder.Create(TPersistentClass(Result.ClassType), True);
      try
        FLookupRoot := Result;
        G := GlobalLoaded;
        if G <> nil then
          FLoaded := G else
          FLoaded := TList<TComponent>.Create;
        try
          if FLoaded.IndexOf(FRoot) < 0 then
            FLoaded.Add(FRoot);
          FOwner := FRoot;
          Include(FRoot.FComponentState, csLoading);
          Include(FRoot.FComponentState, csReading);
          FRoot.ReadState(Self);
          Exclude(FRoot.FComponentState, csReading);
          if G = nil then
            for I := 0 to FLoaded.Count - 1 do TComponent(FLoaded[I]).Loaded;
        finally
          if G = nil then FLoaded.Free;
          FLoaded := nil;
        end;
      finally
        FFinder.Free;
      end;
      while True do
      try
        // Try to fix up all references until no exceptions or the exception
        // itself terminates the loop. This will loop if the error is ignored
        // in, for example, the form designer.
        GlobalFixupReferences;
        Break;
      except
        if not Error(Exception(ExceptObject).Message) then raise;
      end;
    except
      RemoveFixupReferences(Root, '');
      if Root = nil then Result.Free;
      raise;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

procedure TReader.ReadComponents(const AOwner, AParent: TComponent;
  Proc: TReadComponentsProc);
var
  Component: TComponent;
begin
  Root := AOwner;
  Owner := AOwner;
  Parent := AParent;
  BeginReferences;
  try
    FFinder := TClassFinder.Create(TPersistentClass(AOwner.ClassType), True);
    try
      while not EndOfList do
      begin
        ReadSignature;
        Component := ReadComponent(nil);
        if Assigned(Proc) then Proc(Component);
      end;
      ReadListEnd;
      FixupReferences;
    finally
      FFinder.Free;
    end;
  finally
    EndReferences;
  end;
end;

function TReader.ReadSet(SetType: Pointer): Integer;
var
  EnumType: PTypeInfo;
  EnumName: string;
begin
  try
    if ReadValue <> vaSet then PropValueError;
    EnumType := GetTypeData(SetType)^.CompType^;
    Result := 0;
    while True do
    begin
      EnumName := ReadStr;
      if EnumName = '' then Break;
      Include(TIntegerSet(Result), SetElementValue(EnumType, EnumName));
    end;
  except
    SkipSetBody;
    raise;
  end;
end;

procedure TReader.ReadSignature;
var
// TODO -cELBRUS_LONGINT64 : Verify TReader.ReadSignature (change LongInt to UInt32)
  Signature: UInt32;
begin
  Read(Signature, SizeOf(Signature));
  if Signature <> FilerSignature then
    ReadError(@SInvalidImage);
end;

function TReader.ReadStr: string;
var
  L: Byte;
  B: TBytes;
begin
  Read(L, SizeOf(Byte));
  SetLength(B, L);
  Read(B, L);
  Result := TEncoding.UTF8.GetString(B);
end;

function TReader.ReadString: string;
var
  L: Integer;
  LResult: TBytes;
begin
  L := 0;
  case ReadValue of
    vaWString:
      begin
        Read(L, SizeOf(Integer));
        SetLength(LResult, L*sizeof(WideChar));
        Read(LResult, L*sizeof(WideChar));
        Result := TEncoding.Unicode.GetString(LResult);
      end;
    vaUTF8String:
      begin
        Read(L, SizeOf(Integer));
        SetLength(LResult, L);
        Read(LResult, L);
        Result := TEncoding.UTF8.GetString(LResult);
      end;
    vaString:
      begin
        Read(L, SizeOf(Byte));
        SetLength(LResult, L);
        Read(LResult, L);
        Result := TEncoding.Default.GetString(LResult);
      end;
    vaLString:
      begin
        Read(L, SizeOf(Integer));
        SetLength(LResult, L);
        Read(LResult, L);
        Result := TEncoding.Default.GetString(LResult);
      end;
    else
      PropValueError;
  end;
end;

{$IFNDEF NEXTGEN}
function TReader.ReadWideString: string;
begin
  Result := ReadString;
end;
{$ENDIF !NEXTGEN}

function TReader.ReadValue: TValueType;
{$IF    SizeOf(TValueType) = 1}
var
  Buf: UInt8;
begin
  Read(Buf, 1);
  Result := TValueType(Buf);
end;
{$ELSE  SizeOf(TValueType) = 1}
var
  Buf: UInt32;
begin
  Buf := 0;
  Read(Buf, SizeOf(Result));
  Result := TValueType(Buf);
end;
{$ENDIF SizeOf(TValueType) = 1}

procedure TReader.SetPosition(Value: NativeInt);
begin
  FStream.Position := Value;
  FBufPos := 0;
  FBufEnd := 0;
end;

procedure TReader.SkipSetBody;
begin
  while ReadStr <> '' do begin end;
end;

procedure TReader.SkipValue;

  procedure SkipList;
  begin
    while not EndOfList do SkipValue;
    ReadListEnd;
  end;

  procedure SkipBinary(BytesPerUnit: Integer);
  var
// TODO -cELBRUS_LONGINT64 : Verify TReader.SkipValue.SKipBinary (change LongInt to Int32)
    Count: Int32;
  begin
    Read(Count, SizeOf(Count));
    SkipBytes(Count * BytesPerUnit);
  end;

  procedure SkipCollection;
  begin
    while not EndOfList do
    begin
      if NextValue in [vaInt8, vaInt16, vaInt32] then SkipValue;
      SkipBytes(1);
      while not EndOfList do SkipProperty;
      ReadListEnd;
    end;
    ReadListEnd;
  end;

begin
  case ReadValue of
    vaNull: { no value field, just an identifier };
    vaList: SkipList;
    vaInt8: SkipBytes(SizeOf(Byte));
    vaInt16: SkipBytes(SizeOf(Word));
    vaInt32: SkipBytes(SizeOf(UInt32));
    vaExtended: SkipBytes(SizeOf(TExtended80Rec));
    vaString, vaIdent: ReadStr;
    vaFalse, vaTrue: { no value field, just an identifier };
    vaBinary: SkipBinary(1);
    vaSet: SkipSetBody;
    vaLString: SkipBinary(1);
    vaCollection: SkipCollection;
    vaSingle: SkipBytes(Sizeof(Single));
    vaCurrency: SkipBytes(SizeOf(Currency));
    vaDate: SkipBytes(Sizeof(TDateTime));
    vaWString: SkipBinary(Sizeof(WideChar));
    vaInt64: SkipBytes(Sizeof(Int64));
    vaUTF8String: SkipBinary(1);
    vaDouble: SkipBytes(SizeOf(Double));
  end;
end;

procedure TReader.CopyValue(const Writer: TWriter);

  procedure CopySetBody;
  var
    s: String;
  begin
    Writer.WriteValue(ReadValue);
    repeat
      s := ReadStr;
      Writer.WriteUTF8Str(s);
    until s = '';
  end;

  procedure CopyList;
  begin
    Writer.WriteValue(ReadValue);
    while not EndOfList do
      CopyValue(Writer);
    ReadListEnd;
    Writer.WriteListEnd;
  end;

  procedure CopyBytes(Count: Integer);
  const
    SizeOfBytes = 8192;
  var
    Bytes: TBytes;//array[0..8191] of Char;
  begin
    SetLength(Bytes, SizeOfBytes);
    while Count > SizeOfBytes do
    begin
      Read(Bytes, SizeOfBytes);
      Writer.Write(Bytes, SizeOfBytes);
      Dec(Count, SizeOfBytes);
    end;
    if Count > 0 then
    begin
      Read(Bytes, Count);
      Writer.Write(Bytes, Count);
    end;
  end;

  procedure CopyBinary(BytesPerUnit: Integer);
  var
// TODO -cELBRUS_LONGINT64 : Verify TReader.CopyValue.CopyBinary (change LongInt to Int32)
    Count: Int32;
  begin
    Writer.WriteValue(ReadValue);
    Read(Count, SizeOf(Count));
    Writer.Write(Count, SizeOf(Count));
    CopyBytes(Count * BytesPerUnit);
  end;

begin
  case NextValue of
    vaNull, vaFalse, vaTrue, vaNil:
      Writer.WriteValue(ReadValue);
    vaList, vaCollection:
      CopyList;
    vaInt8, vaInt16, vaInt32:
      Writer.WriteInteger(ReadInteger);
    vaExtended:
      Writer.WriteFloat(ReadFloat);
    vaString:
      Writer.WriteString(ReadString);
    vaIdent:
      Writer.WriteIdent(ReadIdent);
    vaBinary, vaLString, vaUTF8String:
      CopyBinary(1);
    vaWString:
      CopyBinary(Sizeof(WideChar));
    vaSet:
      CopySetBody;
    vaSingle:
      Writer.WriteSingle(ReadSingle);
    vaCurrency:
      Writer.WriteCurrency(ReadCurrency);
    vaDate:
      Writer.WriteDate(ReadDate);
    vaInt64:
      Writer.WriteInteger(ReadInt64);
    vaDouble:
      Writer.WriteDouble(ReadDouble);
  end;
end;

procedure TReader.SkipProperty;
begin
  ReadStr; { Skips property name }
  SkipValue;
end;

procedure TReader.SkipComponent(SkipHeader: Boolean);
var
  Flags: TFilerFlags;
  Position: Integer;
begin
  if SkipHeader then
  begin
    ReadPrefix(Flags, Position);
    ReadStr;
    ReadStr;
  end;
  while not EndOfList do SkipProperty;
  ReadListEnd;
  while not EndOfList do SkipComponent(True);
  ReadListEnd;
end;

function TReader.FindAncestorComponent(const Name: string;
  ComponentClass: TPersistentClass): TComponent;
var
  CompName: string;
begin
  CompName := Name;
  Result := nil;
  if FLookupRoot <> nil then
    Result := FLookupRoot.FindComponent(CompName);
  if Result = nil then
  begin
    if Assigned(FOnAncestorNotFound) then
      FOnAncestorNotFound(Self, CompName, ComponentClass, Result);
    if Result = nil then
      raise EReadError.CreateResFmt(@SAncestorNotFound, [CompName]);
  end;
end;

procedure TReader.ReferenceName(var Name: string);
begin
  if Assigned(FOnReferenceName) then FOnReferenceName(Self, Name);
end;

procedure TReader.SetName(Component: TComponent; var Name: string);
begin
  if Assigned(FOnSetName) then FOnSetName(Self, Component, Name);
  Component.Name := Name;
end;

function TReader.FindComponentClass(const ClassName: string): TComponentClass;
begin
  TPersistentClass(Result) := GetFieldClass(Root, ClassName);
  if not Assigned(Result) and Assigned(FLookupRoot) and (FLookupRoot <> Root) then
    TPersistentClass(Result) := GetFieldClass(FLookupRoot, ClassName);
  if Assigned(FOnFindComponentClass) then
    FOnFindComponentClass(Self, ClassName, Result);
  if (Result = nil) or not Result.InheritsFrom(TComponent) then
    ClassNotFound(ClassName);
end;

procedure TReader.SkipBytes(Count: Integer);
const
  SizeOfBytes = 256;
var
  Bytes: TBytes; //array[0..255] of Char;
begin
  SetLength(Bytes, SizeOfBytes);
  while Count > 0 do
    if Count > SizeOfBytes then
    begin
      Read(Bytes, SizeOfBytes);
      Dec(Count, SizeOfBytes);
    end
    else
    begin
      Read(Bytes, Count);
      Count := 0;
    end;
end;

function TReader.ReadVariant: Variant;

  function ReadCustomVariant: Variant;
  var
    OuterStream, InnerStream: TMemoryStream;
    OuterReader: TReader;
    StreamSize: Integer;
    CustomType: TCustomVariantType;
    CustomTypeClassName: string;
    VarStreamer: IVarStreamable;
  begin
    CheckValue(vaBinary);

    InnerStream := nil;
    OuterStream := TMemoryStream.Create;
    try
      InnerStream := TMemoryStream.Create;

      Read(StreamSize, SizeOf(StreamSize));
      OuterStream.Size := StreamSize;
      Read(OuterStream.Memory^, StreamSize);

      OuterReader := TReader.Create(OuterStream, 1024);
      try
        CustomTypeClassName := OuterReader.ReadString;
        OuterReader.Read(StreamSize, SizeOf(StreamSize));
        InnerStream.Size := StreamSize;
        OuterReader.Read(InnerStream.Memory^, StreamSize);

        if not FindCustomVariantType(CustomTypeClassName, CustomType) or
           not Supports(CustomType, IVarStreamable, VarStreamer) then
          raise EReadError.CreateRes(@SReadError);
        TVarData(Result).VType := CustomType.VarType;
        VarStreamer.StreamIn(TVarData(Result), InnerStream);
      finally
        OuterReader.Free;
      end;
    finally
      InnerStream.Free;
      OuterStream.Free;
    end;
  end;

begin
  VarClear(Result);
  case NextValue of
    vaNil, vaNull:       if ReadValue <> vaNil then
                           Result := System.Variants.Null;
    vaInt8:              Result := Shortint(ReadInteger);
    vaInt16:             Result := Smallint(ReadInteger);
    vaInt32:             Result := ReadInteger;
    vaExtended:          Result := ReadFloat;
    vaSingle:            Result := ReadSingle;
    vaDouble:            Result := ReadDouble;
    vaCurrency:          Result := ReadCurrency;
    vaDate:              Result := ReadDate;
    vaString, vaLString: Result := ReadString;
    vaWString,
    vaUTF8String:        Result := ReadString;
    vaFalse, vaTrue:     Result := (ReadValue = vaTrue);
    vaBinary:            Result := ReadCustomVariant;
    vaInt64:             Result := ReadInt64;
  else
    raise EReadError.CreateRes(@SReadError);
  end;
end;

{ TWriter }

destructor TWriter.Destroy;
begin
  WriteBuffer;
  inherited Destroy;
end;

procedure TWriter.AddAncestor(Component: TComponent);
begin
  FAncestorList.Add(Component);
end;

procedure TWriter.EnsureAtLeast(Amount: Integer);
begin
  if FBufPos + Amount > Length(FBuffer) then
    WriteBuffer;
end;

procedure TWriter.DefineProperty(const Name: string;
  ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
begin
  if HasData and Assigned(WriteData) then
  begin
    WritePropName(Name);
    WriteData(Self);
  end;
end;

procedure TWriter.DefineBinaryProperty(const Name: string;
  ReadData, WriteData: TStreamProc; HasData: Boolean);
begin
  if HasData and Assigned(WriteData) then
  begin
    WritePropName(Name);
    WriteBinary(WriteData);
  end;
end;

function TWriter.GetPosition: NativeInt;
begin
  Result := FStream.Position + FBufPos;
end;

function TWriter.FindMethodName(AMethod: TMethod): string;
begin
  Result := '';
  if Assigned(FOnFindMethodName) then
    FOnFindMethodName(Self, AMethod, Result);
  if Result = '' then
    Result := FLookupRoot.MethodName(AMethod.Code);
end;

procedure TWriter.FlushBuffer;
begin
  WriteBuffer;
end;

procedure TWriter.SetPosition(Value: NativeInt);
var
  StreamPosition: NativeInt;
begin
  StreamPosition := FStream.Position;
  { Only flush the buffer if the repostion is outside the buffer range }
  if (Value < StreamPosition) or (Value > StreamPosition + FBufPos) then
  begin
    WriteBuffer;
    FStream.Position := Value;
  end
  else
    FBufPos := Value - StreamPosition;
end;

procedure TWriter.SetRoot(Value: TComponent);
begin
  inherited SetRoot(Value);
  FLookupRoot := Value;
end;

procedure TWriter.Write(const Buffer; Count: NativeInt);
var
  LShouldWrite,
  BufOffset: NativeInt;
begin
  BufOffset := 0;
  while Count > 0 do
  begin
    { Check the size of the output buffer and flush it if its full }
    LShouldWrite := FBufSize - FBufPos;
    if LShouldWrite = 0 then
    begin
      WriteBuffer();
      LShouldWrite := FBufSize;
    end;

    { Update write size }
    if LShouldWrite >= Count then
      LShouldWrite := Count;

    { Move the data in and update buffer pointer }
    Move(Pointer(PByte(@Buffer) + BufOffset)^, Pointer(PByte(FBuffer) + FBufPos)^, LShouldWrite);
    Inc(FBufPos, LShouldWrite);
    Inc(BufOffset, LShouldWrite);

    { Update left count }
    Dec(Count, LShouldWrite);
  end;
end;

procedure TWriter.WriteBinary(WriteData: TStreamProc);
var
  Stream: TMemoryStream;
// TODO -cELBRUS_LONGINT64 : Verify TReader.CopyValue.CopyBinary (change LongInt to Int32)
  Count: Int32;
begin
  Stream := TMemoryStream.Create;
  try
    WriteData(Stream);
    WriteValue(vaBinary);
    Count := Stream.Size;
    Write(Count, SizeOf(Count));
    Write(Stream.Memory^, Count);
  finally
    Stream.Free;
  end;
end;

procedure TWriter.WriteBuffer;
begin
  FStream.WriteBuffer(FBuffer, FBufPos);
  FBufPos := 0;
end;

procedure TWriter.Write(Buffer: TBytes; Offset, Count: NativeInt);
var
  P, C, S, I: NativeInt;
begin
  P := Offset;
  if Offset + Count > High(Buffer) + 1 then
  begin
    C := High(Buffer) - Offset + 1;
    S := Count - C;
    Count := C;
  end
  else
    S := 0;
  while Count > 0 do
  begin
    C := Length(FBuffer) - FBufPos;
    if C <= 0 then
    begin
      WriteBuffer;
      C := Length(FBuffer);
    end;
    if C > Count then
      C := Count;
    System.Move(Buffer[P], FBuffer[FBufPos], C);
    Inc(P, C);
    Inc(FBufPos, C);
    Dec(Count, C);
  end;
  while S > 0 do
  begin
    C := Length(FBuffer) - FBufPos;
    if C <= 0 then
    begin
      WriteBuffer;
      C := Length(FBuffer);
    end;
    if C > S then
      C := S;
    for I := 0 to C - 1 do
      FBuffer[FBufPos + I] := 0;
    Inc(FBufPos, C);
    Dec(S, C);
  end;
end;

procedure TWriter.Write(Buffer: TBytes; Count: NativeInt);
begin
  Write(Buffer, 0, Count);
end;

{$IFNDEF NEXTGEN}
//procedure TWriter.Write(const Buffer: Pointer; Count: Longint);
//begin
//  Write(BytesOf(Buffer, Count), 0, Count);
//end;

procedure TWriter.WriteVar(const Buffer: AnsiChar; Count: NativeInt);
begin
  EnsureAtLeast(1);
  FBuffer[FBufPos] := Byte(Buffer);
  if Count > 1 then
  begin
    Inc(FBufPos, 1);
    WriteVar(Integer(0), Count - 1);
    Exit;
  end;
  Inc(FBufPos, Count);
end;
{$ENDIF !NEXTGEN}

procedure TWriter.WriteVar(const Buffer: Char; Count: NativeInt);
begin
  EnsureAtLeast(2);
  FBuffer[FBufPos] := Byte(Word(Buffer) and $FF);
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Byte((Word(Buffer) shr 8) and $FF);
    if Count > 2 then
    begin
      Inc(FBufPos, 2);
      WriteVar(Integer(0), Count - 2);
      Exit;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: Int8; Count: NativeInt);
begin
  EnsureAtLeast(1);
  FBuffer[FBufPos] := Byte(Buffer and $FF);
  if Count > 1 then
  begin
    Inc(FBufPos, 1);
    WriteVar(Integer(0), Count - 1);
    Exit;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: UInt8; Count: NativeInt);
begin
  EnsureAtLeast(1);
  FBuffer[FBufPos] := Byte(Buffer and $FF);
  if Count > 1 then
  begin
    Inc(FBufPos, 1);
    WriteVar(Integer(0), Count - 1);
    Exit;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: Int16; Count: NativeInt);
begin
  EnsureAtLeast(2);
  FBuffer[FBufPos] := Byte(Buffer and $FF);
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Byte((Buffer shr 8) and $FF);
    if Count > 2 then
    begin
      Inc(FBufPos, 2);
      WriteVar(Integer(0), Count - 2);
      Exit;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: UInt16; Count: NativeInt);
begin
  EnsureAtLeast(2);
  FBuffer[FBufPos] := Byte(Buffer and $FF);
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Byte((Buffer shr 8) and $FF);
    if Count > 2 then
    begin
      Inc(FBufPos, 2);
      WriteVar(Integer(0), Count - 2);
      Exit;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: Int32; Count: NativeInt);
begin
  EnsureAtLeast(4);
  FBuffer[FBufPos] := Byte(Buffer and $FF);
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Byte((Buffer shr 8) and $FF);
    if Count > 2 then
    begin
      FBuffer[FBufPos + 2] := Byte((Buffer shr 16) and $FF);
      if Count > 3 then
      begin
        FBuffer[FBufPos + 3] := Byte((Buffer shr 24) and $FF);
        if Count > 4 then
        begin
          Inc(FBufPos, 4);
          WriteVar(Integer(0), Count - 4);
          Exit;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: UInt32; Count: NativeInt);
begin
  EnsureAtLeast(4);
  FBuffer[FBufPos] := Byte(Buffer and $FF);
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Byte((Buffer shr 8) and $FF);
    if Count > 2 then
    begin
      FBuffer[FBufPos + 2] := Byte((Buffer shr 16) and $FF);
      if Count > 3 then
      begin
        FBuffer[FBufPos + 3] := Byte((Buffer shr 24) and $FF);
        if Count > 4 then
        begin
          Inc(FBufPos, 4);
          WriteVar(Integer(0), Count - 4);
          Exit;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: Int64; Count: NativeInt);
begin
  EnsureAtLeast(8);
  FBuffer[FBufPos] := Byte(Buffer and $FF);
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Byte((Buffer shr 8) and $FF);
    if Count > 2 then
    begin
      FBuffer[FBufPos + 2] := Byte((Buffer shr 16) and $FF);
      if Count > 3 then
      begin
        FBuffer[FBufPos + 3] := Byte((Buffer shr 24) and $FF);
        if Count > 4 then
        begin
          FBuffer[FBufPos + 4] := Byte((Buffer shr 32) and $FF);
          if Count > 5 then
          begin
            FBuffer[FBufPos + 5] := Byte((Buffer shr 40) and $FF);
            if Count > 6 then
            begin
              FBuffer[FBufPos + 6] := Byte((Buffer shr 48) and $FF);
              if Count > 7 then
              begin
                FBuffer[FBufPos + 7] := Byte((Buffer shr 56) and $FF);
                if Count > 8 then
                begin
                  Inc(FBufPos, 8);
                  WriteVar(Integer(0), Count - 8);
                  Exit;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: UInt64; Count: NativeInt);
begin
  EnsureAtLeast(8);
  FBuffer[FBufPos] := Byte(Buffer and $FF);
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Byte((Buffer shr 8) and $FF);
    if Count > 2 then
    begin
      FBuffer[FBufPos + 2] := Byte((Buffer shr 16) and $FF);
      if Count > 3 then
      begin
        FBuffer[FBufPos + 3] := Byte((Buffer shr 24) and $FF);
        if Count > 4 then
        begin
          FBuffer[FBufPos + 4] := Byte((Buffer shr 32) and $FF);
          if Count > 5 then
          begin
            FBuffer[FBufPos + 5] := Byte((Buffer shr 40) and $FF);
            if Count > 6 then
            begin
              FBuffer[FBufPos + 6] := Byte((Buffer shr 48) and $FF);
              if Count > 7 then
              begin
                FBuffer[FBufPos + 7] := Byte((Buffer shr 56) and $FF);
                if Count > 8 then
                begin
                  Inc(FBufPos, 8);
                  WriteVar(Integer(0), Count - 8);
                  Exit;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: Single; Count: NativeInt);
begin
  EnsureAtLeast(4);
  FBuffer[FBufPos] := Buffer.Bytes[0];
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Buffer.Bytes[1];
    if Count > 2 then
    begin
      FBuffer[FBufPos + 2] := Buffer.Bytes[2];
      if Count > 3 then
      begin
        FBuffer[FBufPos + 3] := Buffer.Bytes[3];
        if Count > 4 then
        begin
          Inc(FBufPos, 4);
          WriteVar(Integer(0), Count - 4);
          Exit;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: Double; Count: NativeInt);
begin
  EnsureAtLeast(8);
  FBuffer[FBufPos] := Buffer.Bytes[0];
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Buffer.Bytes[1];
    if Count > 2 then
    begin
      FBuffer[FBufPos + 2] := Buffer.Bytes[2];
      if Count > 3 then
      begin
        FBuffer[FBufPos + 3] := Buffer.Bytes[3];
        if Count > 4 then
        begin
          FBuffer[FBufPos + 4] := Buffer.Bytes[4];
          if Count > 5 then
          begin
            FBuffer[FBufPos + 5] := Buffer.Bytes[5];
            if Count > 6 then
            begin
              FBuffer[FBufPos + 6] := Buffer.Bytes[6];
              if Count > 7 then
              begin
                FBuffer[FBufPos + 7] := Buffer.Bytes[7];
                if Count > 8 then
                begin
                  Inc(FBufPos, 8);
                  WriteVar(Integer(0), Count - 8);
                  Exit;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteVar(const Buffer: Extended; Count: NativeInt);
begin
{$IFDEF EXTENDEDHAS10BYTES}
  WriteVar(TExtended80Rec(Buffer), Count);
{$ELSE !EXTENDEDHAS10BYTES}
  WriteVar(Double(Buffer), Count);
{$ENDIF !EXTENDEDHAS10BYTES}
end;

procedure TWriter.WriteVar(const Buffer: TExtended80Rec; Count: NativeInt);
begin
  EnsureAtLeast(10);
  FBuffer[FBufPos] := Buffer.Bytes[0];
  if Count > 1 then
  begin
    FBuffer[FBufPos + 1] := Buffer.Bytes[1];
    if Count > 2 then
    begin
      FBuffer[FBufPos + 2] := Buffer.Bytes[2];
      if Count > 3 then
      begin
        FBuffer[FBufPos + 3] := Buffer.Bytes[3];
        if Count > 4 then
        begin
          FBuffer[FBufPos + 4] := Buffer.Bytes[4];
          if Count > 5 then
          begin
            FBuffer[FBufPos + 5] := Buffer.Bytes[5];
            if Count > 6 then
            begin
              FBuffer[FBufPos + 6] := Buffer.Bytes[6];
              if Count > 7 then
              begin
                FBuffer[FBufPos + 7] := Buffer.Bytes[7];
                if Count > 8 then
                begin
                  FBuffer[FBufPos + 8] := Buffer.Bytes[8];
                  if Count > 9 then
                  begin
                    FBuffer[FBufPos + 9] := Buffer.Bytes[9];
                    if Count > 10 then
                    begin
                      Inc(FBufPos, 10);
                      WriteVar(Integer(0), Count - 10);
                      Exit;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Inc(FBufPos, Count);
end;

procedure TWriter.WriteBoolean(Value: Boolean);
begin
  if Value then
    WriteValue(vaTrue)
  else
    WriteValue(vaFalse);
end;

procedure TWriter.WriteChar(Value: Char);
begin
  WriteString(Value);
end;

{$IFNDEF NEXTGEN}
procedure TWriter.WriteWideChar(Value: WideChar);
begin
  WriteString(Value);
end;
{$ENDIF !NEXTGEN}

procedure TWriter.WriteCollection(const Value: TCollection);
var
  I: Integer;
  OldAncestor: TPersistent;
begin
  OldAncestor := Ancestor;
  Ancestor := nil;
  try
    WriteValue(vaCollection);
    if Value <> nil then
      for I := 0 to Value.Count - 1 do
      begin
        WriteListBegin;
        WriteProperties(Value.Items[I]);
        WriteListEnd;
      end;
    WriteListEnd;
  finally
    Ancestor := OldAncestor;
  end;
end;

procedure TWriter.WriteComponent(Component: TComponent);

  function FindAncestor(const Name: string): TComponent;
  var
    I: Integer;
  begin
    for I := 0 to FAncestorList.Count - 1 do
    begin
      Result := FAncestorList[I];
      if SameText(Result.Name, Name) then Exit;
    end;
    Result := nil;
  end;

var
  OldAncestor: TPersistent;
  OldRootAncestor: TComponent;
  AncestorComponent: TComponent;
  I: Integer;
begin
  OldAncestor := Ancestor;
  OldRootAncestor := RootAncestor;
  try
    Include(Component.FComponentState, csWriting);
    for I := 0 to Component.ComponentCount - 1 do
      if csSubComponent in Component.Components[I].ComponentStyle then
        Include(Component.Components[I].FComponentState, csWriting);
    if Assigned(FAncestorList) then
      Ancestor := FindAncestor(Component.Name);
    if Assigned(FOnFindAncestor) and ((Ancestor = nil) or
    (Ancestor is TComponent)) then
    begin
      AncestorComponent := TComponent(Ancestor);
      FOnFindAncestor(Self, Component, Component.Name, AncestorComponent,
        FRootAncestor);
      Ancestor := AncestorComponent;
    end;
    Component.WriteState(Self);
    Exclude(Component.FComponentState, csWriting);
    for I := 0 to Component.ComponentCount - 1 do
      if csSubComponent in Component.Components[I].ComponentStyle then
        Exclude(Component.Components[I].FComponentState, csWriting);
  finally
    Ancestor := OldAncestor;
    FRootAncestor := OldRootAncestor;
  end;
end;

procedure TWriter.WriteData(Instance: TComponent);
var
  PreviousPosition, PropertiesPosition: NativeInt;
  OldAncestorList: TList<TComponent>;
  OldAncestorPos, OldChildPos: NativeInt;
  OldRoot, OldRootAncestor: TComponent;
  Flags: TFilerFlags;
begin
  if FBufSize - FBufPos < Length(Instance.ClassName) +
    Length(Instance.Name) + 1+5+3 then WriteBuffer;
     { Prefix + vaInt + integer + 2 end lists }
  PreviousPosition := Position;
  Flags := [];
  if csInline in Instance.ComponentState then
    if (Ancestor <> nil) and (csAncestor in Instance.ComponentState) and (FAncestorList <> nil) then
      // If the AncestorList is not nil, this really came from an ancestor form
      Include(Flags, ffInherited)
    else
      // otherwise the Ancestor is the original frame
      Include(Flags, ffInline)
  else if Ancestor <> nil then
    Include(Flags, ffInherited);
  if (FAncestorList <> nil) and (FAncestorPos < FAncestorList.Count) and
    ((Ancestor = nil) or (FAncestorList[FAncestorPos] <> Ancestor)) then
    Include(Flags, ffChildPos);
  WritePrefix(Flags, FChildPos);
  if UseQualifiedNames then
    WriteUTF8Str(Instance.ClassType.UnitName + '.' + Instance.ClassName)
  else
    WriteUTF8Str(Instance.ClassName);
  WriteUTF8Str(Instance.Name);
  PropertiesPosition := Position;
  if (FAncestorList <> nil) and (FAncestorPos < FAncestorList.Count) then
  begin
    if Ancestor <> nil then Inc(FAncestorPos);
    Inc(FChildPos);
  end;
  WriteProperties(Instance);
  WriteListEnd;
  OldAncestorList := FAncestorList;
  OldAncestorPos := FAncestorPos;
  OldChildPos := FChildPos;
  OldRoot := FRoot;
  OldRootAncestor := FRootAncestor;
  try
    FAncestorList := nil;
    FAncestorPos := 0;
    FChildPos := 0;
    if not IgnoreChildren then
      try
        if (FAncestor <> nil) and (FAncestor is TComponent) then
        begin
          if (FAncestor is TComponent) and (csInline in TComponent(FAncestor).ComponentState) then
            FRootAncestor := TComponent(FAncestor);
          FAncestorList := TList<TComponent>.Create;
          TComponent(FAncestor).GetChildren(AddAncestor, FRootAncestor);
        end;
        if csInline in Instance.ComponentState then
          FRoot := Instance;
        Instance.GetChildren(WriteComponent, FRoot);
      finally
        FAncestorList.Free;
      end;
  finally
    FAncestorList := OldAncestorList;
    FAncestorPos := OldAncestorPos;
    FChildPos := OldChildPos;
    FRoot := OldRoot;
    FRootAncestor := OldRootAncestor;
  end;
  WriteListEnd;
  if (Instance <> Root) and (Flags = [ffInherited]) and
    (Position = PropertiesPosition + (1 + 1)) then { (1 + 1) is two end lists }
    Position := PreviousPosition;
end;

procedure TWriter.WriteDescendent(const Root: TComponent; const AAncestor: TComponent);
var
  Context: TRttiContext;
begin
  FRootAncestor := AAncestor;
  FAncestor := AAncestor;
  FRoot := Root;
  FLookupRoot := Root;
  WriteSignature;
  Context := TRttiContext.Create;
  try
    WriteComponent(Root);
  finally
    Context.Free;
  end;
end;

procedure TWriter.WriteFloat(const Value: Extended);
{$IFDEF EXTENDEDIS10BYTES}
begin
  WriteValue(vaExtended);
  Write(Value, SizeOf(Extended));
end;
{$ELSE !EXTENDEDIS10BYTES}
var
  E: TExtended80Rec;
begin
  WriteValue(vaExtended);
  E := TExtended80Rec(Value);
  Write(E, SizeOf(E));
end;
{$ENDIF !EXTENDEDIS10BYTES}

procedure TWriter.WriteSingle(const Value: Single);
begin
  WriteValue(vaSingle);
  Write(Value, SizeOf(Single));
end;

procedure TWriter.WriteDouble(const Value: Double);
begin
  WriteValue(vaDouble);
  Write(Value, SizeOf(Double));
end;

procedure TWriter.WriteCurrency(const Value: Currency);
var
  Buf: UInt64;
begin
  Buf := PUint64(@Value)^;
  WriteValue(vaCurrency);
  Write(Buf, SizeOf(UInt64));
end;

procedure TWriter.WriteDate(const Value: TDateTime);
var
  D: Double;
begin
  D := Value;
  WriteValue(vaDate);
  Write(D, SizeOf(Double));
end;

procedure TWriter.WriteIdent(const Ident: string);
begin
  if SameText(Ident, 'False') then WriteValue(vaFalse) else
  if SameText(Ident ,'True') then WriteValue(vaTrue) else
  if SameText(Ident ,'Null') then WriteValue(vaNull) else
  if SameText(Ident, 'nil') then WriteValue(vaNil) else
  begin
    WriteValue(vaIdent);
    WriteUTF8Str(Ident);
  end;
end;

// TODO -cELBRUS_LONGINT64 : Verify TWriter.WriteInteger (change LongInt to Integer)
procedure TWriter.WriteInteger(Value: Integer);
begin
  if (Value >= Low(ShortInt)) and (Value <= High(ShortInt)) then
  begin
    WriteValue(vaInt8);
    Write(Value, SizeOf(Shortint));
  end else
  if (Value >= Low(SmallInt)) and (Value <= High(SmallInt)) then
  begin
    WriteValue(vaInt16);
    Write(Value, SizeOf(Smallint));
  end
  else
  begin
    WriteValue(vaInt32);
    Write(Value, SizeOf(Integer));
  end;
end;

procedure TWriter.WriteInteger(Value: Int64);
begin
// TODO -cELBRUS_LONGINT64 : Verify TWriter.WriteInteger (change LongInt to Integer)
  if (Value >= Low(Integer)) and (Value <= High(Integer)) then
    WriteInteger(Integer(Value))
  else
  begin
    WriteValue(vaInt64);
    Write(Value, Sizeof(Int64));
  end;
end;

procedure TWriter.WriteListBegin;
begin
  WriteValue(vaList);
end;

procedure TWriter.WriteListEnd;
begin
  WriteValue(vaNull);
end;

procedure TWriter.WritePrefix(Flags: TFilerFlags; AChildPos: Integer);
var
  Prefix: Byte;
begin
  if Flags <> [] then
  begin
    Prefix := $F0 or Byte(Flags);
    Write(Prefix, SizeOf(Prefix));
    if ffChildPos in Flags then WriteInteger(AChildPos);
  end;
end;

procedure TWriter.WriteProperties(const Instance: TPersistent);
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
begin
  Count := GetTypeData(Instance.ClassInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropInfos(Instance.ClassInfo, PropList);
      for I := 0 to Count - 1 do
      begin
        PropInfo := PropList^[I];
        if PropInfo = nil then
          Break;
        if IsStoredProp(Instance, PropInfo) then
          WriteProperty(Instance, PropInfo);
      end;
    finally
      FreeMem(PropList, Count * SizeOf(Pointer));
    end;
  end;
  Instance.DefineProperties(Self);
end;

function AncestorIsValid(const Ancestor: TPersistent; const Root, RootAncestor: TComponent): Boolean;
begin
  Result := (Ancestor <> nil) and (RootAncestor <> nil) and
            Root.InheritsFrom(RootAncestor.ClassType);
end;

// P points a length field of ShortString.
function AfterString(const P: PByte): Pointer; inline;
begin
  Result := P + P^ + 1;
end;

function HasCustomAttribute(const Instance: TObject; const PropInfo: PPropInfo): Boolean;
var
  T: PTypeInfo;
  D: PTypeData;
  C1, C2: Word;
  P: PByte;
begin
  T := PTypeInfo(Instance.ClassInfo);

  while T <> nil do
  begin
    D := T.TypeData;
    P := AfterString(@ D.UnitName );

    C1 := PWord(P)^; Inc(P,2);// PropCount;
    while C1 > 0 do
    begin
      P := AfterString(@PPropInfo(P)^.Name);
      Dec(C1);
    end;
    C1 := PWord(P)^; Inc(P,2);// PropDataEx.PropCount
    while C1 > 0 do
    begin
      if PPropInfoEx(P)^.Info.NameFld = PropInfo.NameFld then
      begin
        P := @PPropInfoEx(P)^.AttrData; // PropDataEx.PropList[C1].AttrData
        C2 := PWord(P)^; // TAttrData.Len
        // find target property
        if C2 <> 2 then // Found CustomAttribute
          Exit (True)
        // Continue to search
      end
      else
      begin
        P := @PPropInfoEx(P)^.AttrData; // PropDataEx.PropList[C1].AttrData
        C2 := PWord(P)^; // TAttrData.Len
      end;
      Inc(P,C2);
      Dec(C1);
    end;
    if D^.ParentInfo = nil then
      Break;
    T := D^.ParentInfo^;
  end;
  Result := False;
end;

function IsDefaultPropertyValue(const Instance: TObject; PropInfo: PPropInfo;
  OnGetLookupInfo: TGetLookupInfoEvent; Writer: TWriter = nil;
  OnFindMethodName: TFindMethodNameEvent = nil): Boolean;
var
  PropType: PTypeInfo;
  Ancestor: TPersistent;
  LookupRoot: TComponent;
  RootAncestor: TComponent;
  Root: TComponent;
  AncestorValid: Boolean;

  function FindProperty(var t: TRttiType): TRttiProperty;
  var
    R: TRttiProperty;
  begin
    while t <> nil do
    begin
      for R in t.GetDeclaredProperties do
      begin
        if TRttiInstanceProperty(R).PropInfo.Name = PropInfo.Name then
          exit(R);
      end;
      t := t.BaseType;
    end;
    Result := nil;
  end;

  function GetDefaultAttribute(out Default: Variant): Boolean;
  var
    RttiType: TRttiType;
    Context: TRttiContext;
    Attribute: TCustomAttribute;
    aRttiProperty: TRttiProperty;
  begin
    Result := False;
    Default := System.Variants.Null;
    if HasCustomAttribute(Instance, PropInfo) then
    begin
      Context := TRttiContext.Create;
      try
        RttiType := Context.GetType(Instance.ClassInfo);
        while RttiType <> nil do
        begin
          aRttiProperty := FindProperty(RttiType);
          if aRttiProperty <> nil then
          begin
            for Attribute in aRttiProperty.GetAttributes do
            begin
              if Attribute is TDefaultAttributeBase then
              begin
                Result := True;
                Default := TDefaultAttributeBase(Attribute).Value;
                Exit;
              end;
            end;
          end;
          if RttiType <> nil then
            RttiType := RttiType.BaseType;
        end;
      finally
        Context.Free;
      end;
    end;
  end;

  function IsDefaultOrdProp: Boolean;
  var
// TODO -cELBRUS_LONGINT64 : Verify IsDefaultPropertyValue.IsDefaultOrdProp (change LongInt to Integer)
    Value: Integer;
    Temp: variant;
    Default: Integer;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetOrdProp(Ancestor, PropInfo)
    else
    begin
      if GetDefaultAttribute(Temp) then
      begin
        Result := (Not VarIsNull(Temp)) and (Value = Temp);
      end
      else
      begin
        Default := PPropInfo(PropInfo)^.Default;
        Result := (Default <> Integer($80000000)) and (Value = Default);
      end;
    end;
  end;

  function IsDefaultFloatProp: Boolean;
  var
    Value: Extended;
    Temp: variant;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetFloatProp(Ancestor, PropInfo)
    else
    begin
      if GetDefaultAttribute(Temp) then
        Result := (Not VarIsNull(Temp)) and (Value = Temp)
      else
// TODO -cELBRUS_LONGINT64 : Verify IsDefaultPropertyValue.IsDefaultFloatProp (change LongInt to Integer)
        Result := (PPropInfo(PropInfo)^.Default <> Integer($80000000)) and (Value = 0);
    end;
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
    Temp: variant;
// TODO -cELBRUS_LONGINT64 : Verify IsDefaultPropertyValue.IsDefaultFloatProp (change LongInt to Integer)
    Default: Integer;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetInt64Prop(Ancestor, PropInfo)
    else
    begin
      if GetDefaultAttribute(Temp) then
        Result := (Not VarIsNull(Temp)) and (Value = Temp)
      else
      begin
        Default := PPropInfo(PropInfo)^.Default;
        Result := (Default <> Integer($80000000)) and (Value = Int64(Default));
      end;
    end;
  end;

  function IsDefaultStrProp: Boolean;
  var
    Value: String;
    Temp: variant;
  begin
    Value := GetStrProp(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetStrProp(Ancestor, PropInfo)
    else
    begin
      if GetDefaultAttribute(Temp) then
        Result := (Not VarIsNull(Temp)) and (Value = Temp)
      else
// TODO -cELBRUS_LONGINT64 : Verify IsDefaultPropertyValue.IsDefaultStrProp (change LongInt to Integer)
        Result := (PPropInfo(PropInfo)^.Default <> Integer($80000000)) and (Value = '');
    end;
  end;

  function ObjectAncestorMatch(const AncestorValue, Value: TComponent): Boolean;
  begin
    Result := (AncestorValue <> nil) and (AncestorValue.Owner = RootAncestor) and
      (Value <> nil) and (Value.Owner = Root) and
      SameText(AncestorValue.Name, Value.Name);
  end;

  function IsDefaultObjectProp: Boolean;
  var
    Value: TObject;

    function IsDefault: Boolean;
    var
      AncestorValue: TObject;
    begin
      AncestorValue := nil;
      if AncestorValid then
      begin
        AncestorValue := TObject(GetOrdProp(Ancestor, PropInfo));
        if ObjectAncestorMatch(TComponent(AncestorValue), TComponent(Value)) then
          AncestorValue := Value;
      end;
      Result := Value = AncestorValue;
    end;

  begin
    Result := True;
    Value := TObject(GetOrdProp(Instance, PropInfo));
    if (Value = nil) and not IsDefault then
    begin
      Result := False; // nil wasn't the "default" value
    end
    else if Value is TPersistent then
    begin
      if (Value is TComponent) and
        not (csSubComponent in TComponent(Value).ComponentStyle) then
      begin
        if not IsDefault then
        begin
          // A non sub-component TComponent is only non-default if
          // it actually has a name (that way, it can be streamed out -
          // it can't be streamed without a name).
          if TComponent(Value).Name <> '' then
            Result := False;
        end
      end else
      begin
        Result := False; // The TPersistent should be checked for default's by the caller
      end;
    end;
  end;

  function IsDefaultInterfaceProp: Boolean;
  var
    Intf: IInterface;
    Value: TComponent;

    function IsDefaultValue: Boolean;
    var
      AncestorIntf: IInterface;
      ASR: IInterfaceComponentReference;
    begin
      Result := Intf = nil;
      if AncestorValid then
      begin
        AncestorIntf := GetInterfaceProp(Ancestor, PropInfo);
        Result := Intf = AncestorIntf;
        if not Result then
        begin
          if Supports(AncestorIntf, IInterfaceComponentReference, ASR) then
            Result := ObjectAncestorMatch(ASR.GetComponent, Value);
        end;
      end;
    end;

  var
    SR: IInterfaceComponentReference;
  begin
    Result := True;
    Intf := GetInterfaceProp(Instance, PropInfo);
    if (Intf = nil) or (not Supports(Intf, IInterfaceComponentReference, SR)) then
    begin
      if AncestorValid and (GetInterfaceProp(Ancestor, PropInfo) <> nil) then
        Result := False;
    end
    else
    begin
      Value := SR.GetComponent;
      if not IsDefaultValue then
      begin
        // We can only stream out components (ie: non-default ones)
        // if they actually have a name
        if Value.Name <> '' then
          Result := False;
      end;
    end;
  end;

  function FindMethodName(AMethod: TMethod): string;
  begin
    Result := '';
    if Assigned(OnFindMethodName) then
      OnFindMethodName(Writer, AMethod, Result);
    if Result = '' then
      Result := LookupRoot.MethodName(AMethod.Code);
  end;

  function IsDefaultMethodProp: Boolean;
  var
    Value: TMethod;
    DefaultCode: Pointer;
  begin
    Value := GetMethodProp(Instance, PropInfo);
    DefaultCode := nil;
    if AncestorValid then
      DefaultCode := GetMethodProp(Ancestor, PropInfo).Code;
    Result := (Value.Code = DefaultCode) or
      ((Value.Code <> nil) and (FindMethodName(Value) = ''));
  end;

  function IsDefaultVariantProp: Boolean;
  var
    Value: Variant;
    Temp: variant;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    if AncestorValid then
      Result := VarSameValue(Value, GetVariantProp(Ancestor, PropInfo))
    else
    begin
      if GetDefaultAttribute(Temp) then
        Result := (Not VarIsNull(Temp)) and VarIsClear(Value)
      else
// TODO -cELBRUS_LONGINT64 : Verify IsDefaultPropertyValue.IsDefaultVariantProp (change LongInt to Integer)
        Result := (PPropInfo(PropInfo)^.Default <> Integer($80000000)) and VarIsClear(Value);
    end;
  end;

begin
  Ancestor := nil;
  Root := nil;
  LookupRoot := nil;
  RootAncestor := nil;

  if Assigned(OnGetLookupInfo) then
    OnGetLookupInfo(Ancestor, Root, LookupRoot, RootAncestor);

  AncestorValid := AncestorIsValid(Ancestor, Root, RootAncestor);

  Result := True;
  if (PropInfo^.GetProc <> nil) and
     ((PropInfo^.SetProc <> nil) or
     ((PropInfo^.PropType^.Kind = tkClass) and
      (TObject(GetOrdProp(Instance, PropInfo)) is TComponent) and
      (csSubComponent in TComponent(GetOrdProp(Instance, PropInfo)).ComponentStyle))) then
  begin
    PropType := PropInfo^.PropType^;
    case PropType^.Kind of
      tkInteger, tkChar, tkWChar, tkEnumeration, tkSet:
        Result := IsDefaultOrdProp;
      tkFloat:
        Result := IsDefaultFloatProp;
      tkString, tkLString, tkWString, tkUString:
        Result := IsDefaultStrProp;
      tkClass:
        Result := IsDefaultObjectProp;
      tkMethod:
        Result := IsDefaultMethodProp;
      tkVariant:
        Result := IsDefaultVariantProp;
      tkInt64:
        Result := IsDefaultInt64Prop;
      tkInterface:
        Result := IsDefaultInterfaceProp;
    end;
  end;
end;

procedure TWriter.WriteProperty(const Instance: TPersistent; PropInfo: PPropInfo);
var
  PropType: PTypeInfo;
  AncestorValid: Boolean;

  procedure WritePropPath;
  begin
    WritePropName(PropInfo.NameFld.ToString);
  end;

// TODO -cELBRUS_LONGINT64 : Verify TWriter.WriteProperty.WriteSet (change LongInt to Integer)
  procedure WriteSet(Value: Integer);
  var
    I: Integer;
    BaseType: PTypeInfo;
  begin
    BaseType := GetTypeData(PropType)^.CompType^;
    WriteValue(vaSet);
    for I := 0 to SizeOf(TIntegerSet) * 8 - 1 do
      if I in TIntegerSet(Value) then
        WriteUTF8Str(GetSetElementName(BaseType, I));
    WriteUTF8Str('');
  end;

// TODO -cELBRUS_LONGINT64 : Verify TWriter.WriteProperty.WriteIntProp (change LongInt to Integer)
  procedure WriteIntProp(IntType: PTypeInfo; Value: Integer);
  var
    Ident: string;
    IntToIdent: TIntToIdent;
  begin
    IntToIdent := FindIntToIdent(IntType);
    if Assigned(IntToIdent) and IntToIdent(Value, Ident) then
      WriteIdent(Ident)
    else
      WriteInteger(Value);
  end;

  procedure WriteCollectionProp(const Collection: TCollection);
  var
    SavePropPath: string;
  begin
    WritePropPath;
    SavePropPath := FPropPath;
    try
      FPropPath := '';
      WriteCollection(Collection);
    finally
      FPropPath := SavePropPath;
    end;
  end;

  procedure WriteOrdProp;
// TODO -cELBRUS_LONGINT64 : Verify TWriter.WriteProperty.WriteOrdProp (change LongInt to Integer)
  var
    Value: Integer;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    WritePropPath;
    case PropType^.Kind of
      tkInteger:
        WriteIntProp(PPropInfo(PropInfo)^.PropType^, Value);
      tkChar:
        WriteChar(Chr(Value));
      tkWChar:
        WriteChar(WideChar(Value));
      tkSet:
        WriteSet(Value);
      tkEnumeration:
        WriteIdent(GetEnumName(PropType, Value));
    end;
  end;

  procedure WriteFloatProp;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    WritePropPath;
    WriteFloat(Value);
  end;

  procedure WriteInt64Prop;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    WritePropPath;
    WriteInteger(Value);
  end;

  procedure WriteStrProp;
  var
    Value: String;
  begin
    Value := GetStrProp(Instance, PropInfo);
    WritePropPath;
    WriteString(Value);
  end;

  function OwnedBy(Component, Owner: TComponent): Boolean;
  begin
    Result := True;
    while Component <> nil do
      if Component = Owner then
        Exit
      else
        Component := Component.Owner;
    Result := False;
  end;

  function GetComponentValue(const Component: TComponent): string;
  begin
    if Component.Owner = LookupRoot then
      Result := Component.Name
    else if Component = LookupRoot then
      Result := 'Owner'                                                       { Do not translate }
    else if (Component.Owner <> nil) and (Component.Owner.Name <> '') and
      (Component.Name <> '') then
      if OwnedBy(Component.Owner, LookupRoot) then
        Result := GetComponentValue(Component.Owner) + '.' + Component.Name
      else
        Result := Component.Owner.Name + '.' + Component.Name
    else if Component.Name <> '' then
      Result := Component.Name + '.Owner'                                     { Do not translate }
    else Result := '';
  end;

  procedure WriteObjectProp;
  var
    Value: TObject;
    OldAncestor: TPersistent;
    SavePropPath, ComponentValue: string;
  begin
    Value := TObject(GetOrdProp(Instance, PropInfo));
    if Value = nil then
    begin
      WritePropPath;
      WriteValue(vaNil);
    end
    else if Value is TPersistent then
      if (Value is TComponent) and
        not (csSubComponent in TComponent(Value).ComponentStyle) then
      begin
        ComponentValue := GetComponentValue(TComponent(Value));
        // ComponentValue will never be '' since we are to always
        // write out the value (in other words: it is not the default)
        // but it doesn't hurt to check
        if ComponentValue <> '' then
        begin
          WritePropPath;
          WriteIdent(ComponentValue);
        end;
      end else
      begin
        OldAncestor := Ancestor;
        SavePropPath := FPropPath;
        try
          FPropPath := FPropPath + PropInfo.NameFld.ToString + '.';
          if AncestorValid then
            Ancestor := TPersistent(GetOrdProp(Ancestor, PropInfo));
          WriteProperties(TPersistent(Value));
        finally
          Ancestor := OldAncestor;
          FPropPath := SavePropPath;
        end;
        if Value is TCollection then
        begin
          if not AncestorValid or
            not CollectionsEqual(TCollection(Value),
              TCollection(GetOrdProp(Ancestor, PropInfo)), FLookupRoot, FRootAncestor) then
              WriteCollectionProp(TCollection(Value));
        end;
      end;
  end;

  procedure WriteInterfaceProp;
  var
    Intf: IInterface;
    Value: TComponent;
  var
    SR: IInterfaceComponentReference;
    RefStr: String;
  begin
    Intf := GetInterfaceProp(Instance, PropInfo);
    if Intf = nil then
    begin
      WritePropPath;
      WriteValue(vaNil);
    end
    else if Supports(Intf, IInterfaceComponentReference, SR) then
    begin
      Value := SR.GetComponent;
      RefStr := GetComponentValue(Value);
      Assert(RefStr <> '', 'Component reference name should always be non blank');
      WritePropPath;
      WriteIdent(RefStr);
    end;
    // The else case will not happen because we are to always write out the
    // property at this point, so it will be nil, or support the reference
  end;

  procedure WriteMethodProp;
  var
    Value: TMethod;
  begin
    Value := GetMethodProp(Instance, PropInfo);
    WritePropPath;
    if Value.Code = nil then
      WriteValue(vaNil)
    else
      WriteIdent(FindMethodName(Value));
  end;

  procedure WriteVariantProp;
  var
    Value: Variant;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    WritePropPath;
    WriteVariant(Value);
  end;

begin
  // Using IsDefaultPropertyValue will tell us if we should write out
  // a given property because it was different from the default or
  // different from the Ancestor (if applicable).
  if (PropInfo^.GetProc <> nil) and
     ((PropInfo^.SetProc <> nil) or
     ((PropInfo^.PropType^.Kind = tkClass) and
      (TObject(GetOrdProp(Instance, PropInfo)) is TComponent) and
      (csSubComponent in TComponent(GetOrdProp(Instance, PropInfo)).ComponentStyle))) then
  begin
    if not IsDefaultPropertyValue(Instance, PropInfo, GetLookupInfo, Self, FOnFindMethodName) then
    begin
      AncestorValid := AncestorIsValid(Ancestor, Root, RootAncestor);
      PropType := PropInfo^.PropType^;
      case PropType^.Kind of
        tkInteger, tkChar, tkWChar, tkEnumeration, tkSet:
          WriteOrdProp;
        tkFloat:
          WriteFloatProp;
        tkString, tkLString, tkWString, tkUString:
          WriteStrProp;
        tkClass:
          WriteObjectProp;
        tkMethod:
          WriteMethodProp;
        tkVariant:
          WriteVariantProp;
        tkInt64:
          WriteInt64Prop;
        tkInterface:
          WriteInterfaceProp;
      end;
    end;
  end;
end;

procedure TWriter.WriteVariant(const Value: Variant);
var
  CustomType: TCustomVariantType;
  OuterStream, InnerStream: TMemoryStream;
  OuterWriter: TWriter;
  StreamSize: Integer;
  VarStreamer: IVarStreamable;
  LInt64: Int64;
begin
  if VarIsArray(Value) then
    raise EWriteError.CreateRes(@SWriteError);
  case VarType(Value) and varTypeMask of
    varEmpty:
      WriteValue(vaNil);
    varNull:
      WriteValue(vaNull);
    varOleStr:
      WriteString(Value);
    varString:
      WriteString(Value);
    varByte, varShortInt, varWord, varSmallInt, varInteger:
      WriteInteger(Value);
    varSingle:
      WriteSingle(Value);
    varDouble:
      WriteFloat(Value);
    varCurrency:
      WriteCurrency(Value);
    varDate:
      WriteDate(Value);
    varBoolean:
      if Value then
        WriteValue(vaTrue)
      else
        WriteValue(vaFalse);
    varUInt32, varInt64:
      begin
        LInt64 := Value;
        WriteInteger(LInt64);
      end;
  else
    try
      if not FindCustomVariantType(TVarData(Value).VType, CustomType) or
         not Supports(Value, IVarStreamable, VarStreamer) then
        WriteString(Value)
      else
      begin
        InnerStream := nil;
        OuterStream := TMemoryStream.Create;
        try
          InnerStream := TMemoryStream.Create;
          OuterWriter := TWriter.Create(OuterStream, 1024);
          try
            VarStreamer.StreamOut(TVarData(Value), InnerStream);
            StreamSize := InnerStream.Size;

            OuterWriter.WriteString(CustomType.ClassName);
            OuterWriter.Write(StreamSize, SizeOf(StreamSize));
            OuterWriter.Write(InnerStream.Memory^, StreamSize);
          finally
            OuterWriter.Free;
          end;
          StreamSize := OuterStream.Size;
          WriteValue(vaBinary);
          Write(StreamSize, SizeOf(StreamSize));
          Write(OuterStream.Memory^, StreamSize);
        finally
          InnerStream.Free;
          OuterStream.Free;
        end;
      end;
    except
      raise EWriteError.CreateRes(@SWriteError);
    end;
  end;
end;

procedure TWriter.WritePropName(const PropName: string);
begin
  WriteUTF8Str(FPropPath + PropName);
end;

procedure TWriter.WriteRootComponent(const Root: TComponent);
begin
  WriteDescendent(Root, nil);
end;

procedure TWriter.WriteSignature;
begin
  Write(FilerSignature, SizeOf(FilerSignature));
end;

{$IFDEF NEXTGEN}
procedure TWriter.WriteStr(const Value: String);
{$ELSE !NEXTGEN}
procedure TWriter.WriteStr(const Value: AnsiString);
{$ENDIF NEXTGEN}
var
  L: Integer;
  Buf: TBytes;
begin
{$IFDEF NEXTGEN}
  Buf := TEncoding.UTF8.GetBytes(Value);
{$ELSE !NEXTGEN}
  Buf := TEncoding.UTF8.GetBytes(String(Value));
{$ENDIF NEXTGEN}
  L := Length(Buf);
  if L > 255 then L := 255;
  Write(L, SizeOf(Byte));
  Write(Buf, L);
end;

procedure TWriter.WriteString(const Value: String);
var
  L: Integer;
  StrBuf: TBytes;
  NeedUTF8: Boolean;
begin
  StrBuf := TEncoding.UTF8.GetBytes(Value);
  if Length(StrBuf) < (Length(Value) * SizeOf(WideChar)) then
  begin
    NeedUTF8 := False;
    for L := 0 to Length(StrBuf)-1 do
    begin
      if StrBuf[L] > 127 then
      begin
        NeedUTF8 := True;
        Break;
      end;
    end;

    L := Length(StrBuf);
    if NeedUTF8 then
    begin
      WriteValue(vaUtf8String);
      Write(L, SizeOf(Integer));
      Write(StrBuf, L);
    end
    else
    begin
      if L <= 255 then
      begin
        WriteValue(vaString);
        Write(L, SizeOf(Byte));
      end else
      begin
        WriteValue(vaLString);
        Write(L, SizeOf(Integer));
      end;
        Write(StrBuf, L);
    end;
  end
  else
  begin
    StrBuf := TEncoding.Unicode.GetBytes(Value);
    WriteValue(vaWString);
    L := Length(StrBuf) div 2;
    Write(L, SizeOf(Integer));
    Write(StrBuf, Length(StrBuf));
  end;
end;

procedure TWriter.WriteUTF8Str(const Value: string);
var
  U: TBytes;
  L: Integer;
begin
  U:= TEncoding.UTF8.GetBytes(Value);
  L := Length(U);
  if L > 255 then L := 255;
  Write(L, SizeOf(Byte));
  Write(U, L);
end;

{$IFNDEF NEXTGEN}
procedure TWriter.WriteWideString(const Value: String);
begin
  WriteString(Value);
end;
{$ENDIF !NEXTGEN}

procedure TWriter.WriteValue(Value: TValueType);
{$IF    SizeOf(TValueType) = 1}
var
  Buf: UInt8;
begin
  Buf := UInt8(Value);
  Write(Buf, 1);
end;
{$ELSE  SizeOf(TValueType) = 1}
var
  Buf: UInt32;
begin
  Buf := UInt32(Value);
  Write(Buf, SizeOf(Value));
end;
{$ENDIF SizeOf(TValueType) = 1}

procedure TWriter.GetLookupInfo(var Ancestor: TPersistent; var Root,
  LookupRoot, RootAncestor: TComponent);
begin
  Ancestor := Self.Ancestor;
  Root := Self.Root;
  LookupRoot := Self.LookupRoot;
  RootAncestor := Self.RootAncestor;
end;

{ TParser }

const
  ParseBufSize = 4096;

procedure BinToHex(const Buffer: TBytes; BufOffset: Integer;
  var Text: TBytes; TextOffset: Integer; Count: Integer);
const
  B2HConvert: array[0..15] of Byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41, $42, $43, $44, $45, $46);
var
  I: Integer;
begin
  for I := 0  to Count - 1 do
  begin
    Text[TextOffset + I * 2] := B2HConvert[Buffer[BufOffset + I] shr 4];
    Text[TextOffset + I * 2 + 1] := B2HConvert[Buffer[BufOffset + I] and $0F];
  end;
end;

function HexToBin(const Text: PChar; TextOffset: Integer;
  var Buffer: TBytes; BufOffset: Integer; Count: Integer): Integer; overload;
var
  I, C: Integer;
begin
  C := 0;
  for I := 0  to Count - 1 do
  begin
    if not (Text[TextOffset + I * 2] in H2BValidSet) or not (Text[TextOffset + I * 2 + 1] in H2BValidSet) then Break;
    Buffer[BufOffset + I] :=
      (H2BConvert[Text[TextOffset + I * 2]] shl 4) or
       H2BConvert[Text[TextOffset + I * 2 + 1]];
    Inc(C);
  end;
  Result := C;
end;

function HexToBin(const Text: TBytes; TextOffset: Integer;
  var Buffer: TBytes; BufOffset: Integer; Count: Integer): Integer;
var
  I, C: Integer;
begin
  C := 0;
  for I := 0 to Count - 1 do
  begin
    if not (Chr(Text[TextOffset + I * 2]) in H2BValidSet) or
       not (Chr(Text[TextOffset + I * 2 + 1]) in H2BValidSet) then
      Break;
    Buffer[BufOffset + I] :=
      (H2BConvert[Chr(Text[TextOffset + I * 2])] shl 4) or
       H2BConvert[Chr(Text[TextOffset + I * 2 + 1])];
    Inc(C);
  end;
  Result := C;
end;

{$IFNDEF NEXTGEN}
procedure BinToHex(Buffer: PAnsiChar; Text: PAnsiChar; BufSize: Integer);
const
  Convert: array[0..15] of AnsiChar = AnsiString('0123456789ABCDEF');
var
  I: Integer;
begin
  for I := 0 to BufSize - 1 do
  begin
    Text[0] := Convert[Byte(Buffer[I]) shr 4];
    Text[1] := Convert[Byte(Buffer[I]) and $F];
    Inc(Text, 2);
  end;
end;

procedure BinToHex(Buffer: PAnsiChar; Text: PWideChar; BufSize: Integer);
const
  Convert: array[0..15] of WideChar = '0123456789ABCDEF';
var
  I: Integer;
begin
  for I := 0 to BufSize - 1 do
  begin
    Text[0] := Convert[Byte(Buffer[I]) shr 4];
    Text[1] := Convert[Byte(Buffer[I]) and $F];
    Inc(Text, 2);
  end;
end;

procedure BinToHex(var Buffer; Text: PWideChar; BufSize: Integer);
begin
  BinToHex(@Buffer, Text, BufSize);
end;

procedure BinToHex(var Buffer; Text: PAnsiChar; BufSize: Integer);
begin
  BinToHex(@Buffer, Text, BufSize);
end;

procedure BinToHex(Buffer: Pointer; Text: PWideChar; BufSize: Integer);
begin
  BinToHex(PAnsiChar(Buffer), Text, BufSize);
end;

procedure BinToHex(Buffer: Pointer; Text: PAnsiChar; BufSize: Integer);
begin
  BinToHex(PAnsiChar(Buffer), Text, BufSize);
end;

function HexToBin(Text : PWideChar; Buffer: PAnsiChar; BufSize: Integer): Integer;
var
  I: Integer;
begin
  I := BufSize;
  while I > 0 do
  begin
    if not (Text[0] in H2BValidSet) or not (Text[1] in H2BValidSet) then Break;
    Buffer[0] := AnsiChar((H2BConvert[AnsiChar(Text[0])] shl 4) + H2BConvert[AnsiChar(Text[1])]);
    Inc(Buffer);
    Inc(Text, 2);
    Dec(I);
  end;
  Result := BufSize - I;
end;

function HexToBin(Text : PAnsiChar; Buffer: PAnsiChar; BufSize: Integer): Integer;
var
  I: Integer;
begin
  I := BufSize;
  while I > 0 do
  begin
    if not (Text[0] in H2BValidSet) or not (Text[1] in H2BValidset) then Break;
    Buffer[0] := AnsiChar((H2BConvert[Text[0]] shl 4) + H2BConvert[Text[1]]);
    Inc(Buffer);
    Inc(Text, 2);
    Dec(I);
  end;
  Result := BufSize - I;
end;

function HexToBin(Text: PWideChar; var Buffer; BufSize: Integer): Integer;
begin
  Result := HexToBin(Text, PAnsiChar(@Buffer), BufSize);
end;

function HexToBin(Text: PAnsiChar; var Buffer; BufSize: Integer): Integer;
begin
  Result := HexToBin(Text, PAnsiChar(@Buffer), BufSize);
end;

function HexToBin(Text: PWideChar; Buffer: Pointer; BufSize: Integer): Integer;
begin
  Result := HexToBin(Text, PAnsiChar(Buffer), BufSize);
end;

function HexToBin(Text: PAnsiChar; Buffer: Pointer; BufSize: Integer): Integer;
begin
  Result := HexToBin(Text, PAnsiChar(Buffer), BufSize);
end;
{$ENDIF !NEXTGEN}

constructor TParser.Create(Stream: TStream; AOnError: TParserErrorEvent = nil);
begin
  { Call the other constructor. Use default format settings. }
  Create(Stream, FormatSettings, AOnError);
end;

constructor TParser.Create(Stream: TStream; const FormatSettings: TFormatSettings; AOnError: TParserErrorEvent = nil);
begin
  inherited Create;
  FFormatSettings := FormatSettings;
  FStream := Stream;
  SetLength(FBuffer, ParseBufSize);
  FBuffer[0] := 0;
  FBufPtr := 0;
  FBufEnd := ParseBufSize - 1;
  FSourcePtr := 0;
  FSourceEnd := 0;
  FTokenPtr := 0;
  FSourceLine := 1;
  FOnError := AOnError;
  ReadBuffer;
  FSourcePtr := FSourcePtr + TEncoding.GetBufferEncoding(FBuffer, FEncoding);
  if (FEncoding = nil) or ((FEncoding <> TEncoding.ASCII) and (FEncoding <> TEncoding.ANSI) and (FEncoding <> TEncoding.UTF8)) then
    Error(SAnsiUTF8Expected);
  NextToken;
end;

destructor TParser.Destroy;
begin
  if Length(FBuffer) > 0 then
    FStream.Seek(Int64(FTokenPtr) - Int64(FBufPtr), TSeekOrigin.soCurrent);
  inherited Destroy;
end;

function UTF8Len(B: Byte): Integer; inline;
begin
  case B of
    $00..$7F: Result := 1; //
    $C2..$DF: Result := 2; // 110x xxxx C0 - DF
    $E0..$EF: Result := 3; // 1110 xxxx E0 - EF
    $F0..$F7: Result := 4; // 1111 0xxx F0 - F7 // outside traditional UNICODE
  else
    Result := 0; // Illegal leading character.
  end;
end;

function UTF8ToCategory(const ABuffer: TBytes; var ABufPos: NativeInt): TUnicodeCategory;
var
  CharSize: Integer;
  C: UCS4Char;
begin
  CharSize := UTF8Len(ABuffer[ABufPos]);
  Assert((CharSize > 0) and (CharSize + ABufPos < Length(ABuffer)), 'Invalid UTF8 Character'); // do not localize
  case CharSize of
    1: C := UCS4Char(ABuffer[ABufPos]);
    2: C := UCS4Char(((ABuffer[ABufPos] and $1F) shl 6 ) or (ABuffer[ABufPos + 1] and $3F));
    3: C := UCS4Char(((ABuffer[ABufPos] and $0F) shl 12) or ((ABuffer[ABufPos + 1] and $3F) shl 6 ) or (ABuffer[ABufPos + 2] and $3F));
    4: C := UCS4Char(((ABuffer[ABufPos] and $07) shl 18) or ((ABuffer[ABufPos + 1] and $3F) shl 12) or ((ABuffer[ABufPos + 2] and $3F) shl 6) or (ABuffer[ABufPos + 2] and $3F));
  else
    C := 0;
  end;
  Inc(ABufPos, CharSize);
  if C > $0000FFFF then
    Result := Char.GetUnicodeCategory(Char.ConvertFromUtf32(C), 1)
  else
    Result := Char.GetUnicodeCategory(C);
end;

function TParser.CharType(var ABufPos: NativeInt): TCharType;
begin
  Inc(ABufPos);
  case Char(FBuffer[ABufPos - 1]) of
    'A'..'Z', 'a'..'z', '_': Result := ctLetterStart;
    '0'..'9': Result := ctNumber;
    '#': Result := ctHash;
    '-': Result := ctDash;
    '$': Result := ctDollar;
    '''': Result := ctQuote;
  else
    if (FEncoding = TEncoding.UTF8) and (FBuffer[ABufPos - 1] > 127) then
    begin
      Dec(ABufPos);
      case UTF8ToCategory(FBuffer, ABufPos) of
        TUnicodeCategory.ucLowercaseLetter,
        TUnicodeCategory.ucModifierLetter,
        TUnicodeCategory.ucOtherLetter,
        TUnicodeCategory.ucTitlecaseLetter,
        TUnicodeCategory.ucUppercaseLetter,
        TUnicodeCategory.ucLetterNumber:
          Result := ctLetterStart;

        TUnicodeCategory.ucCombiningMark,
        TUnicodeCategory.ucNonSpacingMark,
        TUnicodeCategory.ucConnectPunctuation,
        TUnicodeCategory.ucFormat,
        TUnicodeCategory.ucDecimalNumber:
          Result := ctLetterNumber;
      else
        Result := ctOther;
      end;
    end else
      Result := ctOther;
  end;
end;

procedure TParser.CheckToken(T: Char);
begin
  if Token <> T then
    case T of
      toSymbol:
        Error(SIdentifierExpected);
      System.Classes.toString, toWString:
        Error(SStringExpected);
      toInteger, toFloat:
        Error(SNumberExpected);
    else
      ErrorFmt(SCharExpected, [T]);
    end;
end;

procedure TParser.CheckTokenSymbol(const S: string);
begin
  if not TokenSymbolIs(S) then
    ErrorFmt(SSymbolExpected, [S]);
end;

procedure TParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

procedure TParser.ErrorFmt(const Ident: string; const Args: array of const);
begin
  ErrorStr(Format(Ident, Args));
end;

procedure TParser.ErrorStr(const Message: string);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnError) then
    FOnError(Self, Message, Handled);
  if not Handled then
    raise EParserError.CreateResFmt(@SParseError, [Message, FSourceLine]);
end;

function TParser.GetLinePos: NativeInt;
begin
  Result := FTokenPtr - LineStart(FBuffer, FTokenPtr);
end;

procedure TParser.HexToBinary(Stream: TStream);
var
  Count: Integer;
  Buffer: TBytes;
begin
  SetLength(Buffer, 256);
  SkipBlanks;
  while Char(FBuffer[FSourcePtr]) <> '}' do
  begin
    Count := HexToBin(FBuffer, FSourcePtr, Buffer, 0, Length(Buffer));
    if Count = 0 then
    begin
      Error(SInvalidBinary);
      Exit;
    end;
    Stream.Write(Buffer, 0, Count);
    Inc(FSourcePtr, Count * 2);
    SkipBlanks;
  end;
  NextToken;
end;

function TParser.NextToken: Char;
var
  I, J: Integer;
  IsWideStr: Boolean;
  P, Q, S: NativeInt;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  Q := P;
  case CharType(Q) of
    ctLetterStart:
      begin
        P := Q;
        while CharType(Q) in [ctLetterStart, ctLetterNumber, ctNumber] do
          P := Q;
        Result := toSymbol;
      end;
    ctHash, ctQuote:
      begin
        IsWideStr := False;
        J := 0;
        S := P;
        while True do
          case Char(FBuffer[P]) of
            '#':
              begin
                Inc(P);
                I := 0;
                while FBuffer[P] in [Ord('0')..Ord('9')] do
                begin
                  I := I * 10 + (FBuffer[P] - Ord('0'));
                  Inc(P);
                end;
                if (I > 127) then
                  IsWideStr := True;
                Inc(J);
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case FBuffer[P] of
                    0, 10, 13:
                      begin
                        Error(SInvalidString);
                        Break;
                      end;
                    Ord(''''):
                      begin
                        Inc(P);
                        if Char(FBuffer[P]) <> '''' then
                          Break;
                      end;
                  end;
                  Inc(J);
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        P := S;
        if IsWideStr then
          SetLength(FWideStr, J);
        J := 0;
        while True do
          case Char(FBuffer[P]) of
            '#':
              begin
                Inc(P);
                I := 0;
                while FBuffer[P] in [Ord('0')..Ord('9')] do
                begin
                  I := I * 10 + (FBuffer[P] - Ord('0'));
                  Inc(P);
                end;
                if IsWideStr then
                begin
                  FWideStr[J] := WideChar(SmallInt(I));
                  Inc(J);
                end else
                begin
                  FBuffer[S] := I;
                  Inc(S);
                end;
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case FBuffer[P] of
                    0, 10, 13:
                      begin
                        Error(SInvalidString);
                        Break;
                      end;
                    Ord(''''):
                      begin
                        Inc(P);
                        if FBuffer[P] <> Ord('''') then
                          Break;
                      end;
                  end;
                  if IsWideStr then
                  begin
                    FWideStr[J] := WideChar(FBuffer[P]);
                    Inc(J);
                  end else
                  begin
                    FBuffer[S] := FBuffer[P];
                    Inc(S);
                  end;
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        FStringPtr := S;
        if IsWideStr then
          Result := toWString
        else
          Result := System.Classes.toString;
      end;
    ctDollar:
      begin
        Inc(P);
        while FBuffer[P] in [Ord('0')..Ord('9'), Ord('A')..Ord('F'), Ord('a')..Ord('f')] do
          Inc(P);
        Result := toInteger;
      end;
    ctDash, ctNumber:
      begin
        Inc(P);
        while FBuffer[P] in [Ord('0')..Ord('9')] do
          Inc(P);
        Result := toInteger;
        while FBuffer[P] in [Ord('0')..Ord('9'), Ord('.'), Ord('e'), Ord('E'), Ord('+'), Ord('-')] do
        begin
          Inc(P);
          Result := toFloat;
        end;
        if FBuffer[P] in [Ord('c'), Ord('C'), Ord('d'), Ord('D'), Ord('s'), Ord('S'), Ord('f'), Ord('F')] then
        begin
          Result := toFloat;
          FFloatType := Char(FBuffer[P]);
          Inc(P);
        end else
          FFloatType := #0;
      end;
  else
    Result := Char(FBuffer[P]);
    if Result <> toEOF then
      Inc(P);
  end;
  FSourcePtr := P;
  FToken := Result;
end;

procedure TParser.ReadBuffer;
var
  Count: NativeInt;
begin
  Inc(FOrigin, FSourcePtr);
  FBuffer[FSourceEnd] := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then
    Move(FBuffer[FSourcePtr], FBuffer[0], Count);
  FBufPtr := Count;
  Inc(FBufPtr, FStream.Read(FBuffer, FBufPtr, FBufEnd - FBufPtr));
  FSourcePtr := 0;
  FSourceEnd := FBufPtr;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = 0 then
      Error(SLineTooLong);
  end;
  FSaveChar := FBuffer[FSourceEnd];
  FBuffer[FSourceEnd] := 0;
end;

procedure TParser.SkipBlanks;
begin
  while True do
  begin
    case FBuffer[FSourcePtr] of
      0:
        begin
          ReadBuffer;
          if FBuffer[FSourcePtr] = 0 then
            Exit;
          Continue;
        end;
      10:
        Inc(FSourceLine);
      33..255:
        Exit;
    end;
    Inc(FSourcePtr);
  end;
end;

function TParser.SourcePos: NativeInt;
begin
  Result := FOrigin + FTokenPtr;
end;

function TParser.TokenFloat: Extended;
begin
  if FFloatType <> #0 then
    Dec(FSourcePtr);
  Result := StrToFloat(TokenString, FFormatSettings);
  if FFloatType <> #0 then
    Inc(FSourcePtr);
end;

function TParser.TokenInt: Int64;
begin
  Result := StrToInt64(TokenString);
end;

function TParser.TokenString: string;
var
  L: NativeInt;
begin
  if FToken = System.Classes.toString then
    L := FStringPtr - FTokenPtr
  else
    L := FSourcePtr - FTokenPtr;
  Result := FEncoding.GetString(FBuffer, FTokenPtr, L);
end;

function TParser.TokenWideString: UnicodeString;
begin
  if FToken = System.Classes.toString then
    Result := TokenString
  else
    Result := string.Create(FWideStr);
end;

function TParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (Token = toSymbol) and SameText(S, TokenString);
end;

function TParser.TokenComponentIdent: string;
var
  P, Q: NativeInt;
begin
  CheckToken(toSymbol);
  P := FSourcePtr;
  while FBuffer[P] = Ord('.') do
  begin
    Inc(P);
    Q := P;
    if CharType(Q) <> ctLetterStart then
      Error(SIdentifierExpected);
    repeat
      P := Q;
    until not (CharType(Q) in [ctLetterStart, ctLetterNumber, ctNumber]);
  end;
  FSourcePtr := P;
  Result := TokenString;
end;

{ Binary to text conversion }

procedure ObjectBinaryToText(const Input, Output: TStream);
var
  NestingLevel: Integer;
  Reader: TReader;
  Writer: TWriter;
  ObjectName, PropName: string;
  UTF8Idents: Boolean;
  MemoryStream: TMemoryStream;
  LFormatSettings: TFormatSettings;

  procedure WriteIndent;
  var
    Buf: TBytes;
    I: Integer;
  begin
    Buf := TBytes.Create($20, $20);
    for I := 1 to NestingLevel do Writer.Write(Buf, Length(Buf));
  end;

  procedure WriteTBytes(S: TBytes);
  begin
    Writer.Write(S, Length(S));
  end;

  procedure WriteAsciiStr(const S: String);
  var
    Buf: TBytes;
    I: Integer;
  begin
    SetLength(Buf, S.Length);
    for I := Low(S) to High(S) do
      Buf[I-Low(S)] := Byte(S[I]);
    Writer.Write(Buf, Length(Buf));
  end;

  procedure WriteByte(const B: Byte);
  begin
    Writer.Write(B, 1);
  end;

  procedure WriteUTF8Str(const S: string);
  var
    Ident: TBytes; // UTF8String;
  begin
    Ident := TEncoding.UTF8.GetBytes(S);
{ TODO -otarisawa : Is it good logic? }
    if not UTF8Idents and (Length(Ident) > S.Length) then
      UTF8Idents := True;
    WriteTBytes(Ident);
  end;

  procedure NewLine;
  begin
    WriteAsciiStr(sLineBreak);
    WriteIndent;
  end;

  procedure ConvertValue; forward;

  procedure ConvertHeader;
  var
    ClassName: string;
    Flags: TFilerFlags;
    Position: Integer;
  begin
    Reader.ReadPrefix(Flags, Position);
    ClassName := Reader.ReadStr;
    ObjectName := Reader.ReadStr;
    WriteIndent;
    if ffInherited in Flags then
      WriteAsciiStr('inherited ')
    else if ffInline in Flags then
      WriteAsciiStr('inline ')
    else
      WriteAsciiStr('object ');
    if ObjectName <> '' then
    begin
      WriteUTF8Str(ObjectName);
      WriteAsciiStr(': ');
    end;
    WriteUTF8Str(ClassName);
    if ffChildPos in Flags then
    begin
      WriteAsciiStr(' [');
      WriteAsciiStr(IntToStr(Position));
      WriteAsciiStr(']');
    end;

    if ObjectName = '' then
      ObjectName := ClassName;  // save for error reporting

    WriteAsciiStr(sLineBreak);
  end;

  procedure ConvertBinary;
  const
    BytesPerLine = 32;
  var
    MultiLine: Boolean;
    I: Integer;
    Count: Integer;
    Buffer: TBytes; // array[0..BytesPerLine - 1] of AnsiChar;
    Text: TBytes; // array[0..BytesPerLine * 2 - 1] of AnsiChar;
  begin
    SetLength(Buffer, BytesPerLine);
    SetLength(Text, BytesPerLine*2+1);

    Reader.ReadValue;
    WriteAsciiStr('{');
    Inc(NestingLevel);
    Reader.Read(Count, SizeOf(Count));
    MultiLine := Count >= BytesPerLine;
    while Count > 0 do
    begin
      if MultiLine then NewLine;
      if Count >= 32 then I := 32 else I := Count;
      Reader.Read(Buffer, I);
      BinToHex(Buffer, 0, Text, 0, I);
      Writer.Write(Text, I * 2);
      Dec(Count, I);
    end;
    Dec(NestingLevel);
    WriteAsciiStr('}');
  end;

  procedure ConvertProperty; forward;

  procedure ConvertValue;
  const
    LineLength = 64;
  var
    I, J, K, L: Integer;
    S: String;
    W: String;
    LineBreak: Boolean;
  begin
    case Reader.NextValue of
      vaList:
        begin
          Reader.ReadValue;
          WriteAsciiStr('(');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            ConvertValue;
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteAsciiStr(')');
        end;
      vaInt8, vaInt16, vaInt32:
        WriteAsciiStr(IntToStr(Reader.ReadInteger));
      vaExtended, vaDouble:
        WriteAsciiStr(FloatToStrF(Reader.ReadFloat, ffFixed, 16, 18, LFormatSettings));
      vaSingle:
        WriteAsciiStr(FloatToStr(Reader.ReadSingle, LFormatSettings) + 's');
      vaCurrency:
        WriteAsciiStr(FloatToStr(Reader.ReadCurrency * 10000, LFormatSettings) + 'c');
      vaDate:
        WriteAsciiStr(FloatToStr(Reader.ReadDate, LFormatSettings) + 'd');
      vaWString, vaUTF8String:
        begin
          W := Reader.ReadString;
          L := High(W);
          if L = High('') then WriteAsciiStr('''''') else
          begin
            I := Low(W);
            Inc(NestingLevel);
            try
              if L > LineLength then NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (W[I] >= ' ') and (W[I] <> '''') and (Ord(W[i]) <= 127) then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (W[I] < ' ') or (W[I] = '''') or
                    ((I - K) >= LineLength) or (Ord(W[i]) > 127);
                  if ((I - K) >= LineLength) then LineBreak := True;
                  WriteAsciiStr('''');
                  while J < I do
                  begin
                    WriteByte(Byte(W[J]));
                    Inc(J);
                  end;
                  WriteAsciiStr('''');
                end else
                begin
                  WriteAsciiStr('#');
                  WriteAsciiStr(IntToStr(Ord(W[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteAsciiStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      vaString, vaLString:
        begin
          S := Reader.ReadString;
          L := High(S);
          if L = High('') then WriteAsciiStr('''''') else
          begin
            I := Low(S);
            Inc(NestingLevel);
            try
              if L > LineLength then NewLine;
              K := I;
              repeat
                LineBreak := False;
                if (S[I] >= ' ') and (S[I] <> '''') then
                begin
                  J := I;
                  repeat
                    Inc(I)
                  until (I > L) or (S[I] < ' ') or (S[I] = '''') or
                    ((I - K) >= LineLength);
                  if ((I - K) >= LineLength) then
                  begin
                    LIneBreak := True;
{ TODO -otarisawa : Does it need? }
//                    if ByteType(S, I) = mbTrailByte then Dec(I);
                  end;
                  WriteAsciiStr('''');
{ TODO -otarisawa : ZEROSTRINGS }
                  WriteAsciiStr(S.Substring(J-Low(S), I-J));
                  WriteAsciiStr('''');
                end else
                begin
                  WriteAsciiStr('#');
                  WriteAsciiStr(IntToStr(Ord(S[I])));
                  Inc(I);
                  if ((I - K) >= LineLength) then LineBreak := True;
                end;
                if LineBreak and (I <= L) then
                begin
                  WriteAsciiStr(' +');
                  NewLine;
                  K := I;
                end;
              until I > L;
            finally
              Dec(NestingLevel);
            end;
          end;
        end;
      vaIdent, vaFalse, vaTrue, vaNil, vaNull:
        WriteUTF8Str(Reader.ReadIdent);
      vaBinary:
        ConvertBinary;
      vaSet:
        begin
          Reader.ReadValue;
          WriteAsciiStr('[');
          I := 0;
          while True do
          begin
            S := Reader.ReadStr;
            if S = '' then Break;
            if I > 0 then WriteAsciiStr(', ');
            WriteUtf8Str(S);
            Inc(I);
          end;
          WriteAsciiStr(']');
        end;
      vaCollection:
        begin
          Reader.ReadValue;
          WriteAsciiStr('<');
          Inc(NestingLevel);
          while not Reader.EndOfList do
          begin
            NewLine;
            WriteAsciiStr('item');
            if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then
            begin
              WriteAsciiStr(' [');
              ConvertValue;
              WriteAsciiStr(']');
            end;
            WriteAsciiStr(sLineBreak);
            Reader.CheckValue(vaList);
            Inc(NestingLevel);
            while not Reader.EndOfList do
              ConvertProperty;
            Reader.ReadListEnd;
            Dec(NestingLevel);
            WriteIndent;
            WriteAsciiStr('end');
          end;
          Reader.ReadListEnd;
          Dec(NestingLevel);
          WriteAsciiStr('>');
        end;
      vaInt64:
        WriteAsciiStr(IntToStr(Reader.ReadInt64));
    else
      raise EReadError.CreateResFmt(@sPropertyException,
        [ObjectName, DotSep, PropName, IntToStr(Ord(Reader.NextValue))]);
    end;
  end;

  procedure ConvertProperty;
  begin
    WriteIndent;
    PropName := Reader.ReadStr;  // save for error reporting
    WriteUTF8Str(PropName);
    WriteAsciiStr(' = ');
    ConvertValue;
    WriteAsciiStr(sLineBreak);
  end;

  procedure ConvertObject;
  begin
    ConvertHeader;
    Inc(NestingLevel);
    while not Reader.EndOfList do
      ConvertProperty;
    Reader.ReadListEnd;
    while not Reader.EndOfList do
      ConvertObject;
    Reader.ReadListEnd;
    Dec(NestingLevel);
    WriteIndent;
    WriteAsciiStr('end' + sLineBreak);
  end;

begin
  NestingLevel := 0;
  UTF8Idents := False;
  Reader := TReader.Create(Input, 4096);
  LFormatSettings := TFormatSettings.Create('en-US'); // do not localize
  LFormatSettings.DecimalSeparator := '.';
  try
    MemoryStream := TMemoryStream.Create;
    try
      Writer := TWriter.Create(MemoryStream, 4096);
      try
        Reader.ReadSignature;
        ConvertObject;
      finally
        Writer.Free;
      end;
      if UTF8Idents then
        Output.Write(TEncoding.UTF8.GetPreamble[0], 3);
      Output.Write(MemoryStream.Memory^, MemoryStream.Size);
    finally
      MemoryStream.Free;
    end;
  finally
    Reader.Free;
  end;
end;

type
  TObjectTextConvertProc = procedure (const Input, Output: TStream);
  TNamedObjectTextConvertProc = procedure (const Input, Output: TStream; const Name: string);
  TBinarySignature = record
    BinarySignature: Integer;
    SignatureLength: Integer;
  end;

function Min(I1, I2: Integer): Integer; inline;
begin
  if I1 < I2 then
    Result := I1
  else
    Result := I2;
end;

function IsBinary(const Input: TStream; const Signatures: array of TBinarySignature): Boolean;
var
  I: Integer;
  Signature: Integer;
  Pos: Integer;
begin
  Pos := Input.Position;
  for I := Low(Signatures) to High(Signatures) do
  begin
    Signature := 0;
    Input.Read(Signature, Min(Signatures[I].SignatureLength, SizeOf(Signature)));
    Input.Position := Pos;
    if Signature = Signatures[I].BinarySignature then
      Exit(True);
  end;
  Result := False;
end;

procedure InternalBinaryToText(const Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat;
  ConvertProc: TObjectTextConvertProc; const Signatures: array of TBinarySignature);
var
  Pos: Integer;
  Signature: Integer;
begin
  Pos := Input.Position;
  if IsBinary(Input, Signatures) then
  begin     // definitely binary format
    if OriginalFormat = sofBinary then
      Output.CopyFrom(Input, Input.Size - Input.Position)
    else
    begin
      if OriginalFormat = sofUnknown then
        Originalformat := sofBinary;
      ConvertProc(Input, Output);
    end;
  end else  // might be text format
  begin
    Input.Read(Signature, SizeOf(Signature));
    Input.Position := Pos;
    if OriginalFormat = sofBinary then
      ConvertProc(Input, Output)
    else
    begin
      if OriginalFormat = sofUnknown then
      begin   // text format may begin with "object", "inherited", or whitespace
        if Byte(Signature) in [Ord('o'),Ord('O'), Ord('i'), Ord('I'), Ord(' '), 13, 11, 9] then
          OriginalFormat := sofText
        else if (Signature and $00FFFFFF) = $00BFBBEF then
          OriginalFormat := sofUTF8Text
        else    // not binary, not text... let it raise the exception
        begin
          ConvertProc(Input, Output);
          Exit;
        end;
      end;
      if OriginalFormat in [sofText, sofUTF8Text] then
        Output.CopyFrom(Input, Input.Size - Input.Position);
    end;
  end;
end;

procedure InternalTextToBinary(const Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat;
  ConvertProc: TNamedObjectTextConvertProc; const Signatures: array of TBinarySignature; const Name: string);
var
  Pos: Integer;
  Signature: Integer;
begin
  Pos := Input.Position;
  if IsBinary(Input, Signatures) then
  begin     // definitely binary format
    if OriginalFormat = sofUnknown then
      Originalformat := sofBinary;
    if OriginalFormat = sofBinary then
      Output.CopyFrom(Input, Input.Size - Input.Position)
    else    // let it raise the exception
      ConvertProc(Input, Output, Name);
  end else  // might be text format
  begin
    Input.Read(Signature, SizeOf(Signature));
    Input.Position := Pos;
    case OriginalFormat of
      sofUnknown:
        begin  // text format may begin with "object", "inherited", or whitespace
          // TODO -oUnassigned : Combine with similar line above in a helper.
          if Byte(Signature) in [Ord('o'),Ord('O'), Ord('i'), Ord('I'), Ord(' '), 13, 11, 9] then
            OriginalFormat := sofText
          else if (Signature and $00FFFFFF) = $00BFBBEF then
            OriginalFormat := sofUTF8Text;
          // if its not binary, not text... let it raise the exception
          ConvertProc(Input, Output, Name);
        end;
      sofBinary:   ConvertProc(Input, Output, Name);
      sofText,
      sofUTF8Text: Output.CopyFrom(Input, Input.Size - Input.Position);
    end;
  end;
end;

const
  FilerSignatures: array[0..0] of TBinarySignature = (
   (BinarySignature: Integer(Ord('0') shl 24 + Ord('F') shl 16 + Ord('P') shl 8 + Ord('T'));
    SignatureLength: SizeOf(Integer)));

procedure ObjectBinaryToText(const Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat);
begin
  InternalBinaryToText(Input, Output, OriginalFormat, ObjectBinaryToText, FilerSignatures);
end;

{ Text to binary conversion }

procedure ObjectTextToBinary(const Input, Output: TStream);
var
  Parser: TParser;
  Writer: TWriter;
  FFmtSettings: TFormatSettings;
  TokenStr: String;

  function ConvertOrderModifier: Integer;
  begin
    Result := -1;
    if Parser.Token = '[' then
    begin
      Parser.NextToken;
      Parser.CheckToken(toInteger);
      Result := Parser.TokenInt;
      Parser.NextToken;
      Parser.CheckToken(']');
      Parser.NextToken;
    end;
  end;

  procedure ConvertHeader(IsInherited, IsInline: Boolean);
  var
    ClassName, ObjectName: string;
    Flags: TFilerFlags;
    Position: Integer;
  begin
    Parser.CheckToken(toSymbol);
    ClassName := Parser.TokenString;
    ObjectName := '';
    if Parser.NextToken = ':' then
    begin
      Parser.NextToken;
      Parser.CheckToken(toSymbol);
      ObjectName := ClassName;
      ClassName := Parser.TokenString;
      Parser.NextToken;
    end;
    Flags := [];
    Position := ConvertOrderModifier;
    if IsInherited then
      Include(Flags, ffInherited);
    if IsInline then
      Include(Flags, ffInline);
    if Position >= 0 then
      Include(Flags, ffChildPos);
    Writer.WritePrefix(Flags, Position);
    Writer.WriteUTF8Str(ClassName);
    Writer.WriteUTF8Str(ObjectName);
  end;

  procedure ConvertProperty; forward;

  procedure ConvertValue;
  var
    Order: Integer;

    function CombineString: String;
    begin
      Result := Parser.TokenWideString;
      while Parser.NextToken = '+' do
      begin
        Parser.NextToken;
        if not (Parser.Token in [System.Classes.toString, toWString]) then
          Parser.CheckToken(System.Classes.toString);
        Result := Result + Parser.TokenWideString;
      end;
    end;

  begin
    if Parser.Token in [System.Classes.toString, toWString] then
      Writer.WriteString(CombineString)
    else
    begin
      case Parser.Token of
        toSymbol:
          Writer.WriteIdent(Parser.TokenComponentIdent);
        toInteger:
          Writer.WriteInteger(Parser.TokenInt);
        toFloat:
          begin
            case Parser.FloatType of
              's', 'S': Writer.WriteSingle(Parser.TokenFloat);
              'c', 'C': Writer.WriteCurrency(Parser.TokenFloat / 10000);
              'd', 'D': Writer.WriteDate(Parser.TokenFloat);
            else
              Writer.WriteFloat(Parser.TokenFloat);
            end;
          end;
        '[':
          begin
            Parser.NextToken;
            Writer.WriteValue(vaSet);
            if Parser.Token <> ']' then
              while True do
              begin
                TokenStr := Parser.TokenString;
                case Parser.Token of
                  toInteger: begin end;
                  System.Classes.toString,toWString: TokenStr := '#' + IntToStr(Ord(TokenStr.Chars[0]));
                else
                  Parser.CheckToken(toSymbol);
                end;
                Writer.WriteUTF8Str(TokenStr);
                if Parser.NextToken = ']' then Break;
                Parser.CheckToken(',');
                Parser.NextToken;
              end;
            Writer.WriteUTF8Str('');
          end;
        '(':
          begin
            Parser.NextToken;
            Writer.WriteListBegin;
            while Parser.Token <> ')' do ConvertValue;
            Writer.WriteListEnd;
          end;
        '{':
          Writer.WriteBinary(Parser.HexToBinary);
        '<':
          begin
            Parser.NextToken;
            Writer.WriteValue(vaCollection);
            while Parser.Token <> '>' do
            begin
              Parser.CheckTokenSymbol('item');
              Parser.NextToken;
              Order := ConvertOrderModifier;
              if Order <> -1 then Writer.WriteInteger(Order);
              Writer.WriteListBegin;
              while not Parser.TokenSymbolIs('end') do ConvertProperty;
              Writer.WriteListEnd;
              Parser.NextToken;
            end;
            Writer.WriteListEnd;
          end;
      else
        Parser.Error(SInvalidProperty);
      end;
      Parser.NextToken;
    end;
  end;

  procedure ConvertProperty;
  var
    PropName: string;
  begin
    Parser.CheckToken(toSymbol);
    PropName := Parser.TokenString;
    Parser.NextToken;
    while Parser.Token = '.' do
    begin
      Parser.NextToken;
      Parser.CheckToken(toSymbol);
      PropName := PropName + '.' + Parser.TokenString;
      Parser.NextToken;
    end;
    Writer.WriteUTF8Str(PropName);
    Parser.CheckToken('=');
    Parser.NextToken;
    ConvertValue;
  end;

  procedure ConvertObject;
  var
    InheritedObject: Boolean;
    InlineObject: Boolean;
  begin
    InheritedObject := False;
    InlineObject := False;
    if Parser.TokenSymbolIs('INHERITED') then
      InheritedObject := True
    else if Parser.TokenSymbolIs('INLINE') then
      InlineObject := True
    else
      Parser.CheckTokenSymbol('OBJECT');
    Parser.NextToken;
    ConvertHeader(InheritedObject, InlineObject);
    while not Parser.TokenSymbolIs('END') and
      not Parser.TokenSymbolIs('OBJECT') and
      not Parser.TokenSymbolIs('INHERITED') and
      not Parser.TokenSymbolIs('INLINE') do
      ConvertProperty;
    Writer.WriteListEnd;
    while not Parser.TokenSymbolIs('END') do ConvertObject;
    Writer.WriteListEnd;
    Parser.NextToken;
  end;
begin
  { Initialize a new TFormatSettings block }
  FFmtSettings := FormatSettings;
  FFmtSettings.DecimalSeparator := '.';

  { Create the parser instance }
  Parser := TParser.Create(Input, FFmtSettings);
  try
    Writer := TWriter.Create(Output, 4096);
    try
      Writer.WriteSignature;
      ConvertObject;
    finally
      Writer.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure InternalObjectTextToBinary(const Input, Output: TStream; const Name: string);
begin
  ObjectTextToBinary(Input, Output);
end;

procedure ObjectTextToBinary(const Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat);
begin
  InternalTextToBinary(Input, Output, OriginalFormat, InternalObjectTextToBinary, FilerSignatures, '')
end;

{ Resource to text conversion }

const
  ResourceSignatures: array[0..1] of TBinarySignature = (
   (BinarySignature: $FF;
    SignatureLength: SizeOf(Byte)),
   (BinarySignature: 0;
    SignatureLength: SizeOf(Integer)));

procedure ObjectResourceToText(const Input, Output: TStream);
begin
  Input.ReadResHeader;
  ObjectBinaryToText(Input, Output);
end;

procedure ObjectResourceToText(const Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat);
begin
  InternalBinaryToText(Input, Output, OriginalFormat, ObjectResourceToText, ResourceSignatures);
end;

{ Text to resource conversion }

procedure ObjectTextToResource(const Input, Output: TStream; const Name: string);
var
  MemoryStream: TMemoryStream;
  MemorySize: Longint;
begin
  MemoryStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(Input, MemoryStream);
    WriteObjectResourceHeader(MemoryStream, Output, Name);
    MemorySize := MemoryStream.Size;
    Output.Write(MemoryStream.Memory^, MemorySize);
  finally
    MemoryStream.Free;
  end;
end;

procedure ObjectTextToResource(const Input, Output: TStream; var OriginalFormat: TStreamOriginalFormat;
  const Name: string);
begin
  InternalTextToBinary(Input, Output, OriginalFormat, ObjectTextToResource, ResourceSignatures, Name);
end;

function TestStreamFormat(const Stream: TStream): TStreamOriginalFormat;
var
  Pos: Integer;
  Signature: Integer;
begin
  Pos := Stream.Position;
  Signature := 0;
  Stream.Read(Signature, SizeOf(Signature));
  Stream.Position := Pos;
  if (Byte(Signature) = $FF) or (Signature = Integer(FilerSignature)) or (Signature = 0) then
    Result := sofBinary
    // text format may begin with "object", "inherited", or whitespace
  else if Byte(Signature) in [Ord('o'), Ord('O'), Ord('i'), Ord('I'), Ord(' ') , 13, 11, 9] then
    Result := sofText
  else if (Signature and $00FFFFFF) = $00BFBBEF then
    Result := sofUTF8Text
  else
    Result := sofUnknown;
end;

function GetResourceName(const ObjStream: TStream; var AName: string): Boolean;
var
  LPos: Int64;
  LName: TBytes;
  I: Integer;
  Len: Byte;
begin
  Result := False;
  LPos := ObjStream.Position;
  try
// TODO -cELBRUS_LONGINT64 : Verify GetResourceName (change LongInt to UInt32)
    ObjStream.Position := SizeOf(UInt32); { Skip header }
    ObjStream.Read(Len, 1);

    { Skip over object prefix if it is present }
    if Len and $F0 = $F0 then
    begin
      if ffChildPos in TFilerFlags((Len and $F0)) then
      begin
        ObjStream.Read(Len, 1);
        case TValueType(Len) of
          vaInt8: Len := 1;
          vaInt16: Len := 2;
          vaInt32: Len := 4;
        end;
        ObjStream.Read(I, Len);
      end;
      ObjStream.Read(Len, 1);
    end;

    SetLength(LName, Len);
    ObjStream.Read(LName, Len);
    // See if there are any UTF8 chars in the name
    for I := Low(LName) to High(LName) do
      if LName[I] and $80 <> 0 then
      begin
        AName := UpperCase(TEncoding.UTF8.GetString(LName));
        Result := True;
        Exit;
      end;
    AName := UpperCase(TEncoding.Default.GetString(LName));
  finally
    ObjStream.Position := LPos;
  end;
end;

procedure WriteObjectResourceHeader(const ObjStream, Output: TStream; const Name: string);
var
  I: Integer;
  Is32bit: Boolean;
  ResName: string;
  Size: Integer;

  procedure NameLengthError(const S: string); inline;
  begin
    raise EParserError.CreateResFmt(@sComponentNameTooLong, [S]);
  end;

begin
  Size := ObjStream.Size;
  if Name = '' then
    Is32Bit := GetResourceName(ObjStream, ResName)
  else
  begin
    ResName := Name;
    Is32Bit := False;
    for I := 0 to ResName.Length - 1 do
      if ResName.Chars[I] > #$007F then
      begin
        Is32Bit := True;
        Break;
      end;
  end;
  if Length(ResName) > 70 then
    NameLengthError(ResName);
  if Is32Bit then
    Write32bitResourceHeader(TEncoding.Unicode.GetBytes(ResName), Size, Output)
  else
    Write16bitResourceHeader(TEncoding.Default.GetBytes(ResName), Size, Output);
end;

procedure Write16bitResourceHeader(const AName: TBytes; DataSize: Integer; const Output: TStream);
var
  Data: Word;
  NameSize: Integer;
begin
  NameSize := Length(AName) + 1;
  // Write the resource type
  Data := $FF;
  Output.Write(Data, SizeOf(Byte));
  Data := 10;
  Output.Write(Data, SizeOf(Data));
  // Write the resource name
  Output.WriteBuffer(AName, NameSize - 1);
  Data := 0;
  // TODO : Finalize the SizeOf here for all platforms.
  Output.Write(Data, SizeOf(Byte{AnsiChar}));
  // Write the resource flags
  Data := $1030;
  Output.Write(Data, SizeOf(Data));
  // Write the data size
  Output.Write(DataSize, SizeOf(DataSize));
end;

procedure Write32bitResourceHeader(const AName: TBytes; DataSize: Integer; const Output: TStream);
var
  Data: Integer;
  NameSize: Integer;
begin
  Output.Write(Dummy32bitResHeader, Length(Dummy32bitResHeader));

  // Write the data size
  Output.Write(DataSize, SizeOf(DataSize));
  // Write the header size
  NameSize := Length(AName) + 2;
  Data := 8 + 4{TypeSize} + NameSize + 16;
  Output.Write(Data, SizeOf(Data));
  // Write the resource type (RT_RCDATA)
  Data := $000AFFFF;
  Output.Write(Data, SizeOf(Data));
  // Now write the resource name
  Output.WriteBuffer(AName, NameSize - 2);
  Data := 0;
  Output.Write(Data, SizeOf(WideChar));
  // Finish off by writing the final 16 bytes which contain Version, LangID, and other fields
  Data := 0;
  Output.Write(Data, SizeOf(Data)); // DataVersion
  Output.Write(Data, SizeOf(Word)); // MemoryFlags
  Data := $0409;                    // For right now use US as the LandID since it is converted to this anyway
  Output.Write(Data, SizeOf(Word)); // LangID
  Data := 0;
  Output.Write(Data, SizeOf(Data)); // Version
  Output.Write(Data, SizeOf(Data)); // Characteristics
end;

type
  TSyncProc = record
    SyncRec: TThread.PSynchronizeRecord;
    Queued: Boolean;
    Signal: TObject;
  end;
  PSyncProc = ^TSyncProc;

  TExternalThread = class(TThread)
  protected
    procedure Execute; override; // This never runs.
  public
    constructor Create;
  end;

  TAnonymousThread = class(TThread)
  private
    FProc: TProc;
  protected
    procedure Execute; override;
  public
    constructor Create(const AProc: TProc);
  end;

{ TExternalThread }

constructor TExternalThread.Create;
begin
  FExternalThread := True;
  FStarted := True;
  inherited Create(False);
end;

procedure TExternalThread.Execute;
begin
  // nothing happening here.
end;

{ TAnonymousThread }

constructor TAnonymousThread.Create(const AProc: TProc);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FProc := AProc;
end;

procedure TAnonymousThread.Execute;
begin
  FProc();
end;

var
  SyncList: TList = nil;
  [Unsafe] ThreadLock: TObject;
  [Unsafe] ExternalThreads: TThreadList<TThread>;

procedure InitThreadSynchronization;
begin
  ThreadLock := TObject.Create;
{$IFDEF AUTOREFCOUNT}
  ThreadLock.__ObjAddRef;
{$ENDIF}
{$IF Defined(MSWINDOWS)}
  SyncEvent := CreateEvent(nil, True, False, nil);
  if SyncEvent = 0 then
    RaiseLastOSError;
{$ELSEIF Defined(POSIX)}
  if pipe(SyncEvent) < 0 then
    RaiseLastOSError;
{$ENDIF POSIX}
end;

procedure DoneThreadSynchronization;
begin
  ThreadLock.DisposeOf;
{$IFDEF AUTOREFCOUNT}
  ThreadLock.__ObjRelease;
{$ENDIF}
{$IF Defined(MSWINDOWS)}
  CloseHandle(SyncEvent);
{$ELSEIF Defined(POSIX)}
  __close(SyncEvent.ReadDes);
  __close(SyncEvent.WriteDes);
{$ENDIF POSIX}
end;

// TODO -odtotoliciu -cTest : WRITE TEST: procedure FreeExternalThreads;
procedure FreeExternalThreads;
var
  I: Integer;
  aList: TList<TThread>;
  LExternalThreads: TThreadList<TThread>;
begin
  Pointer(LExternalThreads) := AtomicExchange(Pointer(ExternalThreads), nil);
  if LExternalThreads <> nil then
  begin
    aList := LExternalThreads.LockList;
    try
      for I := 0 to aList.Count - 1 do
        TObject(aList.Items[I]).DisposeOf;
    finally
      LExternalThreads.UnlockList;
    end;
  end;
  LExternalThreads.Free;
end;

procedure ResetSyncEvent;
{$IF Defined(MSWINDOWS)}
begin
  ResetEvent(SyncEvent);
end;
{$ELSEIF Defined(POSIX)}
var
  nRead: Integer;
  Dummy: Byte;
begin
  if (ioctl(SyncEvent.ReadDes, FIONREAD, @nRead) = 0) and (nRead > 0) then
    __read(SyncEvent.ReadDes, @Dummy, SizeOf(Dummy));
end;
{$ELSE OTHERPLATFORM}
  {$MESSAGE Fatal 'Method not implemented for Platform'}
{$ENDIF OTHERPLATFORM}

procedure WaitForSyncEvent(Timeout: Integer);
{$IF Defined(MSWINDOWS)}
begin
  if WaitForSingleObject(SyncEvent, Timeout) = WAIT_OBJECT_0 then
    ResetSyncEvent;
end;
{$ELSEIF Defined(POSIX)}
var
  EventFds: fd_set;
  Tm: timeval;
begin
  __FD_ZERO(EventFds);
  __FD_SET(SyncEvent.ReadDes, EventFds);
  Tm.tv_sec := Timeout div 1000;
  Tm.tv_usec := (Timeout mod 1000) * 1000;
  if select(SyncEvent.ReadDes + 1, @EventFds, nil, nil, @Tm) > 0 then
    ResetSyncEvent;
end;
{$ELSE OTHERPLATFORM}
  {$MESSAGE Fatal 'Method not implemented for Platform'}
{$ENDIF OTHERPLATFORM}

procedure SignalSyncEvent;
{$IF Defined(MSWINDOWS)}
begin
  SetEvent(SyncEvent);
end;
{$ELSEIF Defined(POSIX)}
const
  Dummy: Byte = 0;
var
  nRead: Integer;
begin
  if (ioctl(SyncEvent.ReadDes, FIONREAD, @nRead) = 0) and (nRead = 0) then
    __write(SyncEvent.WriteDes, @Dummy, SizeOf(Dummy));
end;
{$ELSE OTHERPLATFORM}
  {$MESSAGE Fatal 'Method not implemented for Platform'}
{$ENDIF OTHERPLATFORM}

function CheckSynchronize(Timeout: Integer = 0): Boolean;
var
  SyncProc: PSyncProc;
  LocalSyncList: TList;
begin
{$IF Defined(MSWINDOWS)}
  if SyncEvent = 0 then
    Exit(False);
{$ELSEIF Defined(POSIX)}
  if (SyncEvent.ReadDes = 0) or (SyncEvent.WriteDes = 0) then
    Exit(False);
{$ENDIF POSIX}
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    raise EThread.CreateResFmt(@SCheckSynchronizeError, [TThread.CurrentThread.ThreadID]);
  if Timeout > 0 then
    WaitForSyncEvent(Timeout)
  else
    ResetSyncEvent;
  LocalSyncList := nil;
  TMonitor.Enter(ThreadLock);
  try
    Pointer(LocalSyncList) := AtomicExchange(Pointer(SyncList), Pointer(LocalSyncList));
    try
      Result := (LocalSyncList <> nil) and (LocalSyncList.Count > 0);
      if Result then
      begin
        while LocalSyncList.Count > 0 do
        begin
          SyncProc := LocalSyncList[0];
          LocalSyncList.Delete(0);
          TMonitor.Exit(ThreadLock);
          try
            try
              if Assigned(SyncProc.SyncRec.FMethod) then
                SyncProc.SyncRec.FMethod()
              else if Assigned(SyncProc.SyncRec.FProcedure) then
                SyncProc.SyncRec.FProcedure();
            except
              if not SyncProc.Queued then
                SyncProc.SyncRec.FSynchronizeException := AcquireExceptionObject
              else if Assigned(ApplicationHandleException) then
                ApplicationHandleException(SyncProc.SyncRec.FThread);
            end;
          finally
            SyncProc.SyncRec.FThread := nil;
            TMonitor.Enter(ThreadLock);
          end;
          if not SyncProc.Queued then
            TMonitor.Pulse(SyncProc.Signal)
          else
          begin
            Dispose(SyncProc.SyncRec);
            Dispose(SyncProc);
          end;
        end;
      end;
    finally
      LocalSyncList.Free;
    end;
  finally
    TMonitor.Exit(ThreadLock);
  end;
end;

function ThreadProc(const Thread: TThread): Integer;
var
  FreeThread: Boolean;
{$IFDEF MACOS}
  pool: Pointer;
{$ENDIF MACOS}
begin
{$IFDEF AUTOREFCOUNT}
  Thread.__ObjAddRef; // this ensures the instance remains for as long as the thread is running
{$ENDIF}
  TThread.FCurrentThread := Thread;
{$IF Defined(POSIX)}
  if Thread.FSuspended then
    pthread_mutex_lock(Thread.FCreateSuspendedMutex);
{$ENDIF POSIX}
{$IFDEF MACOS}
  // Register the auto release pool
  pool := objc_msgSend(objc_msgSend(objc_getClass('NSAutoreleasePool'),
                                    sel_getUid('alloc')), sel_getUid('init'));
{$ENDIF MACOS}
  try
    Thread.FStarted := True;
    if not Thread.Terminated then
    try
      Thread.Execute;
    except
      Thread.FFatalException := AcquireExceptionObject;
    end;
  finally
    Result := Thread.FReturnValue;
    FreeThread := Thread.FFreeOnTerminate;
    Thread.DoTerminate;
    Thread.FFinished := True;
    SignalSyncEvent;
    if FreeThread then
    begin
      Thread.DisposeOf;
{$IFDEF AUTOREFCOUNT}
      Thread.__ObjRelease; // This will clear the thread reference that was added by setting FreeOnTerminate.
{$ENDIF}
    end;
{$IFDEF AUTOREFCOUNT}
    Thread.__ObjRelease; // This will clear the thread reference we added above. This may initiate disposal.
{$ENDIF}
{$IFDEF USE_LIBICU}
    // Destroy Collator Cache
    if IsICUAvailable then
      ClearCollatorCache;
{$ENDIF}
{$IF Defined(MSWINDOWS)}
    EndThread(Result);
{$ELSEIF Defined(POSIX)}
{$IFDEF MACOS}
    // Last thing to do in thread is to drain the pool
    objc_msgSend(pool, sel_getUid('drain'));
{$ENDIF MACOS}
{$IFDEF ANDROID}
    // Detach the NativeActivity virtual machine to ensure the proper relase of JNI context attached to the current thread
    PJavaVM(System.JavaMachine)^.DetachCurrentThread(PJavaVM(System.JavaMachine));
{$ENDIF ANDROID}
    // Directly call pthread_exit since EndThread will detach the thread causing
    // the pthread_join in TThread.WaitFor to fail.  Also, make sure the EndThreadProc
    // is called just like EndThread would do. EndThreadProc should not return
    // and call pthread_exit itself.
    if Assigned(EndThreadProc) then
      EndThreadProc(Result);
    pthread_exit(Result);
{$ENDIF POSIX}
  end;
end;

constructor TThread.Create;
begin
  Create(False);
end;

constructor TThread.Create(CreateSuspended: Boolean);
{$IFDEF POSIX}
var
  ErrCode: Integer;
{$ENDIF POSIX}
begin
  inherited Create;
  FSuspended := not FExternalThread;
  FCreateSuspended := CreateSuspended and not FExternalThread;
  if not FExternalThread then
  begin
{$IF Defined(MSWINDOWS)}
    FHandle := BeginThread(nil, 0, @ThreadProc, Pointer(Self), CREATE_SUSPENDED, FThreadID);
    if FHandle = 0 then
      raise EThread.CreateResFmt(@SThreadCreateError, [SysErrorMessage(GetLastError)]);
{$ELSEIF Defined(POSIX)}
    pthread_mutex_init(FCreateSuspendedMutex, nil);
    pthread_mutex_lock(FCreateSuspendedMutex);
    ErrCode := BeginThread(nil, @ThreadProc, Pointer(Self), FThreadID);
    if ErrCode <> 0 then
      raise EThread.CreateResFmt(@SThreadCreateError, [SysErrorMessage(ErrCode)]);
{$ENDIF POSIX}
  end else
  begin
{$IFDEF MSWINDOWS}
    FHandle := Winapi.Windows.GetCurrentThread;
{$ENDIF MSWINDOWS}
    FThreadId := GetCurrentThreadId;
  end;
end;

class constructor TThread.Create;
begin
  InitThreadSynchronization;
  FProcessorCount := System.CPUCount;
end;

class function TThread.CreateAnonymousThread(const ThreadProc: TProc): TThread;
begin
  Result := TAnonymousThread.Create(ThreadProc);
end;

destructor TThread.Destroy;
begin
  if (FThreadID <> 0) and not FFinished and not FExternalThread then
  begin
    Terminate;
    if FCreateSuspended or FSuspended then
      Resume;
{$IFDEF MSWINDOWS}
    while not FStarted do
{$ELSE}
    while not ((not FCreateSuspended or FInitialSuspendDone) and FStarted) do
{$ENDIF}
      Yield;
    WaitFor;
  end;
  RemoveQueuedEvents(Self);
{$IF Defined(MSWINDOWS)}
  if (FHandle <> 0) and not FExternalThread then CloseHandle(FHandle);
{$ELSEIF Defined(POSIX)}
  // This final check is to ensure that even if the thread was never waited on
  // its resources will be freed.
  if (FThreadID <> 0) and not FExternalThread then pthread_detach(pthread_t(FThreadID));
  pthread_mutex_destroy(FCreateSuspendedMutex);
{$ENDIF POSIX}
  inherited Destroy;
  FFatalException.Free;
end;

class destructor TThread.Destroy;
begin
  FreeAndNil(SyncList);
  FreeExternalThreads;
  DoneThreadSynchronization;
end;

procedure TThread.AfterConstruction;
begin
  if not FCreateSuspended and not FExternalThread then
    InternalStart(True);
end;

class function TThread.CheckTerminated: Boolean;
var
  Thread: TThread;
begin
  Thread := CurrentThread;
  if Thread is TExternalThread then
    raise EThreadExternalException.CreateRes(@SThreadExternalCheckTerminated);
  Result := Thread.Terminated;
end;

procedure TThread.CheckThreadError(ErrCode: Integer);
begin
  if ErrCode <> 0 then
    raise EThread.CreateResFmt(@SThreadError, [SysErrorMessage(ErrCode), ErrCode]);
end;

procedure TThread.CheckThreadError(Success: Boolean);
begin
  if not Success then
    CheckThreadError(GetLastError);
end;

procedure TThread.CallOnTerminate;
begin
  if Assigned(FOnTerminate) then FOnTerminate(Self);
end;

procedure TThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then Synchronize(CallOnTerminate);
end;

procedure TThread.TerminatedSet;
begin
end;

class function TThread.GetCPUUsage(var PrevSystemTimes: TSystemTimes): Integer;
var
  CurSystemTimes: TSystemTimes;
  Usage, Idle: UInt64;
begin
  Result := 0;
  if GetSystemTimes(CurSystemTimes) then
  begin
    Usage :=
      (CurSystemTimes.UserTime - PrevSystemTimes.UserTime) +
      (CurSystemTimes.KernelTime - PrevSystemTimes.KernelTime) +
      (CurSystemTimes.NiceTime - PrevSystemTimes.NiceTime);
    Idle := CurSystemTimes.IdleTime - PrevSystemTimes.IdleTime;
    if Usage > Idle then
      Result := (Usage - Idle) * 100 div Usage;
    PrevSystemTimes := CurSystemTimes;
  end;
end;

class function TThread.GetCurrentThread: TThread;
var
  ExternalThread: TThread;
  LExternalThreads: TThreadList<TThread>;
begin
  if FCurrentThread = nil then
  begin
    ExternalThread := TExternalThread.Create;
    if ExternalThreads = nil then
    begin
      LExternalThreads := TThreadList<TThread>.Create;
      if AtomicCmpExchange(Pointer(ExternalThreads), Pointer(LExternalThreads), nil) <> nil then
        LExternalThreads.Free;
      {$IFDEF AUTOREFCOUNT}
      ExternalThreads.__ObjAddRef;
      {$ENDIF AUTOREFCOUNT}
    end;
    ExternalThreads.Add(ExternalThread);
    FCurrentThread := ExternalThread;
  end;
  Result := FCurrentThread;
end;

class function TThread.GetIsSingleProcessor: Boolean;
begin
  Result := FProcessorCount < 2;
end;

procedure TThread.InternalStart(Force: Boolean);
begin
  if (FCreateSuspended or Force) and not FFinished and not FExternalThread then
  begin
    FSuspended := False;
    FCreateSuspended := False;
{$IF Defined(MSWINDOWS)}
    if ResumeThread(FHandle) <> 1 then
      raise EThread.Create(SThreadStartError);
{$ELSEIF Defined(POSIX)}
    pthread_mutex_unlock(FCreateSuspendedMutex);
{$ENDIF}
  end else
    raise EThread.Create(SThreadStartError);
end;

{$IF Defined(MSWINDOWS)}
const
  Priorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := GetThreadPriority(FHandle);
  CheckThreadError(P <> THREAD_PRIORITY_ERROR_RETURN);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then Result := I;
end;

procedure TThread.SetPriority(Value: TThreadPriority);
begin
  CheckThreadError(SetThreadPriority(FHandle, Priorities[Value]));
end;

{$ELSEIF Defined(POSIX)}
function TThread.GetPriority: Integer;
var
  P: Integer;
  J: sched_param;
begin
  {
    Posix Priority is based on the Schedule policy.
    There are 3 different kinds of policy.  See SetPolicy.

        Policy          Type         Priority
      ----------      --------       --------
      SCHED_RR        RealTime         1-99
      SCHED_FIFO      RealTime         1-99
      SCHED_OTHER     Regular           0

    SCHED_RR and SCHED_FIFO can only be set by root.
  }
  CheckThreadError(pthread_getschedparam(pthread_t(FThreadID), P, J));
  Result := J.sched_priority;
end;

{
  Note that to fully utilize Posix Scheduling, see SetPolicy.
}
procedure TThread.SetPriority(Value: Integer);
var
  P: sched_param;
begin
  if Value <> Priority then
  begin
    P.sched_priority := Value;
    CheckThreadError(pthread_setschedparam(pthread_t(FThreadID), Policy, P));
  end;
end;

function TThread.GetPolicy: Integer;
var
  J: sched_param;
begin
  CheckThreadError(pthread_getschedparam(pthread_t(FThreadID), Result, J));
end;

{
  Note that to fully utilize Posix Scheduling, SetPolicy needs to
  be used as well.  See SetPriority for the relationship between these
  methods.
}
procedure TThread.SetPolicy(Value: Integer);
var
  P: sched_param;
begin
  if Value <> Policy then
  begin
    P.sched_priority := GetPriority;
    CheckThreadError(pthread_setschedparam(pthread_t(FThreadID), Value, P));
  end;
end;
{$ENDIF POSIX}

{$IFDEF MACOS}
var
  _mach_host_self: function: mach_port_t cdecl;
{$ENDIF MACOS}

class function TThread.GetSystemTimes(out SystemTimes: TSystemTimes): Boolean;
{$IF Defined(MSWINDOWS)}
var
  Idle, User, Kernel: TFileTime;
begin
  Result := Winapi.Windows.GetSystemTimes(Idle, Kernel, User);
  if Result then
  begin
    SystemTimes.IdleTime := UInt64(Idle.dwHighDateTime) shl 32 or Idle.dwLowDateTime;
    SystemTimes.Usertime := UInt64(User.dwHighDateTime) shl 32 or User.dwLowDateTime;
    SystemTimes.KernelTime := UInt64(Kernel.dwHighDateTime) shl 32 or Kernel.dwLowDateTime;
    SystemTimes.NiceTime := 0;
  end;
end;
{$ELSEIF Defined(MACOS)}
var
  Info: host_cpu_load_info_t;
  CPUInfoSize: mach_msg_type_number_t;
  NumCPUs: natural_t;
  I: Integer;
  libcmod: HMODULE;
begin
  // TODO: Figure out why referencing mach_host_self directly causes the unit tests to fail on Lion. Oddly
  // if the call to fstat is removed from UnitTest.System.TextInOut the direct call to mach_host_self can remain
  // and the tests complete (failures in the disabled test, notwithstanding).
  if @_mach_host_self = nil then
  begin
    libcmod := LoadLibrary(libc);
    if libcmod <> 0 then
    try
       _mach_host_self := GetProcAddress(libcmod, 'mach_host_self');
    finally
      FreeLibrary(libcmod);
    end;
  end;
  Result := (@_mach_host_self <> nil) and (host_processor_info(_mach_host_self, PROCESSOR__CPU_LOAD_INFO, NumCPUs, processor_info_array_t(Info), CPUInfoSize) = KERN_SUCCESS);
  if Result then
  try
    SystemTimes.IdleTime := 0;
    SystemTimes.UserTIme := 0;
    SystemTimes.KernelTime := 0;
    SystemTimes.NiceTime := 0;
    for I := 0 to NumCPUs - 1 do
    begin
      Inc(SystemTimes.IdleTime, Info[I].cpu_ticks[CPU_STATE_IDLE]);
      Inc(SystemTimes.UserTime, Info[I].cpu_ticks[CPU_STATE_USER]);
      // include idle time in kernel time in order to be consistent with Windows.
      Inc(SystemTimes.KernelTime, Info[I].cpu_ticks[CPU_STATE_SYSTEM] + Info[I].cpu_ticks[CPU_STATE_IDLE]);
      Inc(SystemTimes.NiceTime, Info[I].cpu_ticks[CPU_STATE_NICE]);
    end;
  finally
    vm_deallocate(mach_task_self, vm_address_t(Info), CPUInfoSize * SizeOf(Integer));
  end;
end;
{$ELSEIF Defined(ANDROID) or Defined(LINUX)}
const
  BufferSize = 1024;
var
  FileDescriptor: Integer;
  MarshalBuffer: MarshaledAString;
  Content, Line: string;
  BytesReaded : Integer;
  Lines, LineSegments: TArray<string>;
begin
  Result := False;
  // read the stats from /proc/stat
  FileDescriptor := __open('/proc/stat', O_RDONLY, 0);
  if FileDescriptor = -1 then
    Exit;

  MarshalBuffer := AllocMem(BufferSize + 1);
  repeat
    BytesReaded := __read(FileDescriptor, Pointer(MarshalBuffer), BufferSize);
    if BytesReaded > 0 then
      Content := Content + string(MarshalBuffer);
  until (BytesReaded < BufferSize);
  __close(FileDescriptor);
  FreeMem(MarshalBuffer);

  SystemTimes.UserTime := 0;
  SystemTimes.NiceTime := 0;
  SystemTimes.KernelTime := 0;
  SystemTimes.IdleTime := 0;

  Lines := Content.Split([#10]);
  for Line in Lines do
  begin
    if Line.StartsWith('cpu') then
    begin
      LineSegments := Line.Split([' '], TStringSplitOptions.ExcludeEmpty);
      Inc(SystemTimes.UserTime, StrToInt64(LineSegments[1]));
      Inc(SystemTimes.NiceTime, StrToInt64(LineSegments[2]));
      // include idle time in kernel time in order to be consistent with Windows.
      Inc(SystemTimes.KernelTime, StrToInt64(LineSegments[3]) + StrToInt64(LineSegments[4]));
      Inc(SystemTimes.IdleTime, StrToInt64(LineSegments[4]));
      Result := True
    end
    else
      Break;
  end;
end;
{$ELSE OTHERPLATFORM}
  {$MESSAGE Fatal 'Method not implemented for Platform'}
{$ENDIF OTHERPLATFORM}

procedure TThread.SetFreeOnTerminate(Value: Boolean);
begin
  if Value <> FFreeOnTerminate then
  begin
{$IFDEF AUTOREFCOUNT}
    if Value then
      __ObjAddRef;
{$ENDIF}
    FFreeOnTerminate := Value;
{$IFDEF AUTOREFCOUNT}
    if not Value then
      __ObjRelease;
{$ENDIF}
  end;
end;

class function TThread.GetTickCount: Cardinal;
{$IF Defined(MSWINDOWS)}
begin
  Result := Winapi.Windows.GetTickCount;
end;
{$ELSEIF Defined(MACOS)}
begin
  Result := AbsoluteToNanoseconds(mach_absolute_time) div 1000000;
end;
{$ELSEIF Defined(POSIX)}
var
  res: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @res);
  Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) div 1000000;
end;
{$ELSE OTHERPLATFORM}
  {$MESSAGE Fatal 'Method not implemented for Platform'}
{$ENDIF OTHERPLATFORM}

procedure TThread.Queue(AMethod: TThreadMethod);
begin
  Queue(Self, AMethod);
end;

procedure TThread.Queue(AThreadProc: TThreadProcedure);
begin
  Queue(Self, AThreadProc);
end;

class procedure TThread.Queue(const AThread: TThread; AMethod: TThreadMethod);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  try
    LSynchronize.FThread := AThread;
    LSynchronize.FSynchronizeException := nil;
    LSynchronize.FMethod := AMethod;
    Synchronize(LSynchronize, True);
  finally
    if MainThreadID = CurrentThread.ThreadID then
      Dispose(LSynchronize);
  end;
end;

class procedure TThread.Queue(const AThread: TThread; AThreadProc: TThreadProcedure);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  try
    LSynchronize.FThread := AThread;
    LSynchronize.FSynchronizeException := nil;
    LSynchronize.FMethod := nil;
    LSynchronize.FProcedure := AThreadProc;
    Synchronize(LSynchronize, True);
  finally
    if MainThreadID = CurrentThread.ThreadID then
      Dispose(LSynchronize);
  end;
end;

class procedure TThread.RemoveQueuedEvents(const AThread: TThread; AMethod: TThreadMethod);
var
  I: Integer;
  SyncProc: PSyncProc;
begin
  TMonitor.Enter(ThreadLock);
  try
    if SyncList <> nil then
      for I := SyncList.Count - 1 downto 0 do
      begin
        SyncProc := SyncList[I];
        if (SyncProc.Signal = nil) and
          (((AThread <> nil) and (SyncProc.SyncRec.FThread = AThread)) or
            (Assigned(AMethod) and (TMethod(SyncProc.SyncRec.FMethod).Code = TMethod(AMethod).Code) and
             (TMethod(SyncProc.SyncRec.FMethod).Data = TMethod(AMethod).Data))) then
        begin
          SyncList.Delete(I);
          Dispose(SyncProc.SyncRec);
          Dispose(SyncProc);
        end;
      end;
  finally
    TMonitor.Exit(ThreadLock);
  end;
end;

class procedure TThread.SetReturnValue(Value: Integer);
var
  Thread: TThread;
begin
  Thread := CurrentThread;
  if Thread is TExternalThread then
    raise EThreadExternalException.CreateRes(@SThreadExternalSetReturnValue);
  Thread.ReturnValue := Value;
end;

class procedure TThread.StaticQueue(const AThread: TThread; AMethod: TThreadMethod);
begin
  Queue(AThread, AMethod);
end;

class procedure TThread.Synchronize(ASyncRec: PSynchronizeRecord; QueueEvent: Boolean = False; ForceQueue: Boolean = False);
var
  SyncProc: TSyncProc;
  SyncProcPtr: PSyncProc;
begin
  if (CurrentThread.ThreadID = MainThreadID) and not (QueueEvent and ForceQueue) then
  begin
    if Assigned(ASyncRec.FMethod) then
      ASyncRec.FMethod()
    else if Assigned(ASyncRec.FProcedure) then
      ASyncRec.FProcedure();
  end else
  begin
    if QueueEvent then
      New(SyncProcPtr)
    else
      SyncProcPtr := @SyncProc;
    if not QueueEvent then
      SyncProcPtr.Signal := TObject.Create
    else
      SyncProcPtr.Signal := nil;
    try
      TMonitor.Enter(ThreadLock);
      try
        SyncProcPtr.Queued := QueueEvent;
        if SyncList = nil then
          SyncList := TList.Create;
        SyncProcPtr.SyncRec := ASyncRec;
        SyncList.Add(SyncProcPtr);
        SignalSyncEvent;
        if Assigned(WakeMainThread) then
          WakeMainThread(SyncProcPtr.SyncRec.FThread);
        if not QueueEvent then
          TMonitor.Wait(SyncProcPtr.Signal, ThreadLock, INFINITE)
      finally
        TMonitor.Exit(ThreadLock);
      end;
    finally
      if not QueueEvent then
        SyncProcPtr.Signal.Free;
    end;
    if not QueueEvent and Assigned(ASyncRec.FSynchronizeException) then
      raise ASyncRec.FSynchronizeException;
  end;
end;

procedure TThread.Synchronize(AMethod: TThreadMethod);
begin
  Synchronize(Self, AMethod);
end;

procedure TThread.Synchronize(AThreadProc: TThreadProcedure);
begin
  Synchronize(Self, AThreadProc);
end;

class procedure TThread.Synchronize(const AThread: TThread; AMethod: TThreadMethod);
var
  FSynchronize: TSynchronizeRecord;
begin
  FSynchronize.FThread := AThread;
  FSynchronize.FSynchronizeException := nil;
  FSynchronize.FMethod := AMethod;
  FSynchronize.FProcedure := nil;
  Synchronize(@FSynchronize);
end;

class procedure TThread.Synchronize(const AThread: TThread; AThreadProc: TThreadProcedure);
var
  FSynchronize: TSynchronizeRecord;
begin
  FSynchronize.FThread := AThread;
  FSynchronize.FSynchronizeException := nil;
  FSynchronize.FMethod := nil;
  FSynchronize.FProcedure := AThreadProc;
  TThread.Synchronize(@FSynchronize);
end;

class procedure TThread.ForceQueue(const AThread: TThread; const AMethod: TThreadMethod);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  LSynchronize.FThread := AThread;
  LSynchronize.FSynchronizeException := nil;
  LSynchronize.FMethod := AMethod;
  LSynchronize.FProcedure := nil;
  Synchronize(LSynchronize, True, True);
end;

class procedure TThread.ForceQueue(const AThread: TThread; const AThreadProc: TThreadProcedure);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  LSynchronize.FThread := AThread;
  LSynchronize.FSynchronizeException := nil;
  LSynchronize.FMethod := nil;
  LSynchronize.FProcedure := AThreadProc;
  Synchronize(LSynchronize, True, True);
end;

class procedure TThread.StaticSynchronize(const AThread: TThread; AMethod: TThreadMethod);
begin
  Synchronize(AThread, AMethod);
end;

class procedure TThread.RemoveQueuedEvents(const AThread: TThread);
var
  I: Integer;
  SyncProc: PSyncProc;
begin
  TMonitor.Enter(ThreadLock);
  try
    if SyncList <> nil then
      for I := SyncList.Count - 1 downto 0 do
      begin
        SyncProc := SyncList[I];
        if (SyncProc.Signal = nil) and
          ((AThread <> nil) and (SyncProc.SyncRec.FThread = AThread)) then
        begin
          SyncList.Delete(I);
          Dispose(SyncProc.SyncRec);
          Dispose(SyncProc);
        end;
      end;
  finally
    TMonitor.Exit(ThreadLock);
  end;
end;

class procedure TThread.RemoveQueuedEvents(AMethod: TThreadMethod);
begin
  RemoveQueuedEvents(nil, AMethod);
end;

procedure TThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend
    else
      Resume;
end;

class procedure TThread.SpinWait(Iterations: Integer);
{$IF Defined(X86ASM) or Defined(X64ASM)}
asm
    CMP  Iterations, 0
    JNG  @Done
@Loop:
    PAUSE
    DEC  Iterations
    CMP  Iterations, 0
    JG   @Loop
@Done:
end;
{$ELSE PUREPASCAL}
begin
  while Iterations > 0 do
  begin
    System.YieldProcessor;
    Dec(Iterations);
  end;
end;
{$ENDIF PUREPASCAL}

class procedure TThread.Sleep(Timeout: Integer);
begin
{$IF Defined(MSWINDOWS)}
  Winapi.Windows.Sleep(Timeout);
{$ELSEIF Defined(POSIX)}
  usleep(Timeout * 1000);
{$ENDIF POSIX}
end;

class procedure TThread.Yield;
begin
{$IF Defined(MSWINDOWS)}
  SwitchToThread;
{$ELSEIF Defined(POSIX)}
  sched_yield;
{$ENDIF POSIX}
end;

procedure TThread.Start;
begin
  InternalStart(False);
end;

procedure TThread.Suspend;
var
  OldSuspend: Boolean;
begin
  OldSuspend := FSuspended;
  try
    FSuspended := True;
{$IF Defined(MSWINDOWS)}
    CheckThreadError(Integer(SuspendThread(FHandle)) >= 0);
{$ELSEIF Defined(MACOS)}
    CheckThreadError(thread_suspend(pthread_mach_thread_np(pthread_t(FThreadID))));
{$ELSEIF Defined(POSIX)}
{
  Suspend in POSIX is NOT implemented,
  so the thread can not be suspended after start.
}
{$ENDIF POSIX}
  except
    FSuspended := OldSuspend;
    raise;
  end;
end;

procedure TThread.Resume;
{$IF Defined(MSWINDOWS)}
var
  SuspendCount: Integer;
begin
  SuspendCount := ResumeThread(FHandle);
  CheckThreadError(SuspendCount >= 0);
  if SuspendCount = 1 then
    FSuspended := False;
end;
{$ELSEIF Defined(MACOS)}
begin
  if not FInitialSuspendDone then
  begin
    FInitialSuspendDone := True;
    pthread_mutex_unlock(FCreateSuspendedMutex);
  end else
    CheckThreadError(thread_resume(pthread_mach_thread_np(pthread_t(FThreadID))));
  FSuspended := False;
end;
{$ELSEIF Defined(POSIX)}
{
  Resume in POSIX is NOT implemented because the thread can not be suspended
  at runtime (after start). The follow code remains for threads created
  suspended.
}
begin
  if not FInitialSuspendDone then
  begin
    FInitialSuspendDone := True;
    pthread_mutex_unlock(FCreateSuspendedMutex);
  end;
  FSuspended := False;
end;
{$ELSE OTHERPLATFORM}
  {$MESSAGE Fatal 'Method not implemented for Platform'}
{$ENDIF OTHERPLATFORM}

procedure TThread.Terminate;
begin
  if FExternalThread then
    raise EThread.CreateRes(@SThreadExternalTerminate);
  FTerminated := True;
  TerminatedSet;
end;

function TThread.WaitFor: LongWord;
{$IF Defined(MSWINDOWS)}
var
  H: array[0..1] of THandle;
  WaitResult: Cardinal;
{$IF not Declared(System.Embedded)}
  Msg: TMsg;
{$ENDIF}
begin
  if FExternalThread then
    raise EThread.CreateRes(@SThreadExternalWait);
  H[0] := FHandle;
  if CurrentThread.ThreadID = MainThreadID then
  begin
{$IF not Declared(System.Embedded)}
    WaitResult := 0;
{$ENDIF}
    H[1] := SyncEvent;
    repeat
{$IF Defined(NEXTGEN) and Declared(System.Embedded)}
      WaitResult := WaitForMultipleObjects(2, @H, False, 1000);
{$ELSE}
      { This prevents a potential deadlock if the background thread
        does a SendMessage to the foreground thread }
      if WaitResult = WAIT_OBJECT_0 + 2 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      WaitResult := MsgWaitForMultipleObjects(2, H, False, 1000, QS_SENDMESSAGE);
{$ENDIF}
      CheckThreadError(WaitResult <> WAIT_FAILED);
      if WaitResult = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
    until WaitResult = WAIT_OBJECT_0;
  end else WaitForSingleObject(H[0], INFINITE);
  CheckThreadError(GetExitCodeThread(H[0], Result));
end;
{$ELSEIF Defined(POSIX)}
var
  X: Pointer;
  ID: pthread_t;
begin
  if FExternalThread then
    raise EThread.CreateRes(@SThreadExternalWait);
  ID := pthread_t(FThreadID);
  if CurrentThread.ThreadID = MainThreadID then
    while not FFinished do
      CheckSynchronize(1000);
  FThreadID := 0;
  X := @Result;
  CheckThreadError(pthread_join(ID, X));
end;
{$ENDIF POSIX}

{$IFNDEF NEXTGEN}
class procedure TThread.NameThreadForDebugging(AThreadName: AnsiString; AThreadID: TThreadID);
begin
  NameThreadForDebugging(string(AThreadName), AThreadID);
end;
{$ENDIF !NEXTGEN}

class procedure TThread.NameThreadForDebugging(AThreadName: string; AThreadID: TThreadID);
{$IF Defined(MSWINDOWS)}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: MarshaledAString;    // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
var
  ThreadNameInfo: TThreadNameInfo;
  M:TMarshaller;
begin
  if IsDebuggerPresent then
  begin
    ThreadNameInfo.FType := $1000;
    ThreadNameInfo.FName := M.AsAnsi(AThreadName).ToPointer;
    ThreadNameInfo.FThreadID := AThreadID;
    ThreadNameInfo.FFlags := 0;

    try
      RaiseException($406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo);
    except
    end;
  end;
end;
{$ELSE MSWINDOWS}
const
  cExceptionMessage = 'Type=$1000,Name=%s,ThreadID=%d,Flags=0';
  EMBDBKPRESENTNAME = 'EMB_DBK_PRESENT';
{$IF Defined(MACOS)}
  OLDEMBDBKPRESENTNAME = 'EMB_MACOSX_DBK_PRESENT'; // ToDo: we should keep this name for backward compatibility for few years.
{$ENDIF}
begin
{$IF Defined(MACOS)}
  if (getenv(EMBDBKPRESENTNAME) <> nil) or (getenv(OLDEMBDBKPRESENTNAME) <> nil) then
{$ELSEIF Defined(ANDROID)}
  if (System.DebugHook <> 0) or (getenv(EMBDBKPRESENTNAME) <> nil) then
{$ELSE}
  if (getenv(EMBDBKPRESENTNAME) <> nil) then
{$ENDIF}
  begin
    try
      raise EThreadNameException.Create(Format(cExceptionMessage, [AThreadName, AThreadID]));
    except
    end;
  end;
end;
{$ENDIF !MSWINDOWS}

{ DefaultAttribute }

constructor DefaultAttribute.Create(const DefaultValue: Boolean);
begin
  inherited Create;
  FValue := Ord(DefaultValue);
end;

{$IFNDEF NEXTGEN}
constructor DefaultAttribute.Create(const DefaultValue: AnsiChar);
begin
  inherited Create;
  FValue := DefaultValue;
end;
{$ENDIF !NEXTGEN}

constructor DefaultAttribute.Create(const DefaultValue: Char);
begin
  inherited Create;
  FValue := DefaultValue;
end;

constructor DefaultAttribute.Create(const DefaultValue: Integer);
begin
  inherited Create;
  FValue := DefaultValue;
end;

constructor DefaultAttribute.Create(const DefaultValue: Cardinal);
begin
  inherited Create;
  FValue := DefaultValue;
end;

constructor DefaultAttribute.Create(const DefaultValue: Int64);
begin
  inherited Create;
  FValue := DefaultValue;
end;

constructor DefaultAttribute.Create(const DefaultValue: UInt64);
begin
  inherited Create;
  FValue := DefaultValue;
end;

constructor DefaultAttribute.Create(const DefaultValue: String);
begin
  inherited Create;
  FValue := DefaultValue;
end;

constructor DefaultAttribute.Create(const DefaultValue: Extended);
begin
  inherited Create;
  FValue := DefaultValue;
end;

{ NoDefaultAttribute }

constructor NoDefaultAttribute.Create;
begin
  inherited Create;
  FValue := System.Variants.Null;
end;

{ ObservableMemberAttribute }

constructor ObservableMemberAttribute.Create(const AMemberName: string);
begin
  inherited Create;
  FMemberName := AMemberName;
end;

{ TComponentEnumerator }

constructor TComponentEnumerator.Create(AComponent: TComponent);
begin
  inherited Create;
  FIndex := -1;
  FComponent := AComponent;
end;

function TComponentEnumerator.GetCurrent: TComponent;
begin
  Result := FComponent.Components[FIndex];
end;

function TComponentEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FComponent.ComponentCount - 1;
  if Result then
    Inc(FIndex);
end;

{ TComponent }

constructor TComponent.Create(AOwner: TComponent);
begin
  FComponentStyle := [csInheritable];
  if AOwner <> nil then AOwner.InsertComponent(Self);
end;

destructor TComponent.Destroy;
begin
  Destroying;
  RemoveFreeNotifications;
  DestroyComponents;
  if FOwner <> nil then FOwner.RemoveComponent(Self);
  FObservers.Free;
  inherited Destroy;
end;

procedure TComponent.AsyncSchedule(const ASyncResult: TBaseAsyncResult);
var
  LState: TComponentState;
begin
  LState := FComponentState; // Snag a local copy
  if csDestroying in LState then
    raise EInvalidOperation.CreateRes(@sBeginInvokeDestroying);
  TThread.Queue(nil, ASyncResult.DoAsyncDispatch);
end;

procedure TComponent.BeforeDestruction;
begin
  if not (csDestroying in ComponentState) then
    Destroying;
end;

function TComponent.BeginInvoke(const AProc: TProc; const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncProcedureResult.Create(AProc, AContext, Self).Invoke;
end;

function TComponent.BeginInvoke(const AProc: TASyncProcedureEvent; const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncProcedureResultEvent.Create(AProc, AContext, Self).Invoke;
end;

function TComponent.BeginInvoke(const AFunc: TAsyncFunctionEvent; const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncFunctionResultEvent.Create(AFunc, AContext, Self).Invoke;
end;

function TComponent.BeginInvoke(const AProc: TAsyncConstArrayProcedureEvent; const Params: array of const;
  const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncConstArrayProcedureResult.Create(AProc, AContext, Self, Params).Invoke;
end;

function TComponent.BeginInvoke(const AFunc: TAsyncConstArrayFunctionEvent; const Params: array of const;
  const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncConstArrayFunctionResult.Create(AFunc, AContext, Self, Params).Invoke;
end;

function TComponent.BeginInvoke(const AProc: TAsyncConstArrayProc; const Params: array of const;
  const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncConstArrayProcResult.Create(AProc, AContext, Self, Params).Invoke;
end;

function TComponent.BeginInvoke<TResult>(const AFunc: TAsyncConstArrayFunc<TResult>; const Params: array of const;
  const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncConstArrayFuncResult<TResult>.Create(AFunc, AContext, Self, Params).Invoke;
end;

function TComponent.BeginInvoke<TResult>(const AFunc: TFunc<TResult>; const AContext: TObject): IAsyncResult;
begin
  Result := TAsyncFunctionResult<TResult>.Create(AFunc, AContext, Self).Invoke;
end;

procedure TComponent.RemoveFreeNotifications;
begin
  if FFreeNotifies <> nil then
  begin
    while Assigned(FFreeNotifies) and (FFreeNotifies.Count > 0) do
      TComponent(FFreeNotifies[FFreeNotifies.Count - 1]).Notification(Self, opRemove);
    FreeAndNil(FFreeNotifies);
  end;
end;

procedure TComponent.FreeNotification(AComponent: TComponent);
begin
  if (Owner = nil) or (AComponent.Owner <> Owner) then
  begin
    // Never acquire a reference to a component that is being deleted.
    Assert(not (csDestroying in (ComponentState + AComponent.ComponentState)),
      'Component already destroyed: '+ Name + ' ClassName: ' + ClassName);  // do not localize
    if not Assigned(FFreeNotifies) then FFreeNotifies := TList<TComponent>.Create;
    if FFreeNotifies.IndexOf(AComponent) < 0 then
    begin
      FFreeNotifies.Add(AComponent);
      AComponent.FreeNotification(Self);
    end;
  end;
  Include(FComponentState, csFreeNotification);
end;

procedure TComponent.ReadDeltaState;
var
  Handled: Boolean;
begin
  if not (csDesigning in ComponentState) then
  begin
     Handled := False;
     DoGetDeltaStreams(ReadDeltaStream, Handled);
     if not Handled then
       GetDeltaStreams(ReadDeltaStream);
  end;
end;

procedure TComponent.ReadDeltaStream(const S: TStream);
begin
  S.ReadComponent(Self);
end;

procedure TComponent.ReadLeft(Reader: TReader);
begin
  LongRec(FDesignInfo).Lo := Reader.ReadInteger;
end;

procedure TComponent.ReadTop(Reader: TReader);
begin
  LongRec(FDesignInfo).Hi := Reader.ReadInteger;
end;

procedure TComponent.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(FDesignInfo).Lo);
end;

procedure TComponent.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(FDesignInfo).Hi);
end;

procedure TComponent.Insert(AComponent: TComponent);
begin
  if FComponents = nil then FComponents := TList<TComponent>.Create;
  FComponents.Add(AComponent);
  if FSortedComponents <> nil then
    AddSortedComponent(AComponent);
  AComponent.FOwner := Self;
end;

procedure TComponent.Remove(AComponent: TComponent);
var
  Count: Integer;
begin
  AComponent.FOwner := nil;
  Count := FComponents.Count;
  if Count > 0 then
  begin
    { On destruction usually the last item is deleted first }
    if FComponents[Count - 1] = AComponent then
      FComponents.Delete(Count - 1)
    else
      FComponents.Remove(AComponent);

    if FSortedComponents <> nil then
      FSortedComponents.Remove(AComponent);
  end;
  if FComponents.Count = 0 then
  begin
    FreeAndNil(FSortedComponents);
    FreeAndNil(FComponents);
  end;
end;

procedure TComponent.InsertComponent(const AComponent: TComponent);
var
  Notifier: IDesignerNotify;
begin
  GetComponentDesigner(Self, Notifier);
  if Notifier <> nil then
    Notifier.CanInsertComponent(AComponent);
  AComponent.ValidateContainer(Self);
  if AComponent.FOwner <> nil then
    AComponent.FOwner.RemoveComponent(AComponent);
  ValidateRename(AComponent, '', AComponent.FName);
  Insert(AComponent);
  AComponent.SetReference(True);
  if csDesigning in ComponentState then
    AComponent.SetDesigning(True);
  Notification(AComponent, opInsert);
end;

procedure TComponent.RemoveComponent(const AComponent: TComponent);
begin
  ValidateRename(AComponent, AComponent.FName, '');
  Notification(AComponent, opRemove);
  AComponent.SetReference(False);
  Remove(AComponent);
end;

procedure TComponent.DestroyComponents;
var
  Instance: TComponent;
begin
  FreeAndNil(FSortedComponents);
  while FComponents <> nil do
  begin
    Instance := FComponents.Last;
    if (csFreeNotification in Instance.FComponentState)
      or (FComponentState * [csDesigning, csInline] = [csDesigning, csInline]) then
      RemoveComponent(Instance)
    else
      Remove(Instance);
    Instance.DisposeOf;
  end;
end;

procedure TComponent.Destroying;
var
  I: Integer;
begin
  if not (csDestroying in FComponentState) then
  begin
    Include(FComponentState, csDestroying);
    if FComponents <> nil then
      for I := 0 to FComponents.Count - 1 do
        TComponent(FComponents[I]).Destroying;
  end;
end;

procedure TComponent.DoGetDeltaStreams(Proc: TGetStreamProc; var Handled: Boolean);
begin
  if Assigned(FOnGetDeltaStreams) then
    FOnGetDeltaStreams(Self, Proc, Handled);
end;

procedure TComponent.RemoveNotification(const AComponent: TComponent);
var
  Count: Integer;
begin
  if FFreeNotifies <> nil then
  begin
    Count := FFreeNotifies.Count;
    if Count > 0 then
    begin
      { On destruction usually the last item is deleted first }
      if FFreeNotifies[Count - 1] = AComponent then
        FFreeNotifies.Delete(Count - 1)
      else
        FFreeNotifies.Remove(AComponent);
    end;
    if FFreeNotifies.Count = 0 then
      FreeAndNil(FFreeNotifies);
  end;
end;

procedure TComponent.RemoveFreeNotification(AComponent: TComponent);
begin
  RemoveNotification(AComponent);
  AComponent.RemoveNotification(Self);
end;

procedure TComponent.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  if (Operation = opRemove) and (AComponent <> nil) then
    RemoveFreeNotification(AComponent);
  if FComponents <> nil then
  begin
    I := FComponents.Count - 1;
    while I >= 0 do
    begin
      TComponent(FComponents[I]).Notification(AComponent, Operation);
      Dec(I);
      if I >= FComponents.Count then
        I := FComponents.Count - 1;
    end;
  end;
end;

procedure TComponent.DefineProperties(Filer: TFiler);
var
  Ancestor: TComponent;
// TODO -cELBRUS_LONGINT64 : Verify TComponent.DefineProperties (change LongInt to Integer)
  Info: Integer;
begin
  Info := 0;
  Ancestor := TComponent(Filer.Ancestor);
  if Ancestor <> nil then Info := Ancestor.FDesignInfo;
  Filer.DefineProperty('Left', ReadLeft, WriteLeft,
    LongRec(FDesignInfo).Lo <> LongRec(Info).Lo);
  Filer.DefineProperty('Top', ReadTop, WriteTop,
    LongRec(FDesignInfo).Hi <> LongRec(Info).Hi);
end;

function TComponent.HasParent: Boolean;
begin
  Result := False;
end;

procedure TComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

function TComponent.GetChildOwner: TComponent;
begin
  Result := nil;
end;

function TComponent.GetChildParent: TComponent;
begin
  Result := Self;
end;

function TComponent.GetEnumerator: TComponentEnumerator;
begin
  Result := TComponentEnumerator.Create(Self);
end;

function TComponent.GetNamePath: string;
begin
  Result := FName;
end;

function TComponent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TComponent.SetChildOrder(Child: TComponent; Order: Integer);
begin
end;

function TComponent.GetParentComponent: TComponent;
begin
  Result := nil;
end;

procedure TComponent.SetParentComponent(Value: TComponent);
begin
end;

procedure TComponent.Updating;
begin
  Include(FComponentState, csUpdating);
end;

procedure TComponent.Updated;
begin
  Exclude(FComponentState, csUpdating);
end;

procedure TComponent.Loaded;
begin
  Exclude(FComponentState, csLoading);
end;

procedure TComponent.PaletteCreated;
begin
  // Notification
end;

procedure TComponent.ReadState(Reader: TReader);
begin
  Reader.ReadData(Self);
end;

procedure TComponent.WriteState(Writer: TWriter);
begin
  Writer.WriteData(Self);
end;

procedure TComponent.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
  if (AComponent <> nil) and not SameText(CurName, NewName) and
    (AComponent.Owner = Self) and (FindComponent(NewName) <> nil) then
    raise EComponentError.CreateResFmt(@SDuplicateName, [NewName]);
  if (csDesigning in ComponentState) and (Owner <> nil) then
    Owner.ValidateRename(AComponent, CurName, NewName);
end;

procedure TComponent.ValidateContainer(AComponent: TComponent);
begin
  AComponent.ValidateInsert(Self);
end;

procedure TComponent.ValidateInsert(AComponent: TComponent);
begin
end;

class constructor TComponent.Create;
begin
  FComparer := TDelegatedComparer<TComponent>.Create(
    function(const Item1, Item2: TComponent): Integer
    begin
      Result := CompareText(Item1.Name, Item2.Name);
    end);
end;

function TComponent.FindComponent(const AName: string): TComponent;
var
  I: Integer;
begin
  Result := nil;
  if (AName <> '') and (FComponents <> nil) then
  begin
    if FSortedComponents = nil then
    begin
      { Fill the sorted list (and sort it) }
      FSortedComponents := TList<TComponent>.Create;
      FSortedComponents.Count := FComponents.Count;
      for I := 0 to FComponents.Count - 1 do
        FSortedComponents[I] := FComponents[I];

      FSortedComponents.Sort(FComparer);
    end;
    Result := FindSortedComponent(AName, I);
  end;
end;

function TComponent.FindSortedComponent(const AName: string; var Index: Integer): TComponent;
var
  L, H, I, C: Integer;
begin
  L := 0;
  H := FSortedComponents.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Result := TComponent(FSortedComponents.List[I]);
    C := CompareText(Result.Name, AName);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Index := I;
        Exit;
      end;
    end;
  end;
  Index := L;
  Result := nil;
end;

procedure TComponent.AddSortedComponent(const AComponent: TComponent);
var
  Index: Integer;
begin
  FindSortedComponent(AComponent.Name, Index);
  FSortedComponents.Insert(Index, AComponent);
end;

procedure TComponent.RemoveSortedComponent(const AComponent: TComponent);
begin
  FSortedComponents.Remove(AComponent);
end;

procedure TComponent.SetName(const NewName: TComponentName);
begin
  if FName <> NewName then
  begin
    if (NewName <> '') and not IsValidIdent(NewName) then
      raise EComponentError.CreateResFmt(@SInvalidName, [NewName]);
    if FOwner <> nil then
      FOwner.ValidateRename(Self, FName, NewName) else
      ValidateRename(nil, FName, NewName);
    SetReference(False);
    ChangeName(NewName);
    SetReference(True);
  end;
end;

procedure TComponent.ChangeName(const NewName: TComponentName);
begin
  FName := NewName;
  if (FOwner <> nil) and (FOwner.FSortedComponents <> nil) then
  begin
    FOwner.RemoveSortedComponent(Self);
    FOwner.AddSortedComponent(Self);
  end;
end;

function TComponent.GetComponentIndex: Integer;
begin
  if (FOwner <> nil) and (FOwner.FComponents <> nil) then
    Result := FOwner.FComponents.IndexOf(Self) else
    Result := -1;
end;

procedure TComponent.GetDeltaStreams(Proc: TGetStreamProc);
begin
  // Fill in with code to locate and iterate over n delta streams
end;

function TComponent.GetComponent(AIndex: Integer): TComponent;
begin
  if FComponents = nil then TList.Error(@SListIndexError, AIndex);
  Result := FComponents[AIndex];
end;

function TComponent.GetComponentCount: Integer;
begin
  if FComponents <> nil then
    Result := FComponents.Count else
    Result := 0;
end;

procedure TComponent.SetComponentIndex(Value: Integer);
var
  I, Count: Integer;
begin
  if FOwner <> nil then
  begin
    I := FOwner.FComponents.IndexOf(Self);
    if I >= 0 then
    begin
      Count := FOwner.FComponents.Count;
      if Value < 0 then Value := 0;
      if Value >= Count then Value := Count - 1;
      if Value <> I then
      begin
        FOwner.FComponents.Delete(I);
        FOwner.FComponents.Insert(Value, Self);
      end;
    end;
  end;
end;

function TComponent.GetObservers: TObservers;
begin
  if FObservers = nil then
  begin
    FObservers := TObservers.Create;
    FObservers.OnCanObserve := CanObserve;
    FObservers.OnObserverAdded := ObserverAdded;
  end;
  Result := FObservers;
end;

procedure TComponent.SetAncestor(Value: Boolean);
var
  I: Integer;
begin
  if Value then
    Include(FComponentState, csAncestor) else
    Exclude(FComponentState, csAncestor);
  for I := 0 to ComponentCount - 1 do
    Components[I].SetAncestor(Value);
end;

procedure TComponent.SetDesigning(Value, SetChildren: Boolean);
var
  I: Integer;
begin
  if Value then
    Include(FComponentState, csDesigning) else
    Exclude(FComponentState, csDesigning);
  if SetChildren then
    for I := 0 to ComponentCount - 1 do Components[I].SetDesigning(Value);
end;

procedure TComponent.SetInline(Value: Boolean);
begin
  if Value then
    Include(FComponentState, csInline) else
    Exclude(FComponentState, csInline);
end;

procedure TComponent.SetDesignInstance(Value: Boolean);
begin
  if Value then
    Include(FComponentState, csDesignInstance) else
    Exclude(FComponentState, csDesignInstance);
end;

procedure TComponent.SetReference(Enable: Boolean);
var
  Field: ^TComponent;
begin
  if (FOwner <> nil) then
  begin
    Field := FOwner.FieldAddress(FName);
    if Field <> nil then
      if Enable then Field^ := Self else Field^ := nil;
  end;
end;

function TComponent.EndFunctionInvoke(const AsyncResult: IAsyncResult): TObject;
begin
  if AsyncResult is TAsyncFunctionResultEvent then
    Result := (AsyncResult as TAsyncFunctionResultEvent).GetRetVal
  else
    Result := (AsyncResult as TAsyncConstArrayFunctionResult).GetRetVal;
end;

procedure TComponent.EndInvoke(const ASyncResult: IAsyncResult);
begin
  (AsyncResult as TBaseAsyncResult).WaitForCompletion;
end;

function TComponent.EndInvoke<TResult>(const AsyncResult: IAsyncResult): TResult;
begin
  if AsyncResult is TAsyncFunctionResult<TResult> then
    Result := (AsyncResult as TAsyncFunctionResult<TResult>).GetRetVal
  else
    Result := (AsyncResult as TAsyncConstArrayFuncResult<TResult>).GetRetVal;
end;

function TComponent.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := (Action <> nil) and not Action.Suspended and Action.HandlesTarget(Self);
  if Result then
    Action.ExecuteTarget(Self);
end;

function TComponent.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := (Action <> nil) and not Action.Suspended and Action.HandlesTarget(Self);
  if Result then
    Action.UpdateTarget(Self);
end;

procedure TComponent.SetSubComponent(IsSubComponent: Boolean);
begin
  if IsSubComponent then
    Include(FComponentStyle, csSubComponent)
  else
    Exclude(FComponentStyle, csSubComponent);
end;

function TComponent.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
end;

procedure TComponent.ObserverAdded(const ID: Integer; const Observer: IObserver);
begin

end;

function TComponent.GetComObject: IUnknown;
begin
  if FVCLComObject = nil then
  begin
    if Assigned(CreateVCLComObjectProc) then CreateVCLComObjectProc(Self);
    if FVCLComObject = nil then
      raise EComponentError.CreateResFmt(@SNoComSupport, [ClassName]);
  end;
  IVCLComObject(FVCLComObject).QueryInterface(IUnknown, Result);
end;

function TComponent.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  if FVCLComObject <> nil then
    Result := IVCLComObject(FVCLComObject).SafeCallException(
      ExceptObject, ExceptAddr)
  else
    Result := inherited SafeCallException(ExceptObject, ExceptAddr);
end;

procedure TComponent.FreeOnRelease;
begin
  if FVCLComObject <> nil then IVCLComObject(FVCLComObject).FreeOnRelease;
end;

class procedure TComponent.UpdateRegistry(Register: Boolean; const ClassID, ProgID: string);
begin
end;

{ TComponent.IInterface }

function TComponent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if FVCLComObject = nil then
  begin
    if GetInterface(IID, Obj) then Result := S_OK
    else Result := E_NOINTERFACE
  end
  else
    Result := IVCLComObject(FVCLComObject).QueryInterface(IID, Obj);
end;

function TComponent._AddRef: Integer;
begin
  if FVCLComObject = nil then
    Result := -1   // -1 indicates no reference counting is taking place
  else
    Result := IVCLComObject(FVCLComObject)._AddRef;
end;

function TComponent._Release: Integer;
begin
  if FVCLComObject = nil then
    Result := -1   // -1 indicates no reference counting is taking place
  else
    Result := IVCLComObject(FVCLComObject)._Release;
end;

{ TComponent.IDispatch }

function TComponent.GetTypeInfoCount(out Count: Integer): HResult;
begin
  if FVCLComObject = nil then
    Result := E_NOTIMPL
  else
    Result := IVCLComObject(FVCLComObject).GetTypeInfoCount(Count);
end;

function TComponent.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  if FVCLComObject = nil then
    Result := E_NOTIMPL
  else
    Result := IVCLComObject(FVCLComObject).GetTypeInfo(
      Index, LocaleID, TypeInfo);
end;

function TComponent.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  if FVCLComObject = nil then
    Result := E_NOTIMPL
  else
    Result := IVCLComObject(FVCLComObject).GetIDsOfNames(IID, Names,
      NameCount, LocaleID, DispIDs);
end;

function TComponent.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  if FVCLComObject = nil then
    Result := E_NOTIMPL
  else
    Result := IVCLComObject(FVCLComObject).Invoke(DispID, IID, LocaleID,
      Flags, Params, VarResult, ExcepInfo, ArgErr);
end;

{ TComponent.IInterfaceComponentReference.GetComponent
  Return a reference to the component that can be queried at load time to
  obtain an interface.  The name of the reference component will be written to
  the stream (same as a normal component reference) so that it can be located
  again at load time.
  In the case of aggregation, the reference component for an interface might
  not be the same as the class that implements the interface itself.
  Aggregate implementation classes should not implement
  IInterfaceComponentReference, but should defer requests for that interface
  to the controlling component.
}

function TComponent.IntfGetComponent: TComponent;
begin
  Result := Self;
end;

function TComponent.IsImplementorOf(const I: IInterface): Boolean;
var
  ICR: IInterfaceComponentReference;
begin
  Result := (I <> nil) and Supports(I, IInterfaceComponentReference, ICR)
    and (ICR.GetComponent = Self);
end;

{ TComponent.ReferenceInterface
  Establishes (opInsert) or removes (opRemove) internal links that
  notify us when the component that implements the given interface is
  destroyed.  The function result indicates whether the function was able
  to establish/remove a notification link or not.  A result of False
  doesn't necessarily indicate an error, but it does mean that the
  interface's implementor does not participate in the interfaced component
  reference model.  This could mean that the given interface employs true
  reference counting, independent of component lifetimes.  That doesn't
  affect the use of interface properties at runtime, but non-component
  interfaces cannot be stored by the property streaming system.

  When implementing components with interface-type properties, implement
  setter methods for the interface-type properties like this:

  procedure TMyComponent.SetMyIntfProp(const Value: IMyInterface);
  begin
    ReferenceInterface(FIntfField, opRemove);
    FIntfField := Value;
    ReferenceInterface(FIntfField, opInsert);
  end;

  Also override Notification to do the following for each interface property
  in your component:

  procedure TMyComponent.Notification(AComponent: TComponent; Operation: TOperation);
  begin
    inherited;
    if Assigned(MyIntfProp) and AComponent.IsImplementorOf(MyIntfProp) then
      MyIntfProp := nil;
    ... repeat for other interface properties ...
  end;

  Note that the Notification code assigns nil to the *property*, not to the
  private field, so that the property setter will call
  ReferenceInterface(FIntfField, opRemove to undo any links established by
  a previous opInsert operation.  All assignments to the interface property
  *must* be made through the property setter.

  TComponent.ReferenceInterface hides the details of how links are
  established between the implementor and the holder of an interface.
  The implementation details may change in the future.  Code that relies
  on those implementation details (instead of using ReferenceInterface)
  will not be supported.  In particular, avoid the temptation to use
  IInterfaceComponentReference in your own code, as this interface may
  not be available in the future.
}

function TComponent.ReferenceInterface(const I: IInterface; Operation: TOperation): Boolean;
var
  ICR: IInterfaceComponentReference;
begin
  Result := (I <> nil) and Supports(I, IInterfaceComponentReference, ICR);
  if Result then
    if Operation = opInsert then
      ICR.GetComponent.FreeNotification(Self)
    else
      ICR.GetComponent.RemoveFreeNotification(Self);
end;

{ TBasicActionLink }

constructor TBasicActionLink.Create(AClient: TObject);
begin
  inherited Create;
  AssignClient(AClient);
end;

procedure TBasicActionLink.AssignClient(AClient: TObject);
begin
end;

destructor TBasicActionLink.Destroy;
begin
  if FAction <> nil then FAction.UnRegisterChanges(Self);
  inherited Destroy;
end;

procedure TBasicActionLink.Change;
begin
  if Assigned(OnChange) then OnChange(FAction);
end;

function TBasicActionLink.Execute(AComponent: TComponent): Boolean;
begin
  FAction.ActionComponent := AComponent;
  Result := FAction.Execute;
end;

procedure TBasicActionLink.SetAction(Value: TBasicAction);
begin
  if Value <> FAction then
  begin
    if FAction <> nil then FAction.UnRegisterChanges(Self);
    FAction := Value;
    if Value <> nil then Value.RegisterChanges(Self);
  end;
end;

function TBasicActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := True;
end;

procedure TBasicActionLink.SetOnExecute(Value: TNotifyEvent);
begin
end;

function TBasicActionLink.Update: Boolean;
begin
  Result := not FAction.Suspended and FAction.Update;
end;

{ TBasicAction }

constructor TBasicAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList<TBasicActionLink>.Create;
end;

destructor TBasicAction.Destroy;
begin
  inherited Destroy;
  if Assigned(ActionComponent) then
    ActionComponent.RemoveFreeNotification(Self);
  if Assigned(FClients) then
    while FClients.Count > 0 do
      UnRegisterChanges(TBasicActionLink(FClients.Last));
  FClients.Free;
end;

function TBasicAction.Suspended: Boolean;
begin
  Result := False;
end;

function TBasicAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := False;
end;

procedure TBasicAction.ExecuteTarget(Target: TObject);
begin
end;

function TBasicAction.GetClient(Index: Integer): TBasicActionLink;
begin
  Result := FClients[Index];
end;

function TBasicAction.GetClientCount: Integer;
begin
  if FClients <> nil then
    Result := FClients.Count
  else
    Result := 0;
end;

procedure TBasicAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = ActionComponent) then
    FActionComponent := nil;
end;

procedure TBasicAction.UpdateTarget(Target: TObject);
begin
end;

function TBasicAction.Execute: Boolean;
begin
  if Assigned(FOnExecute) then
  begin
    FOnExecute(Self);
    Result := True;
  end
  else Result := False;
end;

function TBasicAction.Update: Boolean;
begin
  if Assigned(FOnUpdate) then
  begin
    FOnUpdate(Self);
    Result := True;
  end
  else Result := False;
end;

procedure TBasicAction.SetOnExecute(Value: TNotifyEvent);
var
  I: Integer;
begin
  if (TMethod(Value).Code <> TMethod(OnExecute).Code) or
     (TMethod(Value).Data <> TMethod(OnExecute).Data) then
  begin
    for I := 0 to FClients.Count - 1 do
      TBasicActionLink(FClients[I]).SetOnExecute(Value);
    FOnExecute := Value;
    Change;
  end;
end;

procedure TBasicAction.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBasicAction.RegisterChanges(const Value: TBasicActionLink);
begin
  Value.FAction := Self;
  FClients.Add(Value);
end;

procedure TBasicAction.UnRegisterChanges(const Value: TBasicActionLink);
var
  I: Integer;
begin
  for I := 0 to FClients.Count - 1 do
    if FClients[I] = Value then
    begin
      Value.FAction := nil;
      FClients.Delete(I);
      Break;
    end;
end;

procedure TBasicAction.SetActionComponent(const Value: TComponent);
begin
  if FActionComponent <> Value then
  begin
    if Assigned(FActionComponent) then
      FActionComponent.RemoveFreeNotification(Self);
    FActionComponent := Value;
    if Assigned(FActionComponent) then
      FActionComponent.FreeNotification(Self);
  end;
end;

{ TStreamAdapter }

constructor TStreamAdapter.Create(Stream: TStream;
  Ownership: TStreamOwnership);
begin
  inherited Create;
  FStream := Stream;
  FOwnership := Ownership;
end;

destructor TStreamAdapter.Destroy;
begin
  if FOwnership = soOwned then
  begin
    FStream.Free;
    FStream := nil;
  end;
  inherited Destroy;
end;

function TStreamAdapter.Read(pv: Pointer; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
var
  NumRead: LongInt;
begin
  try
    if pv = nil then
    begin
      Result := STG_E_INVALIDPOINTER;
      Exit;
    end;
    NumRead := FStream.Read(pv^, cb);
    if pcbRead <> nil then pcbRead^ := NumRead;
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

function TStreamAdapter.Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
var
  NumWritten: LongInt;
begin
  try
    if pv = nil then
    begin
      Result := STG_E_INVALIDPOINTER;
      Exit;
    end;
    NumWritten := FStream.Write(pv^, cb);
    if pcbWritten <> nil then pcbWritten^ := NumWritten;
    Result := S_OK;
  except
    Result := STG_E_CANTSAVE;
  end;
end;

function TStreamAdapter.Seek(dlibMove: Largeint; dwOrigin: DWORD; out libNewPosition: LargeUInt): HResult;
var
  NewPos: LargeUInt;
begin
  try
    if (Integer(dwOrigin) < STREAM_SEEK_SET) or (dwOrigin > STREAM_SEEK_END) then
    begin
      Result := STG_E_INVALIDFUNCTION;
      Exit;
    end;
    NewPos := FStream.Seek(dlibMove, dwOrigin);
    if @libNewPosition <> nil then libNewPosition := NewPos;
    Result := S_OK;
  except
    Result := STG_E_INVALIDPOINTER;
  end;
end;

function TStreamAdapter.SetSize(libNewSize: LargeUInt): HResult;
var
  LPosition: Int64;
begin
  try
    LPosition := FStream.Position;
    FStream.Size := libNewSize;
    if FStream.Size < LPosition then
      LPosition := FStream.Size;
    FStream.Position := LPosition;
    if libNewSize <> FStream.Size then
      Result := E_FAIL
    else
      Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TStreamAdapter.CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt;
  out cbWritten: LargeUInt): HResult;
const
  MaxBufSize = 1024 * 1024;  // 1mb
var
  Buffer: Pointer;
  BufSize, N, I, R: Integer;
  BytesRead, BytesWritten, W: LargeUInt;
begin
  Result := S_OK;
  BytesRead := 0;
  BytesWritten := 0;
  try
    if cb > MaxBufSize then
      BufSize := MaxBufSize
    else
      BufSize := Integer(cb);
    GetMem(Buffer, BufSize);
    try
      while cb > 0 do
      begin
        if cb > MaxInt then
          I := MaxInt
        else
          I := cb;
        while I > 0 do
        begin
          if I > BufSize then N := BufSize else N := I;
          R := FStream.Read(Buffer^, N);
          if R = 0 then Exit; // The end of the stream was hit.
          Inc(BytesRead, R);
          W := 0;
          Result := stm.Write(Buffer, R, @W);
          Inc(BytesWritten, W);
          if (Result = S_OK) and (Integer(W) <> R) then Result := E_FAIL;
          if Result <> S_OK then Exit;
          Dec(I, R);
          Dec(cb, R);
        end;
      end;
    finally
      FreeMem(Buffer);
      if (@cbWritten <> nil) then cbWritten := BytesWritten;
      if (@cbRead <> nil) then cbRead := BytesRead;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TStreamAdapter.Commit(grfCommitFlags: DWORD): HResult;
begin
  Result := S_OK;
end;

function TStreamAdapter.Revert: HResult;
begin
  Result := STG_E_REVERTED;
end;

function TStreamAdapter.LockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: DWORD): HResult;
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TStreamAdapter.UnlockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: DWORD): HResult;
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult;
begin
  Result := S_OK;
  try
    if (@statstg <> nil) then
      begin
        FillChar(statstg, SizeOf(statstg), 0);
        statstg.dwType := STGTY_STREAM;
        statstg.cbSize := FStream.Size;
        statstg.grfLocksSupported := LOCK_WRITE;
      end;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TStreamAdapter.Clone(out stm: IStream): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure FreeIntConstList;
var
  I: Integer;
  aList: TList<TIntConst>;
begin
  aList := IntConstList.LockList;
  try
    for I := 0 to aList.Count - 1 do
      TIntConst(aList.Items[I]).DisposeOf;
  finally
    IntConstList.UnlockList;
  end;
  IntConstList.Free;
end;

procedure ModuleUnload(Instance: IntPtr);
begin
  UnregisterModuleClasses(HMODULE(Instance));
end;

{ TDataModule }

constructor TDataModule.Create(AOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    if (ClassType <> TDataModule) and not (csDesigning in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TDataModule) then
        raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
      if OldCreateOrder then DoCreate;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

procedure TDataModule.AfterConstruction;
begin
  if not OldCreateOrder then DoCreate;
end;

constructor TDataModule.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited Create(AOwner);

  if Assigned(AddDataModule) and (Dummy >= 0) then
    AddDataModule(Self);
end;

procedure TDataModule.BeforeDestruction;
begin
  GlobalNameSpace.BeginWrite;
  Destroying;
  RemoveFixupReferences(Self, '');
  if not OldCreateOrder then DoDestroy;
end;

destructor TDataModule.Destroy;
begin
  if not (csDestroying in ComponentState) then GlobalNameSpace.BeginWrite;
  try
    if OldCreateOrder then DoDestroy;
    if Assigned(RemoveDataModule) then
      RemoveDataModule(Self);
    inherited Destroy;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

procedure TDataModule.DoCreate;
begin
  if Assigned(FOnCreate) then
  try
    FOnCreate(Self);
  except
    if not HandleCreateException then
      raise;
  end;
end;

procedure TDataModule.DoDestroy;
begin
  if Assigned(FOnDestroy) then
  try
    FOnDestroy(Self);
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

procedure TDataModule.DefineProperties(Filer: TFiler);
var
  Ancestor: TDataModule;

  function DoWriteWidth: Boolean;
  begin
    Result := True;
    if Ancestor <> nil then Result := FDesignSize.X <> Ancestor.FDesignSize.X;
  end;

  function DoWriteHorizontalOffset: Boolean;
  begin
    if Ancestor <> nil then
      Result := FDesignOffset.X <> Ancestor.FDesignOffset.X else
      Result := FDesignOffset.X <> 0;
  end;

  function DoWriteVerticalOffset: Boolean;
  begin
    if Ancestor <> nil then
      Result := FDesignOffset.Y <> Ancestor.FDesignOffset.Y else
      Result := FDesignOffset.Y <> 0;
  end;

  function DoWriteHeight: Boolean;
  begin
    Result := True;
    if Ancestor <> nil then Result := FDesignSize.Y <> Ancestor.FDesignSize.Y;
  end;

begin
  inherited DefineProperties(Filer);
  Ancestor := TDataModule(Filer.Ancestor);
  Filer.DefineProperty('Height', ReadHeight, WriteHeight, DoWriteHeight);
  Filer.DefineProperty('HorizontalOffset', ReadHorizontalOffset,
    WriteHorizontalOffset, DoWriteHorizontalOffset);
  Filer.DefineProperty('VerticalOffset', ReadVerticalOffset,
    WriteVerticalOffset, DoWriteVerticalOffset);
  Filer.DefineProperty('Width', ReadWidth, WriteWidth, DoWriteWidth);
end;

procedure TDataModule.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
end;

function TDataModule.HandleCreateException: Boolean;
begin
  if Assigned(ApplicationHandleException) then
  begin
    ApplicationHandleException(Self);
    Result := True;
  end
  else
    Result := False;
end;

procedure TDataModule.ReadState(Reader: TReader);
begin
  FOldCreateOrder := not ModuleIsCPP;
  inherited ReadState(Reader);
end;

procedure TDataModule.ReadWidth(Reader: TReader);
begin
  FDesignSize.X := Reader.ReadInteger;
end;

procedure TDataModule.ReadHorizontalOffset(Reader: TReader);
begin
  FDesignOffset.X := Reader.ReadInteger;
end;

procedure TDataModule.ReadVerticalOffset(Reader: TReader);
begin
  FDesignOffset.Y := Reader.ReadInteger;
end;

procedure TDataModule.ReadHeight(Reader: TReader);
begin
  FDesignSize.Y := Reader.ReadInteger;
end;

procedure TDataModule.WriteWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FDesignSize.X);
end;

procedure TDataModule.WriteHorizontalOffset(Writer: TWriter);
begin
  Writer.WriteInteger(FDesignOffset.X);
end;

procedure TDataModule.WriteVerticalOffset(Writer: TWriter);
begin
  Writer.WriteInteger(FDesignOffset.Y);
end;

procedure TDataModule.WriteHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FDesignSize.Y);
end;

{$IFDEF MSWINDOWS}

{ Object instance management }

type
  PObjectInstance = ^TObjectInstance;
  TObjectInstance = packed record
    Code: Byte;
    Offset: Integer;
    case Integer of
      0: (Next: PObjectInstance);
      1: (FMethod: TMethod);
  end;

const
{$IF Defined(CPUX86)}
  CodeBytes = 2;
{$ELSEIF Defined(CPUX64)}
  CodeBytes = 8;
{$ENDIF CPU}
  InstanceCount = (4096 - SizeOf(Pointer) * 2 - CodeBytes) div SizeOf(TObjectInstance) - 1;

type
  PInstanceBlock = ^TInstanceBlock;
  TInstanceBlock = packed record
    Next: PInstanceBlock;
    Code: array[1..CodeBytes] of Byte;
    WndProcPtr: Pointer;
    Instances: array[0..InstanceCount] of TObjectInstance;
  end;

var
  InstBlockList: PInstanceBlock;
  InstFreeList: PObjectInstance;

{ Standard window procedure }
function StdWndProc(Window: HWND; Message: UINT; WParam: WPARAM; LParam: WPARAM): LRESULT; stdcall;
{$IF Defined(CPUX86)}
{ In    ECX = Address of method pointer }
{ Out   EAX = Result }
asm
        XOR     EAX,EAX
        PUSH    EAX
        PUSH    LParam
        PUSH    WParam
        PUSH    Message
        MOV     EDX,ESP
        MOV     EAX,[ECX].Longint[4]
        CALL    [ECX].Pointer
        ADD     ESP,12
        POP     EAX
end;
{$ELSEIF Defined(CPUX64)}
{ In    R11 = Address of method pointer }
{ Out   RAX = Result }
var
  Msg: TMessage;
asm
        .PARAMS 2
        MOV     Msg.Msg,Message
        MOV     Msg.WParam,WParam
        MOV     Msg.LParam,LParam
        MOV     Msg.Result,0
        LEA     RDX,Msg
        MOV     RCX,[R11].TMethod.Data
        CALL    [R11].TMethod.Code
        MOV     RAX,Msg.Result
end;
{$ENDIF CPUX64}

{ Allocate an object instance }

function CalcJmpOffset(Src, Dest: Pointer): Longint;
begin
  Result := IntPtr(Dest) - (IntPtr(Src) + 5);
end;

function MakeObjectInstance(const AMethod: TWndMethod): Pointer;
const
  BlockCode: array[1..CodeBytes] of Byte = (
{$IF Defined(CPUX86)}
    $59,                       { POP ECX }
    $E9);                      { JMP StdWndProc }
{$ELSEIF Defined(CPUX64)}
    $41,$5b,                   { POP R11 }
    $FF,$25,$00,$00,$00,$00);  { JMP [RIP+0] }
{$ENDIF}
  PageSize = 4096;
var
  Block: PInstanceBlock;
  Instance: PObjectInstance;
begin
  if InstFreeList = nil then
  begin
    Block := VirtualAlloc(nil, PageSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    Block^.Next := InstBlockList;
    Move(BlockCode, Block^.Code, SizeOf(BlockCode));
{$IF Defined(CPUX86)}
    Block^.WndProcPtr := Pointer(CalcJmpOffset(@Block^.Code[2], @StdWndProc));
{$ELSEIF Defined(CPUX64)}
    Block^.WndProcPtr := @StdWndProc;
{$ENDIF}
    Instance := @Block^.Instances;
    repeat
      Instance^.Code := $E8;  { CALL NEAR PTR Offset }
      Instance^.Offset := CalcJmpOffset(Instance, @Block^.Code);
      Instance^.Next := InstFreeList;
      InstFreeList := Instance;
      Inc(PByte(Instance), SizeOf(TObjectInstance));
    until IntPtr(Instance) - IntPtr(Block) >= SizeOf(TInstanceBlock);
    InstBlockList := Block;
  end;
  Result := InstFreeList;
  Instance := InstFreeList;
  InstFreeList := Instance^.Next;
  Instance^.FMethod := TMethod(AMethod);
end;

{ Free an object instance }

procedure FreeObjectInstance(ObjectInstance: Pointer);
begin
  if ObjectInstance <> nil then
  begin
    PObjectInstance(ObjectInstance)^.Next := InstFreeList;
    InstFreeList := ObjectInstance;
  end;
end;

procedure CleanupInstFreeList(BlockStart, BlockEnd: PByte);
var
  Prev, Next, Item: PObjectInstance;
begin
  Prev := nil;
  Item := InstFreeList;
  while Item <> nil do
  begin
    Next := Item.Next;
    if (PByte(Item) >= BlockStart) and (PByte(Item) <= BlockEnd) then
    begin
      Item := Prev;
      if Prev = nil then
        InstFreeList := Next
      else
        Prev.Next := Next;
    end;
    Prev := Item;
    Item := Next;
  end;
end;

function GetFreeInstBlockItemCount(Item: PObjectInstance; Block: PInstanceBlock): Integer;
var
  I: Integer;
begin
  Result := 0;
  while Item <> nil do
  begin
    for I := High(Block.Instances) downto 0 do
    begin
      if @Block.Instances[I] = Item then
      begin
        Inc(Result);
        Break;
      end;
    end;
    Item := Item.Next;
  end;
end;

procedure ReleaseObjectInstanceBlocks;
var
  NextBlock, Block, PrevBlock: PInstanceBlock;
  UnusedCount: Integer;
begin
  Block := InstBlockList;
  PrevBlock := nil;
  while Block <> nil do
  begin
    NextBlock := Block.Next;

    { Obtain the number of free items in the InstanceBlock }
    UnusedCount := GetFreeInstBlockItemCount(InstFreeList, Block);

    { Release memory if the InstanceBlock contains only "free" items }
    if UnusedCount = Length(Block.Instances) then
    begin
      { Remove all InstFreeList items that refer to the InstanceBlock }
      CleanupInstFreeList(PByte(Block), PByte(Block) + SizeOf(TInstanceBlock) - 1);

      VirtualFree(Block, 0, MEM_RELEASE);

      Block := PrevBlock;
      if PrevBlock = nil then
        InstBlockList := NextBlock
      else
        PrevBlock.Next := NextBlock;
    end;

    { Next InstanceBlock }
    PrevBlock := Block;
    Block := NextBlock;
  end;
end;


var
  UtilWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindow');

function AllocateHWnd(const AMethod: TWndMethod): HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  UtilWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, UtilWindowClass.lpszClassName,
    TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Winapi.Windows.UnregisterClass(UtilWindowClass.lpszClassName, HInstance);
    Winapi.Windows.RegisterClass(UtilWindowClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowClass.lpszClassName,
    '', WS_POPUP {+ 0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if Assigned(AMethod) then
    SetWindowLongPtr(Result, GWL_WNDPROC, IntPtr(MakeObjectInstance(AMethod)));
end;

procedure DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLongPtr(Wnd, GWL_WNDPROC));
  DestroyWindow(Wnd);
  if Instance <> @DefWindowProc then FreeObjectInstance(Instance);
end;

{$ENDIF MSWINDOWS}

{ TComponent.TComponentAsyncResult }

constructor TComponent.TComponentAsyncResult.Create(const AContext: TObject; const AComponent: TComponent);
begin
  inherited Create(AContext);
  FComponent := AComponent;
end;

procedure TComponent.TComponentAsyncResult.Schedule;
begin
  FComponent.AsyncSchedule(Self);
  FComponent := nil;
end;

{ TComponent.TAsyncConstArrayResult }

constructor TComponent.TAsyncConstArrayResult.Create(const AContext: TObject; const AComponent: TComponent; const Params: array of const);
begin
  inherited Create(AContext, AComponent);
  FParams := System.Rtti.ArrayOfConstToTValueArray(Params);
end;

{ TComponent.TAsyncConstArrayProcedureResult }

procedure TComponent.TAsyncConstArrayProcResult.AsyncDispatch;
begin
  FAsyncProcedure(TValueArrayToArrayOfConst(FParams));
end;

constructor TComponent.TAsyncConstArrayProcResult.Create(const AAsyncProcedure: TAsyncConstArrayProc;
  const AContext: TObject; const AComponent: TComponent; const Params: array of const);
begin
  inherited Create(AContext, AComponent, Params);
  FAsyncProcedure := AAsyncProcedure;
end;

{ TComponent.TAsyncConstArrayFunctionResult<TResult> }

procedure TComponent.TAsyncConstArrayFuncResult<TResult>.AsyncDispatch;
begin
  FRetVal := FAsyncFunction(TValueArrayToArrayOfConst(FParams));
end;

constructor TComponent.TAsyncConstArrayFuncResult<TResult>.Create(const AAsyncFunction: TAsyncConstArrayFunc<TResult>;
  const AContext: TObject; const AComponent: TComponent; const Params: array of const);
begin
  inherited Create(AContext, AComponent, Params);
  FAsyncFunction := AAsyncFunction;
end;

function TComponent.TAsyncConstArrayFuncResult<TResult>.GetRetVal: TResult;
begin
  WaitForCompletion;
  Result := FRetVal;
end;

{ TComponent.TAsyncConstArrayProcedureResult }

procedure TComponent.TAsyncConstArrayProcedureResult.AsyncDispatch;
begin
  FAsyncProcedure(Self, TValueArrayToArrayOfConst(FParams));
end;

constructor TComponent.TAsyncConstArrayProcedureResult.Create(const AAsyncProcedure: TAsyncConstArrayProcedureEvent;
  const AContext: TObject; const AComponent: TComponent; const Params: array of const);
begin
  inherited Create(AContext, AComponent, Params);
  FAsyncProcedure := AAsyncProcedure;
end;

{ TComponent.TAsyncConstArrayFunctionResult }

procedure TComponent.TAsyncConstArrayFunctionResult.AsyncDispatch;
begin
  FAsyncFunction(Self, FRetVal, TValueArrayToArrayOfConst(FParams));
end;

constructor TComponent.TAsyncConstArrayFunctionResult.Create(const AAsyncFunction: TAsyncConstArrayFunctionEvent;
  const AContext: TObject; const AComponent: TComponent; const Params: array of const);
begin
  inherited Create(AContext, AComponent, Params);
  FAsyncFunction := AAsyncFunction;
end;

function TComponent.TAsyncConstArrayFunctionResult.GetRetVal: TObject;
begin
  WaitForCompletion;
  Result := FRetVal;
end;

{ TComponent.TAsyncProcedureResult }

procedure TComponent.TAsyncProcedureResult.AsyncDispatch;
begin
  FAsyncProcedure();
end;

constructor TComponent.TAsyncProcedureResult.Create(const AAsyncProcedure: TProc; const AContext: TObject;
  const AComponent: TComponent);
begin
  inherited Create(AContext, AComponent);
  if not Assigned(AAsyncProcedure) then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  FAsyncProcedure := AAsyncProcedure;
end;

{ TBaseAsyncFunctionResult<TResult> }

procedure TComponent.TAsyncFunctionResult<TResult>.AsyncDispatch;
begin
  FRetVal := FAsyncFunction();
end;

constructor TComponent.TAsyncFunctionResult<TResult>.Create(const AAsyncFunction: TFunc<TResult>; const AContext: TObject;
  const AComponent: TComponent);
begin
  inherited Create(AContext, AComponent);
  if not Assigned(AAsyncFunction) then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  FASyncFunction := AAsyncFunction;
end;

function TComponent.TAsyncFunctionResult<TResult>.GetRetVal: TResult;
begin
  WaitForCompletion;
  Result := FRetVal;
end;

{ TComponent.TAsyncProcedureResultEvent }

procedure TComponent.TAsyncProcedureResultEvent.AsyncDispatch;
begin
  FAsyncProcedure(Self);
end;

constructor TComponent.TAsyncProcedureResultEvent.Create(const AAsyncProcedure: TAsyncProcedureEvent; const AContext: TObject;
  const AComponent: TComponent);
begin
  inherited Create(AContext, AComponent);
  if not Assigned(AAsyncProcedure) then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  FAsyncProcedure := AAsyncProcedure;
end;

{ TComponent.TAsyncFunctionResultEvent }

procedure TComponent.TAsyncFunctionResultEvent.AsyncDispatch;
begin
  FAsyncFunction(Self, FRetVal);
end;

constructor TComponent.TAsyncFunctionResultEvent.Create(const AAsyncFunction: TAsyncFunctionEvent; const AContext: TObject;
  const AComponent: TComponent);
begin
  inherited Create(AContext, AComponent);
  if not Assigned(AAsyncFunction) then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  FAsyncFunction := AAsyncFunction;
end;

function TComponent.TAsyncFunctionResultEvent.GetRetVal: TObject;
begin
  WaitForCompletion;
  Result := FRetVal;
end;

{ EFileStreamError }

constructor EFileStreamError.Create(ResStringRec: PResStringRec;
  const FileName: string);
begin
  inherited CreateResFmt(ResStringRec, [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
end;

{ TStringReader }

procedure TStringReader.Close;
begin
  FData := '';
  FIndex := -1;
end;

constructor TStringReader.Create(S: string);
begin
  inherited Create;

  FIndex := Low(S);
  FData := S;
end;

function TStringReader.Peek: Integer;
begin
  Result := -1;
  if (FIndex >= Low(FData)) and (FIndex <= High(FData)) then
    Result := Integer(FData[FIndex]);
end;

function TStringReader.Read: Integer;
begin
  Result := -1;
  if (FIndex >= Low(FData)) and (FIndex <= High(FData)) then
  begin
    Result := Integer(FData[FIndex]);
    Inc(FIndex);
    if FIndex > High(FData) then
      FIndex := -1;
  end;
end;

function TStringReader.Read(var Buffer: TCharArray; Index,
  Count: Integer): Integer;
begin
  Result := -1;

  if FIndex = -1 then
    Exit;

  if Length(Buffer) < Index + Count then
    raise EArgumentOutOfRangeException.Create(SInsufficientReadBuffer);

  if Count > FData.Length - FIndex + Low(FData) then
    Count := FData.Length - FIndex + Low(FData);
  Result := Count;

  FData.CopyTo(FIndex-Low(FData), Buffer, Index, Count);

  Inc(FIndex, Count);
  if FIndex > High(FData) then
    FIndex := -1;
end;

function TStringReader.ReadBlock(var Buffer: TCharArray; Index,
  Count: Integer): Integer;
begin
  Result := Read(Buffer, Index, Count);
end;

function TStringReader.ReadLine: string;
var
  StartIndex: Integer;
  EndIndex: Integer;
begin
  Result := '';
  if FIndex = -1 then
    Exit;

  StartIndex := FIndex;
  EndIndex := FIndex;

  while True do
  begin
    if EndIndex > High(FData) then
    begin
      FIndex := EndIndex;
      Break;
    end;
    if FData[EndIndex] = #10 then
    begin
      FIndex := EndIndex + 1;
      Break;
    end
    else
    if (FData[EndIndex] = #13) and (EndIndex + 1 <= High(FData)) and (FData[EndIndex + 1] = #10) then
    begin
      FIndex := EndIndex + 2;
      Break;
    end
    else
    if FData[EndIndex] = #13 then
    begin
      FIndex := EndIndex + 1;
      Break;
    end;
    Inc(EndIndex);
  end;

  Result := FData.SubString(StartIndex-Low(FData), (EndIndex - StartIndex));

  if FIndex > High(FData) then
    FIndex := -1;
end;

function TStringReader.ReadToEnd: string;
begin
  Result := '';
  if FIndex = -1 then
    Exit;
  Result := FData.SubString(FIndex-Low(FData));
  FIndex := -1;
end;

{ TStringWriter }

procedure TStringWriter.Close;
begin
end;

constructor TStringWriter.Create;
begin
  inherited Create;

  FOwnsBuilder := True;
  FBuilder := TStringBuilder.Create;
end;

constructor TStringWriter.Create(Builder: TStringBuilder);
begin
  inherited Create;

  if not Assigned(Builder) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Builder']); // DO NOT LOCALIZE

  FOwnsBuilder := False;
  FBuilder := Builder;
end;

destructor TStringWriter.Destroy;
begin
  if FOwnsBuilder then
  begin
    FBuilder.Free;
    FBuilder := nil;
  end;
  inherited;
end;

procedure TStringWriter.Flush;
begin

end;

function TStringWriter.ToString: string;
begin
  Result := FBuilder.ToString;
end;

procedure TStringWriter.Write(Value: Cardinal);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(Value: Boolean);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(Value: Char);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(const Value: TCharArray; Index, Count: Integer);
begin
  FBuilder.Append(Value, Index, Count);
end;

procedure TStringWriter.Write(const Format: string; Args: array of const);
begin
  FBuilder.AppendFormat(Format, Args);
end;

procedure TStringWriter.Write(Value: UInt64);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(Value: TObject);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(Value: Single);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(const Value: string);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(Value: Int64);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(const Value: TCharArray);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(Value: Double);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.Write(Value: Integer);
begin
  FBuilder.Append(Value);
end;

procedure TStringWriter.WriteLine(const Value: TCharArray);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: Double);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: Integer);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine;
begin
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: Boolean);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: Char);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: Int64);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: UInt64);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(const Format: string; Args: array of const);
begin
  FBuilder.AppendFormat(Format, Args);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(const Value: TCharArray; Index, Count: Integer);
begin
  FBuilder.Append(Value, Index, Count);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: Cardinal);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: TObject);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(Value: Single);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(const Value: string);
begin
  FBuilder.Append(Value);
  FBuilder.AppendLine;
end;

{ TStreamWriter }

procedure TStreamWriter.Close;
begin
  Flush;
  if FOwnsStream  then
    FreeAndNil(FStream);
end;

constructor TStreamWriter.Create(Stream: TStream);
begin
  inherited Create;
  FOwnsStream := False;
  FStream := Stream;
  FEncoding := TEncoding.UTF8;
  SetLength(FBuffer, 1024);
  FBufferIndex := 0;
  FNewLine := sLineBreak;
  FAutoFlush := True;
end;

constructor TStreamWriter.Create(Stream: TStream; Encoding: TEncoding; BufferSize: Integer);
begin
  inherited Create;
  FOwnsStream := False;
  FStream := Stream;
  FEncoding := Encoding;
  if BufferSize >= 128 then
    SetLength(FBuffer, BufferSize)
  else
    SetLength(FBuffer, 128);
  FBufferIndex := 0;
  FNewLine := sLineBreak;
  FAutoFlush := True;
  if Stream.Position = 0 then
    WriteBytes(FEncoding.GetPreamble);
end;

constructor TStreamWriter.Create(const Filename: string; Append: Boolean);
begin
  if (not FileExists(Filename)) or (not Append) then
    FStream := TFileStream.Create(Filename, fmCreate)
  else
  begin
    FStream := TFileStream.Create(Filename, fmOpenWrite);
    FStream.Seek(0, soEnd);
  end;
  Create(FStream);
  FOwnsStream := True;
end;

constructor TStreamWriter.Create(const Filename: string; Append: Boolean;
  Encoding: TEncoding; BufferSize: Integer);
begin
  if (not FileExists(Filename)) or (not Append) then
    FStream := TFileStream.Create(Filename, fmCreate)
  else
  begin
    FStream := TFileStream.Create(Filename, fmOpenWrite);
    FStream.Seek(0, soEnd);
  end;
  Create(FStream, Encoding, BufferSize);
  FOwnsStream := True;
end;

destructor TStreamWriter.Destroy;
begin
  Close;
  SetLength(FBuffer, 0);
  inherited;
end;

procedure TStreamWriter.Flush;
begin
  if FBufferIndex = 0 then
    Exit;
  if FStream = nil then
    Exit;

  FStream.Write(FBuffer[0], FBufferIndex);
  FBufferIndex := 0;
end;

procedure TStreamWriter.OwnStream;
begin
  FOwnsStream := True;
end;

procedure TStreamWriter.Write(Value: Cardinal);
begin
  WriteBytes(FEncoding.GetBytes(UIntToStr(Value)));
end;

procedure TStreamWriter.Write(const Value: string);
begin
  WriteBytes(FEncoding.GetBytes(Value));
end;

procedure TStreamWriter.Write(Value: UInt64);
begin
  WriteBytes(FEncoding.GetBytes(UIntToStr(Value)));
end;

procedure TStreamWriter.Write(const Value: TCharArray; Index, Count: Integer);
var
  Bytes: TBytes;
begin
  SetLength(Bytes, Count * 4);
  SetLength(Bytes, FEncoding.GetBytes(Value, Index, Count, Bytes, 0));
  WriteBytes(Bytes);
end;

procedure TStreamWriter.WriteBytes(Bytes: TBytes);
var
  ByteIndex: Integer;
  WriteLen: Integer;
begin
  ByteIndex := 0;

  while ByteIndex < Length(Bytes) do
  begin
    WriteLen := Length(Bytes) - ByteIndex;
    if WriteLen > Length(FBuffer) - FBufferIndex then
      WriteLen := Length(FBuffer) - FBufferIndex;

    Move(Bytes[ByteIndex], FBuffer[FBufferIndex], WriteLen);

    Inc(FBufferIndex, WriteLen);
    Inc(ByteIndex, WriteLen);

    if FBufferIndex >= Length(FBuffer) then
      Flush;
  end;

  if FAutoFlush then
    Flush;
end;

procedure TStreamWriter.Write(const Format: string; Args: array of const);
begin
  WriteBytes(FEncoding.GetBytes(System.SysUtils.Format(Format, Args)));
end;

procedure TStreamWriter.Write(Value: Single);
begin
  WriteBytes(FEncoding.GetBytes(FloatToStr(Value)));
end;

procedure TStreamWriter.Write(const Value: TCharArray);
begin
  WriteBytes(FEncoding.GetBytes(Value));
end;

procedure TStreamWriter.Write(Value: Double);
begin
  WriteBytes(FEncoding.GetBytes(FloatToStr(Value)));
end;

procedure TStreamWriter.Write(Value: Integer);
begin
  WriteBytes(FEncoding.GetBytes(IntToStr(Value)));
end;

procedure TStreamWriter.Write(Value: Char);
begin
  WriteBytes(FEncoding.GetBytes(Value));
end;

procedure TStreamWriter.Write(Value: TObject);
begin
  WriteBytes(FEncoding.GetBytes(Value.ToString));
end;

procedure TStreamWriter.Write(Value: Int64);
begin
  WriteBytes(FEncoding.GetBytes(IntToStr(Value)));
end;

procedure TStreamWriter.Write(Value: Boolean);
begin
  WriteBytes(FEncoding.GetBytes(BoolToStr(Value, True)));
end;

procedure TStreamWriter.WriteLine(const Value: TCharArray);
begin
  WriteBytes(FEncoding.GetBytes(Value));
  WriteBytes(FEncoding.GetBytes(FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Double);
begin
  WriteBytes(FEncoding.GetBytes(FloatToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Integer);
begin
  WriteBytes(FEncoding.GetBytes(IntToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine;
begin
  WriteBytes(FEncoding.GetBytes(FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Boolean);
begin
  WriteBytes(FEncoding.GetBytes(BoolToStr(Value, True) + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Char);
begin
  WriteBytes(FEncoding.GetBytes(Value));
  WriteBytes(FEncoding.GetBytes(FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Int64);
begin
  WriteBytes(FEncoding.GetBytes(IntToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: UInt64);
begin
  WriteBytes(FEncoding.GetBytes(UIntToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine(const Format: string; Args: array of const);
begin
  WriteBytes(FEncoding.GetBytes(System.SysUtils.Format(Format, Args) + FNewLine));
end;

procedure TStreamWriter.WriteLine(const Value: TCharArray; Index, Count: Integer);
var
  Bytes: TBytes;
begin
  SetLength(Bytes, Count * 4);
  SetLength(Bytes, FEncoding.GetBytes(Value, Index, Count, Bytes, 0));
  WriteBytes(Bytes);
  WriteBytes(FEncoding.GetBytes(FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Cardinal);
begin
  WriteBytes(FEncoding.GetBytes(UIntToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: TObject);
begin
  WriteBytes(FEncoding.GetBytes(Value.ToString + FNewLine));
end;

procedure TStreamWriter.WriteLine(Value: Single);
begin
  WriteBytes(FEncoding.GetBytes(FloatToStr(Value) + FNewLine));
end;

procedure TStreamWriter.WriteLine(const Value: string);
begin
  WriteBytes(FEncoding.GetBytes(Value + FNewLine));
end;

{ TStreamReader }

constructor TStreamReader.Create(Stream: TStream);
begin
  Create(Stream, TEncoding.UTF8, True);
end;

constructor TStreamReader.Create(Stream: TStream; DetectBOM: Boolean);
begin
  Create(Stream, TEncoding.UTF8, DetectBOM);
end;

constructor TStreamReader.Create(Stream: TStream; Encoding: TEncoding;
  DetectBOM: Boolean; BufferSize: Integer);
begin
  inherited Create;

  if not Assigned(Stream) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Stream']); // DO NOT LOCALIZE
  if not Assigned(Encoding) then
    raise EArgumentException.CreateResFmt(@SParamIsNil, ['Encoding']); // DO NOT LOCALIZE

  FBufferedData := TStringBuilder.Create;
  FEncoding := Encoding;
  FBufferSize := BufferSize;
  if FBufferSize < 128 then
    FBufferSize := 128;
  FNoDataInStream := False;
  FStream := Stream;
  FOwnsStream := False;
  FDetectBOM := DetectBOM;
  FSkipPreamble := not FDetectBOM;
end;

constructor TStreamReader.Create(const Filename: string; Encoding: TEncoding;
  DetectBOM: Boolean; BufferSize: Integer);
begin
  Create(TFileStream.Create(Filename, fmOpenRead), Encoding, DetectBOM, BufferSize);
  FOwnsStream := True;
end;

constructor TStreamReader.Create(const Filename: string; DetectBOM: Boolean);
begin
  Create(TFileStream.Create(Filename, fmOpenRead), DetectBOM);
  FOwnsStream := True;
end;

constructor TStreamReader.Create(const Filename: string);
begin
  Create(TFileStream.Create(Filename, fmOpenRead));
  FOwnsStream := True;
end;

destructor TStreamReader.Destroy;
begin
  Close;
  inherited;
end;

procedure TStreamReader.Close;
begin
  if (FStream <> nil) and FOwnsStream then
  begin
    FStream.Free;
    FStream := nil;
  end;

  if FBufferedData <> nil then
  begin
    FBufferedData.Free;
    FBufferedData := nil;
  end;
end;

procedure TStreamReader.DiscardBufferedData;
begin
  if FBufferedData <> nil then
  begin
    FBufferedData.Remove(0, FBufferedData.Length);
    FNoDataInStream := False;
  end;
end;

function TStreamReader.DetectBOM(var Encoding: TEncoding; Buffer: TBytes): Integer;
var
  LEncoding: TEncoding;
begin
  // try to automatically detect the buffer encoding
  LEncoding := nil;
  Result := TEncoding.GetBufferEncoding(Buffer, LEncoding);
  // detected encoding points to Default and param Encoding requests some other
  // type of Encoding; set the Encoding param to UTF8 (Default)
  if (LEncoding = TEncoding.Default) and (Encoding <> TEncoding.Default) then
    Encoding := TEncoding.UTF8
  else
    Encoding := LEncoding;

  FDetectBOM := False;
end;

procedure TStreamReader.FillBuffer(var Encoding: TEncoding);
const
  BufferPadding = 4;
var
  LString: string;
  LBuffer: TBytes;
  BytesRead: Integer;
  StartIndex: Integer;
  ByteCount: Integer;
  ByteBufLen: Integer;
  ExtraByteCount: Integer;

  procedure AdjustEndOfBuffer(const LBuffer: TBytes; Offset: Integer);
  var
    Pos, Size: Integer;
    Rewind: Integer;
  begin
    Dec(Offset);
    for Pos := Offset downto 0 do
    begin
      for Size := Offset - Pos + 1 downto 1 do
      begin
        if Encoding.GetCharCount(LBuffer, Pos, Size) > 0 then
        begin
          Rewind := Offset - (Pos + Size - 1);
          FStream.Position := FStream.Position - Rewind;
          BytesRead := BytesRead - Rewind;
          Exit;
        end;
      end;
    end;
  end;

begin
  SetLength(LBuffer, FBufferSize + BufferPadding);

  // Read data from stream
  BytesRead := FStream.Read(LBuffer[0], FBufferSize);
  FNoDataInStream := BytesRead < FBufferSize;

  // Adjust the end of the buffer to be sure we have a valid encoding
  if not FNoDataInStream then
    AdjustEndOfBuffer(LBuffer, BytesRead);

  // Check for byte order mark and calc start index for character data
  if FDetectBOM then
    StartIndex := DetectBOM(Encoding, LBuffer)
  else if FSkipPreamble then
    StartIndex := SkipPreamble(Encoding, LBuffer)
  else
    StartIndex := 0;

  // Convert to string and calc byte count for the string
  ByteBufLen := BytesRead - StartIndex;
  LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
  ByteCount := FEncoding.GetByteCount(LString);

  // If byte count <> number of bytes read from the stream
  // the buffer boundary is mid-character and additional bytes
  // need to be read from the stream to complete the character
  ExtraByteCount := 0;
  while (ByteCount <> ByteBufLen) and (ExtraByteCount < FEncoding.GetMaxByteCount(1)) do
  begin
    // Expand buffer if padding is used
    if (StartIndex + ByteBufLen) = Length(LBuffer) then
      SetLength(LBuffer, Length(LBuffer) + BufferPadding);

    // Read one more byte from the stream into the
    // buffer padding and convert to string again
    BytesRead := FStream.Read(LBuffer[StartIndex + ByteBufLen], 1);
    if BytesRead = 0 then
      // End of stream, append what's been read and discard remaining bytes
      Break;

    Inc(ExtraByteCount);

    Inc(ByteBufLen);
    LString := FEncoding.GetString(LBuffer, StartIndex, ByteBufLen);
    ByteCount := FEncoding.GetByteCount(LString);
  end;

  // Add string to character data buffer
  FBufferedData.Append(LString);
end;

function TStreamReader.GetEndOfStream: Boolean;
begin
  if not FNoDataInStream and (FBufferedData <> nil) and (FBufferedData.Length < 1) then
    FillBuffer(FEncoding);
  Result := FNoDataInStream and ((FBufferedData = nil) or (FBufferedData.Length = 0));
end;

procedure TStreamReader.OwnStream;
begin
  FOwnsStream := True;
end;

function TStreamReader.Peek: Integer;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    if FBufferedData.Length < 1 then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData.Chars[0]);
  end;
end;

function TStreamReader.Read(var Buffer: TCharArray; Index,
  Count: Integer): Integer;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    while (FBufferedData.Length < Count) and (not EndOfStream) and (not FNoDataInStream) do
      FillBuffer(FEncoding);

    if FBufferedData.Length > Count then
      Result := Count
    else
      Result := FBufferedData.Length;

    FBufferedData.CopyTo(0, Buffer, Index, Result);
    FBufferedData.Remove(0, Result);
  end;
end;

function TStreamReader.ReadBlock(var Buffer: TCharArray; Index,
  Count: Integer): Integer;
begin
  Result := Read(Buffer, Index, Count);
end;

function TStreamReader.Read: Integer;
begin
  Result := -1;
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    if FBufferedData.Length < 1 then
      FillBuffer(FEncoding);
    Result := Integer(FBufferedData.Chars[0]);
    FBufferedData.Remove(0, 1);
  end;
end;

function TStreamReader.ReadLine: string;
var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;
  LChar: Char;
begin
  Result := '';
  if FBufferedData = nil then
    Exit;
  NewLineIndex := 0;
  PostNewLineIndex := 0;

  while True do
  begin
    if (NewLineIndex + 2 > FBufferedData.Length) and (not FNoDataInStream) then
      FillBuffer(FEncoding);

    if NewLineIndex >= FBufferedData.Length then
    begin
      if FNoDataInStream then
      begin
        PostNewLineIndex := NewLineIndex;
        Break;
      end
      else
      begin
        FillBuffer(FEncoding);
        if FBufferedData.Length = 0 then
          Break;
      end;
    end;
    LChar := FBufferedData[NewLineIndex];
    if LChar = #10 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end
    else
    if (LChar = #13) and (NewLineIndex + 1 < FBufferedData.Length) and (FBufferedData[NewLineIndex + 1] = #10) then
    begin
      PostNewLineIndex := NewLineIndex + 2;
      Break;
    end
    else
    if LChar = #13 then
    begin
      PostNewLineIndex := NewLineIndex + 1;
      Break;
    end;

    Inc(NewLineIndex);
  end;

  Result := FBufferedData.ToString;
  SetLength(Result, NewLineIndex);
  FBufferedData.Remove(0, PostNewLineIndex);
end;

function TStreamReader.ReadToEnd: string;
begin
  Result := '';
  if (FBufferedData <> nil) and (not EndOfStream) then
  begin
    repeat
      FillBuffer(FEncoding);
    until FNoDataInStream;
    Result := FBufferedData.ToString;
    FBufferedData.Remove(0, FBufferedData.Length);
  end;
end;

function TStreamReader.SkipPreamble(Encoding: TEncoding; Buffer: TBytes): Integer;
var
  I: Integer;
  LPreamble: TBytes;
  BOMPresent: Boolean;
begin
  Result := 0;
  LPreamble := Encoding.GetPreamble;
  if (Length(LPreamble) > 0) then
  begin
    if Length(Buffer) >= Length(LPreamble) then
    begin
      BOMPresent := True;
      for I := 0 to Length(LPreamble) - 1 do
        if LPreamble[I] <> Buffer[I] then
        begin
          BOMPresent := False;
          Break;
        end;
      if BOMPresent then
        Result := Length(LPreamble);
    end;
  end;
  FSkipPreamble := False;
end;

{ TBinaryReader }

procedure TBinaryReader.Close;
begin
  if FOwnsStream then
    FreeAndNil(FStream);
end;

constructor TBinaryReader.Create(Stream: TStream; AEncoding: TEncoding; AOwnsStream: Boolean);
begin
  inherited Create;
  if Stream = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  FStream := Stream;
  if AEncoding = nil then
    FEncoding := TEncoding.UTF8
  else
    FEncoding := AEncoding;
  FOwnsStream := AOwnsStream;
  FTwoBytesPerChar := FEncoding is TUnicodeEncoding;
  FMaxCharsSize := FEncoding.GetMaxByteCount($80);
end;

constructor TBinaryReader.Create(const Filename: string; Encoding: TEncoding);
begin
  Create(TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite), Encoding, True);
end;

destructor TBinaryReader.Destroy;
begin
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

function TBinaryReader.GetBaseStream: TStream;
begin
  Result := FStream;
end;

function TBinaryReader.InternalReadChar: Integer;
var
  CharCount: Integer;
  ByteCount: Integer;
  Index: Integer;
  Position: Int64;
  CharByte: Byte;
begin
  if FCharBytes = nil then
    SetLength(FCharBytes, $80);
  if FOneChar = nil then
    SetLength(FOneChar, 1);
  Index := 0;
  Position := FStream.Position;
  try
    CharCount := 0;
    if FTwoBytesPerChar then
      ByteCount := 2
    else
      ByteCount := 1;
    while (CharCount = 0) and (Index < Length(FCharBytes)) do
    begin
      if FStream.Read(CharByte, SizeOf(CharByte)) = 0 then
        ByteCount := 0;
      FCharBytes[Index] := CharByte;
      Inc(Index);
      if ByteCount = 2 then
      begin
        if FStream.Read(CharByte, SizeOf(CharByte)) = 0 then
          ByteCount := 1;
        FCharBytes[Index] := CharByte;
        Inc(Index);
      end;
      if ByteCount = 0 then
        Exit(-1);
      CharCount := FEncoding.GetChars(FCharBytes, 0, Index, FOneChar, 0);
    end;
  except
    FStream.Position := Position;
    raise;
  end;
  if CharCount > 0 then
    Result := Integer(FOneChar[0])
  else
    Result := -1;
end;

function TBinaryReader.InternalReadChars(const Chars: TCharArray; Index, Count: Integer): Integer;
var
  BytesToRead, RemainingChars, CharCount: Integer;
begin
  if FCharBytes = nil then
    SetLength(FCharBytes, $80);
  RemainingChars := Count;
  while RemainingChars > 0 do
  begin
    BytesToRead := RemainingChars;
    if FTwoBytesPerChar then
      BytesToRead := BytesToRead shl 1;
    if BytesToRead > Length(FCharBytes) then
      BytesToRead := Length(FCharBytes);
    BytesToRead := FStream.Read(FCharBytes[0], BytesToRead);
    if BytesToRead = 0 then
      Break;
    CharCount := FEncoding.GetChars(FCharBytes, 0, BytesToRead, Chars, Index);
    Dec(RemainingChars, CharCount);
    Inc(Index, CharCount);
  end;
  Result := Count - RemainingChars;
end;

function TBinaryReader.PeekChar: Integer;
var
  Position: Int64;
begin
  Position := FStream.Position;
  try
    Result := InternalReadChar;
  finally
    FStream.Position := Position;
  end;
end;

function TBinaryReader.Read(var Buffer: TCharArray; Index, Count: Integer): Integer;
begin
  if Index < 0 then
    raise EArgumentOutOfRangeException.CreateResFmt(@sArgumentOutOfRange_NeedNonNegValue, ['Index']); // do not localize
  if Count < 0 then
    raise EArgumentOutOfRangeException.CreateResFmt(@sArgumentOutOfRange_NeedNonNegValue, ['Count']); // do not localize
  if Length(Buffer) - Index < Count  then
    raise EArgumentOutOfRangeException.CreateRes(@sArgumentOutOfRange_OffLenInvalid);
  Result := InternalReadChars(Buffer, Index, Count);
end;

function TBinaryReader.Read: Integer;
begin
  Result := InternalReadChar;
end;

function TBinaryReader.Read(const Buffer: TBytes; Index, Count: Integer): Integer;
begin
  if Index < 0 then
    raise EArgumentOutOfRangeException.CreateResFmt(@sArgumentOutOfRange_NeedNonNegValue, ['Index']); // do not localize
  if Count < 0 then
    raise EArgumentOutOfRangeException.CreateResFmt(@sArgumentOutOfRange_NeedNonNegValue, ['Count']); // do not localize
  if Length(Buffer) - Index < Count  then
    raise EArgumentOutOfRangeException.CreateRes(@sArgumentOutOfRange_OffLenInvalid);
  Result := FStream.Read(Buffer[Index], Count);
end;

function TBinaryReader.Read7BitEncodedInt: Integer;
var
  Shift: Integer;
  Value: Integer;
begin
  Shift := 0;
  Result := 0;
  repeat
    if Shift = 35 then
      raise EStreamError.CreateRes(@SInvalid7BitEncodedInteger);
    Value := ReadByte;
    Result := Result or ((Value and $7F) shl Shift);
    Inc(Shift, 7);
  until Value and $80 = 0;
end;

function TBinaryReader.ReadBoolean: Boolean;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadByte: Byte;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadBytes(Count: Integer): TBytes;
var
  BytesRead: Integer;
begin
  if Count < 0 then
    raise EArgumentOutOfRangeException.CreateResFmt(@sArgumentOutOfRange_NeedNonNegValue, ['Count']); // do not localize
  SetLength(Result, Count);
  BytesRead := FStream.Read(Result[0], Count);
  if BytesRead <> Count then
    SetLength(Result, BytesRead);
end;

function TBinaryReader.ReadCardinal: Cardinal;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadChar: Char;
var
  Value: Integer;
begin
  Value := Read;
  if Value = -1 then
    raise EStreamError.CreateRes(@SReadPastEndOfStream);
  Result := Char(Value);
end;

function TBinaryReader.ReadChars(Count: Integer): TCharArray;
var
  CharsRead: Integer;
begin
  if Count < 0 then
    raise EArgumentOutOfRangeException.CreateResFmt(@sArgumentOutOfRange_NeedNonNegValue, ['Count']); // do not localize
  SetLength(Result, Count);
  CharsRead := InternalReadChars(Result, 0, Count);
  if CharsRead <> Count then
    SetLength(Result, CharsRead);
end;

function TBinaryReader.ReadDouble: Double;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadInt64: Int64;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadInteger: Integer;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadInt32: Integer;
begin
  Result := ReadInteger;
end;

function TBinaryReader.ReadShortInt: ShortInt;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadSByte: ShortInt;
begin
  Result := ReadShortInt;
end;

function TBinaryReader.ReadSmallInt: SmallInt;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadInt16: SmallInt;
begin
  Result := ReadSmallInt;
end;

function TBinaryReader.ReadSingle: Single;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadString: string;
var
  Bytes: TBytes;
  ByteCount, BytesRead: Integer;
begin
  ByteCount := Read7BitEncodedInt;
  if ByteCount < 0 then
    raise EStreamError.CreateRes(@SInvalidStringLength);
  if ByteCount > 0 then
  begin
    SetLength(Bytes, ByteCount);
    BytesRead := FStream.Read(Bytes[0], ByteCount);
    if BytesRead <> ByteCount then
      raise EStreamError.CreateRes(@SReadPastEndOfStream);
    Result := FEncoding.GetString(Bytes);
  end else
    Result := '';
end;

function TBinaryReader.ReadUInt32: Cardinal;
begin
  Result := ReadCardinal;
end;

function TBinaryReader.ReadUInt64: UInt64;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadWord: Word;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryReader.ReadUInt16: Word;
begin
  Result := ReadWord;
end;

{ TNullStream }

type
  TNullStream = class(TStream)
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Read64(Buffer: TBytes; Offset, Count: Int64): Int64; override;
    function Write64(const Buffer: TBytes; Offset, Count: Int64): Int64; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

function TNullStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TNullStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TNullStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := 0;
end;

function TNullStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := 0;
end;

function TNullStream.Read64(Buffer: TBytes; Offset, Count: Int64): Int64;
begin
  Result := 0;
end;

function TNullStream.Write64(const Buffer: TBytes; Offset, Count: Int64): Int64;
begin
  Result := 0;
end;

function TNullStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := 0;
end;

function TNullStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := 0;
end;

{ TBinaryWriter }

constructor TBinaryWriter.Create(Stream: TStream; Encoding: TEncoding);
begin
  Create(Stream, Encoding, False);
end;

constructor TBinaryWriter.Create(Stream: TStream);
begin
  Create(Stream, nil, False);
end;

constructor TBinaryWriter.Create(Stream: TStream; Encoding: TEncoding; AOwnsStream: Boolean);
begin
  inherited Create;
  if Stream = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  FStream := Stream;
  if Encoding = nil then
    FEncoding := TEncoding.UTF8
  else
    FEncoding := Encoding;
  FOwnsStream := AOwnsStream;
end;

procedure TBinaryWriter.Close;
begin
  if FOwnsStream then
    FreeAndNil(FStream);
end;

constructor TBinaryWriter.Create(const Filename: string; Append: Boolean; Encoding: TEncoding);
begin
  if (not FileExists(Filename)) or (not Append) then
    FStream := TFileStream.Create(Filename, fmCreate)
  else
  begin
    FStream := TFileStream.Create(Filename, fmOpenWrite);
    FStream.Seek(0, soEnd);
  end;
  Create(FStream, Encoding, True);
end;

constructor TBinaryWriter.Create(const Filename: string; Append: Boolean);
begin
  Create(Filename, Append, nil);
end;

constructor TBinaryWriter.Create;
begin
  Create(TNullStream.Create, nil, True);
end;

destructor TBinaryWriter.Destroy;
begin
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
  if FOwnsStream then
    FStream.Free;
  inherited;
end;

class destructor TBinaryWriter.Destroy;
begin
  FNull.Free;
end;

class function TBinaryWriter.GetNull: TBinaryWriter;
var
  Writer: TBinaryWriter;
begin
  if FNull = nil then
  begin
    Writer := TBinaryWriter.Create;
    Writer := AtomicCmpExchange(Pointer(FNull), Pointer(Writer), nil);
    Writer.Free;
  end;
  Result := FNull;
end;

function TBinaryWriter.GetBaseStream: TStream;
begin
  Result := FStream;
end;

function TBinaryWriter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;

procedure TBinaryWriter.Write(Value: Char);
var
  Bytes: TBytes;
begin
  if Value.IsSurrogate then
    raise EArgumentException.CreateRes(@SNoSurrogates);
  Bytes := FEncoding.GetBytes(Value);
  FStream.WriteBuffer(Bytes, Length(Bytes));
end;

procedure TBinaryWriter.Write(Value: Byte);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(Value: Boolean);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(const Value: TCharArray);
var
  Bytes: TBytes;
begin
  Bytes := FEncoding.GetBytes(Value);
  FStream.WriteBuffer(Bytes, Length(Bytes));
end;

procedure TBinaryWriter.Write(const Value: string);
var
  Bytes: TBytes;
begin
  Bytes := FEncoding.GetBytes(Value);
  Write7BitEncodedInt(Length(Bytes));
  FStream.WriteBuffer(Bytes, Length(Bytes));
end;

procedure TBinaryWriter.Write(Value: Single);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(Value: Int64);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(const Value: TBytes; Index, Count: Integer);
begin
  if Index < 0 then
    raise EArgumentOutOfRangeException.CreateResFmt(@sArgumentOutOfRange_NeedNonNegValue, ['Index']); // do not localize
  if Count < 0 then
    raise EArgumentOutOfRangeException.CreateResFmt(@sArgumentOutOfRange_NeedNonNegValue, ['Count']); // do not localize
  if Length(Value) - Index < Count  then
    raise EArgumentOutOfRangeException.CreateRes(@sArgumentOutOfRange_OffLenInvalid);
  FStream.WriteBuffer(Value, Index, Count);
end;

procedure TBinaryWriter.Write(const Value: TBytes);
begin
  FStream.WriteBuffer(Value, Length(Value));
end;

procedure TBinaryWriter.Write(const Value: TCharArray; Index, Count: Integer);
var
  Bytes: TBytes;
begin
  Bytes := FEncoding.GetBytes(Value, Index, Count);
  FStream.WriteBuffer(Bytes, Length(Bytes));
end;

procedure TBinaryWriter.Write(Value: UInt64);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(Value: Double);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(Value: SmallInt);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(Value: Integer);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(Value: Cardinal);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(Value: Word);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write(Value: ShortInt);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TBinaryWriter.Write7BitEncodedInt(Value: Integer);
begin
  repeat
    if Value > $7f then
      Write(Byte((Value and $7f) or $80))
    else
      Write(Byte(Value));
    Value := Value shr 7;
  until Value = 0;
end;

{ ComponentPlatforms }

constructor ComponentPlatformsAttribute.Create(const Platforms: TPlatformIds);
begin
  FPlatforms := Platforms;
end;

{ TLoginCredentialService }

class constructor TLoginCredentialService.Create;
begin
  FLoginHandlers := TStringList.Create;
end;

class destructor TLoginCredentialService.Destroy;
begin
  if FLoginHandlers <> nil then
    FreeAndNil(FLoginHandlers);
end;

class function TLoginCredentialService.IndexOfHandler(const Context: TLoginCredentialEvent): Integer;
var
  Method: TMethod;
begin
  for Result := FLoginHandlers.Count - 1 downto 0 do
  begin
    Method := TMethod(TLoginCredentialEventObject(FLoginHandlers.Objects[Result]).Handler);
    if (Method.Code = TMethod(Context).Code) and (Method.Data = TMethod(Context).Data) then
      Exit;
  end;
  Result := -1;
end;

constructor TLoginCredentialService.TLoginCredentialEventObject.Create(const NewHandler: TLoginCredentialEvent);
begin
  Handler := NewHandler;
end;

class procedure TLoginCredentialService.RegisterLoginHandler(const Context: string; const HandlerEvent: TLoginCredentialEvent);
var
  Index: Integer;
  Event: TLoginCredentialEventObject;
begin
  Index := IndexOfHandler(HandlerEvent);
  if (Index < 0) or not SameText(Context, FLoginHandlers[Index]) then
  begin
    Event := TLoginCredentialEventObject.Create(HandlerEvent);
    try
      FLoginHandlers.AddObject(Context, TObject(Event));
    except
      Event.Free;
      raise;
    end;
  end;
end;

class procedure TLoginCredentialService.UnregisterLoginHandler(const Context: string; const HandlerEvent: TLoginCredentialEvent);
var
  I: Integer;
  Method: TMethod;
begin
  for I := FLoginHandlers.Count - 1 downto 0 do
  begin
    Method := TMethod(TLoginCredentialEventObject(FLoginHandlers.Objects[I]).Handler);
    if SameText(FLoginHandlers[I], Context) and (Method.Code = TMethod(HandlerEvent).Code) and (Method.Data = TMethod(HandlerEvent).Data) then
    begin
      TLoginCredentialEventObject(FLoginHandlers.Objects[I]).DisposeOf;
      FLoginHandlers.Delete(I);
      Break;
    end;
  end;
end;

class function TLoginCredentialService.GetLoginCredentialEvent(const Context: string): TLoginCredentialEvent;
var
  I: Integer;
begin
  for I := FLoginHandlers.Count - 1 downto 0 do
    if SameText(Context, FLoginHandlers[I]) then
    begin
      Result := TLoginCredentialEventObject(FLoginHandlers.Objects[I]).Handler;
      Exit;
    end;
  for I := FLoginHandlers.Count - 1 downto 0 do
    if FLoginHandlers[I] = '' then
    begin
      Result := TLoginCredentialEventObject(FLoginHandlers.Objects[I]).Handler;
      Exit;
    end;
  Result := nil;
end;

class function TLoginCredentialService.GetLoginCredentials(const Context: string; Sender: TObject; const Callback: TLoginEvent): Boolean;
var
  Handler: TLoginCredentialEvent;
begin
  Result := True;
  Handler := GetLoginCredentialEvent(Context);
  if Assigned(Handler) then
    Handler(Sender, Callback, Result)
  else
    raise ELoginCredentialError.CreateRes(@SServiceNotFound);
end;

class function TLoginCredentialService.GetLoginCredentials(const Context: string; const Callback: TLoginFunc): Boolean;
var
  Proxy: TLoginFuncProxy;
begin
  Proxy := TLoginFuncProxy.Create(Callback);
  try
    Result := GetLoginCredentials(Context, Proxy, Proxy.LoginEvent);
  finally
    Proxy.Free;
  end;
end;

class function TLoginCredentialService.GetLoginCredentials(const Context: string; var Username, Password: string): Boolean;
var
  Domain: string;
begin
  Result := GetLoginCredentials(Context, Username, Password, Domain);
end;

class function TLoginCredentialService.GetLoginCredentials(const Context: string; var Username, Password, Domain: string): Boolean;
var
  LUsername, LPassword, LDomain: string;
begin
  Result := GetLoginCredentials(Context,
    function (const AUsername, APassword, ADomain: string): Boolean
    begin
      Result := True;
      LUsername := AUsername;
      LPassword := APassword;
      LDomain := ADomain;
    end);
  if Result then
  begin
    Username := LUsername;
    Password := LPassword;
    Domain := LDomain;
  end;
end;

{ TLoginCredentialService.TLoginFuncProxy }

constructor TLoginCredentialService.TLoginFuncProxy.Create(const ALoginFunc: TLoginFunc);
begin
  inherited Create;
  FLoginFunc := ALoginFunc;
end;

procedure TLoginCredentialService.TLoginFuncProxy.LoginEvent(Sender: TObject; const Username, Password, Domain: string; var Handled: Boolean);
begin
  Handled := FLoginFunc(Username, Password, Domain);
end;

{ TObserverMapping }

constructor TObserverMapping.Create;
begin
  FMappings := TStringList.Create;
end;

class destructor TObserverMapping.Destroy;
begin
  FreeAndNil(TObserverMapping.FInstance);
  inherited;
end;

destructor TObserverMapping.Destroy;
begin
  FMappings.Free;
  inherited;
end;

class function TObserverMapping.GetObserverID(const Key: string): Integer;
begin
  Result := Instance.FMappings.IndexOf(Key);
  if Result = -1 then
  begin
    Instance.FMappings.Add(Key);
    Result := Instance.FMappings.Count;
  end
  else
    Inc(Result);

  //Offset ID by reserved ones
  Result := Result + TObserverMapping.MappedID;
end;

class function TObserverMapping.Instance: TObserverMapping;
begin
  if FInstance = nil then
    FInstance := TObserverMapping.Create;
  Result := FInstance;
end;

{ TObservers }

constructor TObservers.Create;
begin
  inherited Create;
  FObservers := TDictionary<Integer, IInterfaceList>.Create;;
end;

destructor TObservers.Destroy;
begin
  FObservers.Free;
  inherited;
end;

function TObservers.CanObserve(const ID: Integer): Boolean;
begin
  Result := Assigned(FCanObserve) and FCanObserve(ID);
end;

procedure TObservers.AddObserver(const IDs: array of Integer; const AIntf: IInterface);
var
  LList: IInterfaceList;
  LEditObserver: IEditLinkObserver;
  LID: Integer;
  LObserver: IObserver;
  J, I: Integer;
begin
  if not Supports(AIntf, IObserver, LObserver) then
    raise EObserverException.CreateRes(@sObserverNoInterface);
  for I := 0 to Length(IDs) - 1 do
  begin
    if not CanObserve(IDs[I]) then
      raise EObserverException.CreateRes(@sObserverUnsupported);
    LID := IDs[I];
    if FObservers.TryGetValue(LID, LList) then
    begin
      //make sure the interface is not isinglecastobserver, since there cannot be more than one!
      if Supports(AIntf, ISingleCastObserver) then
      begin
        if Supports(AIntf, IEditLinkObserver, LEditObserver) and LEditObserver.IsReadOnly then
        begin
          // Anything to do?  If this is read only, we should allow the
          // observer for one way notifications
        end
        else if Supports(AIntf, IEditLinkObserver, LEditObserver) and not LEditObserver.IsReadOnly then
        begin
          // If this is not read only, we should make sure no other
          // editing (non-readonly) observer is present first
          for J := 0 to LList.Count - 1 do
          begin
            if Supports(LList[J], IEditLinkObserver, LEditObserver) and not LEditObserver.IsReadOnly then
              raise EObserverException.CreateRes(@sObserverMultipleSingleCast);
          end;
        end
        else
          raise EObserverException.CreateRes(@sObserverMultipleSingleCast);
      end;
    end else
    begin
      LList := TInterfaceList.Create;
      FObservers.Add(LID, LList);
    end;
    LList.Add(AIntf);
    if Assigned(FObserverAdded) then
      FObserverAdded(IDs[I], LObserver);
  end;
end;

procedure TObservers.AddObserver(const ID: Integer; const AIntf: IInterface);
begin
  AddObserver([ID], AIntf);
end;

function TObservers.GetMultiCastObserver(const ID: Integer): IInterfaceList;
var
  LList: IInterfaceList;
  LObserver: IObserver;
  I: Integer;
begin
  Result := TInterfaceList.Create;
  if FObservers.TryGetValue(ID, LList) then
  begin
    for I := 0 to LList.Count - 1 do
    begin
      if Supports(LList[I], IMultiCastObserver, LObserver) then
        if LObserver.Active then
          Result.Add(LList[I]);
    end;
  end;
  if Result.Count = 0 then
    raise EObserverException.CreateResFmt(@sObserverNoMulticastFound, [ID]);
end;

function TObservers.GetSingleCastObserver(const ID: Integer): IInterface;
var
  LList: IInterfaceList;
  LObserver: IObserver;
  LEditObserver: IEditLinkObserver;
  LLast: IInterface;
  I: Integer;
begin
  Result := nil;
  LLast := nil;
  if FObservers.TryGetValue(ID, LList) then
  begin
    //There should never be more than one single cast observer per ID
    for I := 0 to LList.Count - 1 do
    begin
      if Supports(LList.Items[0], ISingleCastObserver, LObserver) then
      begin
        LLast := LObserver;
        if LObserver.Active and Supports(LObserver, IEditLinkObserver, LEditObserver) then
          if not LEditObserver.IsReadOnly then
            Exit(LObserver);
      end;
    end;
  end;
  if LLast <> nil then
    Exit(LLast);
  raise EObserverException.CreateResFmt(@sObserverNoSinglecastFound, [ID]);
end;

function TObservers.IsObserving(const ID: Integer): Boolean;
var
  I: Integer;
  LObserver: IObserver;
  LList: IInterfaceList;
begin
  if FObservers.TryGetValue(ID, LList) then
  begin
    for I := 0 to LList.Count - 1 do
    begin
      if Supports(LList[I], IObserver, LObserver) then
        if LObserver.Active then
          Exit(True);
    end;
  end;
  Result := False;
end;

function TObservers.TryIsObserving(const ID: Integer; out AIntf: IInterface): Boolean;
var
  I: Integer;
  LObserver: IObserver;
  LList: IInterfaceList;
begin
  if FObservers.TryGetValue(ID, LList) then
  begin
    //make sure at least one the observers with this ID is active
    for I := 0 to LList.Count - 1 do
    begin
      if Supports(LList[I], IObserver, LObserver) then
        if LObserver.Active then
        begin
          AIntf := LObserver;
          Exit(True);
        end;
    end;
  end;
  Result := False;
end;

procedure TObservers.RemoveObserver(const ID: Integer;
  const AIntf: IInterface);
begin
  RemoveObserver([ID], AIntf);
end;

procedure TObservers.RemoveObserver(const IDs: array of Integer; const AIntf: IInterface);
var
  LID: Integer;
  LList: IInterfaceList;
  I: Integer;
begin
  for I := 0 to Length(IDs) - 1 do
  begin
    LID := IDs[I];
    if FObservers.TryGetValue(LID, LList) then
    begin
      LList.Remove(AIntf);
      if LList.Count = 0 then
      begin
        FObservers.Remove(LID);
        LList := nil;
      end;
    end;
  end;
end;

{ TLinkObservers }

class function TLinkObservers.GetEditGridLink(const AObservers: TObservers): IEditGridLinkObserver;
var
  LID: Integer;
begin
  LID := TObserverMapping.EditGridLinkID;
  if AObservers.IsObserving(LID) then
    Result := AObservers.GetSingleCastObserver(LID) as IEditGridLinkObserver
  else
    raise EObserverException.Create(sObserverNotAvailable);
end;

class function TLinkObservers.GetEditLink(const AObservers: TObservers): IEditLinkObserver;
var
  LID: Integer;
begin
  LID := TObserverMapping.EditLinkID;
  if AObservers.IsObserving(LID) then
    Result := AObservers.GetSingleCastObserver(LID) as IEditLinkObserver
  else
    raise EObserverException.Create(sObserverNotAvailable);
end;

class procedure TLinkObservers.EditLinkUpdate(const AObservers: TObservers);
begin
  GetEditLink(AObservers).Update;
end;

class function TLinkObservers.EditLinkTrackUpdate(const AObservers: TObservers): Boolean;
var
  LLink: IEditLinkObserver;
  LTrack: IObserverTrack;
begin
  Result := False;
  LLink := GetEditLink(AObservers);
  if Supports(LLink, IObserverTrack, LTrack) then
    if LTrack.Track then
    begin
      LLink.Update;
      Result := True;
    end;
end;

class procedure TLinkObservers.EditLinkReset(const AObservers: TObservers);
begin
  GetEditLink(AObservers).Reset;
end;

class function TLinkObservers.EditLinkIsReadOnly(const AObservers: TObservers): Boolean;
begin
  Result := GetEditLink(AObservers).IsReadOnly;
end;

class procedure TLinkObservers.EditLinkSetIsReadOnly(const AObservers: TObservers; AValue: Boolean);
begin
  GetEditLink(AObservers).IsReadOnly := AValue;
end;

class procedure TLinkObservers.EditLinkModified(AObservers: TObservers);
begin
  GetEditLink(AObservers).Modified;
end;

class function TLinkObservers.EditLinkEdit(const AObservers: TObservers): Boolean;
begin
  Result := GetEditLink(AObservers).Edit;
end;

class function TLinkObservers.EditLinkIsEditing(const AObservers: TObservers): Boolean;
begin
  Result := GetEditLink(AObservers).IsEditing;
end;

class function TLinkObservers.EditLinkIsModified(const AObservers: TObservers): Boolean;
begin
  Result := GetEditLink(AObservers).IsModified;
end;

class function TLinkObservers.EditLinkIsValidChar(const AObservers: TObservers; AKey: Char): Boolean;
begin
  Result := GetEditLink(AObservers).IsValidChar(AKey);
end;

class procedure TLinkObservers.EditGridLinkUpdate(const AObservers: TObservers);
begin
  GetEditGridLink(AObservers).Update;
end;

class procedure TLinkObservers.EditGridLinkReset(const AObservers: TObservers);
begin
  GetEditGridLink(AObservers).Reset;
end;

class function TLinkObservers.EditGridLinkIsReadOnly(const AObservers: TObservers): Boolean;
begin
  Result := GetEditGridLink(AObservers).IsReadOnly;
end;

class procedure TLinkObservers.EditGridLinkSetIsReadOnly(const AObservers: TObservers; AValue: Boolean);
begin
  GetEditGridLink(AObservers).IsReadOnly := AValue;
end;

class procedure TLinkObservers.EditGridLinkModified(const AObservers: TObservers);
begin
  GetEditGridLink(AObservers).Modified;
end;

class function TLinkObservers.EditGridLinkEdit(const AObservers: TObservers): Boolean;
begin
  Result := GetEditGridLink(AObservers).Edit;
end;

class function TLinkObservers.EditGridLinkIsEditing(const AObservers: TObservers): Boolean;
begin
  Result := GetEditGridLink(AObservers).IsEditing;
end;

class function TLinkObservers.EditGridLinkIsModified(const AObservers: TObservers): Boolean;
begin
  Result := GetEditGridLink(AObservers).IsModified;
end;

class function TLinkObservers.EditGridLinkIsValidChar(const AObservers: TObservers; AKey: Char): Boolean;
begin
  Result := GetEditGridLink(AObservers).IsValidChar(AKey);
end;

class procedure TLinkObservers.PositionLinkPosChanged(const AObservers: TObservers);
var
  LList: IInterfaceList;
  LPositionObserver: IPositionLinkObserver;
  LPositionObserver170: IPositionLinkObserver170;
  I: Integer;
begin
  if AObservers.IsObserving(TObserverMapping.PositionLinkID) then
  begin
    LList := AObservers.GetMultiCastObserver(TObserverMapping.PositionLinkID);
    for I := 0 to LList.Count - 1 do
    begin
      if Supports(LList[I], IPositionLinkObserver, LPositionObserver) then
        LPositionObserver.PosChanged
      else if Supports(LList[I], IPositionLinkObserver170, LPositionObserver170) then
        LPositionObserver170.PosChanged;
    end;
  end;
end;

class procedure TLinkObservers.PositionLinkPosChanging(const AObservers: TObservers);
var
  LList: IInterfaceList;
  LPositionObserver: IPositionLinkObserver;
  I: Integer;
begin
  if AObservers.IsObserving(TObserverMapping.PositionLinkID) then
  begin
    LList := AObservers.GetMultiCastObserver(TObserverMapping.PositionLinkID);
    for I := 0 to LList.Count - 1 do
    begin
      if Supports(LList[I], IPositionLinkObserver, LPositionObserver) then
        LPositionObserver.PosChanging;
    end;
  end;
end;

class procedure TLinkObservers.ListSelectionChanged(const AObservers: TObservers);
begin
  if AObservers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    if TLinkObservers.EditLinkIsEditing(AObservers) then
    begin
      TLinkObservers.EditLinkModified(AObservers);
      TLinkObservers.EditLinkTrackUpdate(AObservers);
      TLinkObservers.PositionLinkPosChanged(AObservers);
    end
    else
      TLinkObservers.EditLinkReset(AObservers);
  end
  else if AObservers.IsObserving(TObserverMapping.PositionLinkID) then
    TLinkObservers.PositionLinkPosChanged(AObservers);
  if AObservers.IsObserving(TObserverMapping.ControlValueID) then
  begin
    TLinkObservers.ControlValueModified(AObservers);
    TLinkObservers.ControlValueTrackUpdate(AObservers);
  end;
end;

class procedure TLinkObservers.ControlValueUpdate(AObservers: TObservers);
var
  LList: IInterfaceList;
  LControlValueObserver: IControlValueObserver;
  I: Integer;
begin
  LList := AObservers.GetMultiCastObserver(TObserverMapping.ControlValueID);
  for I := 0 to LList.Count - 1 do
  begin
    if Supports(LList[I], IControlValueObserver, LControlValueObserver) then
      LControlValueObserver.ValueUpdate;
  end;
end;

class procedure TLinkObservers.ControlValueModified(AObservers: TObservers);
var
  LList: IInterfaceList;
  LControlValueObserver: IControlValueObserver;
  I: Integer;
begin
  LList := AObservers.GetMultiCastObserver(TObserverMapping.ControlValueID);
  for I := 0 to LList.Count - 1 do
  begin
    if Supports(LList[I], IControlValueObserver, LControlValueObserver) then
      LControlValueObserver.ValueModified;
  end;
end;

class function TLinkObservers.ControlValueTrackUpdate(const AObservers: TObservers): Boolean;
var
  LList: IInterfaceList;
  LControlValueObserver: IControlValueObserver;
  LTrack: IObserverTrack;
  I: Integer;
begin
  Result := False;
  LList := AObservers.GetMultiCastObserver(TObserverMapping.ControlValueID);
  for I := 0 to LList.Count - 1 do
  begin
    if Supports(LList[I], IControlValueObserver, LControlValueObserver) then
      if Supports(LControlValueObserver, IObserverTrack, LTrack) then
        if LTrack.Track then
        begin
          LControlValueObserver.ValueUpdate;
          Result := True;
        end;
  end;
end;

class function TLinkObservers.AllowControlChange(const AControl: TComponent): Boolean;
begin
  if AControl.Observers.IsObserving(TObserverMapping.EditLinkID) then
    Result := TLinkObservers.EditLinkEdit(AControl.Observers)
  else
    Result := True;
end;

class procedure TLinkObservers.ControlChanged(const AControl: TComponent);
begin
  if AControl.Observers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    if TLinkObservers.EditLinkEdit(AControl.Observers) then
    begin
      TLinkObservers.EditLinkModified(AControl.Observers);
      TLinkObservers.EditLinkUpdate(AControl.Observers);
    end;
  end;
  if AControl.Observers.IsObserving(TObserverMapping.ControlValueID) then
  begin
    TLinkObservers.ControlValueModified(AControl.Observers);
    TLinkObservers.ControlValueUpdate(AControl.Observers);
  end;
  if AControl.Observers.IsObserving(TObserverMapping.PositionLinkID) then
    TLinkObservers.PositionLinkPosChanged(AControl.Observers);
end;

procedure CheckForCycles(const Obj: TObject; const PostFoundCycle: TPostFoundCycleProc); overload;
type
  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TManagedField;
  end;
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
  {$IFDEF CPU64BITS}
    _Padding: Integer; // Make 16 byte align for payload..
  {$ENDIF CPU64BITS}
    RefCnt: Integer;
    Length: NativeInt;
  end;
var
  Instances: TDictionary<Pointer, Integer>;
  ReferenceStack: TStack<IntPtr>;

  procedure WalkStructure(const Struct: Pointer; ATypeInfo: PTypeInfo); forward;
  procedure WalkArray(const AArray: Pointer; ATypeInfo: PTypeInfo); forward;
  procedure WalkDynArray(const AArray: Pointer; ATypeInfo: PTypeInfo); forward;
  procedure WalkInterface(const Intf: IInterface); forward;
  procedure WalkPointer(const Ref: Pointer; ATypeInfo: PTypeInfo); forward;

  procedure WalkInstance(const Instance: Pointer);
  var
    ClassType: TClass;
    InitTable: PTypeInfo;
  begin
    if Instance <> nil then
    begin
      if Instances.ContainsKey(Instance) then
      begin
        Instances[Instance] := Instances[Instance] + 1;
        if Instances[Instance] > 1 then
        begin
          PostFoundCycle(TObject(Instance).ClassName, IntPtr(Instance), ReferenceStack);
          Exit;
        end;
      end else
        Instances.Add(Instance, 1);
      ReferenceStack.Push(IntPtr(Instance));
      ClassType := TObject(Instance).ClassType;
      while ClassType <> nil do
      begin
        InitTable := PPTypeInfo(PByte(ClassType) + vmtInitTable)^;
        if InitTable <> nil then
          WalkStructure(Instance, InitTable);
        ClassType := ClassType.ClassParent;
      end;
      Instances[Instance] := Instances[Instance] - 1;
      ReferenceStack.Pop;
    end;
  end;

  function DynArrayLen(A: Pointer): NativeInt; inline;
  begin
    Result := 0;
    if A <> nil then
      Result := PDynArrayRec(PByte(A) - SizeOf(TDynArrayRec))^.Length;
  end;

  procedure WalkStructure(const Struct: Pointer; ATypeInfo: PTypeInfo);
  var
    I: Integer;
    TypeData: PFieldTable;
    Info: PTypeInfo;
    Data: PTypeData;
    FldRef: Pointer;
  begin
    TypeData := PFieldTable(GetTypeData(ATypeInfo));
    for I := 0 to TypeData.Count - 1 do
    begin
      if TypeData.Fields[I].TypeRef = nil then
        Break;
      Info := TypeData.Fields[I].TypeRef^;
      Data := GetTypeData(Info);
      FldRef := PByte(Struct) + TypeData.Fields[I].FldOffset;
      case Info.Kind of
        tkClass: WalkInstance(PPointer(FldRef)^);
        tkArray: if Data.ArrayData.ElType <> nil then WalkArray(FldRef, Info);
        tkRecord: WalkStructure(FldRef, Info);
        tkInterface: WalkInterface(IInterface(PPointer(FldRef)^));
        tkDynArray: if Data.elType <> nil then WalkDynArray(PPointer(FldRef)^, Info);
        tkPointer: if Data.RefType <> nil then WalkPointer(PPointer(FldRef)^, Data.RefType^);
      end;
    end;
  end;

  procedure WalkDynArray(const AArray: Pointer; ATypeInfo: PTypeInfo);
  var
    I: NativeInt;
    Data: PTypeData;
    PElement: PByte;
  begin
    Data := GetTypeData(ATypeInfo);
    Assert(ATypeInfo.Kind = tkDynArray);
    if (AArray <> nil) and (Data.elType^.Kind in [tkClass, tkInterface, tkRecord, tkPointer, tkDynArray, tkArray]) then
    begin
      PElement := AArray;
      for I := 0 to DynArrayLen(PElement) - 1 do
      begin
        case Data.elType^.Kind of
          tkClass: WalkInstance(PPointer(PElement)^);
          tkInterface: WalkInterface(IInterface(PPointer(PElement)^));
          tkRecord: WalkStructure(PElement, Data.elType^);
          tkDynArray: WalkDynArray(PElement, Data.elType^);
          tkArray: WalkArray(PElement, Data.elType^);
          tkPointer: WalkPointer(PPointer(PElement)^, Data.RefType^);
        end;
        Inc(PElement, Data.elSize);
      end;
    end;
  end;

  procedure WalkArray(const AArray: Pointer; ATypeInfo: PTypeInfo);
  var
    I: Integer;
    PElement: PByte;
    Data: PTypeData;
    ElSize: Integer;
  begin
    Data := GetTypeData(ATypeInfo);
    ElSize := Data.ArrayData.Size div Data.ArrayData.ElCount;
    PElement := AArray;
    if AArray <> nil then
    begin
      for I := 0 to Data.ArrayData.ElCount - 1 do
      begin
        case Data.ArrayData.ElType^.Kind of
          tkClass: WalkInstance(PPointer(PElement)^);
          tkInterface: WalkInterface(IInterface(PPointer(PElement)^));
          tkRecord: WalkStructure(PElement, Data.ArrayData.ElType^);
          tkArray: WalkArray(PElement, Data.ArrayData.ElType^);
          tkDynArray: WalkDynArray(PPointer(PElement)^, Data.ArrayData.ElType^);
          tkPointer: WalkPointer(PPointer(PElement)^, Data.ArrayData.ElType^);
        end;
        Inc(PElement, ElSize);
      end;
    end;
  end;

  procedure WalkInterface(const Intf: IInterface);
  var
    O: TObject;
  begin
    O := TObject(Intf);
    if O <> nil then
      WalkInstance(O);
  end;

  procedure WalkPointer(const Ref: Pointer; ATypeInfo: PTypeInfo);
  var
    Data: PTypeData;
  begin
    Data := GetTypeData(ATypeInfo);
    case ATypeInfo.Kind of
      tkClass: WalkInstance(Ref);
      tkInterface: WalkInterface(IInterface(Ref));
      tkRecord: WalkStructure(Ref, ATypeInfo);
      tkArray: WalkArray(Ref, ATypeInfo);
      tkDynArray: WalkArray(Ref, ATypeInfo);
      tkPointer: WalkPointer(Ref, Data.RefType^);
    end;
  end;

begin
  Instances := TDictionary<Pointer, Integer>.Create;
  try
    ReferenceStack := TStack<IntPtr>.Create;
    try
      WalkInstance(Obj);
    finally
      ReferenceStack.Free;
    end;
  finally
    Instances.Free;
  end;
end;

procedure CheckForCycles(const Intf: IInterface; const PostFoundCycle: TPostFoundCycleProc); overload;
var
  O: TObject;
begin
  O := TObject(Intf);
  if O <> nil then
    CheckForCycles(O, PostFoundCycle);
end;

type
  TMultiWaitEventImpl = class(TMultiWaitEvent)
  private
    FState: Integer;
    FWaiters: TArray<TMultiWaitEvent.TWaitInfo>;
  protected type
    TWaitInfo = TMultiWaitEvent.TWaitInfo;
    PWaitInfo = TMultiWaitEvent.PWaitInfo;
  protected
    class function CreateInstance: TMultiWaitEvent; override;
    class procedure ArgumentException; override;
    class function GetTickCount: Cardinal; override;
    procedure Lock; override;
    procedure Unlock; override;
    procedure AtomicSetEventState(State: Boolean); override;
    procedure NotifyWaiters; override;
    procedure ClearWaiters; override;
    procedure PushWaiter(const Waiter: TMultiWaitEvent.TWaitInfo); override;
    procedure RemoveWaiter(Index: Integer); override;
    function GetWaiterCount: Integer; override;
    function GetWaiter(Index: Integer): TMultiWaitEvent.PWaitInfo; override;
  public
    constructor Create; // inject a Create method into this class to keep from overflowing the stack

    function WaitFor(Timeout: Cardinal = INFINITE): TWaitResult; override;
  end;

{ TMultiWaitEventImpl }

class procedure TMultiWaitEventImpl.ArgumentException;
begin
  raise EArgumentOutOfRangeException.CreateRes(@sMustWaitOnOneEvent);
end;

procedure TMultiWaitEventImpl.AtomicSetEventState(State: Boolean);
var
  LState: Integer;
begin
  repeat
    LState := FState;
  until AtomicCmpExchange(FState, Integer(State), LState) = LState;
end;

procedure TMultiWaitEventImpl.ClearWaiters;
begin
  FWaiters := nil;
end;

constructor TMultiWaitEventImpl.Create;
begin
end;

class function TMultiWaitEventImpl.CreateInstance: TMultiWaitEvent;
begin
  Result := TMultiWaitEventImpl.Create;
end;

class function TMultiWaitEventImpl.GetTickCount: Cardinal;
begin
  Result := TThread.GetTickCount;
end;

function TMultiWaitEventImpl.GetWaiter(Index: Integer): PWaitInfo;
begin
  Result := @FWaiters[Index];
end;

function TMultiWaitEventImpl.GetWaiterCount: Integer;
begin
  Result := Length(FWaiters);
end;

procedure TMultiWaitEventImpl.Lock;
begin
  TMonitor.Enter(Self);
end;

procedure TMultiWaitEventImpl.NotifyWaiters;
begin
  TMonitor.PulseAll(Self);
end;

procedure TMultiWaitEventImpl.Unlock;
begin
  TMonitor.Exit(Self);
end;

procedure TMultiWaitEventImpl.PushWaiter(const Waiter: TWaitInfo);
begin
  FWaiters := FWaiters + [Waiter];
end;

procedure TMultiWaitEventImpl.RemoveWaiter(Index: Integer);
begin
  Delete(FWaiters, Index, 1);
end;

function TMultiWaitEventImpl.WaitFor(Timeout: Cardinal): TWaitResult;
begin
  Lock;
  try
    if TMonitor.Wait(Self, Timeout) then
      Result := TWaitResult.wrSignaled
    else
      Result := TWaitResult.wrTimeout;
  finally
    Unlock;
  end;
end;

{ TBaseAsyncResult }

procedure TBaseAsyncResult.Complete;
begin
  SetFlagsAtomic([TAsyncFlag.Completed], [TAsyncFlag.Completed]);
  if FAsyncHandle <> nil then
    FAsyncHandle.SetEvent;
end;

constructor TBaseAsyncResult.Create;
begin
  raise ENoConstructException.CreateResFmt(@sNoConstruct, [ClassName]);
end;

constructor TBaseAsyncResult.Create(const AContext: TObject);
begin
  inherited Create;
  FContext := AContext;
end;

class constructor TBaseAsyncResult.Create;
begin
  TMultiWaitEventImpl.FMultiEventType := TMultiWaitEventImpl;
end;

destructor TBaseAsyncResult.Destroy;
begin
  if not (GetIsCompleted or GetCompletedSynchronously) and (TAsyncFlag.Invoked in FAsyncFlags) then
  begin
    Cancel;
    WaitForCompletion;
  end;
  FAsyncHandle.Free;
  FInvokingException.Free;
  inherited;
end;

class procedure TBaseAsyncResult.Dispatch(const AsyncResult: TBaseAsyncResult);
begin
  AsyncResult.DoAsyncDispatch;
end;

class destructor TBaseAsyncResult.Destroy;
begin
  TMultiWaitEventImpl.FMultiEventType := nil;
end;

function TBaseAsyncResult.Cancel: Boolean;
begin
  // You cannot cancel an already completed operation.
  Result := not (FAsyncFlags * [TAsyncFlag.Cancelled, TAsyncFlag.Completed] = [TAsyncFlag.Cancelled]) and DoCancel;
  if Result then
    SetFlagsAtomic([TAsyncFlag.Cancelled], [TAsyncFlag.Cancelled]);
end;

procedure TBaseAsyncResult.DoAsyncDispatch;
begin
  if FInvokingThread = TThread.CurrentThread.ThreadID then
    SetFlagsAtomic([TAsyncFlag.Synchronous], [TAsyncFlag.Synchronous]);
  try
    try
      AsyncDispatch;
    except
      if not (TAsyncFlag.Synchronous in FAsyncFlags) then
        FInvokingException := AcquireExceptionObject
      else
        raise;
    end;
  finally
    Complete;
    _Release;
  end;
end;

function TBaseAsyncResult.DoCancel: Boolean;
begin
  // By default, all Async cannnot be cancelled. Descendants can override this behaviour and do the required processing;
  Result := False;
end;

function TBaseAsyncResult.GetAsyncContext: TObject;
begin
  Result := FContext;
end;

function TBaseAsyncResult.GetAsyncWaitEvent: TMultiWaitEvent;
var
  LMultiWait: TObject;
begin
  if FAsyncHandle = nil then
  begin
    LMultiWait := TMultiWaitEvent.Create;
{$IFDEF AUTOREFCOUNT}
    LMultiWait.__ObjAddRef;
{$ENDIF}
    if AtomicCmpExchange(Pointer(FASyncHandle), Pointer(LMultiWait), Pointer(nil)) <> nil then
    begin
{$IFDEF AUTOREFCOUNT}
      LMultiWait.__ObjRelease;
{$ENDIF}
      LMultiWait.Free;
    end;
    if GetIsCompleted then
      FAsyncHandle.SetEvent;
  end;
  Result := FAsyncHandle;
end;

function TBaseAsyncResult.GetCompletedSynchronously: Boolean;
begin
  Result := FAsyncFlags * [TAsyncFlag.Completed, TAsyncFlag.Synchronous] = [TAsyncFlag.Completed, TAsyncFlag.Synchronous];
end;

function TBaseAsyncResult.GetIsCancelled: Boolean;
begin
  Result := TAsyncFlag.Cancelled in FAsyncFlags;
end;

function TBaseAsyncResult.GetIsCompleted: Boolean;
begin
  Result := TAsyncFlag.Completed in FAsyncFlags;
end;

function TBaseAsyncResult.Invoke: IAsyncResult;
begin
  SetFlagsAtomic([TAsyncFlag.Invoked], [TAsyncFlag.Invoked]);
  FInvokingThread := TThread.CurrentThread.ThreadID;
  _AddRef;
  Result := Self;
  Schedule;
end;

procedure TBaseAsyncResult.Schedule;
begin
  TThread.Queue(nil, DoAsyncDispatch);
end;

procedure TBaseAsyncResult.SetFlagsAtomic(Value, Mask: TAsyncFlags);
var
  NewFlags, CurrentFlags: TAsyncFlags;
begin
  while True do
  begin
    CurrentFlags := FAsyncFlags;
    NewFlags := (CurrentFlags - Mask) + Value;
    if AtomicCmpExchange(Integer(FAsyncFlags), Integer(NewFlags), Integer(CurrentFlags)) = Integer(CurrentFlags) then
      Exit;
  end;
end;

procedure TBaseAsyncResult.WaitForCompletion;
var
  LException: TObject;
begin
  if not (TAsyncFlag.Completed in FAsyncFlags) and not (TAsyncFlag.Synchronous in FAsyncFlags) then
    ASyncWaitEvent.WaitFor(INFINITE);
  LException := AtomicExchange(Pointer(FInvokingException), nil);
  if LException <> nil then
    raise LException;
end;

initialization
  AddModuleUnloadProc(ModuleUnload);
{$IF Defined(MSWINDOWS)}
  GlobalNameSpace := TMultiReadExclusiveWriteSynchronizer.Create;
{$ELSEIF Defined(POSIX)}
  GlobalNameSpace := TSimpleRWSync.Create;
{$ENDIF POSIX}
  DictComparer := TOrdinalIStringComparer.Create;
  RegGroups := TRegGroups.Create;
  IntConstList := TThreadList<TIntConst>.Create;
  GlobalFixupList := TThreadList<TPropFixup>.Create;

finalization
  UnRegisterModuleClasses(HInstance);
  GlobalNameSpace.BeginWrite;
  FreeIntConstList;
  RemoveFixupReferences(nil, '');
  FreeAndNil(GlobalFixupList);
  FreeAndNil(GlobalLists);
  FreeAndNil(RegGroups);
  FreeAndNil(DictComparer);
  GlobalNameSpace := nil;
  RemoveModuleUnloadProc(ModuleUnload);
  FreeAndNil(FindGlobalComponentProcs);
{$IFDEF MSWINDOWS}
  ReleaseObjectInstanceBlocks;
{$ENDIF}
end.

