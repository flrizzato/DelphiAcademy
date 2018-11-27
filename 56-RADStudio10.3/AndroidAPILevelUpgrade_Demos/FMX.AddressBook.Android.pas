{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2014-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.AddressBook.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Generics.Collections, System.Permissions, System.Rtti, System.Types, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Provider, Androidapi.JNIBridge, Androidapi.JNI.Embarcadero,
  Androidapi.JNI.Net, FMX.Graphics, FMX.AddressBook, FMX.AddressBook.Types, FMX.Surfaces;

type
  TAndroidAddressBookContact = class;
  TAddressBookChangesListener = class;

  /// <summary>Implementation AddressBook for Android</summary>
  TAndroidAddressBook = class(TInterfacedObject, IFMXAddressBookService)
  private
    FObserver: JAddressBookObserver;
    FListener: TAddressBookChangesListener;
    FProcessing: Boolean;
    FRevertTimeStamp: TDateTime;
    FOnExternalChange: TExternalChangeEvent;
    FOnPermissionRequest: TPermissionRequestEvent;
    function RecordExists(const AContentType: JString; const AContact: TAndroidAddressBookContact): Boolean;
    procedure SaveStructureName(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SaveOrganization(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SaveNickName(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SavePhones(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SaveAddresses(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SaveDates(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SaveMessagingServices(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SavePhoto(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SaveURLs(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SaveEmails(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure SaveRelatedNames(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
    procedure FetchContacts(var AContacts: TAddressBookContacts; const AFilter: string = '');
    procedure FetchGroups(const AGroups: TAddressBookGroups; const AFilter: string = '');
    procedure OnRequestPermissionsResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
  protected
    /// <summary>Invokes user <c>OnPermissionRequest</c> event handler</summary>
    procedure DoPermissionRequest(const AMessage: string; const AAccessGranted: Boolean);
    /// <summary>Sets up address book fields, listener and observers</summary>
    procedure InitializeAddressBook;
    /// <summary>Invokes user <c>OnContactChange</c> event handler</summary>
    procedure DoExternalChange;
    { IFMXAddressBookService }
    { Changes }
    procedure RevertCurrentChangesAndUpdate;
    { Permissions }
    procedure RequestPermission(ADisplayRationaleEvent: TDisplayRationaleEvent = nil);
    function AuthorizationStatus(const AAccessType: TAddressBookAccessType): TAuthorizationStatus;
    {$REGION 'Sources'}
    procedure AllSources(var ASources: TAddressBookSources);
    function SourceByID(const AID: string): TAddressBookSource;
    function DefaultSource: TAddressBookSource;
    {$ENDREGION}
    { Contacts }
    procedure AllContacts(const ASource: TAddressBookSource; var AContacts: TAddressBookContacts);
    function ContactByID(const AID: Int32): TAddressBookContact;
    function CreateContact(const ASource: TAddressBookSource = nil): TAddressBookContact;
    procedure SaveContact(const AContact: TAddressBookContact);
    procedure RemoveContact(const AContact: TAddressBookContact); overload;
    procedure RemoveContact(const AID: Integer); overload;
    { Groups }
    procedure AllGroups(const ASource: TAddressBookSource; var AGroups: TAddressBookGroups);
    function GroupByID(const AID: Int32): TAddressBookGroup;
    function CreateGroup(const ASource: TAddressBookSource = nil): TAddressBookGroup;
    procedure SaveGroup(const AGroup: TAddressBookGroup);
    procedure RemoveGroup(const AGroup: TAddressBookGroup); overload;
    procedure RemoveGroup(const AID: Integer); overload;
    { Contacts in Group }
    procedure AllContactsInGroups(const AGroups: TAddressBookGroups; var AContacts: TAddressBookContacts);
    procedure AddContactIntoGroup(const AGroup: TAddressBookGroup; const AContact: TAddressBookContact);
    procedure RemoveContactFromGroup(const AGroup: TAddressBookGroup; const AContact: TAddressBookContact);
    { Handlers }
    function GetOnPermissionRequest: TPermissionRequestEvent;
    procedure SetOnPermissionRequest(const AHandler: TPermissionRequestEvent);
    function GetOnExternalChange: TExternalChangeEvent;
    procedure SetOnExternalChange(const AHandler: TExternalChangeEvent);
  public
    destructor Destroy; override;
    /// <summary>Start batch processing of the contacts database. </summary>
    /// <remarks>Android notifies about every change in the contacts database tables. Contacts may hold several records
    /// in the table, updating such contacts them may cause multiple onChange events. This pair of methods allows
    /// to block invocation of OnExternalChange during batch operations.</remarks>
    procedure BeginProcessing;
    /// <summary>End batch processing of the contacts database and invoke OnExternalChange if the data were modified.</summary>
    procedure EndProcessing;
    property Processing: Boolean read FProcessing;
    property RevertTimeStamp: TDateTime read FRevertTimeStamp;
  end;

  TAndroidAddressBookSource = class(TAddressBookSource)
  private
    FAccountType: string;
    FAccountName: string;
  protected
    function GetID: string; override;
    function GetSourceName: string; override;
    function GetSourceType: string; override;
  public
    constructor Create(const AAccountName, AAccountType: string);
  end;

  TAddressBookChangesListener = class(TJavaLocal, JOnAddressBookChangesListener)
  private
    [Weak] FAddressBook: TAndroidAddressBook;
  public
    constructor Create(const AAddressBook: TAndroidAddressBook);
    { JOnAddressBookChangesListener }
    procedure onChanged(selfChange: Boolean); cdecl;
  end;

  TRecordState = (Adding, Viewing, Modification);

  TAndroidAddressBookGroup = class(TAddressBookGroup)
  public const
    UndefinedID = -1;
  private
    [Weak] FAddressBook: TAndroidAddressBook;
    FState: TRecordState;
    FID: Int64;
    FName: string;
    FNameChangeTimestamp: TDateTime;
    FAccountType: string;
    FAccountName: string;
    procedure SetID(const AValue: Int64);
  protected
    procedure SetName(const AValue: string); override;
    function GetName: string; override;
    function GetID: Integer; override;
    function GetSource: TAddressBookSource; override;
  public
    constructor CreateNew(const AAddressBook: TAndroidAddressBook; const ASource: TAddressBookSource);
    constructor CreateFromExisting(const AAddressBook: TAndroidAddressBook; const AID: Int64);
    property AccountType: string read FAccountType write FAccountType;
    property AccountName: string read FAccountName write FAccountName;
    property ID: Int64 read FID write SetID;
    property State: TRecordState read FState;
  end;

  TTranslateCallback = function (const AType: Integer): string;

  /// <summary>Implementation personal contact information on Android</summary>
  TAndroidAddressBookContact = class(TAddressBookContact)
  public type
    TFieldValue = record
      Value: TValue;
      TimeStamp: TDateTime;
      constructor Create(const AValue: TValue; const ATimeStamp: TDateTime);
    end;
    TContactChanges = TDictionary<TContactField, TFieldValue>;
  public const
    UndefinedID = -1;
  private
    [Weak] FAddressBook: TAndroidAddressBook;
    FState: TRecordState;
    FContactID: Integer;
    FRawContactID: Integer;
    FAccountType: string;
    FAccountName: string;
    { Insert / Update operations }
    FListChanges: TContactChanges;
    procedure SetContactID(const Value: Integer);
    function GetContactID: Integer;
    function GetAccountName: string;
  private
    { Ordinary types }
    function GetStringFieldValue(const AFieldName: JString; const AContentType: JString): string;
    { Lists }
    function GetDates: TContactDates; overload;
  protected
    { Ordinary types }
    function GetStringValue(const AIndex: TContactField): string; override;
    procedure SetStringValue(const AIndex: TContactField; const AValue: string); override;
    function GetBitmapValue(const AIndex: TContactField): TBitmapSurface; override;
    procedure SetBitmapValue(const AIndex: TContactField; const AValue: TBitmapSurface); override;
    function GetDateTimeValue(const AIndex: TContactField): TDateTime; override;
    procedure SetDateTimeValue(const AIndex: TContactField; const AValue: TDateTime); override;
    { Custom fields }
    function GetDates(const AIndex: TContactField): TContactDates; overload; override;
    procedure SetDates(const AIndex: TContactField; const AValue: TContactDates); override;
    function GetAddresses(const AIndex: TContactField): TContactAddresses; override;
    procedure SetAddresses(const AIndex: TContactField; const AValue: TContactAddresses); override;
    function GetSocialProfiles(const AIndex: TContactField): TContactSocialProfiles; override;
    procedure SetSocialProfiles(const AIndex: TContactField; const AValue: TContactSocialProfiles); override;
    function GetMessagingServices(const AIndex: TContactField): TContactMessagingServices; override;
    procedure SetMessagingServices(const AIndex: TContactField; const AValue: TContactMessagingServices); override;
    function GetPhones(const AIndex: TContactField): TContactPhones; override;
    procedure SetPhones(const AIndex: TContactField; const AValue: TContactPhones); override;
    function GetEmails(const AIndex: TContactField): TContactEmails; override;
    procedure SetEmails(const AIndex: TContactField; const AValue: TContactEmails); override;
    function GetRelatedNames(const AIndex: TContactField): TContactRelatedNames; override;
    procedure SetRelatedNames(const AIndex: TContactField; const AValue: TContactRelatedNames); override;
    function GetURLs(const AIndex: TContactField): TContactURLs; override;
    procedure SetURLs(const AIndex: TContactField; const AValue: TContactURLs); override;
    function GetSource: TAddressBookSource; override;
    function GetID: Int32; override;
    function GetContactKind: TContactKind; override;
    function GetDisplayName: string; override;
  public
    constructor CreateNew(const AAddressBook: TAndroidAddressBook; const ASource: TAddressBookSource); reintroduce; overload;
    constructor CreateFromExisting(const AAddressBook: TAndroidAddressBook; const AContactID: Integer); reintroduce; overload;
    destructor Destroy; override;
    /// <summary>Checks, was specified field changed or not?</summary>
    function HasChanges(const AFieldType: TContactField): Boolean;
    function HasAtLeastOneChange(const AFieldTypes: TContactFields): Boolean;
    /// <summary>Changes state from View -> Modified</summary>
    procedure Modified;
    procedure AllGroups(var AGroups: TAddressBookGroups); override;
  public
    /// <summary>Value of "contacts._id" table</summary>
    property ContactID: Integer read GetContactID write SetContactID;
    /// <summary>Value of "raw_contacts._id" table</summary>
    property RawContactID: Integer read FRawContactID write FRawContactID;
    /// <summary>Value of "raw_contacts.sourceid" table</summary>
    property AccountType: string read FAccountType write FAccountType;
    /// <summary>Value of "raw_contacts.sourceid" table</summary>
    property AccountName: string read GetAccountName write FAccountName;
    /// <summary>State of current contact record</summary>
    property State: TRecordState read FState;
    /// <summary>List of changed fields values. If field was changed, this list will contains pair field type and new value</summary>
    property Changes: TContactChanges read FListChanges;
  end;

  /// <summary>Addapter for simplification of working with JContentProviderOperation. Works with delphi types</summary>
  TOperationBuilderAdapter = class
  private
    FBuilder: JContentProviderOperation_Builder;
    constructor Create;
  public
    { Construction }
    /// <summary>Creates query for inserting new record. <c>RawContactID</c> will be linked with previous operation</sumary>
    class function CreateInsert(const AContentItemType: JString): TOperationBuilderAdapter; overload;
    /// <summary>Creates query for inserting new record with specified <c>RawContactID</c>.</sumary>
    class function CreateInsert(const AContentItemType: JString; const ARawContactID: Int32): TOperationBuilderAdapter; overload;
    /// <summary>Creates query for updating existed record with specified <c>RawContactID</c>.</sumary>
    class function CreateUpdate(const AContentItemType: JString; const ARawContactID: Int32): TOperationBuilderAdapter; overload;
    /// <summary>Creates query for deleting record with specified <c>RawContactID</c>.</sumary>
    class function CreateDelete(const AContentItemType: JString; const ARawContactID: Int32): TOperationBuilderAdapter; overload;
    { Parameters }
    procedure AddValue(const AFieldName: string; const AValue: string); overload;
    procedure AddValue(const AFieldName: JString; const AValue: string); overload;
    procedure AddValue(const AFieldName: string; const AValue: JString); overload;
    procedure AddValue(const AFieldName: string; const AValue: Int64); overload;
    procedure AddValue(const AFieldName: string; const ABitmapSurface: TBitmapSurface); overload;
    procedure AddValue(const AFieldName: JString; const AValue: TDateTime); overload;
    { Build }
    /// <summary>Creates and returns instance of JContentProviderOperation based on filled parameters</summary>
    function Build: JContentProviderOperation;
  end;

  TQueryAdapter = class
  public type
    TQueryCallback = reference to procedure (ACursor: JCursor);
  private
    FContentURI: Jnet_Uri;
    FFields: TJavaObjectArray<JString>;
    FParams: TJavaObjectArray<JString>;
    FFilter: string;
  public
    constructor Create(const AContentURI: Jnet_Uri);
    destructor Destroy; override;
    procedure SetFields(const ASource: array of const);
    procedure SetFilter(const AFilter: string; const AParams: array of const);
    function Execute(const ACallback: TQueryCallback; const AOnlyFirst: Boolean = False): Boolean;
  public
    property Filter: string read FFilter write FFilter;
  end;

  TAndroidAddressBookHelper = class
  public
    class function IntegerToPhoneKind(const AType: Integer): TContactPhone.TLabelKind; static;
    class function PhoneKindToInteger(const AKind: TContactPhone.TLabelKind): Integer; static;
    class function IntegerToURLKind(const AType: Integer): TContactURL.TLabelKind; static;
    class function URLKindToInteger(const AKind: TContactURL.TLabelKind): Integer; static;
    class function IntegerToEmailKind(const AType: Integer): TContactEmail.TLabelKind; static;
    class function EmailKindToInteger(const AKind: TContactEmail.TLabelKind): Integer; static;
    class function IntegerToRelatedNamesKind(const AType: Integer): TContactRelatedName.TLabelKind; static;
    class function RelatedNameKindToInteger(const AKind: TContactRelatedName.TLabelKind): Integer; static;
    class function IntegerToDateKind(const AType: Integer): TContactDate.TLabelKind; static;
    class function DateKindToInteger(const AType: TContactDate.TLabelKind): Integer; static;
    class function IntegerToMessagingServiceKind(const AType: Integer): TContactMessagingService.TServiceKind; static;
    class function MessagingServiceKindToInteger(const AKind: TContactMessagingService.TServiceKind): Integer; static;
    class function IntegerToMessagingServiceLabelKind(const AType: Integer): TContactMessagingService.TLabelKind; static;
    class function MessagingServiceLabelKindToInteger(const AKind: TContactMessagingService.TLabelKind): Integer; static;
    class function IntegerToAddressLabelKind(const AType: Integer): TContactAddress.TLabelKind; static;
    class function AddressKindToInteger(const AKind: TContactAddress.TLabelKind): Integer; static;
  end;

{ TAndroidAddressBookServices }

  TAndroidAddressBookServices = class(TInterfacedObject, IFMXAddressBookFactory, IFMXAddressBookSupportedLabelKinds)
  public
    { IFMXAddressBookFactory }
    function CreateAddressBook: IFMXAddressBookService;
    { IFMXAddressBookSupportedLabelKinds }
    function AddressesLabelKinds: TContactAddress.TLabelKinds;
    function SocialProfilesLabelKinds: TContactSocialProfile.TLabelKinds;
    function SocialProfilesServiceKinds: TContactSocialProfile.TServiceKinds;
    function MessagingServicesLabelKinds: TContactMessagingService.TLabelKinds;
    function MessagingServicesKinds: TContactMessagingService.TServiceKinds;
    function DatesLabelKinds: TContactDate.TLabelKinds;
    function PhonesLabelKinds: TContactPhone.TLabelKinds;
    function EmailsLabelKinds: TContactEmail.TLabelKinds;
    function RelatedNamesLabelKinds: TContactRelatedName.TLabelKinds;
    function URLsLabelKinds: TContactURL.TLabelKinds;
  end;

procedure RegisterService;

implementation

uses
  System.SysUtils, System.Classes, System.Math, Androidapi.Helpers, Androidapi.JNI,
  Androidapi.JNI.Accounts, Androidapi.JNI.Os, FMX.Helpers.Android, FMX.Platform, FMX.Consts, FMX.Types;

procedure RegisterService;
var
  AddressBookServices: TAndroidAddressBookServices;
begin
  AddressBookServices := TAndroidAddressBookServices.Create;
  TPlatformServices.Current.AddPlatformService(IFMXAddressBookFactory, AddressBookServices);
  TPlatformServices.Current.AddPlatformService(IFMXAddressBookSupportedLabelKinds, AddressBookServices);
end;

const
  AndroidSystemDateFormat = 'yyyy-mm-dd';

function TryJavaDateStringToDateTime(const ASource: JString; out ADate: TDateTime): Boolean;
const
  EmptyYear = '--';
var
  FormatSettings: TFormatSettings;
  DateStr: string;
  Parts: TArray<string>;
begin
  FormatSettings := TFormatSettings.Invariant;
  FormatSettings.DateSeparator := '-';
  FormatSettings.ShortDateFormat := AndroidSystemDateFormat;
  DateStr := JStringToString(ASource);
  if DateStr.StartsWith(EmptyYear) then
  begin
    // Date without Year part
    DateStr := DateStr.Remove(0, 2);
    Parts := DateStr.Split(FormatSettings.DateSeparator);
    if Length(Parts) = 2 then
    begin
      ADate := EncodeDate(1900, Parts[0].ToInteger, Parts[1].ToInteger);
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := TryStrToDate(DateStr, ADate, FormatSettings);
end;

function DateToJavaDate(const ADate: TDateTime): JString;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := TFormatSettings.Invariant;
  FormatSettings.DateSeparator := '-';
  FormatSettings.ShortDateFormat := AndroidSystemDateFormat;
  Result := StringToJString(DateToStr(ADate, FormatSettings));
end;

{ TAndroidAddressBook }

function TAndroidAddressBook.RecordExists(const AContentType: JString; const AContact: TAndroidAddressBookContact): Boolean;
const
  Filter = 'mimetype = ? AND raw_contact_id = ?';
var
  DataCursor: JCursor;
  Projection: TJavaObjectArray<JString>;
  QueryParams: TJavaObjectArray<JString>;
begin
  Result := False;
  Projection := TJavaObjectArray<JString>.Create(0);
  QueryParams := CreateJavaStringArray([AContentType, AContact.RawContactID]);
  try
    try
      DataCursor := TAndroidHelper.ContentResolver.query(TJContactsContract_Data.JavaClass.CONTENT_URI, Projection,
        StringToJString(Filter), QueryParams, nil);
      if DataCursor <> nil then
        try
          Result := DataCursor.getCount > 0;
        finally
          DataCursor.close;
        end;
    except on E: EJNIException do
      raise EAddressBookExecute.CreateFmt(SCannotCheckExistingDataRecord, [E.Message]);
    end;
  finally
    QueryParams.Free;
    Projection.Free;
  end;
end;

procedure TAndroidAddressBook.SaveStructureName(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
var
  Builder: TOperationBuilderAdapter;
begin
  case AContact.State of
    TRecordState.Adding:
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
    TRecordState.Modification:
      if RecordExists(TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE, AContact) then
        Builder := TOperationBuilderAdapter.CreateUpdate(TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID)
      else
        Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
  else
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);
  end;

  try
    if AContact.HasChanges(TContactField.FirstName) then
      Builder.AddValue(TJCommonDataKinds_StructuredName.JavaClass.GIVEN_NAME, AContact.FirstName);

    if AContact.HasChanges(TContactField.LastName) then
      Builder.AddValue(TJCommonDataKinds_StructuredName.JavaClass.FAMILY_NAME, AContact.LastName);

    if AContact.HasChanges(TContactField.MiddleName) then
      Builder.AddValue(TJCommonDataKinds_StructuredName.JavaClass.MIDDLE_NAME, AContact.MiddleName);

    if AContact.HasChanges(TContactField.Prefix) then
      Builder.AddValue(TJCommonDataKinds_StructuredName.JavaClass.PREFIX, AContact.Prefix);

    if AContact.HasChanges(TContactField.Suffix) then
      Builder.AddValue(TJCommonDataKinds_StructuredName.JavaClass.SUFFIX, AContact.Suffix);

    if AContact.HasChanges(TContactField.FirstNamePhonetic) then
      Builder.AddValue(TJCommonDataKinds_StructuredName.JavaClass.PHONETIC_GIVEN_NAME, AContact.FirstNamePhonetic);

    if AContact.HasChanges(TContactField.LastNamePhonetic) then
      Builder.AddValue(TJCommonDataKinds_StructuredName.JavaClass.PHONETIC_FAMILY_NAME, AContact.LastNamePhonetic);

    if AContact.HasChanges(TContactField.MiddleNamePhonetic) then
      Builder.AddValue(TJCommonDataKinds_StructuredName.JavaClass.PHONETIC_MIDDLE_NAME, AContact.MiddleNamePhonetic);
    AOperations.add(Builder.Build);
  finally
    Builder.Free;
  end;
end;

procedure TAndroidAddressBook.SaveURLs(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
var
  Value: TContactURL;
  Builder: TOperationBuilderAdapter;
begin
  if AContact.State = TRecordState.Viewing then
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);

  if AContact.State = TRecordState.Modification then
  begin
    Builder := TOperationBuilderAdapter.CreateDelete(TJCommonDataKinds_Website.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
    try
      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;

  for Value in AContact.URLs do
  begin
    if AContact.State = TRecordState.Adding then
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Website.JavaClass.CONTENT_ITEM_TYPE)
    else
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Website.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);

    try
      Builder.AddValue('data1', Value.URL);
      Builder.AddValue('data2', TAndroidAddressBookHelper.URLKindToInteger(Value.LabelKind));
      if (Value.LabelKind = TContactURL.TLabelKind.Custom) or not (Value.LabelKind in TContactURL.SupportedLabelKinds) then
        Builder.AddValue('data3', Value.LabelText)
      else
        Builder.AddValue('data3', string.Empty);

      AOperations.add(Builder.build);
    finally
      Builder.Free;
    end;
  end;
end;

procedure TAndroidAddressBook.SetOnExternalChange(const AHandler: TExternalChangeEvent);
begin
  FOnExternalChange := AHandler;
end;

procedure TAndroidAddressBook.SetOnPermissionRequest(const AHandler: TPermissionRequestEvent);
begin
  FOnPermissionRequest := AHandler;
end;

function TAndroidAddressBook.SourceByID(const AID: string): TAddressBookSource;
var
  Sources: TAddressBookSources;
  I: Integer;
begin
  Result := nil;
  Sources := TAddressBookSources.Create;
  AllSources(Sources);
  for I := 0 to Sources.Count - 1 do
    if SameText(Sources[I].ID, AID) then
    begin
      Result := Sources[I];
      Break;
    end;
end;

procedure TAndroidAddressBook.SaveOrganization(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
var
  Builder: TOperationBuilderAdapter;
begin
  case AContact.State of
    TRecordState.Adding:
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Organization.JavaClass.CONTENT_ITEM_TYPE);
    TRecordState.Modification:
      if RecordExists(TJCommonDataKinds_Organization.JavaClass.CONTENT_ITEM_TYPE, AContact) then
        Builder := TOperationBuilderAdapter.CreateUpdate(TJCommonDataKinds_Organization.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID)
      else
        Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Organization.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
  else
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);
  end;

  try
    if AContact.HasChanges(TContactField.Organization) then
      Builder.AddValue(TJCommonDataKinds_Organization.JavaClass.COMPANY, AContact.Organization);

    if AContact.HasChanges(TContactField.JobTitle) then
      Builder.AddValue(TJCommonDataKinds_Organization.JavaClass.TITLE, AContact.JobTitle);

    if AContact.HasChanges(TContactField.Department) then
      Builder.AddValue(TJCommonDataKinds_Organization.JavaClass.DEPARTMENT, AContact.Department);
    AOperations.add(Builder.Build);
  finally
    Builder.Free;
  end;
end;

procedure TAndroidAddressBook.SaveNickName(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
var
  Builder: TOperationBuilderAdapter;
begin
  case AContact.State of
    TRecordState.Adding:
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Nickname.JavaClass.CONTENT_ITEM_TYPE);
    TRecordState.Modification:
      if RecordExists(TJCommonDataKinds_Nickname.JavaClass.CONTENT_ITEM_TYPE, AContact) then
        Builder := TOperationBuilderAdapter.CreateUpdate(TJCommonDataKinds_Nickname.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID)
      else
        Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Nickname.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
  else
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);
  end;

  try
    if AContact.HasChanges(TContactField.NickName) then
      Builder.AddValue(TJCommonDataKinds_Nickname.JavaClass.NAME, AContact.NickName);
    AOperations.add(Builder.Build);
  finally
    Builder.Free;
  end;
end;

procedure TAndroidAddressBook.SaveAddresses(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
const
  CustomLabelType = 0;
var
  Builder: TOperationBuilderAdapter;
  Address: TContactAddress;
begin
  if AContact.State = TRecordState.Viewing then
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);

  if AContact.State = TRecordState.Modification then
  begin
    Builder := TOperationBuilderAdapter.CreateDelete(TJCommonDataKinds_StructuredPostal.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
    try
      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;

  for Address in AContact.Addresses do
  begin
    if AContact.State = TRecordState.Adding then
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_StructuredPostal.JavaClass.CONTENT_ITEM_TYPE)
    else
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_StructuredPostal.JavaClass.CONTENT_ITEM_TYPE,
        AContact.RawContactID);
    try
      Builder.AddValue(TJCommonDataKinds_StructuredPostal.JavaClass.CITY, Address.City);
      Builder.AddValue(TJCommonDataKinds_StructuredPostal.JavaClass.COUNTRY, Address.Country);
      Builder.AddValue(TJCommonDataKinds_StructuredPostal.JavaClass.STREET, Address.Street);
      Builder.AddValue(TJCommonDataKinds_StructuredPostal.JavaClass.REGION, Address.State);
      Builder.AddValue(TJCommonDataKinds_StructuredPostal.JavaClass.POSTCODE, Address.ZIP);
      Builder.AddValue('data2', TAndroidAddressBookHelper.AddressKindToInteger(Address.LabelKind));
      if (Address.LabelKind = TContactAddress.TLabelKind.Custom) or not Address.IsLabelKindSupported then
        Builder.AddValue('data3', Address.LabelText)
      else
        Builder.AddValue('data3', string.Empty);

      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;
end;

procedure TAndroidAddressBook.SaveDates(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
const
  BirthdayType = 3;
var
  Builder: TOperationBuilderAdapter;
  DateEvent: TContactDate;
begin
  if AContact.State = TRecordState.Viewing then
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);

  if AContact.State = TRecordState.Modification then
  begin
    Builder := TOperationBuilderAdapter.CreateDelete(TJCommonDataKinds_Event.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
    try
      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;

  // Android stores Birthday in dates list and doesn't give special access. We provide separated access to Birthday.
  // So we emulate working with this field and save it in dates list.
  if not IsNan(AContact.Birthday) then
  begin
    if AContact.State = TRecordState.Adding then
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Event.JavaClass.CONTENT_ITEM_TYPE)
    else
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Event.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);

    try
      Builder.AddValue(TJCommonDataKinds_Event.JavaClass.START_DATE, AContact.Birthday);
      Builder.AddValue('data2', BirthdayType);
      Builder.AddValue('data3', SBirthday);

      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;

  for DateEvent in AContact.Dates do
  begin
    if AContact.State = TRecordState.Adding then
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Event.JavaClass.CONTENT_ITEM_TYPE)
    else
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Event.JavaClass.CONTENT_ITEM_TYPE, AContact.ContactID);

    Builder.AddValue(TJCommonDataKinds_Event.JavaClass.START_DATE, DateEvent.Date);
    Builder.AddValue('data2', TAndroidAddressBookHelper.DateKindToInteger(DateEvent.LabelKind));
    if (DateEvent.LabelKind = TContactDate.TLabelKind.Custom) or not DateEvent.IsLabelKindSupported then
      Builder.AddValue('data3', DateEvent.LabelText)
    else
      Builder.AddValue('data3', string.Empty);

    AOperations.add(Builder.Build);
  end;
end;

procedure TAndroidAddressBook.SaveEmails(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
var
  Value: TContactEmail;
  Builder: TOperationBuilderAdapter;
begin
  if AContact.State = TRecordState.Viewing then
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);

  if AContact.State = TRecordState.Modification then
  begin
    Builder := TOperationBuilderAdapter.CreateDelete(TJCommonDataKinds_Email.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
    try
      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;

  for Value in AContact.EMails do
  begin
    if AContact.State = TRecordState.Adding then
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Email.JavaClass.CONTENT_ITEM_TYPE)
    else
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Email.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);

    try
      Builder.AddValue('data1', Value.Email);
      Builder.AddValue('data2', TAndroidAddressBookHelper.EmailKindToInteger(Value.LabelKind));
      if (Value.LabelKind = TContactEmail.TLabelKind.Custom) or not Value.IsLabelKindSupported then
        Builder.AddValue('data3', Value.LabelText)
      else
        Builder.AddValue('data3', string.Empty);

      AOperations.add(Builder.build);
    finally
      Builder.Free;
    end;
  end;
end;

procedure TAndroidAddressBook.SaveMessagingServices(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
const
  CustomProtocolType = -1;
  CustomLabelType = 0;
var
  Service: TContactMessagingService;
  Builder: TOperationBuilderAdapter;
begin
  if AContact.State = TRecordState.Viewing then
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);

  if AContact.State = TRecordState.Modification then
  begin
    Builder := TOperationBuilderAdapter.CreateDelete(TJCommonDataKinds_Im.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
    try
      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;

  for Service in AContact.MessagingServices do
  begin
    if AContact.State = TRecordState.Adding then
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Im.JavaClass.CONTENT_ITEM_TYPE)
    else
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Im.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID {ContactID});
    try
      Builder.AddValue('data1', Service.UserName); // Data
      Builder.AddValue('data2', TAndroidAddressBookHelper.MessagingServiceLabelKindToInteger(Service.LabelKind)); // Type
      if (Service.LabelKind = TContactMessagingService.TLabelKind.Custom) or not Service.IsLabelKindSupported then
        Builder.AddValue('data3', Service.LabelText) // Label
      else
        Builder.AddValue('data3', string.Empty);

      Builder.AddValue('data5', TAndroidAddressBookHelper.MessagingServiceKindToInteger(Service.ServiceKind)); // Protocol type
      if (Service.ServiceKind = TContactMessagingService.TServiceKind.Custom) or not Service.IsServiceKindSupported then
        Builder.AddValue('data6', Service.ServiceName) // Protocol name
      else
        Builder.AddValue('data6', string.Empty);

      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;
end;

procedure TAndroidAddressBook.SavePhones(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
var
  Value: TContactPhone;
  Builder: TOperationBuilderAdapter;
begin
  if AContact.State = TRecordState.Viewing then
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);

  if AContact.State = TRecordState.Modification then
  begin
    Builder := TOperationBuilderAdapter.CreateDelete(TJCommonDataKinds_Phone.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
    try
      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;

  for Value in AContact.Phones do
  begin
    if AContact.State = TRecordState.Adding then
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Phone.JavaClass.CONTENT_ITEM_TYPE)
    else
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Phone.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);

    try
      Builder.AddValue('data1', Value.Number);
      Builder.AddValue('data2', TAndroidAddressBookHelper.PhoneKindToInteger(Value.LabelKind));
      if (Value.LabelKind = TContactPhone.TLabelKind.Custom) or not Value.IsLabelKindSupported then
        Builder.AddValue('data3', Value.LabelText)
      else
        Builder.AddValue('data3', string.Empty);

      AOperations.add(Builder.build);
    finally
      Builder.Free;
    end;
  end;
end;

procedure TAndroidAddressBook.SavePhoto(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
var
  Builder: TOperationBuilderAdapter;
begin
  if AContact.State = TRecordState.Viewing then
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);

  if (AContact.Photo = nil) and (AContact.State = TRecordState.Modification) then
  begin
    Builder := TOperationBuilderAdapter.CreateDelete(TJCommonDataKinds_Photo.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
    try
      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
    Exit;
  end;

  case AContact.State of
    TRecordState.Adding:
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Photo.JavaClass.CONTENT_ITEM_TYPE);
    TRecordState.Modification:
      if RecordExists(TJCommonDataKinds_Organization.JavaClass.CONTENT_ITEM_TYPE, AContact) then
        Builder := TOperationBuilderAdapter.CreateUpdate(TJCommonDataKinds_Photo.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID)
      else
        Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Photo.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID)
  end;

  try
    Builder.AddValue('is_super_primary', 1);
    Builder.AddValue('data15', AContact.Photo);

    AOperations.add(Builder.Build);
  finally
    Builder.Free;
  end;
end;

procedure TAndroidAddressBook.SaveRelatedNames(var AOperations: JArrayList; const AContact: TAndroidAddressBookContact);
var
  Value: TContactRelatedName;
  Builder: TOperationBuilderAdapter;
begin
  if AContact.State = TRecordState.Viewing then
    raise EAddressBookExecute.Create(SCannotSaveNotModifiedContact);

  if AContact.State = TRecordState.Modification then
  begin
    Builder := TOperationBuilderAdapter.CreateDelete(TJCommonDataKinds_Relation.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);
    try
      AOperations.add(Builder.Build);
    finally
      Builder.Free;
    end;
  end;

  for Value in AContact.RelatedNames do
  begin
    if AContact.State = TRecordState.Adding then
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Relation.JavaClass.CONTENT_ITEM_TYPE)
    else
      Builder := TOperationBuilderAdapter.CreateInsert(TJCommonDataKinds_Relation.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID);

    try
      Builder.AddValue('data1', Value.Name);
      Builder.AddValue('data2', TAndroidAddressBookHelper.RelatedNameKindToInteger(Value.LabelKind));
      if (Value.LabelKind = TContactRelatedName.TLabelKind.Custom) or not Value.IsLabelKindSupported then
        Builder.AddValue('data3', Value.LabelText)
      else
        Builder.AddValue('data3', string.Empty);

      AOperations.add(Builder.build);
    finally
      Builder.Free;
    end;
  end;
end;

procedure TAndroidAddressBook.DoPermissionRequest(const AMessage: string; const AAccessGranted: Boolean);
begin
  if Assigned(FOnPermissionRequest) then
    FOnPermissionRequest(Self, AMessage, AAccessGranted);
end;

function TAndroidAddressBook.DefaultSource: TAddressBookSource;
const
  DefaultGoogleAccountType = 'com.google';
var
  Sources: TAddressBookSources;
  I: Integer;
begin
  Sources := TAddressBookSources.Create;
  AllSources(Sources);
  for I := 0 to Sources.Count - 1 do
    if SameText(Sources[I].SourceType, DefaultGoogleAccountType) then
    begin
      Result := Sources[I];
      Break;
    end;
  if Result = nil then
    Result := TAndroidAddressBookSource.Create(string.Empty, string.Empty);
end;

destructor TAndroidAddressBook.Destroy;
begin
  TAndroidHelper.ContentResolver.unregisterContentObserver(FObserver);
  FListener.Free;
  inherited;
end;

procedure TAndroidAddressBook.BeginProcessing;
begin
  if FProcessing then
    raise EAddressBookException.Create(SCannotBeginNewProcessing);
  FProcessing := True;
end;

procedure TAndroidAddressBook.EndProcessing;
begin
  FProcessing := False;
end;

procedure TAndroidAddressBook.FetchContacts(var AContacts: TAddressBookContacts; const AFilter: string = '');
const
  SortOrder = 'display_name ASC';
  FilterDeleted = 'deleted = 0';
  ColumnRawContactID = 0;
  ColumnAccountType = 1;
  ColumnAccountName = 2;
  ColumnContactID = 3;
var
  Filter: string;
  Cursor: JCursor;
  Projection: TJavaObjectArray<JString>;
  Person: TAndroidAddressBookContact;
begin
  if AFilter.IsEmpty then
    Filter := FilterDeleted
  else
    Filter := Format('%s AND %s', [FilterDeleted, AFilter]);

  // Be carefull, if you change this array, you also should change columns indexes for getting correct values of field
  // by index. We don't search columns index in Cursor for increasing speed of fetching records
  // raw_contacts._id
  // raw_contacts.account_type
  // raw_contacts.account_name
  // raw_contacts.contact_id
  Projection := CreateJavaStringArray(['_id', 'account_type', 'account_name', 'contact_id']);
  try
    try
      Cursor := TAndroidHelper.ContentResolver.query(TJContactsContract_RawContacts.JavaClass.CONTENT_URI, Projection,
        StringToJString(Filter), nil, StringToJString(SortOrder));
      if Cursor <> nil then
        try
          while Cursor.moveToNext do
          begin
            Person := TAndroidAddressBookContact.CreateFromExisting(Self, Cursor.getInt(ColumnContactID)); // raw_contacts.contact_id
            Person.RawContactID := Cursor.getInt(ColumnRawContactID); // raw_contacts._id
            Person.AccountType := JStringToString(Cursor.getString(ColumnAccountType)); // raw_contacts.account_type
            Person.AccountName := JStringToString(Cursor.getString(ColumnAccountName)); // raw_contacts.account_name
            AContacts.Add(Person);
          end;
        finally
          Cursor.close;
        end;
    except on E: EJNIException do
      raise EAddressBookExecute.CreateFmt(SCannotFetchContacts, [E.Message]);
    end;
  finally
    Projection.Free;
  end;
end;

procedure TAndroidAddressBook.FetchGroups(const AGroups: TAddressBookGroups; const AFilter: string);
const
  FilterDeleted = 'deleted = 0'; // and group_visible = 0
  ColumnGroupID = 0;
  ColumnAccountType = 1;
  ColumnAccountName = 2;
var
  Filter: string;
  Projection: TJavaObjectArray<JString>;
  GroupCursor: JCursor;
  Group: TAndroidAddressBookGroup;
begin
  if AFilter.IsEmpty then
    Filter := FilterDeleted
  else
    Filter := Format('%s AND %s', [FilterDeleted, AFilter]);

  Projection := CreateJavaStringArray(['_id', 'account_type', 'account_name']);
  try
    try
      GroupCursor := TAndroidHelper.ContentResolver.query(TJContactsContract_Groups.JavaClass.CONTENT_URI, Projection,
        StringToJString(Filter), nil, nil);
      if GroupCursor <> nil then
        try
          while GroupCursor.moveToNext do
          begin
            Group := TAndroidAddressBookGroup.CreateFromExisting(Self, GroupCursor.getLong(ColumnGroupID));
            Group.AccountType := JStringToString(GroupCursor.getString(ColumnAccountType));
            Group.AccountName := JStringToString(GroupCursor.getString(ColumnAccountName));
            AGroups.Add(Group);
          end;
        finally
          GroupCursor.close;
        end
    except on E: EJNIException do
      raise EAddressBookExecute.CreateFmt(SCannotFetchGroups, [E.Message]);
    end;
  finally
    Projection.Free;
  end;
end;

procedure TAndroidAddressBook.DoExternalChange;
begin
  if Assigned(FOnExternalChange) then
    FOnExternalChange(Self);
end;

procedure TAndroidAddressBook.OnRequestPermissionsResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
var
  &Message: string;
  AccessGranted: Boolean;
  ReadAuthorizationGranted, WriteAuthorizationGranted: Boolean;
begin
  if Length(APermissions) = 3 then
  begin
    ReadAuthorizationGranted := AGrantResults[0] = TPermissionStatus.Granted;
    WriteAuthorizationGranted := AGrantResults[1] = TPermissionStatus.Granted;
    if ReadAuthorizationGranted and WriteAuthorizationGranted then
      AccessGranted := True
    else if ReadAuthorizationGranted and not WriteAuthorizationGranted then
    begin
      &Message := SPermissionCannotChangeDataInAddressBook;
      AccessGranted := True;
    end
    else if not ReadAuthorizationGranted and WriteAuthorizationGranted then
    begin
      &Message := SPermissionCannotGetDataFromAddressBook;
      AccessGranted := True;
    end
    else
    begin
      &Message := Format(SRequiredPermissionsAreAbsent, ['WRITE_CONTACTS, READ_CONTACTS']);
      AccessGranted := False;
    end;
    if AccessGranted then
      InitializeAddressBook;
    DoPermissionRequest(&Message, AccessGranted);
  end;
end;

procedure TAndroidAddressBook.RequestPermission(ADisplayRationaleEvent: TDisplayRationaleEvent);
begin
  PermissionsService.RequestPermissions(
    [JStringToString(TJManifest_permission.JavaClass.READ_CONTACTS),
     JStringToString(TJManifest_permission.JavaClass.WRITE_CONTACTS),
     JStringToString(TJManifest_permission.JavaClass.GET_ACCOUNTS)],
    OnRequestPermissionsResult, ADisplayRationaleEvent)
end;

procedure TAndroidAddressBook.RevertCurrentChangesAndUpdate;
begin
  // Android saves changes instantly and there are no transaction. So we emulates reverting of user's changes.
  // If changes was made before this timestamp, we will revert it in a future
  FRevertTimeStamp := Now;
end;

function TAndroidAddressBook.AuthorizationStatus(const AAccessType: TAddressBookAccessType): TAuthorizationStatus;
var
  PermissionGranted: Boolean;
begin
  PermissionGranted := False;
  Result := TAuthorizationStatus.Denied;
  case AAccessType of
    TAddressBookAccessType.Read:
      PermissionGranted := PermissionsService.IsPermissionGranted(
        JStringToString(TJManifest_permission.JavaClass.READ_CONTACTS));
    TAddressBookAccessType.Write:
      PermissionGranted := PermissionsService.IsPermissionGranted(
        JStringToString(TJManifest_permission.JavaClass.WRITE_CONTACTS));
  end;
  if PermissionGranted then
    Result := TAuthorizationStatus.Authorized
end;

procedure TAndroidAddressBook.AllContacts(const ASource: TAddressBookSource; var AContacts: TAddressBookContacts);
begin
  if (ASource <> nil) and not (ASource is TAndroidAddressBookSource) then
    raise EAddressBookWrongArgs.CreateFmt(SCannotFetchAllContactsWrongClassArg, ['ASource', 'TAndroidAddressBookSource'])
  else if AContacts = nil then
    raise EAddressBookWrongArgs.CreateFmt(SCannotFetchAllContactNilArg, ['AContacts']);

  if ASource = nil then
    FetchContacts(AContacts)
  else
    FetchContacts(AContacts, Format('account_name = "%s" AND account_type = "%s"', [ASource.SourceName, ASource.SourceType]));
end;

function TAndroidAddressBook.ContactByID(const AID: Int32): TAddressBookContact;
var
  Contacts: TAddressBookContacts;
begin
  Contacts := TAddressBookContacts.Create;
  FetchContacts(Contacts, Format('_id = %d', [AID]));
  if Contacts.Count > 0 then
    try
      Result := Contacts[0];
      // Contacts is a TObjectList (owned), so when we free list, it will also free contact object.
      // Prevent it by extracting object from list
      Contacts.Extract(Result);
    finally
      Contacts.Free;
    end
  else
    Result := nil;
end;

function TAndroidAddressBook.CreateContact(const ASource: TAddressBookSource): TAddressBookContact;
begin
  Result := TAndroidAddressBookContact.CreateNew(Self, ASource);
end;

procedure TAndroidAddressBook.SaveContact(const AContact: TAddressBookContact);
var
  Operations: JArrayList;
  AndroidPerson: TAndroidAddressBookContact;
  Results: TJavaObjectArray<JContentProviderResult>;

  procedure UpdateContact(const AContact: TAndroidAddressBookContact);
  begin
    // Structured name
    if AContact.HasAtLeastOneChange(StructuredNameFields) then
      SaveStructureName(Operations, AContact);

    // Organization
    if AContact.HasAtLeastOneChange(CompanyFields) then
      SaveOrganization(Operations, AContact);

    // Nick Name
    if AContact.HasChanges(TContactField.NickName) then
      SaveNickName(Operations, AContact);

    // Photo
    if AContact.HasChanges(TContactField.Photo) then
      SavePhoto(Operations, AContact);

    // Phones
    if AContact.HasChanges(TContactField.Phones) then
      SavePhones(Operations, AContact);

    // URLS
    if AContact.HasChanges(TContactField.URLs) then
      SaveURLs(Operations, AContact);

    // EMails
    if AContact.HasChanges(TContactField.EMails) then
      SaveEmails(Operations, AContact);

    // Related Names
    if AContact.HasChanges(TContactField.RelatedNames) then
      SaveRelatedNames(Operations, AContact);

    // Address
    if AContact.HasChanges(TContactField.Addresses) then
      SaveAddresses(Operations, AContact);

    // Dates
    if AContact.HasAtLeastOneChange([TContactField.Dates, TContactField.Birthday]) then
      SaveDates(Operations, AContact);

    // Messaging Services
    if AContact.HasChanges(TContactField.MessagingServices) then
      SaveMessagingServices(Operations, AContact);
  end;

  procedure AddNewContact(const AContact: TAndroidAddressBookContact);
  var
    OperationBuilder: JContentProviderOperation_Builder;
  begin
    OperationBuilder := TJContentProviderOperation.JavaClass.newInsert(TJContactsContract_RawContacts.JavaClass.CONTENT_URI);
    OperationBuilder.withValue(StringToJString('account_type'), StringToJString(AContact.AccountType));
    OperationBuilder.withValue(StringToJString('account_name'), StringToJString(AContact.AccountName));
    Operations.add(OperationBuilder.build);

    // Structured name
    if AContact.HasAtLeastOneChange(StructuredNameFields) then
      SaveStructureName(Operations, AContact);

    // Organization
    if AContact.HasAtLeastOneChange(CompanyFields) then
      SaveOrganization(Operations, AContact);

    // Nick Name
    if AContact.HasChanges(TContactField.NickName) then
      SaveNickName(Operations, AContact);

    // Photo
    if AContact.HasChanges(TContactField.Photo) then
      SavePhoto(Operations, AContact);

    // Phones
    if AContact.HasChanges(TContactField.Phones) then
      SavePhones(Operations, AContact);

    // URLS
    if AContact.HasChanges(TContactField.URLs) then
      SaveURLs(Operations, AContact);

    // EMails
    if AContact.HasChanges(TContactField.EMails) then
      SaveEmails(Operations, AContact);

    // Related Names
    if AContact.HasChanges(TContactField.RelatedNames) then
      SaveRelatedNames(Operations, AContact);

    // Address
    if AContact.HasChanges(TContactField.Addresses) then
      SaveAddresses(Operations, AContact);

    // Dates
    if AContact.HasAtLeastOneChange([TContactField.Dates, TContactField.Birthday]) then
      SaveDates(Operations, AContact);

    // Messaging Services
    if AContact.HasChanges(TContactField.MessagingServices) then
      SaveMessagingServices(Operations, AContact);
  end;

begin
  if not (AContact is TAndroidAddressBookContact) then
    raise EAddressBookWrongArgs.CreateFmt(SCannotSaveContactWrongClassArg, ['APerson', 'TAndroidAddressBookContact']);

  AndroidPerson := TAndroidAddressBookContact(AContact);
  try
    // We save only new or modified contact
    if AndroidPerson.State <> TRecordState.Viewing then
      try
        Operations := TJArrayList.Create;

        if AndroidPerson.State = TRecordState.Adding then
          AddNewContact(AndroidPerson)
        else
          UpdateContact(AndroidPerson);

        BeginProcessing;
        try
          Results := TAndroidHelper.ContentResolver.applyBatch(TJContactsContract.JavaClass.AUTHORITY, Operations);
        finally
          EndProcessing;
        end;

        if AndroidPerson.State = TRecordState.Adding then
          if Results.Length > 0 then
          begin
            AndroidPerson.RawContactID := TJContentUris.JavaClass.parseId(Results[0].uri);
            AndroidPerson.FState := TRecordState.Viewing;
          end
          else
            raise EAddressBookExecute.Create(SCannotExtractContactID);
      finally
        AndroidPerson.Changes.Clear;
      end;
  except on E: EJNIException do
    raise EAddressBookExecute.CreateFmt(SCannotSaveContact, [E.Message]);
  end;
end;

procedure TAndroidAddressBook.RemoveContact(const AContact: TAddressBookContact);
begin
  if AContact = nil then
    raise EAddressBookWrongArgs.CreateFmt(SCannotRemoveContactNilArg, ['AContact']);

  RemoveContact(AContact.ID);
end;

procedure TAndroidAddressBook.AllGroups(const ASource: TAddressBookSource; var AGroups: TAddressBookGroups);
begin
  if (ASource <> nil) and not (ASource is TAndroidAddressBookSource) then
    raise EAddressBookWrongArgs.CreateFmt(SCannotFetchAllGroupsWrongClassArg, ['ASource', 'TAndroidAddressBookSource'])
  else if AGroups = nil then
    raise EAddressBookWrongArgs.CreateFmt(SCannotFetchAllGroupsNilArg, ['AGroups']);

  if ASource = nil then
    FetchGroups(AGroups)
  else
    FetchGroups(AGroups, Format('account_name = "%s" AND account_type = "%s"', [ASource.SourceName, ASource.SourceType]));
end;

procedure TAndroidAddressBook.AllSources(var ASources: TAddressBookSources);
var
  AccountManager: JAccountManager;
  Accounts: TJavaObjectArray<JAccount>;
  I: Integer;
  AccountName: string;
  AccountType: string;
begin
  if ASources = nil then
    raise EAddressBookWrongArgs.CreateFmt(SCannotFetchAllSourcesNilArg, ['ASources'])
  else
  if not PermissionsService.IsPermissionGranted(JStringToString(TJManifest_permission.JavaClass.GET_ACCOUNTS)) then
    raise EAddressBookAccess.Create(SPermissionCannotGetAccounts);

  AccountManager := TJAccountManager.JavaClass.get(TAndroidHelper.Context);
  Accounts := AccountManager.getAccounts;
  for I := 0 to Accounts.Length - 1 do
  begin
    AccountName := JStringToString(Accounts[I].name);
    AccountType := JStringToString(Accounts[I].&type);

    ASources.Add(TAndroidAddressBookSource.Create(AccountName, AccountType));
  end;
end;

function TAndroidAddressBook.CreateGroup(const ASource: TAddressBookSource): TAddressBookGroup;
begin
  Result := TAndroidAddressBookGroup.CreateNew(Self, ASource);
end;

function TAndroidAddressBook.GetOnExternalChange: TExternalChangeEvent;
begin
  Result := FOnExternalChange;
end;

function TAndroidAddressBook.GetOnPermissionRequest: TPermissionRequestEvent;
begin
  Result := FOnPermissionRequest;
end;

function TAndroidAddressBook.GroupByID(const AID: Int32): TAddressBookGroup;
var
  Groups: TAddressBookGroups;
begin
  Groups := TAddressBookGroups.Create;
  FetchGroups(Groups, Format('_id = %d', [AID]));
  if Groups.Count > 0 then
    try
      Result := Groups[0];
      // Groups is a TObjectList (owned), so when we free list, it will also free group object.
      // Prevent it by extracting object from list
      Groups.Extract(Result);
    finally
      Groups.Free;
    end
  else
    Result := nil;
end;

procedure TAndroidAddressBook.InitializeAddressBook;
begin
  FListener := TAddressBookChangesListener.Create(Self);
  FObserver := TJAddressBookObserver.JavaClass.init(FListener);

  FProcessing := False;
  FRevertTimeStamp := Now;

  TAndroidHelper.ContentResolver.registerContentObserver(TJContactsContract_RawContacts.JavaClass.CONTENT_URI, True, FObserver);
  TAndroidHelper.ContentResolver.registerContentObserver(TJContactsContract_Data.JavaClass.CONTENT_URI, True, FObserver);
end;

procedure TAndroidAddressBook.SaveGroup(const AGroup: TAddressBookGroup);

  procedure AddNewGroup(const AGroup: TAndroidAddressBookGroup);
  var
    Values: JContentValues;
    RecordURI: Jnet_Uri;
  begin
    Values := TJContentValues.Create;
    Values.put(StringToJString('title'), StringToJString(AGroup.Name));
    Values.put(StringToJString('favorites'), Int64ToJLong(1));
    Values.put(StringToJString('group_visible'), Int64ToJLong(1));
    Values.put(StringToJString('account_type'), StringToJString(AGroup.AccountType));
    Values.put(StringToJString('account_name'), StringToJString(AGroup.AccountName));

    RecordURI := TAndroidHelper.ContentResolver.insert(TJContactsContract_Groups.JavaClass.CONTENT_URI, Values);
    AGroup.ID := TJContentUris.JavaClass.parseId(RecordURI);
    AGroup.FState := TRecordState.Viewing;
  end;

  procedure UpdateGroup(const AGroup: TAndroidAddressBookGroup);
  var
    Values: JContentValues;
    Where: JString;
  begin
    Values := TJContentValues.Create;
    Values.put(StringToJString('title'), StringToJString(AGroup.Name));
    Where := StringToJString(Format('_ID = %d', [AGroup.ID]));
    TAndroidHelper.ContentResolver.update(TJContactsContract_Groups.JavaClass.CONTENT_URI, Values, Where, nil);
    AGroup.FState := TRecordState.Viewing;
  end;

var
  AndroidGroup: TAndroidAddressBookGroup;
begin
  if not (AGroup is TAndroidAddressBookGroup) then
    raise EAddressBookWrongArgs.CreateFmt(SCannotSaveGroupWrongClassArg, ['AGroup', 'TAndroidAddressBookGroup']);

  AndroidGroup := TAndroidAddressBookGroup(AGroup);
  BeginProcessing;
  try
    if AndroidGroup.State = TRecordState.Adding then
      AddNewGroup(AndroidGroup)
    else if AndroidGroup.State = TRecordState.Modification then
      UpdateGroup(AndroidGroup);
  finally
    EndProcessing;
  end;
end;

procedure TAndroidAddressBook.RemoveGroup(const AGroup: TAddressBookGroup);
begin
  if AGroup = nil then
    raise EAddressBookWrongArgs.CreateFmt(SCannotRemoveGroupNilArg, ['AGroup']);

  RemoveGroup(AGroup.ID);
end;

procedure TAndroidAddressBook.AllContactsInGroups(const AGroups: TAddressBookGroups; var AContacts: TAddressBookContacts);
const
  Filter = 'mimetype = ? AND data1 = ?'; //data1 = GROUP_ROW_ID
  SortOrder = 'display_name ASC';
  ColumnContactID = 0;
  ColumnRawContactID = 1;
  ColumnAccountType = 2;

var
  PersonsRawID: TList<Integer>;

  procedure ReadContactsAndCloseCursor(const ACursor: JCursor; var AContacts: TAddressBookContacts);
  var
    Person: TAndroidAddressBookContact;
    ContactID: Integer;
    RawContactID: Integer;
    AccountType: string;
  begin
    if ACursor <> nil then
      try
        while ACursor.moveToNext do
        begin
          RawContactID := ACursor.getInt(ColumnRawContactID);
          // Android allows to add one contact in specified group several times. So, when we filter data table by group,
          // we can receive several instance of the same contact. For avoiding it we collect ID of all contacts and match
          // new id in this list.
          if not PersonsRawID.Contains(RawContactID) then
          begin
            ContactID := ACursor.getInt(ColumnContactID);
            Person := TAndroidAddressBookContact.CreateFromExisting(Self, ContactID);
            Person.RawContactID := RawContactID;
            // A concatenation of the account type and data set (delimited by a forward slash)
            AccountType := JStringToString(ACursor.getString(ColumnAccountType)); // account_type_and_data_set
            if not AccountType.IsEmpty then
              Person.AccountType := AccountType.Split(['/'])[0];
            AContacts.Add(Person);

            PersonsRawID.Add(Person.RawContactID);
          end;
        end;
      finally
        ACursor.close;
      end;
  end;

var
  GroupCursor: JCursor;
  Projection: TJavaObjectArray<JString>;
  QueryParams: TJavaObjectArray<JString>;
  I: Integer;
  GroupID: Int64;
begin
  PersonsRawID := TList<Integer>.Create;
  try
    // Be carefull, if you change this array, you also should change columns indexes for getting correct values of field
    // by index. We don't search columns index in Cursor for increasing speed of fetching records
    Projection := CreateJavaStringArray(['contact_id', 'raw_contact_id', 'account_type_and_data_set']);
    try
      QueryParams := TJavaObjectArray<JString>.Create(2);
      QueryParams[0] := TJCommonDataKinds_GroupMembership.JavaClass.CONTENT_ITEM_TYPE;
      try
        for I := 0 to AGroups.Count - 1 do
          if AGroups[I] is TAndroidAddressBookGroup then
          begin
            GroupID := TAndroidAddressBookGroup(AGroups[I]).ID;
            QueryParams[1] := StringToJString(GroupID.ToString);
            try
              GroupCursor := TAndroidHelper.ContentResolver.query(TJContactsContract_Data.JavaClass.CONTENT_URI, Projection,
                StringToJString(Filter), QueryParams, StringToJString(SortOrder));

              ReadContactsAndCloseCursor(GroupCursor, AContacts);
            except on E: EJNIException do
              raise EAddressBookExecute.CreateFmt(SCannotFetchContactInGroup, [GroupID, E.Message]);
            end;
          end;
      finally
        QueryParams.Free;
      end;
    finally
      Projection.Free;
    end;
  finally
    PersonsRawID.Free;
  end;
end;

procedure TAndroidAddressBook.AddContactIntoGroup(const AGroup: TAddressBookGroup; const AContact: TAddressBookContact);

  function IsContactInGroup(const AContact: TAndroidAddressBookContact; const AGroup: TAndroidAddressBookGroup): Boolean;
  const
    Filter = 'mimetype = ? AND raw_contact_id = ? AND data1 = ?'; // GROUP_ROW_ID = data1
  var
    DataCursor: JCursor;
    Projection: TJavaObjectArray<JString>;
    QueryParams: TJavaObjectArray<JString>;
  begin
    Result := False;
    Projection := TJavaObjectArray<JString>.Create(0);
    QueryParams := CreateJavaStringArray([TJCommonDataKinds_GroupMembership.JavaClass.CONTENT_ITEM_TYPE, AContact.RawContactID,
      AGroup.ID]);
    try
      try
        DataCursor := TAndroidHelper.ContentResolver.query(TJContactsContract_Data.JavaClass.CONTENT_URI, Projection,
          StringToJString(Filter), QueryParams, nil);
        if DataCursor <> nil then
          try
            Result := DataCursor.getCount > 0;
          finally
            DataCursor.close;
          end;
      except on E: EJNIException do
        raise EAddressBookExecute.CreateFmt(SCannotCheckExistingDataRecord, [E.Message]);
      end;
    finally
      QueryParams.Free;
      Projection.Free;
    end;
  end;

var
  Values: JContentValues;
  Contact: TAndroidAddressBookContact;
  Group: TAndroidAddressBookGroup;
begin
  if not (AContact is TAndroidAddressBookContact) then
    raise EAddressBookWrongArgs.CreateFmt(SCannotAddContactIntoGroupWrongClassArg, ['AContact', 'TAndroidPerson']);

  if not (AGroup is TAndroidAddressBookGroup) then
    raise EAddressBookWrongArgs.CreateFmt(SCannotAddContactIntoGroupWrongClassArg, ['AGroup', 'TAndroidAddressBookGroup']);

  Contact := TAndroidAddressBookContact(AContact);
  Group := TAndroidAddressBookGroup(AGroup);

  if Contact.State = TRecordState.Adding then
    raise EAddressBookWrongArgs.Create(SCannotAddContactIntoGroupContactIsNotInAddressBook);

  if Group.State = TRecordState.Adding then
    raise EAddressBookWrongArgs.Create(SCannotAddContactIntoGroupGroupIsNotInAddressBook);

  if not IsContactInGroup(Contact, Group) then
  begin
    Values := TJContentValues.Create;
    Values.put(StringToJString('mimetype'), TJCommonDataKinds_GroupMembership.JavaClass.CONTENT_ITEM_TYPE);
    Values.put(StringToJString('raw_contact_id'), Int64ToJLong(Contact.RawContactID));
    Values.put(StringToJString('data1'), Int64ToJLong(Group.ID)); // GROUP_ROW_ID = data1
    BeginProcessing;
    try
      TAndroidHelper.ContentResolver.insert(TJContactsContract_Data.JavaClass.CONTENT_URI, Values);
    finally
      EndProcessing;
    end;
  end;
end;

procedure TAndroidAddressBook.RemoveContact(const AID: Integer);
const
  Where = '_id = ?';
begin
  BeginProcessing;
  try
    try
      TAndroidHelper.ContentResolver.delete(TJContactsContract_RawContacts.JavaClass.CONTENT_URI, StringToJString(Where),
        CreateJavaStringArray([AID]));
    except on E: EJNIException do
      raise EAddressBookExecute.CreateFmt(SCannotRemoveContact, [E.Message]);
    end;
  finally
    EndProcessing;
  end;
end;

procedure TAndroidAddressBook.RemoveContactFromGroup(const AGroup: TAddressBookGroup; const AContact: TAddressBookContact);
const
  Where = 'raw_contact_id = ? AND mimetype = ? AND data1 = ?'; // data1 - GROUP_ROW_ID
var
  Args: TJavaObjectArray<JString>;
begin
  if not (AContact is TAndroidAddressBookContact) then
    raise EAddressBookWrongArgs.CreateFmt(SCannotRemoveContactFromGroupWrongClassArg, ['AContact', 'TAndroidPerson']);

  if not (AGroup is TAndroidAddressBookGroup) then
    raise EAddressBookWrongArgs.CreateFmt(SCannotRemoveContactFromGroupWrongClassArg, ['AGroup', 'TAndroidAddressBookGroup']);

  Args := CreateJavaStringArray([TAndroidAddressBookContact(AContact).RawContactID,
    TJCommonDataKinds_GroupMembership.JavaClass.CONTENT_ITEM_TYPE, TAndroidAddressBookGroup(AGroup).ID]);
  BeginProcessing;
  try
    try
      TAndroidHelper.ContentResolver.delete(TJContactsContract_Data.JavaClass.CONTENT_URI, StringToJString(Where), Args);
    except on E: EJNIException do
      raise EAddressBookExecute.CreateFmt(SCannotRemoveContactFromGroup, [E.Message]);
    end;
  finally
    EndProcessing;
    Args.Free;
  end;
end;

procedure TAndroidAddressBook.RemoveGroup(const AID: Integer);
const
  Where = '_ID = ?';
begin
  BeginProcessing;
  try
    try
      TAndroidHelper.ContentResolver.delete(TJContactsContract_Groups.JavaClass.CONTENT_URI, StringToJString(Where),
        CreateJavaStringArray([AID]));
    except on E: EJNIException do
      raise EAddressBookExecute.CreateFmt(SCannotRemoveGroup, [E.Message]);
    end;
  finally
    EndProcessing;
  end;
end;

{ TAndroidAddressBookContact }

procedure TAndroidAddressBookContact.AllGroups(var AGroups: TAddressBookGroups);
const
  Filter = 'mimetype = ? and raw_contact_id = ?';
  ColumnGroupID = 0;
var
  Projection: TJavaObjectArray<JString>;
  ContactsCursor: JCursor;
  QueryParams: TJavaObjectArray<JString>;
  GroupID: Integer;
  GroupsIDs: TList<Int64>;
begin
  if AGroups = nil then
    raise EAddressBookWrongArgs.CreateFmt(SCannotFetchAllGroupsNilArg, ['AGroups']);

  GroupsIDs := TList<Int64>.Create;
  try
    Projection := CreateJavaStringArray([TJCommonDataKinds_GroupMembership.JavaClass.GROUP_ROW_ID]);
    try
      QueryParams := CreateJavaStringArray([TJCommonDataKinds_GroupMembership.JavaClass.CONTENT_ITEM_TYPE, RawContactID]);
      try
        try
          ContactsCursor := TAndroidHelper.ContentResolver.query(TJContactsContract_Data.JavaClass.CONTENT_URI,
            Projection, StringToJString(Filter), QueryParams, nil);
          if ContactsCursor <> nil then
            try
              while ContactsCursor.moveToNext do
              begin
                GroupID := ContactsCursor.getInt(ColumnGroupID);
                if not GroupsIDs.Contains(GroupID) then
                begin
                  AGroups.add(TAndroidAddressBookGroup.CreateFromExisting(FAddressBook, GroupID));
                  GroupsIDs.Add(GroupID);
                end;
              end;
            finally
              ContactsCursor.close;
            end;
        except on E: EJNIException do
          raise EAddressBookExecute.CreateFmt(SCannotFetchAllGroupsFromContact, [E.Message]);
        end;
      finally
        QueryParams.Free;
      end;
    finally
      Projection.Free;
    end;
  finally
    GroupsIDs.Free;
  end;
end;

constructor TAndroidAddressBookContact.CreateFromExisting(const AAddressBook: TAndroidAddressBook; const AContactID: Integer);
begin
  if AAddressBook = nil then
    raise EAddressBookWrongArgs.CreateFmt(SCannotCreateContactNilArg, ['AAddressBook']);

  FAddressBook := TAndroidAddressBook(AAddressBook);
  FListChanges := TContactChanges.Create;
  ContactID := AContactID;
  FAccountType := string.Empty;
  FAccountName := string.Empty;
  FRawContactID := UndefinedID;
end;

constructor TAndroidAddressBookContact.CreateNew(const AAddressBook: TAndroidAddressBook; const ASource: TAddressBookSource);
begin
  CreateFromExisting(AAddressBook, UndefinedID);

  if ASource <> nil then
  begin
    FAccountType := ASource.SourceType;
    FAccountName := ASource.SourceName;
  end;
end;

destructor TAndroidAddressBookContact.Destroy;
begin
  FListChanges.Free;
  inherited;
end;

function TAndroidAddressBookContact.GetAccountName: string;
const
  ColumnAccountName = 0;
var
  Projection: TJavaObjectArray<JString>;
  ContactsCursor: JCursor;
  Filter: JString;
begin
  if FAccountName.IsEmpty and (State <> TRecordState.Adding) then
  begin
    Filter := StringToJString(Format('_id = %d', [RawContactID]));
    Projection := CreateJavaStringArray(['account_name']);
    try
      try
        ContactsCursor := TAndroidHelper.ContentResolver.query(TJContactsContract_RawContacts.JavaClass.CONTENT_URI,
          Projection, Filter, nil, nil);
        if ContactsCursor <> nil then
          try
            if ContactsCursor.moveToNext then
              FAccountName := JStringToString(ContactsCursor.getString(ColumnAccountName));
          finally
            ContactsCursor.close;
          end;
      except on E: EJNIException do
        raise EAddressBookExecute.CreateFmt('Cannot extract account_name of contact. %s', [E.Message]);
      end;
    finally
      Projection.Free;
    end;
  end;
  Result := FAccountName;
end;

function TAndroidAddressBookContact.GetAddresses(const AIndex: TContactField): TContactAddresses;
const
  Filter = 'mimetype = ? AND raw_contact_id = ?';
  ColumnLabel = 0;
  ColumnStreet = 1;
  ColumnCity = 2;
  ColumnRegion = 3;
  ColumnPostCode = 4;
  ColumnCountry = 5;
  ColumnType = 6;
var
  Query: TQueryAdapter;
  AddressesList: TContactAddresses;
begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TContactAddresses>
  else if AIndex = TContactField.Addresses then
  begin
    AddressesList := TContactAddresses.Create;
    Result := AddressesList;

    Query := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
    try
      // Be carefull, if you change this array, you also should change columns indexes for getting correct values of field
      // by index. We don't search columns index in Cursor for increasing speed of fetching records
      // 0 - data3  - Label
      // 1 - data4  - Street
      // 2 - data7  - City
      // 3 - data8  - Region
      // 4 - data9  - PostCode
      // 5 - data10 - Country
      // 6 - data2  - Type
      Query.SetFields(['data3', 'data4', 'data7', 'data8', 'data9', 'data10', 'data2']);
      Query.SetFilter(Filter, [TJCommonDataKinds_StructuredPostal.JavaClass.CONTENT_ITEM_TYPE, RawContactID]);
      Query.Execute(
        procedure (ACursor: JCursor)
        var
          Address: TContactAddress;
        begin
          Address := TContactAddress.Create;
          Address.Street := JStringToString(ACursor.getString(ColumnStreet));
          Address.City := JStringToString(ACursor.getString(ColumnCity));
          Address.State := JStringToString(ACursor.getString(ColumnRegion));
          Address.ZIP := JStringToString(ACursor.getString(ColumnPostCode));
          Address.Country := JStringToString(ACursor.getString(ColumnCountry));
          Address.LabelKind := TAndroidAddressBookHelper.IntegerToAddressLabelKind(ACursor.getInt(ColumnType));
          if Address.LabelKind = TContactAddress.TLabelKind.Custom then
            Address.LabelText := JStringToString(ACursor.getString(ColumnLabel));

          AddressesList.Add(Address);
        end);
    finally
      Query.Free;
    end;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.GetBitmapValue(const AIndex: TContactField): TBitmapSurface;
type
  TPhotoType = (Original, Thumbnail);

  function GetBitmap(const APhotoType: TPhotoType): TBitmapSurface;
  var
    ContactURI: Jnet_Uri;
    Stream: JInputStream;
    Bitmap: JBitmap;
    BitmapSurface: TBitmapSurface;
  begin
    if State = TRecordState.Adding then
      Result := nil
    else
    begin
      ContactURI := TJContentUris.JavaClass.withAppendedId(TJContactsContract_Contacts.JavaClass.CONTENT_URI, ContactID);
      Stream := TJContactsContract_Contacts.JavaClass.openContactPhotoInputStream(TAndroidHelper.Context.getContentResolver,
        ContactURI, APhotoType = TPhotoType.Original);
      Bitmap := TJBitmapFactory.JavaClass.decodeStream(Stream);
      if Bitmap <> nil then
      begin
        BitmapSurface := TBitmapSurface.Create;
        if JBitmapToSurface(Bitmap, BitmapSurface) then
          Result := BitmapSurface
        else
          Result := nil;
      end
      else
        Result := nil;
    end;
  end;

begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TBitmapSurface>
  else
    case AIndex of
      TContactField.Photo:
        Result := GetBitmap(TPhotoType.Original);
      TContactField.PhotoThumbnail:
        Result := GetBitmap(TPhotoType.Thumbnail);
    else
      raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
    end;
end;

function TAndroidAddressBookContact.GetContactID: Integer;
const
  ColumnContactID = 0;
var
  Projection: TJavaObjectArray<JString>;
  ContactsCursor: JCursor;
  Filter: JString;
begin
  if (FContactID = UndefinedID) and (State <> TRecordState.Adding) then
  begin
    Filter := StringToJString(Format('_id = %d', [RawContactID]));
    Projection := CreateJavaStringArray(['contact_id']);
    try
      try
        ContactsCursor := TAndroidHelper.ContentResolver.query(TJContactsContract_RawContacts.JavaClass.CONTENT_URI,
          Projection, Filter, nil, nil);
        if ContactsCursor <> nil then
          try
            if ContactsCursor.moveToNext then
              FContactID := ContactsCursor.getInt(ColumnContactID);
          finally
            ContactsCursor.close;
          end;
      except on E: EJNIException do
        raise EAddressBookExecute.CreateFmt('Cannot get contact_id of contact. %s', [E.Message]);
      end;
    finally
      Projection.Free;
    end;
  end;
  Result := FContactID;
end;

function TAndroidAddressBookContact.GetContactKind: TContactKind;
begin
  if TOSVersion.Check(5, 0) then
  begin
    if TJContactsContract_Contacts.JavaClass.isEnterpriseContactId(ContactID) then
      Result := TContactKind.Organization
    else
      Result := TContactKind.Person;
  end
  else
    Result := TContactKind.Person;
end;

function TAndroidAddressBookContact.GetDateTimeValue(const AIndex: TContactField): TDateTime;

  function GetBirthday: TDateTime;
  const
    Filter = 'mimetype = ? AND raw_contact_id = ? AND data2 = ?';  // data2 - Type
    ColumnStartDate = 0;
  var
    BirthdayDate: TDateTime;
    Query: TQueryAdapter;
  begin
    BirthdayDate := NaN;
    Query := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
    try
      Query.SetFields([TJCommonDataKinds_Event.JavaClass.START_DATE]);
      Query.SetFilter(Filter, [TJCommonDataKinds_Event.JavaClass.CONTENT_ITEM_TYPE, RawContactID,
         TJCommonDataKinds_Event.JavaClass.TYPE_BIRTHDAY]);
      Query.Execute(procedure (ACursor: JCursor)
        begin
          TryJavaDateStringToDateTime(ACursor.getString(ColumnStartDate), BirthdayDate);
        end, True);
    finally
      Query.Free
    end;
    Result := BirthdayDate;
  end;

begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TDateTime>
  else if AIndex = TContactField.Birthday then
    // Android stores Birthday days in Dates array instead of separated field.
    Result := GetBirthday
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.GetDisplayName: string;
const
  ColumnDisplayName = 0;
var
  Query: TQueryAdapter;
  LDisplayName: string;
begin
  LDisplayName := string.Empty;
  Query := TQueryAdapter.Create(TJContactsContract_RawContacts.JavaClass.CONTENT_URI);
  try
    Query.SetFilter('contact_id = ?', [ContactID]);
    Query.SetFields(['display_name']);
    Query.Execute(procedure (ACursor: JCursor)
      begin
        LDisplayName := JStringToString(ACursor.getString(ColumnDisplayName));
      end, True);
  finally
    Query.Free;
  end;
  Result := LDisplayName;
end;

function TAndroidAddressBookContact.GetEmails(const AIndex: TContactField): TContactEmails;
const
  Filter = 'mimetype = ? AND raw_contact_id = ?';
  ColumnFieldLabel = 0;
  ColumnFieldValue = 1;
  ColumnFieldType = 2;
var
  Query: TQueryAdapter;
  EmailsList: TContactEmails;
begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TContactEmails>
  else if AIndex = TContactField.EMails then
  begin
    EmailsList := TContactEmails.Create;
    Result := EmailsList;
    Query := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
    try
      Query.SetFields(['data3', TJCommonDataKinds_Email.JavaClass.ADDRESS, 'data2']);
      Query.SetFilter(Filter, [TJCommonDataKinds_Email.JavaClass.CONTENT_ITEM_TYPE, RawContactID]);
      Query.Execute(
        procedure (ACursor: JCursor)
        var
          Email: string;
          Kind: TContactEmail.TLabelKind;
          EmailLabel: string;
        begin
          Email:= JStringToString(ACursor.getString(ColumnFieldValue));
          Kind := TAndroidAddressBookHelper.IntegerToEmailKind(ACursor.getInt(ColumnFieldType));
          if Kind = TContactEmail.TLabelKind.Custom then
          begin
            EmailLabel := JStringToString(ACursor.getString(ColumnFieldLabel));
            EmailsList.AddEmail(EmailLabel, Email);
          end
          else
            EmailsList.AddEmail(Kind, Email);
         end);
    finally
      Query.Free;
    end;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.GetID: Int32;
begin
  Result := RawContactID;
end;

function TAndroidAddressBookContact.GetMessagingServices(const AIndex: TContactField): TContactMessagingServices;
const
  Filter = 'mimetype = ? AND raw_contact_id= ?';
  ColumnData = 0;
  ColumnType = 1;
  ColumnLabel = 2;
  ColumnProtocolType = 3;
  ColumnCustomProtocol = 4;
var
  Query: TQueryAdapter;
  ServicesList: TContactMessagingServices;
begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TContactMessagingServices>
  else if AIndex = TContactField.MessagingServices then
  begin
    ServicesList := TContactMessagingServices.Create;
    Result := ServicesList;

    Query := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
    try
      // Be carefull, if you change this array, you also should change columns indexes for getting correct values of field
      // by index. We don't search columns index in Cursor for increasing speed of fetching records
      // data1 - DATA
      // data2 - TYPE
      // data3 - LABEL
      // data5 - PROTOCOL
      // data6 - CUSTOM_PROTOCOL
      Query.SetFields(['data1', 'data2', 'data3', 'data5', 'data6']);
      Query.SetFilter(Filter, [TJCommonDataKinds_Im.JavaClass.CONTENT_ITEM_TYPE, RawContactID]);
      Query.Execute(
        procedure (ACursor: JCursor)
        var
          MessageService: TContactMessagingService;
        begin
          MessageService := TContactMessagingService.Create;
          MessageService.UserName := JStringToString(ACursor.getString(ColumnData));
          MessageService.LabelKind := TAndroidAddressBookHelper.IntegerToMessagingServiceLabelKind(ACursor.getInt(ColumnType));
          if MessageService.LabelKind = TContactMessagingService.TLabelKind.Custom then
            MessageService.LabelText := JStringToString(ACursor.getString(ColumnLabel));
          MessageService.ServiceKind := TAndroidAddressBookHelper.IntegerToMessagingServiceKind(ACursor.getInt(ColumnProtocolType));
          if MessageService.ServiceKind = TContactMessagingService.TServiceKind.Custom then
            MessageService.ServiceName := JStringToString(ACursor.getString(ColumnCustomProtocol));

          ServicesList.Add(MessageService);
        end);
    finally
      Query.Free;
    end;
  end
  else
    raise  EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.GetDates(const AIndex: TContactField): TContactDates;
begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TContactDates>
  else if AIndex = TContactField.Dates then
    Result := GetDates
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.GetDates: TContactDates;
const
  Filter = 'mimetype = ? AND raw_contact_id = ?';
  ColumnLabel = 0;
  ColumnStartDate = 1;
  ColumnDateType = 2;
var
  Query: TQueryAdapter;
  DatesList: TContactDates;
  BirthdayExcluded: Boolean;
begin
  DatesList := TContactDates.Create;
  Result := DatesList;
  BirthdayExcluded := False;
  Query := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
  try
    Query.SetFields(['data3', TJCommonDataKinds_Event.JavaClass.START_DATE, 'data2']);
    Query.SetFilter(Filter, [TJCommonDataKinds_Event.JavaClass.CONTENT_ITEM_TYPE, RawContactID]);
    Query.Execute(
      procedure (ACursor: JCursor)
      var
        EventDate: TDateTime;
        ContactDate: TContactDate;
      begin
        if TryJavaDateStringToDateTime(ACursor.getString(ColumnStartDate), EventDate) then
        begin
          // Android stores Birthday date in Dates list. We provide separated access to Birthday. So we should exclude
          // Birthday from this list. Pay attenation, that Dates can hold more than one birthday date, it's not correct,
          // but we keep another birthday dates in Dates list.
          if (ACursor.getInt(ColumnDateType) = TJCommonDataKinds_Event.JavaClass.TYPE_BIRTHDAY) and not BirthdayExcluded then
          begin
            BirthdayExcluded := True;
            Exit;
          end;

          ContactDate := TContactDate.Create;
          ContactDate.Date := EventDate;
          ContactDate.LabelKind := TAndroidAddressBookHelper.IntegerToDateKind(ACursor.getInt(ColumnDateType));
          if ContactDate.LabelKind = TContactDate.TLabelKind.Custom then
            ContactDate.LabelText := JStringToString(ACursor.getString(ColumnLabel));

          DatesList.Add(ContactDate);
        end
      end);
  finally
    Query.Free;
  end;
end;

function TAndroidAddressBookContact.GetPhones(const AIndex: TContactField): TContactPhones;
const
  Filter = 'mimetype = ? AND raw_contact_id = ?';
  ColumnFieldLabel = 0;
  ColumnFieldValue = 1;
  ColumnFieldType = 2;
var
  Query: TQueryAdapter;
  PhonesList: TContactPhones;
begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TContactPhones>
  else if AIndex = TContactField.Phones then
  begin
    PhonesList := TContactPhones.Create;
    Result := PhonesList;

    Query := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
    try
      Query.SetFields(['data3', TJCommonDataKinds_Phone.JavaClass.NUMBER, 'data2']);
      Query.SetFilter(Filter, [TJCommonDataKinds_Phone.JavaClass.CONTENT_ITEM_TYPE, RawContactID]);
      Query.Execute(
        procedure (ACursor: JCursor)
        var
          PhoneKind: TContactPhone.TLabelKind;
          PhoneLabel: string;
          PhoneNumber: string;
        begin
          PhoneNumber := JStringToString(ACursor.getString(ColumnFieldValue));
          PhoneKind := TAndroidAddressBookHelper.IntegerToPhoneKind(ACursor.getInt(ColumnFieldType));
          if PhoneKind = TContactPhone.TLabelKind.Custom then
          begin
            PhoneLabel := JStringToString(ACursor.getString(ColumnFieldLabel));
            PhonesList.AddPhone(PhoneLabel, PhoneNumber);
          end
          else
            PhonesList.AddPhone(PhoneKind, PhoneNumber);
        end);
    finally
      Query.Free;
    end;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.GetRelatedNames(const AIndex: TContactField): TContactRelatedNames;
const
  Filter = 'mimetype = ? AND raw_contact_id = ?';
  ColumnFieldLabel = 0;
  ColumnFieldValue = 1;
  ColumnFieldType = 2;
var
  Query: TQueryAdapter;
  RelatedNamesList: TContactRelatedNames;
begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TContactRelatedNames>
  else if AIndex = TContactField.RelatedNames then
  begin
    RelatedNamesList := TContactRelatedNames.Create;
    Result := RelatedNamesList;
    Query := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
    try
      Query.SetFields(['data3', TJCommonDataKinds_Relation.JavaClass.NAME, 'data2']);
      Query.SetFilter(Filter, [TJCommonDataKinds_Relation.JavaClass.CONTENT_ITEM_TYPE, RawContactID]);
      Query.Execute(
        procedure (ACursor: JCursor)
        var
          Name: string;
          Kind: TContactRelatedName.TLabelKind;
          NameLabel: string;
        begin
          Name:= JStringToString(ACursor.getString(ColumnFieldValue));
          Kind := TAndroidAddressBookHelper.IntegerToRelatedNamesKind(ACursor.getInt(ColumnFieldType));
          if Kind = TContactRelatedName.TLabelKind.Custom then
          begin
            NameLabel := JStringToString(ACursor.getString(ColumnFieldLabel));
            RelatedNamesList.AddRelatedName(NameLabel, Name);
          end
          else
            RelatedNamesList.AddRelatedName(Kind, Name);
        end);
    finally
      Query.Free;
    end;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.GetStringFieldValue(const AFieldName, AContentType: JString): string;
const
  Filter = 'mimetype = ? AND raw_contact_id = ?';
  ColumnFieldName = 0;
var
  Querry: TQueryAdapter;
  FieldValue: string;
begin
  Result := string.Empty;
  Querry := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
  try
    Querry.SetFields([AFieldName]);
    Querry.SetFilter(Filter, [AContentType, RawContactID]);
    Querry.Execute(procedure (ACursor: JCursor)
      begin
        FieldValue := JStringToString(ACursor.getString(ColumnFieldName));
      end, True);
    Result := FieldValue;
  finally
    Querry.Free;
  end;
end;

function TAndroidAddressBookContact.GetSocialProfiles(const AIndex: TContactField): TContactSocialProfiles;
begin
  raise EAddressBookExecute.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.GetSource: TAddressBookSource;
begin
  Result := TAndroidAddressBookSource.Create(AccountName, AccountType);
end;

function TAndroidAddressBookContact.GetStringValue(const AIndex: TContactField): string;
begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.ToString
  else
    case AIndex of
      TContactField.FirstName:
        Result := GetStringFieldValue(TJCommonDataKinds_StructuredName.JavaClass.GIVEN_NAME, TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.LastName:
        Result := GetStringFieldValue(TJCommonDataKinds_StructuredName.JavaClass.FAMILY_NAME, TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.MiddleName:
        Result := GetStringFieldValue(TJCommonDataKinds_StructuredName.JavaClass.MIDDLE_NAME, TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.Prefix:
        Result := GetStringFieldValue(TJCommonDataKinds_StructuredName.JavaClass.PREFIX, TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.Suffix:
        Result := GetStringFieldValue(TJCommonDataKinds_StructuredName.JavaClass.SUFFIX, TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.NickName:
        Result := GetStringFieldValue(TJCommonDataKinds_Nickname.JavaClass.NAME, TJCommonDataKinds_Nickname.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.FirstNamePhonetic:
        Result := GetStringFieldValue(TJCommonDataKinds_StructuredName.JavaClass.PHONETIC_GIVEN_NAME, TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.LastNamePhonetic:
        Result := GetStringFieldValue(TJCommonDataKinds_StructuredName.JavaClass.PHONETIC_FAMILY_NAME, TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.MiddleNamePhonetic:
        Result := GetStringFieldValue(TJCommonDataKinds_StructuredName.JavaClass.PHONETIC_MIDDLE_NAME, TJCommonDataKinds_StructuredName.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.Organization:
        Result := GetStringFieldValue(TJCommonDataKinds_Organization.JavaClass.COMPANY, TJCommonDataKinds_Organization.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.JobTitle:
        Result := GetStringFieldValue(TJCommonDataKinds_Organization.JavaClass.TITLE, TJCommonDataKinds_Organization.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.Department:
        Result := GetStringFieldValue(TJCommonDataKinds_Organization.JavaClass.DEPARTMENT, TJCommonDataKinds_Organization.JavaClass.CONTENT_ITEM_TYPE);
      TContactField.Note:
        Result := GetStringFieldValue(TJCommonDataKinds_Note.JavaClass.NOTE, TJCommonDataKinds_Note.JavaClass.CONTENT_ITEM_TYPE);
    else
      raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
    end;
end;

function TAndroidAddressBookContact.GetURLs(const AIndex: TContactField): TContactURLs;
const
  Filter = 'mimetype = ? AND raw_contact_id = ?';
  ColumnFieldLabel = 0;
  ColumnFieldValue = 1;
  ColumnFieldType = 2;
var
  Query: TQueryAdapter;
  URLList: TContactURLs;
begin
  if HasChanges(AIndex) then
    Result := FListChanges.Items[AIndex].Value.AsType<TContactURLs>
  else if AIndex = TContactField.URLs then
  begin
    URLList := TContactURLs.Create;
    Result := URLList;
    Query := TQueryAdapter.Create(TJContactsContract_Data.JavaClass.CONTENT_URI);
    try
      Query.SetFields(['data3', TJCommonDataKinds_Website.JavaClass.URL, 'data2']);
      Query.SetFilter(Filter, [TJCommonDataKinds_Website.JavaClass.CONTENT_ITEM_TYPE, RawContactID]);
      Query.Execute(
        procedure (ACursor: JCursor)
        var
          Kind: TContactURL.TLabelKind;
          URLLabel: string;
          URL: string;
        begin
          URL := JStringToString(ACursor.getString(ColumnFieldValue));
          Kind := TAndroidAddressBookHelper.IntegerToURLKind(ACursor.getInt(ColumnFieldType));
          if Kind = TContactURL.TLabelKind.Custom then
          begin
            URLLabel := JStringToString(ACursor.getString(ColumnFieldLabel));
            URLList.AddURL(URLLabel, URL);
          end
          else
            URLList.AddURL(Kind, URL);
        end);
    finally
      Query.Free;
    end;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

function TAndroidAddressBookContact.HasAtLeastOneChange(const AFieldTypes: TContactFields): Boolean;
var
  FieldType: TContactField;
begin
  Result := False;
  for FieldType in AFieldTypes do
    if HasChanges(FieldType) then
      Exit(True);
end;

function TAndroidAddressBookContact.HasChanges(const AFieldType: TContactField): Boolean;
begin
  // If user invokes RevertCurrentChangesAndUpdate, we should cut changes before specified revert time stamp
  Result := Changes.ContainsKey(AFieldType) and (Changes.Items[AFieldType].TimeStamp > FAddressBook.RevertTimeStamp);
end;

procedure TAndroidAddressBookContact.Modified;
begin
  if State = TRecordState.Viewing then
    FState := TRecordState.Modification;
end;

procedure TAndroidAddressBookContact.SetAddresses(const AIndex: TContactField; const AValue: TContactAddresses);
begin
  if AIndex in [TContactField.Addresses] then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TContactAddresses>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetBitmapValue(const AIndex: TContactField; const AValue: TBitmapSurface);
begin
  if AIndex in [TContactField.Photo] then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TBitmapSurface>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetContactID(const Value: Integer);
begin
  if FContactID <> Value then
  begin
    FContactID := Value;
    FRawContactID := UndefinedID;
    if FContactID = UndefinedID then
      FState := TRecordState.Adding
    else
      FState := TRecordState.Viewing;
  end;
end;

procedure TAndroidAddressBookContact.SetDateTimeValue(const AIndex: TContactField; const AValue: TDateTime);
begin
  if AIndex in [TContactField.Birthday] then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TDateTime>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetEmails(const AIndex: TContactField; const AValue: TContactEmails);
begin
  if AIndex = TContactField.EMails then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TContactEmails>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetDates(const AIndex: TContactField; const AValue: TContactDates);
begin
  if AIndex in [TContactField.Dates] then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TContactDates>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetMessagingServices(const AIndex: TContactField; const AValue: TContactMessagingServices);
begin
  if AIndex in [TContactField.MessagingServices] then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TContactMessagingServices>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetPhones(const AIndex: TContactField; const AValue: TContactPhones);
begin
  if AIndex = TContactField.Phones then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TContactPhones>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetRelatedNames(const AIndex: TContactField; const AValue: TContactRelatedNames);
begin
  if AIndex = TContactField.RelatedNames then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TContactRelatedNames>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetSocialProfiles(const AIndex: TContactField; const AValue: TContactSocialProfiles);
begin
  raise EAddressBookExecute.Create(SSocialProfilesAreNotSupported);
end;

procedure TAndroidAddressBookContact.SetStringValue(const AIndex: TContactField; const AValue: string);
begin
  if AIndex in [TContactField.FirstName, TContactField.LastName, TContactField.MiddleName, TContactField.Prefix,
    TContactField.Suffix, TContactField.NickName, TContactField.FirstNamePhonetic, TContactField.LastNamePhonetic,
    TContactField.MiddleNamePhonetic, TContactField.Organization, TContactField.JobTitle, TContactField.Department,
    TContactField.Note] then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(AValue, Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

procedure TAndroidAddressBookContact.SetURLs(const AIndex: TContactField; const AValue: TContactURLs);
begin
  if AIndex = TContactField.URLs then
  begin
    FListChanges.Remove(AIndex);
    FListChanges.Add(AIndex, TFieldValue.Create(TValue.From<TContactURLs>(AValue), Now));
    Modified;
  end
  else
    raise EAddressBookWrongArgs.CreateFmt(SFieldTypeIsNotSupportedOnCurrentPlatform, [AIndex.ToString]);
end;

{ TAndroidAddressBookTranslator }

class function TAndroidAddressBookHelper.PhoneKindToInteger(const AKind: TContactPhone.TLabelKind): Integer;
begin
  case AKind of
    TContactPhone.TLabelKind.Home:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_HOME;
    TContactPhone.TLabelKind.Mobile:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_MOBILE;
    TContactPhone.TLabelKind.Work:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_WORK;
    TContactPhone.TLabelKind.FaxWork:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_FAX_WORK;
    TContactPhone.TLabelKind.FaxHome:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_FAX_HOME;
    TContactPhone.TLabelKind.FaxOther:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_OTHER_FAX;
    TContactPhone.TLabelKind.Pager:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_PAGER;
    TContactPhone.TLabelKind.Other:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_OTHER;
    TContactPhone.TLabelKind.Callback:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_CALLBACK;
    TContactPhone.TLabelKind.Car:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_CAR;
    TContactPhone.TLabelKind.CompanyMain:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_COMPANY_MAIN;
    TContactPhone.TLabelKind.ISDN:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_ISDN;
    TContactPhone.TLabelKind.Main:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_MAIN;
    TContactPhone.TLabelKind.Radio:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_RADIO;
    TContactPhone.TLabelKind.Telex:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_TELEX;
    TContactPhone.TLabelKind.TTYTDD:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_TTY_TDD;
    TContactPhone.TLabelKind.WorkMobile:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_WORK_MOBILE;
    TContactPhone.TLabelKind.WorkPager:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_WORK_PAGER;
    TContactPhone.TLabelKind.Assistant:
      Result := TJCommonDataKinds_Phone.JavaClass.TYPE_ASSISTANT;
  else
    Result := 0;
  end;
end;

class function TAndroidAddressBookHelper.RelatedNameKindToInteger(const AKind: TContactRelatedName.TLabelKind): Integer;
begin
  case AKind of
    TContactRelatedName.TLabelKind.Mother:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_MOTHER;
    TContactRelatedName.TLabelKind.Father:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_FATHER;
    TContactRelatedName.TLabelKind.Parent:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_PARENT;
    TContactRelatedName.TLabelKind.Sister:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_SISTER;
    TContactRelatedName.TLabelKind.Brother:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_BROTHER;
    TContactRelatedName.TLabelKind.Child:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_CHILD;
    TContactRelatedName.TLabelKind.Friend:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_FRIEND;
    TContactRelatedName.TLabelKind.Spouse:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_SPOUSE;
    TContactRelatedName.TLabelKind.Partner:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_PARTNER;
    TContactRelatedName.TLabelKind.Manager:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_MANAGER;
    TContactRelatedName.TLabelKind.Assistant:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_ASSISTANT;
    TContactRelatedName.TLabelKind.DomesticPartner:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_DOMESTIC_PARTNER;
    TContactRelatedName.TLabelKind.ReferredBy:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_REFERRED_BY;
    TContactRelatedName.TLabelKind.Relative:
      Result := TJCommonDataKinds_Relation.JavaClass.TYPE_RELATIVE;
  else
    Result := 0;
  end;
end;

class function TAndroidAddressBookHelper.URLKindToInteger(const AKind: TContactURL.TLabelKind): Integer;
begin
  case AKind of
    TContactURL.TLabelKind.HomePage:
      Result := TJCommonDataKinds_Website.JavaClass.TYPE_HOMEPAGE;
    TContactURL.TLabelKind.Blog:
      Result := TJCommonDataKinds_Website.JavaClass.TYPE_BLOG;
    TContactURL.TLabelKind.Profile:
      Result := TJCommonDataKinds_Website.JavaClass.TYPE_PROFILE;
    TContactURL.TLabelKind.Home:
      Result := TJCommonDataKinds_Website.JavaClass.TYPE_HOME;
    TContactURL.TLabelKind.Work:
      Result := TJCommonDataKinds_Website.JavaClass.TYPE_WORK;
    TContactURL.TLabelKind.FTP:
      Result := TJCommonDataKinds_Website.JavaClass.TYPE_FTP;
    TContactURL.TLabelKind.Other:
      Result := TJCommonDataKinds_Website.JavaClass.TYPE_OTHER;
  else
    Result := 0;
  end;
end;

class function TAndroidAddressBookHelper.AddressKindToInteger(const AKind: TContactAddress.TLabelKind): Integer;
begin
  case AKind of
    TContactAddress.TLabelKind.Home:
      Result := TJCommonDataKinds_StructuredPostal.JavaClass.TYPE_HOME;
    TContactAddress.TLabelKind.Work:
      Result := TJCommonDataKinds_StructuredPostal.JavaClass.TYPE_WORK;
    TContactAddress.TLabelKind.Other:
      Result := TJCommonDataKinds_StructuredPostal.JavaClass.TYPE_OTHER;
  else
    Result := 0;
  end;
end;

class function TAndroidAddressBookHelper.DateKindToInteger(const AType: TContactDate.TLabelKind): Integer;
begin
  case AType of
    TContactDate.TLabelKind.Birthday:
      Result := TJCommonDataKinds_Event.JavaClass.TYPE_BIRTHDAY;
    TContactDate.TLabelKind.Anniversary:
      Result := TJCommonDataKinds_Event.JavaClass.TYPE_ANNIVERSARY;
    TContactDate.TLabelKind.Other:
      Result := TJCommonDataKinds_Event.JavaClass.TYPE_OTHER;
  else
    Result := 0;
  end;
end;

class function TAndroidAddressBookHelper.EmailKindToInteger(const AKind: TContactEmail.TLabelKind): Integer;
begin
  case AKind of
    TContactEmail.TLabelKind.Home:
      Result := TJCommonDataKinds_Email.JavaClass.TYPE_HOME;
    TContactEmail.TLabelKind.Mobile:
    Result := TJCommonDataKinds_Email.JavaClass.TYPE_MOBILE;
    TContactEmail.TLabelKind.Work:
      Result := TJCommonDataKinds_Email.JavaClass.TYPE_WORK;
    TContactEmail.TLabelKind.Other:
      Result := TJCommonDataKinds_Email.JavaClass.TYPE_OTHER;
  else
    Result := 0;
  end;
end;

class function TAndroidAddressBookHelper.IntegerToAddressLabelKind(const AType: Integer): TContactAddress.TLabelKind;
begin
  if AType = TJCommonDataKinds_StructuredPostal.JavaClass.TYPE_HOME then
    Result := TContactAddress.TLabelKind.Home
  else if AType = TJCommonDataKinds_StructuredPostal.JavaClass.TYPE_WORK then
    Result := TContactAddress.TLabelKind.Work
  else if AType = TJCommonDataKinds_StructuredPostal.JavaClass.TYPE_OTHER then
    Result := TContactAddress.TLabelKind.Other
  else
    Result := TContactAddress.TLabelKind.Custom;
end;

class function TAndroidAddressBookHelper.IntegerToDateKind(const AType: Integer): TContactDate.TLabelKind;
begin
  if AType = TJCommonDataKinds_Event.JavaClass.TYPE_ANNIVERSARY then
    Result := TContactDate.TLabelKind.Anniversary
  else if AType = TJCommonDataKinds_Event.JavaClass.TYPE_BIRTHDAY then
    Result := TContactDate.TLabelKind.Birthday
  else if AType = TJCommonDataKinds_Event.JavaClass.TYPE_OTHER then
    Result := TContactDate.TLabelKind.Other
  else
    Result := TContactDate.TLabelKind.Custom;
end;

class function TAndroidAddressBookHelper.IntegerToEmailKind(const AType: Integer): TContactEmail.TLabelKind;
begin
  if AType = TJCommonDataKinds_Email.JavaClass.TYPE_HOME then
    Result := TContactEmail.TLabelKind.Home
  else if AType = TJCommonDataKinds_Email.JavaClass.TYPE_WORK then
    Result := TContactEmail.TLabelKind.Work
  else if AType = TJCommonDataKinds_Email.JavaClass.TYPE_OTHER then
    Result := TContactEmail.TLabelKind.Other
  else if AType = TJCommonDataKinds_Email.JavaClass.TYPE_MOBILE then
    Result := TContactEmail.TLabelKind.Mobile
  else
    Result := TContactEmail.TLabelKind.Custom;
end;

class function TAndroidAddressBookHelper.IntegerToMessagingServiceKind(const AType: Integer): TContactMessagingService.TServiceKind;
begin
  if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_AIM then
    Result := TContactMessagingService.TServiceKind.AIM
  else if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_MSN then
    Result := TContactMessagingService.TServiceKind.MSN
  else if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_YAHOO then
    Result := TContactMessagingService.TServiceKind.Yahoo
  else if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_SKYPE then
    Result := TContactMessagingService.TServiceKind.Skype
  else if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_QQ then
    Result := TContactMessagingService.TServiceKind.QQ
  else if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_GOOGLE_TALK then
    Result := TContactMessagingService.TServiceKind.GoogleTalk
  else if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_ICQ then
    Result := TContactMessagingService.TServiceKind.ICQ
  else if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_JABBER then
    Result := TContactMessagingService.TServiceKind.Jabber
  else if AType = TJCommonDataKinds_Im.JavaClass.PROTOCOL_NETMEETING then
    Result := TContactMessagingService.TServiceKind.NetMeeting
  else
    Result := TContactMessagingService.TServiceKind.Custom;
end;

class function TAndroidAddressBookHelper.IntegerToMessagingServiceLabelKind(const AType: Integer): TContactMessagingService.TLabelKind;
begin
  if AType = TJCommonDataKinds_Im.JavaClass.TYPE_HOME then
    Result := TContactMessagingService.TLabelKind.Home
  else if AType = TJCommonDataKinds_Im.JavaClass.TYPE_OTHER then
    Result := TContactMessagingService.TLabelKind.Other
  else if AType = TJCommonDataKinds_Im.JavaClass.TYPE_WORK then
    Result := TContactMessagingService.TLabelKind.Work
  else
    Result := TContactMessagingService.TLabelKind.Custom;
end;

class function TAndroidAddressBookHelper.IntegerToPhoneKind(const AType: Integer): TContactPhone.TLabelKind;
begin
  if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_HOME then
    Result := TContactPhone.TLabelKind.Home
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_MOBILE then
    Result := TContactPhone.TLabelKind.Mobile
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_WORK then
    Result := TContactPhone.TLabelKind.Work
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_FAX_WORK then
    Result := TContactPhone.TLabelKind.FaxWork
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_FAX_HOME then
    Result := TContactPhone.TLabelKind.FaxHome
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_PAGER then
    Result := TContactPhone.TLabelKind.Pager
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_OTHER then
    Result := TContactPhone.TLabelKind.Other
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_CALLBACK then
    Result := TContactPhone.TLabelKind.Callback
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_CAR then
    Result := TContactPhone.TLabelKind.Car
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_COMPANY_MAIN then
    Result := TContactPhone.TLabelKind.CompanyMain
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_ISDN then
    Result := TContactPhone.TLabelKind.ISDN
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_MAIN then
    Result := TContactPhone.TLabelKind.Main
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_OTHER_FAX then
    Result := TContactPhone.TLabelKind.FaxOther
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_RADIO then
    Result := TContactPhone.TLabelKind.Radio
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_TELEX then
    Result := TContactPhone.TLabelKind.Telex
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_TTY_TDD then
    Result := TContactPhone.TLabelKind.TTYTDD
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_WORK_MOBILE then
    Result := TContactPhone.TLabelKind.Mobile
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_WORK_PAGER then
    Result := TContactPhone.TLabelKind.WorkPager
  else if AType = TJCommonDataKinds_Phone.JavaClass.TYPE_ASSISTANT then
    Result := TContactPhone.TLabelKind.Assistant
  else
    Result := TContactPhone.TLabelKind.Custom;
end;

class function TAndroidAddressBookHelper.IntegerToRelatedNamesKind(const AType: Integer): TContactRelatedName.TLabelKind;
begin
  if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_ASSISTANT then
    Result := TContactRelatedName.TLabelKind.Assistant
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_BROTHER then
    Result := TContactRelatedName.TLabelKind.Brother
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_CHILD then
    Result := TContactRelatedName.TLabelKind.Child
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_DOMESTIC_PARTNER then
    Result := TContactRelatedName.TLabelKind.DomesticPartner
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_FATHER then
    Result := TContactRelatedName.TLabelKind.Father
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_FRIEND then
    Result := TContactRelatedName.TLabelKind.Friend
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_MANAGER then
    Result := TContactRelatedName.TLabelKind.Manager
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_MOTHER then
    Result := TContactRelatedName.TLabelKind.Mother
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_PARENT then
    Result := TContactRelatedName.TLabelKind.Parent
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_PARTNER then
    Result := TContactRelatedName.TLabelKind.Partner
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_REFERRED_BY then
    Result := TContactRelatedName.TLabelKind.ReferredBy
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_RELATIVE then
    Result := TContactRelatedName.TLabelKind.Relative
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_SISTER then
    Result := TContactRelatedName.TLabelKind.Sister
  else if AType = TJCommonDataKinds_Relation.JavaClass.TYPE_SPOUSE then
    Result := TContactRelatedName.TLabelKind.Spouse
  else
    Result := TContactRelatedName.TLabelKind.Custom;
end;

class function TAndroidAddressBookHelper.IntegerToURLKind(const AType: Integer): TContactURL.TLabelKind;
begin
  if AType = TJCommonDataKinds_Website.JavaClass.TYPE_HOMEPAGE then
    Result := TContactURL.TLabelKind.HomePage
  else if AType = TJCommonDataKinds_Website.JavaClass.TYPE_BLOG then
    Result := TContactURL.TLabelKind.Blog
  else if AType = TJCommonDataKinds_Website.JavaClass.TYPE_PROFILE then
    Result := TContactURL.TLabelKind.Profile
  else if AType = TJCommonDataKinds_Website.JavaClass.TYPE_HOME then
    Result := TContactURL.TLabelKind.Home
  else if AType = TJCommonDataKinds_Website.JavaClass.TYPE_WORK then
    Result := TContactURL.TLabelKind.Work
  else if AType = TJCommonDataKinds_Website.JavaClass.TYPE_FTP then
    Result := TContactURL.TLabelKind.FTP
  else if AType = TJCommonDataKinds_Website.JavaClass.TYPE_OTHER then
    Result := TContactURL.TLabelKind.Other
  else
    Result := TContactURL.TLabelKind.Custom;
end;

class function TAndroidAddressBookHelper.MessagingServiceKindToInteger(const AKind: TContactMessagingService.TServiceKind): Integer;
begin
  case AKind of
    TContactMessagingService.TServiceKind.Yahoo:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_YAHOO;
    TContactMessagingService.TServiceKind.Jabber:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_JABBER;
    TContactMessagingService.TServiceKind.MSN:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_MSN;
    TContactMessagingService.TServiceKind.ICQ:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_ICQ;
    TContactMessagingService.TServiceKind.AIM:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_AIM;
    TContactMessagingService.TServiceKind.QQ:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_QQ;
    TContactMessagingService.TServiceKind.GoogleTalk:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_GOOGLE_TALK;
    TContactMessagingService.TServiceKind.Skype:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_SKYPE;
    TContactMessagingService.TServiceKind.NetMeeting:
      Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_NETMEETING;
  else
    Result := TJCommonDataKinds_Im.JavaClass.PROTOCOL_CUSTOM;
  end;
end;

class function TAndroidAddressBookHelper.MessagingServiceLabelKindToInteger(const AKind: TContactMessagingService.TLabelKind): Integer;
begin
  case AKind of
    TContactMessagingService.TLabelKind.Home:
      Result := TJCommonDataKinds_Im.JavaClass.TYPE_HOME;
    TContactMessagingService.TLabelKind.Work:
      Result := TJCommonDataKinds_Im.JavaClass.TYPE_WORK;
    TContactMessagingService.TLabelKind.Other:
      Result := TJCommonDataKinds_Im.JavaClass.TYPE_OTHER;
  else
    Result := 0;
  end;
end;

{ TAndroidAddressBookGroup }

procedure TAndroidAddressBookGroup.SetID(const AValue: Int64);
begin
  FID := AValue;
  FName := string.Empty;
  if FID <> UndefinedID then
    FState := TRecordState.Viewing
  else
    FState := TRecordState.Adding;
end;

procedure TAndroidAddressBookGroup.SetName(const AValue: string);
begin
  if FName <> AValue then
  begin
    FName := AValue;
    FNameChangeTimestamp := Now;
    if State = TRecordState.Viewing then
      FState := TRecordState.Modification;
  end;
end;

function TAndroidAddressBookGroup.GetSource: TAddressBookSource;
begin
  Result := TAndroidAddressBookSource.Create(AccountName, AccountType);
end;

function TAndroidAddressBookGroup.GetID: Integer;
begin
  if State <> TRecordState.Adding then
    Result := FID
  else
    Result := UndefinedID;
end;

function TAndroidAddressBookGroup.GetName: string;
const
  Filter = '_ID = ?';
var
  Query: TQueryAdapter;
begin
  // Ignore changes, if user invoked AddressBook.RevertCurrentChanges
  if (FState = TRecordState.Viewing) or (FState = TRecordState.Modification) and (FNameChangeTimestamp > FAddressBook.RevertTimeStamp) then
  begin
    Query := TQueryAdapter.Create(TJContactsContract_Groups.JavaClass.CONTENT_URI);
    try
      Query.SetFilter(Filter, [ID]);
      Query.SetFields(['title']);
      Query.Execute(procedure (ACursor: JCursor)
        begin
          FName := JStringToString(ACursor.getString(0));
        end, True);
    finally
      Query.Free
    end;
  end;

  Result := FName;
end;

constructor TAndroidAddressBookGroup.CreateFromExisting(const AAddressBook: TAndroidAddressBook; const AID: Int64);
begin
  if AAddressBook = nil then
    raise EAddressBookWrongArgs.CreateFmt(SCannotCreateGroupNilArg, ['AAddressBook']);

  FAddressBook := AAddressBook;
  FNameChangeTimestamp := Now;
  FID := AID;
  if FID = UndefinedID then
    FState := TRecordState.Adding
  else
    FState := TRecordState.Viewing;
end;

constructor TAndroidAddressBookGroup.CreateNew(const AAddressBook: TAndroidAddressBook; const ASource: TAddressBookSource);
begin
  CreateFromExisting(AAddressBook, UndefinedID);

  if ASource <> nil then
  begin
    FAccountType := ASource.SourceType;
    FAccountName := ASource.SourceName;
  end;
end;

{ TAndroidAddressBookFactory }

function TAndroidAddressBookServices.CreateAddressBook: IFMXAddressBookService;
begin
  Result := TAndroidAddressBook.Create;
end;


function TAndroidAddressBookServices.AddressesLabelKinds: TContactAddress.TLabelKinds;
begin
  Result := [TContactAddress.TLabelKind.Custom, TContactAddress.TLabelKind.Home, TContactAddress.TLabelKind.Work,
    TContactAddress.TLabelKind.Other];
end;

function TAndroidAddressBookServices.DatesLabelKinds: TContactDate.TLabelKinds;
begin
  Result := [TContactDate.TLabelKind.Custom, TContactDate.TLabelKind.Birthday, TContactDate.TLabelKind.Anniversary,
    TContactDate.TLabelKind.Other];
end;

function TAndroidAddressBookServices.EmailsLabelKinds: TContactEmail.TLabelKinds;
begin
  Result := [TContactEmail.TLabelKind.Custom, TContactEmail.TLabelKind.Home, TContactEmail.TLabelKind.Mobile,
    TContactEmail.TLabelKind.Work, TContactEmail.TLabelKind.Other];
end;

function TAndroidAddressBookServices.MessagingServicesLabelKinds: TContactMessagingService.TLabelKinds;
begin
  Result := [TContactMessagingService.TLabelKind.Custom, TContactMessagingService.TLabelKind.Home,
    TContactMessagingService.TLabelKind.Work, TContactMessagingService.TLabelKind.Other];
end;

function TAndroidAddressBookServices.MessagingServicesKinds: TContactMessagingService.TServiceKinds;
begin
  Result := [TContactMessagingService.TServiceKind.Custom, TContactMessagingService.TServiceKind.Yahoo,
    TContactMessagingService.TServiceKind.Jabber, TContactMessagingService.TServiceKind.MSN,
    TContactMessagingService.TServiceKind.MSN, TContactMessagingService.TServiceKind.ICQ,
    TContactMessagingService.TServiceKind.ICQ, TContactMessagingService.TServiceKind.AIM,
    TContactMessagingService.TServiceKind.QQ, TContactMessagingService.TServiceKind.GoogleTalk,
    TContactMessagingService.TServiceKind.Skype, TContactMessagingService.TServiceKind.NetMeeting];
end;

function TAndroidAddressBookServices.PhonesLabelKinds: TContactPhone.TLabelKinds;
begin
  Result := [TContactPhone.TLabelKind.Custom, TContactPhone.TLabelKind.Home, TContactPhone.TLabelKind.Mobile,
    TContactPhone.TLabelKind.Work, TContactPhone.TLabelKind.FaxWork, TContactPhone.TLabelKind.FaxHome,
    TContactPhone.TLabelKind.Pager, TContactPhone.TLabelKind.Other, TContactPhone.TLabelKind.Callback,
    TContactPhone.TLabelKind.Car, TContactPhone.TLabelKind.CompanyMain, TContactPhone.TLabelKind.ISDN,
    TContactPhone.TLabelKind.Main, TContactPhone.TLabelKind.FaxOther, TContactPhone.TLabelKind.Radio,
    TContactPhone.TLabelKind.Telex, TContactPhone.TLabelKind.TTYTDD, TContactPhone.TLabelKind.WorkMobile,
    TContactPhone.TLabelKind.WorkPager, TContactPhone.TLabelKind.Assistant];
end;

function TAndroidAddressBookServices.RelatedNamesLabelKinds: TContactRelatedName.TLabelKinds;
begin
  Result := [TContactRelatedName.TLabelKind.Custom, TContactRelatedName.TLabelKind.Mother,
    TContactRelatedName.TLabelKind.Father, TContactRelatedName.TLabelKind.Parent, TContactRelatedName.TLabelKind.Sister,
    TContactRelatedName.TLabelKind.Brother, TContactRelatedName.TLabelKind.Child, TContactRelatedName.TLabelKind.Friend,
    TContactRelatedName.TLabelKind.Spouse, TContactRelatedName.TLabelKind.Partner, TContactRelatedName.TLabelKind.Manager,
    TContactRelatedName.TLabelKind.Assistant, TContactRelatedName.TLabelKind.DomesticPartner,
    TContactRelatedName.TLabelKind.ReferredBy, TContactRelatedName.TLabelKind.Relative];
end;

function TAndroidAddressBookServices.SocialProfilesLabelKinds: TContactSocialProfile.TLabelKinds;
begin
  Result := [];
end;

function TAndroidAddressBookServices.SocialProfilesServiceKinds: TContactSocialProfile.TServiceKinds;
begin
  Result := [];
end;

function TAndroidAddressBookServices.URLsLabelKinds: TContactURL.TLabelKinds;
begin
  Result := [TContactURL.TLabelKind.Custom, TContactURL.TLabelKind.HomePage, TContactURL.TLabelKind.Blog,
    TContactURL.TLabelKind.Profile, TContactURL.TLabelKind.Home, TContactURL.TLabelKind.Work, TContactURL.TLabelKind.FTP,
    TContactURL.TLabelKind.Other];
end;

{ TAndroidOperationBuilder }

class function TOperationBuilderAdapter.CreateInsert(const AContentItemType: JString): TOperationBuilderAdapter;
begin
  Result := TOperationBuilderAdapter.Create;
  Result.FBuilder := TJContentProviderOperation.JavaClass.newInsert(TJContactsContract_Data.JavaClass.CONTENT_URI);
  Result.FBuilder.withValueBackReference(StringToJString('raw_contact_id'), 0);
  Result.AddValue('mimetype', AContentItemType);
end;

procedure TOperationBuilderAdapter.AddValue(const AFieldName, AValue: string);
begin
  FBuilder.withValue(StringToJString(AFieldName), StringToJString(AValue));
end;

procedure TOperationBuilderAdapter.AddValue(const AFieldName: JString; const AValue: string);
begin
  FBuilder.withValue(AFieldName, StringToJString(AValue));
end;

procedure TOperationBuilderAdapter.AddValue(const AFieldName: string; const AValue: Int64);
begin
  FBuilder.withValue(StringToJString(AFieldName), Int64ToJLong(AValue));
end;

procedure TOperationBuilderAdapter.AddValue(const AFieldName: string; const AValue: JString);
begin
  FBuilder.withValue(StringToJString(AFieldName), AValue);
end;

procedure TOperationBuilderAdapter.AddValue(const AFieldName: string; const ABitmapSurface: TBitmapSurface);
var
  AndroidBitmap: JBitmap;
  Stream: JByteArrayOutputStream;
begin
  AndroidBitmap := TJBitmap.JavaClass.createBitmap(ABitmapSurface.Width, ABitmapSurface.Height, TJBitmap_Config.JavaClass.ARGB_8888);
  if SurfaceToJBitmap(ABitmapSurface, AndroidBitmap) then
  begin
    Stream := TJByteArrayOutputStream.Create;
    AndroidBitmap.compress(TJBitmap_CompressFormat.JavaClass.PNG, 0, Stream);
    FBuilder.withValue(StringToJString(AFieldName), TJObject.Wrap(Stream.toByteArray.ToPointer)); // PHOTO
  end
  else
    raise EAddressBookExecute.Create(SCannotConvertTBitmapToJBitmap);
end;

function TOperationBuilderAdapter.Build: JContentProviderOperation;
begin
  Result := FBuilder.build;
end;

constructor TOperationBuilderAdapter.Create;
begin
  // Just hide from public for motivation use class function instead;
end;

class function TOperationBuilderAdapter.CreateDelete(const AContentItemType: JString; const ARawContactID: Int32): TOperationBuilderAdapter;
const
  Filter = 'raw_contact_id = ? AND mimetype = ?';
var
  QueryParams: TJavaObjectArray<JString>;
begin
  Result := TOperationBuilderAdapter.Create;
  Result.FBuilder := TJContentProviderOperation.JavaClass.newDelete(TJContactsContract_Data.JavaClass.CONTENT_URI);
  QueryParams := CreateJavaStringArray([ARawContactID, AContentItemType]);
  try
    Result.FBuilder.withSelection(StringToJString(Filter), QueryParams);
  finally
    QueryParams.Free;
  end;
end;

class function  TOperationBuilderAdapter.CreateInsert(const AContentItemType: JString; const ARawContactID: Int32): TOperationBuilderAdapter;
begin
  Result := TOperationBuilderAdapter.Create;
  Result.FBuilder := TJContentProviderOperation.JavaClass.newInsert(TJContactsContract_Data.JavaClass.CONTENT_URI);
  Result.AddValue('raw_contact_id', ARawContactID);
  Result.AddValue('mimetype', AContentItemType);
end;

class function TOperationBuilderAdapter.CreateUpdate(const AContentItemType: JString; const ARawContactID: Int32): TOperationBuilderAdapter;
var
  Arguments: TJavaObjectArray<JString>;
begin
  Result := TOperationBuilderAdapter.Create;
  Result.FBuilder := TJContentProviderOperation.JavaClass.newUpdate(TJContactsContract_Data.JavaClass.CONTENT_URI);
  Arguments := CreateJavaStringArray([ARawContactID , AContentItemType]);
  Result.FBuilder.withSelection(StringToJString('raw_contact_id = ? AND mimetype = ?'), Arguments);
end;

procedure TOperationBuilderAdapter.AddValue(const AFieldName: JString; const AValue: TDateTime);
begin
  FBuilder.withValue(AFieldName, DateToJavaDate(AValue));
end;

{ TAddressBookChangesListener }

constructor TAddressBookChangesListener.Create(const AAddressBook: TAndroidAddressBook);
begin
  if AAddressBook = nil then
    raise EAddressBookWrongArgs.CreateFmt(SWrongParameter, ['AAddressBook']);
  inherited Create;
  FAddressBook := AAddressBook;
end;

procedure TAddressBookChangesListener.onChanged(selfChange: Boolean);
begin
  if not FAddressBook.Processing and not selfChange then
    TThread.Queue(nil, procedure
    begin
      FAddressBook.DoExternalChange;
    end);
end;

{ TAndroidAddressBookContact.TFieldValue }

constructor TAndroidAddressBookContact.TFieldValue.Create(const AValue: TValue; const ATimeStamp: TDateTime);
begin
  Value := AValue;
  TimeStamp := ATimeStamp;
end;

{ TQuerryAdapter }

constructor TQueryAdapter.Create(const AContentURI: Jnet_Uri);
begin
  inherited Create;
  FContentURI := AContentURI;
end;

destructor TQueryAdapter.Destroy;
begin
  FFields.Free;
  FParams.Free;
  inherited;
end;

function TQueryAdapter.Execute(const ACallback: TQueryCallback; const AOnlyFirst: Boolean = False): Boolean;
var
  DataCursor: JCursor;
begin
  Result := Assigned(ACallback);
  if Result then
    try
      DataCursor := TAndroidHelper.ContentResolver.query(FContentURI, FFields, StringToJString(FFilter), FParams, nil);
      if DataCursor <> nil then
        try
          while DataCursor.moveToNext do
          begin
            ACallback(DataCursor);
            if AOnlyFirst then
              Break;
          end;
        finally
          DataCursor.close;
        end;
    except
      on E: EJNIException do
      begin
        raise EAddressBookExecute.CreateFmt(SCannotExtractMultipleStringValue, [E.Message]);
      end;
    end;
end;

procedure TQueryAdapter.SetFields(const ASource: array of const);
begin
  FreeAndNil(FFields);
  FFields := CreateJavaStringArray(ASource);
end;

procedure TQueryAdapter.SetFilter(const AFilter: string; const AParams: array of const);
begin
  FreeAndNil(FParams);
  FParams := CreateJavaStringArray(AParams);
  FFilter := AFilter;
end;

{ TAndroidAddressBookSource }

constructor TAndroidAddressBookSource.Create(const AAccountName, AAccountType: string);
begin
  FAccountName := AAccountName;
  FAccountType := AAccountType;
end;

function TAndroidAddressBookSource.GetID: string;
begin
   Result := Format('%s/%s', [SourceName, SourceType]);
end;

function TAndroidAddressBookSource.GetSourceName: string;
begin
  Result := FAccountName;
end;

function TAndroidAddressBookSource.GetSourceType: string;
begin
  Result := FAccountType;
end;

end.
