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
unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Permissions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  System.Notification, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.AddressBook, FMX.TabControl, FMX.Calendar, FMX.DateTimeCtrls,
  FMX.ListBox, FMX.Layouts, System.ImageList, FMX.ImgList, FMX.Surfaces, FMX.Ani, FMX.Objects,
  System.Generics.Collections, System.Generics.Defaults, FMX.AddressBook.Types;

resourcestring
  NotificationMessage = 'Don''t forget to congratulate %s';

type
  TFetchContactThread = class;

  TFormMain = class(TForm)
    NotificationCenter1: TNotificationCenter;
    ListView1: TListView;
    ToolBar1: TToolBar;
    LabelTitle: TLabel;
    SpeedButton1: TSpeedButton;
    ImageList1: TImageList;
    AddressBook: TAddressBook;
    ProgressPanel: TRectangle;
    ProgressBar: TProgressBar;
    LabelProgress: TLabel;
    ProgressPanelAnimation: TFloatAnimation;
    procedure ListView1PullRefresh(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure AddressBookPermissionRequest(ASender: TObject; const AMessage: string; const AAccessGranted: Boolean);
    procedure FormShow(Sender: TObject);
    procedure AddressBookExternalChange(ASender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FContacts: TAddressBookContacts;
    FThread: TFetchContactThread;
    FPermissionReadContacts: string;
    procedure FillContactsList;
    function CreateRoundPhoto(const ASource: TBitmapSurface): TBitmap;
    procedure PostNotification(const ADisplayName: string; const ABirthday: TDateTime; const ARemainderDays: Integer);
    function DefineRemainedDays(const ABirthday: TDate): Integer;
    function ChangeYear(const ADateTime: TDateTime; const ANewYear: Word): TDateTime;
    { TFetchContactThread events handlers }
    procedure ContactLoadingBegin(Sender: TObject);
    procedure ContactLoaded(const ATotalCount:Integer; const ANumber: Integer; const ABirthday: TDateTime; const ADisplayName: string;
      const APhoto: TBitmapSurface);
    procedure ContactLoadingEnd(Sender: TObject);
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
  public
    procedure SortContacts;
  end;

  TFetchContactThread = class(TThread)
  public type
    TOnContactLoaded = procedure (const ATotalCount: Integer; const ANumber: Integer; const ABirthday: TDateTime;
      const ADisplayName: string; const APhoto: TBitmapSurface) of object;
  private
    [Weak] FAllContacts: TAddressBookContacts;
    FOnStart: TNotifyEvent;
    FOnContactLoaded: TOnContactLoaded;
  protected
    procedure Execute; override;
    procedure DoStart;
    procedure DoContactLoaded(const ANumber: Integer; const ABirthday: TDateTime; const ADisplayName: string;
      const APhoto: TBitmapSurface);
  public
    constructor Create(const AContacts: TAddressBookContacts);
    property OnContactLoaded: TOnContactLoaded read FOnContactLoaded write FOnContactLoaded;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

var
  FormMain: TFormMain;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  System.DateUtils, System.Math, FMX.DialogService;

{$R *.fmx}

const
  Months: array [1..12] of string = ('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August',
    'September', 'October', 'November', 'December');

{ TFormMain }

procedure TFormMain.AddressBookExternalChange(ASender: TObject);
begin
  AddressBook.RevertCurrentChangesAndUpdate;
end;

procedure TFormMain.AddressBookPermissionRequest(ASender: TObject; const AMessage: string;
  const AAccessGranted: Boolean);
begin
  if AAccessGranted then
    FillContactsList
  else
  begin
    ProgressPanelAnimation.Enabled := True;
    ProgressPanelAnimation.Start;
    ProgressBar.Visible := False;
    LabelProgress.Text := 'User not allowed to read contacts. '+ AMessage;
  end;
end;

function TFormMain.ChangeYear(const ADateTime: TDateTime; const ANewYear: Word): TDateTime;
var
  Year: Word;
  Month: Word;
  Days: Word;
begin
  DecodeDate(ADateTime, Year, Month, Days);
  if Month = MonthFebruary then
    Days := Min(Days, DaysInAMonth(ANewYear, MonthFebruary));
  Result := EncodeDate(ANewYear, Month, Days);
  ReplaceTime(Result, ADateTime);
end;

procedure TFormMain.ContactLoaded(const ATotalCount: Integer; const ANumber: Integer; const ABirthday: TDateTime; const ADisplayName: string;
  const APhoto: TBitmapSurface);
var
  Item: TListViewItem;
  RemainderDays: Integer;
begin
  if not IsNan(ABirthday) then
  begin
    RemainderDays := DefineRemainedDays(ABirthday);
    Item := ListView1.Items.Add;
    Item.Detail := FormatDateTime('dd mmmm', ABirthday);
    Item.Text := ADisplayName;
    Item.Tag := RemainderDays;
    Item.ButtonText := Format('%d days', [RemainderDays]);
    if APhoto = nil then
      Item.ImageIndex := 0
    else
      Item.Bitmap := CreateRoundPhoto(APhoto);

    PostNotification(ADisplayName, ABirthday, RemainderDays);
    SortContacts;
  end;
  ProgressBar.Value := ANumber + 1;  // Number start from 0, so we need to add 1
  LabelProgress.Text := Format('Loaded %d from %d contacts', [ANumber, ATotalCount]);
end;

procedure TFormMain.ContactLoadingBegin(Sender: TObject);
begin
  ListView1.Items.Clear;
  ListView1.PullRefreshWait := True;
  NotificationCenter1.CancelAll;
  ProgressPanelAnimation.Inverse := False;
  ProgressPanelAnimation.Enabled := True;
  ProgressPanelAnimation.Start;
  ProgressBar.Value := 0;
  ProgressBar.Max := FContacts.Count;
end;

procedure TFormMain.ContactLoadingEnd(Sender: TObject);
begin
  ListView1.PullRefreshWait := False;
  ProgressPanelAnimation.Inverse := True;
  ProgressPanelAnimation.Start;
end;

function TFormMain.CreateRoundPhoto(const ASource: TBitmapSurface): TBitmap;
var
  Size: Integer;
begin
  Result := TBitmap.Create;
  Size := Min(ASource.Height, ASource.Width);
  Result.SetSize(Size, Size);
  Result.Clear(TAlphaColorRec.Null);
  if Result.Canvas.BeginScene then
    try
      Result.Canvas.Fill.Bitmap.Bitmap.Assign(ASource);
      Result.Canvas.Fill.Kind := TBrushKind.Bitmap;
      Result.Canvas.FillEllipse(TRectF.Create(0, 0, Size, Size), 1);
    finally
      Result.Canvas.EndScene;
    end;
end;

function TFormMain.DefineRemainedDays(const ABirthday: TDate): Integer;
var
  EventDate: TDateTime;
begin
  EventDate := ChangeYear(ABirthday, CurrentYear);
  Result := DaysBetween(Date, EventDate);
  if (Result > 0) and (EventDate < Now) then
    Result := DaysInYear(EventDate) - Result - 1;
end;

// Optional rationale display routine to display permission requirement rationale to the user
procedure TFormMain.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
var
  I: Integer;
  RationaleMsg: string;
begin
  for I := 0 to Pred(Length(APermissions)) do
  begin
    if APermissions[I] = FPermissionReadContacts then
      RationaleMsg := RationaleMsg + 'The app needs to look in the address book';
    if I <> Pred(Length(APermissions)) then
      RationaleMsg := RationaleMsg + SLineBreak + SLineBreak;
  end;

  // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
  // After the user sees the explanation, invoke the post-rationale routine to request the permissions
  TDialogService.ShowMessage(RationaleMsg,
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc
    end)
end;

procedure TFormMain.FillContactsList;
begin
  FreeAndNil(FContacts);
  FContacts := TAddressBookContacts.Create(True);
  AddressBook.AllContacts(FContacts);

  if (FThread <> nil) and not FThread.Finished then
  begin
    FThread.Terminate;
    FThread.WaitFor;
  end;
  FreeAndNil(FThread);

  FThread := TFetchContactThread.Create(FContacts);
  FThread.OnContactLoaded := ContactLoaded;
  FThread.OnStart := ContactLoadingBegin;
  FThread.OnTerminate := ContactLoadingEnd;
  FThread.Start;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if (FThread <> nil) and not FThread.Finished then
  begin
    FThread.OnContactLoaded := nil;
    FThread.Terminate;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
{$IFDEF ANDROID}
  FPermissionReadContacts := JStringToString(TJManifest_permission.JavaClass.READ_CONTACTS);
{$ENDIF}
  AddressBook.RequestPermission(DisplayRationale);
end;

procedure TFormMain.ListView1PullRefresh(Sender: TObject);
begin
  ListView1.PullRefreshWait := True;
  FillContactsList;
end;

procedure TFormMain.PostNotification(const ADisplayName: string; const ABirthday: TDateTime;
  const ARemainderDays: Integer);
var
  Notification: TNotification;
  EventDate: TDateTime;
begin
  Notification := NotificationCenter1.CreateNotification;
  try
    EventDate := ABirthday;
    EventDate := RecodeTime(EventDate, 11, 0, 0, 0);
    EventDate := ChangeYear(EventDate, CurrentYear);
    if EventDate < Now then
      EventDate := ChangeYear(EventDate, CurrentYear + 1);
    Notification.RepeatInterval := TRepeatInterval.Year;
    Notification.FireDate := EventDate;
    Notification.AlertBody := Format(NotificationMessage, [ADisplayName]);
    Notification.EnableSound := True;
    NotificationCenter1.ScheduleNotification(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TFormMain.SortContacts;
begin
  ListView1.BeginUpdate;
  try
    ListView1.Items.Sort(TComparer<TListViewItem>.Construct(
      function(const Left, Right: TListViewItem): Integer
      begin
        Result := CompareValue(Left.Tag, Right.Tag);
      end));
  finally
    ListView1.EndUpdate;
  end;
end;

procedure TFormMain.SpeedButton1Click(Sender: TObject);
begin
  ListView1.SearchVisible := not ListView1.SearchVisible;
end;

{ TFetchContactThread }

constructor TFetchContactThread.Create(const AContacts: TAddressBookContacts);
begin
  inherited Create(True);
  FAllContacts := AContacts;
end;

procedure TFetchContactThread.DoContactLoaded(const ANumber: Integer; const ABirthday: TDateTime; const ADisplayName: string;
  const APhoto: TBitmapSurface);
begin
  if Assigned(FOnContactLoaded) then
    FOnContactLoaded(FAllContacts.Count, ANumber, ABirthday, ADisplayName, APhoto);
end;

procedure TFetchContactThread.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TFetchContactThread.Execute;
var
  Contact: TAddressBookContact;
  Index: Integer;
  DisplayName: string;
  Photo: TBitmapSurface;
  Birthday: TDateTime;
begin
  Synchronize(DoStart);
  Index := 0;
  for Contact in FAllContacts do
  begin
    if Terminated then
      Exit;

    DisplayName := Contact.DisplayName;
    Photo := Contact.PhotoThumbnail;
    Birthday := Contact.Birthday;
    try
      Synchronize(procedure
        begin
          DoContactLoaded(Index, Birthday, DisplayName, Photo);
        end);
    finally
      if Photo <> nil then
        Photo.Free;
    end;
    Inc(Index);
  end;
end;

end.
