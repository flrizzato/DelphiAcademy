unit uMeetingControl;

interface

uses
  Classes, Controls, uAbstractControl, uMeetingDM, DB, dbClient;

type
  TMeetingControl = class(TAbstractControl)
  strict private
    class var FInstance: TMeetingControl;

  var
    fMeetingDM: TMeetingDM;
  public
    function CheckMeetingDate: boolean;
    function CheckMeetingOwner: boolean;
    function CheckNumberOfParts: boolean;
    constructor Create(AOwner: TComponent); overload; override;
    procedure CreateDefaultForm; override;
    procedure CreateDefaultDM; override;
    function CreateTimeRoomForm(var Time: TTime; var Room_ID: integer;
      var Room_Name: string): boolean;
    destructor Destroy; override;
    function GetFreeTimeList(Meeting_ID: integer; StartDate: TDate;
      Duration: integer): TClientDataSet;
    function GetFreeRoomList(Meeting_ID: integer; StartDate: TDate;
      StartTime: TTime; Duration: integer): TClientDataSet;
    class function GetInstance: TMeetingControl;
    procedure LocateUser(UserName: string);
    procedure SendMeetingNotification(User_ID: integer; StartDate: TDateTime;
      StartTime: TTime; Duration: integer; RoomName: string; Topic: string;
      Action: integer);
    function ValidateMeetingRoom(Meeting_ID: integer; Room_ID: integer;
      StartDate: TDateTime; StartTime: TTime; Duration: integer): boolean;
    function ValidateParticipants(Meeting_ID: integer; User_ID: integer;
      StartDate: TDateTime; StartTime: TTime; Duration: integer): boolean;
  end;

implementation

uses
  Forms, SysUtils, Dialogs, DateUtils, uMeetingForm, uMeetingTimeRoomForm,
  uUserControl, uMsgControl, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.DBX.Migrate;

{ TMeetingControl }

function TMeetingControl.CheckMeetingDate: boolean;
begin
  result := (fMeetingDM.cdsControl.FieldByName('STARTDATE').AsDateTime >= Date);
end;

function TMeetingControl.CheckMeetingOwner: boolean;
begin
  result := (TUserControl.GetInstance.fUserID = fMeetingDM.cdsControl.
    FieldByName('USER_ID').AsInteger);
end;

function TMeetingControl.CheckNumberOfParts: boolean;
begin
  result := (fMeetingDM.cdsParticipants.RecordCount >= 2);
end;

constructor TMeetingControl.Create(AOwner: TComponent);
begin
  fMeetingDM := nil;
  fMeetingForm := nil;
  inherited Create(AOwner);
end;

procedure TMeetingControl.CreateDefaultForm;
begin
  inherited;
  if fMeetingForm = nil then
  begin
    fMeetingForm := TMeetingForm.Create(Self);

    fMeetingForm.datControl.DataSet := fMeetingDM.cdsControl;
    fMeetingForm.datSearch.DataSet := fMeetingDM.cdsSearchPart;
    fMeetingForm.datParticipants.DataSet := fMeetingDM.cdsParticipants;
    fMeetingForm.datControl.DataSet.Open;
  end
  else
    fMeetingForm.Show;
end;

procedure TMeetingControl.CreateDefaultDM;
begin
  inherited;
  fMeetingDM := TMeetingDM.Create(Self);
  fMeetingDM.DBConnection := DBConnection;
end;

function TMeetingControl.CreateTimeRoomForm(var Time: TTime;
  var Room_ID: integer; var Room_Name: string): boolean;
var
  fTimeRoomForm: TMeetingTimeRoomForm;
begin
  result := False;

  if fMeetingDM.cdsControl.FieldByName('STARTDATE').IsNull or
    fMeetingDM.cdsControl.FieldByName('DURATION').IsNull then
    raise Exception.Create
      ('"Date" and "Duration" are required fields for this operation!');

  if not CheckMeetingDate then
    raise Exception.Create('Retroactivity is not allowed!');

  if not CheckNumberOfParts then
    raise Exception.Create
      ('At least two participants must be selected for this operation!');

  fTimeRoomForm := TMeetingTimeRoomForm.Create(nil);
  try
    fTimeRoomForm.Meeting_ID := fMeetingDM.cdsControl.FieldByName('MEETING_ID')
      .AsInteger;
    fTimeRoomForm.StartDate := fMeetingDM.cdsControl.FieldByName('STARTDATE')
      .AsDateTime;
    fTimeRoomForm.Duration := fMeetingDM.cdsControl.FieldByName('DURATION')
      .AsInteger;
    if fTimeRoomForm.ShowModal = mrOK then
    begin
      Time := fTimeRoomForm.datFreeTimeList.DataSet.FieldByName('FREE_TIME')
        .AsDateTime;
      Room_ID := fTimeRoomForm.datFreeRoomList.DataSet.FieldByName('ROOM_ID')
        .AsInteger;
      Room_Name := fTimeRoomForm.datFreeRoomList.DataSet.FieldByName
        ('NAME').AsString;

      result := True;
    end;
  finally
    fTimeRoomForm.Free;
  end;
end;

destructor TMeetingControl.Destroy;
begin
  inherited;
end;

function TMeetingControl.GetFreeTimeList(Meeting_ID: integer; StartDate: TDate;
  Duration: integer): TClientDataSet;
var
  bFree: boolean;
  sUsers: string;
  sqlMeeting, sqlTimeSlot: TFDQuery;
  MeetingStart, MeetingEnd: TDateTime;
begin
  result := TClientDataSet.Create(Self);
  result.FieldDefs.Add('FREE_TIME', ftTime);
  result.CreateDataSet;
  result.Fields[0].DisplayLabel := 'Meeting Time';

  sqlTimeSlot := TFDQuery.Create(nil);
  sqlTimeSlot.Connection := DBConnection;
  sqlMeeting := TFDQuery.Create(nil);
  sqlMeeting.Connection := DBConnection;
  try
    try
      sUsers := '';
      fMeetingDM.cdsParticipants.First;
      while not fMeetingDM.cdsParticipants.Eof do
      begin
        if sUsers <> '' then
          sUsers := sUsers + ', ';
        sUsers := sUsers + fMeetingDM.cdsParticipants.FieldByName
          ('USER_ID').AsString;
        fMeetingDM.cdsParticipants.Next;
      end;

      sqlMeeting.SQL.Add('SELECT PAR.USER_ID,');
      sqlMeeting.SQL.Add('       MEE.STARTTIME AS MEETING_START,');
      sqlMeeting.SQL.Add
        ('       MEE.STARTTIME+(MEE.DURATION*60) AS MEETING_END');
      sqlMeeting.SQL.Add('  FROM MEETING MEE');
      sqlMeeting.SQL.Add
        (' INNER JOIN PARTICIPANT PAR ON (MEE.MEETING_ID = PAR.MEETING_ID)');
      sqlMeeting.SQL.Add(' WHERE MEE.MEETING_ID <> ' + IntToStr(Meeting_ID));
      sqlMeeting.SQL.Add('   AND MEE.STARTDATE = ' +
        DataFormat(StartDate, 'D'));
      sqlMeeting.SQL.Add('   AND PAR.USER_ID IN (' + sUsers + ')');

      sqlTimeSlot.SQL.Add('  SELECT TIM.MEETINGTIME');
      sqlTimeSlot.SQL.Add('    FROM TIMESLOTS TIM');
      sqlTimeSlot.SQL.Add('ORDER BY TIM.MEETINGTIME');

      sqlTimeSlot.Open;
      While not sqlTimeSlot.Eof do
      begin
        MeetingStart := sqlTimeSlot.FieldByName('MEETINGTIME').AsDateTime;
        MeetingEnd := IncMinute(MeetingStart, Duration);

        bFree := True;
        sqlMeeting.Open;
        While not sqlMeeting.Eof and bFree do
        begin
          bFree := not(((MeetingStart >= sqlMeeting.FieldByName('MEETING_START')
            .AsDateTime) and (MeetingStart <= sqlMeeting.FieldByName
            ('MEETING_END').AsDateTime)) or
            ((MeetingEnd >= sqlMeeting.FieldByName('MEETING_START').AsDateTime)
            and (MeetingEnd <= sqlMeeting.FieldByName('MEETING_END')
            .AsDateTime)));
          sqlMeeting.Next;
        end;
        sqlMeeting.Close;

        if bFree then
          result.InsertRecord([MeetingStart]);

        sqlTimeSlot.Next;
      end;
    except
      on E: Exception do
      begin
        result.EmptyDataSet;
        raise Exception.Create(MO_ExceptionMsg + E.Message);
      end;
    end;
  finally
    sqlMeeting.Free;
    sqlTimeSlot.Free;
  end;
end;

function TMeetingControl.GetFreeRoomList(Meeting_ID: integer; StartDate: TDate;
  StartTime: TTime; Duration: integer): TClientDataSet;
var
  bFree: boolean;
  sqlMeeting, sqlRoomList: TFDQuery;
  MeetingStart, MeetingEnd: TDateTime;
begin
  result := TClientDataSet.Create(Self);
  result.FieldDefs.Add('ROOM_ID', ftInteger);
  result.FieldDefs.Add('NAME', ftString, 30);
  result.FieldDefs.Add('LOCATION', ftString, 30);
  result.FieldDefs.Add('CAPACITY', ftInteger);
  result.CreateDataSet;
  result.Fields[0].DisplayLabel := 'Room ID';
  result.Fields[1].DisplayLabel := 'Room Name';
  result.Fields[2].DisplayLabel := 'Location';
  result.Fields[3].DisplayLabel := 'Capacity';

  sqlMeeting := TFDQuery.Create(nil);
  sqlMeeting.Connection := DBConnection;
  sqlRoomList := TFDQuery.Create(nil);
  sqlRoomList.Connection := DBConnection;
  try
    try
      MeetingStart := StartTime;
      MeetingEnd := IncMinute(MeetingStart, Duration);

      sqlRoomList.SQL.Add('  SELECT ROO.ROOM_ID,');
      sqlRoomList.SQL.Add('         ROO.NAME,');
      sqlRoomList.SQL.Add('         ROO.LOCATION,');
      sqlRoomList.SQL.Add('         ROO.CAPACITY');
      sqlRoomList.SQL.Add('    FROM ROOM ROO');
      sqlRoomList.SQL.Add('ORDER BY ROO.ROOM_ID');

      sqlRoomList.Open;
      while not sqlRoomList.Eof do
      begin
        sqlMeeting.SQL.Clear;
        sqlMeeting.SQL.Add('SELECT MEE.STARTTIME AS MEETING_START,');
        sqlMeeting.SQL.Add
          ('       MEE.STARTTIME+(MEE.DURATION*60) AS MEETING_END');
        sqlMeeting.SQL.Add('  FROM MEETING MEE');
        sqlMeeting.SQL.Add(' WHERE MEE.MEETING_ID <> ' + IntToStr(Meeting_ID));
        sqlMeeting.SQL.Add('   AND MEE.ROOM_ID = ' + sqlRoomList.FieldByName
          ('ROOM_ID').AsString);
        sqlMeeting.SQL.Add('   AND MEE.STARTDATE = ' +
          DataFormat(StartDate, 'D'));

        bFree := True;
        sqlMeeting.Open;
        While not sqlMeeting.Eof and bFree do
        begin
          bFree := not(((MeetingStart >= sqlMeeting.FieldByName('MEETING_START')
            .AsDateTime) and (MeetingStart <= sqlMeeting.FieldByName
            ('MEETING_END').AsDateTime)) or
            ((MeetingEnd >= sqlMeeting.FieldByName('MEETING_START').AsDateTime)
            and (MeetingEnd <= sqlMeeting.FieldByName('MEETING_END')
            .AsDateTime)));
          sqlMeeting.Next;
        end;

        if bFree then
          result.InsertRecord([sqlRoomList.FieldByName('ROOM_ID').AsInteger,
            sqlRoomList.FieldByName('NAME').AsString,
            sqlRoomList.FieldByName('LOCATION').AsString,
            sqlRoomList.FieldByName('CAPACITY').AsInteger]);

        sqlRoomList.Next;
      end;
    except
      on E: Exception do
      begin
        result.EmptyDataSet;
        raise Exception.Create(MO_ExceptionMsg + E.Message);
      end;
    end;
  finally
    sqlMeeting.Free;
  end;
end;

class function TMeetingControl.GetInstance: TMeetingControl;
begin
  if FInstance = nil then
  begin
    FInstance := uMeetingControl.TMeetingControl.Create(Application);
  end;
  result := FInstance;
end;

procedure TMeetingControl.LocateUser(UserName: string);
begin
  if UserName <> '' then
    fMeetingDM.cdsControl.Locate('USER_NAME', UserName,
      [loPartialKey, loCaseInsensitive]);
end;

procedure TMeetingControl.SendMeetingNotification(User_ID: integer;
  StartDate: TDateTime; StartTime: TTime; Duration: integer; RoomName: string;
  Topic: string; Action: integer);
var
  sUserMsg: string;
  sUserMail: string;
begin
  case Action of
    0:
      sUserMsg := 'Your Meeting has been modified!' + #13;
    1:
      sUserMsg := 'You have a new Meeting!' + #13;
    2:
      sUserMsg := 'Your Meeting has been canceled!' + #13;
  end;
  sUserMsg := sUserMsg + 'Date: ' + DateToStr(StartDate) + #13 + 'Hour: ' +
    TimeToStr(StartTime) + #13 + 'Duration: ' + IntToStr(Duration) + #13 +
    'Room: ' + RoomName + #13 + 'Topic: ' + Topic;

  sUserMail := TUserControl.GetInstance.GetUserMail(User_ID);
  TMsgControl.GetInstance.PostNotification(sUserMail, sUserMsg);
end;

function TMeetingControl.ValidateMeetingRoom(Meeting_ID: integer;
  Room_ID: integer; StartDate: TDateTime; StartTime: TTime;
  Duration: integer): boolean;
var
  sqlMeetingRoom: TFDQuery;
  MeetingStart, MeetingEnd: TDateTime;
begin
  MeetingStart := StartTime;
  MeetingEnd := IncMinute(MeetingStart, Duration);

  sqlMeetingRoom := TFDQuery.Create(nil);
  sqlMeetingRoom.Connection := DBConnection;
  try
    try
      sqlMeetingRoom.SQL.Add('SELECT MEE.STARTTIME AS MEETING_START,');
      sqlMeetingRoom.SQL.Add
        ('       MEE.STARTTIME+(MEE.DURATION*60) AS MEETING_END');
      sqlMeetingRoom.SQL.Add('  FROM MEETING MEE');
      sqlMeetingRoom.SQL.Add(' WHERE MEE.ROOM_ID = ' + IntToStr(Room_ID));
      sqlMeetingRoom.SQL.Add('   AND MEE.MEETING_ID <> ' +
        IntToStr(Meeting_ID));
      sqlMeetingRoom.SQL.Add('   AND MEE.STARTDATE = ' +
        DataFormat(StartDate, 'D'));

      result := True;
      sqlMeetingRoom.Open;
      While not sqlMeetingRoom.Eof do
      begin
        result := not
          (((MeetingStart >= sqlMeetingRoom.FieldByName('MEETING_START')
          .AsDateTime) and (MeetingStart <= sqlMeetingRoom.FieldByName
          ('MEETING_END').AsDateTime)) or
          ((MeetingEnd >= sqlMeetingRoom.FieldByName('MEETING_START')
          .AsDateTime) and (MeetingEnd <= sqlMeetingRoom.FieldByName
          ('MEETING_END').AsDateTime)));
        sqlMeetingRoom.Next;
      end;
    except
      on E: Exception do
        raise Exception.Create(MO_ExceptionMsg + E.Message);
    end;
  finally
    sqlMeetingRoom.Free;
  end;
end;

function TMeetingControl.ValidateParticipants(Meeting_ID: integer;
  User_ID: integer; StartDate: TDateTime; StartTime: TTime;
  Duration: integer): boolean;
var
  sqlMeetingUser: TFDQuery;
  MeetingStart, MeetingEnd: TDateTime;
begin
  MeetingStart := StartTime;
  MeetingEnd := IncMinute(MeetingStart, Duration);

  sqlMeetingUser := TFDQuery.Create(nil);
  sqlMeetingUser.Connection := DBConnection;
  try
    try
      sqlMeetingUser.SQL.Add('SELECT MEE.STARTTIME AS MEETING_START,');
      sqlMeetingUser.SQL.Add
        ('       MEE.STARTTIME+(MEE.DURATION*60) AS MEETING_END');
      sqlMeetingUser.SQL.Add('  FROM MEETING MEE');
      sqlMeetingUser.SQL.Add
        (' INNER JOIN PARTICIPANT PAR ON (MEE.MEETING_ID = PAR.MEETING_ID)');
      sqlMeetingUser.SQL.Add(' WHERE MEE.MEETING_ID <> ' +
        IntToStr(Meeting_ID));
      sqlMeetingUser.SQL.Add('   AND MEE.STARTDATE = ' +
        DataFormat(StartDate, 'D'));
      sqlMeetingUser.SQL.Add('   AND PAR.USER_ID = ' + IntToStr(User_ID));

      result := True;
      sqlMeetingUser.Open;
      While not sqlMeetingUser.Eof do
      begin
        result := not
          (((MeetingStart >= sqlMeetingUser.FieldByName('MEETING_START')
          .AsDateTime) and (MeetingStart <= sqlMeetingUser.FieldByName
          ('MEETING_END').AsDateTime)) or
          ((MeetingEnd >= sqlMeetingUser.FieldByName('MEETING_START')
          .AsDateTime) and (MeetingEnd <= sqlMeetingUser.FieldByName
          ('MEETING_END').AsDateTime)));
        sqlMeetingUser.Next;
      end;
    except
      on E: Exception do
        raise Exception.Create(MO_ExceptionMsg + E.Message);
    end;
  finally
    sqlMeetingUser.Free;
  end;
end;

end.
