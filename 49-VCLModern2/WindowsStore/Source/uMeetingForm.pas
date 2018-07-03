unit uMeetingForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uAbstractDataTabForm, DB, DBActns, ActnList, ImgList, Grids,
  DBGrids, StdCtrls, Buttons, ExtCtrls, ComCtrls, ToolWin, DBCtrls, Mask,
  System.Actions, Vcl.WinXCalendars, System.ImageList, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors,
  Data.Bind.Components, Data.Bind.DBScope;

type
  TMeetingForm = class(TAbstractDataTabForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label6: TLabel;
    dbeMeetingID: TDBEdit;
    dbeDuration: TDBEdit;
    dbeStartTime: TDBEdit;
    dbeRoomID: TDBEdit;
    dbeTopic: TDBMemo;
    dbeRoomName: TDBEdit;
    butTimeRoomForm: TBitBtn;
    panParticipants: TPanel;
    lblParticipants: TLabel;
    dbgSearch: TDBGrid;
    dbgParticipants: TDBGrid;
    Label7: TLabel;
    edtSearch: TEdit;
    butUserSearch: TBitBtn;
    butUserAdd: TSpeedButton;
    butUserDel: TSpeedButton;
    datSearch: TDataSource;
    datParticipants: TDataSource;
    cpkStartDate: TCalendarPicker;
    procedure FormDestroy(Sender: TObject);
    procedure butTimeRoomFormClick(Sender: TObject);
    procedure datControlStateChange(Sender: TObject);
    procedure butUserAddClick(Sender: TObject);
    procedure butUserDelClick(Sender: TObject);
    procedure datSearchDataChange(Sender: TObject; Field: TField);
    procedure datParticipantsDataChange(Sender: TObject; Field: TField);
    procedure butUserSearchClick(Sender: TObject);
    procedure cpkStartDateCloseUp(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMeetingForm: TMeetingForm;

implementation

uses DateUtils, uMeetingDM, uMeetingControl, uAbstractDataForm;

{$R *.dfm}

procedure TMeetingForm.butTimeRoomFormClick(Sender: TObject);
var
  Time: TTime;
  Room_ID: integer;
  Room_Name: string;
begin
  inherited;
  if TMeetingControl.GetInstance.CreateTimeRoomForm(Time, Room_ID, Room_Name)
  then
  begin
    datControl.DataSet.FieldByName('STARTTIME').AsDateTime := Time;
    datControl.DataSet.FieldByName('ROOM_ID').AsInteger := Room_ID;
    datControl.DataSet.FieldByName('ROOM_NAME').AsString := Room_Name;
  end;
end;

procedure TMeetingForm.butUserAddClick(Sender: TObject);
begin
  inherited;
  if datControl.State in [dsInsert, dsEdit] then
  begin
    datParticipants.DataSet.InsertRecord
      ([datControl.DataSet.FieldByName('MEETING_ID').AsInteger,
      datSearch.DataSet.FieldByName('USER_ID').AsInteger,
      datSearch.DataSet.FieldByName('USER_NAME').AsString]);
    datSearch.DataSet.Delete;
  end;
end;

procedure TMeetingForm.butUserDelClick(Sender: TObject);
begin
  inherited;
  if datControl.State in [dsInsert, dsEdit] then
  begin
    datSearch.DataSet.InsertRecord
      ([datParticipants.DataSet.FieldByName('USER_ID').AsInteger,
      datParticipants.DataSet.FieldByName('USER_NAME').AsString]);
    datParticipants.DataSet.Delete;
  end;
end;

procedure TMeetingForm.butUserSearchClick(Sender: TObject);
begin
  inherited;
  TMeetingControl.GetInstance.LocateUser(edtSearch.Text);
end;

procedure TMeetingForm.cpkStartDateCloseUp(Sender: TObject);
begin
  inherited;
  // datControl.DataSet.FieldByName('STARTDATE').AsDateTime := cpkStartDate.Date;
end;

procedure TMeetingForm.datControlStateChange(Sender: TObject);
begin
  inherited;
  butUserAdd.Enabled := (datControl.State in [dsInsert, dsEdit]) and
    not datSearch.DataSet.IsEmpty;
  butUserDel.Enabled := (datControl.State in [dsInsert, dsEdit]) and
    not datParticipants.DataSet.IsEmpty;
  butTimeRoomForm.Enabled := datControl.State in [dsInsert, dsEdit];
  cpkStartDate.Enabled := datControl.State in [dsInsert, dsEdit];
end;

procedure TMeetingForm.datParticipantsDataChange(Sender: TObject;
  Field: TField);
begin
  inherited;
  butUserDel.Enabled := (datControl.State in [dsInsert, dsEdit]) and
    not datParticipants.DataSet.IsEmpty;
  if datControl.DataSet.FieldByName('STARTDATE').AsDateTime > 0 then
    cpkStartDate.Date := datControl.DataSet.FieldByName('STARTDATE').AsDateTime;
end;

procedure TMeetingForm.datSearchDataChange(Sender: TObject; Field: TField);
begin
  inherited;
  butUserAdd.Enabled := (datControl.State in [dsInsert, dsEdit]) and
    not datSearch.DataSet.IsEmpty;
end;

procedure TMeetingForm.FormDestroy(Sender: TObject);
begin
  inherited;
  fMeetingForm := nil;
end;

end.
