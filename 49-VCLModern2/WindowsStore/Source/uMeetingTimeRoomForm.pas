unit uMeetingTimeRoomForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uAbstractForm, StdCtrls, Buttons, ExtCtrls, Grids, DBGrids,
  DB, DBClient;

type
  TMeetingTimeRoomForm = class(TAbstractForm)
    pnlBottom: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    dbgFreeTimeList: TDBGrid;
    datFreeTimeList: TDataSource;
    bevLeft: TBevel;
    dbgFreeRoomList: TDBGrid;
    datFreeRoomList: TDataSource;
    procedure datFreeTimeListDataChange(Sender: TObject; Field: TField);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Meeting_ID: integer;
    StartDate: TDateTime;
    Duration: integer;
  end;

implementation

uses uMeetingControl;

{$R *.dfm}

procedure TMeetingTimeRoomForm.datFreeTimeListDataChange(Sender: TObject;
  Field: TField);
var
  FreeTime: TTime;
begin
  inherited;
  if datFreeTimeList.DataSet <> nil then
  begin
    FreeTime := datFreeTimeList.DataSet.FieldByName('FREE_TIME').AsDateTime;
    datFreeRoomList.DataSet := TMeetingControl.GetInstance.GetFreeRoomList
      (Meeting_ID, StartDate, FreeTime, Duration);
    datFreeRoomList.DataSet.First;
  end;
end;

procedure TMeetingTimeRoomForm.FormShow(Sender: TObject);
begin
  inherited;
  datFreeTimeList.DataSet := TMeetingControl.GetInstance.GetFreeTimeList
    (Meeting_ID, StartDate, Duration);
  datFreeTimeList.DataSet.First;
end;

end.
