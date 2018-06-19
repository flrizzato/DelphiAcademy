unit uFormCals;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.WinXCalendars, Vcl.WinXCtrls,
  Vcl.StdCtrls, System.Math;

type
  TForm3 = class(TForm)
    CalendarView1: TCalendarView;
    CalendarPicker1: TCalendarPicker;
    ToggleSwitch1: TToggleSwitch;
    memLog: TMemo;
    procedure CalendarView1DrawDayItem(Sender: TObject;
      DrawParams: TDrawViewInfoParams; CalendarViewViewInfo: TCellItemViewInfo);
    procedure CalendarView1DrawMonthItem(Sender: TObject;
      DrawParams: TDrawViewInfoParams; CalendarViewViewInfo: TCellItemViewInfo);
    procedure CalendarView1DrawYearItem(Sender: TObject;
      DrawParams: TDrawViewInfoParams; CalendarViewViewInfo: TCellItemViewInfo);
    procedure ToggleSwitch1Click(Sender: TObject);
    procedure CalendarView1Click(Sender: TObject);
  private
    function CurrentMonth: word;
    function CurrentYear: integer;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  System.DateUtils;

{$R *.dfm}

procedure TForm3.CalendarView1Click(Sender: TObject);
var i: integer;
begin
  memLog.Clear;
  for i := 0 to CalendarView1.SelectionCount-1 do
     memLog.Lines.Add(DateToStr(CalendarView1.SelectedDates[i]));
end;

procedure TForm3.CalendarView1DrawDayItem(Sender: TObject;
  DrawParams: TDrawViewInfoParams; CalendarViewViewInfo: TCellItemViewInfo);
var
  d: integer;
begin
  d := DayOfTheWeek(CalendarViewViewInfo.Date);
  if (d = 6) or (d = 7) then
    DrawParams.ForegroundColor := clRed;
end;

procedure TForm3.CalendarView1DrawMonthItem(Sender: TObject;
  DrawParams: TDrawViewInfoParams; CalendarViewViewInfo: TCellItemViewInfo);
begin
  if MonthOf(CalendarViewViewInfo.Date) = CurrentMonth then
    DrawParams.ForegroundColor := clRed;
end;

procedure TForm3.CalendarView1DrawYearItem(Sender: TObject;
  DrawParams: TDrawViewInfoParams; CalendarViewViewInfo: TCellItemViewInfo);
begin
  if YearOf(CalendarViewViewInfo.Date) = CurrentYear then
    DrawParams.ForegroundColor := clRed;
end;

function TForm3.CurrentMonth: word;
var
  y, m, d: word;
begin
  DecodeDate(Now, y, m, d);
  Result := m;
end;

function TForm3.CurrentYear: integer;
var
  y, m, d: word;
begin
  DecodeDate(Now, y, m, d);
  Result := y;
end;

procedure TForm3.ToggleSwitch1Click(Sender: TObject);
begin
  if ToggleSwitch1.State = TToggleSwitchState.tssOn then
    CalendarView1.SelectionMode := TSelectionMode.smMultiple
  else
    CalendarView1.SelectionMode := TSelectionMode.smSingle;
end;

end.
