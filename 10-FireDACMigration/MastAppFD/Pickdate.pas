unit Pickdate;

interface

uses Windows, Classes, Graphics, Forms, Controls, Buttons,
  SysUtils, StdCtrls, Grids, Calendar, ExtCtrls;

type
  TBrDateForm = class(TForm)
    Calendar1: TCalendar;
    OkBtn: TButton;
    CancelBtn: TButton;
    TitleLabel: TLabel;
    PrevMonthBtn: TSpeedButton;
    NextMonthBtn: TSpeedButton;
    Bevel1: TBevel;
    procedure PrevMonthBtnClick(Sender: TObject);
    procedure NextMonthBtnClick(Sender: TObject);
    procedure Calendar1Change(Sender: TObject);
  private
    procedure SetDate(Date: TDateTime);
    function GetDate: TDateTime;
  public
    property Date: TDateTime read GetDate write SetDate;
  end;

var
  BrDateForm: TBrDateForm;

implementation

{$R *.dfm}

procedure TBrDateForm.SetDate(Date: TDateTime);
begin
 Calendar1.CalendarDate := Date;
end;

function TBrDateForm.GetDate: TDateTime;
begin
  Result := Calendar1.CalendarDate;
end;
         
procedure TBrDateForm.PrevMonthBtnClick(Sender: TObject);
begin
  Calendar1.PrevMonth;
end;

procedure TBrDateForm.NextMonthBtnClick(Sender: TObject);
begin
  Calendar1.NextMonth;
end;

procedure TBrDateForm.Calendar1Change(Sender: TObject);
begin
  TitleLabel.Caption := FormatDateTime('MMMM, YYYY', Calendar1.CalendarDate);
end;

end.
