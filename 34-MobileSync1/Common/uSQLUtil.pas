unit uSQLUtil;

interface

type
  TSQLUtil = class
  public
    class function SQLDate(date: TDate): string; overload;
    class function SQLDate(date: string): string; overload;
    class function SQLDateTime(date: TDateTime): String; overload;
    class function SQLDateTime(date: string): string; overload;
    class function SQLString(str: string): string;
    class function SQLInteger(str: string): string;
  end;

implementation

uses
  System.DateUtils, System.SysUtils;

class function TSQLUtil.SQLDate(date: TDate): string;
const
  months: array [1..12] of string = ('JAN', 'FEB', 'MAR', 'APR', 'MAY',
    'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
var
  d, y: integer;
  m: string;
begin
  d := DayOf(date);
  y := YearOf(date);
  m := months[MonthOf(date)];
  Result := QuotedSTR(Format('%d-%s-%d', [d, m, y]));
end;

class function TSQLUtil.SQLDate(date: string): string;
const
  months: array [1 .. 12] of string = ('JAN', 'FEB', 'MAR', 'APR', 'MAY',
    'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
var
  fDate: TDateTime;
  d, y: integer;
  m: string;
begin
  fDate := StrToDateTime(date);
  d := DayOf(fDate);
  y := YearOf(fDate);
  m := months[MonthOf(fDate)];
  Result := QuotedSTR(Format('%d-%s-%d', [d, m, y]));
end;

class function TSQLUtil.SQLDateTime(date: TDateTime): String;
var
  d: String;
  h, m, s: integer;
begin
  d := SQLDate(date);
  h := HourOf(date);
  m := MinuteOf(date);
  s := SecondOf(date);
  Result := QuotedSTR(d.Replace(#39, '') + Format(' %d:%d:%d', [h, m, s]));
end;

class function TSQLUtil.SQLDateTime(date: string): String;
var
  fDate: TDateTime;
  d: String;
  h, m, s: integer;
begin
  fDate := StrToDateTime(date);
  d := SQLDate(fDate);
  h := HourOf(fDate);
  m := MinuteOf(fDate);
  s := SecondOf(fDate);
  Result := QuotedSTR(d.Replace(#39, '') + Format(' %d:%d:%d', [h, m, s]));
end;

class function TSQLUtil.SQLInteger(str: string): string;
begin
  if str.Trim.IsEmpty then
    Result := 'NULL'
  else
    Result := str.Trim;
end;

class function TSQLUtil.SQLString(str: string): string;
begin
  if str.Trim.IsEmpty then
    Result := 'NULL'
  else
    Result := QuotedSTR(str.Trim);
end;

end.
