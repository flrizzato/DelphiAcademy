unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  Max = 5000000;

var
  Form1: TForm1;

implementation

uses System.Threading,
  System.Diagnostics,
  System.SyncObjs;

{$R *.fmx}

function IsPrime(N: Integer): Boolean;
var
  Test, k: Integer;
begin
  if N <= 3 then
    IsPrime := N > 1
  else if ((N mod 2) = 0) or ((N mod 3) = 0) then
    IsPrime := False
  else
  begin
    IsPrime := True;
    k := Trunc(Sqrt(N));
    Test := 5;
    while Test <= k do
    begin
      if ((N mod Test) = 0) or ((N mod (Test + 2)) = 0) then
      begin
        IsPrime := False;
        break; { jump out of the for loop }
      end;
      Test := Test + 6;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I, Tot: Integer;
  SW: TStopwatch;
begin
  Tot := 0;
  SW := TStopwatch.Create;
  SW.Start;
  for I := 1 to Max do
  begin
    if IsPrime(I) then
      Inc(Tot);
  end;
  SW.Stop;
  Memo1.Lines.Add
    (Format('Sequential For loop. Time (in milliseconds): %d - Primes found: %d',
    [SW.ElapsedMilliseconds, Tot]));
  Memo1.Lines.Add('---');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Tot: Integer;
  SW: TStopwatch;
begin
  try
    Tot := 0;
    SW := TStopwatch.Create;
    SW.Start;
    TParallel.For(2, 1, Max,
      procedure(I: Int64)
      begin
        if IsPrime(I) then
          TInterlocked.Increment(Tot);
      end);
    SW.Stop;
    Memo1.Lines.Add
      (Format('Parallel For loop. Time (in milliseconds): %d - Primes found: %d',
      [SW.ElapsedMilliseconds, Tot]));
    Memo1.Lines.Add('---');
  except
    on E: EAggregateException do
      ShowMessage(E.ToString);
  end;
end;

end.
