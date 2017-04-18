unit UnitFormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.Edit, FMX.EditBox,
  FMX.NumberBox;

type
  TFormMain = class(TForm)
    GridLayoutRectangles: TGridLayout;
    NumberBoxStride: TNumberBox;
    Label1: TLabel;
    ToolBar1: TToolBar;
    ButtonNormalFor: TButton;
    ButtonStartParallelFor: TButton;
    procedure ButtonStartParallelForClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RectangleClick(Sender: TObject);
    procedure ButtonNormalForClick(Sender: TObject);
  private
    { Private declarations }
    procedure ClearRectangles;
    procedure PaintRectangle(Index: Integer);
  public
    { Public declarations }
    FRectangles: array of TRectangle;
    FPopup: TPopup;
    FPopupRect: TRoundRect;
    FPopupLabel: TLabel;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Threading, System.UIConsts;

procedure TFormMain.ButtonNormalForClick(Sender: TObject);
var
  lFrom, lTo: Integer;
begin
  ClearRectangles;
  lFrom := Low(FRectangles);
  lTo := High(FRectangles);
  { TTask.Run executes the entire simple-for loop in parallel,
    which enables you to actually see the progress of the for-loop }
  TTask.Run(
    procedure
    var
      Index: Integer;
    begin
      for Index := lFrom to lTo do
      begin
        PaintRectangle(Index);
      end;
    end);
end;

procedure TFormMain.ButtonStartParallelForClick(Sender: TObject);
var
  lStride, lFrom, lTo: Integer;
begin
  ClearRectangles;
  lFrom := Low(FRectangles);
  lTo := High(FRectangles);
  lStride := Trunc(NumberBoxStride.Value);
  { TTask.Run executes the entire parallel for loop itself in parallel,
    which enables you to actually see the progress of the for-loop }
  TTask.Run(
    procedure
    begin
      TParallel.For(lStride, lFrom, lTo, PaintRectangle)
    end);
end;

procedure TFormMain.ClearRectangles;
var
  I: Integer;
begin
  for I := Low(FRectangles) to High(FRectangles) do
  begin
    FRectangles[I].Fill.Color := TALphaColors.Grey;
    TLabel(FRectangles[I].TagObject).Text := '';
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
  lRectangle: TRectangle;
  lLabel: TLabel;
begin
  SetLength(FRectangles, 100);
  for I := Low(FRectangles) to High(FRectangles) do
  begin
    lRectangle := TRectangle.Create(GridLayoutRectangles);
    lRectangle.Parent := GridLayoutRectangles;
    lRectangle.OnClick := RectangleClick;
    lRectangle.Tag := I;
    lLabel := TLabel.Create(lRectangle);
    lLabel.Parent := lRectangle;
    lLabel.Position.X := 2;
    lLabel.Width := 28;
    lLabel.Height := 28;
    lRectangle.TagObject := lLabel;
    FRectangles[I] := lRectangle;
  end;
  FPopup := TPopup.Create(Self);
  FPopup.Width := 75;
  FPopupRect := TRoundRect.Create(FPopup);
  FPopupRect.Parent := FPopup;
  FPopupRect.Fill.Color := TALphaColors.Yellow;
  FPopupLabel := TLabel.Create(FPopup);
  FPopupLabel.Parent := FPopup;
  FPopupLabel.Width := FPopup.Width;
  FPopupLabel.Position.X := 5;
  FPopupRect.BoundsRect := FPopupLabel.BoundsRect;
  ClearRectangles;
end;

procedure TFormMain.PaintRectangle(Index: Integer);
var
  lR, lG, lB: Byte;
  lColor: TAlphaColor;
begin
  lR := TThread.CurrentThread.ThreadID MOD High(Byte);
  lG := (2 * lR) MOD High(Byte);
  lB := (4 * lR) MOD High(Byte);
  lColor := MakeColor(lR, lG, lB, High(Byte));

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      FRectangles[Index].Fill.Color := lColor;
      TLabel(FRectangles[Index].TagObject).Text := Index.ToString;
      FRectangles[Index].TagString := TimeToStr(Now);
      FRectangles[Index].Repaint;
    end);

  Sleep(100);
end;

procedure TFormMain.RectangleClick(Sender: TObject);
begin
  FPopup.IsOpen := False;
  FPopup.PlacementTarget := (Sender as TRectangle);
  FPopupLabel.Text := (Sender as TRectangle).TagString;
  FPopup.IsOpen := True;
end;

end.
