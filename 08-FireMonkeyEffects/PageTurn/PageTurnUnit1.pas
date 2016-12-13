unit PageTurnUnit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Effects,
  FMX.Filter.Effects, FMX.Objects, FMX.Layouts, FMX.Ani, Data.Bind.GenData,
  Data.Bind.Components, Data.Bind.ObjectScope, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  FMX.StdCtrls, FMX.Edit;

type
  TPageTurnSampleForm = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Swipe: TSwipeTransitionEffect;
    Memo3: TMemo;
    Memo4: TMemo;
    LayoutVisible: TLayout;
    LayoutVirtual: TLayout;
    LayoutGlass: TLayout;
    SwipePoint: TSelectionPoint;
    SwipeAnimation: TPathAnimation;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    EditLoremIpsum1: TEdit;
    LinkControlToFieldLoremIpsum1: TLinkControlToField;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LayoutGlassMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure LayoutGlassMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure LayoutGlassMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SwipeAnimationProcess(Sender: TObject);
    procedure SwipeAnimationFinish(Sender: TObject);
    procedure LayoutGlassMouseLeave(Sender: TObject);
  private
    { Private declarations }
    FCurPage: Integer;
    procedure FillMemo(memo: TMemo);
    procedure FinishTurn;
    const VisibleLines = 30;
  public
    { Public declarations }
  end;

var
  PageTurnSampleForm: TPageTurnSampleForm;

implementation

{$R *.fmx}

procedure TPageTurnSampleForm.FillMemo(memo: TMemo);
var
  i: Integer;
begin
  Memo.Text := EditLoremIpsum1.Text;
  PrototypeBindSource1.Next;
  for I := 0 to 20 do
    Memo.Text := Memo.Text + Format('Page %d ', [FCurPage]);
  Memo.Text := Memo.Text + EditLoremIpsum1.Text;
  Memo.Text := Memo.Text + EditLoremIpsum1.Text;
  PrototypeBindSource1.Next;
  Inc(FCurPage);
end;

procedure TPageTurnSampleForm.FinishTurn;
begin
  if not Swipe.Enabled then exit;
  if SwipeAnimation.Enabled then exit;

  SwipeAnimation.Path.Clear;
  SwipePoint.Position.Point := PointF(0, 0);
  SwipeAnimation.Path.MoveTo(Swipe.MousePoint);
  if Swipe.MousePoint.X <= ClientWidth - ClientWidth / 4 then
  begin
    SwipeAnimation.Path.LineTo(PointF(Swipe.MousePoint.X / 2, 0));
    SwipeAnimation.Path.LineTo(PointF(0, 0));
  end
  else
  begin
    SwipeAnimation.Path.LineTo(PointF(ClientWidth, 0));
  end;
  SwipeAnimation.Start;
end;


procedure TPageTurnSampleForm.FormCreate(Sender: TObject);
begin
  FCurPage := 1;
  Swipe.CornerPoint := PointF(LayoutGlass.Width, 0);
  Swipe.MousePoint  := PointF(LayoutGlass.Width - 5, 5);
  FillMemo(Memo1);
  FillMemo(Memo2);
  FillMemo(Memo3);
  FillMemo(Memo4);
end;

procedure TPageTurnSampleForm.FormResize(Sender: TObject);
begin
  Memo1.Width := ClientWidth / 2;
  Memo1.Height := ClientHeight;

  Memo2.Width := ClientWidth - Memo1.Width;
  Memo2.Position.X := Memo1.Width;
  Memo2.Height := ClientHeight;

  Memo4.Position := Memo2.Position;
  Memo4.Size := Memo2.Size;

  Memo3.Position := Memo1.Position;
  Memo3.Size := Memo1.Size;
end;

procedure TPageTurnSampleForm.LayoutGlassMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  vmemo: TBitmap;
begin
  Swipe.MousePoint := PointF(x,y);
  Swipe.Enabled := True;
  LayoutVirtual.BringToFront;
  vmemo := LayoutVirtual.MakeScreenshot;
  Swipe.Target.Assign(vmemo);
  vmemo.FlipHorizontal;
  Swipe.Back.Assign(vmemo);
  vmemo.Free;
  LayoutVisible.BringToFront;
end;

procedure TPageTurnSampleForm.LayoutGlassMouseLeave(Sender: TObject);
begin
  FinishTurn;
end;

procedure TPageTurnSampleForm.LayoutGlassMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if ssLeft in Shift then
  begin
    Swipe.MousePoint := (TPointF.Create(X,Y));
  end;
end;

procedure TPageTurnSampleForm.LayoutGlassMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FinishTurn;
end;

procedure TPageTurnSampleForm.SwipeAnimationFinish(Sender: TObject);
begin
  SwipeAnimation.Enabled := False;
  Swipe.Enabled := False;
  if Swipe.MousePoint.X = 0 then
  begin
    Memo1.Text := Memo3.Text;
    Memo2.Text := Memo4.Text;
    FillMemo(Memo3);
    FillMemo(Memo4);
  end;
end;

procedure TPageTurnSampleForm.SwipeAnimationProcess(Sender: TObject);
begin
  Swipe.MousePoint := SwipePoint.Position.Point;
end;

end.
