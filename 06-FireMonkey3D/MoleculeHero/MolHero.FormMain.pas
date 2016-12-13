unit MolHero.FormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Types3D, FMX.Objects3D,
  FMX.Edit, FMX.Materials, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.Ani, FMX.MaterialSources, FMX.Controls3D,
  FMX.Viewport3D, FMX.StdCtrls, System.Math.Vectors, FMX.Controls.Presentation,
  FMX.Layouts, FMX.MultiView, FMX.Objects, FMX.ListView.Types, FMX.ListView,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  MolHero.Types, MolHero.Materials, MolHero.MoleculeModel;

const
  CAMERA_MAX_Z = -2;
  CAMERA_MIN_Z = -102;
  ZOOM_STEP = 2;

type
  TFormMain = class(TForm)
    ViewportMain: TViewport3D;
    CameraZ: TCamera;
    Light1: TLight;
    DummyMain: TDummy;
    DummyXY: TDummy;
    DummyModel: TDummy;
    LayoutHeader: TLayout;
    SpeedButtonMenu: TSpeedButton;
    LabelTitle: TLabel;
    LayoutClient: TLayout;
    MultiViewMain: TMultiView;
    RectangleMenu: TRectangle;
    LayoutMenuTop: TLayout;
    RectangleMenuTop: TRectangle;
    LabelMenuTitle: TLabel;
    ListViewMoleculeList: TListView;
    LayoutBottom: TLayout;
    SpeedButtonViewModeAB: TSpeedButton;
    SpeedButtonViewModeA: TSpeedButton;
    SpeedButtonViewModeB: TSpeedButton;
    SpeedButtonZoomOut: TSpeedButton;
    SpeedButtonZoomIn: TSpeedButton;
    SpeedButtonInfo: TSpeedButton;
    procedure ViewportMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure ViewportMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure ViewportMainMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ListViewMoleculeListItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure SpeedButtonViewModeABClick(Sender: TObject);
    procedure SpeedButtonViewModeAClick(Sender: TObject);
    procedure SpeedButtonViewModeBClick(Sender: TObject);
    procedure SpeedButtonZoomInClick(Sender: TObject);
    procedure SpeedButtonZoomOutClick(Sender: TObject);
    procedure SpeedButtonInfoClick(Sender: TObject);
  private
    FDefaultMoleculeIndex: integer;
    FDown: TPointF;
    FCurrViewMode: TMoleculeViewMode;
    procedure LoadDefaultMolecule;
    procedure LoadMoleculeList;
    procedure LoadMoleculeByIndex(index: integer);
    procedure DoZoom(aIn: boolean);
    procedure ShowAppAbout;
    procedure SetCurrViewMode(const Value: TMoleculeViewMode);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses MolHero.DMMain, MolHero.FormAbout;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FDefaultMoleculeIndex := 3;

  MultiViewMain.Mode := TMultiViewMode.Drawer;

  SetCurrViewMode(TMoleculeViewMode.AB);
  SpeedButtonViewModeAB.IsPressed := True;

  LoadMoleculeList;
  LoadDefaultMolecule;
end;

procedure TFormMain.DoZoom(aIn: boolean);
var newZ: single;
begin
  if aIn then
    newZ := CameraZ.Position.Z + ZOOM_STEP
  else
    newZ := CameraZ.Position.Z - ZOOM_STEP;

  if (newZ < CAMERA_MAX_Z) and (newZ > CAMERA_MIN_Z) then
    CameraZ.Position.Z := newZ;
end;

procedure TFormMain.ListViewMoleculeListItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  LoadMoleculeByIndex(aItem.Tag);
  SetCurrViewMode(FCurrViewMode);
  MultiViewMain.HideMaster;
end;

procedure TFormMain.LoadMoleculeByIndex(index: integer);
begin
  DMMain.LoadMoleculeByIndex(DummyModel, index);

  if DMMain.CurrentMolecule <> nil then
    LabelTitle.Text := DMMain.CurrentMolecule.DisplayName
  else
    LabelTitle.Text := '';

  Invalidate;
end;

procedure TFormMain.LoadDefaultMolecule;
begin
  LoadMoleculeByIndex(FDefaultMoleculeIndex);
end;

procedure TFormMain.LoadMoleculeList;
var i, aCount: integer; s: string; lvi: TListViewItem;
begin
  aCount := DMMain.GetMoleculeCount;
  for i := 0 to aCount-1 do
  begin
    s := DMMain.GetMoleculeDisplayName(i);
    lvi := ListViewMoleculeList.Items.Add;
    lvi.Text := s;
    lvi.Tag := i;
  end;
end;

procedure TFormMain.SetCurrViewMode(const Value: TMoleculeViewMode);
var i: integer; sph: TSphere; cyl: TCylinder;
begin
  FCurrViewMode := Value;

  for i := 0 to DummyModel.ChildrenCount-1 do
  begin
    if DummyModel.Children[i] is TSphere then
    begin
      sph := TSphere(DummyModel.Children[i]);
      sph.Visible := FCurrViewMode in [TMoleculeViewMode.AB, TMoleculeViewMode.A];
    end

    else if DummyModel.Children[i] is TCylinder then
    begin
      cyl := TCylinder(DummyModel.Children[i]);
      cyl.Visible := FCurrViewMode in [TMoleculeViewMode.AB, TMoleculeViewMode.B];
    end
  end;
end;

procedure TFormMain.ShowAppAbout;
begin
  if FormAbout = nil then
    FormAbout := TFormAbout.Create(Application);

  FormAbout.Show;
  FormAbout.DoIntroAnimation
end;

procedure TFormMain.SpeedButtonInfoClick(Sender: TObject);
begin
  ShowAppAbout;
end;

procedure TFormMain.SpeedButtonViewModeABClick(Sender: TObject);
begin
  FCurrViewMode := TMoleculeViewMode.AB;
  SetCurrViewMode(FCurrViewMode);
end;

procedure TFormMain.SpeedButtonViewModeAClick(Sender: TObject);
begin
  FCurrViewMode := TMoleculeViewMode.A;
  SetCurrViewMode(FCurrViewMode);
end;

procedure TFormMain.SpeedButtonViewModeBClick(Sender: TObject);
begin
  FCurrViewMode := TMoleculeViewMode.B;
  SetCurrViewMode(FCurrViewMode);
end;

procedure TFormMain.SpeedButtonZoomInClick(Sender: TObject);
begin
  DoZoom(True);
end;

procedure TFormMain.SpeedButtonZoomOutClick(Sender: TObject);
begin
  DoZoom(False);
end;

procedure TFormMain.ViewportMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FDown := PointF(X, Y);
end;

procedure TFormMain.ViewportMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  if (ssLeft in Shift) then
  begin
    DummyXY.RotationAngle.X := DummyXY.RotationAngle.X - ((Y - FDown.Y) * 0.3);
    DummyXY.RotationAngle.Y := DummyXY.RotationAngle.Y + ((X - FDown.X) * 0.3);
    FDown := PointF(X, Y);
  end;
end;

procedure TFormMain.ViewportMainMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  DoZoom(WheelDelta > 0);
end;

end.
