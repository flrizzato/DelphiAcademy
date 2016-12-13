//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit ctrlsdemofrm;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UIConsts,
  FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Types, FMX.Layouts, FMX.Controls, FMX.Styles, FMX.Ani,
  FMX.Edit, FMX.ListBox, FMX.TabControl, FMX.ExtCtrls, FMX.TreeView, FMX.Effects, FMX.Viewport3D,
  FMX.Memo, FMX.Colors, FMX.Menus, FMX.Layers3D, FMX.Types3D, FMX.Controls3D, FMX.StdCtrls,
  FMX.DateTimeCtrls, FMX.ComboTrackBar, FMX.ComboEdit, FMX.SpinBox, FMX.Calendar, FMX.EditBox, FMX.NumberBox,
  FMX.Controls.Presentation, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, FMX.ScrollBox;

type

  { TfrmCtrlsDemo }

  TfrmCtrlsDemo = class(TForm)
    ControlRoot: TLayout;
    btn3DBack: TButton;
    Text2: TLabel;
    Text3: TLabel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBox1: TScrollBox;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    StringComboBox1: TComboBox;
    TabItem3: TTabItem;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    Image1: TImage;
    Path1: TPath;
    Rectangle1: TRectangle;
    Text6: TLabel;
    Ellipse1: TEllipse;
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    TreeViewItem8: TTreeViewItem;
    TreeViewItem9: TTreeViewItem;
    TreeViewItem10: TTreeViewItem;
    TreeViewItem11: TTreeViewItem;
    TreeViewItem12: TTreeViewItem;
    TreeViewItem13: TTreeViewItem;
    TreeViewItem14: TTreeViewItem;
    TreeViewItem15: TTreeViewItem;
    TreeViewItem16: TTreeViewItem;
    TreeViewItem17: TTreeViewItem;
    TreeViewItem18: TTreeViewItem;
    TreeViewItem19: TTreeViewItem;
    TreeViewItem20: TTreeViewItem;
    TreeViewItem21: TTreeViewItem;
    TreeViewItem22: TTreeViewItem;
    TreeViewItem23: TTreeViewItem;
    TreeViewItem24: TTreeViewItem;
    TreeViewItem25: TTreeViewItem;
    TreeViewItem26: TTreeViewItem;
    TreeViewItem27: TTreeViewItem;
    TreeViewItem28: TTreeViewItem;
    TreeViewItem29: TTreeViewItem;
    TreeViewItem30: TTreeViewItem;
    TreeViewItem31: TTreeViewItem;
    TreeViewItem32: TTreeViewItem;
    TreeViewItem33: TTreeViewItem;
    TreeViewItem34: TTreeViewItem;
    TreeViewItem35: TTreeViewItem;
    TreeViewItem36: TTreeViewItem;
    TreeViewItem37: TTreeViewItem;
    TabItem4: TTabItem;
    Expander1: TExpander;
    Label8: TLabel;
    Button3: TButton;
    GroupBox1: TGroupBox;
    AniIndicator1: TAniIndicator;
    Button4: TButton;
    Button5: TButton;
    Label9: TLabel;
    Image3: TImage;
    Label10: TLabel;
    NumberBox1: TNumberBox;
    TrackBar1: TTrackBar;
    Label12: TLabel;
    Button6: TButton;
    ListBox1: TListBox;
    GlowEffect2: TGlowEffect;
    Label13: TLabel;
    Label14: TLabel;
    AngleButton1: TArcDial;
    Label16: TLabel;
    AngleButton2: TArcDial;
    AngleButton3: TArcDial;
    PopupBox1: TPopupBox;
    TextBox3: TEdit;
    Rectangle2: TPanel;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Label19: TLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    StatusBar1: TStatusBar;
    Label20: TLabel;
    Panel2: TPanel;
    TabItem5: TTabItem;
    DropTarget1: TDropTarget;
    TabItem6: TTabItem;
    StringListBox1: TListBox;
    ListTransform: TListBox;
    TrackBar2: TTrackBar;
    Label21: TLabel;
    Label22: TLabel;
    TrackBar3: TTrackBar;
    Ellipse2: TEllipse;
    TextBox1: TEdit;
    TextBox4: TEdit;
    TabItem7: TTabItem;
    CornerButton1: TCornerButton;
    TrackBar4: TTrackBar;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CornerButton2: TCornerButton;
    CornerButton3: TCornerButton;
    CornerButton4: TCornerButton;
    CornerButton5: TCornerButton;
    CornerButton6: TCornerButton;
    Path2: TPath;
    Path3: TPath;
    Label23: TLabel;
    VertScrollBox1: TVertScrollBox;
    Button7: TButton;
    TrackBar5: TTrackBar;
    TextBox5: TEdit;
    Expander2: TExpander;
    Expander3: TExpander;
    Expander4: TExpander;
    TabItem8: TTabItem;
    Memo2: TMemo;
    SpinBox1: TSpinBox;
    Label26: TLabel;
    SmallScrollBar1: TSmallScrollBar;
    CheckBox2: TCheckBox;
    Label27: TLabel;
    ComboTrackBar1: TComboTrackBar;
    AlphaTrackBar1: TAlphaTrackBar;
    BWTrackBar1: TBWTrackBar;
    HueTrackBar1: THueTrackBar;
    Label28: TLabel;
    ComboColorBox1: TComboColorBox;
    CalloutPanel1: TCalloutPanel;
    Label29: TLabel;
    calloutTop: TRadioButton;
    calloutLeft: TRadioButton;
    calloutBottom: TRadioButton;
    calloutRight: TRadioButton;
    Calendar1: TCalendar;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    MenuItem7: TMenuItem;
    TrackBar6: TTrackBar;
    GlowEffect1: TGlowEffect;
    CheckBox8: TCheckBox;
    Timer1: TTimer;
    ALabel1: TLabel;
    ALabel2: TLabel;
    ALabel3: TLabel;
    sbLabel1: TLabel;
    DateEdit1: TDateEdit;
    TreeView2: TTreeView;
    TreeViewItem38: TTreeViewItem;
    TreeViewItem39: TTreeViewItem;
    TreeViewItem40: TTreeViewItem;
    TreeViewItem41: TTreeViewItem;
    TreeViewItem42: TTreeViewItem;
    TreeViewItem43: TTreeViewItem;
    TreeViewItem44: TTreeViewItem;
    TreeViewItem45: TTreeViewItem;
    TreeViewItem46: TTreeViewItem;
    TreeViewItem47: TTreeViewItem;
    TreeViewItem48: TTreeViewItem;
    TreeViewItem49: TTreeViewItem;
    TreeViewItem50: TTreeViewItem;
    ListBox2: TListBox;
    ListBoxItem5: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    ListBoxItem17: TListBoxItem;
    ListBoxItem18: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxItem21: TListBoxItem;
    ListBoxItem22: TListBoxItem;
    ListBoxItem23: TListBoxItem;
    ListBoxItem24: TListBoxItem;
    ListBoxItem25: TListBoxItem;
    ListBoxItem26: TListBoxItem;
    ListBoxItem27: TListBoxItem;
    Text1: TLabel;
    ScaleTrack: TTrackBar;
    TextScale: TLabel;
    Rectangle3: TRectangle;
    Circle1: TCircle;
    Button2: TButton;
    Memo1: TMemo;
    BindingsList1: TBindingsList;
    LinkControlToPropertyValue: TLinkControlToProperty;
    ListBoxItem29: TListBoxItem;
    ListBoxItem30: TListBoxItem;
    ListBoxItem31: TListBoxItem;
    ListBoxItem32: TListBoxItem;
    ListBoxItem33: TListBoxItem;
    ListBoxItem34: TListBoxItem;
    ListBoxItem35: TListBoxItem;
    ListBoxItem36: TListBoxItem;
    ListBoxItem37: TListBoxItem;
    ListBoxItem38: TListBoxItem;
    ListBoxItem39: TListBoxItem;
    ListBoxItem40: TListBoxItem;
    ListBoxItem41: TListBoxItem;
    ListBoxItem42: TListBoxItem;
    ListBoxItem43: TListBoxItem;
    ListBoxItem44: TListBoxItem;
    ListBoxItem45: TListBoxItem;
    ListBoxItem46: TListBoxItem;
    ListBoxItem47: TListBoxItem;
    ListBoxItem48: TListBoxItem;
    ListBoxItem49: TListBoxItem;
    ListBoxItem50: TListBoxItem;
    ListBoxItem51: TListBoxItem;
    ListBoxItem52: TListBoxItem;
    ListBoxItem53: TListBoxItem;
    ListBoxItem54: TListBoxItem;
    ListBoxItem55: TListBoxItem;
    ListBoxItem56: TListBoxItem;
    ListBoxItem57: TListBoxItem;
    ListBoxItem58: TListBoxItem;
    ListBoxItem59: TListBoxItem;
    ListBoxItem60: TListBoxItem;
    ListBoxItem61: TListBoxItem;
    ListBoxItem62: TListBoxItem;
    ListBoxItem63: TListBoxItem;
    ListBoxItem64: TListBoxItem;
    ListBoxItem65: TListBoxItem;
    ListBoxItem66: TListBoxItem;
    ListBoxItem67: TListBoxItem;
    ListBoxItem68: TListBoxItem;
    ListBoxItem69: TListBoxItem;
    ListBoxItem70: TListBoxItem;
    ListBoxItem71: TListBoxItem;
    ListBoxItem72: TListBoxItem;
    ButtonE3: TButton;
    TrackBarE3: TTrackBar;
    EditE3: TEdit;
    ButtonE4: TButton;
    TrackBarE4: TTrackBar;
    EditE4: TEdit;
    Edit1: TEdit;
    procedure AngleButton1Change(Sender: TObject);
    procedure AngleButton2Change(Sender: TObject);
    procedure AngleButton3Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure cornerListChange(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure calloutBottomChange(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure btn3DBackClick(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox8Change(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure DropTarget1DragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure ScaleTrackChange(Sender: TObject);
    procedure DropTarget1Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
  private
    { Private declarations }
    FViewport: TViewport3D;
    FContainer: TLayer3D;
    procedure SwitchTo3D;
    procedure SwitchTo2D;
  public
    { Public declarations }
  end;

var
  frmCtrlsDemo: TfrmCtrlsDemo;

implementation

uses System.Math, aboutboxfrm;

{$R *.fmx}

procedure TfrmCtrlsDemo.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 50 do
    with TRectangle.Create(Self) do
    begin
      parent := ScrollBox1;
      width := (30 + random(150));
      height := (30 + random(150));
      hittest := false;
      Position.x := random(1600);
      Position.y := random(1600);
      XRadius := random(20);
      YRadius := XRadius;
      {$R-}
      fill.Color := ((50 + random(205)) shl 24) or random($FFFFFF);
      {$R+}
    end;
  AngleButton2.Value := 3;
end;

procedure TfrmCtrlsDemo.MenuItem3Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmCtrlsDemo.MenuItem6Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Application);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

procedure TfrmCtrlsDemo.MenuItem7Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    TStyleManager.SetStyle(TStyleStreaming.LoadFromFile(OpenDialog1.FileName));
end;

procedure TfrmCtrlsDemo.AngleButton1Change(Sender: TObject);
begin
  ALabel1.Text := FloatToStr(RoundTo(AngleButton1.Value, -2));
end;

procedure TfrmCtrlsDemo.AngleButton2Change(Sender: TObject);
begin
  ALabel2.Text := FloatToStr(RoundTo(AngleButton2.Value, -2));
end;

procedure TfrmCtrlsDemo.AngleButton3Change(Sender: TObject);
begin
  ALabel3.Text := FloatToStr(RoundTo(AngleButton3.Value, -2));
end;

procedure TfrmCtrlsDemo.ScaleTrackChange(Sender: TObject);
begin
  ControlRoot.Scale.X := ScaleTrack.Value;
  ControlRoot.Scale.Y := ScaleTrack.Value;
  TextScale.Text := IntToStr(Round(ScaleTrack.Value * 100)) + '%';
end;

procedure TfrmCtrlsDemo.ScrollBar1Change(Sender: TObject);
begin
  sbLabel1.Text := FloatToStr(ScrollBar1.Value);
end;

procedure TfrmCtrlsDemo.SwitchTo3D;
var
  LImg: TImage;
begin
  { Create 3D viewport and layer }
  FViewport := TViewport3D.Create(Self);
  FViewport.Parent := Self;
  FViewport.Align := TAlignLayout.Client;
  FViewport.Color := claNull;
  FContainer := TLayer3D.Create(Self);
  FContainer.Parent := FViewport;
  FContainer.Projection := TProjection.Screen;
  FContainer.Align := TAlignLayout.Client;
  LImg := TImage.Create(Self);
  LImg.Align := TAlignLayout.Client;
  LImg.Bitmap.Assign(ControlRoot.MakeScreenshot);
  LImg.Margins := ControlRoot.Margins;
  LImg.Parent := FContainer;
  ControlRoot.Visible := False;
end;

procedure TfrmCtrlsDemo.SwitchTo2D;
begin
  { Free 3D }
  FreeAndNil(FViewport);
  ControlRoot.Visible := True;
end;

procedure TfrmCtrlsDemo.btn3DBackClick(Sender: TObject);
begin
  TButton(Sender).Enabled := false;
  SwitchTo3D;
  if Assigned(FContainer) then
  begin
    TAnimator.AnimateFloat(FContainer, 'Position.Z', 500, 1);
    TAnimator.AnimateFloatDelay(FContainer, 'Position.Z', 0, 1, 1);
    TAnimator.AnimateFloatWait(FContainer, 'RotationAngle.X', 360, 2, TAnimationType.InOut, TInterpolationType.Back);
  end;
  SwitchTo2D;
  TButton(Sender).Enabled := true;
end;

procedure TfrmCtrlsDemo.Button4Click(Sender: TObject);
begin
  MenuItem6Click(self);
end;

procedure TfrmCtrlsDemo.TrackBar2Change(Sender: TObject);
begin
  ListTransform.RotationAngle := TrackBar2.Value;
  TextBox4.RotationAngle := TrackBar2.Value;
end;

procedure TfrmCtrlsDemo.TrackBar3Change(Sender: TObject);
begin
  ListTransform.Opacity := TrackBar3.Value;
  TextBox4.Opacity := TrackBar3.Value;
end;

procedure TfrmCtrlsDemo.TrackBar4Change(Sender: TObject);
begin
  CornerButton1.XRadius := TrackBar4.Value;
  CornerButton1.YRadius := TrackBar4.Value;

  CornerButton2.XRadius := TrackBar4.Value;;
  CornerButton2.YRadius := TrackBar4.Value;;
  CornerButton3.XRadius := TrackBar4.Value;;
  CornerButton3.YRadius := TrackBar4.Value;;
  CornerButton4.XRadius := TrackBar4.Value;;
  CornerButton4.YRadius := TrackBar4.Value;;

  CornerButton5.XRadius := TrackBar4.Value;;
  CornerButton5.YRadius := TrackBar4.Value;;
  CornerButton6.XRadius := TrackBar4.Value;;
  CornerButton6.YRadius := TrackBar4.Value;;
end;

procedure TfrmCtrlsDemo.CheckBox3Change(Sender: TObject);
begin
  if CheckBox3.IsChecked then
    CornerButton1.Corners := CornerButton1.Corners + [TCorner.BottomRight]
  else
    CornerButton1.Corners := CornerButton1.Corners - [TCorner.BottomRight]
end;

procedure TfrmCtrlsDemo.CheckBox4Change(Sender: TObject);
begin
  if CheckBox4.IsChecked then
    CornerButton1.Corners := CornerButton1.Corners + [TCorner.TopRight]
  else
    CornerButton1.Corners := CornerButton1.Corners - [TCorner.TopRight]
end;

procedure TfrmCtrlsDemo.CheckBox5Change(Sender: TObject);
begin
  if CheckBox5.IsChecked then
    CornerButton1.Corners := CornerButton1.Corners + [TCorner.BottomLeft]
  else
    CornerButton1.Corners := CornerButton1.Corners - [TCorner.BottomLeft]
end;

procedure TfrmCtrlsDemo.CheckBox6Change(Sender: TObject);
begin
  if CheckBox6.IsChecked then
    CornerButton1.Corners := CornerButton1.Corners + [TCorner.TopLeft]
  else
    CornerButton1.Corners := CornerButton1.Corners - [TCorner.TopLeft]
end;

procedure TfrmCtrlsDemo.cornerListChange(Sender: TObject);
begin
{  CornerButton1.CornerType := TCornerType(cornerList.ItemIndex);
  CornerButton2.CornerType := TCornerType(cornerList.ItemIndex);
  CornerButton3.CornerType := TCornerType(cornerList.ItemIndex);
  CornerButton4.CornerType := TCornerType(cornerList.ItemIndex);
  CornerButton5.CornerType := TCornerType(cornerList.ItemIndex);
  CornerButton6.CornerType := TCornerType(cornerList.ItemIndex);}
end;

procedure TfrmCtrlsDemo.DropTarget1DragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Link;
end;

procedure TfrmCtrlsDemo.DropTarget1Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  if Data.Source <> nil then
    Edit1.Text := Data.Source.ClassName
  else
    Edit1.Text := Data.Files[0];
end;

procedure TfrmCtrlsDemo.CheckBox8Change(Sender: TObject);
begin
  ListBox1.ShowCheckboxes := CheckBox8.IsChecked;
  TreeView1.ShowCheckboxes := CheckBox8.IsChecked;
end;

procedure TfrmCtrlsDemo.CheckBox2Change(Sender: TObject);
begin
  StringListBox1.MultiSelect := CheckBox2.IsChecked;
end;

procedure TfrmCtrlsDemo.calloutBottomChange(Sender: TObject);
begin
  if calloutLeft.IsChecked then
    CalloutPanel1.CalloutPosition := TCalloutPosition.Left;
  if calloutRight.IsChecked then
    CalloutPanel1.CalloutPosition := TCalloutPosition.Right;
  if calloutTop.IsChecked then
    CalloutPanel1.CalloutPosition := TCalloutPosition.Top;
  if calloutBottom.IsChecked then
    CalloutPanel1.CalloutPosition := TCalloutPosition.Bottom;
end;

end.
