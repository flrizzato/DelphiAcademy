//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit main;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.UITypes, System.UIConsts, System.Types,
  FMX.Types, FMX.Forms, FMX.Controls, FMX.Filter, FMX.Layouts, FMX.ListBox, FMX.StdCtrls, FMX.Styles,
  FMX.Objects, FMX.Colors, FMX.Ani, FMX.Grid,   FMX.Types3D, FMX.Layers3D, FMX.Filter.Standard,
  FMX.Graphics, FMX.Controls.Presentation;

type
  TfrmMain = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    listCat: TListBox;
    Splitter4: TSplitter;
    listFX: TListBox;
    Source: TImage;
    Target: TImage;
    Dest: TImage;
    Splitter1: TSplitter;
    panelSettings: TListBox;
    Panel4: TPanel;
    btnBench: TButton;
    labelBench: TLabel;
    panelTransition: TPanel;
    btnPlay: TButton;
    AniIndicator1: TAniIndicator;
    Resources1: TStyleBook;
    BlurItem: TListBoxItem;
    ColorItem: TListBoxItem;
    ColorAdjustItem: TListBoxItem;
    CompositeItem: TListBoxItem;
    DistortionItem: TListBoxItem;
    GeneratorItem: TListBoxItem;
    GeometryItem: TListBoxItem;
    StyleItem: TListBoxItem;
    TilesItem: TListBoxItem;
    TransitionItem: TListBoxItem;
    procedure FormCreate(Sender: TObject);
    procedure listCat1Click(Sender: TObject);
    procedure listFX1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBench1Click(Sender: TObject);
    procedure btnPlay1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoFloatTrackChange(Sender: TObject);
    procedure DoColorChanged(Sender: TObject);
    procedure DoPointChanged(Sender: TObject; var X, Y: Single);
    procedure UpdatePreview;
    procedure RemoveTransitionAnimation;
    procedure StopTransitionAnimation;
    procedure ClearFilterParamControls;
    procedure RemoveFilter;
  public
    { Public declarations }
    FFilter: TFilter;
    FFilterParams: TFilterRec;
    FTransitionAni: TFloatAnimation;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiTb.fmx ANDROID}
{$R *.iPad.fmx IOS}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  CategoryList: TStrings;
begin
  Dest.Bitmap.Assign(Source.Bitmap);
  CategoryList := TStringList.Create;
  try
    TFilterManager.FillCategory(CategoryList);
    listCat.Items.Assign(CategoryList);
  finally
    CategoryList.Free;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if FFilter <> nil then
    FFilter.Free;
end;

procedure TfrmMain.listCat1Click(Sender: TObject);
var
  FilterList: TStrings;
  CurrentCategory: string;
begin
  StopTransitionAnimation;

  if listCat.ItemIndex < 0 then
    Exit;

  CurrentCategory := listCat.ListItems[listCat.ItemIndex].Text;
  FilterList := TStringList.Create;
  try
    TFilterManager.FillFiltersInCategory(CurrentCategory, FilterList);
    listFX.ItemIndex := -1;
    listFX.Items.Assign(FilterList);
  finally
    FilterList.Free;
  end;
end;

procedure TfrmMain.listFX1Click(Sender: TObject);
var
  i: integer;
  Item: TListboxItem;
  Shape: TSelectionPoint;
  LIsTransition : Boolean;
  LIsTransitionEnable : Boolean;
  CurrentFilterName: string;
  FilterClass: TFilterClass;
begin
  StopTransitionAnimation;
  if listFX.ItemIndex < 0 then
    Exit;

  RemoveTransitionAnimation;
  ClearFilterParamControls;
  RemoveFilter;

  CurrentFilterName := listFX.ListItems[listFX.ItemIndex].Text;
  FFilter := TFilterManager.FilterByName(CurrentFilterName);

  FilterClass := TFilterManager.FilterClassByName(CurrentFilterName);
  FFilterParams := FilterClass.FilterAttr;
  for i := 0 to High(FFilterParams.Values) do
  with FFilterParams.Values[i] do
  begin
    case ValueType of
      TFilterValueType.Float:
        begin
          Item := TListboxItem.Create(nil);
          Item.Parent := panelSettings;
          Item.Height := 64;
          Item.StyleLookup := 'trackitem';
          Item.Text := Name + ' (' + FloatToStrF(Min.AsExtended, ffFixed, 10, 2) + '-' + FloatToStrF(Max.AsExtended, ffFixed, 10, 2) + ')';
          Item.StylesData['desc'] := desc;
          if Assigned(Item.FindStyleResource('track')) then
          begin
            TTrackBar(Item.FindStyleResource('track')).Min := Min.AsExtended;
            TTrackBar(Item.FindStyleResource('track')).Max := Max.AsExtended;
            TTrackBar(Item.FindStyleResource('track')).Value := Default.AsExtended;
            TTrackBar(Item.FindStyleResource('track')).Tag := i;
            Item.StylesData['track'] := TValue.From<TNotifyEvent>(DoFloatTrackChange);
            if Name = 'Progress' then
            begin
              FTransitionAni := TFloatAnimation.Create(Self);
              FTransitionAni.Parent := TTrackBar(Item.FindStyleResource('track'));
              FTransitionAni.StartValue := 0;
              FTransitionAni.StopValue := 100;
              FTransitionAni.PropertyName := 'Value';
              FTransitionAni.Duration := 2;
              FTransitionAni.Loop := true;
              FTransitionAni.AutoReverse := true;
            end;
          end;
          Item.Opacity := 0;
          Item.AnimateFloatDelay('Opacity', 1, 0.5, i / 10);
        end;
      TFilterValueType.Point:
        begin
          Shape := TSelectionPoint.Create(Self);
          Shape.Parent := Dest;
          Shape.GripSize := 7;
          Shape.Position.X := Default.AsType<TPointF>.x;
          Shape.Position.Y := Default.AsType<TPointF>.y;
          Shape.OnTrack := DoPointChanged;
          Shape.Tag := i;
        end;
      TFilterValueType.Color:
        begin
          Item := TListboxItem.Create(nil);
          Item.Parent := panelSettings;
          Item.Height := 64;
          Item.StyleLookup := 'coloritem';
          Item.ApplyStyleLookup;
          Item.Text := Name;
          Item.StylesData['desc'] := desc;
          Item.StylesData['color'] := Default;
          Item.StylesData['color'] := TValue.From<TNotifyEvent>(DoColorChanged);
          if Assigned(Item.FindStyleResource('color')) then
            TFmxObject(Item.FindStyleResource('color')).Tag := i;
          Item.Opacity := 0;
          Item.AnimateFloatDelay('Opacity', 1, 0.5, i / 10);
        end;
    end;
  end;
  UpdatePreview;
  LIsTransition := (listCat.ListItems[listCat.ItemIndex].Text = 'Transition');
  LIsTransitionEnable := (panelTransition.Visible and Assigned(FTransitionAni));
  if (panelTransition.Visible <> LIsTransition) or
    (panelTransition.Visible <> (LIsTransition and LIsTransitionEnable)) then
  begin
    if panelTransition.Visible then
    begin
      btnPlay.AnimateFloatWait('Opacity', 0, 0.3);
      panelTransition.AnimateFloatWait('Height', 0, 0.3);
      panelTransition.Visible := false;
    end
    else
    begin
      panelTransition.Visible := true;
      panelTransition.Height := 0;
      panelTransition.AnimateFloat('Height', 36, 0.3);
      btnPlay.Opacity := 0;
      btnPlay.AnimateFloatDelay('Opacity', 1, 0.3, 0.3);
    end;
  end;
end;

procedure TfrmMain.RemoveFilter;
begin
  if FFilter <> nil then
    FFilter.Free;
end;

procedure TfrmMain.RemoveTransitionAnimation;
begin
  if FTransitionAni <> nil then
    FreeAndNil(FTransitionAni);
end;

procedure TfrmMain.StopTransitionAnimation;
begin
  if (FTransitionAni <> nil) and FTransitionAni.Running then
    FTransitionAni.Stop;
end;

procedure TfrmMain.DoFloatTrackChange(Sender: TObject);
begin
  FFilter.ValuesAsFloat[FFilterParams.Values[TFmxObject(Sender).Tag].Name] := TTrackBar(Sender).Value;
  UpdatePreview;
end;

procedure TfrmMain.DoColorChanged(Sender: TObject);
begin
  FFilter.ValuesAsColor[FFilterParams.Values[TFmxObject(Sender).Tag].Name] := TFmxObject(Sender).Data.AsType<TAlphaColor>;
  UpdatePreview;
end;

procedure TfrmMain.DoPointChanged(Sender: TObject; var X, Y: Single);
begin
  FFilter.ValuesAsPoint[FFilterParams.Values[TFmxObject(Sender).Tag].Name] := TPointF.Create(TControl(Sender).Position.X, TControl(Sender).Position.Y);
  UpdatePreview;
end;

procedure TfrmMain.UpdatePreview;
begin
  if FFilter <> nil then
  begin
    FFilter.ValuesAsBitmap['Input'] := Source.Bitmap;
    FFilter.ValuesAsBitmap['Target'] := Target.Bitmap;
    Dest.Bitmap := FFilter.ValuesAsBitmap['output'];
  end;
end;

function GetTickCount: single;
var
  H, M, S, MS: word;
begin
  DecodeTime(time, H, M, S, MS);
  Result := ((((H * 60 * 60) + (M * 60) + S) * 1000) + MS);
end;

procedure TfrmMain.btnBench1Click(Sender: TObject);
const
  Steps = 200;
var
  i: integer;
  StartTime, NewTime, Time: single;
begin
  btnBench.Enabled := false;
  labelBench.Visible := false;
  AniIndicator1.Visible := true;
  AniIndicator1.Enabled := true;

  Time := 0;
  for i := 1 to Steps do
  begin
    if FFilter <> nil then
    begin
      FFilter.ValuesAsBitmap['input'] := Source.Bitmap;

      StartTime := GetTickCount / 1000;
      FFilter.Apply;
      NewTime := GetTickCount / 1000;

      Time := Time + (NewTime - StartTime);

      Application.ProcessMessages;
    end;
  end;

  AniIndicator1.Enabled := false;
  AniIndicator1.Visible := false;

  labelBench.Visible := true;
  labelBench.Opacity := 0;
  labelBench.AnimateFloat('Opacity', 1, 0.3);
  labelBench.Text := 'Average: ' + FloatToStrF((Time) / Steps, ffFixed, 6, 4) + 'ms';
  btnBench.Enabled := true;
end;

procedure TfrmMain.btnPlay1Click(Sender: TObject);
begin
  if btnPlay.Text = 'Play' then
  begin
    FTransitionAni.Start;
    btnPlay.Text := 'Stop';
  end
  else
  begin
    FTransitionAni.Stop;
    btnPlay.Text := 'Play';
  end;
end;

procedure TfrmMain.ClearFilterParamControls;
begin
  Dest.DeleteChildren;
  panelSettings.Clear;
end;

end.
