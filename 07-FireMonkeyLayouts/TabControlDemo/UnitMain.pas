//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.TypInfo, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms, FMX.Dialogs, FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.StdActns,
  FMX.ImgList, FMX.ExtCtrls, FMX.ListBox, UnitData;

type
  THeaderFooterwithNavigation = class(TForm)
    ActionList1: TActionList;
    PreviousTabAction1: TPreviousTabAction;
    TitleAction: TControlAction;
    NextTabAction1: TNextTabAction;
    TopToolBar: TToolBar;
    btnBack: TSpeedButton;
    ToolBarLabel: TLabel;
    btnNext: TSpeedButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Memo1: TMemo;
    AutoSizeAction: TAction;
    VertScrollBox1: TVertScrollBox;
    CheckBoxAutoSize: TCheckBox;
    WidthAction: TValueRangeAction;
    TrackBarWidth: TTrackBar;
    LabelWidth: TLabel;
    TabItem3: TTabItem;
    HeightAction: TValueRangeAction;
    LabelHeight: TLabel;
    TrackBarHeight: TTrackBar;
    Glyph1: TGlyph;
    PopupBoxTabPosition: TPopupBox;
    LabelTabPosition: TLabel;
    TabPositionAction: TControlAction;
    LabelFullSize: TLabel;
    PopupBoxFullSize: TPopupBox;
    FullSizeAction: TControlAction;
    LabelTabHeight: TLabel;
    TrackBarTabHeight: TTrackBar;
    TabHeightAction: TValueRangeAction;
    BoundsAnimationAction: TAction;
    CheckBoxBoundsAnimation: TCheckBox;
    TouchTrackingAction: TAction;
    CheckBoxHorzTouchTracki: TCheckBox;
    EnableAction: TAction;
    CheckBoxEnabled: TCheckBox;
    ListBoxVisible: TListBox;
    LabelVisible: TLabel;
    StyleBook1: TStyleBook;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    AddAction: TAction;
    DeleteAction: TAction;
    procedure TitleActionUpdate(Sender: TObject);
    procedure TabItem2Painting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure TabItem2Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure AutoSizeActionExecute(Sender: TObject);
    procedure AutoSizeActionUpdate(Sender: TObject);
    procedure WidthActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HeightActionUpdate(Sender: TObject);
    procedure TabPositionActionUpdate(Sender: TObject);
    procedure FullSizeActionUpdate(Sender: TObject);
    procedure TabHeightActionUpdate(Sender: TObject);
    procedure BoundsAnimationActionExecute(Sender: TObject);
    procedure BoundsAnimationActionUpdate(Sender: TObject);
    procedure TouchTrackingActionExecute(Sender: TObject);
    procedure TouchTrackingActionUpdate(Sender: TObject);
    procedure EnableActionExecute(Sender: TObject);
    procedure EnableActionUpdate(Sender: TObject);
    procedure AddActionExecute(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure DeleteActionUpdate(Sender: TObject);
  private
    { Private declarations }
    FListUpdating: Boolean;
    procedure WidthActionChange(Sender: TObject);
    procedure HeightActionChange(Sender: TObject);
    procedure TabPositionChange(Sender: TObject);
    procedure FullSizeChange(Sender: TObject);
    procedure TabHeightActionChange(Sender: TObject);
    procedure ListBoxItemClick(Sender: TObject);
    procedure UpdateListBox;
  public
    { Public declarations }
  end;

var
  HeaderFooterwithNavigation: THeaderFooterwithNavigation;

implementation

uses
  FMX.BehaviorManager;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure THeaderFooterwithNavigation.FormCreate(Sender: TObject);
var
  P: TTabPosition;
  B: TBehaviorBoolean;
begin
  TabHeightAction.ValueRange.OnChanged := TabHeightActionChange;
  WidthAction.ValueRange.OnChanged := WidthActionChange;
  HeightAction.ValueRange.OnChanged := HeightActionChange;
  for P := Low(TTabPosition) to High(TTabPosition) do
  begin
    PopupBoxTabPosition.Items.Add(GetEnumName(TypeInfo(TTabPosition), Ord(P)));
    if P = TabControl1.TabPosition then
      PopupBoxTabPosition.ItemIndex := PopupBoxTabPosition.Items.Count - 1;
  end;
  PopupBoxTabPosition.OnChange := TabPositionChange;
  for B := Low(TBehaviorBoolean) to High(TBehaviorBoolean) do
  begin
    PopupBoxFullSize.Items.Add(GetEnumName(TypeInfo(TBehaviorBoolean), Ord(B)));
    if B = TabControl1.FullSize then
      PopupBoxFullSize.ItemIndex := PopupBoxFullSize.Items.Count - 1;
  end;
  PopupBoxFullSize.OnChange := FullSizeChange;
  UpdateListBox;
end;

procedure THeaderFooterwithNavigation.UpdateListBox;
var
  Item: TListBoxItem;
  I: Integer;
begin
  if not FListUpdating then
  begin
    ListBoxVisible.OnChangeCheck := ListBoxItemClick;
    FListUpdating := True;
    try
      while ListBoxVisible.Count < TabControl1.TabCount do
      begin
        Item := TListBoxItem.Create(Self);
        Item.Selectable := False;
        ListBoxVisible.AddObject(Item);
      end;
      while ListBoxVisible.Count > TabControl1.TabCount do
      begin
        Item := ListBoxVisible.ItemByIndex(ListBoxVisible.Count - 1);
        Item.DisposeOf;
      end;
      for I := 0 to TabControl1.TabCount - 1 do
      begin
        Item := ListBoxVisible.ItemByIndex(I);
        Item.Text := TabControl1.Tabs[I].Text;
        Item.ImageIndex := TabControl1.Tabs[I].ImageIndex;
        Item.IsChecked := TabControl1.Tabs[I].Visible;
      end;
    finally
      FListUpdating := False;
    end;
  end;
end;

procedure THeaderFooterwithNavigation.ListBoxItemClick(Sender: TObject);
begin
  if (Sender is TListBoxItem) and not FListUpdating then
  begin
    TabControl1.Tabs[TListBoxItem(Sender).Index].Visible := TListBoxItem(Sender).IsChecked;
  end;
end;

procedure THeaderFooterwithNavigation.BoundsAnimationActionExecute(Sender: TObject);
begin
  TabControl1.AniCalculations.BoundsAnimation := not TabControl1.AniCalculations.BoundsAnimation;
end;

procedure THeaderFooterwithNavigation.BoundsAnimationActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    TCustomAction(Sender).Text := 'TTabControl.AniCalculations.BoundsAnimation';
    TCustomAction(Sender).Checked := TabControl1.AniCalculations.BoundsAnimation;
  end;
end;

procedure THeaderFooterwithNavigation.DeleteActionExecute(Sender: TObject);
begin
  if TabControl1.TabIndex >= 0 then
  begin
    TabControl1.Delete(TabControl1.TabIndex);
    UpdateListBox;
  end;
end;

procedure THeaderFooterwithNavigation.DeleteActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
    TCustomAction(Sender).Enabled := TabControl1.ActiveTab <> nil;
end;

procedure THeaderFooterwithNavigation.TouchTrackingActionExecute(Sender: TObject);
begin
  if ttHorizontal in TabControl1.AniCalculations.TouchTracking then
    TabControl1.AniCalculations.TouchTracking := TabControl1.AniCalculations.TouchTracking - [ttHorizontal]
  else
    TabControl1.AniCalculations.TouchTracking := TabControl1.AniCalculations.TouchTracking + [ttHorizontal];
end;

procedure THeaderFooterwithNavigation.TouchTrackingActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    TCustomAction(Sender).Text := 'TTabControl.AniCalculations.TouchTracking';
    TCustomAction(Sender).Checked := ttHorizontal in TabControl1.AniCalculations.TouchTracking;
  end;
end;

procedure THeaderFooterwithNavigation.EnableActionExecute(Sender: TObject);
begin
  if (TabControl1.ActiveTab <> nil) then
    TabControl1.ActiveTab.Enabled := not TabControl1.ActiveTab.Enabled;
end;

procedure THeaderFooterwithNavigation.EnableActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    if (TabControl1.ActiveTab <> nil) then
    begin
      TCustomAction(Sender).Checked := TabControl1.ActiveTab.Enabled;
      TCustomAction(Sender).Enabled := True;
    end
    else
    begin
      TCustomAction(Sender).Checked := False;
      TCustomAction(Sender).Enabled := False;
    end;
  end;
end;

procedure THeaderFooterwithNavigation.AddActionExecute(Sender: TObject);
var
  Tab: TTabItem;
  TmpLabel: TLabel;
begin
  Tab := TabControl1.Add(TTabItem);
  Tab.Text := 'Tab #' + Tab.Index.ToString;
  Tab.ImageIndex := Random(MainDataModule.ImageList1.Count);

  TmpLabel := TLabel.Create(Self);
  TmpLabel.StyledSettings := TmpLabel.StyledSettings - [TStyledSetting.Size];
  TmpLabel.TextSettings.Font.Size := 33;
  TmpLabel.AutoSize := True;
  TmpLabel.Align := TAlignLayout.Top;
  TmpLabel.Text := 'New ' + Tab.ClassName + ' #' + Tab.Index.ToString;
  TmpLabel.Parent := Tab;

  TabControl1.ActiveTab := Tab;
  TabControl1.GoToActiveTab;
  UpdateListBox;
end;

procedure THeaderFooterwithNavigation.AutoSizeActionExecute(Sender: TObject);
begin
  if (TabControl1.ActiveTab <> nil) then
    TabControl1.ActiveTab.AutoSize := not TabControl1.ActiveTab.AutoSize;
end;

procedure THeaderFooterwithNavigation.AutoSizeActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    if (TabControl1.ActiveTab <> nil) then
    begin
      TCustomAction(Sender).Checked := TabControl1.ActiveTab.AutoSize;
      TCustomAction(Sender).Enabled := True;
    end
    else
    begin
      TCustomAction(Sender).Checked := False;
      TCustomAction(Sender).Enabled := False;
    end;
  end;
end;

procedure THeaderFooterwithNavigation.TabHeightActionUpdate(Sender: TObject);
begin
  if Sender is TCustomValueRangeAction then
  begin
    TCustomValueRangeAction(Sender).ValueRange.Value := TabControl1.TabHeight;
    TCustomValueRangeAction(Sender).Text := 'TTabControl.TabHeight: ' + TabControl1.TabHeight.ToString;
  end;
end;

procedure THeaderFooterwithNavigation.TabHeightActionChange(Sender: TObject);
begin
  if (Sender is TCustomValueRange) then
    TabControl1.TabHeight := TCustomValueRange(Sender).Value;
end;

procedure THeaderFooterwithNavigation.TabItem2Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  LRect: TRectF;
begin
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := TAlphaColorRec.Red;
  Canvas.StrokeDash := TStrokeDash.Dash;
  Canvas.StrokeThickness := 1;
  LRect := ARect;
  LRect.Inflate(-1.5, -1.5);
  LRect.Offset(0, 2);
  Canvas.DrawRect(LRect, 1, 1, AllCorners, 1, TCornerType.Round);
end;

procedure THeaderFooterwithNavigation.TabItem2Painting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  LRect: TRectF;
begin
  if (Sender is TTabItem) and (TTabItem(Sender).TabControl.EffectiveTabPosition = TTabPosition.Dots) then
  begin
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := TAlphaColorRec.Yellow;
    LRect := ARect;
    LRect.Inflate(2, 2);
    Canvas.FillRect(LRect, 1, 1, AllCorners, 1, TCornerType.Round);
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := TAlphaColorRec.Brown;
    Canvas.StrokeThickness := 2;
    Canvas.StrokeDash := TStrokeDash.Solid;
    LRect := ARect;
    LRect.Inflate(5, 5);
    Canvas.DrawRect(LRect, 2, 2, AllCorners, 0.5, TCornerType.Round);
  end;
end;

procedure THeaderFooterwithNavigation.TabPositionActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
    TCustomAction(Sender).Text := 'TTabControl.EffectiveTabPosition: ' + GetEnumName(TypeInfo(TTabPosition),
      Ord(TabControl1.EffectiveTabPosition));
end;

procedure THeaderFooterwithNavigation.TabPositionChange(Sender: TObject);
var
  S: string;
begin
  S := PopupBoxTabPosition.Text;
  if S <> '' then
    TabControl1.TabPosition := TTabPosition(GetEnumValue(TypeInfo(TTabPosition), S));
end;

procedure THeaderFooterwithNavigation.FullSizeActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
    TCustomAction(Sender).Text := 'TTabControl.EffectiveFullSize: ' + GetEnumName(TypeInfo(TBehaviorBoolean),
      Ord(TabControl1.EffectiveFullSize));
end;

procedure THeaderFooterwithNavigation.FullSizeChange(Sender: TObject);
var
  S: string;
begin
  S := PopupBoxFullSize.Text;
  if S <> '' then
    TabControl1.FullSize := TBehaviorBoolean(GetEnumValue(TypeInfo(TBehaviorBoolean), S));
end;

procedure THeaderFooterwithNavigation.TitleActionUpdate(Sender: TObject);
begin
  if Sender is TCustomAction then
  begin
    if TabControl1.ActiveTab <> nil then
      TCustomAction(Sender).Text := TabControl1.ActiveTab.Text
    else
      TCustomAction(Sender).Text := '';
  end;
end;

procedure THeaderFooterwithNavigation.WidthActionChange(Sender: TObject);
begin
  if (Sender is TCustomValueRange) and (TabControl1.ActiveTab <> nil) then
    TabControl1.ActiveTab.Width := TCustomValueRange(Sender).Value;
end;

procedure THeaderFooterwithNavigation.HeightActionChange(Sender: TObject);
begin
  if (Sender is TCustomValueRange) and (TabControl1.ActiveTab <> nil) then
    TabControl1.ActiveTab.Height := TCustomValueRange(Sender).Value;
end;

procedure THeaderFooterwithNavigation.WidthActionUpdate(Sender: TObject);
begin
  if Sender is TValueRangeAction then
  begin
    if TabControl1.ActiveTab <> nil then
    begin
      TValueRangeAction(Sender).Text := 'TTabItem.Width: ' + TabControl1.ActiveTab.Width.ToString;
      TValueRangeAction(Sender).Enabled := not TabControl1.EffectiveFullSize and not TabControl1.ActiveTab.AutoSize;
      TValueRangeAction(Sender).ValueRange.Value := TabControl1.ActiveTab.Width;
    end
    else
    begin
      TValueRangeAction(Sender).Text := 'nil';
      TValueRangeAction(Sender).Enabled := False;
      TValueRangeAction(Sender).ValueRange.Value := TValueRangeAction(Sender).ValueRange.Min;
    end;
  end;
end;

procedure THeaderFooterwithNavigation.HeightActionUpdate(Sender: TObject);
begin
  if Sender is TValueRangeAction then
  begin
    if TabControl1.ActiveTab <> nil then
    begin
      TValueRangeAction(Sender).Text := 'TTabItem.Height: ' + TabControl1.ActiveTab.Height.ToString;
      TValueRangeAction(Sender).Enabled := (TabControl1.TabHeight = 0) and not TabControl1.ActiveTab.AutoSize;
      TValueRangeAction(Sender).ValueRange.Value := TabControl1.ActiveTab.Height;
    end
    else
    begin
      TValueRangeAction(Sender).Text := 'nil';
      TValueRangeAction(Sender).Enabled := False;
      TValueRangeAction(Sender).ValueRange.Value := TValueRangeAction(Sender).ValueRange.Min;
    end;
  end;
end;

end.
