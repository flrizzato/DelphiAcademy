//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit TabSlideTransition;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Gestures, FMX.StdCtrls,
  FMX.Edit, FMX.ExtCtrls, FMX.TabControl, FMX.Layouts, FMX.Memo, FMX.ListBox, FMX.VirtualKeyboard,FMX.Menus,FMX.Platform,
  System.Actions, FMX.ActnList, System.Math, FMX.DateTimeCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox ;


type
  TTabSlideTransitionFrmBase = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    TabItem5: TTabItem;
    infoAddress: TEdit;
    infoTelephone: TEdit;
    infoEmail: TEdit;
    Memo1: TMemo;
    efirstName: TEdit;
    edInstitution: TEdit;
    edAdmissionDate: TComboBox;
    edCity: TEdit;
    edGraduationDate: TComboBox;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    weEmpName: TEdit;
    weCity: TEdit;
    weOccupiedJob: TEdit;
    weFrom: TComboBox;
    ListBoxItem18: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxItem21: TListBoxItem;
    ListBoxItem22: TListBoxItem;
    weTo: TComboBox;
    ListBoxItem23: TListBoxItem;
    ListBoxItem24: TListBoxItem;
    ListBoxItem25: TListBoxItem;
    ListBoxItem26: TListBoxItem;
    ListBoxItem27: TListBoxItem;
    Button1: TButton;
    ActionList1: TActionList;
    Button2: TButton;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ToolBar1: TToolBar;
    Label2: TLabel;
    NameList: TListBox;
    firstName: TListBoxItem;
    LastName: TListBoxItem;
    PersonalInfoList: TListBox;
    Address: TListBoxItem;
    Phone: TListBoxItem;
    Email: TListBoxItem;
    BirthDate: TListBoxItem;
    EducationList: TListBox;
    Institution: TListBoxItem;
    City: TListBoxItem;
    AdmissionDate: TListBoxItem;
    GraduationDate: TListBoxItem;
    WorkList: TListBox;
    Employer: TListBoxItem;
    EmployerCity: TListBoxItem;
    FromDate: TListBoxItem;
    ToDate: TListBoxItem;
    CurrentJob: TListBoxItem;
    eLastName: TEdit;
    VertScrollBox1: TVertScrollBox;
    MainLayout1: TLayout;
    infoDate: TDateEdit;
    PreviousTabAction: TPreviousTabAction;
    NextTabAction: TNextTabAction;
    TitleAction: TControlAction;
    GestureManager1: TGestureManager;
    procedure UpdateMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormFocusChanged(Sender: TObject);
    procedure FormVirtualKeyboardHidden(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure TabControl1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);

  private
    FService: IFMXVirtualKeyboardToolbarService;
    FKBBounds: TRectF;
    FNeedOffset: Boolean;
    procedure CalcContentBoundsProc(Sender: TObject;
                                var ContentBounds: TRectF);
    procedure UpdateKBBounds;
    procedure RestorePosition;
    function FindListBox(const AObj: TFmxObject): TListBox;
    function GetIsFilled(const AList: TListBox): Boolean;
  public
    { Public declarations }
  end;

var
  TabSlideTransitionFrmBase: TTabSlideTransitionFrmBase;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

function TTabSlideTransitionFrmBase.FindListBox(const AObj: TFmxObject): TListBox;
var
  Obj: TFmxObject;
begin
  Result := nil;
  if AObj.ChildrenCount > 0 then
  begin
    for Obj in AObj.Children do
    begin
      if Obj is TListBox then
        Result := TListBox(Obj)
      else
        Result := FindListBox(Obj);

      if Result <> nil then
        Exit(Result);
    end;
  end;
end;

function TTabSlideTransitionFrmBase.GetIsFilled(const AList: TListBox): Boolean;
var
  I: Integer;
  Item: TListBoxItem;
  Obj: TFmxObject;
begin
  Result := True;
  for I := 0 to AList.Count - 1 do
  begin
    Item := AList.ItemByIndex(I);
    for Obj in Item.Children do
    begin
      if Obj is TEdit then
        Result := TEdit(Obj).Text <> '';
      if Obj is TDateEdit then
        Result := not TDateEdit(Obj).IsEmpty;
      if Obj is TComboBox then
        Result := TComboBox(Obj).ItemIndex >= 0;

      if not Result then
        Exit(Result);
    end;
    if not Result then
      Exit(Result);
  end;
end;

procedure TTabSlideTransitionFrmBase.FormCreate(Sender: TObject);
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, IInterface(FService)) then
  begin
    FService.SetToolbarEnabled(True);
    FService.SetHideKeyboardButtonVisibility(True);
  end;
  VertScrollBox1.OnCalcContentBounds := CalcContentBoundsProc;
end;

procedure TTabSlideTransitionFrmBase.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
var
  LTab: TTabItem;
  LList: TListBox;
begin
  LTab := TabControl1.ActiveTab;

  if Action = NextTabAction then
  begin
    LList := FindListBox(LTab);
    if LList <> nil then
      NextTabAction.Enabled := GetIsFilled(LList);
  end;

  if Action = TitleAction then
    TitleAction.Text := LTab.Text;

  if LTab = TabItem5 then
      UpdateMemo;
end;

procedure TTabSlideTransitionFrmBase.CalcContentBoundsProc(Sender: TObject; var ContentBounds: TRectF);
begin
  if FNeedOffset and (FKBBounds.Top > 0) then
  begin
    ContentBounds.Bottom := Max(ContentBounds.Bottom,
                                2 * ClientHeight - FKBBounds.Top);
  end;
end;

procedure TTabSlideTransitionFrmBase.FormFocusChanged(Sender: TObject);
begin
  UpdateKBBounds;
end;

procedure TTabSlideTransitionFrmBase.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    TabControl1.Previous;
    Key := 0;
  end;
end;

procedure TTabSlideTransitionFrmBase.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  FKBBounds.Create(0, 0, 0, 0);
  FNeedOffset := False;
  RestorePosition;
end;

procedure TTabSlideTransitionFrmBase.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  FKBBounds := TRectF.Create(Bounds);
  FKBBounds.TopLeft := ScreenToClient(FKBBounds.TopLeft);
  FKBBounds.BottomRight := ScreenToClient(FKBBounds.BottomRight);
  UpdateKBBounds;
end;

procedure TTabSlideTransitionFrmBase.UpdateMemo;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Personal information');
  Memo1.Lines.Add('Name: ' + eFirstName.Text + ' ' + eLastName.Text);
  Memo1.Lines.Add('Address: ' + infoAddress.Text);
  Memo1.Lines.Add('Telephone: ' + infoTelephone.Text);
  Memo1.Lines.Add('E-mail: ' + infoEmail.Text);
  Memo1.Lines.Add('Date of birth:' + infoDate.Text);
  Memo1.Lines.Add('');

  Memo1.Lines.Add('Education');
  Memo1.Lines.Add(edInstitution.Text + ', ' + edCity.Text + ', ' +
                  edAdmissionDate.Selected.Text + '-' + edGraduationDate.Selected.Text);
  Memo1.Lines.Add('');

  Memo1.Lines.Add('Work experience');
  Memo1.Lines.Add(weOccupiedJob.Text + ' at ' +  weEmpName.Text + ', ' + weCity.Text + ', ' +
                weFrom.Selected.Text + '-' + weTo.Selected.Text );
end;

procedure TTabSlideTransitionFrmBase.UpdateKBBounds;
var
  LFocused : TControl;
  LFocusRect: TRectF;
begin
  FNeedOffset := False;
  if Assigned(Focused) then
  begin
    LFocused := TControl(Focused.GetObject);
    LFocusRect := LFocused.AbsoluteRect;
    LFocusRect.Offset(VertScrollBox1.ViewportPosition);
    if (LFocusRect.IntersectsWith(TRectF.Create(FKBBounds))) and
       (LFocusRect.Bottom > FKBBounds.Top) then
    begin
      FNeedOffset := True;
      MainLayout1.Align := TAlignLayout.Horizontal;
      VertScrollBox1.RealignContent;
      Application.ProcessMessages;
      VertScrollBox1.ViewportPosition :=
        PointF(VertScrollBox1.ViewportPosition.X,
               LFocusRect.Bottom - FKBBounds.Top);
    end;
  end;
  if not FNeedOffset then
    RestorePosition;
end;

procedure TTabSlideTransitionFrmBase.RestorePosition;
begin
  VertScrollBox1.ViewportPosition := PointF(VertScrollBox1.ViewportPosition.X, 0);
  MainLayout1.Align := TAlignLayout.Client;
  VertScrollBox1.RealignContent;
end;

procedure TTabSlideTransitionFrmBase.TabControl1Gesture(Sender: TObject; const EventInfo: TGestureEventInfo;
  var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if NextTabAction.Enabled then
          TabControl1.Next;
        Handled := True;
      end;
    sgiRight:
      begin
        TabControl1.Previous;
        Handled := True;
      end;
  end;
end;

end.
