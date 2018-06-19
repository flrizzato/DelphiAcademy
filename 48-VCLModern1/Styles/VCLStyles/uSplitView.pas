//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uSplitView;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  System.Actions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.WinXCtrls,
  Vcl.StdCtrls,
  Vcl.CategoryButtons,
  Vcl.Buttons,
  Vcl.ImgList,
  Vcl.Imaging.PngImage,
  Vcl.ComCtrls,
  Vcl.ActnList;

type
  TSplitViewForm = class(TForm)
    pnlToolbar: TPanel;
    grpDisplayMode: TRadioGroup;
    grpPlacement: TRadioGroup;
    pnlSettings: TPanel;
    grpCloseStyle: TRadioGroup;
    chkUseAnimation: TCheckBox;
    SV: TSplitView;
    catMenuItems: TCategoryButtons;
    lstLog: TListBox;
    imlIcons: TImageList;
    imgMenu: TImage;
    cbxVclStyles: TComboBox;
    grpAnimation: TGroupBox;
    lblLog: TLabel;
    lblAnimationDelay: TLabel;
    lblAnimationStep: TLabel;
    trkAnimationDelay: TTrackBar;
    trkAnimationStep: TTrackBar;
    ActionList1: TActionList;
    actHome: TAction;
    actLayout: TAction;
    actPower: TAction;
    chkCloseOnMenuClick: TCheckBox;
    lblTitle: TLabel;
    lblVclStyle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure grpDisplayModeClick(Sender: TObject);
    procedure grpPlacementClick(Sender: TObject);
    procedure grpCloseStyleClick(Sender: TObject);
    procedure SVClosed(Sender: TObject);
    procedure SVClosing(Sender: TObject);
    procedure SVOpened(Sender: TObject);
    procedure SVOpening(Sender: TObject);
    procedure catMenuItemsCategoryCollapase(Sender: TObject; const Category: TButtonCategory);
    procedure imgMenuClick(Sender: TObject);
    procedure chkUseAnimationClick(Sender: TObject);
    procedure trkAnimationDelayChange(Sender: TObject);
    procedure trkAnimationStepChange(Sender: TObject);
    procedure actHomeExecute(Sender: TObject);
    procedure actLayoutExecute(Sender: TObject);
    procedure actPowerExecute(Sender: TObject);
    procedure cbxVclStylesChange(Sender: TObject);
  private
    procedure Log(const Msg: string);
  public
  end;

var
  SplitViewForm: TSplitViewForm;

implementation

uses
  Vcl.Themes;

{$R *.dfm}

procedure TSplitViewForm.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  for StyleName in TStyleManager.StyleNames do
    cbxVclStyles.Items.Add(StyleName);

  cbxVclStyles.ItemIndex := cbxVclStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;

procedure TSplitViewForm.cbxVclStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle(cbxVclStyles.Text);
end;

procedure TSplitViewForm.imgMenuClick(Sender: TObject);
begin
  if SV.Opened then
    SV.Close
  else
    SV.Open;
end;

procedure TSplitViewForm.grpDisplayModeClick(Sender: TObject);
begin
  SV.DisplayMode := TSplitViewDisplayMode(grpDisplayMode.ItemIndex);
end;

procedure TSplitViewForm.grpCloseStyleClick(Sender: TObject);
begin
  SV.CloseStyle := TSplitViewCloseStyle(grpCloseStyle.ItemIndex);
end;

procedure TSplitViewForm.grpPlacementClick(Sender: TObject);
begin
  SV.Placement := TSplitViewPlacement(grpPlacement.ItemIndex);
end;

procedure TSplitViewForm.SVClosed(Sender: TObject);
begin
  // When TSplitView is closed, adjust ButtonOptions and Width
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions - [boShowCaptions];
  if SV.CloseStyle = svcCompact then
    catMenuItems.Width := SV.CompactWidth;
end;

procedure TSplitViewForm.SVClosing(Sender: TObject);
begin
//
end;

procedure TSplitViewForm.SVOpened(Sender: TObject);
begin
  // When not animating, change size of catMenuItems when TSplitView is opened
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
  catMenuItems.Width := SV.OpenedWidth;
end;

procedure TSplitViewForm.SVOpening(Sender: TObject);
begin
  // When animating, change size of catMenuItems at the beginning of open
  catMenuItems.ButtonOptions := catMenuItems.ButtonOptions + [boShowCaptions];
  catMenuItems.Width := SV.OpenedWidth;
end;

procedure TSplitViewForm.chkUseAnimationClick(Sender: TObject);
begin
  SV.UseAnimation := chkUseAnimation.Checked;
  lblAnimationDelay.Enabled := SV.UseAnimation;
  trkAnimationDelay.Enabled := SV.UseAnimation;
  lblAnimationStep.Enabled := SV.UseAnimation;
  trkAnimationStep.Enabled := SV.UseAnimation;
end;

procedure TSplitViewForm.trkAnimationDelayChange(Sender: TObject);
begin
  SV.AnimationDelay := trkAnimationDelay.Position * 5;
  lblAnimationDelay.Caption := 'Animation Delay (' + IntToStr(SV.AnimationDelay) + ')';
end;

procedure TSplitViewForm.trkAnimationStepChange(Sender: TObject);
begin
  SV.AnimationStep := trkAnimationStep.Position * 5;
  lblAnimationStep.Caption := 'Animation Step (' + IntToStr(SV.AnimationStep) + ')';
end;

procedure TSplitViewForm.actHomeExecute(Sender: TObject);
begin
  Log(actHome.Caption + ' Clicked');
  if SV.Opened and chkCloseOnMenuClick.Checked then
    SV.Close;
end;

procedure TSplitViewForm.actLayoutExecute(Sender: TObject);
begin
  Log(actLayout.Caption + ' Clicked');
  if SV.Opened and chkCloseOnMenuClick.Checked then
    SV.Close;
end;

procedure TSplitViewForm.actPowerExecute(Sender: TObject);
begin
  Log(actPower.Caption + ' Clicked');
  if SV.Opened and chkCloseOnMenuClick.Checked then
    SV.Close;
end;

procedure TSplitViewForm.catMenuItemsCategoryCollapase(Sender: TObject; const Category: TButtonCategory);
begin
  // Prevent the catMenuItems Category group from being collapsed
  catMenuItems.Categories[0].Collapsed := False;
end;

procedure TSplitViewForm.Log(const Msg: string);
var
  Idx: Integer;
begin
  Idx := lstLog.Items.Add(Msg);
  lstLog.TopIndex := Idx;
end;

end.
