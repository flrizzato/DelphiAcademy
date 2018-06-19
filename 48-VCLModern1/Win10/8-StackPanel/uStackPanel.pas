//---------------------------------------------------------------------------

// This software is Copyright (c) 2017 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uStackPanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.WinXPanels;

type
  TStackPanelForm = class(TForm)
    Panel1: TPanel;
    lblVclStyle: TLabel;
    cbxVclStyles: TComboBox;
    grpOrientation: TRadioGroup;
    Panel2: TPanel;
    grpControlPositioning: TRadioGroup;
    trkSpacing: TTrackBar;
    lblSpacing: TLabel;
    grpPositioning: TRadioGroup;
    lstControls: TListBox;
    lblPadding: TLabel;
    trkPadding: TTrackBar;
    lblControlOverride: TLabel;
    StackPanel1: TStackPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure cbxVclStylesChange(Sender: TObject);
    procedure grpOrientationClick(Sender: TObject);
    procedure grpPositioningClick(Sender: TObject);
    procedure trkSpacingChange(Sender: TObject);
    procedure lstControlsClick(Sender: TObject);
    procedure grpControlPositioningClick(Sender: TObject);
    procedure trkPaddingChange(Sender: TObject);
  private
    procedure ResetControl;
  public
    { Public declarations }
  end;

var
  StackPanelForm: TStackPanelForm;

implementation

{$R *.dfm}

uses
  Vcl.Themes;

var
  DefaultLabel1Width, DefaultLabel1Height,
  DefaultEdit1Width,  DefaultEdit1Height,
  DefaultButton1Width,DefaultButton1Height,
  DefaultMemo1Width,  DefaultMemo1Height: integer;

procedure TStackPanelForm.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  for StyleName in TStyleManager.StyleNames do
    cbxVclStyles.Items.Add(StyleName);

  cbxVclStyles.ItemIndex := cbxVclStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);

  DefaultLabel1Width   :=  Label1.Width;
  DefaultEdit1Width    :=  Edit1.Width;
  DefaultButton1Width  :=  Button1.Width;
  DefaultMemo1Width    :=  Memo1.Width;
  DefaultLabel1Height  :=  Label1.Height;
  DefaultEdit1Height   :=  Edit1.Height;
  DefaultButton1Height :=  Button1.Height;
  DefaultMemo1Height   :=  Memo1.Height;

  lstControls.ItemIndex := 0;
end;

procedure TStackPanelForm.cbxVclStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle(cbxVclStyles.Text);
end;

procedure TStackPanelForm.grpOrientationClick(Sender: TObject);
begin
  StackPanel1.Orientation := TStackPanelOrientation(grpOrientation.ItemIndex);
  if StackPanel1.Orientation = spoVertical then
  begin
    grpPositioning.Caption := 'Horizontal Positioning';
    grpPositioning.Items.Clear;
    grpPositioning.Items.Add('sphpLeft');
    grpPositioning.Items.Add('sphpCenter');
    grpPositioning.Items.Add('sphpRight');
    grpPositioning.Items.Add('sphpFill');
    grpPositioning.ItemIndex := Ord(StackPanel1.HorizontalPositioning) - 1;

    grpControlPositioning.Items.Clear;
    grpControlPositioning.Items.Add('sphpDefault');
    grpControlPositioning.Items.Add('sphpLeft');
    grpControlPositioning.Items.Add('sphpCenter');
    grpControlPositioning.Items.Add('sphpRight');
    grpControlPositioning.Items.Add('sphpFill');
    grpControlPositioning.ItemIndex := -1;
  end
  else
  begin
    grpPositioning.Caption := 'Vertical Positioning';
    grpPositioning.Items.Clear;
    grpPositioning.Items.Add('spvpTop');
    grpPositioning.Items.Add('spvpCenter');
    grpPositioning.Items.Add('spvpBottom');
    grpPositioning.Items.Add('spvpFill');
    grpPositioning.ItemIndex := Ord(StackPanel1.VerticalPositioning) - 1;

    grpControlPositioning.Items.Clear;
    grpControlPositioning.Items.Add('spvpDefault');
    grpControlPositioning.Items.Add('spvpTop');
    grpControlPositioning.Items.Add('spvpCenter');
    grpControlPositioning.Items.Add('spvpBottom');
    grpControlPositioning.Items.Add('spvpFill');
    grpControlPositioning.ItemIndex := -1;
  end;
end;

procedure TStackPanelForm.grpPositioningClick(Sender: TObject);
begin
  StackPanel1.DisableAlign; // Stop align.
  try
    ResetControl; // Revert width / height of all controls.
    // no sphpDefault/spvpDefault in this case. To skip sphpDefault/spvpDefault, 1 is added.
    if StackPanel1.Orientation = spoVertical then
      StackPanel1.HorizontalPositioning := TStackPanelHorizontalPositioning(grpPositioning.ItemIndex + 1)
    else
      StackPanel1.VerticalPositioning := TStackPanelVerticalPositioning(grpPositioning.ItemIndex + 1);
  finally
    StackPanel1.EnableAlign; // restart align with new parameter.
  end;
end;

procedure TStackPanelForm.trkSpacingChange(Sender: TObject);
begin
  StackPanel1.Spacing := trkSpacing.Position;
end;

procedure TStackPanelForm.trkPaddingChange(Sender: TObject);
var
  P: Integer;
begin
  P := trkPadding.Position;
  StackPanel1.Padding.SetBounds( P, P, P, P );
end;

procedure TStackPanelForm.ResetControl;
begin
  // Get the default height/width of the controls.
  Label1.Width  := DefaultLabel1Width;
  Label1.Height := DefaultLabel1Height;
  Edit1.Width   := DefaultEdit1Width;
  Edit1.Height  := DefaultEdit1Height;
  Button1.Width := DefaultButton1Width;
  Button1.Height:= DefaultButton1Height;
  Memo1.Width   := DefaultMemo1Width;
  Memo1.Height  := DefaultMemo1Height;
end;

procedure TStackPanelForm.lstControlsClick(Sender: TObject);
begin
  if StackPanel1.Orientation = spoVertical then
    grpControlPositioning.ItemIndex := Ord(StackPanel1.ControlCollection[lstControls.ItemIndex].HorizontalPositioning)
  else
    grpControlPositioning.ItemIndex := Ord(StackPanel1.ControlCollection[lstControls.ItemIndex].VerticalPositioning);
end;

procedure TStackPanelForm.grpControlPositioningClick(Sender: TObject);
begin
  StackPanel1.DisableAlign; // Stop align.
  try
    ResetControl; // Revert width / height of all controls.
    if StackPanel1.Orientation = spoVertical then
      StackPanel1.ControlCollection[lstControls.ItemIndex].HorizontalPositioning := TStackPanelControlHorizontalPositioning(grpControlPositioning.ItemIndex)
    else
      StackPanel1.ControlCollection[lstControls.ItemIndex].VerticalPositioning := TStackPanelControlVerticalPositioning(grpControlPositioning.ItemIndex);
  finally
    StackPanel1.EnableAlign; // restart align with new parameter.
  end;
end;

end.
