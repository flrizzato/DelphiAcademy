unit uToggleSwitch;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.WinXCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  System.Actions,
  Vcl.ActnList;

type
  TToggleSwitchForm = class(TForm)
    chkEnabled: TCheckBox;
    cbxVclStyles: TComboBox;
    edtCaptionOff: TEdit;
    edtCaptionOn: TEdit;
    lblCaptionOn: TLabel;
    lblCaptionOff: TLabel;
    grpStateCaptions: TGroupBox;
    lblVclStyle: TLabel;
    chkShowStateCaptions: TCheckBox;
    grpAlignment: TRadioGroup;
    TS: TToggleSwitch;
    grpState: TRadioGroup;
    chkReadOnly: TCheckBox;
    grpColors: TGroupBox;
    lblColor: TLabel;
    cbxColor: TColorBox;
    lblThumbColor: TLabel;
    cbxThumbColor: TColorBox;
    lblFrameColor: TLabel;
    cbxFrameColor: TColorBox;
    procedure FormCreate(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure cbxVclStylesChange(Sender: TObject);
    procedure edtCaptionOffChange(Sender: TObject);
    procedure edtCaptionOnChange(Sender: TObject);
    procedure chkShowStateCaptionsClick(Sender: TObject);
    procedure grpAlignmentClick(Sender: TObject);
    procedure cbxThumbColorChange(Sender: TObject);
    procedure cbxFrameColorChange(Sender: TObject);
    procedure cbxColorChange(Sender: TObject);
    procedure grpStateClick(Sender: TObject);
    procedure chkReadOnlyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ToggleSwitchForm: TToggleSwitchForm;

implementation

uses
  Vcl.Themes;

{$R *.dfm}

procedure TToggleSwitchForm.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  for StyleName in TStyleManager.StyleNames do
    cbxVclStyles.Items.Add(StyleName);

  cbxVclStyles.ItemIndex := cbxVclStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;

procedure TToggleSwitchForm.cbxVclStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle(cbxVclStyles.Text);
end;

procedure TToggleSwitchForm.grpStateClick(Sender: TObject);
begin
  TS.State := TToggleSwitchState(grpState.ItemIndex);
end;

procedure TToggleSwitchForm.chkEnabledClick(Sender: TObject);
begin
  TS.Enabled := chkEnabled.Checked;
end;

procedure TToggleSwitchForm.chkReadOnlyClick(Sender: TObject);
begin
  TS.ReadOnly := chkReadOnly.Checked;
end;

procedure TToggleSwitchForm.chkShowStateCaptionsClick(Sender: TObject);
begin
  TS.ShowStateCaption := chkShowStateCaptions.Checked;
end;

procedure TToggleSwitchForm.edtCaptionOffChange(Sender: TObject);
begin
  TS.StateCaptions.CaptionOff := edtCaptionOff.Text;
end;

procedure TToggleSwitchForm.edtCaptionOnChange(Sender: TObject);
begin
  TS.StateCaptions.CaptionOn := edtCaptionOn.Text;
end;

procedure TToggleSwitchForm.grpAlignmentClick(Sender: TObject);
begin
  TS.Alignment := TLeftRight(grpAlignment.ItemIndex);
end;

procedure TToggleSwitchForm.cbxColorChange(Sender: TObject);
begin
  TS.Color := cbxColor.Selected;
end;

procedure TToggleSwitchForm.cbxFrameColorChange(Sender: TObject);
begin
  TS.FrameColor := cbxFrameColor.Selected;
end;

procedure TToggleSwitchForm.cbxThumbColorChange(Sender: TObject);
begin
  TS.ThumbColor := cbxThumbColor.Selected;
end;

end.
