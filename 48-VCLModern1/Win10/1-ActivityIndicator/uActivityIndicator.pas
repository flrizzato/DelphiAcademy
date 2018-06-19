unit uActivityIndicator;

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
  Vcl.WinXCtrls, Vcl.StdCtrls, System.ImageList, Vcl.ImgList, Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  TActivityIndicatorForm = class(TForm)
    chkAnimate: TCheckBox;
    trkFrameDelay: TTrackBar;
    Label1: TLabel;
    grpIndicatorType: TRadioGroup;
    grpIndicatorSize: TRadioGroup;
    grpIndicatorColor: TRadioGroup;
    cbxVclStyles: TComboBox;
    Label2: TLabel;
    ActivityIndicator1: TActivityIndicator;
    procedure FormCreate(Sender: TObject);
    procedure chkAnimateClick(Sender: TObject);
    procedure trkFrameDelayChange(Sender: TObject);
    procedure grpIndicatorTypeClick(Sender: TObject);
    procedure grpIndicatorSizeClick(Sender: TObject);
    procedure grpIndicatorColorClick(Sender: TObject);
    procedure cbxVclStylesChange(Sender: TObject);
  private
    AI: TActivityIndicator;
  public
  end;

var
  ActivityIndicatorForm: TActivityIndicatorForm;

implementation

{$R *.dfm}

uses Vcl.Themes;

procedure TActivityIndicatorForm.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  AI := TActivityIndicator.Create( Self );
  AI.Parent := Self;
  AI.Top := 25;
  AI.Left := 290;

  for StyleName in TStyleManager.StyleNames do
    cbxVclStyles.Items.Add(StyleName);

  cbxVclStyles.ItemIndex := cbxVclStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;

procedure TActivityIndicatorForm.cbxVclStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle( cbxVclStyles.Text );
end;

procedure TActivityIndicatorForm.grpIndicatorColorClick(Sender: TObject);
begin
  AI.IndicatorColor := TActivityIndicatorColor(grpIndicatorColor.ItemIndex);
end;

procedure TActivityIndicatorForm.grpIndicatorSizeClick(Sender: TObject);
begin
  AI.IndicatorSize := TActivityIndicatorSize(grpIndicatorSize.ItemIndex);
end;

procedure TActivityIndicatorForm.grpIndicatorTypeClick(Sender: TObject);
begin
  AI.IndicatorType := TActivityIndicatorType(grpIndicatorType.ItemIndex);
end;

procedure TActivityIndicatorForm.trkFrameDelayChange(Sender: TObject);
begin
  AI.FrameDelay := trkFrameDelay.Position * 10;
end;

procedure TActivityIndicatorForm.chkAnimateClick(Sender: TObject);
begin
  AI.Animate := chkAnimate.Checked;
end;


end.
