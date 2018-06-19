unit uSearchBox;

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
  Vcl.ExtCtrls;

type
  TSearchBoxForm = class(TForm)
    grpSearchIndicator: TRadioGroup;
    Label2: TLabel;
    cbxVclStyles: TComboBox;
    lstLog: TListBox;
    Label1: TLabel;
    chkEnabled: TCheckBox;
    grpBiDiMode: TRadioGroup;
    lblSearchIndicatorHelp: TLabel;
    SB: TSearchBox;
    procedure FormCreate(Sender: TObject);
    procedure grpSearchIndicatorClick(Sender: TObject);
    procedure cbxVclStylesChange(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure grpBiDiModeClick(Sender: TObject);
    procedure SBInvokeSearch(Sender: TObject);
  private
    procedure Log( const Msg: string );
  public
  end;

var
  SearchBoxForm: TSearchBoxForm;

implementation

uses
  Vcl.Themes;

{$R *.dfm}

const
  IndicatorHelpText: array[TSearchBoxIndicator] of string =
    ('Enter text. Press Enter or click the search text indicator to generate OnInvokeSearch event.',
     'Click the audio indicator to generate OnInvokeSearch event.');

procedure TSearchBoxForm.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  lblSearchIndicatorHelp.Caption := IndicatorHelpText[ SB.SearchIndicator ];

  for StyleName in TStyleManager.StyleNames do
    cbxVclStyles.Items.Add(StyleName);

  cbxVclStyles.ItemIndex := cbxVclStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;

procedure TSearchBoxForm.cbxVclStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle(cbxVclStyles.Text);
end;

procedure TSearchBoxForm.grpSearchIndicatorClick(Sender: TObject);
begin
  SB.SearchIndicator := TSearchBoxIndicator(grpSearchIndicator.ItemIndex);
  lblSearchIndicatorHelp.Caption := IndicatorHelpText[ SB.SearchIndicator ];
end;

procedure TSearchBoxForm.chkEnabledClick(Sender: TObject);
begin
  SB.Enabled := chkEnabled.Checked;
end;

procedure TSearchBoxForm.grpBiDiModeClick(Sender: TObject);
begin
  SB.BiDiMode := TBiDiMode(grpBiDiMode.ItemIndex);
end;

procedure TSearchBoxForm.Log(const Msg: string);
var
  Idx: Integer;
begin
  Idx := lstLog.Items.Add(Msg);
  lstLog.TopIndex := Idx;
end;

procedure TSearchBoxForm.SBInvokeSearch(Sender: TObject);
begin
  if SB.SearchIndicator = sbiText then
    Log('Text: OnInvokeSearch - "' + SB.Text + '"')
  else
    Log('Audio: OnInvokeSearch');
end;

end.
