//---------------------------------------------------------------------------

// This software is Copyright (c) 2017 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uDateTimePickers;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.WinXPickers, Vcl.WinXCalendars, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TDateTimePickersForm = class(TForm)
    lblVclStyle: TLabel;
    cbxVclStyles: TComboBox;
    chkShowOkCancel: TCheckBox;
    Label1: TLabel;
    grpColors: TGroupBox;
    lblColor: TLabel;
    lblHighlightColor: TLabel;
    lblPopupColor: TLabel;
    lblSelectionColor: TLabel;
    lblSelectionFontColor: TLabel;
    cbxColor: TColorBox;
    cbxHighlightColor: TColorBox;
    cbxPopupColor: TColorBox;
    cbxSelectionColor: TColorBox;
    cbxSelectionFontColor: TColorBox;
    lblFontColor: TLabel;
    cbxFontColor: TColorBox;
    DatePicker1: TDatePicker;
    TimePicker1: TTimePicker;
    spnDropDownCount: TUpDown;
    edtDropDownCount: TEdit;
    lblHotColor: TLabel;
    cbxHotColor: TColorBox;
    cbxDateFormats: TComboBox;
    cbxTimeFormats: TComboBox;
    lblDateFormats: TLabel;
    lblTimeFormats: TLabel;
    lblMinuteIncrement: TLabel;
    cbxMinuteIncrements: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure cbxVclStylesChange(Sender: TObject);
    procedure chkShowOkCancelClick(Sender: TObject);
    procedure spnDropDownCountClick(Sender: TObject; Button: TUDBtnType);
    procedure cbxColorChange(Sender: TObject);
    procedure cbxFontColorChange(Sender: TObject);
    procedure cbxHighlightColorChange(Sender: TObject);
    procedure cbxPopupColorChange(Sender: TObject);
    procedure cbxSelectionColorChange(Sender: TObject);
    procedure cbxSelectionFontColorChange(Sender: TObject);
    procedure cbxHotColorChange(Sender: TObject);
    procedure cbxDateFormatsChange(Sender: TObject);
    procedure cbxTimeFormatsChange(Sender: TObject);
    procedure cbxMinuteIncrementsChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DateTimePickersForm: TDateTimePickersForm;

implementation

{$R *.dfm}

uses
  Vcl.Themes;

procedure TDateTimePickersForm.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  for StyleName in TStyleManager.StyleNames do
    cbxVclStyles.Items.Add(StyleName);

  cbxVclStyles.ItemIndex := cbxVclStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;

procedure TDateTimePickersForm.cbxVclStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle(cbxVclStyles.Text);
end;

procedure TDateTimePickersForm.spnDropDownCountClick(Sender: TObject; Button: TUDBtnType);
begin
  DatePicker1.DropDownCount := spnDropDownCount.Position;
  TimePicker1.DropDownCount := spnDropDownCount.Position;
end;

procedure TDateTimePickersForm.chkShowOkCancelClick(Sender: TObject);
begin
  DatePicker1.ShowOkCancel := chkShowOkCancel.Checked;
  TimePicker1.ShowOkCancel := chkShowOkCancel.Checked;
end;

procedure TDateTimePickersForm.cbxDateFormatsChange(Sender: TObject);
begin
  DatePicker1.DateFormat := cbxDateFormats.Text;
end;

procedure TDateTimePickersForm.cbxTimeFormatsChange(Sender: TObject);
begin
  TimePicker1.TimeFormat := cbxTimeFormats.Text;
end;

procedure TDateTimePickersForm.cbxMinuteIncrementsChange(Sender: TObject);
begin
  TimePicker1.MinuteIncrement := StrToIntDef(cbxMinuteIncrements.Text, 1);
end;

procedure TDateTimePickersForm.cbxColorChange(Sender: TObject);
begin
  DatePicker1.Color := cbxColor.Selected;
  TimePicker1.Color := cbxColor.Selected;
end;

procedure TDateTimePickersForm.cbxFontColorChange(Sender: TObject);
begin
  DatePicker1.Font.Color := cbxFontColor.Selected;
  TimePicker1.Font.Color := cbxFontColor.Selected;
end;

procedure TDateTimePickersForm.cbxHotColorChange(Sender: TObject);
begin
  DatePicker1.HotColor := cbxHotColor.Selected;
  TimePicker1.HotColor := cbxHotColor.Selected;
end;

procedure TDateTimePickersForm.cbxHighlightColorChange(Sender: TObject);
begin
  DatePicker1.HighlightColor := cbxHighlightColor.Selected;
  TimePicker1.HighlightColor := cbxHighlightColor.Selected;
end;

procedure TDateTimePickersForm.cbxPopupColorChange(Sender: TObject);
begin
  DatePicker1.PopupColor := cbxPopupColor.Selected;
  TimePicker1.PopupColor := cbxPopupColor.Selected;
end;

procedure TDateTimePickersForm.cbxSelectionColorChange(Sender: TObject);
begin
  DatePicker1.SelectionColor := cbxSelectionColor.Selected;
  TimePicker1.SelectionColor := cbxSelectionColor.Selected;
end;

procedure TDateTimePickersForm.cbxSelectionFontColorChange(Sender: TObject);
begin
  DatePicker1.SelectionFontColor := cbxSelectionFontColor.Selected;
  TimePicker1.SelectionFontColor := cbxSelectionFontColor.Selected;
end;


end.
