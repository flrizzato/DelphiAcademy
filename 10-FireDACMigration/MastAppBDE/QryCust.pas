unit QryCust;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TQueryCustDlg = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    FromEdit: TEdit;
    ToEdit: TEdit;
    CancelBtn: TButton;
    OkBtn: TButton;
    Msglab: TLabel;
    PopupCalBtnFrom: TSpeedButton;
    PopupCalToBtn: TSpeedButton;
    Bevel1: TBevel;
    procedure OkBtnClick(Sender: TObject);
    procedure PopupCalBtnFromClick(Sender: TObject);
    procedure PopupCalToBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetFromDate: TDateTime;
    function GetToDate: TDateTime;
    procedure SetFromDate(NewDate: TDateTime);
    procedure SetToDate(NewDate: TDateTime);
  public
    property FromDate: TDateTime read GetFromDate write SetFromDate;
    property ToDate: TDateTime read GetToDate write SetToDate;
  end;

var
  QueryCustDlg: TQueryCustDlg;

implementation

{$R *.dfm}

uses Pickdate;

procedure TQueryCustDlg.SetFromDate(NewDate: TDateTime);
begin
  FromEdit.Text := DateToStr(NewDate);
end;

procedure TQueryCustDlg.SetToDate(NewDate: TDateTime);
begin
  ToEdit.Text := DateToStr(NewDate);
end;

function TQueryCustDlg.GetFromDate: TDateTime;
begin
  if FromEdit.Text = '' then Result := 0
  else Result := StrToDate(FromEdit.Text);
end;

function TQueryCustDlg.GetToDate: TDateTime;
begin
  if ToEdit.Text = '' then Result := 0
  else Result := StrToDate(ToEdit.Text);
end;

procedure TQueryCustDlg.OkBtnClick(Sender: TObject);
begin
  try
    if (ToDate <> 0) and (ToDate < FromDate) then
    begin
      ShowMessage('"TO" date cannot be less than "FROM" date');
      ModalResult := mrNone;
    end
    else ModalResult := mrOk;
  except
    ShowMessage('  Invalid date specified');
    ModalResult := mrNone;
  end;
end;

procedure TQueryCustDlg.PopupCalBtnFromClick(Sender: TObject);
begin
  BrDateForm.Date := StrToDate(FromEdit.Text);  { start with current date }
  if BrDateForm.ShowModal = mrOk then
    FromEdit.Text := DateToStr(BrDateForm.Date);
end;

procedure TQueryCustDlg.PopupCalToBtnClick(Sender: TObject);
begin
  BrDateForm.Date := StrToDate(ToEdit.Text);    { start with current date }
  if BrDateForm.ShowModal = mrOk then
    ToEdit.Text := DateToStr(BrDateForm.Date);
end;

procedure TQueryCustDlg.FormCreate(Sender: TObject);
begin
  MsgLab.Caption := 'Customers with LastInvoiceDate ranging:';
  FromDate := EncodeDate(1995, 01, 01);
  ToDate := Now;
end;

end.
