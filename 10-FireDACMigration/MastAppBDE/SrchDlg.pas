unit SrchDlg;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DBTables, DB, StdCtrls, ExtCtrls, Grids, DBGrids, Buttons;

type
  TSearchDlg = class(TForm)
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    OKBtn: TButton;
    CancelBtn: TButton;
    SearchEd: TEdit;
    OrderCombo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    SearchButton: TSpeedButton;
    procedure DBGrid1DblClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure OrderComboChange(Sender: TObject);
    procedure SearchEdKeyPress(Sender: TObject; var Key: Char);
    procedure SearchEdChange(Sender: TObject);
  private
    SrchFld: TField;
    function GetCustNo: Double;
    procedure SetCustNo(NewCustNo: Double);
    function GetPartNo: Double;
    procedure SetPartNo(NewPartNo: Double);
  public
    property PartNo: Double read GetPartNo write SetPartNo;
    property CustNo: Double read GetCustNo write SetCustNo;
    function ShowModalCust: Integer;
    function ShowModalParts: Integer;
  end;

var
  SearchDlg: TSearchDlg;

implementation

uses DataMod;

{$R *.dfm}

function TSearchDlg.GetCustNo: Double;
begin
  Result := MastData.CustCustNo.Value;
end;

procedure TSearchDlg.SetCustNo(NewCustNo: Double);
begin
  MastData.Cust.Locate('CustNo', NewCustNo, []);
end;

function TSearchDlg.GetPartNo: Double;
begin
  Result := MastData.PartsPartNo.Value;
end;

procedure TSearchDlg.SetPartNo(NewPartNo: Double);
begin
  MastData.Parts.Locate('PartNo', NewPartNo, []);
end;

function TSearchDlg.ShowModalCust: Integer;
begin
  OrderCombo.Items.Clear;
  OrderCombo.Items.Add('Company');
  OrderCombo.Items.Add('CustNo');
  OrderCombo.ItemIndex := 0;
  Datasource.Dataset := MastData.Cust;
  OrderComboChange(nil);
  Caption := 'Select a Customer';
  Result := ShowModal;
end;

function TSearchDlg.ShowModalParts: Integer;
begin
  OrderCombo.Items.Clear;
  OrderCombo.Items.Add('Description');
  OrderCombo.Items.Add('PartNo');
  OrderCombo.ItemIndex := 0;
  Datasource.Dataset := MastData.Parts;
  OrderComboChange(nil);
  Caption := 'Select a Part';
  Result := ShowModal;
end;

procedure TSearchDlg.DBGrid1DblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSearchDlg.SearchButtonClick(Sender: TObject);
begin
  if not Datasource.Dataset.Locate(OrderCombo.Text, SearchEd.Text,
    [loCaseInsensitive, loPartialKey]) then
      MessageDlg('No matching record found.', mtInformation, [mbOK], 0);
end;

procedure TSearchDlg.OrderComboChange(Sender: TObject);
begin
  SrchFld := Datasource.Dataset.FieldByName(OrderCombo.Text);
  SearchEd.Text := '';
end;

procedure TSearchDlg.SearchEdKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(SrchFld) and (Key > ' ') and not (SrchFld.IsValidChar(Key)) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
end;

procedure TSearchDlg.SearchEdChange(Sender: TObject);
begin
  SearchButton.Enabled := SearchEd.Text <> '';
end;

end.
