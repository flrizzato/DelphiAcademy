unit BrCstOrd;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DBTables, DB, DBCtrls, StdCtrls, ExtCtrls, Grids, DBGrids, Buttons;

type
  TBrCustOrdForm = class(TForm)
    CtrlsPanel: TPanel;
    Navigator: TDBNavigator;
    BtnPanel: TPanel;
    EditBtn: TButton;
    CloseBtn: TButton;
    ActivateBtn: TSpeedButton;
    DefineBtn: TSpeedButton;
    CustPanel: TPanel;
    CustGrid: TDBGrid;
    OrdersPanel: TPanel;
    OrdersGrid: TDBGrid;
    Bevel1: TBevel;
    procedure CustGridEnter(Sender: TObject);
    procedure SetQuery(Sender: TObject);
    procedure ActivateQuery(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure OrdersGridEnter(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FActiveSource: TDataSource;
    function GetCustNo: Double;
    function GetOrderNo: Double;
    procedure SetActiveSource(DataSource: TDataSource);
    procedure SetCustNo(NewCustNo: Double);
    procedure SetOrderNo(NewOrderNo: Double);
    property ActiveSource: TDataSource read FActiveSource write SetActiveSource;
  public
    property CustNo: Double read GetCustNo write SetCustNo;
    property OrderNo: Double read GetOrderNo write SetOrderNo;
  end;

var
  BrCustOrdForm: TBrCustOrdForm;

implementation

uses QryCust, EdCust, EdOrders, Main, DataMod;

{$R *.dfm}

{ Retrieve from active customer table or query--whichever is active }

function TBrCustOrdForm.GetCustNo: Double;
begin
  Result := MastData.CustMasterSrc.Dataset.Fields[0].AsFloat;
end;

procedure TBrCustOrdForm.SetCustNo(NewCustNo: Double);
begin
  MastData.CustMasterSrc.Dataset := MastData.Cust;
  MastData.Cust.Locate('CustNo', NewCustNo, []);
end;

function TBrCustOrdForm.GetOrderNo: Double;
begin
  Result := MastData.OrdByCustOrderNo.Value;
end;

procedure TBrCustOrdForm.SetOrderNo(NewOrderNo: Double);
begin
  MastData.OrdByCust.Locate('OrderNo', NewOrderNo, []);
end;

procedure TBrCustOrdForm.SetActiveSource(DataSource: TDataSource);
begin
  FActiveSource := DataSource;
  Navigator.DataSource := FActiveSource;
end;

procedure TBrCustOrdForm.CustGridEnter(Sender: TObject);
begin
  ActiveSource := MastData.CustMasterSrc;
  CustGrid.Options := CustGrid.Options + [dgAlwaysShowSelection];
  OrdersGrid.Options := OrdersGrid.Options - [dgAlwaysShowSelection];
end;

procedure TBrCustOrdForm.OrdersGridEnter(Sender: TObject);
begin
  ActiveSource := MastData.OrdByCustSrc;
  OrdersGrid.Options := OrdersGrid.Options + [dgAlwaysShowSelection];
  CustGrid.Options := CustGrid.Options - [dgAlwaysShowSelection];
end;

procedure TBrCustOrdForm.SetQuery(Sender: TObject);
begin
  if QueryCustDlg.ShowModal = mrOK then
    ActivateQuery(Self);
end;

procedure TBrCustOrdForm.ActivateQuery(Sender: TObject);
begin
  if not ActivateBtn.Down then
    MastData.CustMasterSrc.Dataset := MastData.Cust
  else
    with MastData.CustQuery do
    try
      Close;
      Params[0].AsDatetime := QueryCustDlg.FromDate;
      Params[1].AsDatetime := QueryCustDlg.ToDate;
      Open;
      { Any records in the result set? }
      if BOF and EOF then Abort;
      MastData.CustMasterSrc.Dataset := MastData.CustQuery;
    except
      MastData.CustMasterSrc.Dataset := MastData.Cust;
      ActivateBtn.Down := false;
      ShowMessage('No matching records in the specified date range.');
    end;
end;

procedure TBrCustOrdForm.EditBtnClick(Sender: TObject);
var
  F: TFloatField;
begin
  F := ActiveSource.Dataset.Fields[0] as TFloatField;
  if ActiveSource = MastData.CustMasterSrc then
    EdCustForm.Edit(F.Value)
  else
  begin
    EdOrderForm.Edit(F.Value);
    ActiveSource.Dataset.Refresh;
  end;
end;

procedure TBrCustOrdForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TBrCustOrdForm.FormShow(Sender: TObject);
begin
  MastData.Cust.Open;
  MastData.Cust.First;
end;

end.
