unit EDOrders;

{ See the comments in MAIN.PAS for information about this project }

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Dialogs, Forms, 
  StdCtrls, DBGrids, DBCtrls, DB, Buttons, Grids, ExtCtrls, Mask;

type
  TEdOrderForm = class(TForm)
    HeaderPanel: TPanel;
    ShipToAdd1Edit: TDBEdit;
    ShipToAdd2Edit: TDBEdit;
    CustAdd1Edit: TDBEdit;
    CustAdd2Edit: TDBEdit;
    ShipToCompanyEdit: TDBEdit;
    CustCityEdit: TDBEdit;
    CustStateEdit: TDBEdit;
    CustZipEdit: TDBEdit;
    ShipToCityEdit: TDBEdit;
    ShipToStateEdit: TDBEdit;
    ShipToZipEdit: TDBEdit;
    ModeIndicator: TLabel;
    POEdit: TDBEdit;
    TermsCombo: TDBComboBox;
    PaymentCombo: TDBComboBox;
    ShipViaCombo: TDBComboBox;
    Speedbar: TPanel;
    DBNavBtns: TDBNavigator;
    SaleDateEdit: TDBEdit;
    DBEditBtns: TDBNavigator;
    ActiveSource: TDataSource;
    ItemsGrid: TDBGrid;
    AmountPaidEdit: TDBEdit;
    TotalEdit: TDBEdit;
    TaxTotalEdit: TDBEdit;
    FreightEdit: TDBEdit;
    AmountDueEdit: TDBEdit;
    TaxRateEdit: TDBEdit;
    CloseBtn: TButton;
    CancelBtn: TButton;
    PostBtn: TButton;
    PopupCalBtn: TSpeedButton;
    Image1: TImage;
    PrintBtn: TSpeedButton;
    SoldByCombo: TDBLookupComboBox;
    Bevel1: TBevel;
    CompanyCombo: TDBLookupComboBox;
    DBText1: TDBText;
    CustNoEdit: TDBEdit;
    procedure ItemsGridEnter(Sender: TObject);
    procedure ActiveSourceStateChange(Sender: TObject);
    procedure ItemsGridExit(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure PostBtnClick(Sender: TObject);
    procedure PickPartNo(Sender: TObject);
    procedure PickDate(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure SaleDateEditKeyPress(Sender: TObject; var Key: Char);
    procedure OrdersSourceStateChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SoldByComboKeyPress(Sender: TObject; var Key: Char);
  public
    procedure Enter;
    procedure Edit(OrderNo: Double);
  end;

var
  EdOrderForm: TEdOrderForm;

implementation

uses
  DataMod, SrchDlg, Pickdate;

{$R *.dfm}

const
  DatasetStates: array[TDataSetState] of string =
    ('Not active', 'Browsing', 'Editing', 'Inserting', '', '', '', '', '', '', '', '','');
  HelpTopicEdit = 2;
  HelpTopicBrowse = 3;


{ ======================= Public Methods ======================= }

procedure TEdOrderForm.Enter;
begin
  MastData.OrdersSource.OnStateChange := OrdersSourceStateChange;
  try
    MastData.Orders.Open;
    MastData.Orders.Insert;
    ShowModal;
  finally
    MastData.OrdersSource.OnStateChange := nil;
  end;
end;

procedure TEdOrderForm.Edit(OrderNo: Double);
begin
  MastData.OrdersSource.OnStateChange := OrdersSourceStateChange;
  try
    MastData.Orders.Open;
    MastData.Orders.Locate('OrderNo', OrderNo, []);
    ShowModal;
  finally
    MastData.OrdersSource.OnStateChange := nil;
  end;
end;

{ ======================  Event Handlers  ====================== }

{ These two methods enable the navigators to service both the Orders
  and Items tables by switching the ActiveSource between them. }

procedure TEdOrderForm.ItemsGridEnter(Sender: TObject);
begin
  ActiveSource.Dataset := MastData.Items;
end;

procedure TEdOrderForm.ItemsGridExit(Sender: TObject);
begin
  ActiveSource.Dataset := MastData.Orders;
end;

{ Update the mode indicator when the state of the "Active" datasource
  (Orders or Items) changes }

procedure TEdOrderForm.ActiveSourceStateChange(Sender: TObject);
begin
  with ActiveSource do
  begin
    if Dataset <> nil then ModeIndicator.Caption :=
      Format('[%S: %S]', [Dataset.Name, DatasetStates[State]]);
    if State in [dsEdit, dsInsert] then
    begin
      HelpContext := HelpTopicEdit;
      ModeIndicator.Font.Color := clRed;
    end
    else
    begin
      HelpContext := HelpTopicBrowse;
      ModeIndicator.Font.Color := clBlue;
    end;
  end;
end;

{ Enable or disable buttons as needed when the state of the orders table changes}

procedure TEdOrderForm.OrdersSourceStateChange(Sender: TObject);
begin
  PostBtn.Enabled := MastData.Orders.State in dsEditModes;
  CancelBtn.Enabled := PostBtn.Enabled;
  CloseBtn.Enabled := MastData.Orders.State = dsBrowse;
end;

{ Browse a calendar to pick an invoice date }

procedure TEdOrderForm.PickDate(Sender: TObject);
begin
  BrDateForm.Date := MastData.OrdersSaleDate.Value; 	{ start with current date }
  if BrDateForm.ShowModal = mrOk then
  begin
    MastData.Orders.Edit;
    MastData.OrdersSaleDate.Value := BrDateForm.Date;
    SaleDateEdit.SelectAll;
  end;
end;

{ Ctrl+Enter in the SaleDate edit control brings up PickDate dialog }

procedure TEdOrderForm.SaleDateEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^J then
  begin
    PickDate(Sender);
    Key := #0;
  end;
end;

{ Clicking on the PartNo button in the grid brings up PickPartNo dialog }

procedure TEdOrderForm.PickPartNo(Sender: TObject);
begin
  if ItemsGrid.SelectedField = MastData.ItemsPartNo then	{ PartNo column only }
  begin
    if MastData.ItemsPartNo.Value <> 0 then
      SearchDlg.PartNo := MastData.ItemsPartNo.Value;	{ start with current PartNo }
    if SearchDlg.ShowModalParts = mrOk then
    begin
      MastData.Items.Edit;
      MastData.ItemsPartNo.Value := SearchDlg.PartNo;
    end;
  end;
end;

{ Begins a series of cascading Before and After post events }

procedure TEdOrderForm.PostBtnClick(Sender: TObject);
begin
  MastData.Orders.Post;
end;

{ Cancels insert or edit on the Orders table }

procedure TEdOrderForm.CancelBtnClick(Sender: TObject);
begin
  MastData.OrdersAfterCancel(MastData.Orders);
end;

{ Prints snapshot of the form. An invoice report is available via the Main window }

procedure TEdOrderForm.PrintBtnClick(Sender: TObject);
begin
  if Confirm('Print image of this form window?') then Print;
end;

procedure TEdOrderForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := MastData.DataSetApplyUpdates(MastData.Orders, ModalResult = mrOK);
end;

procedure TEdOrderForm.SoldByComboKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in [#13, #27]) then
    Key := #0;
end;

end.
