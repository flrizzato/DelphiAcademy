unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.Bind.Controls, IPPeerClient,
  REST.Backend.EMSServices, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  Data.Bind.EngExt, Vcl.Bind.DBEngExt, Vcl.Bind.Grid, System.Rtti,
  System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, Data.Bind.Grid, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Backend.EMSFireDAC,
  REST.Backend.EMSProvider, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.Bind.Navigator, Vcl.Grids;

type
  TMainForm = class(TForm)
    StringGridBindSourceDB1: TStringGrid;
    NavigatorBindSourceDB1: TBindNavigator;
    Panel1: TPanel;
    butUpdateCustomers: TButton;
    butGetCustomers: TButton;
    EMSProvider1: TEMSProvider;
    EMSFireDACClient1: TEMSFireDACClient;
    FDSchemaAdapter1: TFDSchemaAdapter;
    FDTableAdapter1: TFDTableAdapter;
    FDMemTable1: TFDMemTable;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    BindSourceDB1: TBindSourceDB;
    butGetOneCustomer: TButton;
    procedure butGetCustomersClick(Sender: TObject);
    procedure butUpdateCustomersClick(Sender: TObject);
    procedure butGetOneCustomerClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure GridAutoSizeCols(Grid: TStringGrid);
var
  c: integer;

  procedure AutoSizeCol(Grid: TStringGrid; Column: integer);
  var
    i, W, WMax: integer;
  begin
    WMax := 0;
    for i := 0 to (Grid.RowCount - 1) do
    begin
      W := Grid.Canvas.TextWidth(Grid.Cells[Column, i]);
      if W > WMax then
        WMax := W;
    end;
    Grid.ColWidths[Column] := WMax + 10;
  end;

begin
  for c := 0 to Grid.ColCount - 1 do
    AutoSizeCol(Grid, c);
end;

procedure TMainForm.butGetCustomersClick(Sender: TObject);
begin
  EMSFireDACClient1.GetEndpoint.Params.Clear;
  EMSFireDACClient1.GetData;
  GridAutoSizeCols(StringGridBindSourceDB1);
end;

procedure TMainForm.butGetOneCustomerClick(Sender: TObject);
begin
  EMSFireDACClient1.GetEndpoint.Params.AddItem('CUST_NO', '1001');
  EMSFireDACClient1.GetData;
  GridAutoSizeCols(StringGridBindSourceDB1);
end;

procedure TMainForm.butUpdateCustomersClick(Sender: TObject);
begin
  EMSFireDACClient1.PostUpdates;
end;

end.
