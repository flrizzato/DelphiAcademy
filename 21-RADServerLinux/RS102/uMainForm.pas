unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, REST.Backend.EMSServices,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Stan.Param, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Backend.EMSFireDAC,
  REST.Backend.EMSProvider, Vcl.ToolWin, Vcl.ComCtrls, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, Vcl.Bind.Grid, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Vcl.Bind.Navigator, Data.Bind.Components, Data.Bind.Grid, Vcl.Grids,
  Data.Bind.DBScope;

type
  TForm1 = class(TForm)
    EMSProvider1: TEMSProvider;
    EMSFireDACClient1: TEMSFireDACClient;
    FDSchemaAdapter1: TFDSchemaAdapter;
    FDTableAdapter1: TFDTableAdapter;
    FDMemTable1: TFDMemTable;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    StringGridBindSourceDB1: TStringGrid;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    NavigatorBindSourceDB1: TBindNavigator;
    Panel1: TPanel;
    butUpdateCustomers: TButton;
    butGetCustomers: TButton;
    procedure butGetCustomersClick(Sender: TObject);
    procedure butUpdateCustomersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

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

procedure TForm1.butGetCustomersClick(Sender: TObject);
begin
  EMSFireDACClient1.GetData;
  GridAutoSizeCols(StringGridBindSourceDB1);
end;

procedure TForm1.butUpdateCustomersClick(Sender: TObject);
begin
  EMSFireDACClient1.PostUpdates;
end;

end.
