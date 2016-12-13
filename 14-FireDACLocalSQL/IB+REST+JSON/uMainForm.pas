unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Data.DBXInterBase, Data.FMTBcd, IPPeerClient, IBX.IBDatabase, Vcl.DBLogDlg,
  IBX.IBCustomDataSet, IBX.IBTable, Datasnap.DBClient, REST.Response.Adapter,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, Data.DB,
  Data.SqlExpr, FMX.StdCtrls, System.Rtti, FMX.Bind.Grid,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Data.Bind.Grid, FMX.Layouts,
  FMX.Grid, Data.Bind.DBScope, FireDAC.Stan.StorageJSON,
  FireDAC.Stan.StorageBin,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait, FireDAC.DApt,
  FireDAC.Phys.SQLiteVDataSet, FireDAC.Comp.UI, Data.Win.ADODB, FMX.Memo,
  FMX.Grid.Style, FMX.ScrollBox, FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    DbxConnection: TSQLConnection;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTAdapter: TRESTResponseDataSetAdapter;
    CDSREST: TClientDataSet;
    IBDatabase1: TIBDatabase;
    SalesTable: TIBTable;
    IBTransaction1: TIBTransaction;
    CustomersTable: TSQLTable;
    RestSalesReps: TFDMemTable;
    CDSRESTid: TWideStringField;
    CDSRESTname: TWideStringField;
    CDSRESTusername: TWideStringField;
    CDSRESTemail: TWideStringField;
    CDSRESTaddress: TWideStringField;
    CDSRESTphone: TWideStringField;
    CDSRESTwebsite: TWideStringField;
    CDSRESTcompany: TWideStringField;
    RestSalesRepsRestID: TIntegerField;
    RestSalesRepsSales_REP: TIntegerField;
    FDConnection1: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDLocalSQL1: TFDLocalSQL;
    FDQuery1: TFDQuery;
    RegionsTable: TADOQuery;
    ADOConnection1: TADOConnection;
    Button1: TButton;
    RegionsTableRegionID: TIntegerField;
    RegionsTableRegionDescription: TWideStringField;
    BindSourceDB1: TBindSourceDB;
    GridBindSourceDB1: TGrid;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    BindingsList1: TBindingsList;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  RestSalesReps.EmptyDataSet;
  RestSalesReps.Open;
  RestSalesReps.AppendRecord([1, 11]);
  RestSalesReps.AppendRecord([2, 61]);
  RestSalesReps.AppendRecord([3, 127]);
  RestSalesReps.AppendRecord([4, 72]);
  RestSalesReps.AppendRecord([5, 134]);

  FDQuery1.Open;
end;

end.
