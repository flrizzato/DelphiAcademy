unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, Vcl.DBGrids, Vcl.DBCtrls;

type
  TForm1 = class(TForm)
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    edtHostName: TEdit;
    DBGrid2: TDBGrid;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses ClientModuleUnit2, DBClient;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClientModule2.DSRestCnn.Host := edtHostName.Text;
  ClientModule2.LoadCustomersAndSales;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ClientModule2.SaveCustomersAndSales;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TClientDataSet(DataSource1.DataSet).CancelUpdates;
  TClientDataSet(DataSource2.DataSet).CancelUpdates;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ClientModule2.SaveCustomersAndSalesManual;
end;

end.
