unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    edtHostName: TEdit;
    DataSource2: TDataSource;
    DBGrid2: TDBGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses ClientClassesUnit1, ClientModuleUnit1, Datasnap.DBClient;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ClientModule1.SQLConnection1.Params.Values['HostName'] :=
    edtHostName.Text;
  TClientDataSet(DataSource1.DataSet).Open;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  TClientDataSet(DataSource1.DataSet).ApplyUpdates(-1);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  TClientDataSet(DataSource1.DataSet).CancelUpdates;
end;

end.
