unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.VCLUI.Wait, Data.DB, Vcl.StdCtrls, FireDAC.Comp.UI,
  FireDAC.Phys.IBBase, Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Comp.BatchMove.JSON, FireDAC.Comp.BatchMove,
  FireDAC.Comp.BatchMove.DataSet, FireDAC.Stan.ExprFuncs;

type
  TForm1 = class(TForm)
    EmployeeConnection: TFDConnection;
    ProjectTable: TFDQuery;
    DBGrid1: TDBGrid;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    DataSource1: TDataSource;
    ProjectTablePROJ_ID: TStringField;
    ProjectTablePROJ_NAME: TStringField;
    ProjectTablePROJ_DESC: TMemoField;
    ProjectTableTEAM_LEADER: TSmallintField;
    ProjectTablePRODUCT: TStringField;
    Button1: TButton;
    FDBatchMove1: TFDBatchMove;
    FDBatchMoveDataSetReader1: TFDBatchMoveDataSetReader;
    FDBatchMoveJSONWriter1: TFDBatchMoveJSONWriter;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses uJSONHelper;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ProjectTable.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  sstr: TStringStream;
begin
  sstr := TStringStream.Create;
  try
    ProjectTable.DisableControls;
    FDBatchMoveJSONWriter1.Stream := sstr;
    FDBatchMove1.Execute;
    Memo1.Lines.Text := sstr.DataString;
  finally
    sstr.Free;
    ProjectTable.EnableControls;
  end
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Text := ProjectTable.AsJSONArray;
end;

end.
