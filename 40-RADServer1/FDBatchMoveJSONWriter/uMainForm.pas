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
    DBGrid1: TDBGrid;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    DataSource1: TDataSource;
    Button1: TButton;
    FDBatchMove1: TFDBatchMove;
    FDBatchMoveDataSetReader1: TFDBatchMoveDataSetReader;
    FDBatchMoveJSONWriter1: TFDBatchMoveJSONWriter;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    SalesTable: TFDQuery;
    SalesTablePO_NUMBER: TStringField;
    SalesTableCUST_NO: TIntegerField;
    SalesTableSALES_REP: TSmallintField;
    SalesTableORDER_STATUS: TStringField;
    SalesTableORDER_DATE: TSQLTimeStampField;
    SalesTableSHIP_DATE: TSQLTimeStampField;
    SalesTableDATE_NEEDED: TSQLTimeStampField;
    SalesTablePAID: TStringField;
    SalesTableQTY_ORDERED: TIntegerField;
    SalesTableTOTAL_VALUE: TCurrencyField;
    SalesTableDISCOUNT: TSingleField;
    SalesTableITEM_TYPE: TStringField;
    SalesTableAGED: TFMTBCDField;
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

uses uJSONHelper, System.JSON.Types;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SalesTable.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  sstr: TStringStream;
begin
  JSONFormatSettings.ShortDateFormat := 'dd/mm/yy';

  sstr := TStringStream.Create;
  SalesTable.DisableControls;
  try
    FDBatchMoveJSONWriter1.Stream := sstr;
    FDBatchMove1.Execute;
    Memo1.Lines.Text := sstr.DataString;
  finally
    sstr.Free;
    SalesTable.First;
    SalesTable.EnableControls;
  end
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Text := SalesTable.AsJSONArray;
end;

end.
