unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Vcl.StdCtrls, Vcl.ExtCtrls, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids,
  Datasnap.DBClient, FireDACJSONReflect, System.JSON;

type
  TForm1 = class(TForm)
    EmployeeConnection: TFDConnection;
    SalesTable: TFDQuery;
    Panel1: TPanel;
    butDSJSON: TButton;
    memJSON: TMemo;
    dbgJSON: TDBGrid;
    SalesTableDS: TDataSource;
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
    procedure butDSJSONClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses DataSetConverter4D.Helper, DataSetConverter4D.Impl;

procedure TForm1.butDSJSONClick(Sender: TObject);
begin
  SalesTable.Open;
  try
    SalesTable.DisableControls;
    memJSON.Text := SalesTable.AsJSONArrayString;
  finally
    SalesTable.EnableControls;
  end;
end;

end.
