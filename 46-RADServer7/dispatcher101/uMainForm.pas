unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, Vcl.ExtCtrls, Vcl.DBCtrls,
  Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  IPPeerClient, Vcl.StdCtrls, REST.Backend.EMSProvider, REST.Backend.PushTypes,
  REST.Backend.MetaTypes, System.JSON, REST.Backend.EMSServices,
  Data.Bind.Components, Data.Bind.ObjectScope, REST.Backend.BindSource,
  REST.Backend.ServiceComponents;

type
  TMainForm = class(TForm)
    EmployeeConnection: TFDConnection;
    SalesTable: TFDQuery;
    DataSource1: TDataSource;
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
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    EMSProvider1: TEMSProvider;
    Panel1: TPanel;
    Button1: TButton;
    BackendPush1: TBackendPush;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
var
  Data: TPushData;
begin
  Data := TPushData.Create;
  try
    Data.GCM.Title    := 'Compra aprobada #' + SalesTablePO_NUMBER.AsString;
    Data.GCM.Message  := 'La compra con el valor de ' + SalesTableTOTAL_VALUE.AsString +
     ' fue aprobada';
    BackEndPush1.PushData(Data);
  finally
    Data.Free;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SalesTable.Open;
end;

end.
