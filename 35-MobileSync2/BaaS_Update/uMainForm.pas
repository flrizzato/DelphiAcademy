unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids,
  IPPeerClient, REST.Backend.ServiceTypes, REST.Backend.MetaTypes, System.JSON,
  REST.Backend.ParseProvider, REST.Backend.Providers,
  REST.Backend.ServiceComponents, REST.Backend.ParseServices;

type
  TMainForm = class(TForm)
    FDConnection1: TFDConnection;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    FDQuery1: TFDQuery;
    DataSource1: TDataSource;
    FDQuery1PARTNO: TFloatField;
    FDQuery1VENDORNO: TFloatField;
    FDQuery1DESCRIPTION: TStringField;
    FDQuery1ONHAND: TFloatField;
    FDQuery1ONORDER: TFloatField;
    FDQuery1COST: TFloatField;
    FDQuery1LISTPRICE: TFloatField;
    FDQuery1OBJECTID: TStringField;
    ParseProvider1: TParseProvider;
    BackendStorage1: TBackendStorage;
    procedure FormCreate(Sender: TObject);
    procedure FDQuery1AfterPost(DataSet: TDataSet);
  private
    { Private declarations }
    procedure CreateOrUpdateParts;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses DataSetConverter4D.Helper, DataSetConverter4D.Impl, System.IOUtils;

{$R *.dfm}

procedure TMainForm.CreateOrUpdateParts;
var
  LEntity: TBackendEntityValue;
begin
  if FDQuery1OBJECTID.AsString.IsEmpty then
  begin
    BackendStorage1.Storage.CreateObject('parts',
      FDQuery1.AsJSONObject, LEntity);
    FDQuery1.Edit;
    FDQuery1OBJECTID.AsString := LEntity.ObjectID;
    FDQuery1.Post;
  end
  else
  begin
    BackendStorage1.Storage.UpdateObject('parts', FDQuery1OBJECTID.AsString,
      FDQuery1.AsJSONObject, LEntity);
  end;

  FDQuery1.ApplyUpdates(-1);
end;

procedure TMainForm.FDQuery1AfterPost(DataSet: TDataSet);
begin
  CreateOrUpdateParts;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDQuery1.Open;
  ParseProvider1.BaseURL := 'https://parseapi.back4app.com/';
end;

end.
