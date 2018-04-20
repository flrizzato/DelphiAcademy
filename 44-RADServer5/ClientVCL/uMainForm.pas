unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ExtCtrls, Vcl.DBCtrls,
  Vcl.Grids, Vcl.DBGrids, IPPeerClient, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  REST.Response.Adapter, Vcl.StdCtrls, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Comp.UI;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    Panel1: TPanel;
    Button1: TButton;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1: TFDMemTable;
    DataSource1: TDataSource;
    FDMemTable1CUST_NO: TFDAutoIncField;
    FDMemTable1CUSTOMER: TStringField;
    FDMemTable1CONTACT_FIRST: TStringField;
    FDMemTable1CONTACT_LAST: TStringField;
    FDMemTable1PHONE_NO: TStringField;
    FDMemTable1ADDRESS_LINE1: TStringField;
    FDMemTable1ADDRESS_LINE2: TStringField;
    FDMemTable1CITY: TStringField;
    FDMemTable1STATE_PROVINCE: TStringField;
    FDMemTable1COUNTRY: TStringField;
    FDMemTable1POSTAL_CODE: TStringField;
    FDMemTable1ON_HOLD: TStringField;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    RESTMethods: TRESTRequest;
    RESTMethodsResponse: TRESTResponse;
    procedure Button1Click(Sender: TObject);
    procedure FDMemTable1AfterPost(DataSet: TDataSet);
    procedure FDMemTable1AfterDelete(DataSet: TDataSet);
    procedure FDMemTable1BeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    bInsert: boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  REST.Types, uJSONHelper;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FDMemTable1.AfterPost := nil;
  FDMemTable1.AfterDelete := nil;
  try
    RESTRequest1.Method := rmGET;
    RESTRequest1.Execute;
    FDMemTable1.CommitUpdates;
  finally
    FDMemTable1.AfterPost := FDMemTable1AfterPost;
    FDMemTable1.AfterDelete := FDMemTable1AfterDelete;
  end;
end;

procedure TForm1.FDMemTable1BeforePost(DataSet: TDataSet);
begin
  bInsert := DataSet.State = dsInsert;
end;

procedure TForm1.FDMemTable1AfterPost(DataSet: TDataSet);
var
  LMemTable: TFDCustomMemTable;
begin
  LMemTable := MemTableCreateDelta(FDMemTable1);
  try
    try
      if bInsert then
      begin
        LMemTable.FilterChanges := [rtInserted];
        RESTMethods.Method := rmPOST;
        RESTMethods.ResourceSuffix := '';
      end
      else
      begin
        LMemTable.FilterChanges := [rtModified];
        RESTMethods.Method := rmPUT;
        RESTMethods.ResourceSuffix := LMemTable.FieldByName('CUST_NO').AsString;
      end;

      RESTMethods.ClearBody;
      RESTMethods.AddBody(LMemTable.AsJSONObject);
      RESTMethods.Execute;

      FDMemTable1.CommitUpdates;
    except
      on E: Exception do
        raise Exception.Create('Error Message: ' + E.Message);
    end;
  finally
    LMemTable.Free;
  end;
end;

procedure TForm1.FDMemTable1AfterDelete(DataSet: TDataSet);
var
  LMemTable: TFDCustomMemTable;
begin
  LMemTable := MemTableCreateDelta(FDMemTable1);
  try
    try
      LMemTable.FilterChanges := [rtDeleted];

      RESTMethods.ClearBody;
      RESTMethods.Method := rmDELETE;
      RESTMethods.ResourceSuffix := LMemTable.FieldByName('CUST_NO').AsString;
      RESTMethods.Execute;

      FDMemTable1.CommitUpdates;
    except
      on E: Exception do
        raise Exception.Create('Error Message: ' + E.Message);
    end;
  finally
    LMemTable.Free;
  end;
end;

end.
