unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, REST.Backend.ServiceTypes,
  System.JSON, REST.Backend.EMSProvider, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Client, REST.Backend.EndPoint,
  REST.Backend.EMSServices, Data.DB, REST.Backend.MetaTypes,
  REST.Backend.BindSource, REST.Backend.ServiceComponents, Vcl.Grids,
  Vcl.DBGrids, Vcl.StdCtrls, Vcl.ExtCtrls, REST.Response.Adapter,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait, FireDAC.Comp.UI, Vcl.DBCtrls;

type
  TMainForm = class(TForm)
    BackendEndpointGET: TBackendEndpoint;
    EMSProvider1: TEMSProvider;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    Panel1: TPanel;
    edtUser: TEdit;
    edtPass: TEdit;
    butLogin: TButton;
    Label1: TLabel;
    Label2: TLabel;
    DBGrid1: TDBGrid;
    BackendAuth1: TBackendAuth;
    butGetCountry: TButton;
    FDMemTable1: TFDMemTable;
    DataSource1: TDataSource;
    RESTResponse1: TRESTResponse;
    FDMemTable1COUNTRY: TStringField;
    FDMemTable1CURRENCY: TStringField;
    BackendEndpointACT: TBackendEndpoint;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    DBNavigator1: TDBNavigator;
    procedure butGetCountryClick(Sender: TObject);
    procedure FDMemTable1AfterPost(DataSet: TDataSet);
    procedure FDMemTable1AfterDelete(DataSet: TDataSet);
    procedure FDMemTable1BeforePost(DataSet: TDataSet);
    procedure butLoginClick(Sender: TObject);
  private
    { Private declarations }
    bInsert: boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses REST.Types, uJSONHelper;

procedure TMainForm.butGetCountryClick(Sender: TObject);
begin
  FDMemTable1.AfterPost := nil;
  FDMemTable1.AfterDelete := nil;
  try
    BackendEndpointGET.Execute;
    FDMemTable1.CommitUpdates;
  finally
    FDMemTable1.AfterPost := FDMemTable1AfterPost;
    FDMemTable1.AfterDelete := FDMemTable1AfterDelete;
  end;
end;

procedure TMainForm.butLoginClick(Sender: TObject);
begin
  BackendAuth1.UserName := edtUser.Text;
  BackendAuth1.Password := edtPass.Text;
  if not BackendAuth1.LoggedIn then
   begin
     BackendAuth1.Login;
     butLogin.Caption := 'Logout';
   end
  else
   begin
     BackendAuth1.Logout;
     butLogin.Caption := 'Login';
   end;
end;

procedure TMainForm.FDMemTable1AfterDelete(DataSet: TDataSet);
var
  LMemTable: TFDCustomMemTable;
begin
  LMemTable := MemTableCreateDelta(FDMemTable1);
  try
    try
      LMemTable.FilterChanges := [rtDeleted];

      BackendEndpointACT.ClearBody;
      BackendEndpointACT.Method := rmDELETE;
      BackendEndpointACT.ResourceSuffix := LMemTable.FieldByName('COUNTRY').AsString;
      BackendEndpointACT.Execute;

      FDMemTable1.CommitUpdates;
    except
      on E: Exception do
        raise Exception.Create('Error Message: ' + E.Message);
    end;
  finally
    LMemTable.Free;
  end;
end;

procedure TMainForm.FDMemTable1AfterPost(DataSet: TDataSet);
var
  LMemTable: TFDCustomMemTable;
begin
  LMemTable := MemTableCreateDelta(FDMemTable1);
  try
    try
      if bInsert then
      begin
        LMemTable.FilterChanges := [rtInserted];
        BackendEndpointACT.Method := rmPOST;
        BackendEndpointACT.ResourceSuffix := '';
      end
      else
      begin
        LMemTable.FilterChanges := [rtModified];
        BackendEndpointACT.Method := rmPUT;
        BackendEndpointACT.ResourceSuffix := LMemTable.FieldByName('COUNTRY').AsString;
      end;

      BackendEndpointACT.ClearBody;
      BackendEndpointACT.AddBody(LMemTable.AsJSONObject);
      BackendEndpointACT.Execute;

      FDMemTable1.CommitUpdates;
    except
      on E: Exception do
        raise Exception.Create('Error Message: ' + E.Message);
    end;
  finally
    LMemTable.Free;
  end;
end;

procedure TMainForm.FDMemTable1BeforePost(DataSet: TDataSet);
begin
  bInsert := DataSet.State = dsInsert;
end;

end.

