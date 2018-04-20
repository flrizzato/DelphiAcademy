unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  IPPeerClient, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Response.Adapter, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.DBScope, System.Actions, FMX.ActnList;

type
  TForm2 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ListView1: TListView;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1: TFDMemTable;
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
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    LinkListControlToField1: TLinkListControlToField;
    procedure Button1Click(Sender: TObject);
    procedure ListView1ItemClickEx(const Sender: TObject; ItemIndex: Integer;
      const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  REST.Types, uJSONHelper;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  FDMemTable1.AfterPost := nil;
  FDMemTable1.AfterDelete := nil;
  try
    RESTRequest1.Method := rmGET;
    RESTRequest1.Execute;
    FDMemTable1.CommitUpdates;
  finally
    //FDMemTable1.AfterPost := FDMemTable1AfterPost;
    //FDMemTable1.AfterDelete := FDMemTable1AfterDelete;
  end;

end;

procedure TForm2.ListView1ItemClickEx(const Sender: TObject; ItemIndex: Integer;
  const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
begin
  if ItemObject.Name = 'A' then
    NextTabAction1.Execute;

end;

end.
