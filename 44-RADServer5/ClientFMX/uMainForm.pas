unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  IPPeerClient, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.UI.Intf, FireDAC.Comp.UI, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Response.Adapter, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, System.Rtti,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, Data.Bind.DBScope, System.Actions, FMX.ActnList, FMX.Edit,
  FMX.ListBox, FMX.Layouts, Data.Bind.Controls, FMX.Bind.Navigator,
  FireDAC.FMXUI.Wait, FMX.Objects, FMX.Ani;

type
  TMainForm = class(TForm)
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
    RESTMethods: TRESTRequest;
    RESTMethodsResponse: TRESTResponse;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    ActionList1: TActionList;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    LinkListControlToField1: TLinkListControlToField;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkControlToField6: TLinkControlToField;
    LinkControlToField7: TLinkControlToField;
    LinkControlToField8: TLinkControlToField;
    LinkControlToField9: TLinkControlToField;
    LinkControlToField10: TLinkControlToField;
    LinkControlToField11: TLinkControlToField;
    LinkControlToField12: TLinkControlToField;
    Button1: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Rectangle1: TRectangle;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    LiveBindingsBindNavigateInsert1: TFMXBindNavigateInsert;
    LiveBindingsBindNavigateDelete1: TFMXBindNavigateDelete;
    LiveBindingsBindNavigatePost1: TFMXBindNavigatePost;
    LiveBindingsBindNavigateCancel1: TFMXBindNavigateCancel;
    CalloutRectangle1: TCalloutRectangle;
    Label1: TLabel;
    FloatAnimation1: TFloatAnimation;
    procedure ListView1ItemClickEx(const Sender: TObject; ItemIndex: Integer;
      const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure ListView1PullRefresh(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FDMemTable1AfterDelete(DataSet: TDataSet);
    procedure FDMemTable1AfterPost(DataSet: TDataSet);
    procedure FDMemTable1BeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    bInsert: boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  REST.Types, uJSONHelper, qdac_fmx_vkhelper;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  PreviousTabAction1.Execute;
end;

procedure TMainForm.FDMemTable1AfterDelete(DataSet: TDataSet);
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

procedure TMainForm.FDMemTable1BeforePost(DataSet: TDataSet);
begin
  bInsert := DataSet.State = dsInsert;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ListView1PullRefresh(Self);
{$ELSE}
  FloatAnimation1.Enabled := True;
  FloatAnimation1.Start;
{$ENDIF}
end;

procedure TMainForm.ListView1ItemClickEx(const Sender: TObject;
  ItemIndex: Integer; const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable);
begin
  if ItemObject is TListItemAccessory then
    NextTabAction1.Execute;
end;

procedure TMainForm.ListView1PullRefresh(Sender: TObject);
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

end.
