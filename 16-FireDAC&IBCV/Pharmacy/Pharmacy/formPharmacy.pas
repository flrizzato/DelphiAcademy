//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit formPharmacy;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Vcl.ComCtrls, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.DBScope, Vcl.OleCtrls, SHDocVw, Vcl.Samples.Spin,
  Data.Bind.Controls, Vcl.Buttons, Vcl.Bind.Navigator, dmLocalData, Data.DB,
  Vcl.Grids, Vcl.DBGrids, frameData;

type
  TfrmPharmacy = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    edtSite: TLabeledEdit;
    edtCentralServer: TLabeledEdit;
    Splitter1: TSplitter;
    bsCategory: TBindSourceDB;
    BindingsList1: TBindingsList;
    lvCategory: TListView;
    LinkListControlToField2: TLinkListControlToField;
    lvMedicine: TListView;
    bsMedicines: TBindSourceDB;
    LinkListControlToField1: TLinkListControlToField;
    MemoPATIENT_ADVICE: TMemo;
    LinkControlToFieldPATIENT_ADVICE: TLinkControlToField;
    MemoSPECIAL_WARNINGS: TMemo;
    LinkControlToFieldSPECIAL_WARNINGS: TLinkControlToField;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Splitter2: TSplitter;
    lvCustomers: TListView;
    bsCustomer: TBindSourceDB;
    LinkListControlToField3: TLinkListControlToField;
    Panel4: TPanel;
    MemoCUSTOMER_ADDRESS: TMemo;
    LinkControlToFieldCUSTOMER_ADDRESS: TLinkControlToField;
    LabeledEditCUSTOMER_NAME: TLabeledEdit;
    LinkControlToFieldCUSTOMER_NAME: TLinkControlToField;
    gbCustomer: TGroupBox;
    Label1: TLabel;
    Splitter3: TSplitter;
    GroupBox3: TGroupBox;
    bsOrder: TBindSourceDB;
    Panel5: TPanel;
    btnAddOrder: TButton;
    PageControl2: TPageControl;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    WebBrowser1: TWebBrowser;
    btnWebdetailsLoad: TButton;
    lblURL: TLabel;
    Label2: TLabel;
    bsPharmacy: TBindSourceDB;
    LinkControlToField1: TLinkControlToField;
    LinkPropertyToFieldCaption: TLinkPropertyToField;
    seQuantity: TSpinEdit;
    NavigatorBindSourceDB3: TBindNavigator;
    BindNavigator1: TBindNavigator;
    gridOrders: TDBGrid;
    TabSheet3: TTabSheet;
    PageControl3: TPageControl;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    FrameMedicine: TFrame1;
    FrameCategory: TFrame1;
    FrameLinks: TFrame1;
    Panel3: TPanel;
    btnFetchDeltas: TButton;
    btnPostDeltas: TButton;
    Panel6: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    cbShowMerged: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Controller : TComponent;
    dtmdLocalDB : TdtmdLocalDB;
  public
    { Public declarations }
  end;

var
  frmPharmacy: TfrmPharmacy;

implementation

{$R *.dfm}

uses formPharmacyController;

procedure TfrmPharmacy.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Controller);
  FreeAndNil(dtmdLocalDB);
end;

procedure TfrmPharmacy.FormCreate(Sender: TObject);
begin
  dtmdLocalDB := TdtmdLocalDB.Create(Self);
  Controller := TdmPharmacyController.Create(Self,dtmdLocalDB);
end;

end.
