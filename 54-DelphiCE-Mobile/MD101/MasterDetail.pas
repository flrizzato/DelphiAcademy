unit MasterDetail;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, Data.Bind.GenData,
  Fmx.Bind.GenData, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Objects, FMX.StdCtrls, FMX.ListView, FMX.ListView.Appearances,
  FMX.Layouts, FMX.MultiView,FMX.Memo, Fmx.Bind.Navigator, System.Actions, FMX.ActnList,
  FMX.ListView.Adapters.Base, FMX.ScrollBox, FMX.Controls.Presentation;

type
  TMasterDetailForm = class(TForm)
    MultiView1: TMultiView;
    Layout1: TLayout;
    ListView1: TListView;
    MasterToolbar: TToolBar;
    MasterLabel: TLabel;
    DetailToolbar: TToolBar;
    DetailLabel: TLabel;
    MasterButton: TSpeedButton;
    imgContact: TImage;
    lblName: TLabel;
    lblTitle: TLabel;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkPropertyToFieldBitmap: TLinkPropertyToField;
    LinkPropertyToFieldText: TLinkPropertyToField;
    LinkPropertyToFieldText2: TLinkPropertyToField;
    Layout2: TLayout;
    Layout3: TLayout;
    Memo1: TMemo;
    LinkControlToField1: TLinkControlToField;
    LinkListControlToField1: TLinkListControlToField;
    ActionList1: TActionList;
    LiveBindingsBindNavigateNext1: TFMXBindNavigateNext;
    LiveBindingsBindNavigatePrior1: TFMXBindNavigatePrior;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MasterDetailForm: TMasterDetailForm;

implementation

{$R *.fmx}
{$R *.iPad.fmx IOS}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TMasterDetailForm.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
begin
  MultiView1.HideMaster;
end;

end.
