//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit SurfSpots;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, FMX.Edit, Data.Bind.Components, Data.Bind.DBScope,
  FMX.Layouts, FMX.ListBox, REST.Response.Adapter, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, REST.Client, Data.Bind.ObjectScope,
  FMX.ListView.Types, FMX.ListView, FMX.StdCtrls, FMX.TabControl, FMX.WebBrowser,
  System.Sensors, FMX.Memo, System.Actions, FMX.ActnList, FMX.MultiView,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation;

type
  TSurfSpotFinder = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    FDMemTable1: TFDMemTable;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    AllSurfSpotsListView: TListView;
    LinkListControlToField1: TLinkListControlToField;
    LinkFillControlToField2: TLinkFillControlToField;
    WebBrowser1: TWebBrowser;
    SurfSpotDetailsToolbar: TToolBar;
    SpotName: TLabel;
    LinkPropertyToFieldText: TLinkPropertyToField;
    SurfSpotInfoList: TListBox;
    LatitudeItem: TListBoxItem;
    LongitudeItem: TListBoxItem;
    LinkPropertyToFieldItemDataText: TLinkPropertyToField;
    LinkPropertyToFieldItemDataText2: TLinkPropertyToField;
    CountyItem: TListBoxItem;
    LinkPropertyToFieldItemDataText3: TLinkPropertyToField;
    AllSurfSpotsToolbar: TToolBar;
    AllSurfSpotsToollabel: TLabel;
    backbtn: TSpeedButton;
    LinkFillControlToField3: TLinkFillControlToField;
    LinkFillControlToField4: TLinkFillControlToField;
    FavoritesListBox: TListBox;
    LinkListControlToField2: TLinkListControlToField;
    FavoritesToolbar: TToolBar;
    FavoritesToollabel: TLabel;
    backtoHome: TSpeedButton;
    GotoFavorites: TSpeedButton;
    SaveasFavorite: TSpeedButton;
    MultiView1: TMultiView;
    MultiView2: TMultiView;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FavoritesListBoxItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure SaveasFavoriteClick(Sender: TObject);
    procedure AllSurfSpotsListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure backtoHomeClick(Sender: TObject);
    procedure MultiView1Hidden(Sender: TObject);
    procedure MultiView1StartShowing(Sender: TObject);
    procedure MultiView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    procedure UpdateWebBrowserVisibility;
  public
    { Public declarations }
  end;

var
  SurfSpotFinder: TSurfSpotFinder;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

uses
System.IOUtils;

procedure TSurfSpotFinder.AllSurfSpotsListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
const
  LGoogleMapsURL: String = 'https://maps.google.com/maps?q=%s,%s';

begin
  WebBrowser1.Navigate(Format(LGoogleMapsURL, [LatitudeItem.Text, LongitudeItem.Text]));
  MultiView1.HideMaster;
end;

procedure TSurfSpotFinder.FormCreate(Sender: TObject);
begin
  RESTRequest1.Execute;
  if FileExists(TPath.GetDocumentsPath + PathDelim + 'myfile.txt') then
    FavoritesListBox.Items.LoadfromFile(TPath.GetDocumentsPath + PathDelim + 'myfile.txt');
end;

procedure TSurfSpotFinder.FormShow(Sender: TObject);
begin
  RESTRequest1.Execute;
end;

procedure TSurfSpotFinder.MultiView1Hidden(Sender: TObject);
begin
  UpdateWebBrowserVisibility;
end;

procedure TSurfSpotFinder.MultiView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  UpdateWebBrowserVisibility;
end;

procedure TSurfSpotFinder.MultiView1StartShowing(Sender: TObject);
begin
  if Sender = MultiView2 then
    MultiView1.HideMaster;
  UpdateWebBrowserVisibility;
end;

procedure TSurfSpotFinder.backtoHomeClick(Sender: TObject);
begin
  MultiView2.HideMaster;
end;

procedure TSurfSpotFinder.UpdateWebBrowserVisibility;
begin
  WebBrowser1.Visible := not (MultiView1.MasterContent.AbsoluteRect.IntersectsWith(WebBrowser1.AbsoluteRect) or
    MultiView2.MasterContent.AbsoluteRect.IntersectsWith(WebBrowser1.AbsoluteRect));
end;

procedure TSurfSpotFinder.FavoritesListBoxItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  LPosLock: IScopeLocks;
begin
  Supports(BindSourceDB1, IScopeLocks, LPosLock);
  LPosLock.PosLockEnter;
  try
    FDMemTable1.Locate('spot_name', Item.Text);
  finally
    LPosLock.PosLockLeave;
  end;
  MultiView2.HideMaster;
end;

procedure TSurfSpotFinder.SaveasFavoriteClick(Sender: TObject);
begin
  if FavoritesListBox.Items.IndexOf(SpotName.Text) < 0 then
  begin
    FavoritesListBox.Items.Add(Spotname.Text);
    ShowMessage('Saved as Favorite');
    FavoritesListBox.Items.SaveToFile(TPath.GetDocumentsPath + PathDelim + 'myfile.txt');
  end
  else
    ShowMessage('Item already saved as Favorite');
end;


end.


