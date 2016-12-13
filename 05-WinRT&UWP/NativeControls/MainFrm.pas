
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Advertising, FMX.Layouts, FMX.ListBox, FMX.StdCtrls,
  FMX.Calendar, FMX.Controls.Presentation, FMX.Edit, FMX.TabControl, FMX.ScrollBox, FMX.Memo, FMX.ListView.Types,
  FMX.ListView, Data.Bind.Components, Data.Bind.ObjectScope, Fmx.Bind.GenData, Data.Bind.GenData, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.MultiView, FMX.Objects, FMX.Colors, FMX.WebBrowser;

type
  TFormMain = class(TForm)
    Calendar1: TCalendar;
    TabControl1: TTabControl;
    TabItemCalendar: TTabItem;
    TabItemListView: TTabItem;
    Label1: TLabel;
    TabItemMemoFontSettings: TTabItem;
    Label9: TLabel;
    Memo2: TMemo;
    ListView1: TListView;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    Layout1: TLayout;
    ToolBar1: TToolBar;
    Label11: TLabel;
    TabItemMenu: TTabItem;
    ListViewMenu: TListView;
    TabItemSwitch: TTabItem;
    Label5: TLabel;
    Switch1: TSwitch;
    TabItemEditTextAlign: TTabItem;
    Edit4: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    Label12: TLabel;
    Edit7: TEdit;
    Label13: TLabel;
    TabItemEditCursorColor: TTabItem;
    Edit1: TEdit;
    Label4: TLabel;
    Label14: TLabel;
    Edit8: TEdit;
    TabItemEditCustomBackground: TTabItem;
    Edit9: TEdit;
    Label15: TLabel;
    Rectangle1: TRectangle;
    Image1: TImage;
    ClearEditButton1: TClearEditButton;
    TabItemEditFonstSettings: TTabItem;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit13: TEdit;
    TabItemEditClearButton: TTabItem;
    Edit3: TEdit;
    ClearEditButton2: TClearEditButton;
    Label3: TLabel;
    Edit12: TEdit;
    ClearEditButton3: TClearEditButton;
    Edit5: TEdit;
    Label7: TLabel;
    TabItemEditPrompt: TTabItem;
    Edit2: TEdit;
    Label2: TLabel;
    SpeedButtonBackToMenu: TSpeedButton;
    TabItemMemoDetectingLinks: TTabItem;
    Memo3: TMemo;
    Label10: TLabel;
    TabItemMemoCheckSpelling: TTabItem;
    Memo4: TMemo;
    TabItemScrollBox: TTabItem;
    PresentedScrollBox1: TPresentedScrollBox;
    TrackBar1: TTrackBar;
    ColorQuad1: TColorQuad;
    ColorPicker1: TColorPicker;
    TabItemMemoCustomBackground: TTabItem;
    Image2: TImage;
    Rectangle2: TRectangle;
    Memo5: TMemo;
    TabItemWebBrowser: TTabItem;
    WebBrowser1: TWebBrowser;
    TabItemMemoCursorColor: TTabItem;
    Label8: TLabel;
    Label16: TLabel;
    Memo1: TMemo;
    Memo6: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonBackToMenuClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure ListViewMenuItemClickEx(const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF;
      const ItemObject: TListItemDrawable);
  private
    FMenu: TDictionary<TListViewItem, TTabItem>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.Math;

{$R *.fmx}

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FMenu := TDictionary<TListViewItem, TTabItem>.Create;
end;

destructor TFormMain.Destroy;
begin
  FreeAndNil(FMenu);
  inherited;
end;

procedure TFormMain.FormCreate(Sender: TObject);

  procedure AddItem(const AText: string; const ATabItem: TTabItem; const APurpose: TListItemPurpose = TListItemPurpose.None);
  var
    Item: TListViewItem;
  begin
    Item := ListViewMenu.Items.Add;
    Item.Text := AText;
    Item.Purpose := APurpose;
    if Item.Purpose = TListItemPurpose.Header then
      Item.Objects.TextObject.Font.Style := [TFontStyle.fsBold];
    FMenu.Add(Item, ATabItem);
  end;

var
  I: Integer;
  Item: TListViewItem;
begin
  ListViewMenu.BeginUpdate;
  ListViewMenu.Items.Clear;
  try
    AddItem('Edit', nil, TListItemPurpose.Header);
{$IFDEF IOS}
    AddItem('- Prompt', TabItemEditPrompt);
    AddItem('- Color cursor', TabItemEditCursorColor);
    AddItem('- Clear button', TabItemEditClearButton);
    AddItem('- Custom background', TabItemEditCustomBackground);
{$ENDIF}
    AddItem('- Text alignment', TabItemEditTextAlign);
    AddItem('- Font settings', TabItemEditFonstSettings);

    AddItem('Memo', nil, TListItemPurpose.Header);
{$IFDEF IOS}
    AddItem('- Color cursor', TabItemMemoCursorColor);
    AddItem('- Detecting phones, links, address, events', TabItemMemoDetectingLinks);
    AddItem('- Check spelling', TabItemMemoCheckSpelling);
    AddItem('- Custom background', TabItemMemoCustomBackground);
{$ENDIF}
    AddItem('- Font settings', TabItemMemoFontSettings);

{$IFDEF IOS}
    AddItem('Calendar', nil, TListItemPurpose.Header);
    AddItem('- Calendar', TabItemCalendar);

    AddItem('List View', nil, TListItemPurpose.Header);
    AddItem('- Headers and indicator', TabItemListView);
{$ENDIF}

    AddItem('ScrollBox', nil, TListItemPurpose.Header);
    AddItem('- Custom content size', TabItemScrollBox);

{$IFDEF IOS}
    AddItem('Switch', nil, TListItemPurpose.Header);
    AddItem('- Switch', TabItemSwitch);
{$ENDIF}

    AddItem('Web Browser', nil, TListItemPurpose.Header);
    AddItem('- Web Browser', TabItemWebBrowser);
  finally
    TabControl1.EndUpdate;
  end;
end;

procedure TFormMain.ListViewMenuItemClickEx(const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable);
var
  TabItem: TTabItem;
begin
  if FMenu.TryGetValue(ListViewMenu.Items[ItemIndex], TabItem) then
    TabControl1.ActiveTab := TabItem;
end;

procedure TFormMain.SpeedButtonBackToMenuClick(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItemMenu;
end;

procedure TFormMain.TabControl1Change(Sender: TObject);
begin
  SpeedButtonBackToMenu.Visible := TabControl1.ActiveTab <> TabItemMenu;
end;

end.
