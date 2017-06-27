unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, FMX.Effects;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    Button1: TButton;
    StyleBook1: TStyleBook;
    GridPanelLayout1: TGridPanelLayout;
    SearchLayout: TLayout;
    SearchIcon: TImage;
    SearchLabel: TLabel;
    BarcodeLayout: TLayout;
    BarcodeIcon: TImage;
    BarcodeLabel: TLabel;
    StoreDealsLayout: TLayout;
    StoreDealsIcon: TImage;
    StoreDealsLabel: TLabel;
    BookmarkedOffersLayout: TLayout;
    BookmarksIcon: TImage;
    BookmarkedOffersLabel: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
{$R *.NmXhdpiPh.fmx ANDROID}

end.
