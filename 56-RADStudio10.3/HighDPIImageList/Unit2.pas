unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ToolWin, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  TForm2 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ImageList1: TImageList;
    Button2: TButton;
    ImageCollection1: TImageCollection;
    VirtualImageList1: TVirtualImageList;
    Button3: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button2Click(Sender: TObject);
begin
  ScaleBy(2, 1);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  if ToolBar1.Images = ImageList1 then begin
    ToolBar1.Images := VirtualImageList1;
    Button1.Images := VirtualImageList1;
  end else begin
    ToolBar1.Images := ImageList1;
    Button1.Images := ImageList1;
  end;
end;

end.
