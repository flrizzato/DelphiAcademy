unit CommonDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ExtDlgs, Jpeg, PngIMage,
  Vcl.Themes, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    Button1: TButton;
    FontDialog1: TFontDialog;
    OpenDialog1: TOpenDialog;
    PrintDialog1: TPrintDialog;
    Button2: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    OpenTextFileDialog1: TOpenTextFileDialog;
    FindDialog1: TFindDialog;
    PageSetupDialog1: TPageSetupDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ReplaceDialog1: TReplaceDialog;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button10: TButton;
    Label1: TLabel;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button10Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ColorDialog1.Execute;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  OpenDialog1.Execute;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  PrintDialog1.Execute;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  PageSetupDialog1.Execute;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  FontDialog1.Execute;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  OpenTextFileDialog1.Execute;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  OpenPictureDialog1.Execute;
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
  TStyleManager.SetStyle(ComboBox1.Items[ComboBox1.ItemIndex]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Clear;
  ComboBox1.Items.AddStrings(TStyleManager.StyleNames);
  ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;


end.
