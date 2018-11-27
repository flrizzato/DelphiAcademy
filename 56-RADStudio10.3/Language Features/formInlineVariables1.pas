unit formInlineVariables1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmInlineVariables1 = class(TForm)
    lotScale: TScaledLayout;
    lotTryMe: TLayout;
    lotMain: TLayout;
    lotDescription: TLayout;
    lblTitle: TLabel;
    Text1: TText;
    wbShowCode: TWebBrowser;
    mmohtml: TMemo;
    MemoResult: TMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    pnlButtons: TPanel;
    btnTry: TButton;
    btnNext: TButton;
    StyleBook1: TStyleBook;
    procedure FormShow(Sender: TObject);
    procedure btnTryClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmInlineVariables1: TfrmInlineVariables1;

implementation

{$R *.fmx}

uses formInlineVariables2;

procedure TfrmInlineVariables1.btnNextClick(Sender: TObject);
begin
  Application.MainForm := frmInlineVariables2;
  frmInlineVariables2.Left := Self.Left;
  frmInlineVariables2.Top := Self.Top;
  frmInlineVariables2.Width := Self.Width;
  frmInlineVariables2.Height := Self.Height;
  frmInlineVariables2.Show;
  Self.Hide;
end;

procedure TfrmInlineVariables1.btnTryClick(Sender: TObject);
begin
  var idx: int32;
  idx := 12;
  MemoResult.Lines.Add('idx = '+IntToStr(idx));
end;

procedure TfrmInlineVariables1.FormShow(Sender: TObject);
begin
  Caption := lblTitle.Text;
  wbShowCode.Navigate(mmohtml.Text);
end;

end.
