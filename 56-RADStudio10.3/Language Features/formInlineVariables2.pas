unit formInlineVariables2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmInlineVariables2 = class(TForm)
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
    btnPrev: TButton;
    btnTry: TButton;
    btnNext: TButton;
    StyleBook1: TStyleBook;
    procedure FormShow(Sender: TObject);
    procedure btnTryClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmInlineVariables2: TfrmInlineVariables2;

implementation

{$R *.fmx}

uses formInlineVariables3, formInlineVariables1;

procedure TfrmInlineVariables2.btnNextClick(Sender: TObject);
begin
  Application.MainForm := frmInlineVariables3;
  frmInlineVariables3.Left := Self.Left;
  frmInlineVariables3.Top := Self.Top;
  frmInlineVariables3.Width := Self.Width;
  frmInlineVariables3.Height := Self.Height;
  frmInlineVariables3.Show;
  Self.Hide;
end;

procedure TfrmInlineVariables2.btnPrevClick(Sender: TObject);
begin
  Application.MainForm := frmInlineVariables1;
  frmInlineVariables1.Left := Self.Left;
  frmInlineVariables1.Top := Self.Top;
  frmInlineVariables1.Width := Self.Width;
  frmInlineVariables1.Height := Self.Height;
  frmInlineVariables1.Show;
  Self.Hide;
end;

procedure TfrmInlineVariables2.btnTryClick(Sender: TObject);
begin
  if 1=1 then begin
    var idx: int32;
    idx := 5;
    MemoResult.Lines.Add('idx = '+IntToStr(idx));
  end;
  //- idx may not be used here ;
end;

procedure TfrmInlineVariables2.FormShow(Sender: TObject);
begin
  Caption := lblTitle.Text;
  wbShowCode.Navigate(mmohtml.Text);
end;

end.
