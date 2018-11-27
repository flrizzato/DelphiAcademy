unit formTypeInference1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmTypeInference1 = class(TForm)
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
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnTryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTypeInference1: TfrmTypeInference1;

implementation

{$R *.fmx}

uses formTypeInference2, formInlineConstants;

procedure TfrmTypeInference1.btnNextClick(Sender: TObject);
begin
  Application.MainForm := frmTypeInference2;
  frmTypeInference2.Left := Self.Left;
  frmTypeInference2.Top := Self.Top;
  frmTypeInference2.Width := Self.Width;
  frmTypeInference2.Height := Self.Height;
  frmTypeInference2.Show;
  Self.Hide;
end;

procedure TfrmTypeInference1.btnPrevClick(Sender: TObject);
begin
  Application.MainForm := frmInlineConstants;
  frmInlineConstants.Left := Self.Left;
  frmInlineConstants.Top := Self.Top;
  frmInlineConstants.Width := Self.Width;
  frmInlineConstants.Height := Self.Height;
  frmInlineConstants.Show;
  Self.Hide;
end;

procedure TfrmTypeInference1.btnTryClick(Sender: TObject);
begin
  var I := 22;
  var J := 'Hello Cruel World';
  MemoResult.Lines.Add(J);
  MemoResult.Lines.Add('I = '+IntToStr(I));
end;

procedure TfrmTypeInference1.FormShow(Sender: TObject);
begin
  Caption := lblTitle.Text;
  wbShowCode.Navigate(mmohtml.Text);
end;

end.
