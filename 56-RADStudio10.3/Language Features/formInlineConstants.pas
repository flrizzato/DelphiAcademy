unit formInlineConstants;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmInlineConstants = class(TForm)
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
  frmInlineConstants: TfrmInlineConstants;

implementation

{$R *.fmx}

uses formInlineVariables3, formTypeInference1;

procedure TfrmInlineConstants.btnNextClick(Sender: TObject);
begin
  Application.MainForm := frmTypeInference1;
  frmTypeInference1.Left := Self.Left;
  frmTypeInference1.Top := Self.Top;
  frmTypeInference1.Width := Self.Width;
  frmTypeInference1.Height := Self.Height;
  frmTypeInference1.Show;
  Self.Hide;
end;

procedure TfrmInlineConstants.btnPrevClick(Sender: TObject);
begin
  Application.MainForm := frmInlineVariables3;
  frmInlineVariables3.Left := Self.Left;
  frmInlineVariables3.Top := Self.Top;
  frmInlineVariables3.Width := Self.Width;
  frmInlineVariables3.Height := Self.Height;
  frmInlineVariables3.Show;
  Self.Hide;
end;

procedure TfrmInlineConstants.btnTryClick(Sender: TObject);
begin
  const M: string = 'hello ';
  const N = 'world';
  MemoResult.Lines.Add(M+N);
end;

procedure TfrmInlineConstants.FormShow(Sender: TObject);
begin
  Caption := lblTitle.Text;
  wbShowCode.Navigate(mmohtml.Text);
end;

end.
