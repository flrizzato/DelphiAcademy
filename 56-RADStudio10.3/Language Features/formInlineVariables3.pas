unit formInlineVariables3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmInlineVariables3 = class(TForm)
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
    procedure btnTryClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmInlineVariables3: TfrmInlineVariables3;

implementation

{$R *.fmx}

uses formInlineConstants, formInlineVariables2;

procedure TfrmInlineVariables3.btnNextClick(Sender: TObject);
begin
  Application.MainForm := frmInlineConstants;
  frmInlineConstants.Left := Self.Left;
  frmInlineConstants.Top := Self.Top;
  frmInlineConstants.Width := Self.Width;
  frmInlineConstants.Height := Self.Height;
  frmInlineConstants.Show;
  Self.Hide;
end;

procedure TfrmInlineVariables3.btnPrevClick(Sender: TObject);
begin
  Application.MainForm := frmInlineVariables2;
  frmInlineVariables2.Left := Self.Left;
  frmInlineVariables2.Top := Self.Top;
  frmInlineVariables2.Width := Self.Width;
  frmInlineVariables2.Height := Self.Height;
  frmInlineVariables2.Show;
  Self.Hide;
end;

procedure TfrmInlineVariables3.btnTryClick(Sender: TObject);
begin
  for var idx := 0 to 5 do begin
    MemoResult.Lines.Add('idx = '+IntToStr(idx));
  end;
end;

procedure TfrmInlineVariables3.FormShow(Sender: TObject);
begin
  Caption := lblTitle.Text;
  wbShowCode.Navigate(mmohtml.Text);
end;

end.
