unit formTypeInference2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmTypeInference2 = class(TForm)
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
  frmTypeInference2: TfrmTypeInference2;

implementation
uses
  system.generics.collections, formTypeInference1, formLinuxAnsiStrings;

{$R *.fmx}


procedure TfrmTypeInference2.btnNextClick(Sender: TObject);
begin
  Application.MainForm := frmLinuxAnsiStrings;
  frmLinuxAnsiStrings.Left := Self.Left;
  frmLinuxAnsiStrings.Top := Self.Top;
  frmLinuxAnsiStrings.Width := Self.Width;
  frmLinuxAnsiStrings.Height := Self.Height;
  frmLinuxAnsiStrings.Show;
  Self.Hide;
end;

procedure TfrmTypeInference2.btnPrevClick(Sender: TObject);
begin
  Application.MainForm := frmTypeInference1;
  frmTypeInference1.Left := Self.Left;
  frmTypeInference1.Top := Self.Top;
  frmTypeInference1.Width := Self.Width;
  frmTypeInference1.Height := Self.Height;
  frmTypeInference1.Show;
  Self.Hide;
end;

procedure TfrmTypeInference2.btnTryClick(Sender: TObject);
begin
  var MyDictionary := TDictionary<string,integer>.Create;
  MyDictionary.Add('one',1);
  var APair := MyDictionary.ExtractPair('one');
  MemoResult.Lines.Add(APair.Value.ToString);
end;

procedure TfrmTypeInference2.FormShow(Sender: TObject);
begin
  Caption := lblTitle.Text;
  wbShowCode.Navigate(mmohtml.Text);
end;

end.
