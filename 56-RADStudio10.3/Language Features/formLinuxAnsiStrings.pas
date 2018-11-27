unit formLinuxAnsiStrings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmLinuxAnsiStrings = class(TForm)
    lotScale: TScaledLayout;
    lotTryMe: TLayout;
    lotMain: TLayout;
    lotDescription: TLayout;
    lblTitle: TLabel;
    Text1: TText;
    Splitter2: TSplitter;
    pnlButtons: TPanel;
    btnPrev: TButton;
    btnNext: TButton;
    StyleBook1: TStyleBook;
    Text2: TText;
    Text3: TText;
    Text4: TText;
    procedure FormShow(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLinuxAnsiStrings: TfrmLinuxAnsiStrings;

implementation

{$R *.fmx}

uses formCppCompatibiliy, formTypeInference2;

procedure TfrmLinuxAnsiStrings.btnNextClick(Sender: TObject);
begin
  Application.MainForm := frmCppCompatibility;
  frmCppCompatibility.Left := Self.Left;
  frmCppCompatibility.Top := Self.Top;
  frmCppCompatibility.Width := Self.Width;
  frmCppCompatibility.Height := Self.Height;
  frmCppCompatibility.Show;
  Self.Hide;
end;

procedure TfrmLinuxAnsiStrings.btnPrevClick(Sender: TObject);
begin
  Application.MainForm := frmTypeInference2;
  frmTypeInference2.Left := Self.Left;
  frmTypeInference2.Top := Self.Top;
  frmTypeInference2.Width := Self.Width;
  frmTypeInference2.Height := Self.Height;
  frmTypeInference2.Show;
  Self.Hide;
end;

procedure TfrmLinuxAnsiStrings.FormShow(Sender: TObject);
begin
  Caption := lblTitle.Text;
end;

end.
