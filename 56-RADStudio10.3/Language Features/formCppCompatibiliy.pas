unit formCppCompatibiliy;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Objects, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TfrmCppCompatibility = class(TForm)
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
    Text5: TText;
    procedure FormShow(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCppCompatibility: TfrmCppCompatibility;

implementation

{$R *.fmx}

uses formLinuxAnsiStrings;

procedure TfrmCppCompatibility.btnNextClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmCppCompatibility.btnPrevClick(Sender: TObject);
begin
  Application.MainForm := frmLinuxAnsiStrings;
  frmLinuxAnsiStrings.Left := Self.Left;
  frmLinuxAnsiStrings.Top := Self.Top;
  frmLinuxAnsiStrings.Width := Self.Width;
  frmLinuxAnsiStrings.Height := Self.Height;
  frmLinuxAnsiStrings.Show;
  Self.Hide;
end;

procedure TfrmCppCompatibility.FormShow(Sender: TObject);
begin
  Caption := lblTitle.Text;
end;

end.
