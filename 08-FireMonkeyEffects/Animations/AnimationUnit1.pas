unit AnimationUnit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Colors;

type
  TForm29 = class(TForm)
    FloatAnimation1: TFloatAnimation;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    FloatAnimation2: TFloatAnimation;
    Button1: TButton;
    FloatAnimation3: TFloatAnimation;
    FloatAnimation4: TFloatAnimation;
    Rectangle1: TRectangle;
    ColorPanel1: TColorPanel;
    Button2: TButton;
    procedure Rectangle1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form29: TForm29;

implementation

{$R *.fmx}

uses AnimationUnit2;

procedure TForm29.Button2Click(Sender: TObject);
begin
  Form1.Show;
end;

procedure TForm29.Rectangle1Click(Sender: TObject);
begin
  Rectangle1.AnimateColor('Fill.Color', ColorPanel1.Color, 2,
    TAnimationType.InOut, TInterpolationType.Elastic);
end;

end.
