unit MolHero.FormAbout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  FMX.Viewport3D, System.Math.Vectors, FMX.Types3D, FMX.Controls3D,
  FMX.Objects3D, FMX.Objects, FMX.Layers3D, FMX.Ani;

type
  TFormAbout = class(TForm)
    spdbtnBack: TSpeedButton;
    Viewport3D1: TViewport3D;
    LayoutHeader: TLayout;
    Layer3DMain: TLayer3D;
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Dummy1: TDummy;
    Camera1: TCamera;
    Light1: TLight;
    Label6: TLabel;
    procedure spdbtnBackClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure DoIntroAnimation;
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.fmx}

uses MolHero.FormMain;

procedure TFormAbout.DoIntroAnimation;
begin
  Layer3DMain.Position.Point := Point3D(0,0,50);
  TAnimator.AnimateFloat(Layer3DMain, 'Position.Z', 0, 2);
  TAnimator.AnimateFloat(Layer3DMain, 'RotationAngle.Y', 360, 2);
end;

procedure TFormAbout.spdbtnBackClick(Sender: TObject);
begin
  FormMain.Show;
  self.Hide;
end;

end.
