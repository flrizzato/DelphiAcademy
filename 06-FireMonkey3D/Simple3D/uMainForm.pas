unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.MaterialSources, FMX.Objects3D, FMX.Controls3D,
  FMX.Viewport3D, FMX.Ani;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    SphereTerra: TSphere;
    SphereJupiter: TSphere;
    Dummy1: TDummy;
    TextureMaterialSource1: TTextureMaterialSource;
    TextureMaterialSource2: TTextureMaterialSource;
    FloatAnimation1: TFloatAnimation;
    procedure SphereTerraClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.SphereTerraClick(Sender: TObject);
begin
  FloatAnimation1.Enabled := not FloatAnimation1.Enabled;
end;

end.
