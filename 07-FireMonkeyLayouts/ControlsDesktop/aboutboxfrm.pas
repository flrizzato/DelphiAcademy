//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit aboutboxfrm;

interface

uses
  System.SysUtils, System.Classes, FMX.Viewport3D, FMX.StdCtrls, FMX.MaterialSources,
  FMX.Forms, FMX.Dialogs, FMX.Types, FMX.Objects, FMX.Effects, FMX.Forms3D, FMX.Controls3D,
  FMX.Controls, FMX.Ani, FMX.Layouts, FMX.Objects3D, FMX.Types3D, FMX.Materials;

type

  TfrmAbout = class(TForm)
    Rectangle1: TRectangle;
    ShadowEffect1: TShadowEffect;
    Button1: TButton;
    Text1: TText;
    Viewport3D1: TViewport3D;
    Light1: TLight;
    Cube1: TCube;
    FloatAnimation1: TFloatAnimation;
    ColorAnimation1: TColorAnimation;
    Text3D1: TText3D;
    GlowEffect1: TGlowEffect;
    GouraudMaterialSource1: TLightMaterialSource;
    GouraudMaterialSource2: TLightMaterialSource;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.fmx}

procedure TfrmAbout.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
