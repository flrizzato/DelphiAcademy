unit UCommon;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.MaterialSources, FMX.Controls;

type
  TDmCommon = class(TDataModule)
    LightMaterialStickerGreen: TLightMaterialSource;
    LightMaterialStickerRed: TLightMaterialSource;
    LightMaterialStickerOrange: TLightMaterialSource;
    LightMaterialStickerWhite: TLightMaterialSource;
    LightMaterialStickerBlue: TLightMaterialSource;
    LightMaterialStickerYellow: TLightMaterialSource;
    LightMaterialCube: TLightMaterialSource;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  DmCommon: TDmCommon;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
