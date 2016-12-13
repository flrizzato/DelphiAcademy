unit MolHero.Materials;

interface

uses
  FMX.Types3D, FMX.MaterialSources, MolHero.Utils;

type
  TAtomMaterials = class
  private
    FMats: array[0..CHEM_ELEMENTS_COUNT] of TMaterialSource; // "0" for unknown atom type
  public
    destructor Destroy; override;
    function GetAtomMaterial(AtomicNr: byte): TMaterialSource;
  end;

implementation

uses
  FMX.Materials;

{ TAtomMaterials }

destructor TAtomMaterials.Destroy;
var i: integer;
begin
  for i := Low(FMats) to High(FMats) do
    FMats[i].Free;

  inherited;
end;

function TAtomMaterials.GetAtomMaterial(AtomicNr: byte): TMaterialSource;
var mat: TLightMaterialSource;
begin
  if AtomicNr <= CHEM_ELEMENTS_COUNT then
  begin
    if FMats[AtomicNr-1] = nil then
    begin
      mat := TLightMaterialSource.Create(nil);
      mat.Diffuse := AtomicNrToColor(AtomicNr);
      FMats[AtomicNr-1] := mat;
    end;
    Result := FMats[AtomicNr-1];
  end
  else
    Result := nil;
end;

end.
