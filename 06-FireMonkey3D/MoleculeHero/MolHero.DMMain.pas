unit MolHero.DMMain;

interface

uses
  System.SysUtils, System.Classes, FMX.Controls3D,
  MolHero.MoleculeModel, MolHero.Materials;

type
  TDMMain = class(TDataModule)
  private
    FMolecule: TMolecule;
    FAtomMaterials: TAtomMaterials;
    procedure LoadMolecule(const ctr: TControl3D; const mol: TMolecule; const mats: TAtomMaterials);
    function AtomicNrToScale(nr: byte): double;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetMoleculeCount: integer;
    function GetMoleculeDisplayName(i: integer): string;
    procedure GetMoleculeByIndex(const index: integer; const AMolecule: TMolecule);
    procedure LoadMoleculeToCtrl3D(const ctrl: TControl3D; const m: TMolecule);
    procedure LoadMoleculeByIndex(const ctrl: TControl3D; const index: integer);
    property CurrentMolecule: TMolecule read FMolecule;
  end;

var
  DMMain: TDMMain;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses System.Types, System.Math, System.Math.Vectors, FMX.Objects3D,
  MolHero.ImportPDB, MolHero.FormRes;

{$R *.dfm}

{ TDMMain }

function TDMMain.AtomicNrToScale(nr: byte): double;
begin
  Result := 1; // to be implemented
end;

constructor TDMMain.Create(AOwner: TComponent);
begin
  inherited;

  FAtomMaterials := TAtomMaterials.Create;

  if FormRes = nil then
    FormRes := TFormRes.Create(AOwner);
end;

destructor TDMMain.Destroy;
begin
  FMolecule.Free;
  FAtomMaterials.Free;
  inherited;
end;

function TDMMain.GetMoleculeDisplayName(i: integer): string;
begin
  Result := FormRes.GetMoleculeName(i);
end;

procedure TDMMain.GetMoleculeByIndex(const index: integer;
  const AMolecule: TMolecule);
var pdb: TStringDynArray;
begin
  pdb := FormRes.GetMoleculePDB(index);
  try
    GetMoleculeFromStrings(pdb, aMolecule);
    aMolecule.DisplayName := FormRes.GetMoleculeName(index);
  finally
    Finalize(pdb);
  end;
end;

function TDMMain.GetMoleculeCount: integer;
begin
  Result := FormRes.GetMoleculeCount;
end;

procedure TDMMain.LoadMoleculeToCtrl3D(const ctrl: TControl3D; const m: TMolecule);
begin
  LoadMolecule(ctrl, m, FAtomMaterials);
end;

procedure TDMMain.LoadMolecule(const ctr: TControl3D;
  const Mol: TMolecule; const mats: TAtomMaterials);
var i: integer; sph: TSphere; atom: TAtomData; scale: double;
  bond: TBondData; pStart, pEnd, pMid: TPoint3D; cyl: TCylinder;
begin
  ctr.DeleteChildren;

  for i := 0 to Mol.Atoms.Count-1 do
  begin
    atom := Mol.Atoms[i];

    sph := TSphere.Create(ctr);

    sph.MaterialSource := mats.GetAtomMaterial(atom.AtomKind);

    scale := AtomicNrToScale(atom.AtomKind);
    sph.Scale.X := scale;
    sph.Scale.Y := scale;
    sph.Scale.Z := scale;

    sph.Position.X := atom.Pos.X;
    sph.Position.Y := atom.Pos.Y;
    sph.Position.Z := atom.Pos.Z;

    sph.TagString := 'Atom ' + i.ToString + ' [' + sph.Position.ToString + ']';

    ctr.AddObject(sph);
  end;

  for i := 0 to Mol.Bonds.Count-1 do
  begin
    bond := Mol.Bonds[i];

    pStart := mol.Atoms[bond.IdStart].Pos;
    pEnd := mol.Atoms[bond.IdEnd].Pos;

    pMid := Point3D((pStart.X+pEnd.X)/2,(pStart.Y+pEnd.Y)/2,(pStart.Z+pEnd.Z)/2);
    cyl := TCylinder.Create(ctr);
    cyl.Position.Point := pMid;
    cyl.Height := pStart.Distance(pEnd);
    cyl.Width := 0.1;
    cyl.Depth := 0.1;
    cyl.RotationAngle.Z := -180/Pi*ArcTan2(pEnd.X-pStart.X, pEnd.Y-pStart.Y);
    cyl.RotationAngle.X := 180/Pi*ArcTan2(pEnd.Z-pStart.Z, Sqrt(Sqr(pEnd.Y-pStart.Y)+Sqr(pEnd.X-pStart.X)));
    cyl.MaterialSource := mats.GetAtomMaterial(1);  // temp fake implementataion

    cyl.TagString := 'Bond ' + i.ToString + ' [' + bond.IdStart.ToString + '--' + bond.IdEnd.ToString + ']';

    ctr.AddObject(cyl);
  end;

end;

procedure TDMMain.LoadMoleculeByIndex(const ctrl: TControl3D;
  const index: integer);
begin
  if FMolecule = nil then
    FMolecule := TMolecule.Create
  else
    FMolecule.Clear;

  GetMoleculeByIndex(index, FMolecule);
  LoadMoleculeToCtrl3D(ctrl, FMolecule);
end;

end.
