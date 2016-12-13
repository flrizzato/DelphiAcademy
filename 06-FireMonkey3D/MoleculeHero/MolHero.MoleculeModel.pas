unit MolHero.MoleculeModel;

interface

uses
  System.Types,
  System.Math.Vectors,
  System.Generics.Collections;

type
  TAtomData = record
    AtomKind: integer;
    Pos: TPoint3D;
    Symbol: string;
  end;

  TAtoms = TList<TAtomData>;

  TBondKind = record
    const b1 = 1;
    const b2 = 2;
    const b3 = 3;
  end;

  TBondData = record
    BondKind: integer;
    IdStart, IdEnd: integer;
  end;

  TBonds = TList<TBondData>;

  TMolecule = class
  private
    FAtoms: TAtoms;
    FBonds: TBonds;
    FDisplayName: string;
    procedure SetDisplayName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Atoms: TAtoms read FAtoms;
    property Bonds: TBonds read FBonds;
    property DisplayName: string read FDisplayName write SetDisplayName;
  end;

implementation

{ TMolecule }

constructor TMolecule.Create;
begin
  FAtoms := TList<TAtomData>.Create;
  FBonds := TList<TBondData>.Create;
end;

destructor TMolecule.Destroy;
begin
  FAtoms.Free;
  FBonds.Free;
  inherited;
end;

procedure TMolecule.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TMolecule.Clear;
begin
  FAtoms.Clear;
  FBonds.Clear;
end;

end.
