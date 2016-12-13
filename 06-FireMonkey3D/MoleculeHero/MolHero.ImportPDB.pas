// PDB file format from
// http://deposit.rcsb.org/adit/docs/pdb_atom_format.html

// atom color coding from
// http://mrdoob.github.com/three.js/examples/js/loaders/PDBLoader.js
// part of ThreeJS "molecules" demo:
// http://mrdoob.github.com/three.js/examples/css3d_molecules.html

unit MolHero.ImportPDB;

interface

uses MolHero.MoleculeModel, System.Types;

function GetMoleculeFromStrings(const sl: TStringDynArray; const m: TMolecule): boolean;

implementation

uses System.Classes, System.SysUtils, IOUtils, MolHero.Utils, MolHero.FormRes;

function ReadStringsFromFile(const aFileName: string): TStringDynArray;
begin
  if TFile.Exists(aFileName) then
    Result := TFile.ReadAllLines(aFileName)
  else
    Result := nil;
end;

function ExtrAtomKind(const s: string): integer;
begin
  try
    Result := ElementSymbolToAtomicNr(UpperCase(trim(s)));
  except
    Result := -1;
  end;
end;

function ExtrAtomCoord(s: string): double;
begin
  s := trim(s);
  if not TryStrToFloat(s, Result) then
  begin
    s := StringReplace(s,'.',',',[]);
    Result := StrToFloat(s);
  end;
end;

procedure TranslateAtoms(const m: TMolecule; minX, minY, minZ, maxX, maxY, maxZ: double);
var deltaX, deltaY, deltaZ: double;
  i: Integer; a: TAtomData;
begin
  deltaX := (maxX - minX)/2 + minX;
  deltaY := (maxY - minY)/2 + minY;
  deltaZ := (maxZ - minZ)/2 + minZ;

  for i := 0 to m.Atoms.Count-1 do
  begin
    a := m.Atoms[i];
    a.Pos.X := a.Pos.X - deltaX;
    a.Pos.Y := a.Pos.Y - deltaY;
    a.Pos.Z := a.Pos.Z - deltaZ;
  end;
end;

function GetMoleculeFromStrings(const sl: TStringDynArray; const m: TMolecule): boolean;
var s: string; i,j: integer; a: TAtomData; b: TBondData;
  e: string; bs: string; minX, minY, minZ, maxX, maxY, maxZ: double;
  isFirstAtom: boolean;
begin
  Result := false;

  if sl <> nil then
  begin

    isFirstAtom := True;
    for i := 0 to Length(sl)-1 do
    begin
      s := sl[i];

      if (copy(s,1,4) = 'ATOM') or (copy(s,1,6) = 'HETATM') then
      begin
        e := trim(copy(s,77,2));
        if e = '' then
          e := trim(copy(s,13,4));
        a.Symbol := e;

        a.AtomKind := ExtrAtomKind(e);

        a.Pos.X := ExtrAtomCoord(copy(s,31,8));
        a.Pos.Y := ExtrAtomCoord(copy(s,39,8));
        a.Pos.Z := ExtrAtomCoord(copy(s,47,8));

        if isFirstAtom then
        begin
          minX := a.Pos.X;
          minY := a.Pos.Y;
          minZ := a.Pos.Z;
          maxX := a.Pos.X;
          maxY := a.Pos.Y;
          maxZ := a.Pos.Z;
        end
        else
        begin
          if a.Pos.X < minX then minX := a.Pos.X;
          if a.Pos.Y < minY then minY := a.Pos.Y;
          if a.Pos.Z < minZ then minZ := a.Pos.Z;
          if a.Pos.X > maxX then maxX := a.Pos.X;
          if a.Pos.Y > maxY then maxY := a.Pos.Y;
          if a.Pos.Z > maxZ then maxZ := a.Pos.Z;
        end;

        isFirstAtom := False;

        m.Atoms.Add(a);
      end;

      if copy(s,1,6) = 'CONECT' then
      begin
        b.IdStart := StrToInt(copy(s,7,5))-1;

        for j := 0 to 3 do
        begin
          bs := trim(copy(s,12+5*j,5));
          if bs <> '' then
          begin
            b.IdEnd := StrToInt(bs)-1;
            if b.IdEnd > 0 then
              m.Bonds.Add(b);
          end;
        end;
      end;
    end;

    Result := true;

    if not isFirstAtom then
      TranslateAtoms(m, minX, minY, minZ, maxX, maxY, maxZ);

  end;
end;

end.
