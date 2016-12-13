unit MolHero.FormRes;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.TabControl;

type
  TFormRes = class(TForm)
    MemoCaffeine: TMemo;
    TabControlPDBs: TTabControl;
    TabItemCoffeine: TTabItem;
    TabItemBucky: TTabItem;
    MemoBucky: TMemo;
    TabItemAspirin: TTabItem;
    MemoAspirin: TMemo;
    TabItemCholesterol: TTabItem;
    MemoCholesterol: TMemo;
    TabItemCocaine: TTabItem;
    MemoCocaine: TMemo;
    TabItemCu: TTabItem;
    MemoCu: TMemo;
    TabItemCubane: TTabItem;
    MemoCubane: TMemo;
    TabItemDiamond: TTabItem;
    MemoDiamond: TMemo;
    TabItemEthanol: TTabItem;
    TabItemGlucose: TTabItem;
    MemoGlucose: TMemo;
    MemoEthanol: TMemo;
    TabItemGraphite: TTabItem;
    TabItemLSD: TTabItem;
    TabItemLycopene: TTabItem;
    TabItemYbco: TTabItem;
    TabItemNicotine: TTabItem;
    TabItemNaCl: TTabItem;
    MemoNaCl: TMemo;
    MemoNicotine: TMemo;
    MemoYbco: TMemo;
    MemoLycopene: TMemo;
    MemoLSD: TMemo;
    MemoGraphite: TMemo;
    TabControlMain: TTabControl;
    TabItemBuiltin: TTabItem;
    TabItemCustom: TTabItem;
  private
//    FDefaultMoleculeIndex: integer;
    function MemoToLines(const aMemo: TMemo): TStringDynArray;
  public
    constructor Create(AOwner: TComponent); override;
//    function GetDefaultPDB: TStringDynArray;
//    function GetDefaultPDBName: string;
    function GetMoleculeCount: integer;
    function GetMoleculeName(const i: integer): string;
    function GetMoleculePDB(const i: integer): TStringDynArray;
  end;

var
  FormRes: TFormRes;

implementation

{$R *.fmx}

{ TFormRes }

constructor TFormRes.Create(AOwner: TComponent);
begin
  inherited;
end;

function TFormRes.MemoToLines(const aMemo: TMemo): TStringDynArray;
var i, aCount: integer; sl: TStringDynArray;
begin
  aCount := aMemo.Lines.Count;
  SetLength(sl, aCount);
  for i := 0 to aCount-1 do
    sl[i] := aMemo.Lines[i];
  Result := sl;
end;

function TFormRes.GetMoleculeCount: integer;
begin
//  Result := 16;    // "Glucose and Coccaine give errors
  Result := 14;
end;

function TFormRes.GetMoleculeName(const i: integer): string;
begin
  if i = 0 then
    Result := 'Aspirin'

  else if i = 1 then
    Result := 'Bucky'

  else if i = 2 then
    Result := 'Cholesterol'

  else if i = 3 then
    Result := 'Coffeine'

  else if i = 4 then
    Result := 'Cu'

  else if i = 5 then
    Result := 'Cubane'

  else if i = 6 then
    Result := 'Diamond'

  else if i = 7 then
    Result := 'Ethanol'

  else if i = 8 then
    Result := 'Graphite'

  else if i = 9 then
    Result := 'LSD'

  else if i = 10 then
    Result := 'Lycopene'

  else if i = 11 then
    Result := 'NaCl'

  else if i = 12 then
    Result := 'Nicotine'

  else if i = 13 then
    Result := 'ybco'

  else if i = 14 then
    Result := 'Coccaine'

  else if i = 15 then
    Result := 'Glucose'

  else
    Result := '';
end;

function TFormRes.GetMoleculePDB(const i: integer): TStringDynArray;
begin
  if i = 0 then
    Result := MemoToLines(MemoAspirin)

  else if i = 1 then
    Result := MemoToLines(MemoBucky)

  else if i = 2 then
    Result := MemoToLines(MemoCholesterol)

  else if i = 3 then
    Result := MemoToLines(MemoCaffeine)

  else if i = 4 then
    Result := MemoToLines(MemoCu)

  else if i = 5 then
    Result := MemoToLines(MemoCubane)

  else if i = 6 then
    Result := MemoToLines(MemoDiamond)

  else if i = 7 then
    Result := MemoToLines(MemoEthanol)

  else if i = 8 then
    Result := MemoToLines(MemoGraphite)

  else if i = 9 then
    Result := MemoToLines(MemoLSD)

  else if i = 10 then
    Result := MemoToLines(MemoLycopene)

  else if i = 11 then
    Result := MemoToLines(MemoNaCl)

  else if i = 12 then
    Result := MemoToLines(MemoNicotine)

  else if i = 13 then
    Result := MemoToLines(Memoybco)

  else if i = 14 then
    Result := MemoToLines(MemoCocaine)

  else if i = 15 then
    Result := MemoToLines(MemoGlucose)

  else
    Result := nil;

end;

end.
