unit MolHero.Utils;

interface

uses
  System.UITypes;

// http://en.wikipedia.org/wiki/Chemical_element
const
  CHEM_ELEMENTS_COUNT = 118;

///	<summary>
///	  Converts chemical element symbol to its atomic number
///	</summary>
///	<param name="symbol">
///	  Atomic symbol to convert. Characters case is not important
///	</param>
///	<returns>
///	  Returns atomic number of the symbol or "0" if symbol is not recognized
///	</returns>
function ElementSymbolToAtomicNr(const symbol: string): byte;

function AtomicNrToColor(const nr: byte): TAlphaColor;

implementation

uses
  System.SysUtils,
  System.UIConsts;

function ElementSymbolToAtomicNr(const symbol: string): byte;
var s: string;
begin
  s := LowerCase(symbol);
  if s = 'h' then Result := 1 else
  if s = 'he' then Result := 2 else
  if s = 'li' then Result := 3 else
  if s = 'be' then Result := 4 else
  if s = 'b' then Result := 5 else
  if s = 'c' then Result := 6 else
  if s = 'n' then Result := 7 else
  if s = 'o' then Result := 8 else
  if s = 'f' then Result := 9 else
  if s = 'ne' then Result := 10 else
  if s = 'na' then Result := 11 else
  if s = 'mg' then Result := 12 else
  if s = 'al' then Result := 13 else
  if s = 'si' then Result := 14 else
  if s = 'p' then Result := 15 else
  if s = 's' then Result := 16 else
  if s = 'cl' then Result := 17 else
  if s = 'ar' then Result := 18 else
  if s = 'k' then Result := 19 else
  if s = 'ca' then Result := 20 else
  if s = 'sc' then Result := 21 else
  if s = 'ti' then Result := 22 else
  if s = 'v' then Result := 23 else
  if s = 'cr' then Result := 24 else
  if s = 'mn' then Result := 25 else
  if s = 'fe' then Result := 26 else
  if s = 'co' then Result := 27 else
  if s = 'ni' then Result := 28 else
  if s = 'cu' then Result := 29 else
  if s = 'zn' then Result := 30 else
  if s = 'ga' then Result := 31 else
  if s = 'ge' then Result := 32 else
  if s = 'as' then Result := 33 else
  if s = 'se' then Result := 34 else
  if s = 'br' then Result := 35 else
  if s = 'kr' then Result := 36 else
  if s = 'rb' then Result := 37 else
  if s = 'sr' then Result := 38 else
  if s = 'y' then Result := 39 else
  if s = 'zr' then Result := 40 else
  if s = 'nb' then Result := 41 else
  if s = 'mo' then Result := 42 else
  if s = 'tc' then Result := 43 else
  if s = 'ru' then Result := 44 else
  if s = 'rh' then Result := 45 else
  if s = 'pd' then Result := 46 else
  if s = 'ag' then Result := 47 else
  if s = 'cd' then Result := 48 else
  if s = 'in' then Result := 49 else
  if s = 'sn' then Result := 50 else
  if s = 'sb' then Result := 51 else
  if s = 'te' then Result := 52 else
  if s = 'i' then Result := 53 else
  if s = 'xe' then Result := 54 else
  if s = 'cs' then Result := 55 else
  if s = 'ba' then Result := 56 else
  if s = 'la' then Result := 57 else
  if s = 'ce' then Result := 58 else
  if s = 'pr' then Result := 59 else
  if s = 'nd' then Result := 60 else
  if s = 'pm' then Result := 61 else
  if s = 'sm' then Result := 62 else
  if s = 'eu' then Result := 63 else
  if s = 'gd' then Result := 64 else
  if s = 'tb' then Result := 65 else
  if s = 'dy' then Result := 66 else
  if s = 'ho' then Result := 67 else
  if s = 'er' then Result := 68 else
  if s = 'tm' then Result := 69 else
  if s = 'yb' then Result := 70 else
  if s = 'lu' then Result := 71 else
  if s = 'hf' then Result := 72 else
  if s = 'ta' then Result := 73 else
  if s = 'w' then Result := 74 else
  if s = 're' then Result := 75 else
  if s = 'os' then Result := 76 else
  if s = 'ir' then Result := 77 else
  if s = 'pt' then Result := 78 else
  if s = 'au' then Result := 79 else
  if s = 'hg' then Result := 80 else
  if s = 'tl' then Result := 81 else
  if s = 'pb' then Result := 82 else
  if s = 'bi' then Result := 83 else
  if s = 'po' then Result := 84 else
  if s = 'at' then Result := 85 else
  if s = 'rn' then Result := 86 else
  if s = 'fr' then Result := 87 else
  if s = 'ra' then Result := 88 else
  if s = 'ac' then Result := 89 else
  if s = 'th' then Result := 90 else
  if s = 'pa' then Result := 91 else
  if s = 'u' then Result := 92 else
  if s = 'np' then Result := 93 else
  if s = 'pu' then Result := 94 else
  if s = 'am' then Result := 95 else
  if s = 'cm' then Result := 96 else
  if s = 'bk' then Result := 97 else
  if s = 'cf' then Result := 98 else
  if s = 'es' then Result := 99 else
  if s = 'fm' then Result := 100 else
  if s = 'md' then Result := 101 else
  if s = 'no' then Result := 102 else
  if s = 'lr' then Result := 103 else
  if s = 'rf' then Result := 104 else
  if s = 'db' then Result := 105 else
  if s = 'sg' then Result := 106 else
  if s = 'bh' then Result := 107 else
  if s = 'hs' then Result := 108 else
  if s = 'mt' then Result := 109 else
  if s = 'ds' then Result := 110 else
  if s = 'rg' then Result := 111 else
  if s = 'cn' then Result := 112 else
  if s = 'uut' then Result := 113 else
  if s = 'uuq' then Result := 114 else
  if s = 'uup' then Result := 115 else
  if s = 'uuh' then Result := 116 else
  if s = 'uus' then Result := 117 else
  if s = 'uuo' then Result := 118 else
    Result := 0;
end;

type
  TColorRGB = array[0..2] of byte;

const
  ATOM_COLOR: array[0..CHEM_ELEMENTS_COUNT-1] of TColorRGB = (
    {h} (255,255,255),
    {he} (217,255,255),
    {li} (204,128,255),
    {be} (194,255,0),
    {b} (255,181,181),
    {c} (144,144,144),
    {n} (48,80,248),
    {o} (255,13,13),
    {f} (144,224,80),
    {ne} (179,227,245),
    {na} (171,92,242),
    {mg} (138,255,0),
    {al} (191,166,166),
    {si} (240,200,160),
    {p} (255,128,0),
    {s} (255,255,48),
    {cl} (31,240,31),
    {ar} (128,209,227),
    {k} (143,64,212),
    {ca} (61,255,0),
    {sc} (230,230,230),
    {ti} (191,194,199),
    {v} (166,166,171),
    {cr} (138,153,199),
    {mn} (156,122,199),
    {fe} (224,102,51),
    {co} (240,144,160),
    {ni} (80,208,80),
    {cu} (200,128,51),
    {zn} (125,128,176),
    {ga} (194,143,143),
    {ge} (102,143,143),
    {as} (189,128,227),
    {se} (255,161,0),
    {br} (166,41,41),
    {kr} (92,184,209),
    {rb} (112,46,176),
    {sr} (0,255,0),
    {y} (148,255,255),
    {zr} (148,224,224),
    {nb} (115,194,201),
    {mo} (84,181,181),
    {tc} (59,158,158),
    {ru} (36,143,143),
    {rh} (10,125,140),
    {pd} (0,105,133),
    {ag} (192,192,192),
    {cd} (255,217,143),
    {in} (166,117,115),
    {sn} (102,128,128),
    {sb} (158,99,181),
    {te} (212,122,0),
    {i} (148,0,148),
    {xe} (66,158,176),
    {cs} (87,23,143),
    {ba} (0,201,0),
    {la} (112,212,255),
    {ce} (255,255,199),
    {pr} (217,255,199),
    {nd} (199,255,199),
    {pm} (163,255,199),
    {sm} (143,255,199),
    {eu} (97,255,199),
    {gd} (69,255,199),
    {tb} (48,255,199),
    {dy} (31,255,199),
    {ho} (0,255,156),
    {er} (0,230,117),
    {tm} (0,212,82),
    {yb} (0,191,56),
    {lu} (0,171,36),
    {hf} (77,194,255),
    {ta} (77,166,255),
    {w} (33,148,214),
    {re} (38,125,171),
    {os} (38,102,150),
    {ir} (23,84,135),
    {pt} (208,208,224),
    {au} (255,209,35),
    {hg} (184,184,208),
    {tl} (166,84,77),
    {pb} (87,89,97),
    {bi} (158,79,181),
    {po} (171,92,0),
    {at} (117,79,69),
    {rn} (66,130,150),
    {fr} (66,0,102),
    {ra} (0,125,0),
    {ac} (112,171,250),
    {th} (0,186,255),
    {pa} (0,161,255),
    {u} (0,143,255),
    {np} (0,128,255),
    {pu} (0,107,255),
    {am} (84,92,242),
    {cm} (120,92,227),
    {bk} (138,79,227),
    {cf} (161,54,212),
    {es} (179,31,212),
    {fm} (179,31,186),
    {md} (179,13,166),
    {no} (189,13,135),
    {lr} (199,0,102),
    {rf} (204,0,89),
    {db} (209,0,79),
    {sg} (217,0,69),
    {bh} (224,0,56),
    {hs} (230,0,46),
    {mt} (235,0,38),
    {ds} (235,0,38),
    {rg} (235,0,38),
    {cn} (235,0,38),
    {uut} (235,0,38),
    {uuq} (235,0,38),
    {uup} (235,0,38),
    {uuh} (235,0,38),
    {uus} (235,0,38),
    {uuo} (235,0,38)
  );

function ColorRGBToAlphaColor(c: TColorRGB): TAlphaColor;
begin
  Result := MakeColor(c[0], c[1], c[2]);
end;

function AtomicNrToColor(const nr: byte): TAlphaColor;
begin
  if (nr < 1) or (nr > 118) then
    Result := MakeColor(255,255,255) // unknown atom type
  else
    Result := ColorRGBToAlphaColor(ATOM_COLOR[nr-1]);
end;

end.
