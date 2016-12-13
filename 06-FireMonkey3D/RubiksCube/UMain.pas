unit UMain;

interface
//               ______             __
//              /\  ___\           /\ \__
//  _____       \ \ \__/ ___    ___\ \ ,_\    __    ___      __     __     __  __
// /\ '__`\      \ \  _\/ __`\/' _ `\ \ \/  /'__`\/' _ `\  /'__`\ /'__`\  /\ \/\ \
// \ \ \_\ \   __ \ \ \/\ \_\ \\ \/\ \ \ \_/\  __//\ \/\ \/\  __//\ \_\ \_\ \ \_\ \
//  \ \ ,__/ / \_\ \ \_\ \____/ \_\ \_\ \__\ \____\ \_\ \_\ \____\ \__/ \_\\ \____/
//   \ \ \/  \/_/  \/_/\/___/ \/_/\/_/\/__/\/____/\/_/\/_/\/____/\/__/\/_/ \/___/
//    \ \_\
//     \/_/     P2F - 2012 - Rubik'sCube
//     FREEWARE - Aucune diffusion  commerciale basée sur ce code source
//     n'est autorisée sans autorisation préalable de l'auteur.
//     Rubik's Cube 3D Pascal Fonteneau
//     Interfaces- Fun Parts - Pascal Fonteneau and Whiler
//     Upgrade on XE5 Mobile version by Thierry Laborde

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Types3D, FMX.Objects3D,
  FMX.Ani, FMX.Layouts, FMX.Memo, System.Math, System.StrUtils, FMX.Effects, FMX.Platform,
  FMX.Objects, FMX.ListBox, FMX.Colors, FMX.StdCtrls, FMX.Controls3D, FMX.MaterialSources, FMX.Viewport3D, FMX.Edit, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  System.Math.Vectors, FMX.Controls.Presentation;

const
  FUN_LABELS : array[0..11] of string     = ('Damier', 'Barre', '2 Cubes',
                                             '3 Cubes', 'Carré', 'Damier coloré',
                                             'Plus', 'Bande', 'Diagonale',
                                             'Angle', 'L', 'Tetris');
  FUN_VALUES : array[0..11] of String =
   ('OORRWWYYGGBB', 'RRGGOORRGGor', 'WWGGRRwOOYBrBrBryOOw',
                                             'GGyRRyowoRByWBOGGOWW', 'gBrOYwgB', 'OORRWWYYGGBBgBrOYwgFB',
                                             'RGGBBRROOWWGGBBRROOYYr', 'rGGRRbyRRWRRBBRRYRRwbRRGG', 'GBROGBROGBRO',
                                             'WGGRbGoRRBgYWWrGGw', 'OBBYRbGyoRyWgRRw', 'GBROGBROGBROWWYY');

  ANIMATION : array[0..10] of String      = ('Linear','Quadratic','Cubic','Quartic','Quintic','Sinusoidal',
                                             'Exponential','Circular','Elastic','Back','Bounce');

  ANIMATION_SPEED = 0.5;

type
  TStickPosition = (PoFront, PoBack, PoTop, PoBottom, PoRight, PoLeft, PoNone);
  TDirection     = (DiNone, DiLeft, DiRight, DiTop, DiBottom);

type
  TFrmMain = class(TForm)
    Viewport3D2: TViewport3D;
    CubeCentral: TCube;
    GreenPivot: TCube;
    FaTurnGreen: TFloatAnimation;
    OrangePivot: TCube;
    FaTurnOrange: TFloatAnimation;
    WhitePivot: TCube;
    FaTurnWhite: TFloatAnimation;
    YellowPivot: TCube;
    FaTurnYellow: TFloatAnimation;
    RedPivot: TCube;
    FaTurnRed: TFloatAnimation;
    BluePivot: TCube;
    FaTurnBlue: TFloatAnimation;
    CC1: TCube;
    PLCC1R: TPlane;
    PLCC1W: TPlane;
    PLCC1V: TPlane;
    CC2: TCube;
    PLCC2R: TPlane;
    PLCC2W: TPlane;
    PLCC2B: TPlane;
    CC5: TCube;
    PLCC5W: TPlane;
    PLCC5V: TPlane;
    PLCC5O: TPlane;
    CC3: TCube;
    PLCC3R: TPlane;
    PLCC3B: TPlane;
    PLCC3J: TPlane;
    CC4: TCube;
    PLCC4R: TPlane;
    PLCC4V: TPlane;
    PLCC4J: TPlane;
    CC6: TCube;
    PLCC6W: TPlane;
    PLCC6B: TPlane;
    PLCC6O: TPlane;
    CC8: TCube;
    PLCC8V: TPlane;
    PLCC8J: TPlane;
    PLCC8O: TPlane;
    CC7: TCube;
    PLCC7B: TPlane;
    PLCC7J: TPlane;
    PLCC7O: TPlane;
    CA1: TCube;
    PLCA1R: TPlane;
    PLCA1W: TPlane;
    CA2: TCube;
    PLCA2R: TPlane;
    PLCA2V: TPlane;
    CA3: TCube;
    PLCA3R: TPlane;
    PLCA3B: TPlane;
    CA4: TCube;
    PLCA4R: TPlane;
    PLCA4J: TPlane;
    CA5: TCube;
    PLCA5W: TPlane;
    PLCA5V: TPlane;
    CA7: TCube;
    PLCA7B: TPlane;
    PLCA7J: TPlane;
    CA8: TCube;
    PLCA8V: TPlane;
    PLCA8J: TPlane;
    CA9: TCube;
    PLCA9W: TPlane;
    PLCA9O: TPlane;
    CA10: TCube;
    PLCA10V: TPlane;
    PLCA10O: TPlane;
    CA11: TCube;
    PLCA11B: TPlane;
    PLCA11O: TPlane;
    CA12: TCube;
    PLCA12J: TPlane;
    PLCA12O: TPlane;
    CA6: TCube;
    PLCA6W: TPlane;
    PLCA6B: TPlane;
    Dummy: TDummy;
    Camera: TCamera;
    MainLight: TLight;
    LayoutParams: TLayout;
    LayoutParamLeft: TLayout;
    LayoutParamRight: TLayout;
    LayoutParamCenter: TLayout;
    BtnRedRight: TButton;
    BtnRedLeft: TButton;
    BtnGreenLeft: TButton;
    BtnGreenRight: TButton;
    BtnWhiteLeft: TButton;
    BtnWhiteRight: TButton;
    BtnYellowRight: TButton;
    BtnYellowLeft: TButton;
    BtnBlueRight: TButton;
    BtnBlueLeft: TButton;
    BtnOrangeRight: TButton;
    BtnOrangeLeft: TButton;
    CbAnimation: TComboBox;
    ArcDialX: TArcDial;
    LabelX: TLabel;
    ArcDialY: TArcDial;
    LabelY: TLabel;
    ArcDialZ: TArcDial;
    LabelZ: TLabel;
    pbFun: TProgressBar;
    cbFun: TComboBox;
    LblFun: TLabel;
    BtnStart: TButton;
    LblAnim: TLabel;
    FloatAnimLayoutParam: TFloatAnimation;
    LayoutBtParam: TLayout;
    BtParams: TButton;
    LblP2f: TLabel;
    StyleBook1: TStyleBook;    procedure FormCreate(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure RotationFinish(Sender: TObject);
    procedure ArcDialChange(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure StickCornerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure StickMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure Viewport3D2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Viewport3D2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Viewport3D2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure StickCornerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single;
      RayPos, RayDir: TVector3D);
    procedure cbFunChange(Sender: TObject);
    procedure RotatePlan(Pos1, Pos2: Single; TheDirection, Di1, Di2, Di3, Di4: TDirection; P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure CbAnimationChange(Sender: TObject);
    procedure BtParamsClick(Sender: TObject);
    procedure FloatAnimLayoutParamFinish(Sender: TObject);
    procedure LblP2fClick(Sender: TObject);
  private
    { Déclarations privées }
    LastDirection: ShortInt;  // Sens de la dernière rotation  90 Aig -90 Inv
    Direction: TDirection;    // Sens du Mvt de la souris
    ClickX: Single;           // Emplacement du clic X initial
    ClickY: Single;           // Emplacement du clic Y initial
    ClickXR: Single;          // Emplacement du clic X Radial
    ClickYR: Single;          // Emplacement du clic Y Radial
    MvtOn: Boolean;           // Vaut Vrai lors d'un clic sur un stick
    MvtAuto: Boolean;
    MvtClickBt: Boolean;
    procedure Rotate(ThePivotAnimation: TFloatAnimation; StartValue, StepValue: Integer); overload;
    procedure Rotate(Value: Char); overload;
    procedure TurnXPlan(TheCube: TCube);
    procedure TurnYPlan(TheCube: TCube);
    procedure TurnZPlan(TheCube: TCube);
    Function  GetStickPosition(TheStick: TPlane): TStickPosition;
    procedure MoveStick(TheStick: TPlane; Px, Py, Pz, Rx, Ry, Rz: Single);
    procedure PlayMvt(TheMvts: String);
    procedure SortRubik;
    procedure SortCubes;
    procedure SortCube(TheCube: TCube; Px, Py, Pz: Single);
    procedure SortSticks;
  public
    { Déclarations publiques }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.fmx}

uses UCommon, ULaunchWebbrowser;

procedure TFrmMain.FloatAnimLayoutParamFinish(Sender: TObject);
begin
  if not(FloatAnimLayoutParam.Inverse) then
  begin
    FloatAnimLayoutParam.Inverse:=True;
    BtParams.StyleLookup:='arrowdowntoolbutton';
    BtParams.Repaint;
  end
  else
  begin
    FloatAnimLayoutParam.Inverse:=False;
    BtParams.StyleLookup:='arrowuptoolbutton';
    BtParams.Repaint;
  end;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  iLoop: Integer;
begin
  LayoutParams.Height:=1;
  BtParams.StyleLookup:='arrowuptoolbutton';

  FaTurnGreen.Duration  := ANIMATION_SPEED;
  FaTurnOrange.Duration := ANIMATION_SPEED;
  FaTurnWhite.Duration  := ANIMATION_SPEED;
  FaTurnYellow.Duration := ANIMATION_SPEED;
  FaTurnRed.Duration    := ANIMATION_SPEED;
  FaTurnBlue.Duration   := ANIMATION_SPEED;

  MvtOn      := False;   // pas de mouvement de la souris en cours
  MvtAuto    := False;   // pas de mouvement automatique en cours
  MvtClickBt := False;

  // Load the arrays
  for iLoop := Low(FUN_LABELS) to High(FUN_LABELS) do
    cbFun.Items.Add(FUN_LABELS[iLoop] + ' (' + IntToStr(Length(FUN_VALUES[iLoop])) + ')');

  for iLoop := Low(ANIMATION) to High(ANIMATION) do
    CbAnimation.Items.Add(ANIMATION[iLoop]);

  CbAnimation.ItemIndex := 0;
  CbAnimation.OnChange(Self);

end;

// ******************************************************
// Lors du changement de l'animation
// ******************************************************
procedure TFrmMain.CbAnimationChange(Sender: TObject);
begin
  FaTurnRed.Interpolation    := TInterpolationType(CbAnimation.ItemIndex);
  FaTurnGreen.Interpolation  := TInterpolationType(CbAnimation.ItemIndex);
  FaTurnWhite.Interpolation  := TInterpolationType(CbAnimation.ItemIndex);
  FaTurnOrange.Interpolation := TInterpolationType(CbAnimation.ItemIndex);
  FaTurnYellow.Interpolation := TInterpolationType(CbAnimation.ItemIndex);
  FaTurnBlue.Interpolation   := TInterpolationType(CbAnimation.ItemIndex);
end;


procedure TFrmMain.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (not(MvtClickBt)) and (not(MvtAuto)) then
    Rotate(KeyChar);
end;

procedure TFrmMain.Rotate(Value: Char);
begin
  MvtClickBt:=True;
  case Value of
    'r': Rotate(FaTurnRed, 360, 270);
    'R': Rotate(FaTurnRed, 0, 90);
    'o': Rotate(FaTurnOrange, 0, 90);
    'O': Rotate(FaTurnOrange, 360, 270);
    'y': Rotate(FaTurnYellow, 0, 90);
    'Y': Rotate(FaTurnYellow, 360, 270);
    'b': Rotate(FaTurnBlue, 0, 90);
    'B': Rotate(FaTurnBlue, 360, 270);
    'g': Rotate(FaTurnGreen, 360, 270);
    'G': Rotate(FaTurnGreen, 0, 90);
    'w': Rotate(FaTurnWhite, 360, 270);
    'W': Rotate(FaTurnWhite, 0, 90);
  end;
  MvtClickBt:=False;
end;

/// *****************************************************
/// Rechercher les cubes Coins et Arretes situés sur le
/// même plan que le cube allant pivoter et en faire ses
/// enfants.
/// Puis, lancement de l'animation ROTATION
/// *****************************************************
procedure TFrmMain.Rotate(ThePivotAnimation: TFloatAnimation; StartValue, StepValue: Integer);
var
  I: Integer;
begin
  // récuperation du sens de la rotation
  LastDirection := 90;      // sens des aiguilles d'une montre pour 90 degres
  if StartValue = 360 then
    LastDirection := -90;   // sens trigo

  // Lier les cubes Coins et Arretes au cube allant pivoter
  for I := 0 to FrmMain.ComponentCount - 1 do
  begin
    if (FrmMain.Components[I] Is TCube) then
    begin
      if ((FrmMain.Components[I] As TCube).Tag = 2) or ((FrmMain.Components[I] As TCube).Tag = 3) then
      begin
        // Red plan
        if (SameValue((FrmMain.Components[I] As TCube).Position.Z, -3, 0.1)) and (ThePivotAnimation = FaTurnRed) then
        begin
          (FrmMain.Components[I] As TCube).Parent     := RedPivot;
          (FrmMain.Components[I] As TCube).Position.Z := 0;
        end;
        // Orange plan
        if (SameValue((FrmMain.Components[I] As TCube).Position.Z, 3, 0.1)) and (ThePivotAnimation = FaTurnOrange) then
        begin
          (FrmMain.Components[I] As TCube).Parent     := OrangePivot;
          (FrmMain.Components[I] As TCube).Position.Z := 0;
        end;
        // Green plan
        if (SameValue((FrmMain.Components[I] As TCube).Position.X, -3, 0.1)) and (ThePivotAnimation = FaTurnGreen) then
        begin
          (FrmMain.Components[I] As TCube).Parent     := GreenPivot;
          (FrmMain.Components[I] As TCube).Position.X := 0;
        end;
        // Blue plan
        if (SameValue((FrmMain.Components[I] As TCube).Position.X, 3, 0.1)) and (ThePivotAnimation = FaTurnBlue) then
        begin
          (FrmMain.Components[I] As TCube).Parent     := BluePivot;
          (FrmMain.Components[I] As TCube).Position.X := 0;
        end;
        // White plan
        if (SameValue((FrmMain.Components[I] As TCube).Position.Y, -3, 0.1)) and (ThePivotAnimation = FaTurnWhite) then
        begin
          (FrmMain.Components[I] As TCube).Parent     := WhitePivot;
          (FrmMain.Components[I] As TCube).Position.Y := 0;
        end;
        // Yellow plan
        if (SameValue((FrmMain.Components[I] As TCube).Position.Y, 3, 0.1)) and (ThePivotAnimation = FaTurnYellow) then
        begin
          (FrmMain.Components[I] As TCube).Parent     := YellowPivot;
          (FrmMain.Components[I] As TCube).Position.Y := 0;
        end;
      end; // si cube Coin ou cuble arrete
    end; // si un cube
  end; // du for

  // Valuer et lancer l'animation
  ThePivotAnimation.StartValue := StartValue;
  ThePivotAnimation.StopValue  := StepValue;
  ThePivotAnimation.Start;
  while ThePivotAnimation.Running do Application.ProcessMessages;
end;

/// *************************************************
/// En fin d'animation ROTATION le cube ayant pivoté
// rend au cube central les enfants
/// *************************************************
procedure TFrmMain.RotationFinish(Sender: TObject);
Var
  I: Integer;
  TmpX, TmpY, TmpZ: Single;
  ThePlan: TCube;
begin
  for I := 0 to FrmMain.ComponentCount - 1 do
  begin
    if (FrmMain.Components[I] Is TCube) then
    begin
      if ((FrmMain.Components[I] As TCube).Tag = 2) or ((FrmMain.Components[I] As TCube).Tag = 3) then
      begin
        if (FrmMain.Components[I] As TCube).Parent <> CubeCentral then
        begin
          TmpX    := (FrmMain.Components[I] As TCube).AbsolutePosition.X;
          TmpY    := (FrmMain.Components[I] As TCube).AbsolutePosition.Y;
          TmpZ    := (FrmMain.Components[I] As TCube).AbsolutePosition.Z;
          ThePlan := TCube((FrmMain.Components[I] As TCube).Parent);
          (FrmMain.Components[I] As TCube).Position.X := TmpX;
          (FrmMain.Components[I] As TCube).Position.Y := TmpY;
          (FrmMain.Components[I] As TCube).Position.Z := TmpZ;
          (FrmMain.Components[I] As TCube).Parent     := CubeCentral;

          // plan Rouge (plan Z)
          if ThePlan = RedPivot then
          begin
            (FrmMain.Components[I] As TCube).Position.Z := -3;
            TurnZPlan((FrmMain.Components[I] As TCube));
          end;

          // plan Orange (Plan Z)
          if ThePlan = OrangePivot then
          begin
            (FrmMain.Components[I] As TCube).Position.Z := 3;
            TurnZPlan((FrmMain.Components[I] As TCube));
          end;

          // plan bleu (Plan X)
          if ThePlan = BluePivot then
          begin
            (FrmMain.Components[I] As TCube).Position.X := 3;
            TurnXPlan((FrmMain.Components[I] As TCube));
          end;

          // plan Vert (Plan X)
          if ThePlan = GreenPivot then
          begin
            (FrmMain.Components[I] As TCube).Position.X := -3;
            TurnXPlan((FrmMain.Components[I] As TCube));
          end;

          // plan Blanc (Plan Y)
          if ThePlan = WhitePivot then
          begin
            (FrmMain.Components[I] As TCube).Position.Y := -3;
            TurnYPlan((FrmMain.Components[I] As TCube));
          end;

          // plan Jaune  (Plan Y)
          if ThePlan = YellowPivot then
          begin
            (FrmMain.Components[I] As TCube).Position.Y := 3;
            TurnYPlan((FrmMain.Components[I] As TCube));
          end;
        end;
      end;
    end;
  end;
end;

/// *************************************************
/// Renvoi la position (le plan ) d'un stick
/// *************************************************
Function TFrmMain.GetStickPosition(TheStick: TPlane): TStickPosition;
begin
  Result := PoNone;
  if SameValue(TheStick.Position.Y, -1.51, 0.1) then
    Result := PoTop;
  if SameValue(TheStick.Position.Y, 1.51, 0.1) then
    Result := PoBottom;
  if SameValue(TheStick.Position.X, -1.51, 0.1) then
    Result := PoLeft;
  if SameValue(TheStick.Position.X, 1.51, 0.1) then
    Result := PoRight;
  if SameValue(TheStick.Position.Z, 1.51, 0.1) then
    Result := PoBack;
  if SameValue(TheStick.Position.Z, -1.51, 0.1) then
    Result := PoFront;
end;

/// *************************************************
/// Place (colle)  un stick sur un plan des 6 plans
/// du cube
/// ************************************************
procedure TFrmMain.MoveStick(TheStick: TPlane; Px, Py, Pz, Rx, Ry, Rz: Single);
begin
  TheStick.ResetRotationAngle; // Remise à zero des angles de rotation  IMPORTANT
  TheStick.Position.X      := Px;
  TheStick.Position.Y      := Py;
  TheStick.Position.Z      := Pz;
  TheStick.RotationAngle.X := Rx;
  TheStick.RotationAngle.Y := Ry;
  TheStick.RotationAngle.Z := Rz;
end;

/// **********************************************
/// Pour le plan Z ( Rouge et Orange)
/// En fonction du dernier sens de rotation
/// replace les sticks sur la bonne facette
/// **********************************************
procedure TFrmMain.TurnZPlan(TheCube: TCube);
var
  I: Integer;
begin
  for I := 0 to TheCube.ChildrenCount - 1 do
  begin
    if LastDirection = 90 then // sens des aiguilles
    begin
      if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoTop then
        MoveStick((TheCube.Children[I] AS TPlane), 1.51, 0, 0, 0, -90, 0) // Right
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoLeft then
        MoveStick((TheCube.Children[I] AS TPlane), 0, -1.51, 0, -90, 0, 0) // Up
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoBottom then
        MoveStick((TheCube.Children[I] AS TPlane), -1.51, 0, 0, 0, 90, 0) // Left
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoRight then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 1.51, 0, 90, 0, 0); // Down

    end
    else
    begin
      if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoTop then
        MoveStick((TheCube.Children[I] AS TPlane), -1.51, 0, 0, 0, 90, 0) // Left
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoLeft then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 1.51, 0, 90, 0, 0) // Down
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoBottom then
        MoveStick((TheCube.Children[I] AS TPlane), 1.51, 0, 0, 0, -90, 0) // Right
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoRight then
        MoveStick((TheCube.Children[I] AS TPlane), 0, -1.51, 0, -90, 0, 0); // Up
    end;
  end;
end;

/// / **********************************************
/// Idem, pour le plan X ( Bleu et vert)
/// / **********************************************
procedure TFrmMain.TurnXPlan(TheCube: TCube);
var
  I: Integer;
begin
  for I := 0 to TheCube.ChildrenCount - 1 do
  begin
    if LastDirection = 90 then // sens INVERSE des aiguilles
    begin
      if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoTop then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 0, -1.51, 0, 0, 0) // Front
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoBack then
        MoveStick((TheCube.Children[I] AS TPlane), 0, -1.51, 0, -90, 0, 0) // Up
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoBottom then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 0, 1.51, 180, 0, 0) // Back
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoFront then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 1.51, 0, 90, 0, 0); // Down
    end
    else
    begin
      if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoTop then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 0, 1.51, 180, 0, 0) // Back
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoBack then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 1.51, 0, 90, 0, 0) // Down
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoBottom then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 0, -1.51, 0, 0, 0) // Front
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoFront then
        MoveStick((TheCube.Children[I] AS TPlane), 0, -1.51, 0, -90, 0, 0); // Up
    end;
  end;
end;

/// **********************************************
/// Idem, pour le plan Y ( Blanc et Jaune)
/// **********************************************
procedure TFrmMain.TurnYPlan(TheCube: TCube);
var
  I: Integer;
begin
  for I := 0 to TheCube.ChildrenCount - 1 do
  begin
    if LastDirection = 90 then // sens des aiguilles
    begin
      if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoFront then
        MoveStick((TheCube.Children[I] AS TPlane), -1.51, 0, 0, 0, 90, 0) // Left
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoLeft then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 0, 1.51, 180, 0, 0) // Back
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoBack then
        MoveStick((TheCube.Children[I] AS TPlane), 1.51, 0, 0, 0, -90, 0) // Right
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoRight then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 0, -1.51, 0, 0, 0); // Front

    end
    else
    begin
      if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoFront then
        MoveStick((TheCube.Children[I] AS TPlane), 1.51, 0, 0, 0, -90, 0) // Right
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoLeft then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 0, -1.51, 0, 0, 0) // Front
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoBack then
        MoveStick((TheCube.Children[I] AS TPlane), -1.51, 0, 0, 0, 90, 0) // Left
      else if GetStickPosition((TheCube.Children[I] AS TPlane)) = PoRight then
        MoveStick((TheCube.Children[I] AS TPlane), 0, 0, 1.51, 180, 0, 0); // Back
    end;
  end;
end;

{$REGION 'Jeu à la souris'}

/// **********************************************
/// en cliquant sur un stick on arme la recherche
/// d'un mouvement
/// **********************************************
procedure TFrmMain.StickMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single;
  RayPos, RayDir: TVector3D);
begin
  if (MvtAuto) or (MvtClickBt) then
    Exit;
  ClickXR := RayDir.X; // emplacement du clic en X  Radial
  ClickYR := RayDir.Y; // emplacement du clic en Y  Radial
  MvtOn   := True;
end;

/// **********************************************
/// En cliquant sur le fond on arme la rotation
/// du cube
/// **********************************************
procedure TFrmMain.Viewport3D2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  ClickX := X; // emplacement du clic en X
  ClickY := Y; // emplacement du clic en Y
end;

/// **********************************************
/// le deplacement de la souris fait pivoter le cube
/// **********************************************
procedure TFrmMain.Viewport3D2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  IF MvtOn then
    Exit;
  if ssLeft in Shift then // si la souris bouge avec le clic droit enfoncé
  begin
    Direction := DiNone;
    IF (ClickX - X) > 3 then
      Direction := DiLeft;
    IF (X - ClickX) > 3 then
      Direction := DiRight;
    IF (ClickY - Y) > 3 then
      Direction := DiTop;
    IF (Y - ClickY) > 3 then
      Direction := DiBottom;

    if Direction <> DiNone then // une direction a été trouvée
    begin
      ClickX := X;
      ClickY := Y;
      if Direction = DiBottom then
      begin
        ArcDialX.Value := ArcDialX.Value - 5;
      end;
      if Direction = DiTop then
      begin
        ArcDialX.Value := ArcDialX.Value + 5;
      end;
      if Direction = DiLeft then
      begin
        ArcDialY.Value := ArcDialY.Value - 5;
      end;
      if Direction = DiRight then
      begin
        ArcDialY.Value := ArcDialY.Value + 5;
      end;
      ArcDialChange(Nil);
    end;
  end;
end;

/// **********************************************
/// Desarmement du mouvement
/// **********************************************
procedure TFrmMain.Viewport3D2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MvtOn := False;
end;

procedure TFrmMain.StickCornerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single;
  RayPos, RayDir: TVector3D);
begin
  MvtOn := False;
end;

procedure TFrmMain.StickCornerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
var
  PosXCube, PosYCube, PosZCube: int64;
begin
  if not MvtOn then
    Exit; // si pas de mouvement d'armé

  if ssLeft in Shift then // si la souris bouge avec le clic droit enfoncé
  begin
    Direction := DiNone;
    IF (ClickXR - RayDir.X) > 0.02 then
      Direction := DiLeft;
    IF (RayDir.X - ClickXR) > 0.02 then
      Direction := DiRight;
    IF (ClickYR - RayDir.Y) > 0.02 then
      Direction := DiTop;
    IF (RayDir.Y - ClickYR) > 0.02 then
      Direction := DiBottom;

    if Direction <> DiNone then // une direction a été trouvée
    begin
      cbFun.ItemIndex := -1;
      // recup de la position XYU du cube sous le stick
      PosXCube := Round(((Sender as TPlane).Parent as TCube).Position.X);
      PosYCube := Round(((Sender as TPlane).Parent as TCube).Position.Y);
      PosZCube := Round(((Sender as TPlane).Parent as TCube).Position.Z);
      // desarment du mouvement et reinitialisation de l'emplacement de la souris
      MvtOn   := False;
      ClickXR := RayDir.X;
      ClickYR := RayDir.Y;
      if (PosZCube = -3) and (SameValue((Sender as TPlane).Position.Z, -1.51, 0.1)) then // Plan Avant Rouge
        RotatePlan(PosXCube, PosYCube, Direction, DiTop, DiBottom, DiRight, DiLeft, 'g', 'G', 'w', 'W', 'Y', 'y', 'B', 'b', 'w',
          'W', 'Y', 'y')
      else if (PosZCube = 3) and (SameValue((Sender as TPlane).Position.Z, 1.51, 0.1)) then // Plan Arriere Orange
        RotatePlan(PosXCube, PosYCube, Direction, DiTop, DiBottom, DiRight, DiLeft, 'g', 'G', 'W', 'w', 'y', 'Y', 'B', 'b', 'W',
          'w', 'y', 'Y')
      else if (PosYCube = -3) and (SameValue((Sender as TPlane).Position.Y, -1.51, 0.1)) then // Plan Haut Blanc
        RotatePlan(PosXCube, PosZCube, Direction, DiTop, DiBottom, DiRight, DiLeft, 'g', 'G', 'R', 'r', 'o', 'O', 'B', 'b', 'R',
          'r', 'o', 'O')
      else if (PosYCube = 3) and (SameValue((Sender as TPlane).Position.Y, 1.51, 0.1)) then // Plan Bas Jaune
        RotatePlan(PosXCube, PosZCube, Direction, DiTop, DiBottom, DiRight, DiLeft, 'g', 'G', 'r', 'R', 'O', 'o', 'B', 'b', 'r',
          'R', 'O', 'o')
      else if (PosXCube = -3) and (SameValue((Sender as TPlane).Position.X, -1.51, 0.1)) then // Plan Gauche Vert
        RotatePlan(PosYCube, PosZCube, Direction, DiRight, DiLeft, DiBottom, DiTop, 'w', 'W', 'r', 'R', 'O', 'o', 'Y', 'y', 'r',
          'R', 'O', 'o')
      else if (PosXCube = 3) and (SameValue((Sender as TPlane).Position.X, 1.51, 0.1)) then // Plan Droit Bleu
        RotatePlan(PosYCube, PosZCube, Direction, DiRight, DiLeft, DiBottom, DiTop, 'w', 'W', 'R', 'r', 'o', 'O', 'Y', 'y', 'R',
          'r', 'o', 'O');
    end;
  end;
end;

procedure TFrmMain.RotatePlan(Pos1, Pos2: Single; TheDirection, Di1, Di2, Di3, Di4: TDirection;
                               P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12: Char);
begin
  if Pos1 = -3 then
  begin
    if (TheDirection = Di1) then
      Rotate(P1);
    if (TheDirection = Di2) then
      Rotate(P2);
    if Pos2 = -3 then
    begin
      if (TheDirection = Di3) then
        Rotate(P3);
      if (TheDirection = Di4) then
        Rotate(P4);
    end
    else
    begin
      if (TheDirection = Di3) then
        Rotate(P5);
      if (TheDirection = Di4) then
        Rotate(P6);
    end;
  end
  else
  begin // coté bas
    if (TheDirection = Di1) then
      Rotate(P7);
    if (TheDirection = Di2) then
      Rotate(P8);
    if Pos2 = -3 then
    begin
      if (TheDirection = Di3) then
        Rotate(P9);
      if (TheDirection = Di4) then
        Rotate(P10);
    end
    else
    begin
      if (TheDirection = Di3) then
        Rotate(P11);
      if (TheDirection = Di4) then
        Rotate(P12);
    end;
  end;
end;

{$ENDREGION}
{$REGION 'Sort the  cube (not solving)'}

procedure TFrmMain.BtnStartClick(Sender: TObject);
begin
  if MessageDlg('Restore initial cube?', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = MrNo then
    Exit;
  SortRubik();
  ArcDialX.Value      := -35;
  ArcDialY.Value      := -45;
  ArcDialZ.Value      := -18;
  ArcDialChange(nil);
  cbFun.ItemIndex     := -1;
  LblP2f.Repaint;
end;

procedure TFrmMain.BtParamsClick(Sender: TObject);
begin
  FloatAnimLayoutParam.Start;
end;

procedure TFrmMain.SortRubik();
begin
  SortCubes();
  SortSticks();
end;

procedure TFrmMain.SortSticks();
begin
  // face Rouge
  MoveStick( PLCC1R,     0,     0, -1.51,   0,   0, 0);
  MoveStick( PLCC2R,     0,     0, -1.51,   0,   0, 0);
  MoveStick( PLCC4R,     0,     0, -1.51,   0,   0, 0);
  MoveStick( PLCC3R,     0,     0, -1.51,   0,   0, 0);
  MoveStick( PLCA1R,     0,     0, -1.51,   0,   0, 0);
  MoveStick( PLCA2R,     0,     0, -1.51,   0,   0, 0);
  MoveStick( PLCA3R,     0,     0, -1.51,   0,   0, 0);
  MoveStick( PLCA4R,     0,     0, -1.51,   0,   0, 0);
  // Face Bleu
  MoveStick( PLCC2B,  1.51,     0,     0,   0, -90, 0);
  MoveStick( PLCC6B,  1.51,     0,     0,   0, -90, 0);
  MoveStick( PLCC3B,  1.51,     0,     0,   0, -90, 0);
  MoveStick( PLCC7B,  1.51,     0,     0,   0, -90, 0);
  MoveStick( PLCA6B,  1.51,     0,     0,   0, -90, 0);
  MoveStick( PLCA3B,  1.51,     0,     0,   0, -90, 0);
  MoveStick(PLCA11B,  1.51,     0,     0,   0, -90, 0);
  MoveStick( PLCA7B,  1.51,     0,     0,   0, -90, 0);
  // Face Verte
  MoveStick( PLCC5V, -1.51,     0,     0,   0,  90, 0);
  MoveStick( PLCC1V, -1.51,     0,     0,   0,  90, 0);
  MoveStick( PLCC8V, -1.51,     0,     0,   0,  90, 0);
  MoveStick( PLCC4V, -1.51,     0,     0,   0,  90, 0);
  MoveStick( PLCA5V, -1.51,     0,     0,   0,  90, 0);
  MoveStick(PLCA10V, -1.51,     0,     0,   0,  90, 0);
  MoveStick( PLCA8V, -1.51,     0,     0,   0,  90, 0);
  MoveStick( PLCA2V, -1.51,     0,     0,   0,  90, 0);
  // Orange
  MoveStick( PLCC6O,     0,     0,  1.51, 180,   0, 0);
  MoveStick( PLCC5O,     0,     0,  1.51, 180,   0, 0);
  MoveStick( PLCC7O,     0,     0,  1.51, 180,   0, 0);
  MoveStick( PLCC8O,     0,     0,  1.51, 180,   0, 0);
  MoveStick( PLCA9O,     0,     0,  1.51, 180,   0, 0);
  MoveStick(PLCA11O,     0,     0,  1.51, 180,   0, 0);
  MoveStick(PLCA10O,     0,     0,  1.51, 180,   0, 0);
  MoveStick(PLCA12O,     0,     0,  1.51, 180,   0, 0);
  // Face Blanche
  MoveStick( PLCC5W,     0, -1.51,     0, -90,   0, 0);
  MoveStick( PLCC6W,     0, -1.51,     0, -90,   0, 0);
  MoveStick( PLCC1W,     0, -1.51,     0, -90,   0, 0);
  MoveStick( PLCC2W,     0, -1.51,     0, -90,   0, 0);
  MoveStick( PLCA9W,     0, -1.51,     0, -90,   0, 0);
  MoveStick( PLCA5W,     0, -1.51,     0, -90,   0, 0);
  MoveStick( PLCA6W,     0, -1.51,     0, -90,   0, 0);
  MoveStick( PLCA1W,     0, -1.51,     0, -90,   0, 0);
  // Face Jaune
  MoveStick( PLCC4J,     0,  1.51,     0,  90,   0, 0);
  MoveStick( PLCC3J,     0,  1.51,     0,  90,   0, 0);
  MoveStick( PLCC8J,     0,  1.51,     0,  90,   0, 0);
  MoveStick( PLCC7J,     0,  1.51,     0,  90,   0, 0);
  MoveStick( PLCA4J,     0,  1.51,     0,  90,   0, 0);
  MoveStick( PLCA8J,     0,  1.51,     0,  90,   0, 0);
  MoveStick( PLCA7J,     0,  1.51,     0,  90,   0, 0);
  MoveStick(PLCA12J,     0,  1.51,     0,  90,   0, 0);
end;

procedure TFrmMain.SortCubes();
begin
  // les cubes coins
  SortCube( CC1, -3, -3, -3);
  SortCube( CC2,  3, -3, -3);
  SortCube( CC3,  3,  3, -3);
  SortCube( CC4, -3,  3, -3);
  SortCube( CC5, -3, -3,  3);
  SortCube( CC6,  3, -3,  3);
  SortCube( CC7,  3,  3,  3);
  SortCube( CC8, -3,  3,  3);
  // les cubes arretes
  SortCube( CA1,  0, -3, -3);
  SortCube( CA2, -3,  0, -3);
  SortCube( CA3,  3,  0, -3);
  SortCube( CA4,  0,  3, -3);
  SortCube( CA5, -3, -3,  0);
  SortCube( CA6,  3, -3,  0);
  SortCube( CA7,  3,  3,  0);
  SortCube( CA8, -3,  3,  0);
  SortCube( CA9,  0, -3,  3);
  SortCube(CA10, -3,  0,  3);
  SortCube(CA11,  3,  0,  3);
  SortCube(CA12,  0,  3,  3);
end;

procedure TFrmMain.SortCube(TheCube: TCube; Px, Py, Pz: Single);
begin
  TheCube.ResetRotationAngle;
  TheCube.Position.X := Px;
  TheCube.Position.Y := Py;
  TheCube.Position.Z := Pz;
end;

{$ENDREGION}
{$REGION 'Jouer des mouvements enregistrés'}

procedure TFrmMain.cbFunChange(Sender: TObject);
begin
  if cbFun.ItemIndex <> -1 then
  begin
    SortRubik();
    PlayMvt(FUN_VALUES[cbFun.ItemIndex]);
  end;
end;

procedure TFrmMain.PlayMvt(TheMvts: String);
var
  I: Integer;
  iMax: Integer;
begin
  // Platform.SetCursor(nil, crHourGlass); // ProcessMessages casse le curseur
  MvtAuto            := True;  // On enchaine une série (l'utilisateur ne peut pas faire de modif manuelle)
  iMax               := Length(TheMvts);
  pbFun.Value        := 1;
  pbFun.Max          := iMax;
  pbFun.Visible      := True;
  for I := 0 to iMax-1 do
  begin
    {$ZEROBASEDSTRINGS ON}
    Rotate(TheMvts[I]);
    {$ZEROBASEDSTRINGS OFF}
    pbFun.Value := pbFun.Value + 1;
  end;
  MvtAuto            := False; // La série est terminée, l'utilisateur peut à nouveau jouer
//  pbFun.Visible      := False;
end;

{$ENDREGION}

{$REGION 'Ouverture de la page Web'}

procedure TFrmMain.LblP2fClick(Sender: TObject);
begin
  LaunchWeb((Sender as TLabel).Text);
end;

{$ENDREGION}
{$REGION 'Rotation automatique du cube'}

/// *************************************************
/// Déplacement de la caméra autour du rubik's cube
/// *************************************************
Procedure TFrmMain.ArcDialChange(Sender: TObject);
begin
  Dummy.RotationAngle.X:=ArcDialX.Value;
  Dummy.RotationAngle.Y:=ArcDialY.Value;
  Dummy.RotationAngle.Z:=ArcDialZ.Value;
end;

/// *************************************************
/// Common procedure for the click on a color button
/// *************************************************
procedure TFrmMain.ColorBtnClick(Sender: TObject);
begin
  cbFun.ItemIndex := -1;
{$ZEROBASEDSTRINGS ON}
  if (not(MvtClickBt)) and (not(MvtAuto)) then
    Rotate((Sender as TButton).Text[0]);
{$ZEROBASEDSTRINGS OFF}
end;

{$ENDREGION}


end.
