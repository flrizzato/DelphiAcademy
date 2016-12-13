unit EffectUnit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Filter.Effects, FMX.Objects,
  FMX.Layouts, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components;

type
  TForm30 = class(TForm)
    Switch1: TSwitch;
    GlowEffect1: TGlowEffect;
    CheckBox1: TCheckBox;
    ShadowEffect1: TShadowEffect;
    ShadowEffect2: TShadowEffect;
    Image1: TImage;
    SmoothMagnifyEffect1: TSmoothMagnifyEffect;
    Layout1: TLayout;
    Layout2: TLayout;
    CheckBox2: TCheckBox;
    BindingsList1: TBindingsList;
    LinkControlToPropertyEnabled: TLinkControlToProperty;
    LinkControlToPropertyVisible: TLinkControlToProperty;
    procedure Layout2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form30: TForm30;

implementation

{$R *.fmx}

procedure TForm30.FormCreate(Sender: TObject);
begin
  Layout2.Visible := True;
end;

procedure TForm30.Layout2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  SmoothMagnifyEffect1.Center := TPointF.Create(x,y);
end;

end.
