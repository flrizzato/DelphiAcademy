unit TransitionUnit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Effects, FMX.Filter.Effects, FMX.Edit, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.TabControl, FMX.Ani, FMX.Layouts, FMX.ListBox;

type
  TForm27 = class(TForm)
    Button2: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Edit1: TEdit;
    Button1: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Tab2Layout: TLayout;
    Tab1Layout: TLayout;
    FloatAnimation1: TFloatAnimation;
    TransitionEffect1: TRotateCrumpleTransitionEffect;
    CheckBox1: TCheckBox;
    ProgressBar1: TProgressBar;
    FloatAnimation2: TFloatAnimation;
    TrackBar1: TTrackBar;
    FloatAnimation3: TFloatAnimation;
    ArcDial1: TArcDial;
    procedure Button2Click(Sender: TObject);
    procedure FloatAnimation1Finish(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form27: TForm27;

implementation

{$R *.fmx}

procedure TForm27.Button2Click(Sender: TObject);
begin
  TransitionEffect1.Enabled := False;
  TransitionEffect1.RandomSeed := Random;

  TransitionEffect1.Parent := TabControl1.ActiveTab;

  TransitionEffect1.Enabled := True;
  FloatAnimation1.Start;
end;

procedure TForm27.FloatAnimation1Finish(Sender: TObject);
begin
  TransitionEffect1.Enabled := False;
  if TabControl1.ActiveTab = TabItem1 then
    TabControl1.ActiveTab := TabItem2
  else
    TabControl1.ActiveTab := TabItem1;
end;

procedure TForm27.FloatAnimation1Process(Sender: TObject);
var
  update: TBitmap;
begin
  if TabControl1.ActiveTab = TabItem1 then
    Update := Tab2Layout.MakeScreenshot
  else
    Update := Tab1Layout.MakeScreenshot;
  TransitionEffect1.Target.Assign(Update);
  Update.Free;
end;

procedure TForm27.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab := TabItem2;
  TabControl1.ActiveTab := TabItem1;
end;

initialization
  Randomize;

end.
