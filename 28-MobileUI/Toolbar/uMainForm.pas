unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Actions,
  FMX.ActnList, FMX.Effects, FMX.Filter.Effects, FMX.Objects, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    Layout1: TLayout;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Image1: TImage;
    BlurEffect1: TBlurEffect;
    SharpenEffect1: TSharpenEffect;
    GlowEffect1: TGlowEffect;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.iPhone55in.fmx IOS}
{$R *.NmXhdpiPh.fmx ANDROID}

{ TForm1 }

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
   BlurEffect1.Enabled := SpeedButton1.IsPressed;
   GlowEffect1.Enabled := SpeedButton2.IsPressed;
   SharpenEffect1.Enabled := SpeedButton3.IsPressed;
end;

end.
