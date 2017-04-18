unit UnitFormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    ButtonTask1: TButton;
    ScrollBarActivity: TScrollBar;
    FloatAnimationActivity: TFloatAnimation;
    LabelTask1: TLabel;
    procedure ButtonTask1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Threading;

procedure TFormMain.ButtonTask1Click(Sender: TObject);
var
  lValue: Integer;
begin
  LabelTask1.Text := '--';
  TTask.Run( procedure
             begin
               {Some calculation that takes time}
               Sleep(3000);
               lValue := Random(10);
               TThread.Synchronize(nil, procedure
                                        begin
                                          LabelTask1.Text := lValue.ToString;
                                        end);
             end );
end;


end.
