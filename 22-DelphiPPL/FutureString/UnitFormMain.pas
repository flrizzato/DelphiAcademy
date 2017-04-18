unit UnitFormMain;

{===============================================================================
 CodeRage 9 - Demo for Parallel Future

 This code shows how to use the parallel function Future.

 If you click on the Get button immediately after the Start Button the GUI
 will freeze for 3 seconds; the time it takes to get the string result from the
 Future. If you wait 3 seconds between clikcing Start and Get you'll get the
 string back immediately.

 Author: Danny Wind
===============================================================================}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Threading,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    ButtonStart: TButton;
    ButtonGet: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonGetClick(Sender: TObject);
  private
    { Private declarations }
    FutureString: IFuture<string>;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonGetClick(Sender: TObject);
begin
  ButtonGet.Text := FutureString.Value;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);
begin
  FutureString := TTask.Future<string>(
    function:string
    begin
      {Some calculation that takes time}
      Sleep(5000);
      Result := 'Hola Delphi Academy ' + Random(42).ToString;
    end);
end;

end.
