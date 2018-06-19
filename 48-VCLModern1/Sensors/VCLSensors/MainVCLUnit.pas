unit MainVCLUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
uses System.TypInfo, System.Sensors;

procedure TForm1.Button1Click(Sender: TObject);
var
  i : integer;
  NumberOfSensors : integer;
begin
  // get list of found sensors - if any
  TSensorManager.Current.Active := true;
  NumberofSensors := TSensorManager.Current.Count;
  Label1.Caption := 'Sensors: '+IntToStr(NumberOfSensors);
  Memo1.Lines.Clear;
  if NumberOfSensors = 0 then
    Memo1.Lines.Add('No Sensors Found')
  else
    for i := 0 to NumberOfSensors-1 do begin
      Memo1.Lines.Add(
        IntToStr(i)
        + ': '
        + TSensorManager.Current.Sensors[i].Name
        + '", Category: '
        + GetEnumName(System.TypeInfo(TSensorCategory),
              Ord(TSensorManager.Current.Sensors[i].Category))
      );
    end;
  TSensorManager.Current.Active := false;
end;

end.
