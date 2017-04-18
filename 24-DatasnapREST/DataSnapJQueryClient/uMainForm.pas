unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Edit3: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses ClientClassesUnit1, ClientModuleUnit1;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Temp: TServerMethods1Client;
  A, B: Double;
begin
  Temp := TServerMethods1Client.Create(ClientModule1.DSRESTConnection1);
  try
    A := StrToFloat(Edit1.Text);
    B := StrToFloat(Edit2.Text);
    Edit3.Text := FloatToStr(Temp.Sum(A, B));
  finally
    Temp.Free;
  end;
end;

end.
