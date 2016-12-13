unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses Androidapi.Helpers,
  Androidapi.JNI.base64coder;

{$R *.NmXhdpiPh.fmx ANDROID}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit2.Text := JStringToString(TJBase64Coder.JavaClass.encodeString
    (StringToJString(Edit1.Text)));
end;

end.
