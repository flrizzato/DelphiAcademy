unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  uClassHelpersDemo, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    butTStrings: TButton;
    lsbSearch: TListBox;
    edtSearch: TEdit;
    butTStringHelper: TButton;
    procedure butTStringsClick(Sender: TObject);
    procedure butTStringHelperClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.butTStringsClick(Sender: TObject);
begin
  if lsbSearch.Items.Contains(edtSearch.Text) then
    ShowMessage('Ok!!!');
end;

procedure TMainForm.butTStringHelperClick(Sender: TObject);
var
  MyString: String;
begin
  MyString := 'This is a string.';

  ShowMessage(MyString.IndexOf('a').ToString); //Working with zero-based or 1-based strings
  ShowMessage(Pos('a', MyString).ToString); //Working with 1-based strings
end;

end.
