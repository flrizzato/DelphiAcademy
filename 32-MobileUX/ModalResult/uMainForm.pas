unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
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

uses uChildForm;

procedure TForm1.Button1Click(Sender: TObject);
var
  dlg: TForm2;
begin
  dlg := TForm2.Create(nil);
  // select current value, if avaialble in the list
  dlg.ListBox1.ItemIndex := dlg.ListBox1.Items.IndexOf(Edit1.Text);
  dlg.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      if ModalResult = mrOK then
      // if OK was pressed and an item is selected, pick it
        if dlg.ListBox1.ItemIndex >= 0 then
          edit1.Text := dlg.ListBox1.Items [dlg.ListBox1.ItemIndex];
    end);
end;

end.
