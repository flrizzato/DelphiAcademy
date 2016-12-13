unit MolHero.FormAdd;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TFormAdd = class(TForm)
    edtURL: TEdit;
    ToolBar1: TToolBar;
    spdbtnBack: TSpeedButton;
    lblEnterURL: TLabel;
    lblTitle: TLabel;
    btnAddMolecule: TButton;
    procedure spdbtnBackClick(Sender: TObject);
    procedure btnAddMoleculeClick(Sender: TObject);
  private
    procedure DoAddMolecule;
  public
    { Public declarations }
  end;

var
  FormAdd: TFormAdd;

implementation

{$R *.fmx}

uses MolHero.FormMain;

procedure TFormAdd.btnAddMoleculeClick(Sender: TObject);
begin
  DoAddMolecule;
  Close;
  FormMain.Show;
end;

procedure TFormAdd.DoAddMolecule;
begin
  ShowMessage('Importing from the URL will be next big feature of the next release of MOLECULE HERO app.');
end;

procedure TFormAdd.spdbtnBackClick(Sender: TObject);
begin
  Close;
  FormMain.Show;
end;

end.
