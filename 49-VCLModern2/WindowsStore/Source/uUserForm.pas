unit uUserForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uAbstractDataTabForm, DB, DBActns, ActnList, ImgList, Grids,
  DBGrids, StdCtrls, Buttons, ExtCtrls, ComCtrls, ToolWin, Mask, DBCtrls,
  System.Actions, System.ImageList;

type
  TUserForm = class(TAbstractDataTabForm)
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    Label5: TLabel;
    DBEdit5: TDBEdit;
    Label6: TLabel;
    DBEdit6: TDBEdit;
    Label7: TLabel;
    DBComboBox1: TDBComboBox;
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fUserForm: TUserForm;

implementation

uses uUserDM;

{$R *.dfm}

procedure TUserForm.FormDestroy(Sender: TObject);
begin
  inherited;
  fUserForm := nil;
end;

end.
