unit uRoomForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uAbstractDataTabForm, DB, DBActns, ActnList, ImgList, Grids,
  DBGrids, StdCtrls, Buttons, ExtCtrls, ComCtrls, ToolWin, Mask, DBCtrls,
  System.Actions, System.ImageList;

type
  TRoomForm = class(TAbstractDataTabForm)
    Label1: TLabel;
    DBEdit1: TDBEdit;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fRoomForm: TRoomForm;

implementation

uses uRoomDM;

{$R *.dfm}

procedure TRoomForm.FormDestroy(Sender: TObject);
begin
  inherited;
  fRoomForm := nil;
end;

end.
