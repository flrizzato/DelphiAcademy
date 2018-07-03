unit uAbstractForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TAbstractForm = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TAbstractForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FormStyle = fsMDIChild then
    Action := caFree;
end;

procedure TAbstractForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TAbstractForm.FormShow(Sender: TObject);
begin
  if FormStyle = fsMDIChild then
  begin
    Top := 2;
    Left := 2;
  end;
end;

end.
