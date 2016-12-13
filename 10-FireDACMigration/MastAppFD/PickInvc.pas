unit PickInvc;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DBCtrls;

type
  TPickOrderNoDlg = class(TForm)
    CancelBtn: TButton;
    OKBtn: TButton;
    DBLookupListBox1: TDBLookupListBox;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PickOrderNoDlg: TPickOrderNoDlg;

implementation

uses DataMod;

{$R *.dfm}

procedure TPickOrderNoDlg.FormShow(Sender: TObject);
begin
  MastData.Orders.Open;
end;

end.
