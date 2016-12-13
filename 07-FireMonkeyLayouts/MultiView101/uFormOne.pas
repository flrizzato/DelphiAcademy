unit uFormOne;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uFormBase, FMX.Layouts, FMX.Controls.Presentation;

type
  TFormOne = class(TFormBase)
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOne: TFormOne;

implementation

{$R *.fmx}

end.
