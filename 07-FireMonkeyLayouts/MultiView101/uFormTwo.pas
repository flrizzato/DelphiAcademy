unit uFormTwo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uFormBase, FMX.Layouts, FMX.Controls.Presentation;

type
  TFormTwo = class(TFormBase)
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTwo: TFormTwo;

implementation

{$R *.fmx}

end.
