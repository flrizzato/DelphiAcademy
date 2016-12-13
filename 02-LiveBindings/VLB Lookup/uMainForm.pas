unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.Bind.GenData, Vcl.StdCtrls,
  Data.Bind.Components, Data.Bind.ObjectScope, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors,
  Data.Bind.Controls, Vcl.ExtCtrls, Vcl.Buttons, Vcl.Bind.Navigator;

type
  TForm1 = class(TForm)
    PrototypeBindSource1: TPrototypeBindSource;
    PrototypeBindSourceFill: TPrototypeBindSource;
    ListBox1: TListBox;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    Shape1: TShape;
    Edit1: TEdit;
    LinkControlToField1: TLinkControlToField;
    LinkPropertyToFieldBrushColor: TLinkPropertyToField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
