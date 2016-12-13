unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Data.Bind.GenData, System.Rtti, FMX.Layouts, FMX.Grid, Fmx.Bind.Navigator,
  Data.Bind.Components, Data.Bind.ObjectScope, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Bindings.Outputs, Data.Bind.Grid, Fmx.Bind.Editors,
  Fmx.Bind.Grid, FMX.Edit, Fmx.Bind.GenData, Data.Bind.Controls, FMX.Grid.Style,
  FMX.Controls.Presentation, FMX.ScrollBox;

type
  TForm1 = class(TForm)
    PrototypeBindSource1: TPrototypeBindSource;
    NavigatorPrototypeBindSource1: TBindNavigator;
    BindingsList1: TBindingsList;
    Grid1: TGrid;
    EditContactName1: TEdit;
    LabelContactName1: TLabel;
    LinkControlToFieldContactName1: TLinkControlToField;
    LinkGridToDataSourcePrototypeBindSource1: TLinkGridToDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.
