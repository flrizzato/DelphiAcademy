
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Advertising, FMX.Layouts, FMX.ListBox, FMX.StdCtrls,
  FMX.Calendar, FMX.Controls.Presentation, FMX.Edit, FMX.TabControl, FMX.ScrollBox, FMX.Memo, FMX.ListView.Types,
  FMX.ListView, Data.Bind.Components, Data.Bind.ObjectScope, Fmx.Bind.GenData, Data.Bind.GenData, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

type
  TForm1 = class(TForm)
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    Calendar1: TCalendar;
    Label4: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label6: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem4: TTabItem;
    Label1: TLabel;
    Label5: TLabel;
    Switch1: TSwitch;
    Label7: TLabel;
    Edit5: TEdit;
    TabItem3: TTabItem;
    Memo1: TMemo;
    Label8: TLabel;
    Label9: TLabel;
    Memo2: TMemo;
    Label10: TLabel;
    Memo3: TMemo;
    ListView1: TListView;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    ClearEditButton1: TClearEditButton;
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
