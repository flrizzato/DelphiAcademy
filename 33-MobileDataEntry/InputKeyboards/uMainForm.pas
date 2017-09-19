unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.ListBox, FMX.Layouts, FMX.StdCtrls, FMX.Controls.Presentation,
  qdac_fmx_vkhelper;

type
  TMainForm = class(TForm)
    ToolBarMain: TToolBar;
    Label1: TLabel;
    ListBoxMain: TListBox;
    ListBoxItem1: TListBoxItem;
    Edit1: TEdit;
    ListBoxItem2: TListBoxItem;
    Edit2: TEdit;
    ListBoxItem3: TListBoxItem;
    Edit3: TEdit;
    ListBoxItem4: TListBoxItem;
    Edit4: TEdit;
    ListBoxItem5: TListBoxItem;
    Edit5: TEdit;
    ListBoxItem6: TListBoxItem;
    Edit6: TEdit;
    ListBoxItem7: TListBoxItem;
    Edit7: TEdit;
    ListBoxItem8: TListBoxItem;
    Edit8: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

end.
