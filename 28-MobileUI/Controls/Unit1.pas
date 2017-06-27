//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.DateTimeCtrls,
  FMX.ExtCtrls, FMX.ListBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts, FMX.TabControl,
  FMX.SearchBox, FMX.EditBox, FMX.SpinBox, FMX.ComboEdit, FMX.Controls.Presentation,
  FMX.ScrollBox;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
    ToolBarPage: TTabItem;
    ToolBar1: TToolBar;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton10: TSpeedButton;
    ToolBar13: TToolBar;
    SpeedButton7: TSpeedButton;
    ToolBar2: TToolBar;
    Label1: TLabel;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton79: TSpeedButton;
    SpeedButton80: TSpeedButton;
    ToolButtonPage: TTabItem;
    ToolBar5: TToolBar;
    SpeedButton27: TSpeedButton;
    SpeedButton31: TSpeedButton;
    SpeedButton30: TSpeedButton;
    SpeedButton29: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton28: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton25: TSpeedButton;
    SpeedButton26: TSpeedButton;
    SpeedButton22: TSpeedButton;
    ToolBar6: TToolBar;
    SpeedButton49: TSpeedButton;
    SpeedButton54: TSpeedButton;
    SpeedButton50: TSpeedButton;
    SpeedButton58: TSpeedButton;
    SpeedButton55: TSpeedButton;
    SpeedButton56: TSpeedButton;
    ToolBar7: TToolBar;
    SpeedButton59: TSpeedButton;
    SpeedButton48: TSpeedButton;
    SpeedButton60: TSpeedButton;
    SpeedButton53: TSpeedButton;
    SpeedButton57: TSpeedButton;
    SpeedButton52: TSpeedButton;
    SpeedButton51: TSpeedButton;
    ToolBar8: TToolBar;
    SpeedButton72: TSpeedButton;
    SpeedButton47: TSpeedButton;
    SpeedButton42: TSpeedButton;
    SpeedButton43: TSpeedButton;
    SpeedButton62: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton65: TSpeedButton;
    SpeedButton63: TSpeedButton;
    SpeedButton64: TSpeedButton;
    SpeedButton61: TSpeedButton;
    SpeedButton8: TSpeedButton;
    ToolBar9: TToolBar;
    SpeedButton20: TSpeedButton;
    SpeedButton40: TSpeedButton;
    SpeedButton34: TSpeedButton;
    SpeedButton38: TSpeedButton;
    SpeedButton33: TSpeedButton;
    SpeedButton32: TSpeedButton;
    SpeedButton35: TSpeedButton;
    SpeedButton41: TSpeedButton;
    SpeedButton39: TSpeedButton;
    SpeedButton36: TSpeedButton;
    SpeedButton37: TSpeedButton;
    Tabs: TTabItem;
    TabControl2: TTabControl;
    TabItem1: TTabItem;
    TabItem10: TTabItem;
    TabItem11: TTabItem;
    TabItem12: TTabItem;
    TabControl3: TTabControl;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    TabItem7: TTabItem;
    TabItem8: TTabItem;
    TabControl4: TTabControl;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    TabItem9: TTabItem;
    TabControl5: TTabControl;
    TabItem13: TTabItem;
    TabItem14: TTabItem;
    TabItem16: TTabItem;
    TabItem15: TTabItem;
    TabItem17: TTabItem;
    ListBoxPage: TTabItem;
    ListBox2: TListBox;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem14: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem17: TListBoxItem;
    ListBoxItem18: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxHeader1: TListBoxHeader;
    Label6: TLabel;
    SearchBox1: TSearchBox;
    ListBox3: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem15: TListBoxItem;
    ListBoxItem21: TListBoxItem;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    ListBoxItem22: TListBoxItem;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    ListBoxItem23: TListBoxItem;
    ListBoxItem24: TListBoxItem;
    Switch2: TSwitch;
    ListBoxItem25: TListBoxItem;
    ListBoxGroupFooter1: TListBoxGroupFooter;
    ListBoxItem7: TListBoxItem;
    ListBoxHeader2: TListBoxHeader;
    Label7: TLabel;
    ControlPage: TTabItem;
    ProgressBar1: TProgressBar;
    TrackBar1: TTrackBar;
    ProgressBar2: TProgressBar;
    TrackBar2: TTrackBar;
    ProgressBar3: TProgressBar;
    TrackBar3: TTrackBar;
    ProgressBar4: TProgressBar;
    TrackBar4: TTrackBar;
    Switch1: TSwitch;
    Label5: TLabel;
    AniIndicator1: TAniIndicator;
    Button15: TButton;
    SpeedButton68: TSpeedButton;
    SpeedButton69: TSpeedButton;
    SpeedButton70: TSpeedButton;
    Button7: TButton;
    Button8: TButton;
    SpeedButton73: TSpeedButton;
    SpeedButton74: TSpeedButton;
    TabItem18: TTabItem;
    ToolBar11: TToolBar;
    Edit4: TEdit;
    ClearEditButton1: TClearEditButton;
    Memo1: TMemo;
    Edit3: TEdit;
    Edit1: TEdit;
    ClearEditButton3: TClearEditButton;
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    PopupBox1: TPopupBox;
    ComboEdit1: TComboEdit;
    GroupBox1: TGroupBox;
    Layout2: TLayout;
    Edit2: TEdit;
    Edit5: TEdit;
    ClearEditButton2: TClearEditButton;
    SpeedButton3: TSpeedButton;
    SpinBox1: TSpinBox;
    DateEdit1: TDateEdit;
    StyleBook1: TStyleBook;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.iPhone55in.fmx IOS}

end.
