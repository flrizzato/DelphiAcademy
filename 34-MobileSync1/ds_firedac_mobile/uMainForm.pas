unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.StdCtrls, FMX.Controls.Presentation, System.Rtti,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, FMX.DateTimeCtrls,
  FMX.Layouts, FMX.Ani;

type
  TMainForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ToolBar1: TToolBar;
    SpeedButton3: TSpeedButton;
    ListView1: TListView;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    ToolBar2: TToolBar;
    DateToSync: TDateEdit;
    SpeedButton1: TSpeedButton;
    LayoutStatus: TLayout;
    LabelStatus: TLabel;
    TrackBarStatus: TTrackBar;
    FloatAnimationStatus: TFloatAnimation;
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses ClientModuleUnit1;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  ClientModule1.LoadParts(DateToSync.Text, LayoutStatus, LabelStatus,
    FloatAnimationStatus);
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
begin
  ClientModule1.qryPARTS.Close;
  ClientModule1.qryPARTS.Open;
end;

end.
