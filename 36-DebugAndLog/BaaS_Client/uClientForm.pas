unit uClientForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  Data.Bind.EngExt, FMX.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  FMX.Bind.Editors, Data.Bind.Components, Data.Bind.DBScope, FMX.Ani,
  FMX.StdCtrls, FMX.Layouts, FMX.DateTimeCtrls, FMX.ListView,
  FMX.Controls.Presentation, FMX.TabControl, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    ToolBar1: TToolBar;
    SpeedButton3: TSpeedButton;
    ListView1: TListView;
    TabItem2: TTabItem;
    ToolBar2: TToolBar;
    DateToSync: TDateEdit;
    SpeedButton1: TSpeedButton;
    LayoutStatus: TLayout;
    LabelStatus: TLabel;
    TrackBarStatus: TTrackBar;
    FloatAnimationStatus: TFloatAnimation;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
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

uses uClientDM;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  ClientDM.LoadParts(DateToSync.Date, LayoutStatus, LabelStatus,
    FloatAnimationStatus);
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
begin
  ClientDM.qryPARTS.Close;
  ClientDM.qryPARTS.Open;
end;

end.
