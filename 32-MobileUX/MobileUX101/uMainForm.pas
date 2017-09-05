unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Bind.GenData, Data.Bind.GenData, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.StdCtrls, FMX.ListView, FMX.Controls.Presentation,
  System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, FMX.Ani, FMX.Layouts, FMX.Effects, FMX.Edit, FMX.Objects;

type
  TMainForm = class(TForm)
    Layout1: TLayout;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    ListView1: TListView;
    RectAnimation1: TRectAnimation;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    BlurEffect1: TBlurEffect;
    Rectangle1: TRectangle;
    edUser: TEdit;
    edPass: TEdit;
    Layout2: TLayout;
    ShadowEffect1: TShadowEffect;
    StyleBook1: TStyleBook;
    Label1: TLabel;
    butOK: TButton;
    butCancel: TButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure butOKClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure ShowPassword;
    procedure HidePassword;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.iPhone47in.fmx IOS}

procedure TMainForm.butCancelClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.butOKClick(Sender: TObject);
begin
  if edUser.Text = 'abc' then
    if edPass.Text = '123' then
      HidePassword;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then begin
    Key := vkTab;
    KeyDown(Key, KeyChar, Shift);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ShowPassword;
end;

procedure TMainForm.HidePassword;
begin
  Layout1.Enabled := True;
  Rectangle1.Visible := False;
  BlurEffect1.Enabled := False;
end;

procedure TMainForm.ShowPassword;
begin
  Layout1.Enabled := False;
  Rectangle1.Visible := True;
  BlurEffect1.Enabled := True;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  PrototypeBindSource1.Active := not PrototypeBindSource1.Active;
end;

end.
