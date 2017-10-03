unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
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
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    StatusBar1: TStatusBar;
    lblStatus: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Edit8Exit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.iPhone55in.fmx IOS}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Label1.Text := 'OK button pressed!';
end;

procedure TMainForm.Edit8Exit(Sender: TObject);
begin
{$IFDEF ANDROID}
  Button1Click(Self);
{$ENDIF}
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
{$IFDEF ANDROID}
  if Key = vkReturn then
  begin
    Key := vkTab;
    KeyDown(Key, KeyChar, Shift);
  end;
{$ENDIF}
end;

end.
