unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.MultiView, FMX.TabControl, FMX.Layouts,
  FMX.ListBox, System.ImageList, FMX.ImgList;

type
  TMainForm = class(TForm)
    MultiViewMain: TMultiView;
    ListBoxMainMenu: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    LayoutMain: TLayout;
    ToolBar1: TToolBar;
    butMultiView: TButton;
    LayoutContainer: TLayout;
    ImageList1: TImageList;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    procedure ListBoxItem1Click(Sender: TObject);
    procedure ListBoxItem2Click(Sender: TObject);
    procedure ListBoxItem3Click(Sender: TObject);
  private
    { Private declarations }
    FActiveForm: TForm;
    procedure FormOpen(aForm: TComponentClass);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses uFormOne, uFormThree, uFormTwo;

{ TForm1 }

procedure TMainForm.FormOpen(aForm: TComponentClass);
var
  i: Integer;
begin
  if (FActiveForm = nil) or (Assigned(FActiveForm) and
    (FActiveForm.ClassName <> aForm.ClassName)) then
  begin
    for i := LayoutContainer.ControlsCount - 1 downto 0 do
      LayoutContainer.RemoveObject(LayoutContainer.Controls[i]);

    FActiveForm.DisposeOf;
    FActiveForm := nil;

    Application.CreateForm(aForm, FActiveForm);
    LayoutContainer.AddObject(TLayout(FActiveForm.FindComponent('LayoutClient')));
  end;

  MultiViewMain.HideMaster;
end;

procedure TMainForm.ListBoxItem1Click(Sender: TObject);
begin
  FormOpen(TFormOne);
end;

procedure TMainForm.ListBoxItem2Click(Sender: TObject);
begin
  FormOpen(TFormTwo);
end;

procedure TMainForm.ListBoxItem3Click(Sender: TObject);
begin
  FormOpen(TFormThree);
end;

end.
