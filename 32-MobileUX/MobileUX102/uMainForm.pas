unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.MultiView,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.TabControl, FMX.Layouts,
  FMX.ListBox, FMX.Effects, FMX.Objects, FMX.ActnList, System.Actions,
  FMX.Gestures;

type
  TMainForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    MultiView1: TMultiView;
    ListBox1: TListBox;
    GridPanelLayoutMain: TGridPanelLayout;
    InventLayout: TLayout;
    Form1Image: TImage;
    GlowEffect1: TGlowEffect;
    Form1Label: TLabel;
    GlowEffect7: TGlowEffect;
    RotativoLayout: TLayout;
    Form2Image: TImage;
    GlowEffect2: TGlowEffect;
    Form2Label: TLabel;
    GlowEffect9: TGlowEffect;
    AnaliseLayout: TLayout;
    Form3Image: TImage;
    GlowEffect3: TGlowEffect;
    Form3Label: TLabel;
    GlowEffect8: TGlowEffect;
    SearchLayout: TLayout;
    EntradaLayout: TLayout;
    SaidaLayout: TLayout;
    LayoutMaster: TLayout;
    ActionListMain: TActionList;
    NextTabAction: TNextTabAction;
    PreviousTabAction: TPreviousTabAction;
    CloseFormAction: TAction;
    butBackButton: TButton;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    GestureManager1: TGestureManager;
    Label1: TLabel;
    procedure MultiView1StartShowing(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloseFormActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Form1ImageClick(Sender: TObject);
    procedure Form2ImageClick(Sender: TObject);
    procedure Form3ImageClick(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
    FActiveForm: TForm;
    procedure DoFormOpen(const aFormClass: TComponentClass);
    procedure DoCleanLayoutMaster;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses uFormOne, uFormThree, uFormTwo,
  FMX.VirtualKeyboard, FMX.Platform, FMX.DialogService;

procedure TMainForm.CloseFormActionExecute(Sender: TObject);
begin
  PreviousTabAction.Execute;
  DoCleanLayoutMaster;
end;

procedure TMainForm.DoCleanLayoutMaster;
var
  i: Integer;
begin
  for i := 0 to LayoutMaster.ChildrenCount - 1 do
    LayoutMaster.RemoveObject(LayoutMaster.Children[i]);
end;

procedure TMainForm.DoFormOpen(const aFormClass: TComponentClass);
begin
  if Assigned(FActiveForm) then
    FreeAndNil(FActiveForm);
  Application.CreateForm(aFormClass, FActiveForm);
  LayoutMaster.AddObject(TLayout(FActiveForm.FindComponent('LayoutDetail')));
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FActiveForm := nil;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  FService: IFMXVirtualKeyboardService;
begin
  if Key = vkHardwareBack then
  begin
    TPlatformServices.Current.SupportsPlatformService
      (IFMXVirtualKeyboardService, IInterface(FService));
    if (FService <> nil) and (TVirtualKeyboardState.Visible
      in FService.VirtualKeyBoardState) then
    begin
      // do nothing...
    end
    else
    begin
      if TabControl1.TabIndex = 0 then
      begin
        Key := 0;
        TDialogService.MessageDialog('Close the Application?',
          TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes],
          TMsgDlgBtn.mbYes, 0,
          procedure(const AResult: TModalResult)
          begin
            case AResult of
              mrYES:
                begin
                  Application.Terminate;
                end;
            end;
          end);
      end
      else
      begin
        Key := 0;
        butBackButton.Action.Execute;
      end;
    end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
{$IFDEF ANDROID}
  CloseFormAction.Visible := False;
{$ELSE}
  CloseFormAction.Visible := True;
{$ENDIF}
end;

procedure TMainForm.ListBox1ItemClick(const Sender: TCustomListBox;
const Item: TListBoxItem);
begin
  DoCleanLayoutMaster;
  if Item.Index = 0 then
    CloseFormActionExecute(Self)
  else if Item.Index = 1 then
    Form1ImageClick(Self)
  else if Item.Index = 2 then
    Form2ImageClick(Self)
  else if Item.Index = 3 then
    Form3ImageClick(Self);
  MultiView1.HideMaster;
end;

procedure TMainForm.MultiView1StartShowing(Sender: TObject);
begin
  MultiView1.Width := 150;
end;

procedure TMainForm.Form1ImageClick(Sender: TObject);
begin
  DoFormOpen(TForm1);
  NextTabAction.Execute;
end;

procedure TMainForm.Form2ImageClick(Sender: TObject);
begin
  DoFormOpen(TForm2);
  NextTabAction.Execute;
end;

procedure TMainForm.Form3ImageClick(Sender: TObject);
begin
  DoFormOpen(TForm3);
  NextTabAction.Execute;
end;

end.
