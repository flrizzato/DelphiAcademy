unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.StdCtrls, FMX.ListView, FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.Controls.Presentation, Data.Bind.EngExt, FMX.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, FMX.Bind.GenData, Data.Bind.GenData,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Bind.Editors, FMX.Ani;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    imgLogo: TImage;
    Label1: TLabel;
    ToolBar2: TToolBar;
    butLogOut: TButton;
    butApprove: TButton;
    butReject: TButton;
    TabControl1: TTabControl;
    tabLogin: TTabItem;
    LayoutLogin: TLayout;
    edtPassword: TEdit;
    PasswordEditButton1: TPasswordEditButton;
    edtUsername: TEdit;
    ClearEditButton1: TClearEditButton;
    butLogin: TButton;
    tabDocumentos: TTabItem;
    StyleBook1: TStyleBook;
    ActionList1: TActionList;
    actLogin: TAction;
    actLogout: TAction;
    actShowTabs: TAction;
    actHideTabs: TAction;
    NextTabAction1: TNextTabAction;
    PreviousTabAction1: TPreviousTabAction;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    PrototypeBindSource1: TPrototypeBindSource;
    ListView1: TListView;
    RectAnimation1: TRectAnimation;
    LinkFillControlToField2: TLinkFillControlToField;
    labelLoggedInUser: TLabel;
    actShowData: TAction;
    procedure actLoginExecute(Sender: TObject);
    procedure actShowTabsExecute(Sender: TObject);
    procedure actHideTabsExecute(Sender: TObject);
    procedure actLogoutExecute(Sender: TObject);
    procedure edtUsernameChangeTracking(Sender: TObject);
    procedure edtPasswordChangeTracking(Sender: TObject);
    procedure actShowDataExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
    function UserLogin(sUser, sPass: string): boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

uses FMX.VirtualKeyboard, FMX.Platform, FMX.DialogService;

procedure TMainForm.actHideTabsExecute(Sender: TObject);
begin
  actLogout.Enabled := False;
  PreviousTabAction1.ExecuteTarget(nil);
end;

procedure TMainForm.actLoginExecute(Sender: TObject);
begin
  if UserLogin(edtUsername.Text, edtPassword.Text) then
  begin
    actShowTabs.Execute;
    labelLoggedInUser.Text := 'User: ' + edtUsername.Text;
  end
  else
  begin
    actHideTabs.Execute;
    labelLoggedInUser.Text := 'Invalid Username/Password!';
  end;
  edtPassword.Text := '';
end;

procedure TMainForm.actLogoutExecute(Sender: TObject);
begin
  labelLoggedInUser.Text := '';
  actHideTabs.Execute;
end;

procedure TMainForm.actShowDataExecute(Sender: TObject);
begin
  PrototypeBindSource1.Active := False;
  PrototypeBindSource1.Active := True;
end;

procedure TMainForm.actShowTabsExecute(Sender: TObject);
begin
  actShowData.Execute;
  actLogout.Enabled := True;
  NextTabAction1.ExecuteTarget(nil);
end;

procedure TMainForm.edtPasswordChangeTracking(Sender: TObject);
begin
  actLogin.Enabled := (edtUsername.Text.Length >= 3) and
    (edtPassword.Text.Length >= 3) and not actLogout.Enabled;
end;

procedure TMainForm.edtUsernameChangeTracking(Sender: TObject);
begin
  actLogin.Enabled := (edtUsername.Text.Length >= 3) and
    (edtPassword.Text.Length >= 3) and not actLogout.Enabled;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then begin
    Key := vkTab;
    KeyDown(Key, KeyChar, Shift);
  end;
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
        actLogout.Execute;
      end;
    end;
  end;
end;

function TMainForm.UserLogin(sUser, sPass: string): boolean;
begin
  Result := (sUser = 'abc') and (sPass = '123');
end;

end.
