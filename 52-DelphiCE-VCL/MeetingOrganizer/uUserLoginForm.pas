unit uUserLoginForm;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs, uAbstractForm, StdCtrls, Buttons;

type
  TUserLoginForm = class(TAbstractForm)
    edtUserLogin: TEdit;
    edtPassword: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblUserLogin: TLabel;
    lblPassword: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edtUserLoginChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserLoginForm: TUserLoginForm;

implementation

uses uUserControl;

{$R *.dfm}

procedure TUserLoginForm.edtUserLoginChange(Sender: TObject);
begin
  inherited;
  btnOK.Enabled := (edtUserLogin.Text <> '') and (edtPassword.Text <> '');
end;

procedure TUserLoginForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  if ModalResult = mrOK then
  begin
    if not TUserControl.GetInstance.ValidateUserLogin(edtUserLogin.Text,
      edtPassword.Text) then
    begin
      MessageDlg('Invalid user/password!', mtWarning, [mbOK], 0);
      CanClose := False;
    end
    else
      TUserControl.GetInstance.AuthorizationLog;
  end;
end;

end.
