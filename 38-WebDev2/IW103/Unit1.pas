unit Unit1;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes, IWHTMLControls,
  IWCompCheckbox, IWCompButton, Vcl.Controls, IWVCLBaseControl, IWBaseControl,
  IWBaseHTMLControl, IWControl, IWCompEdit, IWVCLComponent,
  IWBaseLayoutComponent, IWBaseContainerLayout, IWContainerLayout,
  IWTemplateProcessorHTML;

type
  TIWLogin = class(TIWAppForm)
    txtUser: TIWEdit;
    txtPassword: TIWEdit;
    btnLogin: TIWButton;
    chkRemember: TIWCheckBox;
    LinkForgot: TIWLink;
    IWTemplateProcessorHTML1: TIWTemplateProcessorHTML;
  public
  end;

implementation

{$R *.dfm}


initialization
  TIWLogin.SetAsMainForm;

end.
