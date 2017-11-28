unit Unit1;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes, IWCompButton,
  IWCompListbox, IWCompLabel, Vcl.Controls, IWVCLBaseControl, IWBaseControl,
  IWBaseHTMLControl, IWControl, IWCompEdit, IWVCLComponent,
  IWBaseLayoutComponent, IWBaseContainerLayout, IWContainerLayout,
  IWLayoutMgrHTML;

type
  TIWForm1 = class(TIWAppForm)
    IWEdit1: TIWEdit;
    IWLabel1: TIWLabel;
    IWListbox1: TIWListbox;
    IWButton1: TIWButton;
    IWLayoutMgrHTML1: TIWLayoutMgrHTML;
    procedure IWButton1Click(Sender: TObject);
  public
  end;

implementation

{$R *.dfm}


procedure TIWForm1.IWButton1Click(Sender: TObject);
begin
  IWListbox1.Items.Add(IWEdit1.Text);
end;

initialization
  TIWForm1.SetAsMainForm;

end.
