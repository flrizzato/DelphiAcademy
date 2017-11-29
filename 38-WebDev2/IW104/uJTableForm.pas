unit uJTableForm;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes, IWVCLComponent,
  IWBaseLayoutComponent, IWBaseContainerLayout, IWContainerLayout,
  IWTemplateProcessorHTML;

type
  TIWJTableForm = class(TIWAppForm)
    IWTemplateProcessorHTML1: TIWTemplateProcessorHTML;
  public
  end;

implementation

{$R *.dfm}


initialization
  TIWJTableForm.SetAsMainForm;

end.
