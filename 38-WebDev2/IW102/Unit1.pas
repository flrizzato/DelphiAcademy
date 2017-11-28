unit Unit1;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes, IWCompGrids,
  IWDBGrids, Vcl.Controls, IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl,
  IWControl, IWDBStdCtrls, Data.DB, IWVCLComponent, IWBaseLayoutComponent,
  IWBaseContainerLayout, IWContainerLayout, IWTemplateProcessorHTML;

type
  TIWFormDepto = class(TIWAppForm)
    IWDBGrid1: TIWDBGrid;
    DataSource1: TDataSource;
    IWTemplateProcessorHTML1: TIWTemplateProcessorHTML;
    procedure IWAppFormShow(Sender: TObject);
  public
  end;

implementation

{$R *.dfm}

uses UserSessionUnit;


procedure TIWFormDepto.IWAppFormShow(Sender: TObject);
begin
  DataSource1.DataSet.Open;
end;

initialization
  TIWFormDepto.SetAsMainForm;

end.
