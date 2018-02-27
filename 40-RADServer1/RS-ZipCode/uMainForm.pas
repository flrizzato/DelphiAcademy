unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.Controls.Presentation, FMX.StdCtrls, IPPeerClient,
  REST.Backend.ServiceTypes, System.JSON, REST.Backend.EMSServices, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, FMX.ScrollBox, FMX.Memo,
  Data.Bind.ObjectScope, REST.Client, REST.Backend.EndPoint,
  REST.Backend.EMSProvider;

type
  TForm1 = class(TForm)
    butConsulta: TButton;
    edtConsulta: TEdit;
    EMSProvider1: TEMSProvider;
    BackendEndpoint1: TBackendEndpoint;
    Memo1: TMemo;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    edtCEP: TEdit;
    edtLogradouro: TEdit;
    edtBairro: TEdit;
    edtCidade: TEdit;
    edtUF: TEdit;
    procedure butConsultaClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses uCEPManager;

procedure TForm1.butConsultaClick(Sender: TObject);
var aCEP: TMyCEP;
begin
  if edtConsulta.Text <> '' then
    begin
      BackendEndpoint1.ResourceSuffix := edtConsulta.Text;
      BackendEndpoint1.Execute;

      aCEP := uCEPManager.TCEPManager.JSON2CEP(BackendEndpoint1.Response.JSONText);

      edtCEP.Text := aCEP.fCEP;
      edtLogradouro.Text := aCEP.fLogradouro;
      edtBairro.Text := aCEP.fBairro;
      edtCidade.Text := aCEP.fCidade;
      edtUF.Text := aCEP.fUF;
    end;
end;

end.
