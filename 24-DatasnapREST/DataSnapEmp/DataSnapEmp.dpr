program DataSnapEmp;
{$APPTYPE GUI}

{$R *.dres}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  FormUnit1 in 'FormUnit1.pas' {Form1},
  ServerMethodsUnit1 in 'ServerMethodsUnit1.pas' {ServerMethods1: TDataModule},
  ServerContainerUnit1 in 'ServerContainerUnit1.pas' {ServerContainer1: TDataModule},
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  uEmployeeServer in 'uEmployeeServer.pas' {DSEmployeeServer: TDataModule},
  DataSetConverter4D in 'Lib\DataSetConverter4D.pas',
  DataSetConverter4D.Impl in 'Lib\DataSetConverter4D.Impl.pas',
  DataSetConverter4D.Util in 'Lib\DataSetConverter4D.Util.pas',
  DataSetConverter4D.Helper in 'Lib\DataSetConverter4D.Helper.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
