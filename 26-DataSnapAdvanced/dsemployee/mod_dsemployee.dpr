library mod_dsemployee;

{$R *.dres}

uses
  {$IFDEF MSWINDOWS}
  Winapi.ActiveX,
  {$ENDIF }
  Web.WebBroker,
  Web.ApacheApp,
  Web.HTTPD24Impl,
  Data.DBXCommon,
  Datasnap.DSSession,
  ServerContainerUnit1 in 'ServerContainerUnit1.pas' {ServerContainer1: TDataModule},
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  uEmployeeServer in 'uEmployeeServer.pas' {DSEmployeeServer: TDataModule},
  DataSetConverter4D.Util in 'Lib\DataSetConverter4D.Util.pas',
  DataSetConverter4D.Helper in 'Lib\DataSetConverter4D.Helper.pas',
  DataSetConverter4D.Impl in 'Lib\DataSetConverter4D.Impl.pas',
  DataSetConverter4D in 'Lib\DataSetConverter4D.pas';

{$R *.res}

// httpd.conf entries:
//
(*
 LoadModule dsemployee_module modules/mod_dsemployee.dll

 <Location /xyz>
    SetHandler mod_dsemployee-handler
 </Location>
*)
//
// These entries assume that the output directory for this project is the apache/modules directory.
//
// httpd.conf entries should be different if the project is changed in these ways:
//   1. The TApacheModuleData variable name is changed
//   2. The project is renamed.
//   3. The output directory is not the apache/modules directory
//

// Declare exported variable so that Apache can access this module.
var
  GModuleData: TApacheModuleData;
exports
  GModuleData name 'dsemployee_module';

procedure TerminateThreads;
begin
  TDSSessionManager.Instance.Free;
  Data.DBXCommon.TDBXScheduler.Instance.Free;
end;

begin
{$IFDEF MSWINDOWS}
  CoInitFlags := COINIT_MULTITHREADED;
{$ENDIF}
  Web.ApacheApp.InitApplication(@GModuleData);
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  TApacheApplication(Application).OnTerminate := TerminateThreads;
  Application.Run;
end.
