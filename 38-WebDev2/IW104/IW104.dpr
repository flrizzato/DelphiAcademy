program IW104;

uses
  IWRtlFix,
  IWJclStackTrace,
  IWJclDebug,
  Forms,
  IWStart,
  uJTableForm in 'uJTableForm.pas' {IWJTableForm: TIWAppForm},
  ServerController in 'ServerController.pas' {IWServerController: TIWServerControllerBase},
  UserSessionUnit in 'UserSessionUnit.pas' {IWUserSession: TIWUserSessionBase},
  DataSetConverter4D.Helper in '..\Common\DataSetConverter4D.Helper.pas',
  DataSetConverter4D.Impl in '..\Common\DataSetConverter4D.Impl.pas',
  DataSetConverter4D in '..\Common\DataSetConverter4D.pas',
  DataSetConverter4D.Util in '..\Common\DataSetConverter4D.Util.pas',
  uHandles in 'uHandles.pas';

{$R *.res}

begin
  TIWStart.Execute(True);
end.
