unit uMainDM;

interface

uses
  SysUtils, Classes, DB, WideStrings, Forms, FireDAC.Phys.IB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Comp.Client, FireDAC.DBX.Migrate, FireDAC.VCLUI.Wait,
  FireDAC.Phys.IBLiteDef, FireDAC.Phys.IBDef, FireDAC.Phys.IBBase,
  FireDAC.Comp.UI;

type
  TMainDM = class(TDataModule)
    SQLConnection: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    procedure DataModuleCreate(Sender: TObject);
    procedure SQLConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainDM: TMainDM;

implementation

uses uMainForm, System.IOUtils, Winapi.Windows, Vcl.Dialogs;

{$R *.dfm}

procedure TMainDM.DataModuleCreate(Sender: TObject);
begin
  try
    if not SQLConnection.Connected then
      SQLConnection.Open;
    TMainForm(Application.MainForm).DBConnection := SQLConnection;
  except
    on E: Exception do
      raise Exception.Create('Error Message: ' + E.Message);
  end;
end;

procedure TMainDM.SQLConnectionBeforeConnect(Sender: TObject);
var
  fDebugMode: boolean;
  fDBTemp, fDBFile: string;

  function GetAppDataFolder: string;
  var
    Path: array [0 .. MAX_PATH + 1] of Char;
  begin
    if ExpandEnvironmentStrings('%LOCALAPPDATA%', Path, Length(Path)) > 1 then
      Result := IncludeTrailingPathDelimiter(Path)
    else
      Result := '';
  end;

begin
{$IF DEFINED(DEBUG)}
  fDebugMode := True;
{$ELSE}
  fDebugMode := False;
{$ENDIF}
  if not fDebugMode then
  begin
    fDBTemp := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
      'MEETINGORGANIZER.IB';
    fDBFile := GetAppDataFolder + 'MEETINGORGANIZER.IB';

    try
      if not TFile.Exists(fDBFile) then
        TFile.Copy(fDBTemp, fDBFile);

      if TFile.Exists(fDBFile) then
        SQLConnection.Params.Values['Database'] := fDBFile
      else
        raise Exception.Create('Error Message: ' + fDBFile + ' not found!');
    except
      on E: Exception do
        raise Exception.Create('Error Message' + E.Message);
    end;
  end
  else
  begin
    // keeps the DM configuration...
  end;
end;

end.
