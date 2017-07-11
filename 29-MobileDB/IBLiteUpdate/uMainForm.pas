unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Phys.IBDef, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBLiteDef, Data.DB, FireDAC.Comp.Client, FireDAC.Phys.IBBase,
  FireDAC.Comp.UI, FMX.StdCtrls, FMX.Controls.Presentation,
  FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util, FireDAC.Comp.Script,
  FMX.Layouts, FMX.ListBox;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    butMetadata: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDConnection1: TFDConnection;
    ListBox1: TListBox;
    FDScript1: TFDScript;
    ListBox2: TListBox;
    FDScript2: TFDScript;
    procedure FDConnection1BeforeConnect(Sender: TObject);
    procedure butMetadataClick(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    function GetDBVersion: integer;
    procedure UpdateDBVersion;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  AppVersion: integer = 300;

implementation

{$R *.fmx}

procedure TForm1.butMetadataClick(Sender: TObject);
begin
  // tables
  FDConnection1.GetTableNames('', '', '', ListBox1.Items);
  ListBox1.ItemIndex := 0;
end;

procedure TForm1.FDConnection1BeforeConnect(Sender: TObject);
begin
{$IF DEFINED(MSWINDOWS)}
  FDConnection1.Params.Values['Database'] :=
    IncludeTrailingBackslash(ExtractFilePath(ParamStr(0))) + 'mastsql.gdb';
{$ELSE}
  FDConnection1.Params.Values['Database'] := TPath.GetDocumentsPath + PathDelim
    + 'mastsql.gdb';
{$ENDIF}
end;

procedure TForm1.ListBox1Change(Sender: TObject);
begin
  // fields
  FDConnection1.GetFieldNames('', '', ListBox1.Items[ListBox1.ItemIndex], '',
    ListBox2.Items);
  ListBox2.ItemIndex := 0;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  UpdateDBVersion;
end;

function TForm1.GetDBVersion: integer;
var
  v: variant;
begin
  try
    FDConnection1.Open;
    v := FDConnection1.ExecSQLScalar('SELECT VERSION FROM APPVERSION');
    if not VarIsNull(v) then
      Result := (v)
    else
      Result := -1;
  except
    on E: Exception do
    begin
      Result := -1;
      raise Exception.Create('GetDBVersion: ' + E.Message);
    end;
  end;
end;

procedure TForm1.UpdateDBVersion;
var
  i, CurVer, NewVer: integer;
begin
  CurVer := GetDBVersion;
  NewVer := AppVersion;

  try
    if NewVer > CurVer then
    begin
      for i := 0 to FDScript1.SQLScripts.Count - 1 do
      begin
        if (FDScript1.SQLScripts[i].Name.ToInteger > CurVer) and
          (FDScript1.SQLScripts[i].Name.ToInteger <= NewVer) then
        begin
          FDScript2.ExecuteScript(FDScript1.SQLScripts[i].SQL);
        end;
      end;
      FDConnection1.StartTransaction;
      FDConnection1.ExecSQL('UPDATE APPVERSION SET VERSION = ' +
        NewVer.ToString);
      FDConnection1.Commit;
    end;
  except
    on E: Exception do
      raise Exception.Create('UpdateDBVersion: ' + E.Message);
  end;
end;

end.
