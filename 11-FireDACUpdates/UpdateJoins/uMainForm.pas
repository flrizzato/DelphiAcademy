unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Phys.MSSQLDef, FireDAC.Phys.OracleDef,
  FireDAC.Phys.DB2Def, FireDAC.VCLUI.Wait, Vcl.Buttons, Vcl.ExtCtrls,
  FireDAC.Comp.UI, FireDAC.Phys.DB2, FireDAC.Phys.Oracle, FireDAC.Phys.ODBCBase,
  FireDAC.Phys.MSSQL, FireDAC.Phys.IBBase, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.ComCtrls, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids,
  Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    DataSource1: TDataSource;
    ADPhysIBDriverLink1: TFDPhysIBDriverLink;
    ADPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    ADPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    FDPhysDB2DriverLink1: TFDPhysDB2DriverLink;
    ADGUIxWaitCursor1: TFDGUIxWaitCursor;
    Panel1: TPanel;
    btnConnIB: TSpeedButton;
    btnConnMSSQL: TSpeedButton;
    btnConnORA: TSpeedButton;
    btnConnDB2: TSpeedButton;
    StatusBar1: TStatusBar;
    FDQuery1PLAYLIST: TStringField;
    FDQuery1ARTIST: TStringField;
    FDQuery1ALBUM: TStringField;
    FDQuery1TRACKID: TIntegerField;
    FDQuery1NAME: TStringField;
    FDQuery1COMPOSER: TStringField;
    FDQuery1UNITPRICE: TBCDField;
    FDQuery1GENRE: TStringField;
    FDQuery1MEDIA: TStringField;
    FDQuery1MILLISECONDS: TIntegerField;
    FDQuery1BYTES: TIntegerField;
    FDUpdateSQL1: TFDUpdateSQL;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    btnApply: TButton;
    procedure btnConnIBClick(Sender: TObject);
    procedure btnConnMSSQLClick(Sender: TObject);
    procedure btnConnORAClick(Sender: TObject);
    procedure btnConnDB2Click(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoConnection(Sender: TObject);
    procedure SetupIB;
    procedure SetupMSSQl;
    procedure SetupORA;
    procedure SetupDB2;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnConnIBClick(Sender: TObject);
begin
  DoConnection(Sender);
end;

procedure TMainForm.btnConnMSSQLClick(Sender: TObject);
begin
  DoConnection(Sender);
end;

procedure TMainForm.btnConnORAClick(Sender: TObject);
begin
  DoConnection(Sender);
end;

procedure TMainForm.btnApplyClick(Sender: TObject);
begin
  // -1 tentará gravar todas as atualizações pendentes
  // 0 interrompe o processo ao encontrar o primeiro erro
  // 1 permitirá que (1) erro ocorra, ou (n) erros
  FDQuery1.ApplyUpdates(-1);

  // Limpa o "change log" e marca todos os registros como "not modified"
  FDQuery1.CommitUpdates;
end;

procedure TMainForm.btnConnDB2Click(Sender: TObject);
begin
  DoConnection(Sender);
end;

procedure TMainForm.DoConnection(Sender: TObject);
begin
  if FDQuery1.Active then
    FDQuery1.Close;

  if FDConnection1.Connected then
    FDConnection1.Connected := False;

  FDConnection1.Params.Clear;
  case TComponent(Sender).Tag of
    1:
      SetupIB;
    2:
      SetupMSSQl;
    3:
      SetupORA;
    4:
      SetupDB2;
  end;

  try
    FDConnection1.Connected := true;
    FDQuery1.Open;

    StatusBar1.SimpleText := '  FireDAC is connected using ' +
      FDConnection1.DriverName + ' driver.';
  except
    on E: EFDDBEngineException do
      case E.Kind of
        ekUserPwdInvalid:
          ShowMessage('Usuário ou senha inválidos!');
        ekUserPwdExpired:
          ShowMessage('Password expirado!');
        ekServerGone:
          ShowMessage('Servidor encontra-se inacessível!');
      else
        ShowMessage(E.Message);
      end;
  end;
end;

procedure TMainForm.SetupIB;
begin
  FDConnection1.DriverName := 'IB';
  FDConnection1.Params.Add('Server=LOCALHOST');
  FDConnection1.Params.Add('Database=LOCALHOST:C:\IB_XE7\FIREDAC.IB');
  FDConnection1.Params.Add('User_Name=sysdba');
  FDConnection1.Params.Add('Password=masterkey');
  FDConnection1.Params.Add('CharacterSet=win1252');
end;

procedure TMainForm.SetupMSSQl;
begin
  FDConnection1.DriverName := 'MSSQL';
  FDConnection1.Params.Add('Server=RIZZATOWIN2012');
  FDConnection1.Params.Add('Database=FIREDAC');
  FDConnection1.Params.Add('User_name=FIREDAC');
  FDConnection1.Params.Add('Password=firedac');
end;

procedure TMainForm.SetupORA;
begin
  FDConnection1.DriverName := 'Ora';
  FDConnection1.Params.Add('Database=ORCL');
  FDConnection1.Params.Add('User_Name=FIREDAC');
  FDConnection1.Params.Add('Password=firedac');
end;

procedure TMainForm.SetupDB2;
begin
  FDConnection1.DriverName := 'DB2';
  FDConnection1.Params.Add('ALIAS=SAMPLEDB');
  FDConnection1.Params.Add('User_Name=db2inst1');
  FDConnection1.Params.Add('Password=db2inst1');
end;

end.
