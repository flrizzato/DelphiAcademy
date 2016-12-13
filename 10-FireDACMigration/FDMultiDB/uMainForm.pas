unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Wait,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL, FireDAC.Phys.Oracle,
  FireDAC.Comp.UI, FireDAC.Phys.IBBase, FireDAC.Phys.IB, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.ComCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls, FireDAC.Moni.Base,
  FireDAC.Moni.RemoteClient, FireDAC.Phys.IBDef, FireDAC.Phys.OracleDef,
  FireDAC.Phys.MSSQLDef, FireDAC.Phys.PGDef, FireDAC.Phys.DB2Def,
  FireDAC.Phys.DB2;

type
  TMainForm = class(TForm)
    FDCnn: TFDConnection;
    FDQry: TFDQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ADPhysIBDriverLink1: TFDPhysIBDriverLink;
    ADGUIxWaitCursor1: TFDGUIxWaitCursor;
    Panel1: TPanel;
    ADPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    ADPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    StatusBar1: TStatusBar;
    Button1: TButton;
    SpeedButton4: TSpeedButton;
    FDPhysDB2DriverLink1: TFDPhysDB2DriverLink;
    FDQryCUSTOMERID: TIntegerField;
    FDQryFIRSTNAME: TStringField;
    FDQryLASTNAME: TStringField;
    FDQryCOMPANY: TStringField;
    FDQryADDRESS: TStringField;
    FDQryCITY: TStringField;
    FDQrySTATE: TStringField;
    FDQryCOUNTRY: TStringField;
    FDQryPOSTALCODE: TStringField;
    FDQryPHONE: TStringField;
    FDQryFAX: TStringField;
    FDQryEMAIL: TStringField;
    FDQrySUPPORTREPID: TIntegerField;
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FDQryUpdateError(ASender: TDataSet; AException: EFDException;
      ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
      var AAction: TFDErrorAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetupIB;
    procedure SetupMSSQl;
    procedure SetupORA;
    procedure SetupDB2;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FDQryUpdateError(ASender: TDataSet;
  AException: EFDException; ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
  var AAction: TFDErrorAction);
begin
  ShowMessage(AException.Message);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FDCnn.ApplyUpdates([FDQry]);
end;

procedure TMainForm.SetupIB;
begin
  FDCnn.DriverName := 'IB';
  FDCnn.Params.Add('Server=LOCALHOST');
  FDCnn.Params.Add('Database=LOCALHOST:C:\IB_XE7\FIREDAC.IB');
  FDCnn.Params.Add('User_Name=sysdba');
  FDCnn.Params.Add('Password=masterkey');
  FDCnn.Params.Add('CharacterSet=win1252');
end;

procedure TMainForm.SetupMSSQl;
begin
  FDCnn.DriverName := 'MSSQL';
  FDCnn.Params.Add('Server=RIZZATOWIN2012');
  FDCnn.Params.Add('Database=FIREDAC');
  FDCnn.Params.Add('User_name=FIREDAC');
  FDCnn.Params.Add('Password=firedac');
end;

procedure TMainForm.SetupORA;
begin
  FDCnn.DriverName := 'Ora';
  FDCnn.Params.Add('Database=ORCL');
  FDCnn.Params.Add('User_Name=FIREDAC');
  FDCnn.Params.Add('Password=firedac');
end;

procedure TMainForm.SetupDB2;
begin
  FDCnn.DriverName := 'DB2';
  FDCnn.Params.Add('ALIAS=SAMPLEDB');
  FDCnn.Params.Add('User_Name=db2inst1');
  FDCnn.Params.Add('Password=db2inst1');
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  if FDQry.Active then
    FDQry.Close;

  if FDCnn.Connected then
    FDCnn.Connected := False;

  FDCnn.Params.Clear;
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
    FDCnn.Connected := True;
    FDQry.Open;

    StatusBar1.SimpleText := '  FireDAC is connected using ' + FDCnn.DriverName
      + ' driver.';
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

end.
