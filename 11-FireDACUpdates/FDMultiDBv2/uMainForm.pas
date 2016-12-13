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
  FireDAC.Phys.DB2, Vcl.DBCtrls;

type
  TMainForm = class(TForm)
    FDCnn: TFDConnection;
    FDQry: TFDQuery;
    dtsQry: TDataSource;
    ADPhysIBDriverLink1: TFDPhysIBDriverLink;
    ADGUIxWaitCursor1: TFDGUIxWaitCursor;
    Panel1: TPanel;
    ADPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    ADPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    btnConnIB: TSpeedButton;
    btnConnMSSQL: TSpeedButton;
    btnCnnORA: TSpeedButton;
    StatusBar1: TStatusBar;
    btnConnDB2: TSpeedButton;
    FDPhysDB2DriverLink1: TFDPhysDB2DriverLink;
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
    Panel2: TPanel;
    Panel3: TPanel;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    FDDet: TFDQuery;
    dtsDet: TDataSource;
    FDDetINVOICEID: TIntegerField;
    FDDetCUSTOMERID: TIntegerField;
    FDDetBILLINGADDRESS: TStringField;
    FDDetBILLINGCITY: TStringField;
    FDDetBILLINGSTATE: TStringField;
    FDDetBILLINGCOUNTRY: TStringField;
    FDDetBILLINGPOSTALCODE: TStringField;
    FDDetTOTAL: TBCDField;
    FDDetINVOICEDATE: TDateTimeField;
    btnViewCache: TButton;
    btnUndoLastChange: TButton;
    btnRevert: TButton;
    btnCancelUpdates: TButton;
    btnCreateSavePoint: TButton;
    btnBackToSavePoint: TButton;
    btnApplyUpdates: TButton;
    btnOldValue: TButton;
    DBGrid3: TDBGrid;
    FDCache: TFDMemTable;
    dtsCache: TDataSource;
    FDCacheStatus: TStringField;
    DBNavigator1: TDBNavigator;
    FDQryCUSTOMERID: TFDAutoIncField;
    FDCacheCUSTOMERID: TFDAutoIncField;
    FDCacheFIRSTNAME: TStringField;
    FDCacheLASTNAME: TStringField;
    FDCacheCOMPANY: TStringField;
    FDCacheADDRESS: TStringField;
    FDCacheCITY: TStringField;
    FDCacheSTATE: TStringField;
    FDCacheCOUNTRY: TStringField;
    FDCachePOSTALCODE: TStringField;
    FDCachePHONE: TStringField;
    FDCacheFAX: TStringField;
    FDCacheEMAIL: TStringField;
    FDCacheSUPPORTREPID: TIntegerField;
    procedure FDQryUpdateError(ASender: TDataSet; AException: EFDException;
      ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
      var AAction: TFDErrorAction);
    procedure FDCacheCalcFields(DataSet: TDataSet);
    procedure btnViewCacheClick(Sender: TObject);
    procedure btnOldValueClick(Sender: TObject);
    procedure btnUndoLastChangeClick(Sender: TObject);
    procedure btnRevertClick(Sender: TObject);
    procedure btnCancelUpdatesClick(Sender: TObject);
    procedure btnCreateSavePointClick(Sender: TObject);
    procedure btnBackToSavePointClick(Sender: TObject);
    procedure btnApplyUpdatesClick(Sender: TObject);
    procedure btnConnIBClick(Sender: TObject);
    procedure btnCnnORAClick(Sender: TObject);
    procedure btnConnDB2Click(Sender: TObject);
    procedure btnConnMSSQLClick(Sender: TObject);
  private
    { Private declarations }
    iSavePoint1, iSavePoint2: integer;
  public
    { Public declarations }
    procedure DoConnection(Sender: TObject);
    procedure SetupIB;
    procedure SetupMSSQl;
    procedure SetupORA;
    procedure SetupDB2;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnApplyUpdatesClick(Sender: TObject);
var
  iErr: integer;
begin
  iErr := FDQry.ApplyUpdates(-1);
  // -1 tentará gravar todas as atualizações pendentes
  // 0 interrompe o processo ao encontrar o primeiro erro
  // 1 permitirá que (1) erro ocorra, ou (n) erros

  if iErr > 0 then
  begin
    FDCache.FilterChanges := [rtHasErrors];
  end;

  // Limpa o "change log" e marca todos os registros como "not modified"
  FDQry.CommitUpdates;
end;

procedure TMainForm.btnBackToSavePointClick(Sender: TObject);
begin
  if iSavePoint2 > 0 then
  begin
    FDQry.SavePoint := iSavePoint2;
    iSavePoint2 := 0;
  end
  else if iSavePoint1 > 0 then
  begin
    FDQry.SavePoint := iSavePoint1;
    iSavePoint1 := 0;
  end;
end;

procedure TMainForm.btnCancelUpdatesClick(Sender: TObject);
begin
  FDQry.CancelUpdates;
end;

procedure TMainForm.btnCreateSavePointClick(Sender: TObject);
begin
  if iSavePoint1 = 0 then
    iSavePoint1 := FDQry.SavePoint
  else
    iSavePoint2 := FDQry.SavePoint;
end;

procedure TMainForm.btnOldValueClick(Sender: TObject);
begin
  ShowMessage(Format('Old Value: %s e New Value: %s',
    [FDQry.FieldByName('FirstName').OldValue,
    FDQry.FieldByName('FirstName').Value]));
end;

procedure TMainForm.btnRevertClick(Sender: TObject);
begin
  FDQry.RevertRecord;
end;

procedure TMainForm.btnUndoLastChangeClick(Sender: TObject);
begin
  FDQry.UndoLastChange(true);
end;

procedure TMainForm.btnViewCacheClick(Sender: TObject);
begin
  if FDQry.Active then
  begin
    FDCache.CloneCursor(FDQry, true);
    FDCache.FilterChanges := [rtModified, rtInserted, rtDeleted];
  end;
end;

procedure TMainForm.btnConnIBClick(Sender: TObject);
begin
  DoConnection(Sender);
end;

procedure TMainForm.btnConnMSSQLClick(Sender: TObject);
begin
  DoConnection(Sender);
end;

procedure TMainForm.btnCnnORAClick(Sender: TObject);
begin
  DoConnection(Sender);
end;

procedure TMainForm.btnConnDB2Click(Sender: TObject);
begin
  DoConnection(Sender);
end;

procedure TMainForm.DoConnection(Sender: TObject);
begin
  if FDCache.Active then
    FDCache.Close;

  if FDDet.Active then
    FDDet.Close;

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
    FDCnn.Connected := true;
    FDQry.Open;
    FDDet.Open;

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

procedure TMainForm.FDCacheCalcFields(DataSet: TDataSet);
begin
  case DataSet.UpdateStatus of
    usUnmodified:
      DataSet.FieldByName('Status').AsString := 'Unmodified';
    usModified:
      DataSet.FieldByName('Status').AsString := 'Modified';
    usInserted:
      DataSet.FieldByName('Status').AsString := 'Inserted';
    usDeleted:
      DataSet.FieldByName('Status').AsString := 'Deleted';
  end;
end;

procedure TMainForm.FDQryUpdateError(ASender: TDataSet;
  AException: EFDException; ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
  var AAction: TFDErrorAction);
begin
  ShowMessage(AException.Message);
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

end.
