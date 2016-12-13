unit uMainDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  FireDAC.Phys.OracleDef, FireDAC.Phys.DB2Def, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.IBDef, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.IB, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Phys.IBBase,
  FireDAC.Phys.MSSQL, FireDAC.Phys.ODBCBase, FireDAC.Phys.DB2,
  FireDAC.Phys.Oracle, FireDAC.Comp.UI, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteVDataSet;

type
  TMainDM = class(TDataModule)
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    FDPhysDB2DriverLink1: TFDPhysDB2DriverLink;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDQuery1: TFDQuery;
    FDQuery2: TFDQuery;
    FDQuery3: TFDQuery;
    FDQuery4: TFDQuery;
    FDIBServer: TFDConnection;
    FDMSSQL: TFDConnection;
    FDDB2: TFDConnection;
    FDSQLLite: TFDConnection;
    FDLocalSQL1: TFDLocalSQL;
    FDQuery5: TFDQuery;
    FDORA: TFDConnection;
  private
    { Private declarations }
    procedure SetupIB;
    procedure SetupMSSQl;
    procedure SetupORA;
    procedure SetupDB2;
  public
    { Public declarations }
    procedure ConnectAndOpen;
  end;

var
  MainDM: TMainDM;

implementation

uses
  Vcl.Controls, Vcl.Dialogs, Vcl.Forms;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure TMainDM.ConnectAndOpen;
begin
  try
    Screen.Cursor := crHourGlass;
    try
      SetupIB;
      SetupORA;
      SetupMSSQl;
      SetupDB2;

      FDSQLLite.Open;
      FDLocalSQL1.Active := True;

      FDQuery5.Active := True;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainDM.SetupDB2;
begin
  FDDB2.Params.Clear;
  FDDB2.DriverName := 'DB2';
  FDDB2.Params.Add('Server=172.16.121.242');
  FDDB2.Params.Add('Database=SAMPLE');
  FDDB2.Params.Add('Port=50000');
  FDDB2.Params.Add('Protocol=TCPIP');
  FDDB2.Params.Add('User_Name=db2inst1');
  FDDB2.Params.Add('Password=db2inst1');
  FDDB2.Open;
end;

procedure TMainDM.SetupIB;
begin
  FDIBServer.Params.Clear;
  FDIBServer.DriverName := 'IB';
  FDIBServer.Params.Add('Server=LOCALHOST');
  FDIBServer.Params.Add('Database=LOCALHOST:C:\IB_XE7\FIREDAC.IB');
  FDIBServer.Params.Add('User_Name=sysdba');
  FDIBServer.Params.Add('Password=masterkey');
  FDIBServer.Params.Add('CharacterSet=win1252');
  FDIBServer.Open;
end;

procedure TMainDM.SetupMSSQl;
begin
  FDMSSQL.Params.Clear;
  FDMSSQL.DriverName := 'MSSQL';
  FDMSSQL.Params.Add('Server=RIZZATOWIN2012');
  FDMSSQL.Params.Add('Database=FIREDAC');
  FDMSSQL.Params.Add('User_name=FIREDAC');
  FDMSSQL.Params.Add('Password=firedac');
  FDMSSQL.Open;
end;

procedure TMainDM.SetupORA;
begin
  FDORA.Params.Clear;
  FDORA.DriverName := 'ORA';
  FDORA.Params.Add('Database=ORCL');
  FDORA.Params.Add('User_Name=FIREDAC');
  FDORA.Params.Add('Password=firedac');
  FDORA.Open;
end;

end.
