unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, System.Json,
  Datasnap.DSServer, Datasnap.DSAuth, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.VCLUI.Wait, FireDAC.Stan.StorageBin,
  FireDAC.Stan.StorageJSON, FireDAC.Comp.UI, FireDAC.Phys.IBBase, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Data.FireDACJSONReflect;

type
{$METHODINFO ON}
  TServerMethods1 = class(TDataModule)
    FDCnn: TFDConnection;
    qryParts: TFDQuery;
    qryPartsPARTNO: TFloatField;
    qryPartsVENDORNO: TFloatField;
    qryPartsDESCRIPTION: TStringField;
    qryPartsONHAND: TFloatField;
    qryPartsONORDER: TFloatField;
    qryPartsCOST: TFloatField;
    qryPartsLISTPRICE: TFloatField;
    qryPartsLASTUPDATE: TSQLTimeStampField;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetParts(fLastUpdate: string): TFDJSONDataSets;
  end;
{$METHODINFO OFF}

implementation

{$R *.dfm}

uses System.StrUtils, uSQLUtil;

procedure TServerMethods1.DataModuleCreate(Sender: TObject);
begin
  FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
  FormatSettings.LongDateFormat := 'dd/mm/yyyy hh:nn:ss';
end;

function TServerMethods1.GetParts(fLastUpdate: string): TFDJSONDataSets;
var
  sSQL: string;
begin
  sSQL := 'SELECT * FROM PARTS';
  sSQL := sSQL + ' WHERE LASTUPDATE >= ' + TSQLUtil.SQLDateTime(fLastUpdate);
  sSQL := sSQL + ' ORDER BY PARTNO';

  qryParts.Close;
  qryParts.SQL.Text := sSQL;
  qryParts.Open;

  try
    Result := TFDJSONDataSets.Create;
    TFDJSONDataSetsWriter.ListAdd(Result, qryParts);
  except
    on E: Exception do
    begin
      // add a better log solution for the server side
      raise Exception.Create('GetParts: ' + E.Message);
    end;
  end;
end;

end.
