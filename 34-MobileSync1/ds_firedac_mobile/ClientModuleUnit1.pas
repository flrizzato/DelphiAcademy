unit ClientModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, ClientClassesUnit1, Datasnap.DSClientRest,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.IB, FireDAC.Phys.IBLiteDef, FireDAC.Phys.IBDef,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Comp.UI, FireDAC.Phys.IBBase, FMX.StdCtrls, FMX.Ani, FMX.Layouts,
  FireDAC.Stan.StorageBin, FireDAC.Stan.StorageJSON;

type
  TClientModule1 = class(TDataModule)
    DSRestConnection1: TDSRestConnection;
    FDCnn: TFDConnection;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    qryPARTS: TFDQuery;
    qryPARTSPARTNO: TFloatField;
    qryPARTSVENDORNO: TFloatField;
    qryPARTSDESCRIPTION: TStringField;
    qryPARTSONHAND: TFloatField;
    qryPARTSONORDER: TFloatField;
    qryPARTSCOST: TFloatField;
    qryPARTSLISTPRICE: TFloatField;
    qryPARTSLASTUPDATE: TSQLTimeStampField;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    FInstanceOwner: Boolean;
    FServerMethods1Client: TServerMethods1Client;
    function GetServerMethods1Client: TServerMethods1Client;
    { Private declarations }
  public
    procedure LoadParts(fLastUpdate: string; const LayoutStatus: TLayout;
      const LabelStatus: TLabel; const FloatAnimationStatus: TFloatAnimation);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property InstanceOwner: Boolean read FInstanceOwner write FInstanceOwner;
    property ServerMethods1Client: TServerMethods1Client
      read GetServerMethods1Client write FServerMethods1Client;

  end;

var
  ClientModule1: TClientModule1;

implementation

uses
  Data.FireDACJSONReflect, FMX.Forms, uSQLUtil, FMX.DialogService;

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

constructor TClientModule1.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := True;
end;

procedure TClientModule1.DataModuleCreate(Sender: TObject);
begin
  FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
  FormatSettings.LongDateFormat := 'dd/mm/yyyy hh:nn:ss';
end;

destructor TClientModule1.Destroy;
begin
  FServerMethods1Client.Free;
  inherited;
end;

procedure TClientModule1.LoadParts(fLastUpdate: string;
  const LayoutStatus: TLayout; const LabelStatus: TLabel;
  const FloatAnimationStatus: TFloatAnimation);
var
  fDSList: TFDJSONDataSets;
  fDataSet: TFDDataSet;
  sIns, sUpd: string;
  i, j, k: integer;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        TThread.Synchronize(nil,
          procedure
          begin
            LayoutStatus.Visible := True;
            FloatAnimationStatus.Enabled := True;
            LabelStatus.Text := 'Loading data...';
            Application.ProcessMessages;
          end);

        fDSList := ClientModule1.GetServerMethods1Client.GetParts(fLastUpdate);
        fDataSet := TFDJSONDataSetsReader.GetListValue(fDSList, 0);

        TThread.Synchronize(nil,
          procedure
          begin
            LabelStatus.Text := 'Starting sync process...';
            Application.ProcessMessages;
          end);

        FDCnn.StartTransaction;

        i := 0;
        j := fDataSet.RecordCount;

        while not fDataSet.Eof do
        begin
          sUpd := 'UPDATE PARTS SET ';
          sUpd := sUpd + ' VENDORNO = ' + fDataSet.FieldByName('VENDORNO')
            .AsString + ',';
          sUpd := sUpd + ' DESCRIPTION = ' + TSQLUtil.SQLString
            (fDataSet.FieldByName('DESCRIPTION').AsString) + ',';
          sUpd := sUpd + ' ONHAND = ' + fDataSet.FieldByName('ONHAND')
            .AsString + ',';
          sUpd := sUpd + ' ONORDER = ' + fDataSet.FieldByName('ONORDER')
            .AsString + ',';
          sUpd := sUpd + ' COST = ' + fDataSet.FieldByName('COST')
            .AsString + ',';
          sUpd := sUpd + ' LISTPRICE = ' + fDataSet.FieldByName('LISTPRICE')
            .AsString + ',';
          sUpd := sUpd + ' LASTUPDATE = ' + TSQLUtil.SQLDateTime
            (fDataSet.FieldByName('LASTUPDATE').AsString);
          sUpd := sUpd + ' WHERE PARTNO = ' + fDataSet.FieldByName
            ('PARTNO').AsString;

          sIns := 'INSERT INTO PARTS';
          sIns := sIns + ' (';
          sIns := sIns + ' PARTNO,';
          sIns := sIns + ' VENDORNO,';
          sIns := sIns + ' DESCRIPTION,';
          sIns := sIns + ' ONHAND,';
          sIns := sIns + ' ONORDER,';
          sIns := sIns + ' COST,';
          sIns := sIns + ' LISTPRICE,';
          sIns := sIns + ' LASTUPDATE';
          sIns := sIns + ' ) VALUES (';
          sIns := sIns + fDataSet.FieldByName('PARTNO').AsString + ',';
          sIns := sIns + fDataSet.FieldByName('VENDORNO').AsString + ',';
          sIns := sIns + TSQLUtil.SQLString(fDataSet.FieldByName('DESCRIPTION')
            .AsString) + ',';
          sIns := sIns + fDataSet.FieldByName('ONHAND').AsString + ',';
          sIns := sIns + fDataSet.FieldByName('ONORDER').AsString + ',';
          sIns := sIns + fDataSet.FieldByName('COST').AsString + ',';
          sIns := sIns + fDataSet.FieldByName('LISTPRICE').AsString + ',';
          sIns := sIns + TSQLUtil.SQLDateTime(fDataSet.FieldByName('LASTUPDATE')
            .AsString) + ')';

          k := FDCnn.ExecSQL(sUpd);
          if k = 0 then
          begin
            k := FDCnn.ExecSQL(sIns);
          end;

          if (k = 1) then
          begin
            Inc(i);
            TThread.Synchronize(nil,
              procedure
              begin
                LabelStatus.Text := 'Synchronizing ' + i.ToString + ' from ' +
                  j.ToString + '...';
                Application.ProcessMessages;
              end);
          end;

          fDataSet.Next;
          Sleep(100); //demo
        end;

        FDCnn.Commit;

        TThread.Synchronize(nil,
          procedure
          begin
            FloatAnimationStatus.Enabled := True;
            LayoutStatus.Visible := False;
            Application.ProcessMessages;
            TDialogService.ShowMessage('Synchronized records: ' + i.ToString);
          end);
      except
        on E: Exception do
        begin
          FDCnn.Rollback;
          // add a better log solution for the client side
          raise Exception.Create('LoadParts: ' + E.Message);
        end;
      end;
    end).Start;
end;

function TClientModule1.GetServerMethods1Client: TServerMethods1Client;
begin
  if FServerMethods1Client = nil then
    FServerMethods1Client := TServerMethods1Client.Create(DSRestConnection1,
      FInstanceOwner);
  Result := FServerMethods1Client;
end;

end.
