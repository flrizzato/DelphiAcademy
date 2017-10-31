unit uClientDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBLiteDef, FireDAC.Phys.IBDef, FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI,
  FireDAC.Phys.IBBase, IPPeerClient, REST.Backend.ServiceTypes,
  REST.Backend.MetaTypes, System.JSON, REST.Backend.ParseServices,
  Data.Bind.Components, Data.Bind.ObjectScope, REST.Backend.BindSource,
  REST.Backend.ServiceComponents, REST.Backend.ParseProvider,
  FMX.StdCtrls, FMX.Ani, FMX.Layouts, REST.Response.Adapter;

type
  TClientDM = class(TDataModule)
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
    qryPARTSOBJECTID: TStringField;
    ParseProvider1: TParseProvider;
    BackendQuery1: TBackendQuery;
    memDataSet: TFDMemTable;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    procedure DataModuleCreate(Sender: TObject);
    procedure FDCnnBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadParts(fLastUpdate: TDate; const LayoutStatus: TLayout;
      const LabelStatus: TLabel; const FloatAnimationStatus: TFloatAnimation);
  end;

var
  ClientDM: TClientDM;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}

uses uSQLUtil, FMX.Forms, FMX.DialogService, System.IOUtils, FMX.Types;

procedure TClientDM.DataModuleCreate(Sender: TObject);
begin
  ParseProvider1.BaseURL := 'https://parseapi.back4app.com/';
end;

procedure TClientDM.FDCnnBeforeConnect(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  // use the design time connection definition
{$ELSE}
  FDCnn.Params.Database := TPath.GetDocumentsPath + PathDelim +
    'MASTSQL_MOBILE.GDB';
{$ENDIF}
end;

procedure TClientDM.LoadParts(fLastUpdate: TDate; const LayoutStatus: TLayout;
  const LabelStatus: TLabel; const FloatAnimationStatus: TFloatAnimation);
var
  sIns, sUpd: string;
  i, j, k: integer;
  sQuery: string;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        TThread.Synchronize(nil,
          procedure
          begin
            Log.d('Starting the process...');
            LayoutStatus.Visible := True;
            FloatAnimationStatus.Enabled := True;
            LabelStatus.Text := 'Loading data...';
            Application.ProcessMessages;
          end);

        sQuery := 'where={"updatedAt":{"$gte":{ "__type": "Date", "iso": "' +
          FormatDateTime('yyyy-mm-dd', fLastUpdate) + 'T00:00:00.000Z" }}}';

        Log.d('Loading pending updates...');
        BackendQuery1.QueryLines.Text := sQuery;
        BackendQuery1.Execute;

        TThread.Synchronize(nil,
          procedure
          begin
            LabelStatus.Text := 'Starting sync process...';
            Application.ProcessMessages;
          end);

        FDCnn.StartTransaction;

        i := 0;
        j := memDataSet.RecordCount;

        Log.d('Starting sync process...');
        while not memDataSet.Eof do
        begin
          sUpd := 'UPDATE PARTS SET ';
          sUpd := sUpd + ' VENDORNO = ' + memDataSet.FieldByName('VENDORNO')
            .AsString + ',';
          sUpd := sUpd + ' DESCRIPTION = ' + TSQLUtil.SQLString
            (memDataSet.FieldByName('DESCRIPTION').AsString) + ',';
          sUpd := sUpd + ' ONHAND = ' + memDataSet.FieldByName('ONHAND')
            .AsString + ',';
          sUpd := sUpd + ' ONORDER = ' + memDataSet.FieldByName('ONORDER')
            .AsString + ',';
          sUpd := sUpd + ' COST = ' + memDataSet.FieldByName('COST')
            .AsString + ',';
          sUpd := sUpd + ' LISTPRICE = ' + memDataSet.FieldByName('LISTPRICE')
            .AsString + ',';
          sUpd := sUpd + ' OBJECTID = ' + TSQLUtil.SQLString
            (memDataSet.FieldByName('OBJECTID').AsString);
          sUpd := sUpd + ' WHERE PARTNO = ' + memDataSet.FieldByName
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
          sIns := sIns + ' OBJECTID';
          sIns := sIns + ' ) VALUES (';
          sIns := sIns + memDataSet.FieldByName('PARTNO').AsString + ',';
          sIns := sIns + memDataSet.FieldByName('VENDORNO').AsString + ',';
          sIns := sIns + TSQLUtil.SQLString(memDataSet.FieldByName
            ('DESCRIPTION').AsString) + ',';
          sIns := sIns + memDataSet.FieldByName('ONHAND').AsString + ',';
          sIns := sIns + memDataSet.FieldByName('ONORDER').AsString + ',';
          sIns := sIns + memDataSet.FieldByName('COST').AsString + ',';
          sIns := sIns + memDataSet.FieldByName('LISTPRICE').AsString + ',';
          sIns := sIns + TSQLUtil.SQLString(memDataSet.FieldByName('OBJECTID')
            .AsString) + ')';

          Log.d('Trying to Update Parts : ' + sUpd);
          k := FDCnn.ExecSQL(sUpd);
          if k = 0 then
          begin
            Log.d('Insert Into Parts: ' + sIns);
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
                Log.d(LabelStatus.Text);

                Application.ProcessMessages;
              end);
          end;

          memDataSet.Next;
          Sleep(100); // demo
        end;

        FDCnn.Commit;

        TThread.Synchronize(nil,
          procedure
          begin
            FloatAnimationStatus.Enabled := True;
            LayoutStatus.Visible := False;
            Log.d('Synchronized records: ' + i.ToString);
            Application.ProcessMessages;

            TDialogService.ShowMessage('Synchronized records: ' + i.ToString);
          end);
      except
        on E: Exception do
        begin
          FDCnn.Rollback;

          FloatAnimationStatus.Enabled := True;
          LayoutStatus.Visible := False;
          Application.ProcessMessages;

          Log.d('LoadParts error: ' + E.Message);
          raise Exception.Create('LoadParts error: ' + E.Message);
        end;
      end;
    end).Start;
end;

end.
