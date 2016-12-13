//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit formMergeMainController;

interface

uses
  System.SysUtils, System.Classes, System.Actions, Vcl.ActnList, formMergeMain,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type
  TMergeMainController = class(TDataModule)
    ActionList: TActionList;
    actMerge: TAction;
    actGetIBFileName: TAction;
    procedure actMergeExecute(Sender: TObject);
    procedure actGetIBFileNameExecute(Sender: TObject);
  private
    { Private declarations }
    FView : TfrmMergeMain;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent; AView : TfrmMergeMain); reintroduce;
  end;

var
  MergeMainController: TMergeMainController;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses dmDatabase, VCL.ComCtrls, System.Variants;

{$R *.dfm}

procedure TMergeMainController.actGetIBFileNameExecute(Sender: TObject);
begin
  if FView.FileOpenDialog1.Execute then
    FView.edtIBDatabase.Text := FView.FileOpenDialog1.FileName
end;

procedure TMergeMainController.actMergeExecute(Sender: TObject);
var
  dmIB, dmMSSQL : TdtmdDatabase;

  procedure SetUpProgress(Qry : TFDQuery; ProgressBar : TProgressBar);
  begin
    ProgressBar.Position := 0;
    ProgressBar.Max := Qry.RecordCount
  end;

  procedure RemoveDeletedRecords(IBData, MasterData : TFDQuery; ValueFields: TArray<string>; ProgressBar : TProgressBar);
  var
    LocateData : System.Variant;
    KeyFields : string;
    S: string;
  begin
    SetUpProgress(IBData, ProgressBar);

    IBData.First;

    KeyFields := '';
    for S in ValueFields do begin
      if KeyFields > '' then
        KeyFields := KeyFields+';'+S
      else
        KeyFields := S;
    end;

    while not IBData.Eof do begin
      case Length(ValueFields) of
        1 : LocateData := VarArrayOf([IBData.FieldByName(ValueFields[0]).Value]);
        2 : LocateData := VarArrayOf([IBData.FieldByName(ValueFields[0]).Value,IBData.FieldByName(ValueFields[1]).Value]);
      end;

      if not MasterData.Locate(KeyFields,LocateData,[]) then
        MasterData.Delete;
      ProgressBar.Position := ProgressBar.Position +1;
      IBData.Next;
    end;
  end;


  procedure ProcessUpdatesToIB(IBData, MasterData : TFDQuery; ValueFields: TArray<string>; ProgressBar : TProgressBar);
  var
    LocateData : System.Variant;
    KeyFields : string;
    S: string;
    I: Integer;
    IBField, Field: TField;
  begin
    SetUpProgress(MasterData, ProgressBar);

    MasterData.First;

    KeyFields := '';
    for S in ValueFields do begin
      if KeyFields > '' then
        KeyFields := KeyFields+';'+S
      else
        KeyFields := S;
    end;

    while not MasterData.Eof do begin
      case Length(ValueFields) of
        1 : LocateData := VarArrayOf([MasterData.FieldByName(ValueFields[0]).Value]);
        2 : LocateData := VarArrayOf([MasterData.FieldByName(ValueFields[0]).Value,MasterData.FieldByName(ValueFields[1]).Value]);
      end;

      if not IBData.Locate(KeyFields,LocateData,[]) then begin
        IBData.Insert;
        for I := 0 to Pred(MasterData.FieldCount) do begin
          Field := MasterData.Fields[I];
          IBField := IBData.Fields.FindField(Field.FieldName);
          if IBField <> nil then
            IBField.Value := Field.Value;
        end;
        IBData.Post;
      end else begin
        for I := 0 to Pred(MasterData.FieldCount) do begin
          Field := MasterData.Fields[I];
          IBField := IBData.Fields.FindField(Field.FieldName);
          if IBField <> nil then begin
            if IBField.Value <> Field.Value then begin
              if IBData.State <> TDataSetState.dsEdit then
                IBData.Edit;
              IBField.Value := Field.Value;
            end;
          end;
        end;
        if IBData.State = TDataSetState.dsEdit then
          IBData.Post;
      end;

      ProgressBar.Position := ProgressBar.Position +1;
      MasterData.Next;
    end;
  end;


begin
  // Do the merger
  dmMSSQL := TdtmdDatabase.Create(Self);
  dmIB := TdtmdDatabase.Create(Self);
  try
    dmIB.FDConnection.DriverName := 'IB';
    dmIB.FDConnection.Params.Database := FView.edtIBDatabase.Text;

    dmMSSQL.FDConnection.DriverName := 'MSSQL';
    dmMSSQL.FDConnection.LoginDialog := dmMSSQL.FDGUIxLoginDialog1;
    dmMSSQL.FDConnection.Params.Database := FView.edtMSSQLDatabase.Text;
    dmMSSQL.FDConnection.Params.Values['SERVER'] := FView.edtMSSQLServer.Text;
    dmMSSQL.FDConnection.Transaction.Options.Isolation := TFDTxIsolation.xiReadCommitted;

    dmIB.FDConnection.Open;
    dmMSSQL.FDConnection.Open;

    RemoveDeletedRecords(dmIB.qryCategory, dmMSSQL.qryCategory, ['CATEGORY_ID'], FView.ProgressBar1);
    RemoveDeletedRecords(dmIB.qryMedicine, dmMSSQL.qryMedicine, ['MEDICINE_ID'], FView.ProgressBar2);
    RemoveDeletedRecords(dmIB.qryMedicineCategories, dmMSSQL.qryMedicineCategories, ['MEDICINE_ID','CATEGORY_ID'], FView.ProgressBar3);

    ProcessUpdatesToIB(dmIB.qryCategory, dmMSSQL.qryCategory, ['CATEGORY_ID'], FView.ProgressBar4);
    ProcessUpdatesToIB(dmIB.qryMedicine, dmMSSQL.qryMedicine, ['MEDICINE_ID'], FView.ProgressBar5);
    ProcessUpdatesToIB(dmIB.qryMedicineCategories, dmMSSQL.qryMedicineCategories, ['MEDICINE_ID','CATEGORY_ID'], FView.ProgressBar6);
  finally
    FreeAndNil(dmIB);
    FreeAndNil(dmMSSQL);
  end;
end;

constructor TMergeMainController.Create(AOwner: TComponent;
  AView: TfrmMergeMain);
begin
  inherited Create(AOwner);
  FView := AView;
  FView.btnMerge.Action := actMerge;
  FView.sbInterBase.Action := actGetIBFileName;
end;

end.
