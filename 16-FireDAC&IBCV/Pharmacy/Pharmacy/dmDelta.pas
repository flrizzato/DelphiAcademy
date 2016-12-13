//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit dmDelta;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TdtmdlDelta = class(TDataModule)
    FDTransactionDelta: TFDTransaction;
    qrySubscriptionActive: TFDQuery;
    qryCategory: TFDQuery;
    qryMedicine: TFDQuery;
    qryMedicineCategories: TFDQuery;
    qryCategoryDelta: TFDQuery;
    qryMedicineDelta: TFDQuery;
    qryMedicineCategoriesDelta: TFDQuery;
    mtCategoryMerged: TFDMemTable;
    mtMedicineMerged: TFDMemTable;
    mtMedicineCategoriesMerged: TFDMemTable;
  private
    FOnPostDeltas: TNotifyEvent;
    { Private declarations }
    procedure OpenAll(ID : string; ShowMerged : Boolean);
  public
    { Public declarations }
    constructor Create(AOwner : TComponent; ID : string; ShowMerged : Boolean; LocalDB, RemoteDB : TFDConnection); reintroduce;
    procedure PostDeltas;
    property OnPostDeltas : TNotifyEvent read FOnPostDeltas write FOnPostDeltas;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TdtmdlDelta }

constructor TdtmdlDelta.Create(AOwner: TComponent; ID : string; ShowMerged : Boolean;
  LocalDB, RemoteDB: TFDConnection);

  procedure SetupQry(LocalQuery, DeltaQuery : TFDQuery; SQL : string);
  begin
    LocalQuery.Connection := LocalDB;
    DeltaQuery.Connection := RemoteDB;
    LocalQuery.SQL.Text := SQL;
    DeltaQuery.SQL.Text := SQL;
  end;
begin
  inherited Create(AOwner);
  FDTransactionDelta.Connection := RemoteDB;
  qrySubscriptionActive.Connection := RemoteDB;

  SetupQry(qryCategory, qryCategoryDelta, 'SELECT * FROM CATEGORY');
  SetupQry(qryMedicine, qryMedicineDelta, 'SELECT * FROM MEDICINE');
  SetupQry(qryMedicineCategories, qryMedicineCategoriesDelta, 'SELECT * FROM MEDICINE_CATEGORIES');

  OpenAll(ID, ShowMerged);
end;

procedure TdtmdlDelta.OpenAll(ID : string; ShowMerged : Boolean);
begin
  // Local connection in Cached Update Modes
  qryCategory.CachedUpdates := True;
  qryMedicine.CachedUpdates := True;
  qryMedicineCategories.CachedUpdates := True;

  qryCategory.Open();
  qryMedicine.Open();
  qryMedicineCategories.Open();

  if FDTransactionDelta.Active then
    FDTransactionDelta.Rollback;

  FDTransactionDelta.StartTransaction;

  // Start the transaction to fetch the deltas. - this changes from Select ALL to Select subscribed changes only.
  qrySubscriptionActive.SQL.Text := Format('set subscription sub_medicineupdates at ''%s'' active;',[ID]);
  qrySubscriptionActive.ExecSQL;

  qryCategoryDelta.Open();
  qryMedicineDelta.Open();
  qryMedicineCategoriesDelta.Open();

  // This is for illustation only - Build a picture of local and Merged delta
  if ShowMerged then begin
    mtCategoryMerged.MergeDataSet(qryCategory, TFDMergeDataMode.dmDataSet, mmUpdate);
    mtMedicineMerged.MergeDataSet(qryMedicine, TFDMergeDataMode.dmDataSet, mmUpdate);
    mtMedicineCategoriesMerged.MergeDataSet(qryMedicineCategories, TFDMergeDataMode.dmDataSet, mmUpdate);
  end;
  mtCategoryMerged.MergeDataSet(qryCategoryDelta, TFDMergeDataMode.dmDeltaMerge, mmUpdate);
  mtMedicineMerged.MergeDataSet(qryMedicineDelta, TFDMergeDataMode.dmDeltaMerge, mmUpdate);
  mtMedicineCategoriesMerged.MergeDataSet(qryMedicineCategoriesDelta, TFDMergeDataMode.dmDeltaMerge, mmUpdate);
end;

procedure TdtmdlDelta.PostDeltas;
begin
  qryCategory.MergeDataSet(qryCategoryDelta, TFDMergeDataMode.dmDeltaMerge, mmUpdate);
  qryMedicine.MergeDataSet(qryMedicineDelta, TFDMergeDataMode.dmDeltaMerge, mmUpdate);
  qryMedicineCategories.MergeDataSet(qryMedicineCategoriesDelta, TFDMergeDataMode.dmDeltaMerge, mmUpdate);

  qryCategory.ApplyUpdates;
  qryMedicine.ApplyUpdates;
  qryMedicineCategories.ApplyUpdates;

  FDTransactionDelta.Commit;

  if Assigned(OnPostDeltas) then
    OnPostDeltas(Self)
end;

end.
