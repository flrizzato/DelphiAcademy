//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit dmDatabase;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, Data.DB, FireDAC.Comp.Client, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Phys.IBBase,
  FireDAC.VCLUI.Login;

type
  TdtmdDatabase = class(TDataModule)
    FDConnection: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    qryCategory: TFDQuery;
    qryMedicine: TFDQuery;
    qryMedicineCategories: TFDQuery;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxLoginDialog1: TFDGUIxLoginDialog;
    FDStoredProc1: TFDStoredProc;
    dsCategory: TDataSource;
    FDStoredProc1NEWID: TIntegerField;
    qryMedicineCategoriesCATEGORY_ID: TIntegerField;
    qryMedicineCategoriesMEDICINE_ID: TIntegerField;
    qryMedicineCategoriesMEDICINE_NAME: TStringField;
    procedure FDConnectionAfterConnect(Sender: TObject);
    procedure qryCategoryNewRecord(DataSet: TDataSet);
    procedure qryMedicineNewRecord(DataSet: TDataSet);
    procedure qryMedicineCategoriesNewRecord(DataSet: TDataSet);
  private
    { Private declarations }
    function GetID : Integer;
  public
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdtmdDatabase.FDConnectionAfterConnect(Sender: TObject);
begin
  qryCategory.Open();
  qryMedicine.Open();
  qryMedicineCategories.Open();
end;

function TdtmdDatabase.GetID: Integer;
begin
  FDStoredProc1.ExecProc;
  Result := FDStoredProc1.Params[0].AsInteger;
end;

procedure TdtmdDatabase.qryCategoryNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('CATEGORY_ID').AsInteger := GetID;
end;

procedure TdtmdDatabase.qryMedicineCategoriesNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('MEDICINE_ID').AsInteger := qryMedicine.FieldByName('MEDICINE_ID').AsInteger;
  DataSet.Post;
end;

procedure TdtmdDatabase.qryMedicineNewRecord(DataSet: TDataSet);
begin
  DataSet.FieldByName('MEDICINE_ID').AsInteger := GetID;
end;

end.
