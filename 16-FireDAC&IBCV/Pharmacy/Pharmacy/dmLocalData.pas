//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit dmLocalData;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.IB,
  FireDAC.Phys.IBDef, Data.DB, FireDAC.Comp.Client, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Phys.IBBase;

type
  TdtmdLocalDB = class(TDataModule)
    FDConnection: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    qryCategory: TFDQuery;
    qryMedicines: TFDQuery;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    dsPharmacy: TDataSource;
    qryPharmacy: TFDQuery;
    dsCategory: TDataSource;
    qryCategoryCATEGORY_ID: TIntegerField;
    qryCategoryCATEGORY_NAME: TWideStringField;
    qryCategoryDESCRIPTION: TMemoField;
    qryMedicinesMEDICINE_ID: TIntegerField;
    qryMedicinesMEDICINE_NAME: TWideStringField;
    qryMedicinesURL: TWideStringField;
    qryMedicinesPATIENT_ADVICE: TMemoField;
    qryMedicinesSPECIAL_WARNINGS: TMemoField;
    qryPharmacyPHARMACY_ID: TIntegerField;
    qryPharmacyPHARMACY_ADDRESS: TMemoField;
    qryPharmacyPHARMACY_NAME: TWideStringField;
    qryCustomer: TFDQuery;
    qryOrders: TFDQuery;
    dsCustomer: TDataSource;
    qryCustomerCUSTOMER_ID: TIntegerField;
    qryCustomerPHARMACY_ID: TIntegerField;
    qryCustomerCUSTOMER_NAME: TWideStringField;
    qryCustomerCUSTOMER_ADDRESS: TMemoField;
    qryMedLookup: TFDQuery;
    qryOrdersCUSTOMER_ID: TIntegerField;
    qryOrdersPHARMACY_ID: TIntegerField;
    qryOrdersMEDICINE_ID: TIntegerField;
    qryOrdersDATE_TIME: TSQLTimeStampField;
    qryOrdersQUANTITY: TIntegerField;
    qryOrdersMEDICINE_NAME: TStringField;
    spGetID: TFDStoredProc;
    spGetIDNEWID: TIntegerField;
    dsOrders: TDataSource;
    FDConnectionRemote: TFDConnection;
    procedure FDConnectionAfterConnect(Sender: TObject);
    procedure FDConnectionBeforeDisconnect(Sender: TObject);
    procedure qryCustomerAfterInsert(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetID : Integer;
    procedure RefreshMedicineData(Sender : TObject);
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdtmdLocalDB.FDConnectionAfterConnect(Sender: TObject);
begin
  qryMedLookup.Open();
  qryCategory.Open();
  qryMedicines.Open();
  qryPharmacy.Open();
  qryCustomer.Open();
  qryOrders.Open();
end;

procedure TdtmdLocalDB.FDConnectionBeforeDisconnect(Sender: TObject);
begin
  qryCategory.Close();
  qryMedicines.Close();
  qryPharmacy.Close();
  qryCustomer.Close();
  qryOrders.Close();
  qryMedLookup.Close();
end;

function TdtmdLocalDB.GetID: Integer;
begin
  spGetID.ExecProc;
  Result := spGetID.Params.ParamByName('NewID').Value;
end;

procedure TdtmdLocalDB.qryCustomerAfterInsert(DataSet: TDataSet);
begin
  qryCustomerCUSTOMER_ID.AsInteger := GetID;
end;

procedure TdtmdLocalDB.RefreshMedicineData(Sender : TObject);
begin
  qryCategory.Refresh();
  qryMedicines.Refresh();
  qryMedLookup.Refresh();
end;

end.
