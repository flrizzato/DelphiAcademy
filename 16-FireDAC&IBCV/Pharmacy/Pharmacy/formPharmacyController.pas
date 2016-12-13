//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit formPharmacyController;

interface

uses
  System.SysUtils, System.Classes, formPharmacy, dmLocalData, System.Actions,
  Vcl.ActnList, dmDelta;

type
  TdmPharmacyController = class(TDataModule)
    ActionList: TActionList;
    actAddOrder: TAction;
    actFetchDeltas: TAction;
    actPostDeltas: TAction;
    procedure DataModuleDestroy(Sender: TObject);
    procedure actAddOrderExecute(Sender: TObject);
    procedure actFetchDeltasExecute(Sender: TObject);
    procedure actPostDeltasExecute(Sender: TObject);
  private
    { Private declarations }
    FView: TfrmPharmacy;
    FDataModel: TdtmdLocalDB;
    dtmdlDelta: TdtmdlDelta;
    procedure NavigateToMeds(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AView : TfrmPharmacy; ADataModel : TdtmdLocalDB); reintroduce;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  VCL.Dialogs, System.IniFiles, System.IOUtils;
{ TdmPharmacyController }

procedure TdmPharmacyController.actAddOrderExecute(Sender: TObject);
begin
  FDataModel.qryOrders.Insert;
  FDataModel.qryOrdersMEDICINE_ID.Value := FDataModel.qryMedicinesMEDICINE_ID.Value;
  FDataModel.qryOrdersQUANTITY.Value := FView.seQuantity.Value;
  FDataModel.qryOrders.Post;
end;

procedure TdmPharmacyController.actFetchDeltasExecute(Sender: TObject);
begin
  if Assigned(dtmdlDelta) then
    FreeAndNil(dtmdlDelta);

  FDataModel.FDConnectionRemote.Close;
  FDataModel.FDConnectionRemote.Params.Database := FView.edtCentralServer.Text;
  FDataModel.FDConnectionRemote.Open;

  // Need to define the ID, Just using the record ID for now, but this could be any unique string
  dtmdlDelta := TdtmdlDelta.Create(Self, FDataModel.qryPharmacyPHARMACY_ID.AsString, FView.cbShowMerged.Checked, FDataModel.FDConnection, FDataModel.FDConnectionRemote);
  dtmdlDelta.OnPostDeltas := FDataModel.RefreshMedicineData;

  // Show the data in the frames
  FView.FrameMedicine.dsCurrent.DataSet := dtmdlDelta.qryMedicine;
//  FView.FrameMedicine.dsDelta.DataSet := dtmdlDelta.qryMedicineDelta;
  FView.FrameMedicine.dsDelta.DataSet := dtmdlDelta.mtMedicineMerged;

  FView.FrameCategory.dsCurrent.DataSet := dtmdlDelta.qryCategory;
//  FView.FrameCategory.dsDelta.DataSet := dtmdlDelta.qryCategoryDelta;
  FView.FrameCategory.dsDelta.DataSet := dtmdlDelta.mtCategoryMerged;

  FView.FrameLinks.dsCurrent.DataSet := dtmdlDelta.qryMedicineCategories;
//  FView.FrameLinks.dsDelta.DataSet := dtmdlDelta.qryMedicineCategoriesDelta;
  FView.FrameLinks.dsDelta.DataSet := dtmdlDelta.mtMedicineCategoriesMerged;

end;

procedure TdmPharmacyController.actPostDeltasExecute(Sender: TObject);
begin
  if dtmdlDelta = nil then
    ShowMessage('Please Fetch Deltas first')
  else begin
    dtmdlDelta.PostDeltas;
    actFetchDeltas.Execute;
  end;
end;

constructor TdmPharmacyController.Create(AView: TfrmPharmacy;
  ADataModel: TdtmdLocalDB);
var
  INI : TIniFile;
begin
  inherited Create(AView);
  FView := AView;
  FDataModel := ADataModel;

  // Setup the BindSources.
  FView.bsCategory.DataSource.DataSet := FDataModel.qryCategory;
  FView.bsMedicines.DataSource.DataSet := FDataModel.qryMedicines;
  FView.bsOrder.DataSource.DataSet := FDataModel.qryOrders;
  FView.bsPharmacy.DataSource.DataSet := FDataModel.qryPharmacy;
  FView.bsCustomer.DataSource.DataSet := FDataModel.qryCustomer;
  FView.gridOrders.DataSource := FDataModel.dsOrders;

  INI := TIniFile.Create(ExtractFilePath(ParamStr(0))+'PharmacyApp.Ini');
  try
    FDataModel.FDConnection.Params.Database := INI.ReadString('Database', 'Local','c:\data\pharmacy.ib');
    FView.edtCentralServer.Text := INI.ReadString('Database', 'Remote','127.0.0.1:c:\data\medicines.ib');

    FDataModel.FDConnection.Open;

    // Make the ini first time
    INI.WriteString('Database', 'Local',FDataModel.FDConnection.Params.Database);
    INI.WriteString('Database', 'Remote',FView.edtCentralServer.Text);
  finally
    INI.Free
  end;

  FView.btnAddOrder.Action := actAddOrder;
  FView.btnWebdetailsLoad.OnClick := NavigateToMeds;
  FView.PageControl1.ActivePageIndex := 0;
  FView.PageControl2.ActivePageIndex := 0;

  FView.btnFetchDeltas.Action := actFetchDeltas;
  FView.btnPostDeltas.Action := actPostDeltas;
end;

procedure TdmPharmacyController.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(dtmdlDelta) then
    FreeAndNil(dtmdlDelta);
  FDataModel.FDConnection.Close;
end;

procedure TdmPharmacyController.NavigateToMeds(Sender: TObject);
begin
  FView.WebBrowser1.Navigate(FDataModel.qryMedicinesURL.Value);
  FView.lblURL.Caption := FDataModel.qryMedicinesURL.Value;
end;


end.

