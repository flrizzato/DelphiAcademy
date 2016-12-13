//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program PharmacyApp;

uses
  Vcl.Forms,
  formPharmacy in 'formPharmacy.pas' {frmPharmacy},
  dmLocalData in 'dmLocalData.pas' {dtmdLocalDB: TDataModule},
  formPharmacyController in 'formPharmacyController.pas' {dmPharmacyController: TDataModule},
  frameData in 'frameData.pas' {Frame1: TFrame},
  dmDelta in 'dmDelta.pas' {dtmdlDelta: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPharmacy, frmPharmacy);
  Application.Run;
end.
