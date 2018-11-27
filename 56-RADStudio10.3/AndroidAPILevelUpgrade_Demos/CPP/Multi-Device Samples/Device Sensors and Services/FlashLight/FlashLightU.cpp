//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#ifdef __ANDROID__
#include <Androidapi.Helpers.hpp>
#include <Androidapi.JNI.Os.hpp>
#endif
#include <FMX.DialogService.hpp>
#pragma hdrstop

#include "FlashLightU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TFlashLightForm *FlashLightForm;
//---------------------------------------------------------------------------
__fastcall TFlashLightForm::TFlashLightForm(TComponent *Owner) : TForm(Owner) {
#ifdef __ANDROID__
	FPermissionCamera = JStringToString(TJManifest_permission::JavaClass->CAMERA);
#endif
	DynamicArray<String> permissions;
	permissions.Length = 1;
	permissions[0] = FPermissionCamera;

	PermissionsService()->RequestPermissions(permissions, AccessCameraPermissionRequestResult, DisplayRationale);
}
//---------------------------------------------------------------------------
void __fastcall TFlashLightForm::DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc) {
	// Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
	// After the user sees the explanation, invoke the post-rationale routine to request the permissions
	TDialogService::ShowMessage("The app needs to access the camera in order to work",
		[APostRationaleProc](TModalResult AKey)
		{
        	APostRationaleProc->Invoke();
        });
}
//---------------------------------------------------------------------------
void __fastcall TFlashLightForm::AccessCameraPermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults) {
	// 1 permission involved: CAMERA
	if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted))
		ImageOff->Enabled = Camera->HasFlash;
	else
		TDialogService::ShowMessage("Cannot access the camera flashlight because the required permission has not been granted");
}
//---------------------------------------------------------------------------
void __fastcall TFlashLightForm::ActivateCameraPermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults) {
	// 1 permission involved: CAMERA
	if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted)) {
		Camera->Active = True;
		ImageOff->Visible = false;
		ImageOn->Visible = true;
		SetFlashlightState(true);
		Light->Visible = true;
	}
	else
		TDialogService::ShowMessage("Cannot access the camera flashlight because the required permission has not been granted");
}
//---------------------------------------------------------------------------
void __fastcall TFlashLightForm::SetFlashlightState(bool Active) {
	if (Active)
		Camera->TorchMode = TTorchMode::ModeOn;
	else
		Camera->TorchMode = TTorchMode::ModeOff;
}
//---------------------------------------------------------------------------
void __fastcall TFlashLightForm::ImageOffClick(TObject *Sender) {
	DynamicArray<String> permissions;
	permissions.Length = 1;
	permissions[0] = FPermissionCamera;

	PermissionsService()->RequestPermissions(permissions, ActivateCameraPermissionRequestResult, DisplayRationale);
}
//---------------------------------------------------------------------------
void __fastcall TFlashLightForm::ImageOnClick(TObject *Sender) {
	ImageOff->Visible = true;
	ImageOn->Visible = false;
	SetFlashlightState(false);
	Light->Visible = false;
}
//---------------------------------------------------------------------------
