// ---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// ---------------------------------------------------------------------------

#include <fmx.h>
#ifdef __ANDROID__
#include <Androidapi.Helpers.hpp>
#include <Androidapi.JNI.Os.hpp>
#endif
#include <FMX.DialogService.hpp>
#pragma hdrstop

#include "uMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.LgXhdpiPh.fmx", _PLAT_ANDROID)

TAccessCameraAppForm *AccessCameraAppForm;
// ---------------------------------------------------------------------------
__fastcall TAccessCameraAppForm::TAccessCameraAppForm(TComponent *Owner) : TForm(Owner)
{
#ifdef __ANDROID__
	FPermissionCamera = JStringToString(TJManifest_permission::JavaClass->CAMERA);
	FPermissionReadExternalStorage = JStringToString(TJManifest_permission::JavaClass->READ_EXTERNAL_STORAGE);
	FPermissionWriteExternalStorage = JStringToString(TJManifest_permission::JavaClass->WRITE_EXTERNAL_STORAGE);
#endif
}
// ---------------------------------------------------------------------------
void __fastcall TAccessCameraAppForm::DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc)
{
	String RationaleMsg;
	for (int i = 0; i < APermissions.Length; i++)
    {
		if (APermissions[i] == FPermissionCamera)
			RationaleMsg = RationaleMsg + "The app needs to access the camera to take a photo" + sLineBreak + sLineBreak;
		else if (APermissions[i] == FPermissionReadExternalStorage)
			RationaleMsg = RationaleMsg + "The app needs to read a photo file from your device";
	}

	// Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
	// After the user sees the explanation, invoke the post-rationale routine to request the permissions
	TDialogService::ShowMessage(RationaleMsg,
		[APostRationaleProc](TModalResult AKey)
		{
        	APostRationaleProc->Invoke();
        });
}
// ---------------------------------------------------------------------------
void __fastcall TAccessCameraAppForm::TakePicturePermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults)
{
	// 3 permissions involved: CAMERA, READ_EXTERNAL_STORAGE and WRITE_EXTERNAL_STORAGE
	if ((AGrantResults.Length == 3) &&
		(AGrantResults[0] == TPermissionStatus::Granted) &&
		(AGrantResults[1] == TPermissionStatus::Granted) &&
		(AGrantResults[2] == TPermissionStatus::Granted))
		TakePhotoFromCameraAction1->Execute();
	else
		TDialogService::ShowMessage("Cannot take a photo because the required permissions are not all granted");
}
// ---------------------------------------------------------------------------
void __fastcall TAccessCameraAppForm::TakePhotoFromCameraAction1DidFinishTaking(TBitmap *Image)
{
	/* Assign the image retrieved from the Camera to the TImage component. */
	imgCameraImage->Bitmap->Assign(Image);
}
// ---------------------------------------------------------------------------
void __fastcall TAccessCameraAppForm::btnTakePhotoClick(TObject *Sender)
{
	DynamicArray<String> permissions;
	permissions.Length = 3;
	permissions[0] = FPermissionCamera;
	permissions[1] = FPermissionReadExternalStorage;
	permissions[2] = FPermissionWriteExternalStorage;

	PermissionsService()->RequestPermissions(permissions, TakePicturePermissionRequestResult, DisplayRationale);
}
// ---------------------------------------------------------------------------
