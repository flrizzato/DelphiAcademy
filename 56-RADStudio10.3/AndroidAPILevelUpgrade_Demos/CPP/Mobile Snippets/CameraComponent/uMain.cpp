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

#include "uMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TCameraComponentForm *CameraComponentForm;
//---------------------------------------------------------------------------
__fastcall TCameraComponentForm::TCameraComponentForm(TComponent* Owner)
	: TForm(Owner)
{
#ifdef __ANDROID__
    FPermissionCamera = JStringToString(TJManifest_permission::JavaClass->CAMERA);
#endif
    TStringDynArray perms;
    perms.set_length(1);
    perms[0] = FPermissionCamera;
    PermissionsService()->RequestPermissions(perms, AccessCameraPermissionRequestResult, DisplayRationale);
}
//---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::DisplayRationale(TObject* Sender, const TStringDynArray APermissions, const _di_TProc APostRationaleProc)
{
    // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
    // After the user sees the explanation, invoke the post-rationale routine to request the permissions
    TDialogService::ShowMessage("The app needs to access the camera in order to work",
        [APostRationaleProc](TModalResult AKey)
        {
            APostRationaleProc->Invoke();
        });
}
//---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::AccessCameraPermissionRequestResult(TObject* Sender, const System::TArray__1<String> APermissions, const System::TArray__1<TPermissionStatus> AGrantResults)
{
	// 1 permission involved: CAMERA
	if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted))
        // Fill the resolutions
        FillResolutions();
	else
		ShowMessage("Cannot access the camera because the required permission has not been granted");
}
//---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::ActivateCameraPermissionRequestResult(TObject* Sender, const System::TArray__1<String> APermissions, const System::TArray__1<TPermissionStatus> AGrantResults)
{
	// 1 permission involved: CAMERA
	if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted)) {
        // Turn on the Camera
        CameraComponent->Active = True;
        tbControl->TabIndex = 1;
    }
	else
		ShowMessage("Cannot start the camera because the required permission has not been granted");
}
//---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::btnStartCameraClick(TObject *Sender)
{
    TStringDynArray perms;
    perms.set_length(1);
    perms[0] = FPermissionCamera;
    PermissionsService()->RequestPermissions(perms, ActivateCameraPermissionRequestResult, DisplayRationale);
}
//---------------------------------------------------------------------------
void __fastcall TCameraComponentForm::GetImage()
{
	CameraComponent->SampleBufferToBitmap(imgCameraView->Bitmap, true);
}
void __fastcall TCameraComponentForm::btnStopCameraClick(TObject *Sender)
{
//  Turn off the Camera
	CameraComponent->Active = false;
	tbControl->TabIndex = 0;
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnFrontCameraClick(TObject *Sender)
{
	bool LActive = CameraComponent->Active;
	CameraComponent->Active = false;
	// Select Front Camera
	CameraComponent->Kind = TCameraKind::FrontCamera;
	CameraComponent->Active = LActive;
	FillResolutions();
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnBackCameraClick(TObject *Sender)
{
	bool LActive = CameraComponent->Active;
	CameraComponent->Active = false;
	// Select Back Camera
	CameraComponent->Kind = TCameraKind::BackCamera;
	CameraComponent->Active = LActive;
	FillResolutions();
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::FormCreate(TObject *Sender)
{
	IFMXApplicationEventService *AppEventSvc;
	// Add platform service to see camera state. This is nedded to enable or disable the camera when the application
	// goes to background.
	if (TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXApplicationEventService), &AppEventSvc))
		AppEventSvc->SetApplicationEventHandler(AppEvent);
}
//---------------------------------------------------------------------------

void TCameraComponentForm::FillResolutions()
{
	System::DynamicArray<TVideoCaptureSetting> LSettings = CameraComponent->AvailableCaptureSettings;
	cbResolutions->Clear();
	for (int i = LSettings.Low; i <= LSettings.High; i++)
		cbResolutions->Items->Add(UnicodeString(LSettings[i].Width) + " x " + LSettings[i].Height + " x " + LSettings[i].FrameRate);
	cbResolutions->ItemIndex = 0;
}
//---------------------------------------------------------------------------

bool __fastcall TCameraComponentForm::AppEvent(TApplicationEvent AAppEvent, System::TObject* AContext)
{
	switch (AAppEvent) {
		case TApplicationEvent::WillBecomeInactive:
		case TApplicationEvent::EnteredBackground:
		case TApplicationEvent::WillTerminate:
			CameraComponent->Active = false;
			return true;
        default:
            return false;
	}
	return false;
}
//---------------------------------------------------------------------------

void TCameraComponentForm::ChangeQuality(const TVideoCaptureQuality ANewQuality)
{
	bool LActive = CameraComponent->Active;
	CameraComponent->Active = False;
	CameraComponent->Quality = ANewQuality;
	CameraComponent->Active = LActive;
	ShowCurrentResolution();
}
//---------------------------------------------------------------------------

void TCameraComponentForm::ShowCurrentResolution()
{
	TVideoCaptureSetting LSettings;
	UnicodeString LText;
	switch (CameraComponent->Quality) {
		case TVideoCaptureQuality::PhotoQuality:
			LText = "Photo";
		break;
		case TVideoCaptureQuality::HighQuality:
			LText = "High";
		break;
		case TVideoCaptureQuality::MediumQuality:
			LText = "Medium";
		break;
		case TVideoCaptureQuality::LowQuality:
			LText = "Low";
		break;
		case TVideoCaptureQuality::CaptureSettings:
			LText = "Custom";
		break;
	}
	LSettings = CameraComponent->CaptureSetting;
	lblCurrentResolution->Text = LText + " - " + LSettings.Width + "x" + LSettings.Height + " at " + LSettings.FrameRate + " FPS.";
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::cbResolutionsChange(TObject *Sender)
{
	int LIndex;
	DynamicArray<TVideoCaptureSetting> LSettings;
	bool LActive = CameraComponent->Active;
	CameraComponent->Active = False;
	LIndex = cbResolutions->ItemIndex;
	LSettings = CameraComponent->AvailableCaptureSettings;
	if (LSettings.Length > 0)
		CameraComponent->CaptureSetting = LSettings[LIndex];
	CameraComponent->Active = LActive;
	ShowCurrentResolution();
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::cbPriorityChange(TObject *Sender)
{
	if (lbiResolution->IsSelected)
		CameraComponent->CaptureSettingPriority = TVideoCaptureSettingPriority::Resolution;
	if (lbiFrameRate->IsSelected)
		CameraComponent->CaptureSettingPriority = TVideoCaptureSettingPriority::FrameRate;
	FillResolutions();
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnLowQualityClick(TObject *Sender)
{
	ChangeQuality(TVideoCaptureQuality::LowQuality);
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnMediumQualityClick(TObject *Sender)
{
	ChangeQuality(TVideoCaptureQuality::MediumQuality);
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnHighQualityClick(TObject *Sender)
{
	ChangeQuality(TVideoCaptureQuality::HighQuality);
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnPhotoQualityClick(TObject *Sender)
{
	ChangeQuality(TVideoCaptureQuality::PhotoQuality);
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnOnClick(TObject *Sender)
{
	 bool LActive;
	 // Turn on the Torch, if supported
	 if (CameraComponent->HasTorch) {
		LActive = CameraComponent->Active;
		CameraComponent->Active = false;
		CameraComponent->TorchMode = TTorchMode::ModeOn;
		CameraComponent->Active = LActive;		
	 }
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnOffClick(TObject *Sender)
{
	 bool LActive;
	 // Turn off the Torch, if supported
	 if (CameraComponent->HasTorch) {
		LActive = CameraComponent->Active;
		CameraComponent->Active = false;
		CameraComponent->TorchMode = TTorchMode::ModeOff;
		CameraComponent->Active = LActive;		
	 }
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::btnAutoClick(TObject *Sender)
{
	 bool LActive;
	 // Turn on automatic Torch, if supported
	 if (CameraComponent->HasTorch) {
		LActive = CameraComponent->Active;
		CameraComponent->Active = false;
		CameraComponent->TorchMode = TTorchMode::ModeAuto;
		CameraComponent->Active = LActive;		
	 }
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::CameraComponentSampleBufferReady(TObject *Sender, const TMediaTime ATime)
{
	TThread::Synchronize(TThread::CurrentThread, GetImage);
}
//---------------------------------------------------------------------------

void __fastcall TCameraComponentForm::tbControlChange(TObject *Sender)
{
	CameraComponent->Active = tbControl->ActiveTab == tiPreview;
}
//---------------------------------------------------------------------------
