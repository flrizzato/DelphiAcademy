//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#include <System.IOUtils.hpp>
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

TAudioRecPlayForm *AudioRecPlayForm;
//---------------------------------------------------------------------------
String __fastcall GetAudioFileName(const String AFileName)
{
#if defined(__ANDROID__)
	return IncludeTrailingPathDelimiter(System::Ioutils::TPath::GetTempPath()) + AFileName;
#else
	#if defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
	return IncludeTrailingPathDelimiter(System::Ioutils::TPath::GetDocumentsPath()) + AFileName;
	#else
	return System::Ioutils::TPath::Combine(System::Ioutils::TPath::GetTempPath(), AFileName);
	#endif
#endif
}
//---------------------------------------------------------------------------
__fastcall TAudioRecPlayForm::TAudioRecPlayForm(TComponent *Owner) : TForm(Owner),
#if defined(__ANDROID__) || defined(TARGET_OS_IPHONE) || defined(TARGET_IPHONE_SIMULATOR)
	AUDIO_FILENAME("test.caf")
#else
	AUDIO_FILENAME("test.wav")
#endif
{
#ifdef __ANDROID__
	FPermission = JStringToString(TJManifest_permission::JavaClass->RECORD_AUDIO);
#endif
	FMicrophone = TCaptureDeviceManager::Current->DefaultAudioCaptureDevice;
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc)
{
	// Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
	// After the user sees the explanation, invoke the post-rationale routine to request the permissions
	TDialogService::ShowMessage("We need to be given permission to record some audio with your microphone",
		[APostRationaleProc](TModalResult AKey)
		{
        	APostRationaleProc->Invoke();
        });
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::RequestPermissionsResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults)
{
	// 1 permission involved: RECORD_AUDIO
	if (AGrantResults.Length == 1)
		switch (AGrantResults[0])
        {
            case TPermissionStatus::Granted:
                try
                {
                    FMicrophone->StartCapture();
                }
                catch (Exception &e)
                {
                    TDialogService::ShowMessage("StartCapture: Operation not supported by this device");
                }
                break;
            case TPermissionStatus::Denied:
                TDialogService::ShowMessage("Cannot record audio without the relevant permission being granted");
                break;
            case TPermissionStatus::PermanentlyDenied:
                TDialogService::ShowMessage("If you decide you wish to use the audio recording feature of this app, please go to app settings and enable the microphone permission");
                break;
		}
	else
		TDialogService::ShowMessage("Something went wrong with the permission checking");
}
//---------------------------------------------------------------------------
bool __fastcall TAudioRecPlayForm::HasMicrophone()
{
	return FMicrophone;
}
//---------------------------------------------------------------------------
bool __fastcall TAudioRecPlayForm::IsMicrophoneRecording()
{
	return HasMicrophone() && (FMicrophone->State == TCaptureDeviceState::Capturing);
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::ActionListUpdate(TBasicAction *Action, bool &Handled)
{
	imgOn->Visible = HasMicrophone() && (FMicrophone->State == TCaptureDeviceState::Capturing);
	actStartRecording->Enabled = !IsMicrophoneRecording() && HasMicrophone();
	actStopRecording->Enabled = IsMicrophoneRecording();
	actStop->Enabled = (MediaPlayer->Media != NULL) && (MediaPlayer->State == TMediaState::Playing);
	actPlay->Enabled = FileExists(GetAudioFileName(AUDIO_FILENAME)) && (MediaPlayer->State != TMediaState::Playing);
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::actPlayExecute(TObject *Sender)
{
	if (IsMicrophoneRecording())
		actStopRecording->Execute();
	MediaPlayer->FileName = GetAudioFileName(AUDIO_FILENAME);
	MediaPlayer->Play();
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::actStopExecute(TObject *Sender)
{
	MediaPlayer->Stop();
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::actStartRecordingExecute(TObject *Sender)
{
	actStop->Execute();
	// get the microphone device
	FMicrophone = TCaptureDeviceManager::Current->DefaultAudioCaptureDevice;
	if (HasMicrophone())
    {
		// and attempt to record to 'test.caf' file
		FMicrophone->FileName = GetAudioFileName(AUDIO_FILENAME);

		DynamicArray<String> permissions;
		permissions.Length = 1;
		permissions[0] = FPermission;

		PermissionsService()->RequestPermissions(permissions, RequestPermissionsResult, DisplayRationale);
	}
	else
		TDialogService::ShowMessage("No microphone is available.");
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::actStopRecordingExecute(TObject *Sender)
{
	// stop capturing audio from the microphone
	if (IsMicrophoneRecording())
    {
		try
        {
			FMicrophone->StopCapture();
		}
		catch (Exception &e)
        {
			TDialogService::ShowMessage("Get state: Operation not supported by this device");
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::imgOffClick(TObject *Sender)
{
	actStartRecordingExecute(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TAudioRecPlayForm::imgOnClick(TObject *Sender)
{
	actStopRecordingExecute(Sender);
}
//---------------------------------------------------------------------------
