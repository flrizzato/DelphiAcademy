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

TPhoneDialerForm *PhoneDialerForm;
//---------------------------------------------------------------------------
__fastcall TPhoneDialerForm::TPhoneDialerForm(TComponent *Owner) : TForm(Owner)
{
#ifdef __ANDROID__
    FCallPhonePermission = JStringToString(TJManifest_permission::JavaClass->CALL_PHONE);
#endif
    /* test whether the PhoneDialer services are supported */
    TPlatformServices::Current->SupportsPlatformService(__uuidof(IFMXPhoneDialerService), &FPhoneDialerService);
}
//---------------------------------------------------------------------------
void __fastcall TPhoneDialerForm::DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc)
{
    // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
    // After the user sees the explanation, invoke the post-rationale routine to request the permissions
    TDialogService::ShowMessage("The app needs to be able to support your making phone calls",
        [APostRationaleProc](TModalResult AKey)
        {
            APostRationaleProc->Invoke();
        });
}
//---------------------------------------------------------------------------
void __fastcall TPhoneDialerForm::MakePhoneCallPermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults)
{
	// 1 permission involved: CALL_PHONE
	if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted))
		FPhoneDialerService->Call(edtTelephoneNumber->Text);
	else
		TDialogService::ShowMessage("Cannot make a phone call because the required permission has not been granted");
}
//---------------------------------------------------------------------------
void __fastcall TPhoneDialerForm::btnGetCarrierInfoClick(TObject *Sender)
{
	/* test whether the PhoneDialer services are supported */
	if (FPhoneDialerService != NULL)
    {
		/* if yes, then update the labels with the retrieved information */
		CarrierNameItem->ItemData->Detail = Format("Carrier Name: %s", ARRAYOFCONST((FPhoneDialerService->GetCarrier()->GetCarrierName())));
		CountryCodeItem->ItemData->Detail = Format("ISO Country Code: %s", ARRAYOFCONST((FPhoneDialerService->GetCarrier()->GetIsoCountryCode())));
		NetworkCodeItem->ItemData->Detail = Format("Network Code: %s", ARRAYOFCONST((FPhoneDialerService->GetCarrier()->GetMobileCountryCode())));
		MobileNetworkItem->ItemData->Detail = Format("Mobile Network: %s", ARRAYOFCONST((FPhoneDialerService->GetCarrier()->GetMobileNetwork())));
	}
	else
    {
        TDialogService::ShowMessage("PhoneDialer service not supported");
    }
}
//---------------------------------------------------------------------------
void __fastcall TPhoneDialerForm::btnMakeCallClick(TObject *Sender)
{
	/* test whether the PhoneDialer services are supported */
	if (FPhoneDialerService != NULL)
    {
		/* if the Telephone Number is entered in the edit box then make the call, else display an error message */
		if (edtTelephoneNumber->Text != "")
        {
            DynamicArray<String> permissions;
            permissions.Length = 1;
            permissions[0] = FCallPhonePermission;

            PermissionsService()->RequestPermissions(permissions, MakePhoneCallPermissionRequestResult, DisplayRationale);
		}
		else
        {
			TDialogService::ShowMessage("Please type-in a telephone number.");
			edtTelephoneNumber->SetFocus();
		}
	}
	else
    {
        TDialogService::ShowMessage("PhoneDialer service not supported");
    }
}
//---------------------------------------------------------------------------
