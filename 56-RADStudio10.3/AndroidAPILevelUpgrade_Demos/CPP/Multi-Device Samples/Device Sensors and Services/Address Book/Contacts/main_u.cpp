// ---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.
//
// This software is the copyrighted property of Embarcadero Technologies, Inc.
// ("Embarcadero") and its licensors. You may only use this software if you
// are an authorized licensee of Delphi, C++Builder or RAD Studio
// (the "Embarcadero Products").  This software is subject to Embarcadero's
// standard software license and support agreement that accompanied your
// purchase of the Embarcadero Products and is considered a Redistributable,
// as such term is defined thereunder. Your use of this software constitutes
// your acknowledgement of your agreement to the foregoing software license
// and support agreement.
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

#include <fmx.h>
#ifdef __ANDROID__
#include <Androidapi.Helpers.hpp>
#include <Androidapi.JNI.Os.hpp>
#endif
#include <FMX.DialogService.hpp>
#pragma hdrstop

#include "main_u.h"

// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
#pragma resource ("*.iPhone47in.fmx", _PLAT_IOS)

TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner) {
}
// ---------------------------------------------------------------------------

// Add a new contact to List box
void __fastcall TForm1::AddListViewItem(TAddressBookContact *Contact) {
	TListViewItem *ListViewItem = ListViewContacts->Items->Add();
	__try {
		ListViewItem->Text = Contact->DisplayName;
		ListViewItem->Tag = Contact->ID;
	}
	__finally {
		ListViewItem->Free();
	}
}
// ---------------------------------------------------------------------------

// Fill the Combo box with existed group names
void __fastcall TForm1::FillGroupList(TAddressBookSource *Source) {
	FGroups = new TAddressBookGroups();
	AddressBook1->AllGroups(Source, FGroups);
	try {
		ComboBox1->BeginUpdate();
		ComboBox2->BeginUpdate();
		ComboBox1->Clear();
		ComboBox2->Clear();
		for (int i = 0; i < FGroups->Count; i++) {
			ComboBox1->Items->Add(FGroups->Items[i]->Name);
			ComboBox2->Items->Add(FGroups->Items[i]->Name);
		}
	}
	__finally {
		ComboBox1->Visible = (FGroups->Count != 0);
		ComboBox2->Visible = ComboBox1->Visible;
		btnRemoveGroup->Visible = ComboBox1->Visible;
		ComboBox2->EndUpdate();
		ComboBox1->EndUpdate();
	}
}
// ---------------------------------------------------------------------------

// Fill the List box with display names of existed contacts.
void __fastcall TForm1::FillContactlist(TAddressBookSource *Source) {
	TAddressBookContacts *Contacts = new TAddressBookContacts();
	try {
		AddressBook1->AllContacts(Source, Contacts);
		try {
			ListViewContacts->BeginUpdate();
			ListViewContacts->Items->Clear();
			for (int i = 0; i < Contacts->Count; i++)
				AddListViewItem(Contacts->Items[i]);
		}
		__finally {
			ListViewContacts->EndUpdate();
		}
	}
	__finally {
		Contacts->Free();
	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::FormShow(TObject *Sender) {
	if (AddressBook1->Supported()) {
		// Display this information box while loading the contacts
		TDialogService::ShowMessage("Loading contacts...");
#ifdef __ANDROID__
		FPermissionReadContacts = JStringToString(TJManifest_permission::JavaClass->READ_CONTACTS);
		FPermissionWriteContacts = JStringToString(TJManifest_permission::JavaClass->WRITE_CONTACTS);
		FPermissionCamera = JStringToString(TJManifest_permission::JavaClass->CAMERA);
		FPermissionReadExternalStorage = JStringToString(TJManifest_permission::JavaClass->READ_EXTERNAL_STORAGE);
		FPermissionWriteExternalStorage = JStringToString(TJManifest_permission::JavaClass->WRITE_EXTERNAL_STORAGE);
#endif
		AddressBook1->RequestPermission(DisplayRationale);
	}
	else {
		TDialogService::ShowMessage("This platform does not support the Address Book service.");
	}
	TabControl1->ActiveTab = TabItemContacts;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::AddressBook1PermissionRequest(TObject *ASender, const UnicodeString AMessage,
	const bool AAccessGranted) {
	if (AAccessGranted) {
		ActionRefresh->Execute();
	}
	else {
		ShowMessage("You cannot access Address Book. Reason: " + AMessage);
	}
}
// ---------------------------------------------------------------------------

// Clear the Add Contact form
void __fastcall TForm1::ClearAddContactForm() {
	edtFirstName->Text = "";
	edtLastName->Text = "";
	edtWorkMail->Text = "";
	ComboBox1->ItemIndex = -1;
	Image1->Bitmap->SetSize(0, 0);
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::btnClearClick(TObject *Sender) {
	ClearAddContactForm();
}
// ---------------------------------------------------------------------------

bool __fastcall TForm1::ContactExists(int ID) {
	TAddressBookContact *Contact = AddressBook1->ContactByID(ID);
	bool b = Contact != NULL;
	Contact->Free();
	return b;
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::AddressBook1ExternalChange(TObject *ASender) {
	AddressBook1->RevertCurrentChangesAndUpdate();
	ActionRefresh->Execute();
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ActionRefreshExecute(TObject *Sender) {
	// Select the default source as the current source
	FCurrentSource = AddressBook1->DefaultSource();
	FillGroupList(FCurrentSource);
	FillContactlist(FCurrentSource);
}
// ---------------------------------------------------------------------------

// Add a newly created contact to Address Book
void __fastcall TForm1::ActionAddContactExecute(TObject *Sender) {
	TAddressBookContact *Contact;
	TContactEmails *eMails;
	Contact = AddressBook1->CreateContact(FCurrentSource);

	try {
		try {

			Contact->FirstName = edtFirstName->Text;
			Contact->LastName = edtLastName->Text;
			// Add a photo if selected
			if (!Image1->Bitmap->Size.IsZero()) {
				TBitmapSurface *photo = new TBitmapSurface();
				try {
					photo->Assign(Image1->Bitmap);
					Contact->Photo = photo;
					Image1->Bitmap->SetSize(0, 0);
				}
				__finally {
					photo->Free();
				}
			}
			// Add the work mail
			eMails = new TContactEmails();
			try {
				eMails->AddEmail(TContactEmail::TLabelKind::Work, edtWorkMail->Text);
				Contact->EMails = eMails;
			}
			__finally {
				eMails->Free();
			}
			AddressBook1->SaveContact(Contact);
		}
		catch (const EAddressBookException& E) {
			ShowMessage("Cannot create the contact. " + E.Message);
		}
		// Add the contact to the selected group, if any
		try {
			if ((ComboBox1->ItemIndex > -1) && (ComboBox1->ItemIndex < FGroups->Count)) {
				AddressBook1->AddContactIntoGroup(FGroups->Items[ComboBox1->ItemIndex], Contact);
			}
		}
		catch (const EAddressBookException& E) {
			ShowMessage("Cannot add the created contact to the group . " + E.Message);
		}

		ListViewContacts->BeginUpdate();
		AddListViewItem(Contact);
		TabControl1->ActiveTab = TabItemContacts;
	}
	__finally {

		Contact->Free();
		ListViewContacts->EndUpdate();
		ClearAddContactForm();

	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ActionAddGroupExecute(TObject *Sender) {
	TAddressBookGroup *Group = AddressBook1->CreateGroup(FCurrentSource);
	if (Group != NULL) {
		try {
			Group->Name = edtGroupName->Text;
			try {
				AddressBook1->SaveGroup(Group);
				edtGroupName->Text = "";
				TabControl1->ActiveTab = TabItemContacts;
			}
			catch (const EAddressBookException& E) {
				ShowMessage("Cannot add the group. " + E.Message);
			}
			FillGroupList(FCurrentSource);
		}
		__finally {
			Group->Free();
		}
	}
	else {
		ShowMessage("Cannot add this group.");
	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::ActionList1Update(TBasicAction *Action, bool &Handled) {
	ActionAddContact->Enabled = AddressBook1->Supported() && ((edtFirstName->Text != "") || (edtLastName->Text != ""));
	ActionRefresh->Enabled = AddressBook1->Supported();
	ActionAddGroup->Enabled = AddressBook1->Supported();
	ActionRemoveGroup->Visible = (FGroups != NULL) && (FGroups->Count > 0);
	ActionRemoveContact->Enabled = ListViewContacts->ItemIndex > -1;
}
// ---------------------------------------------------------------------------

// Delete the selected contact (only after a confirmation from user)
void __fastcall TForm1::DeleteContact(TObject * Sender, TModalResult AKey) {
	if (AKey == System::Uitypes::mrYes) {
		int contactIndex = ListViewContacts->ItemIndex;
		if (contactIndex > -1) {
			int ContactID = ListViewContacts->Items->AppearanceItem[contactIndex]->Tag;
			TAddressBookContact *Contact = AddressBook1->ContactByID(ContactID);
			try {
				AddressBook1->RemoveContact(Contact);
				ListViewContacts->BeginUpdate();
				// Check if the contact was really deleted
				if (!ContactExists(ContactID)) {
					ListViewContacts->Items->Delete(contactIndex);
				}
				else
					ShowMessage("Cannot delete this contact: " + Contact->FirstName);
			}
			__finally {
				ListViewContacts->EndUpdate();
				Contact->Free();

			}
		}

	}
}
// ---------------------------------------------------------------------------

// Display a confirmation dialog box
void __fastcall TForm1::ActionRemoveContactExecute(TObject * Sender) {
	TDialogService::MessageDialog("Do you want to delete this contact?", TMsgDlgType::mtConfirmation,
		TMsgDlgButtons() << TMsgDlgBtn::mbYes << TMsgDlgBtn::mbNo, TMsgDlgBtn::mbNo, 0, DeleteContact);
}
// ---------------------------------------------------------------------------

// Remove selected group
void __fastcall TForm1::ActionRemoveGroupExecute(TObject * Sender) {
	if ((ComboBox2->ItemIndex > -1) && (ComboBox2->ItemIndex < FGroups->Count)) {
		AddressBook1->RemoveGroup(FGroups->Items[ComboBox2->ItemIndex]);
		FillGroupList(FCurrentSource);
	}
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::btnLoadPictureClick(TObject *Sender) {
	TStringDynArray perms;
	perms.set_length(1);
	perms[0] = FPermissionReadExternalStorage;
	PermissionsService()->RequestPermissions(perms, LoadPicturePermissionRequestResult, DisplayRationale);
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::btnTakePictureClick(TObject *Sender) {
	TStringDynArray perms;
	perms.set_length(3);
	perms[0] = FPermissionCamera;
	perms[1] = FPermissionReadExternalStorage;
	perms[2] = FPermissionWriteExternalStorage;
	PermissionsService()->RequestPermissions(perms, TakePicturePermissionRequestResult, DisplayRationale);
}
// ---------------------------------------------------------------------------

// Select a photo from a device photo library
void __fastcall TForm1::TakePhotoFromLibraryAction1DidFinishTaking(TBitmap * Image) {
	Image1->Bitmap->Assign(Image);
}
// ---------------------------------------------------------------------------

// Take a photo from a device camera
void __fastcall TForm1::TakePhotoFromCameraAction1DidFinishTaking(TBitmap * Image) {
	Image1->Bitmap->Assign(Image);
}
// ---------------------------------------------------------------------------

// Optional rationale display routine to display permission requirement rationale to the user
void __fastcall TForm1::DisplayRationale(TObject* Sender, const TStringDynArray APermissions,
    const _di_TProc APostRationaleProc) {
    String RationaleMsg;
    for (int i = 0; i < APermissions.Length; i++) {
        if (APermissions[i] == FPermissionReadContacts)
            RationaleMsg = RationaleMsg + "The app needs to look in the address book";
        else if (APermissions[i] == FPermissionWriteContacts)
            RationaleMsg = RationaleMsg + "The app needs to update the address book";
        else if (APermissions[i] == FPermissionCamera)
            RationaleMsg = RationaleMsg + "The app needs to access the camera to take a photo for your new contact";
        else if (APermissions[i] == FPermissionReadExternalStorage)
            RationaleMsg =
                RationaleMsg + "The app needs to read a photo file from your device to associate with your new contact";
        RationaleMsg = RationaleMsg + APermissions[i];
        if (i != APermissions.Length - 1)
            RationaleMsg = RationaleMsg + sLineBreak + sLineBreak;
    }

    // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
    // After the user sees the explanation, invoke the post-rationale routine to request the permissions
    TDialogService::ShowMessage(RationaleMsg, [APostRationaleProc](TModalResult AKey) {APostRationaleProc->Invoke();});
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::TakePicturePermissionRequestResult(TObject* Sender, const System::TArray__1<String>APermissions,
    const System::TArray__1<TPermissionStatus>AGrantResults) {
    // 3 permissions involved: CAMERA, READ_EXTERNAL_STORAGE and WRITE_EXTERNAL_STORAGE
    if ((AGrantResults.Length == 3) && (AGrantResults[0] == TPermissionStatus::Granted) &&
        (AGrantResults[1] == TPermissionStatus::Granted) && (AGrantResults[2] == TPermissionStatus::Granted))
        TakePhotoFromCameraAction1->Execute();
    else
        ShowMessage("Cannot take a photo because the required permissions are not all granted");
}
// ---------------------------------------------------------------------------

void __fastcall TForm1::LoadPicturePermissionRequestResult(TObject* Sender, const System::TArray__1<String>APermissions,
    const System::TArray__1<TPermissionStatus>AGrantResults) {
    // 1 permission involved: CAMERA
    if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted))
        TakePhotoFromLibraryAction1->Execute();
    else
        ShowMessage("Cannot load a photo because the READ_EXTERNAL_STORAGE permission has been denied");
}
