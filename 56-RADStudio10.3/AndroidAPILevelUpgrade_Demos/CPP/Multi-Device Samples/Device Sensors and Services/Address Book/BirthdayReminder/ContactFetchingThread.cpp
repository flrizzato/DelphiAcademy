// ---------------------------------------------------------------------------

#include <System.hpp>
#pragma hdrstop

#include "ContactFetchingThread.h"
#pragma package(smart_init)

// ---------------------------------------------------------------------------
TFetchContactThread::TFetchContactThread(TAddressBookContacts* AContacts)
	: TThread(true) {
	this->allContacts = AContacts;
}

// ---------------------------------------------------------------------------
void __fastcall TFetchContactThread::Execute() {
	NameThreadForDebugging(System::String(L"Fetching contacts"));
	TAddressBookContact* contact;

	Synchronize(DoStart);
	totalCount = allContacts->Count;
	for (int i = 0; i < totalCount; i++) {
		if (Terminated) {
			return;
		}
		contact = allContacts->Items[i];
		currentNumber = i;
		displayName = contact->DisplayName;
		photo = contact->PhotoThumbnail;
		birthday = contact->Birthday;
		try {
			Synchronize(DoContactLoaded);
		}
		__finally {
			if (photo != NULL) {
				photo->Free();
			}
		}
	}
}

// ---------------------------------------------------------------------------
void __fastcall TFetchContactThread::DoStart() {
	if (FOnStart != (TNotifyEvent)NULL) {
		FOnStart(this);
	}
}

// ---------------------------------------------------------------------------
void __fastcall TFetchContactThread::DoContactLoaded() {
	if (FOnContactLoaded != (TOnContactLoaded)NULL) {
		FOnContactLoaded(totalCount, currentNumber, birthday, displayName, photo);
	}
}
