//---------------------------------------------------------------------------
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
//---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

#ifndef main_uH
#define main_uH
// ---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Permissions.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Edit.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.AddressBook.hpp>
#include <FMX.AddressBook.Types.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.SearchBox.hpp>
#include <FMX.ActnList.hpp>
#include <System.Actions.hpp>
#include <System.UITypes.hpp>
#include <FMX.ListView.Adapters.Base.hpp>
#include <FMX.ListView.Appearances.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.MediaLibrary.Actions.hpp>
#include <FMX.StdActns.hpp>
#include <FMX.Objects.hpp>

// ---------------------------------------------------------------------------
class TForm1 : public TForm {
__published: // IDE-managed Components
	TTabControl *TabControl1;
	TTabItem *TabItemContact;
	TToolBar *ToolBar2;
	TLabel *Label1;
	TButton *btnOK;
	TButton *btnClear;
  	TListBoxGroupHeader *ContactInfoHeader;
	TListBoxItem *FirstName;
	TEdit *edtFirstName;
	TListBoxItem *LastName;
	TEdit *edtLastName;
	TListBoxItem *WorkEmail;
	TEdit *edtWorkMail;
	TListBoxItem *Group;
	TComboBox *ComboBox1;
	TTabItem *TabItemContacts;
	TToolBar *ToolBar1;
	TSpeedButton *btnRemove;
	TSpeedButton *btnRefresh;
	TLabel *Label7;
	TTabItem *TabItemGroup;
	TToolBar *ToolBar3;
	TLabel *Label8;
	TListBox *ListBox1;
	TListBoxGroupHeader *ListBoxGroupHeader1;
	TListBoxItem *EnterGroup;
	TEdit *edtGroupName;
	TButton *btnAddGroup;
	TListBoxItem *SelectGroup;
	TComboBox *ComboBox2;
	TButton *btnRemoveGroup;
	TAddressBook *AddressBook1;
	TActionList *ActionList1;
	TAction *ActionAddContact;
	TAction *ActionRefresh;
	TAction *ActionRemoveGroup;
	TAction *ActionAddGroup;
	TAction *ActionRemoveContact;
	TListBoxGroupHeader *ListBoxGroupHeader2;
	TListView *ListViewContacts;
	TListBoxItem *Photo;
	TButton *btnLoadPicture;
	TTakePhotoFromLibraryAction *TakePhotoFromLibraryAction1;
	TImage *Image1;
	TButton *btnTakePicture;
	TTakePhotoFromCameraAction *TakePhotoFromCameraAction1;

	void __fastcall FormShow(TObject *Sender);
	void __fastcall AddressBook1PermissionRequest(TObject *ASender,
		const UnicodeString AMessage, const bool AAccessGranted);
	void __fastcall btnClearClick(TObject *Sender);
	void __fastcall AddressBook1ExternalChange(TObject *ASender);
	void __fastcall ActionRefreshExecute(TObject *Sender);
	void __fastcall ActionAddContactExecute(TObject *Sender);
	void __fastcall ActionAddGroupExecute(TObject *Sender);
	void __fastcall ActionList1Update(TBasicAction *Action, bool &Handled);
	void __fastcall ActionRemoveContactExecute(TObject *Sender);
	void __fastcall ActionRemoveGroupExecute(TObject *Sender);
	void __fastcall TakePhotoFromLibraryAction1DidFinishTaking(TBitmap *Image);
	void __fastcall TakePhotoFromCameraAction1DidFinishTaking(TBitmap *Image);
	void __fastcall btnTakePictureClick(TObject *Sender);
	void __fastcall btnLoadPictureClick(TObject *Sender);

private: // User declarations
	TAddressBookGroups *FGroups;
	TAddressBookSource *FCurrentSource;

	String FPermissionReadContacts;
	String FPermissionWriteContacts;
	String FPermissionCamera;
	String FPermissionReadExternalStorage;
	String FPermissionWriteExternalStorage;

	void __fastcall FillGroupList(TAddressBookSource *Source);
	void __fastcall FillContactlist(TAddressBookSource *Source);
	bool __fastcall ContactExists(int ID);
	void __fastcall AddListViewItem(TAddressBookContact *Contact);
	void __fastcall DeleteContact(TObject* Sender, TModalResult AKey);
	void __fastcall ClearAddContactForm();

	void __fastcall DisplayRationale(TObject* Sender, const TStringDynArray APermissions, const _di_TProc APostRationaleProc);
	void __fastcall TakePicturePermissionRequestResult(TObject* Sender, const System::TArray__1<String> APermissions, const System::TArray__1<TPermissionStatus> AGrantResults);
	void __fastcall LoadPicturePermissionRequestResult(TObject* Sender, const System::TArray__1<String> APermissions, const System::TArray__1<TPermissionStatus> AGrantResults);

public: // User declarations
	__fastcall TForm1(TComponent* Owner);
};

// ---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
// ---------------------------------------------------------------------------
#endif
