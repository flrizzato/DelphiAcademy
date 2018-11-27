//---------------------------------------------------------------------------

#ifndef ContactFetchingThreadH
#define ContactFetchingThreadH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.AddressBook.Types.hpp>
#include <FMX.AddressBook.hpp>
#include <FMX.Surfaces.hpp>
//---------------------------------------------------------------------------

typedef void __fastcall (__closure *TOnContactLoaded) (const int ATotalCount, const int ANumber, const TDateTime ABirthday,
	UnicodeString ADisplayName, TBitmapSurface* APhoto);

class TFetchContactThread : public TThread
{
private:
	TAddressBookContacts *allContacts;
	int totalCount;
	int currentNumber;
    TBitmapSurface* photo;
    UnicodeString displayName;
	TDateTime birthday;
	TNotifyEvent FOnStart;
	TOnContactLoaded FOnContactLoaded;

protected:
	void __fastcall Execute();
	void __fastcall DoStart();
	void __fastcall DoContactLoaded();

public:
	TFetchContactThread(TAddressBookContacts* AContacts);

	__property TOnContactLoaded OnContactLoaded = {read=FOnContactLoaded, write=FOnContactLoaded};
	__property TNotifyEvent OnStart = {read=FOnStart, write=FOnStart};
};
//---------------------------------------------------------------------------
#endif
