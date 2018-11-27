//---------------------------------------------------------------------------

#ifndef MainFrmH
#define MainFrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.AddressBook.hpp>
#include <FMX.AddressBook.Types.hpp>
#include <FMX.Ani.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ImgList.hpp>
#include <FMX.ListView.Adapters.Base.hpp>
#include <FMX.ListView.Appearances.hpp>
#include <FMX.ListView.hpp>
#include <FMX.ListView.Types.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.ImageList.hpp>
#include <System.Notification.hpp>
#include "ContactFetchingThread.h"
//---------------------------------------------------------------------------
class TFormMain : public TForm
{
__published:	// IDE-managed Components
	TListView *ListView1;
	TToolBar *ToolBar1;
	TLabel *LabelTitle;
	TSpeedButton *SpeedButton1;
	TRectangle *ProgressPanel;
	TProgressBar *ProgressBar;
	TLabel *LabelProgress;
	TFloatAnimation *ProgressPanelAnimation;
	TNotificationCenter *NotificationCenter1;
	TImageList *ImageList1;
	TAddressBook *AddressBook;
	void __fastcall AddressBookExternalChange(TObject *ASender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall ListView1PullRefresh(TObject *Sender);
	void __fastcall SpeedButton1Click(TObject *Sender);
	void __fastcall AddressBookPermissionRequest(TObject *ASender, const UnicodeString AMessage, const bool AAccessGranted);
    void __fastcall FormDestroy(TObject *Sender);
private:
	TAddressBookContacts* contacts;
	TFetchContactThread* thread;
	void FillContactsList();
	TBitmap* CreateRoundPhoto(TBitmapSurface* source);
	void PostNotification(const UnicodeString displayName, const TDateTime birthday, const int remainderDays);
	int DefineRemainedDays(const TDate birthday);
	TDateTime ChangeYear(const TDateTime date, const unsigned short newYear);

	String FPermissionReadContacts;

	void __fastcall DisplayRationale(TObject* Sender, const TStringDynArray APermissions, const _di_TProc APostRationaleProc);

	/* TFetchContactThread events handlers */
	void __fastcall ContactLoadingBegin(TObject *Sender);
	void __fastcall ContactLoaded(const int totalCount, const int number, const TDateTime birthday, const UnicodeString displayName,
	  TBitmapSurface* photo);
	void __fastcall ContactLoadingEnd(TObject* Sender);
public:
	__fastcall TFormMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormMain *FormMain;
//---------------------------------------------------------------------------
#endif
