//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Permissions.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Edit.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.PhoneDialer.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TPhoneDialerForm : public TForm
{
__published:	// IDE-managed Components
	TButton *btnGetCarrierInfo;
	TButton *btnMakeCall;
	TEdit *edtTelephoneNumber;
	TLabel *lblTelephoneNumber;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TListBox *ListBox1;
	TListBoxItem *CarrierNameItem;
	TListBoxItem *CountryCodeItem;
	TListBoxItem *NetworkCodeItem;
	TListBoxItem *MobileNetworkItem;
	void __fastcall btnGetCarrierInfoClick(TObject *Sender);
	void __fastcall btnMakeCallClick(TObject *Sender);
private:	// User declarations
    String FCallPhonePermission;
    _di_IFMXPhoneDialerService FPhoneDialerService;
	void __fastcall DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc);
	void __fastcall MakePhoneCallPermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults);
public:		// User declarations
	__fastcall TPhoneDialerForm(TComponent *Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TPhoneDialerForm *PhoneDialerForm;
//---------------------------------------------------------------------------
#endif
