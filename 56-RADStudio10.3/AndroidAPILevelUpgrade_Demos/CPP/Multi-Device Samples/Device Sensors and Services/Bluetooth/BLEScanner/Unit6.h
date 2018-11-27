//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef Unit6H
#define Unit6H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Permissions.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Bluetooth.Components.hpp>
#include <System.Bluetooth.hpp>
//---------------------------------------------------------------------------
class TForm6 : public TForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TListBox *ListBox1;
    TButton *btnStartScan;
    TButton *btnStopScan;
	TProgressBar *ProgressBar1;
	TListBox *ListBox2;
	TTimer *Timer1;
	TBluetoothLE *BluetoothLE1;
	void __fastcall btnStartScanClick(TObject *Sender);
	void __fastcall btnStopScanClick(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall ListBox1ItemClick(TCustomListBox * const Sender, TListBoxItem * const Item);
	void __fastcall BluetoothLE1DiscoverLEDevice(TObject * const Sender, TBluetoothLEDevice * const ADevice,
          int Rssi, TScanResponse * const ScanResponse);
	void __fastcall BluetoothLE1EndDiscoverDevices(TObject * const Sender, TBluetoothLEDeviceList * const ADeviceList);
	void __fastcall BluetoothLE1ServicesDiscovered(TObject * const Sender, TBluetoothGattServiceList * const AServiceList);
private:	// User declarations
    String FLocationPermission;
	bool Scanning;
	DWORD ScanningStart;
    void StartBLEDiscovery();
    void StopBLEDiscovery();
    void __fastcall DisplayRationale(TObject* Sender, const TStringDynArray APermissions, const _di_TProc APostRationaleProc);
    void __fastcall RequestPermissionsResult(TObject* Sender, const System::TArray__1<String> APermissions, const System::TArray__1<TPermissionStatus> AGrantResults);
public:		// User declarations
	__fastcall TForm6(TComponent* Owner);
};

const int ScanningTime = 10000;	//10s in msecs.
//---------------------------------------------------------------------------
extern PACKAGE TForm6 *Form6;
//---------------------------------------------------------------------------
#endif
