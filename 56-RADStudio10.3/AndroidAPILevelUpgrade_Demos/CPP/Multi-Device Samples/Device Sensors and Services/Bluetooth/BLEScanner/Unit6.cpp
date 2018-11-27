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
#include <fmx.h>
#ifdef __ANDROID__
    #include <Androidapi.Helpers.hpp>
    #include <Androidapi.JNI.Os.hpp>
#endif
#include <FMX.DialogService.hpp>
#pragma hdrstop

#include "Unit6.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm6 *Form6;

class TTPDiscoverServices : public TCppInterfacedObject<TProc>
{
  public:
	TTPDiscoverServices(TForm6 *AForm) { FForm = AForm; }
	void __fastcall Invoke();
  private:
	TForm6 *FForm;
	void __fastcall SynchronizeProc();
};

void __fastcall TTPDiscoverServices::SynchronizeProc(void)
{
	FForm->ListBox2->Items->Add("- Service discovery not allowed");
	FForm->ListBox1->Enabled = true;
}

void __fastcall TTPDiscoverServices::Invoke(void)
{
	if(!FForm->BluetoothLE1->DiscoveredDevices->Items[FForm->ListBox1->ItemIndex]->DiscoverServices())
		TThread::Synchronize(NULL, SynchronizeProc);
}

//---------------------------------------------------------------------------
__fastcall TForm6::TForm6(TComponent* Owner)
	: TForm(Owner)
{
#ifdef __ANDROID__
    FLocationPermission = JStringToString(TJManifest_permission::JavaClass->ACCESS_COARSE_LOCATION);
#endif
}
//---------------------------------------------------------------------------
void __fastcall TForm6::DisplayRationale(TObject* Sender, const TStringDynArray APermissions, const _di_TProc APostRationaleProc)
{
    // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
    // After the user sees the explanation, invoke the post-rationale routine to request the permissions
    TDialogService::ShowMessage("We need to be given permission to discover BLE devices",
        [APostRationaleProc](TModalResult AKey)
        {
            APostRationaleProc->Invoke();
        });
}
//---------------------------------------------------------------------------
void __fastcall TForm6::RequestPermissionsResult(TObject* Sender, const System::TArray__1<String> APermissions, const System::TArray__1<TPermissionStatus> AGrantResults)
{
    // 1 permission involved: ACCESS_COARSE_LOCATION
	if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted))
		StartBLEDiscovery();
	else
		ShowMessage("Cannot start BLE scan as the permission was not granted");
}
//---------------------------------------------------------------------------
void __fastcall TForm6::btnStartScanClick(TObject *Sender)
{
    TStringDynArray perms;
    perms.set_length(1);
    perms[0] = FLocationPermission;
    PermissionsService()->RequestPermissions(perms, RequestPermissionsResult, DisplayRationale);
}
//---------------------------------------------------------------------------
void __fastcall TForm6::btnStopScanClick(TObject *Sender)
{
    StopBLEDiscovery();
}
//---------------------------------------------------------------------------
void TForm6::StartBLEDiscovery()
{
	if(!Scanning) {
		ListBox1->Clear();
		ScanningStart = TThread::GetTickCount();
		BluetoothLE1->DiscoverDevices(ScanningTime);
		ProgressBar1->Value = 0;
		Timer1->Enabled = true;
		Scanning = true;
	}
}
//---------------------------------------------------------------------------
void TForm6::StopBLEDiscovery()
{
	Timer1->Enabled = false;
	Scanning = false;
	BluetoothLE1->CancelDiscovery();
}
//---------------------------------------------------------------------------
void __fastcall TForm6::FormShow(TObject *Sender)
{
	Scanning = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::Timer1Timer(TObject *Sender)
{
	DWORD LElapsed = TThread::GetTickCount() - ScanningStart;
	ProgressBar1->Value = ProgressBar1->Max * float(LElapsed)/ScanningTime;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::ListBox1ItemClick(TCustomListBox * const Sender, TListBoxItem * const Item)
{
	btnStopScanClick(btnStopScan);
	ListBox2->Clear();
	ListBox2->Items->Add("- Discovering services -->");

	TThread::CreateAnonymousThread(new TTPDiscoverServices(this))->Start();

	ListBox1->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TForm6::BluetoothLE1DiscoverLEDevice(TObject * const Sender, TBluetoothLEDevice * const ADevice,
		  int Rssi, TScanResponse * const ScanResponse)
{
	int dCount = BluetoothLE1->DiscoveredDevices->Count;
	int numOfDevices = ListBox1->Count;
	String Name;
	for (int i = 0; i < dCount; i++) {
		Name = BluetoothLE1->DiscoveredDevices->Items[i]->DeviceName;
		if(Name == "")
			Name = "Unknown device";
		Name = IntToStr(i+1) + L" - " + Name + L" - " + BluetoothLE1->DiscoveredDevices->Items[i]->Identifier();
		if(numOfDevices == i) {
			 ListBox1->Items->Add(Name);
		}
		else {
			ListBox1->Items->Strings[i] = Name;
		}
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm6::BluetoothLE1EndDiscoverDevices(TObject * const Sender, TBluetoothLEDeviceList * const ADeviceList)
{
	if (Scanning) {
		ProgressBar1->Value = ProgressBar1->Max;
	}
	Timer1->Enabled = false;
	Scanning = false;
}
//---------------------------------------------------------------------------

void __fastcall TForm6::BluetoothLE1ServicesDiscovered(TObject * const Sender, TBluetoothGattServiceList * const AServiceList)
{
	String aux;
	if (AServiceList->Count > 0) {
		for (int i = 0; i < AServiceList->Count; i++) {
			TBluetoothGattService *service = AServiceList->Items[i];
			aux = IntToStr(i+1) + L" - " + service->UUIDName + L" - " + GUIDToString(service->UUID);
			ListBox2->Items->Add(aux);
			for (int c = 0; c < service->Characteristics->Count; c++) {
				TBluetoothGattCharacteristic *chara = service->Characteristics->Items[c];
				aux = L"    - " + chara->UUIDName + L" - " + GUIDToString(chara->UUID);
				ListBox2->Items->Add(aux);
			}
        }
	}
	else {
		ListBox2->Items->Add("- Access not allowed or no services");
	}
	ListBox1->Enabled = true;
}
//---------------------------------------------------------------------------

