//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef uSensorInfoH
#define uSensorInfoH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Permissions.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Sensors.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TfrmAboutSensors : public TForm
{
__published:	// IDE-managed Components
	TListBox *lbMain;
	TTimer *Timer1;
	TLabel *lInfo;
	TButton *btnHide;
	void __fastcall Timer1Timer(TObject *Sender);
	void __fastcall btnHideClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall lbMainItemClick(const TCustomListBox *Sender, const TListBoxItem *Item);
	void __fastcall FormResize(TObject *Sender);
private:	// User declarations
	const int cBorder;
	const String cND;
	const String cForm;
	const String cFormS;

	TCustomSensor *FActiveSensor;
	bool FShowInfo;
	bool FOnOneScreen;
	void __fastcall ReAlignComponents();
	void __fastcall CreateIfExists(TSensorCategory ASensorCategory);
	String __fastcall GetSensorCategoryName(TSensorCategory ASensorCategory);
	void __fastcall ListBoxItemClick(TObject *Sender);
	String __fastcall GetFullInfo(String ACategory, String AType, String AClass, String AAvailableProperties);
	String __fastcall GetInfoAboutLocation(TCustomSensor *ASensor);
	String __fastcall GetInfoAboutEnv(TCustomSensor *ASensor);
	String __fastcall GetInfoAboutMotion(TCustomSensor *ASensor);
	String __fastcall GetInfoAboutOrientation(TCustomSensor *ASensor);
	String __fastcall GetInfoAboutMechanical(TCustomSensor *ASensor);
	String __fastcall GetInfoAboutElectro(TCustomSensor *ASensor);
	String __fastcall GetInfoAboutBiometric(TCustomSensor *ASensor);
	String __fastcall GetInfoAboutLight(TCustomSensor *ASensor);
	String __fastcall GetInfoAboutScanner(TCustomSensor *ASensor);
	String __fastcall GetTypeNameLocation(TLocationSensorType AType);
	String __fastcall GetTypeNameEnv(TEnvironmentalSensorType AType);
	String __fastcall GetTypeNameMotion(TMotionSensorType AType);
	String __fastcall GetTypeNameOrientation(TOrientationSensorType AType);
	String __fastcall GetTypeNameMech(TMechanicalSensorType AType);
	String __fastcall GetTypeNameElectro(TElectricalSensorType AType);
	String __fastcall GetTypeNameBio(TBiometricSensorType AType);
	String __fastcall GetTypeNameLight(TLightSensorType AType);
	String __fastcall GetTypeNameScanner(TScannerSensorType AType);
	String __fastcall GetSensorType(TCustomSensor *ASensor);
	String __fastcall ToFormStr(String AProp, double AVal);
	String __fastcall ToFormStrS(String AProp, String AVal);
	String __fastcall ToFormStrB(String AProp, bool AVal);
public:		// User declarations
	__fastcall TfrmAboutSensors(TComponent *Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmAboutSensors *frmAboutSensors;
//---------------------------------------------------------------------------
#endif
