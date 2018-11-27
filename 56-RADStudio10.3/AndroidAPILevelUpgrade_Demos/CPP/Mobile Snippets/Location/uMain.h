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
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <System.Sensors.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Sensors.hpp>
#include <FMX.WebBrowser.hpp>
#include <System.Sensors.Components.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.EditBox.hpp>
#include <FMX.NumberBox.hpp>
//---------------------------------------------------------------------------
class TForm2 : public TForm
{
__published:	// IDE-managed Components
	TWebBrowser *WebBrowser1;
	TLocationSensor *LocationSensor1;
	TListBox *ListBox1;
	TListBoxItem *lbLocationSensor;
	TSwitch *swLocationSensorActive;
	TListBoxItem *lbTriggerDistance;
	TNumberBox *nbTriggerDistance;
	TButton *Button1;
	TButton *Button2;
	TListBoxItem *lbAccuracy;
	TButton *Button3;
	TButton *Button4;
	TNumberBox *nbAccuracy;
	TListBoxItem *lbLatitude;
	TListBoxItem *lbLongitude;
	TToolBar *ToolBar1;
	TLabel *Label1;
	void __fastcall swLocationSensorActiveSwitch(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall LocationSensor1LocationChanged(TObject *Sender, const TLocationCoord2D &OldLocation, const TLocationCoord2D &NewLocation);
	void __fastcall nbAccuracyChange(TObject *Sender);
	void __fastcall nbTriggerDistanceChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TForm2(TComponent *Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm2 *Form2;
//---------------------------------------------------------------------------
#endif
