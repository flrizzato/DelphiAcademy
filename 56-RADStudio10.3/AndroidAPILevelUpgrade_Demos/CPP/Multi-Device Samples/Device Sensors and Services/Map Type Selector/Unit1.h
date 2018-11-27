//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Permissions.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <FMX.Maps.hpp>
#include <FMX.Edit.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TToolBar *TopToolBar;
	TToolBar *BottomToolBar;
	TMapView *MapView1;
	TEdit *edLat;
	TEdit *edLong;
	TButton *Button1;
	TSpeedButton *SpeedButton1;
	TSpeedButton *SpeedButton2;
	TSpeedButton *SpeedButton3;
	TLabel *Label1;
	TTrackBar *TrackBar1;
	void __fastcall SpeedButton1Click(TObject *Sender);
	void __fastcall SpeedButton2Click(TObject *Sender);
	void __fastcall SpeedButton3Click(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall MapView1MapClick(const TMapCoordinate &Position);
	void __fastcall TrackBar1Change(TObject *Sender);
private:	// User declarations
    String FPermissionFineLocation;
	void __fastcall DisplayRationale(TObject *Sender, const TStringDynArray APermissions, const _di_TProc APostRationaleProc);
	void __fastcall LocationPermissionRequestResult(TObject *Sender, const System::TArray__1<String>  APermissions, const System::TArray__1<TPermissionStatus> AGrantResults);
public:		// User declarations
	__fastcall TForm1(TComponent *Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
