//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#ifndef FlashLightUH
#define FlashLightUH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <System.Permissions.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Effects.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.Media.hpp>
#include <FMX.Objects.hpp>
#include <FMX.Types.hpp>
//---------------------------------------------------------------------------
class TFlashLightForm : public TForm
{
__published:	// IDE-managed Components
	TLayout *ContainerLayout;
	TImage *Light;
	TGlowEffect *GlowEffect1;
	TImage *FlashLight;
	TShadowEffect *FlashLightShadow;
	TLayout *LayoutButtons;
	TImage *ImageOff;
	TImage *ImageOn;
	TCameraComponent *Camera;
	void __fastcall ImageOffClick(TObject *Sender);
	void __fastcall ImageOnClick(TObject *Sender);
private:	// User declarations
    String FPermissionCamera;
	void __fastcall DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc);
	void __fastcall AccessCameraPermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults);
	void __fastcall ActivateCameraPermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults);
	void __fastcall SetFlashlightState(bool Active);
public:		// User declarations
	__fastcall TFlashLightForm(TComponent *Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFlashLightForm *FlashLightForm;
//---------------------------------------------------------------------------
#endif
