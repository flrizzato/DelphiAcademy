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
#include <FMX.ActnList.hpp>
#include <FMX.MediaLibrary.Actions.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdActns.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.Types.hpp>
#include <System.Actions.hpp>
#include <FMX.Controls.Presentation.hpp>
//---------------------------------------------------------------------------
class TCameraRollForm : public TForm
{
__published:	// IDE-managed Components
	TImage *imgPhotoLibraryImage;
	TToolBar *ToolBar1;
	TLabel *Label1;
	TButton *btnPhotoLibrary;
	TActionList *alGetCameraRoll;
	TTakePhotoFromLibraryAction *TakePhotoFromLibraryAction1;
	void __fastcall TakePhotoFromLibraryAction1DidFinishTaking(TBitmap *Image);
    void __fastcall btnPhotoLibraryClick(TObject *Sender);
private:	// User declarations
    String FPermissionReadExternalStorage;
    void __fastcall DisplayRationale(TObject *Sender, const DynamicArray<String> APermissions, const _di_TProc APostRationaleProc);
    void __fastcall LoadPicturePermissionRequestResult(TObject *Sender, const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults);
public:		// User declarations
	__fastcall TCameraRollForm(TComponent *Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCameraRollForm *CameraRollForm;
//---------------------------------------------------------------------------
#endif
