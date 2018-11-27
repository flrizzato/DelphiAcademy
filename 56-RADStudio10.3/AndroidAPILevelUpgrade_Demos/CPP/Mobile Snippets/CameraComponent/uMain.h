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
#include <FMX.Controls.Presentation.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.Media.hpp>
#include <FMX.Objects.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.TabControl.hpp>
#include <FMX.Types.hpp>
//---------------------------------------------------------------------------
class TCameraComponentForm : public TForm
{
__published:	// IDE-managed Components
	TButton *btnStartCamera;
	TButton *btnStopCamera;
	TTabControl *tbControl;
	TTabItem *tiSettings;
	TRectangle *rCameraType;
	TLabel *lblCameraType;
	TSpeedButton *btnBackCamera;
	TSpeedButton *btnFrontCamera;
	TRectangle *rFlashType;
	TLabel *cbCameraFlashType;
	TSpeedButton *btnAuto;
	TSpeedButton *btnOff;
	TSpeedButton *btnOn;
	TRectangle *rCameraResolution;
	TLabel *lblCameraResolution;
	TComboBox *cbResolutions;
	TPanel *pnlCameraResolution;
	TButton *btnLowQuality;
	TButton *btnHighQuality;
	TButton *btnMediumQuality;
	TButton *btnPhotoQuality;
	TComboBox *cbPriority;
	TListBoxItem *lbiResolution;
	TListBoxItem *lbiFrameRate;
	TLabel *lblCurrentResolution;
	TTabItem *tiPreview;
	TImage *imgCameraView;
	TCameraComponent *CameraComponent;
	void __fastcall btnStartCameraClick(TObject *Sender);
	void __fastcall btnStopCameraClick(TObject *Sender);
	void __fastcall btnFrontCameraClick(TObject *Sender);
	void __fastcall btnBackCameraClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall cbResolutionsChange(TObject *Sender);
	void __fastcall cbPriorityChange(TObject *Sender);
	void __fastcall btnLowQualityClick(TObject *Sender);
	void __fastcall btnMediumQualityClick(TObject *Sender);
	void __fastcall btnHighQualityClick(TObject *Sender);
	void __fastcall btnPhotoQualityClick(TObject *Sender);
	void __fastcall btnOnClick(TObject *Sender);
	void __fastcall btnOffClick(TObject *Sender);
	void __fastcall btnAutoClick(TObject *Sender);
	void __fastcall CameraComponentSampleBufferReady(TObject *Sender, const TMediaTime ATime);
	void __fastcall tbControlChange(TObject *Sender);
private:	// User declarations
    String FPermissionCamera;
	void __fastcall DisplayRationale(TObject* Sender, const TStringDynArray APermissions, const _di_TProc APostRationaleProc);
	void __fastcall AccessCameraPermissionRequestResult(TObject* Sender, const System::TArray__1<String> APermissions, const System::TArray__1<TPermissionStatus> AGrantResults);
	void __fastcall ActivateCameraPermissionRequestResult(TObject* Sender, const System::TArray__1<String> APermissions, const System::TArray__1<TPermissionStatus> AGrantResults);
	void __fastcall GetImage();
    bool __fastcall AppEvent(TApplicationEvent AAppEvent, System::TObject* AContext);
	void FillResolutions();
	void ChangeQuality(const TVideoCaptureQuality ANewQuality);
	void ShowCurrentResolution();
public:		// User declarations
	__fastcall TCameraComponentForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TCameraComponentForm *CameraComponentForm;
//---------------------------------------------------------------------------
#endif
