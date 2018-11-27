//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit FlashLightU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Permissions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Effects,
  FMX.Objects, FMX.Layouts, FMX.Media;

type
  TFlashLightForm = class(TForm)
    FlashLight: TImage;
    ImageOn: TImage;
    FlashLightShadow: TShadowEffect;
    Light: TImage;
    ImageOff: TImage;
    ContainerLayout: TLayout;
    Camera: TCameraComponent;
    GlowEffect1: TGlowEffect;
    LayoutButtons: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure ImageOffClick(Sender: TObject);
    procedure ImageOnClick(Sender: TObject);
  private
    FPermissionCamera: string;
    procedure SetFlashlightState(Active: Boolean);
    procedure AccessCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
    procedure ActivateCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
  public
    { Public declarations }
  end;

var
  FlashLightForm: TFlashLightForm;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TFlashLightForm.SetFlashlightState(Active: Boolean);
begin
  if Active then
    Camera.TorchMode := TTorchMode.ModeOn
  else
    Camera.TorchMode := TTorchMode.ModeOff;
end;

procedure TFlashLightForm.AccessCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 1 permission involved: CAMERA
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
    ImageOff.Enabled := Camera.HasFlash
  else
    TDialogService.ShowMessage('Cannot access the camera flashlight because the required permission has not been granted');
end;

procedure TFlashLightForm.ActivateCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 1 permission involved: CAMERA
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    Camera.Active := True;
    ImageOff.Visible := False;
    ImageOn.Visible := True;
    SetFlashlightState(True);
    Light.Visible := True;
  end
  else
    TDialogService.ShowMessage('Cannot access the camera flashlight because the required permission has not been granted');
end;

// Optional rationale display routine to display permission requirement rationale to the user
procedure TFlashLightForm.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
  // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
  // After the user sees the explanation, invoke the post-rationale routine to request the permissions
  TDialogService.ShowMessage('The app needs to access the camera in order to work',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TFlashLightForm.FormCreate(Sender: TObject);
begin
{$IFDEF ANDROID}
  FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
{$ENDIF}
  PermissionsService.RequestPermissions([FPermissionCamera], AccessCameraPermissionRequestResult, DisplayRationale);
end;

procedure TFlashLightForm.ImageOffClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([FPermissionCamera], ActivateCameraPermissionRequestResult, DisplayRationale);
end;

procedure TFlashLightForm.ImageOnClick(Sender: TObject);
begin
  ImageOff.Visible := True;
  ImageOn.Visible := False;
  SetFlashlightState(False);
  Light.Visible := False;
end;

end.
