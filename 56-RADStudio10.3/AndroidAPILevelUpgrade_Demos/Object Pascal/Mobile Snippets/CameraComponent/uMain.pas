//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Permissions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Media,
  FMX.Platform, FMX.Objects, FMX.Layouts, FMX.Memo, FMX.ListBox, FMX.TabControl, FMX.Controls.Presentation;

type
  TCameraComponentForm = class(TForm)
    CameraComponent: TCameraComponent;
    btnStartCamera: TButton;
    btnStopCamera: TButton;
    lblCameraType: TLabel;
    cbCameraFlashType: TLabel;
    imgCameraView: TImage;
    btnFrontCamera: TSpeedButton;
    btnBackCamera: TSpeedButton;
    btnOn: TSpeedButton;
    btnOff: TSpeedButton;
    btnAuto: TSpeedButton;
    lblCameraResolution: TLabel;
    cbResolutions: TComboBox;
    rCameraType: TRectangle;
    rFlashType: TRectangle;
    rCameraResolution: TRectangle;
    tbControl: TTabControl;
    tiSettings: TTabItem;
    tiPreview: TTabItem;
    pnlCameraResolution: TPanel;
    btnLowQuality: TButton;
    btnHighQuality: TButton;
    btnMediumQuality: TButton;
    btnPhotoQuality: TButton;
    cbPriority: TComboBox;
    lbiResolution: TListBoxItem;
    lbiFrameRate: TListBoxItem;
    lblCurrentResolution: TLabel;
    procedure btnStartCameraClick(Sender: TObject);
    procedure btnStopCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFrontCameraClick(Sender: TObject);
    procedure btnBackCameraClick(Sender: TObject);
    procedure btnOnClick(Sender: TObject);
    procedure btnOffClick(Sender: TObject);
    procedure btnAutoClick(Sender: TObject);
    procedure cbResolutionsChange(Sender: TObject);
    procedure btnLowQualityClick(Sender: TObject);
    procedure btnMediumQualityClick(Sender: TObject);
    procedure btnHighQualityClick(Sender: TObject);
    procedure btnPhotoQualityClick(Sender: TObject);
    procedure cbPriorityChange(Sender: TObject);
    procedure CameraComponentSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
    procedure tbControlChange(Sender: TObject);
  private
    { Private declarations }
    FPermissionCamera: string;
    procedure GetImage;
    procedure AccessCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
    procedure ActivateCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
  public
    { Public declarations }
    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure FillResolutions;
    procedure ShowCurrentResolution;
    procedure ChangeQuality(const ANewQuality: TVideoCaptureQuality);
  end;

var
  CameraComponentForm: TCameraComponentForm;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService;

{$R *.fmx}

procedure TCameraComponentForm.FillResolutions;
var
  LSettings: TArray<TVideoCaptureSetting>;
  I: Integer;
begin
  LSettings := CameraComponent.AvailableCaptureSettings;
  cbResolutions.Clear;
  for I := Low(LSettings) to High(LSettings) do
    cbResolutions.Items.Add(LSettings[I].Width.ToString + ' x ' + LSettings[I].Height.ToString + ' x ' +
      LSettings[I].FrameRate.ToString);
  cbResolutions.ItemIndex := 0;
end;

procedure TCameraComponentForm.FormCreate(Sender: TObject);
var
  AppEventSvc: IFMXApplicationEventService;
begin
{$IFDEF ANDROID}
  FPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
{$ENDIF}
  PermissionsService.RequestPermissions([FPermissionCamera], AccessCameraPermissionRequestResult, DisplayRationale);

  {
    Add platform service to see camera state. This is needed to enable or disable the camera when the application
    goes to background.
  }
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(AppEventSvc)) then
    AppEventSvc.SetApplicationEventHandler(AppEvent);
end;

procedure TCameraComponentForm.AccessCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 1 permission involved: CAMERA
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
    { Fill the resolutions. }
    FillResolutions
  else
    ShowMessage('Cannot access the camera because the required permission has not been granted')
end;

procedure TCameraComponentForm.ActivateCameraPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 1 permission involved: CAMERA
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    { Turn on the Camera }
    CameraComponent.Active := True;
    tbControl.TabIndex := 1;
  end
  else
    ShowMessage('Cannot start the camera because the required permission has not been granted')
end;

// Optional rationale display routine to display permission requirement rationale to the user
procedure TCameraComponentForm.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
  // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
  // After the user sees the explanation, invoke the post-rationale routine to request the permissions
  TDialogService.ShowMessage('The app needs to access the camera in order to work',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc
    end)
end;

{ Make sure the camera is released if you are going away.}

function TCameraComponentForm.AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  Result := True;
  case AAppEvent of
    TApplicationEvent.WillBecomeInactive:
      CameraComponent.Active := False;
    TApplicationEvent.EnteredBackground:
      CameraComponent.Active := False;
    TApplicationEvent.WillTerminate:
      CameraComponent.Active := False;
  end;
end;

procedure TCameraComponentForm.btnBackCameraClick(Sender: TObject);
var
  LActive: Boolean;
begin
  { Select Back Camera }
  LActive := CameraComponent.Active;
  try
    CameraComponent.Active := False;
    CameraComponent.Kind := TCameraKind.BackCamera;
  finally
    CameraComponent.Active := LActive;
    FillResolutions;
  end;
end;

procedure TCameraComponentForm.btnFrontCameraClick(Sender: TObject);
var
  LActive: Boolean;
begin
  { Select Front Camera }
  LActive := CameraComponent.Active;
  try
    CameraComponent.Active := False;
    CameraComponent.Kind := TCameraKind.FrontCamera;
  finally
    CameraComponent.Active := LActive;
    FillResolutions;
  end;
end;

procedure TCameraComponentForm.btnAutoClick(Sender: TObject);
var
  LActive: Boolean;
begin
  { Turn on automatic Torch, if supported }
  if CameraComponent.HasTorch then
  begin
    LActive := CameraComponent.Active;
    try
      CameraComponent.Active := False;
      CameraComponent.TorchMode := TTorchMode.ModeAuto;
    finally
      CameraComponent.Active := LActive;
    end;
  end;
end;

procedure TCameraComponentForm.btnOffClick(Sender: TObject);
var
  LActive: Boolean;
begin
  { Turn off the Torch, if supported }
  if CameraComponent.HasTorch then
  begin
    LActive := CameraComponent.Active;
    try
      CameraComponent.Active := False;
      CameraComponent.TorchMode := TTorchMode.ModeOff;
    finally
      CameraComponent.Active := LActive;
    end;
  end;
end;

procedure TCameraComponentForm.btnOnClick(Sender: TObject);
var
  LActive: Boolean;
begin
  { Turn on the Torch, if supported }
  if CameraComponent.HasTorch then
  begin
    LActive := CameraComponent.Active;
    CameraComponent.Active := False;
    CameraComponent.TorchMode := TTorchMode.ModeOn;
    CameraComponent.Active := LActive;
  end;
end;

procedure TCameraComponentForm.btnStartCameraClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([FPermissionCamera], ActivateCameraPermissionRequestResult, DisplayRationale);
end;

procedure TCameraComponentForm.btnStopCameraClick(Sender: TObject);
begin
  { Turn off the Camera }
  CameraComponent.Active := False;
  tbControl.TabIndex := 0;
end;

procedure TCameraComponentForm.CameraComponentSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin
  TThread.Synchronize(TThread.CurrentThread, GetImage);
//  imgCameraView.Width := imgCameraView.Bitmap.Width;
//  imgCameraView.Height := imgCameraView.Bitmap.Height;
end;

procedure TCameraComponentForm.GetImage;
begin
  CameraComponent.SampleBufferToBitmap(imgCameraView.Bitmap, True);
end;

procedure TCameraComponentForm.btnHighQualityClick(Sender: TObject);
begin
  ChangeQuality(TVideoCaptureQuality.HighQuality);
end;

procedure TCameraComponentForm.btnLowQualityClick(Sender: TObject);
begin
  ChangeQuality(TVideoCaptureQuality.LowQuality);
end;

procedure TCameraComponentForm.btnMediumQualityClick(Sender: TObject);
begin
  ChangeQuality(TVideoCaptureQuality.MediumQuality);
end;

procedure TCameraComponentForm.btnPhotoQualityClick(Sender: TObject);
begin
  ChangeQuality(TVideoCaptureQuality.PhotoQuality);
end;

procedure TCameraComponentForm.cbPriorityChange(Sender: TObject);
begin
  if lbiResolution.IsSelected then
    CameraComponent.CaptureSettingPriority := TVideoCaptureSettingPriority.Resolution;
  if lbiFrameRate.IsSelected then
    CameraComponent.CaptureSettingPriority := TVideoCaptureSettingPriority.FrameRate;
  FillResolutions;
end;

procedure TCameraComponentForm.cbResolutionsChange(Sender: TObject);
var
  LIndex: Integer;
  LSettings: TArray<TVideoCaptureSetting>;
  LActive: Boolean;
begin
  LActive := CameraComponent.Active;
  try
    CameraComponent.Active := False;
    LIndex := cbResolutions.ItemIndex;
    LSettings := CameraComponent.AvailableCaptureSettings;
    if Length(LSettings) > 0 then
      CameraComponent.CaptureSetting := LSettings[LIndex];
  finally
    CameraComponent.Active := LActive;
    ShowCurrentResolution;
  end;
end;

procedure TCameraComponentForm.ChangeQuality(const ANewQuality: TVideoCaptureQuality);
var
  LActive: Boolean;
begin
  LActive := CameraComponent.Active;
  try
    CameraComponent.Active := False;
    CameraComponent.Quality := ANewQuality;
  finally
    CameraComponent.Active := LActive;
    ShowCurrentResolution;
  end;
end;

procedure TCameraComponentForm.ShowCurrentResolution;
var
  LSettings: TVideoCaptureSetting;
  LText: string;
begin
  case CameraComponent.Quality of
    TVideoCaptureQuality.PhotoQuality: LText := 'Photo';
    TVideoCaptureQuality.HighQuality: LText := 'High';
    TVideoCaptureQuality.MediumQuality: LText := 'Medium';
    TVideoCaptureQuality.LowQuality: LText := 'Low';
    TVideoCaptureQuality.CaptureSettings: LText := 'Custom';
  end;
  LSettings := CameraComponent.CaptureSetting;
  lblCurrentResolution.Text := LText + ' - ' + LSettings.Width.ToString + 'x' + LSettings.Height.ToString + ' at ' +
    LSettings.FrameRate.ToString + ' FPS.';
end;

procedure TCameraComponentForm.tbControlChange(Sender: TObject);
begin
  if tbControl.ActiveTab = tiSettings then
    CameraComponent.Active := False
  else
    CameraComponent.Active := True;
end;

end.
