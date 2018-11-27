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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Media, FMX.StdCtrls,
  FMX.Objects, System.Actions, FMX.ActnList, FMX.Controls.Presentation;

const
{$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  AUDIO_FILENAME = 'test.caf';
{$ELSE}
  AUDIO_FILENAME = 'test.wav';
{$ENDIF}

type
  TAudioRecPlayForm = class(TForm)
    btnStartRec: TButton;
    btnStopRec: TButton;
    btnStartPlay: TButton;
    btnStopPlay: TButton;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ToolBar2: TToolBar;
    imgOff: TImage;
    imgOn: TImage;
    MediaPlayer: TMediaPlayer;
    ActionList: TActionList;
    actStartRecording: TAction;
    actStopRecording: TAction;
    actPlay: TAction;
    actStop: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actStartRecordingExecute(Sender: TObject);
    procedure actStopRecordingExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actPlayExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure imgOnClick(Sender: TObject);
    procedure imgOffClick(Sender: TObject);
  private
    FMicrophone: TAudioCaptureDevice;
    FPermission: string;
    function HasMicrophone: Boolean;
    function IsMicrophoneRecording: Boolean;
    procedure RequestPermissionsResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
  end;

var
  AudioRecPlayForm: TAudioRecPlayForm;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
{$ENDIF}
  System.IOUtils, FMX.DialogService;

{$R *.fmx}

{ GetAudioFileName resolves the audio file path for either platform. }

function GetAudioFileName(const AFileName: string): string;
begin
{$IFDEF ANDROID}
  Result := TPath.GetTempPath + '/' + AFileName;
{$ELSE}
  {$IFDEF IOS}
    Result := TPath.GetHomePath + '/Documents/' + AFileName;
  {$ELSE}
    Result := TPath.Combine(TPath.GetTempPath, AFileName);
  {$ENDIF}
{$ENDIF}
end;

procedure TAudioRecPlayForm.ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  { Provide feedback on capture on/off}

  imgOn.Visible := HasMicrophone and (FMicrophone.State = TCaptureDeviceState.Capturing);

  { ... and enable buttons accordingly }

  actStartRecording.Enabled := not IsMicrophoneRecording and HasMicrophone;
  actStopRecording.Enabled := IsMicrophoneRecording;
  actStop.Enabled := Assigned(MediaPlayer.Media) and (MediaPlayer.State = TMediaState.Playing);
  actPlay.Enabled := FileExists(GetAudioFileName(AUDIO_FILENAME)) and (MediaPlayer.State <> TMediaState.Playing);
end;

procedure TAudioRecPlayForm.actPlayExecute(Sender: TObject);
begin
  if IsMicrophoneRecording then
    actStopRecording.Execute;

  MediaPlayer.FileName := GetAudioFileName(AUDIO_FILENAME);
  MediaPlayer.Play;
end;

procedure TAudioRecPlayForm.actStartRecordingExecute(Sender: TObject);
begin
  actStop.Execute;

  { get the microphone device }
  FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
  if HasMicrophone then
  begin
    { and attempt to record to 'test.caf' file }
    FMicrophone.FileName := GetAudioFileName(AUDIO_FILENAME);
    PermissionsService.RequestPermissions([FPermission], RequestPermissionsResult, DisplayRationale);
  end
  else
    TDialogService.ShowMessage('No microphone is available.');
end;

// Optional rationale display routine to display permission requirement rationale to the user
procedure TAudioRecPlayForm.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
  // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
  // After the user sees the explanation, invoke the post-rationale routine to request the permissions
  TDialogService.ShowMessage('We need to be given permission to record some audio with your microphone',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TAudioRecPlayForm.RequestPermissionsResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  if (Length(AGrantResults) = 1) then
  begin
    case AGrantResults[0] of
      TPermissionStatus.Granted:
        try
          FMicrophone.StartCapture;
        except
          TDialogService.ShowMessage('StartCapture: Operation not supported by this device');
        end;
      TPermissionStatus.Denied: TDialogService.ShowMessage('Cannot record audio without the relevant permission being granted');
      TPermissionStatus.PermanentlyDenied: TDialogService.ShowMessage('If you decide you wish to use the audio recording feature of this app, please go to app settings and enable the microphone permission');
    end;
  end
  else
    TDialogService.ShowMessage('Something went wrong with the permission checking');
end;

procedure TAudioRecPlayForm.actStopExecute(Sender: TObject);
begin
  MediaPlayer.Stop;
end;

procedure TAudioRecPlayForm.actStopRecordingExecute(Sender: TObject);
begin
  { stop capturing audio from the microphone }
  if IsMicrophoneRecording then
    try
      FMicrophone.StopCapture;
    except
      TDialogService.ShowMessage('Get state: Operation not supported by this device');
    end;
end;

procedure TAudioRecPlayForm.imgOffClick(Sender: TObject);
begin
  { we want the same functionality as clicking the recording button }
  actStartRecording.Execute;
end;

procedure TAudioRecPlayForm.imgOnClick(Sender: TObject);
begin
  actStopRecording.Execute;
end;

procedure TAudioRecPlayForm.FormCreate(Sender: TObject);
begin
{$IFDEF ANDROID}
  FPermission := JStringToString(TJManifest_permission.JavaClass.RECORD_AUDIO);
{$ENDIF}
  FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
end;

function TAudioRecPlayForm.HasMicrophone: Boolean;
begin
  Result := Assigned(FMicrophone);
end;

function TAudioRecPlayForm.IsMicrophoneRecording: Boolean;
begin
  Result := HasMicrophone and (FMicrophone.State = TCaptureDeviceState.Capturing);
end;

end.
