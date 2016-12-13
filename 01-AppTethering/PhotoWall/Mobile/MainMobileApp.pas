//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit MainMobileApp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, System.Actions, FMX.ActnList,
  FMX.StdActns, FMX.MediaLibrary.Actions, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Ani, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Tether.AppProfile, System.Generics.Collections;

type
  TForm49 = class(TForm)
    ToolBarBottom: TToolBar;
    ButtonTakePhotoFromCamera: TButton;
    LbWalls: TListBox;
    AcActions: TActionList;
    TakePhotoFromCameraAction1: TTakePhotoFromCameraAction;
    TakePhotoManager: TTetheringManager;
    TakePhotoAppProfile: TTetheringAppProfile;
    BtRefresh: TButton;
    CalloutPanel1: TCalloutPanel;
    ImCaptured: TImage;
    ToolBarTop: TToolBar;
    Label2: TLabel;
    Label1: TLabel;
    procedure TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
    procedure TakePhotoManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
    procedure FormShow(Sender: TObject);
    procedure BtRefreshClick(Sender: TObject);
    procedure TakePhotoManagerEndManagersDiscovery(const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
    procedure TakePhotoManagerEndProfilesDiscovery(const Sender: TObject; const RemoteProfiles: TTetheringProfileInfoList);
    procedure TakePhotoManagerRemoteManagerShutdown(const Sender: TObject; const ManagerIdentifier: string);
    procedure LbWallsItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
  private
    { Private declarations }
    Connected: Boolean;

    procedure FindWalls;
    function SendImage: Boolean;
    function CheckPhotoWalls: Boolean;
    procedure RefreshList;
  public
    { Public declarations }
  end;

var
  Form49: TForm49;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

function TForm49.SendImage: Boolean;
var
  LStream: TMemoryStream;
begin
  if not ImCaptured.Bitmap.IsEmpty then begin
    if not Connected then
      Connected := TakePhotoAppProfile.Connect(TakePhotoManager.RemoteProfiles[LbWalls.ItemIndex]);
    LStream := TMemoryStream.Create;
    try
      ImCaptured.Bitmap.SaveToStream(LStream);
      Result := TakePhotoAppProfile.SendStream(TakePhotoManager.RemoteProfiles[LbWalls.ItemIndex], 'Photo from mobile', LStream);
    finally
      LStream.Free;
    end;
  end
  else
    ShowMessage('Please, take a photo before sending');
end;

procedure TForm49.BtRefreshClick(Sender: TObject);
begin
  FindWalls;
end;

function TForm49.CheckPhotoWalls: Boolean;
begin
  if LbWalls.ItemIndex >= 0 then
    Result := True
  else
  begin
    Result := False;
    ShowMessage('Please, select a PhotoWall to send the image');
  end;
end;

procedure TForm49.RefreshList;
var
  I: Integer;
begin
  LbWalls.Clear;
  for I := 0 to TakePhotoManager.RemoteProfiles.Count - 1 do
    if (TakePhotoManager.RemoteProfiles[I].ProfileText = 'MediaReceiverApp') or (TakePhotoManager.RemoteProfiles[I].ProfileText = 'VCLMediaReceiverApp') then
      LbWalls.Items.Add(TakePhotoManager.RemoteProfiles[I].ProfileText);
  if LbWalls.Count > 0 then
  begin
    LbWalls.ItemIndex := 0;
    Connected := TakePhotoAppProfile.Connect(TakePhotoManager.RemoteProfiles[0]);
  end;
  // Connect to the first one
end;

procedure TForm49.FindWalls;
var
  I: Integer;
begin
  LbWalls.Clear;
  for I := TakePhotoManager.PairedManagers.Count - 1 downto 0 do
    TakePhotoManager.UnPairManager(TakePhotoManager.PairedManagers[I]);
  TakePhotoManager.DiscoverManagers;
end;

procedure TForm49.FormShow(Sender: TObject);
begin
  FindWalls;
end;

procedure TForm49.LbWallsItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  if CheckPhotoWalls then
  begin
    TakePhotoAppProfile.Connect(TakePhotoManager.RemoteProfiles[LbWalls.ItemIndex]);
    LbWalls.Enabled := False;
    try
      SendImage;
    finally
      LbWalls.Enabled := True;
    end;
  end;
end;

procedure TForm49.TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
begin
  ImCaptured.Bitmap.Assign(Image);
  if CheckPhotoWalls then
    SendImage;
end;

procedure TForm49.TakePhotoManagerEndManagersDiscovery(const Sender: TObject; const RemoteManagers: TTetheringManagerInfoList);
var
  I: Integer;
begin
  for I := 0 to RemoteManagers.Count - 1 do
    if (RemoteManagers[I].ManagerText = 'MediaReceiverManager') or (RemoteManagers[I].ManagerText = 'VCLMediaReceiver') then
      TakePhotoManager.PairManager(RemoteManagers[I]);
end;

procedure TForm49.TakePhotoManagerEndProfilesDiscovery(const Sender: TObject; const RemoteProfiles: TTetheringProfileInfoList);
begin
  RefreshList;
end;

procedure TForm49.TakePhotoManagerRemoteManagerShutdown(const Sender: TObject; const ManagerIdentifier: string);
begin
  RefreshList;
end;

procedure TForm49.TakePhotoManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
begin
  Password := '1234';
end;

end.
