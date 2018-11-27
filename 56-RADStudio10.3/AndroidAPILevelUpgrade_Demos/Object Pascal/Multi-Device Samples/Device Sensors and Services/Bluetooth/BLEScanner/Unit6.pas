//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Bluetooth, FMX.StdCtrls, System.Bluetooth.Components, System.Permissions,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation;

type
  TForm6 = class(TForm)
    Panel1: TPanel;
    ListBox1: TListBox;
    BluetoothLE1: TBluetoothLE;
    btnStartScan: TButton;
    btnStopScan: TButton;
    Timer1: TTimer;
    ProgressBar1: TProgressBar;
    ListBox2: TListBox;
    procedure btnStartScanClick(Sender: TObject);
    procedure btnStopScanClick(Sender: TObject);
    procedure BluetoothLE1DiscoverLEDevice(const Sender: TObject; const ADevice: TBluetoothLEDevice; Rssi: Integer; const ScanResponse: TScanResponse);
    procedure Timer1Timer(Sender: TObject);
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
    procedure FormShow(Sender: TObject);
    procedure BluetoothLE1ServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Scanning: Boolean;
    ScanningStart: Cardinal;
    FLocationPermission: string;
    procedure RequestPermissionsResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
    procedure StartBLEDiscovery;
    procedure StopBLEDiscovery;
  public
    { Public declarations }
  end;

const
  ScanningTime = 10000; // 10s in msecs

var
  Form6: TForm6;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService;

{$R *.fmx}

procedure TForm6.BluetoothLE1DiscoverLEDevice(const Sender: TObject; const ADevice: TBluetoothLEDevice; Rssi: Integer; const ScanResponse: TScanResponse);
var
  PrevDiscoveredDevicesCount: Integer;
  DiscoveredDevicesCount: Integer;
  DiscoveredDeviceIndex: Integer;
  DiscoveredDevice: TBluetoothLEDevice;
  DiscoveredDeviceName: string;
begin
  PrevDiscoveredDevicesCount := Listbox1.Count;
  DiscoveredDevicesCount := BluetoothLE1.DiscoveredDevices.Count;

  for DiscoveredDeviceIndex := 0 to DiscoveredDevicesCount - 1 do
  begin
    DiscoveredDevice := BluetoothLE1.DiscoveredDevices[DiscoveredDeviceIndex];
    DiscoveredDeviceName := DiscoveredDevice.DeviceName;
    if DiscoveredDeviceName = '' then
      DiscoveredDeviceName := 'Unknown device';
    DiscoveredDeviceName := (DiscoveredDeviceIndex + 1).ToString + ' - ' + DiscoveredDeviceName + ' - ' + DiscoveredDevice.Identifier;

    if DiscoveredDeviceIndex = PrevDiscoveredDevicesCount then
      Listbox1.Items.Add(DiscoveredDeviceName)
    else
      Listbox1.Items[DiscoveredDeviceIndex] := DiscoveredDeviceName;
  end;
end;

procedure TForm6.BluetoothLE1EndDiscoverDevices(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
begin
  if Scanning then
    ProgressBar1.Value := ProgressBar1.Max;
  Timer1.Enabled := False;
  Scanning := False;
end;

procedure TForm6.BluetoothLE1ServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
var
  ServiceIndex: Integer;
  Service: TBluetoothGattService;
  CharacteristicIndex: Integer;
  Characteristic: TBluetoothGattCharacteristic;
begin
  if AServiceList.Count > 0 then
  begin
    for ServiceIndex := 0 to AServiceList.Count - 1 do
    begin
      Service := AServiceList[ServiceIndex];
      Listbox2.Items.Add((ServiceIndex + 1).ToString + ' - ' + Service.UUIDName + ' - ' + Service.UUID.ToString);

      for CharacteristicIndex := 0 to Service.Characteristics.Count - 1 do
      begin
        Characteristic := Service.Characteristics[CharacteristicIndex];
        Listbox2.Items.Add('    - ' + Characteristic.UUIDName + ' - ' + Characteristic.UUID.ToString);
      end;
    end;
  end
  else
    Listbox2.Items.Add('- Access not allowed or no service available');

  Listbox1.Enabled := True;
end;

procedure TForm6.btnStartScanClick(Sender: TObject);
begin
  PermissionsService.RequestPermissions([FLocationPermission], RequestPermissionsResult, DisplayRationale);
end;

procedure TForm6.btnStopScanClick(Sender: TObject);
begin
  StopBLEDiscovery
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
{$IFDEF ANDROID}
  FLocationPermission := JStringToString(TJManifest_permission.JavaClass.ACCESS_COARSE_LOCATION);
{$ENDIF}
end;

procedure TForm6.FormShow(Sender: TObject);
begin
  Scanning := False;
end;

procedure TForm6.ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  btnStopScanClick(Sender);
  Listbox2.Clear;
  Listbox2.Items.Add('- Discovering services -->');
  TThread.CreateAnonymousThread(
    procedure
    begin
      if not BluetoothLE1.DiscoveredDevices[ListBox1.ItemIndex].DiscoverServices then
        TThread.Synchronize(nil,
          procedure
          begin
            Listbox2.Items.Add('- Service discovery not allowed');
            Listbox1.Enabled := True;
          end);
    end).Start;
  Listbox1.Enabled := False;
end;

procedure TForm6.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
  TDialogService.ShowMessage('We need to be given permission to discover BLE devices',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TForm6.RequestPermissionsResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 1 permissions involved: ACCESS_COARSE_LOCATION
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
    StartBLEDiscovery
  else
    TDialogService.ShowMessage('Cannot start BLE scan as the permission has not been granted');
end;

procedure TForm6.StartBLEDiscovery;
begin
  if not Scanning then
  begin
    Listbox1.Clear;
    ScanningStart := TThread.GetTickCount;
    BluetoothLE1.DiscoverDevices(ScanningTime);
    ProgressBar1.Value := 0;
    Timer1.Enabled := True;
    Scanning := True;
  end;
end;

procedure TForm6.StopBLEDiscovery;
begin
  Timer1.Enabled := False;
  Scanning := False;
  BluetoothLE1.CancelDiscovery;
end;

procedure TForm6.Timer1Timer(Sender: TObject);
var
  LElapsed: Cardinal;
begin
  LElapsed := TThread.GetTickCount - ScanningStart;
  ProgressBar1.Value := ProgressBar1.Max * (LElapsed.ToSingle / ScanningTime);
end;

end.
