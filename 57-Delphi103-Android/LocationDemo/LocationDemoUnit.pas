// ---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

// ---------------------------------------------------------------------------

unit LocationDemoUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.WebBrowser, System.Sensors,
  System.Sensors.Components, FMX.MultiView, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    WebBrowser1: TWebBrowser;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    Switch1: TSwitch;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItemLatitude: TListBoxItem;
    ListBoxItemLongitude: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItemAdminArea: TListBoxItem;
    ListBoxItemCountryCode: TListBoxItem;
    ListBoxItemCountryName: TListBoxItem;
    ListBoxItemFeatureName: TListBoxItem;
    ListBoxItemLocality: TListBoxItem;
    ListBoxItemPostalCode: TListBoxItem;
    ListBoxItemSubAdminArea: TListBoxItem;
    ListBoxItemSubLocality: TListBoxItem;
    ListBoxItemSubThoroughfare: TListBoxItem;
    ListBoxItemThoroughfare: TListBoxItem;
    Label1: TLabel;
    LocationSensor1: TLocationSensor;
    MultiView1: TMultiView;
    ToolBar1: TToolBar;
    Button1: TButton;
    ToolBar2: TToolBar;
    Button2: TButton;
    Layout1: TLayout;
    procedure LocationSensor1LocationChanged(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
    procedure Switch1Switch(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FGeocoder: TGeocoder;
    procedure OnGeocodeReverseEvent(const Address: TCivicAddress);
    procedure RequestPermissions;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.Jni,
  Androidapi.Jni.Os,
  Androidapi.Jni.Support,
  Androidapi.Jni.GraphicsContentViewText,
  Androidapi.Jni.Widget,
  Androidapi.Jni.JavaTypes,
  Androidapi.JniBridge,
  Androidapi.Helpers,
  FMX.Helpers.Android,
  FMX.DialogService.Async;

const
  MY_PERMISSIONS_REQUEST_ACCESS_LOCATION = 1;

procedure TForm1.Switch1Switch(Sender: TObject);
begin
  // Using helper class from Android Support Library, so no OS version checking required
  if (TJContextCompat.JavaClass.checkSelfPermission(TAndroidHelper.Activity,
    TJManifest_permission.JavaClass.ACCESS_COARSE_LOCATION) <>
    TJPackageManager.JavaClass.PERMISSION_GRANTED) and
    (TJContextCompat.JavaClass.checkSelfPermission(TAndroidHelper.Activity,
    TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION) <>
    TJPackageManager.JavaClass.PERMISSION_GRANTED) then
  begin
    // Permissions are not granted.
    // Should we show an explanation?
    if TJActivityCompat.JavaClass.shouldShowRequestPermissionRationale
      (TAndroidHelper.Activity,
      TJManifest_permission.JavaClass.ACCESS_COARSE_LOCATION) or
      TJActivityCompat.JavaClass.shouldShowRequestPermissionRationale
      (TAndroidHelper.Activity,
      TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION) then
    begin
      // Show an explanation to the user *asynchronously* -- don't block this thread waiting for the user's response!
      // After the user sees and dismissed the explanation, try again to request the permission.
      TDialogServiceAsync.ShowMessage
        ('The device location allows this app to show where you are on the map',
        procedure(const AResult: TModalResult)
        begin
          RequestPermissions
        end);
    end
    else // No explanation needed; request the permission
      RequestPermissions;
  end
  else // Permission has already been granted
    LocationSensor1.Active := Switch1.IsChecked;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MultiView1.HideMaster;
end;

procedure TForm1.LocationSensor1LocationChanged(Sender: TObject;
const OldLocation, NewLocation: TLocationCoord2D);
var
  URLString: String;
  LSettings: TFormatSettings;
  LDecSeparator: Char;
begin
  LDecSeparator := FormatSettings.DecimalSeparator;
  LSettings := FormatSettings;
  try
    FormatSettings.DecimalSeparator := '.';
    // Show current location
    ListBoxItemLatitude.ItemData.Detail :=
      Format('%2.6f', [NewLocation.Latitude]);
    ListBoxItemLongitude.ItemData.Detail :=
      Format('%2.6f', [NewLocation.Longitude]);

    // Show Map using Google Maps
    URLString := Format('https://maps.google.com/maps?q=%2.6f,%2.6f',
      [NewLocation.Latitude, NewLocation.Longitude]);
  finally
    FormatSettings.DecimalSeparator := LDecSeparator;
  end;
  WebBrowser1.Navigate(URLString);

  // Setup an instance of TGeocoder
  try
    if not Assigned(FGeocoder) then
    begin
      if Assigned(TGeocoder.Current) then
        FGeocoder := TGeocoder.Current.Create;
      if Assigned(FGeocoder) then
        FGeocoder.OnGeocodeReverse := OnGeocodeReverseEvent;
    end;
  except
    ListBoxGroupHeader1.Text := 'Geocoder service error.';
  end;

  // Translate location to address
  if Assigned(FGeocoder) and not FGeocoder.Geocoding then
    FGeocoder.GeocodeReverse(NewLocation);
end;

procedure TForm1.OnGeocodeReverseEvent(const Address: TCivicAddress);
begin
  ListBoxItemAdminArea.ItemData.Detail := Address.AdminArea;
  ListBoxItemCountryCode.ItemData.Detail := Address.CountryCode;
  ListBoxItemCountryName.ItemData.Detail := Address.CountryName;
  ListBoxItemFeatureName.ItemData.Detail := Address.FeatureName;
  ListBoxItemLocality.ItemData.Detail := Address.Locality;
  ListBoxItemPostalCode.ItemData.Detail := Address.PostalCode;
  ListBoxItemSubAdminArea.ItemData.Detail := Address.SubAdminArea;
  ListBoxItemSubLocality.ItemData.Detail := Address.SubLocality;
  ListBoxItemSubThoroughfare.ItemData.Detail := Address.SubThoroughfare;
  ListBoxItemThoroughfare.ItemData.Detail := Address.Thoroughfare;
end;

procedure TForm1.RequestPermissions;
begin
  var
  Perms := TJavaObjectArray<JString>.Create(2);
  Perms[0] := TJManifest_permission.JavaClass.ACCESS_COARSE_LOCATION;
  Perms[1] := TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION;
  // Using helper class from Android Support Library, so no OS version checking required
  TJActivityCompat.JavaClass.RequestPermissions(TAndroidHelper.Activity, Perms,
    MY_PERMISSIONS_REQUEST_ACCESS_LOCATION);
  // MY_PERMISSIONS_REQUEST_ACCESS_LOCATION is an app-defined int constant.
  // The callback method, OnPermissionsRequest, gets the result of the request.
end;

end.
