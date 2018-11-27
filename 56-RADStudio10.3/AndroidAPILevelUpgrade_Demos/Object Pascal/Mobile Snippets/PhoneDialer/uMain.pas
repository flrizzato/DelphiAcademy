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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.PhoneDialer,
  FMX.Platform, FMX.Edit, FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation;

type
  TPhoneDialerForm = class(TForm)
    btnGetCarrierInfo: TButton;
    btnMakeCall: TButton;
    edtTelephoneNumber: TEdit;
    lblTelephoneNumber: TLabel;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListBox1: TListBox;
    CarrierNameItem: TListBoxItem;
    CountryCodeItem: TListBoxItem;
    NetworkCodeItem: TListBoxItem;
    MobileNetworkItem: TListBoxItem;
    procedure btnGetCarrierInfoClick(Sender: TObject);
    procedure btnMakeCallClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FPhoneDialerService: IFMXPhoneDialerService;
    FCallPhonePermission: string;
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
    procedure MakePhoneCallPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
  public
    { Public declarations }
  end;

var
  PhoneDialerForm: TPhoneDialerForm;

implementation

uses
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.DialogService;

{$R *.fmx}

procedure TPhoneDialerForm.FormCreate(Sender: TObject);
begin
{$IFDEF ANDROID}
  FCallPhonePermission := JStringToString(TJManifest_permission.JavaClass.CALL_PHONE);
{$ENDIF}
  { test whether the PhoneDialer services are supported }
  TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, FPhoneDialerService);
end;

procedure TPhoneDialerForm.btnGetCarrierInfoClick(Sender: TObject);
begin
  { test whether the PhoneDialer services are supported }
  if FPhoneDialerService <> nil then
  begin
    { if yes, then update the labels with the retrieved information }
    CarrierNameItem.ItemData.Detail := FPhoneDialerService.GetCarrier.GetCarrierName;
    CountryCodeItem.ItemData.Detail := FPhoneDialerService.GetCarrier.GetIsoCountryCode;
    NetworkCodeItem.ItemData.Detail := FPhoneDialerService.GetCarrier.GetMobileCountryCode;
    MobileNetworkItem.ItemData.Detail := FPhoneDialerService.GetCarrier.GetMobileNetwork;
  end
  else
    TDialogService.ShowMessage('PhoneDialer service not supported');
end;

// Optional rationale display routine to display permission requirement rationale to the user
procedure TPhoneDialerForm.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
  // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
  // After the user sees the explanation, invoke the post-rationale routine to request the permissions
  TDialogService.ShowMessage('The app needs to be able to support your making phone calls',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end)
end;

procedure TPhoneDialerForm.btnMakeCallClick(Sender: TObject);
begin
  { test whether the PhoneDialer services are supported }
  if FPhoneDialerService <> nil then
  begin
    { if the Telephone Number is entered in the edit box then make the call, else
      display an error message }
    if edtTelephoneNumber.Text <> '' then
      PermissionsService.RequestPermissions([FCallPhonePermission], MakePhoneCallPermissionRequestResult, DisplayRationale)
    else
    begin
      TDialogService.ShowMessage('Please type in a telephone number.');
      edtTelephoneNumber.SetFocus;
    end;
  end
  else
    TDialogService.ShowMessage('PhoneDialer service not supported');
end;

procedure TPhoneDialerForm.MakePhoneCallPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
begin
  // 1 permission involved: CALL_PHONE
  if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
    FPhoneDialerService.Call(edtTelephoneNumber.Text)
  else
    TDialogService.ShowMessage('Cannot make a phone call because the required permission has not been granted');
end;

end.
