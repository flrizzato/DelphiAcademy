{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Permissions;

{$SCOPEDENUMS ON}

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Types;

type
  EPermissionException = class(Exception);

  TPermissionStatus = (Granted, Denied, PermanentlyDenied);
  /// <summary>Callback type for when the system has processed our permission requests</summary>
  /// <remarks>For each requested permission in APermissions, there is
  /// a Boolean in AGrantResults indicating if the permission was granted.
  /// <para>This type is compatible with an event handler method.</para></remarks>
  TRequestPermissionsResultEvent = procedure(Sender: TObject; const APermissions: TArray<string>;
    const AGrantResults: TArray<TPermissionStatus>) of object;
  /// <summary>Callback type for when the system has processed our permission requests</summary>
  /// <remarks>For each requested permission in APermissions, there is
  /// a Boolean in AGrantResults indicating if the permission was granted.
  /// <para>This type is compatible with an anonymous procedure.</para></remarks>
  TRequestPermissionsResultProc = reference to procedure(const APermissions: TArray<string>;
    const AGrantResults: TArray<TPermissionStatus>);

  /// <summary>Callback type for providing an explanation to the user for the need for a permission</summary>
  /// <remarks>For previously denied permissions that we pre-loaded a rationale string for,
  /// this callback provides the opportunity to display it, but it *must* be done asynchronously.
  /// When the rationale display is over, be sure to call the APostRationalProc routine,
  /// which will then request the permissions that need requesting.
  /// <para>This type is compatible with an event handler method.</para></remarks>
  TDisplayRationaleEvent = procedure(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc) of object;
  /// <summary>Callback type for providing an explanation to the user for the need for a permission</summary>
  /// <remarks>For previously denied permissions that we pre-loaded a rationale string for,
  /// this callback provides the opportunity to display it, but it *must* be done asynchronously.
  /// When the rationale display is over, be sure to call the APostRationalProc routine,
  /// which will then request the permissions that need requesting.
  /// <para>This type is compatible with an anonymous procedure.</para></remarks>
  TDisplayRationaleProc = reference to procedure(const APermissions: TArray<string>; const APostRationaleProc: TProc);

  /// <summary>Base permissions service abstract class</summary>
  TPermissionsService = class abstract
  private
    class function GetDefaultService: TPermissionsService; static;
  protected
    class var FDefaultService: TPermissionsService;
    constructor Create; virtual;
  public
    class destructor UnInitialize;
    /// <summary>Find out if a permission is currently granted to the app</summary>
    /// <remarks>If there is no platform permissions service implemented to actually do any checking,
    /// then we default to responding that all permissions are granted</remarks>
    function IsPermissionGranted(const APermission: string): Boolean; virtual;
    /// <summary>Find out if a permissions are all currently granted to the app</summary>
    function IsEveryPermissionGranted(const APermissions: TArray<string>): Boolean; virtual;
    /// <summary>Request one or more permissions</summary>
    /// <remarks>Any permissions that are not currently granted will be requested.
    /// Beforehand, a rationale may be displayed to the user if:
    /// <para>  i) a rationale string has been set for the permission in question</para>
    /// <para> ii) the OS deems it appropriate (we're requesting a permission again that was previously denied)</para>
    /// <para>iii) a rationale display routine is passed in</para>
    /// The rationale handler must display the passed in rationale string asynchronously and not block the thread.
    /// <para>This overload takes an event handler method.</para></remarks>
    procedure RequestPermissions(const APermissions: TArray<string>;
      const AOnRequestPermissionsResult: TRequestPermissionsResultEvent; AOnDisplayRationale: TDisplayRationaleEvent = nil);
      overload; virtual;
    /// <summary>Request one or more permissions</summary>
    /// <remarks>Any permissions that are not currently granted will be requested.
    /// Beforehand, a rationale may be displayed to the user if:
    /// <para>  i) a rationale string has been set for the permission in question</para>
    /// <para> ii) the OS deems it appropriate (we're requesting a permission again that was previously denied)</para>
    /// <para>iii) a rationale display routine is passed in</para>
    /// The rationale handler must display the passed in rationale string asynchronously and not block the thread.
    /// <para>This overload takes an event handler method.</para></remarks>
    procedure RequestPermissions(const APermissions: TArray<string>;
      const AOnRequestPermissionsResult: TRequestPermissionsResultProc; AOnDisplayRationale: TDisplayRationaleProc = nil);
      overload; virtual;
    /// <summary>Factory property to retrieve an appropriate permissions implementation, if available on the current platform</summary>
    class property DefaultService: TPermissionsService read GetDefaultService;
  end;

  TPermissionsServiceClass = class of TPermissionsService;

var
  PermissionsServiceClass: TPermissionsServiceClass = TPermissionsService;

/// <summary>Helper to call TPermissionsService.DefaultService</summary>
function PermissionsService: TPermissionsService; inline;

implementation

{$IFDEF ANDROID}
uses
  System.Android.Permissions;
{$ENDIF}

{ TPermissionsService }

constructor TPermissionsService.Create;
begin
  inherited Create;
end;

class function TPermissionsService.GetDefaultService: TPermissionsService;
begin
  if (FDefaultService = nil) and (PermissionsServiceClass <> nil) then
    FDefaultService := PermissionsServiceClass.Create;
  Result := FDefaultService;
end;

function TPermissionsService.IsPermissionGranted(const APermission: string): Boolean;
begin
  Result := True
end;

function TPermissionsService.IsEveryPermissionGranted(const APermissions: TArray<string>): Boolean;
begin
  Result := True
end;

procedure TPermissionsService.RequestPermissions(const APermissions: TArray<string>;
  const AOnRequestPermissionsResult: TRequestPermissionsResultEvent; AOnDisplayRationale: TDisplayRationaleEvent);
var
  GrantResults: TArray<TPermissionStatus>;
  I: Integer;
begin
  SetLength(GrantResults, Length(APermissions));
  for I := Low(GrantResults) to High(GrantResults) do
    GrantResults[I] := TPermissionStatus.Granted;
  AOnRequestPermissionsResult(Self, APermissions, GrantResults)
end;

procedure TPermissionsService.RequestPermissions(const APermissions: TArray<string>;
  const AOnRequestPermissionsResult: TRequestPermissionsResultProc; AOnDisplayRationale: TDisplayRationaleProc);
var
  GrantResults: TArray<TPermissionStatus>;
  I: Integer;
begin
  SetLength(GrantResults, Length(APermissions));
  for I := Low(GrantResults) to High(GrantResults) do
    GrantResults[I] := TPermissionStatus.Granted;
  AOnRequestPermissionsResult(APermissions, GrantResults)
end;

class destructor TPermissionsService.UnInitialize;
begin
  FreeAndNil(FDefaultService);
end;

function PermissionsService: TPermissionsService;
begin
  Exit(TPermissionsService.DefaultService)
end;

end.
