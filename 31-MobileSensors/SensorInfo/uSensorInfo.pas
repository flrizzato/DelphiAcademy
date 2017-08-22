//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uSensorInfo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  System.Sensors, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TfrmAboutSensors = class(TForm)
    lbMain: TListBox;
    Timer1: TTimer;
    lInfo: TLabel;
    btnHide: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure lbMainItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FShowInfo : Boolean ;
    FActiveSensor : TCustomSensor;
    FOnOneScreen : Boolean;
    procedure ReAllignComponents;
    procedure CreateIfExists(ASensorCategory : TSensorCategory);
    function GetSensorCategoryName(ASensorCategory : TSensorCategory) : string;
    procedure ListBoxItemClick(Sender: TObject);
    function GetFullInfo(ACategory, AType, AClass, AAvailibleProperties : string): string;

    function GetInfoAboutLocation(ASensor : TCustomSensor): string;
    function GetInfoAboutEnv(ASensor : TCustomSensor): string;
    function GetInfoAboutMotion(ASensor : TCustomSensor): string;
    function GetInfoAboutOrientation(ASensor : TCustomSensor): string;
    function GetInfoAboutMechanical(ASensor : TCustomSensor): string;
    function GetInfoAboutElectro(ASensor : TCustomSensor): string;
    function GetInfoAboutBiometric(ASensor : TCustomSensor): string;
    function GetInfoAboutLight(ASensor : TCustomSensor): string;
    function GetInfoAboutScanner(ASensor : TCustomSensor): string;

    function GetTypeNameLocation(AType : TLocationSensorType): string ;
    function GetTypeNameEnv(AType : TEnvironmentalSensorType): string ;
    function GetTypeNameMotion(AType : TMotionSensorType): string ;
    function GetTypeNameOrientation(AType : TOrientationSensorType): string ;
    function GetTypeNameMech(AType : TMechanicalSensorType): string ;
    function GetTypeNameElectro(AType : TElectricalSensorType): string ;
    function GetTypeNameBio(AType : TBiometricSensorType): string ;
    function GetTypeNameLight(AType : TLightSensorType): string ;
    function GetTypeNameScanner(AType : TScannerSensorType): string ;

    function GetSensorType(ASensor : TCustomSensor): string;
    function ToFormStr(AProp : string; AVal : Single): string;
    function ToFormStrS(AProp : string; AVal : string): string;
    function ToFormStrB(AProp : string; AVal : Boolean): string;
  public
    { Public declarations }
  end;

var
  frmAboutSensors: TfrmAboutSensors;

implementation

{$R *.fmx}

const
  cBorder = 10;
  cND = 'Not defined';

const
  AllCat : TSensorCategories =
  [TSensorCategory.Location, TSensorCategory.Environmental, TSensorCategory.Motion,
  TSensorCategory.Orientation, TSensorCategory.Mechanical, TSensorCategory.Electrical,
  TSensorCategory.Biometric, TSensorCategory.Light, TSensorCategory.Scanner];
  cForm = '  %s =' + sLineBreak + '%30s        %3.5f ' + sLineBreak;
  cFormS = '  %s =' + sLineBreak + '%30s        %s ' + sLineBreak;

procedure TfrmAboutSensors.btnHideClick(Sender: TObject);
begin
  FShowInfo := False;
  if (FActiveSensor <> nil) and (FActiveSensor.Started) then
    FActiveSensor.Stop;
end;

procedure TfrmAboutSensors.CreateIfExists(ASensorCategory: TSensorCategory);
var
  LSensorArray : TSensorArray;
  LSensor : TCustomSensor;
  LHeader : TListBoxGroupHeader;
  LItem : TListBoxItem;
begin
  LSensorArray := TSensorManager.Current.GetSensorsByCategory(ASensorCategory);
  LHeader := TListBoxGroupHeader.Create(Owner);
  LHeader.Parent := lbMain;
  LHeader.Text := GetSensorCategoryName(ASensorCategory);
  LHeader.Height := LHeader.Height * 2;
  for LSensor in LSensorArray do
  begin
    LItem := TListBoxItem.Create(Owner);
    LItem.Parent := lbMain;
    LItem.Text := GetSensorType(LSensor);
    LItem.ItemData.Accessory := TListBoxItemData.TAccessory.aDetail;
    LItem.Data := LSensor;
    LItem.OnClick := ListBoxItemClick;
    LItem.Height := LItem.Height * 2;
    LItem.Font.Size := LItem.Font.Size * 2;
  end;
end;

procedure TfrmAboutSensors.FormCreate(Sender: TObject);
var
  LSensorCat : TSensorCategory ;
begin
  FActiveSensor := nil;
  FShowInfo := False;
  ReAllignComponents;
  TSensorManager.Current.Activate();
  for LSensorCat in AllCat do
    CreateIfExists(LSensorCat);
end;

procedure TfrmAboutSensors.FormResize(Sender: TObject);
begin
  ReAllignComponents;
end;

function TfrmAboutSensors.GetFullInfo(ACategory, AType, AClass,
  AAvailibleProperties: string): string;
begin
  Result := sLineBreak + 'Category:' + sLineBreak
    + '  ' + ACategory + sLineBreak + sLineBreak
    + 'Sensor type:' + sLineBreak
    + '  ' + AType + sLineBreak + sLineBreak
    + 'Base class:' + sLineBreak
    + '  ' + AClass + sLineBreak + sLineBreak
    + 'Available properties:' + sLineBreak
    + AAvailibleProperties;
end;

function TfrmAboutSensors.GetInfoAboutBiometric(ASensor: TCustomSensor): string;
var
  ls : TCustomBiometricSensor;
  LValues : string;
  LProp : TCustomBiometricSensor.TProperty;
begin
  LValues := '';
  ls := TCustomBiometricSensor(ASensor);
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomBiometricSensor.TProperty.HumanPresence:
        LValues := LValues + ToFormStrB('HumanPresence', ls.HumanPresence);
      TCustomBiometricSensor.TProperty.HumanProximity:
        LValues := LValues + ToFormStr('HumanProximity', ls.HumanProximity);
      TCustomBiometricSensor.TProperty.Touch:
        LValues := LValues + ToFormStrB('Touch', ls.Touch);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameBio(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetInfoAboutElectro(ASensor: TCustomSensor): string;
var
  ls : TCustomElectricalSensor;
  LValues : string;
  LProp : TCustomElectricalSensor.TProperty;
begin
  LValues := '';
  ls := TCustomElectricalSensor(ASensor);
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomElectricalSensor.TProperty.Capacitance:
        LValues := LValues + ToFormStr('Capacitance', ls.Capacitance);
      TCustomElectricalSensor.TProperty.Resistance:
        LValues := LValues + ToFormStr('Resistance', ls.Resistance);
      TCustomElectricalSensor.TProperty.Inductance:
        LValues := LValues + ToFormStr('Inductance', ls.Inductance);
      TCustomElectricalSensor.TProperty.Current:
        LValues := LValues + ToFormStr('Current', ls.Current);
      TCustomElectricalSensor.TProperty.Voltage:
        LValues := LValues + ToFormStr('Voltage', ls.Voltage);
      TCustomElectricalSensor.TProperty.Power:
        LValues := LValues + ToFormStr('Power', ls.Power);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameElectro(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetInfoAboutEnv(ASensor: TCustomSensor): string;
var
  ls : TCustomEnvironmentalSensor;
  LValues : string;
  LProp : TCustomEnvironmentalSensor.TProperty;
begin
  LValues := '';
  ls := TCustomEnvironmentalSensor(ASensor);
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomEnvironmentalSensor.TProperty.Temperature:
        LValues := LValues + ToFormStr('Temperature', ls.Temperature);
      TCustomEnvironmentalSensor.TProperty.Pressure:
        LValues := LValues + ToFormStr('Pressure', ls.Pressure);
      TCustomEnvironmentalSensor.TProperty.Humidity:
        LValues := LValues + ToFormStr('Humidity', ls.Humidity);
      TCustomEnvironmentalSensor.TProperty.WindDirection:
        LValues := LValues + ToFormStr('WindDirection', ls.WindDirection);
      TCustomEnvironmentalSensor.TProperty.WindSpeed:
        LValues := LValues + ToFormStr('WindSpeed', ls.WindSpeed);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameEnv(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetInfoAboutLight(ASensor: TCustomSensor): string;
var
  ls : TCustomLightSensor;
  LValues : string;
  LProp : TCustomLightSensor.TProperty;
begin
  LValues := '';
  ls := TCustomLightSensor(ASensor);
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomLightSensor.TProperty.Lux:
        LValues := LValues + ToFormStr('Lux', ls.Lux);
      TCustomLightSensor.TProperty.Temperature:
        LValues := LValues + ToFormStr('Temperature', ls.Temperature);
      TCustomLightSensor.TProperty.Chromacity:
        LValues := LValues + ToFormStr('Chromacity', ls.Chromacity);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameLight(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetInfoAboutLocation(
  ASensor: TCustomSensor): string;
var
  ls : TCustomLocationSensor;
  LValues : string;
  LProp : TCustomLocationSensor.TProperty;
begin
  LValues := '';
  ls := TCustomLocationSensor(ASensor);
  if not ls.Started then
    ls.Start;
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomLocationSensor.TProperty.Latitude:
        LValues := LValues + ToFormStr('Latitude', ls.Latitude);
      TCustomLocationSensor.TProperty.Longitude:
        LValues := LValues + ToFormStr('Longitude', ls.Longitude);
      TCustomLocationSensor.TProperty.ErrorRadius:
        LValues := LValues + ToFormStr('ErrorRadius', ls.ErrorRadius);
      TCustomLocationSensor.TProperty.Altitude:
        LValues := LValues + ToFormStr('Altitude', ls.Altitude);
      TCustomLocationSensor.TProperty.Speed:
        LValues := LValues + ToFormStr('Speed', ls.Speed);
      TCustomLocationSensor.TProperty.TrueHeading:
        LValues := LValues + ToFormStr('TrueHeading', ls.TrueHeading);
      TCustomLocationSensor.TProperty.MagneticHeading:
        LValues := LValues + ToFormStr('MagneticHeading', ls.MagneticHeading);
      TCustomLocationSensor.TProperty.Address1:
        LValues := LValues + ToFormStrS('Address1', ls.Address1);
      TCustomLocationSensor.TProperty.Address2:
        LValues := LValues + ToFormStrS('Address2', ls.Address2);
      TCustomLocationSensor.TProperty.City:
        LValues := LValues + ToFormStrS('City', ls.City);
      TCustomLocationSensor.TProperty.StateProvince:
        LValues := LValues + ToFormStrS('StateProvince', ls.StateProvince);
      TCustomLocationSensor.TProperty.PostalCode:
        LValues := LValues + ToFormStrS('PostalCode', ls.PostalCode);
      TCustomLocationSensor.TProperty.CountryRegion:
        LValues := LValues + ToFormStrS('CountryRegion', ls.CountryRegion);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameLocation(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetInfoAboutMechanical(
  ASensor: TCustomSensor): string;
var
  ls : TCustomMechanicalSensor;
  LValues : string;
  LProp : TCustomMechanicalSensor.TProperty;
begin
  LValues := '';
  ls := TCustomMechanicalSensor(ASensor);
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomMechanicalSensor.TProperty.SwitchState:
        LValues := LValues + ToFormStrB('SwitchState', ls.SwitchState);
      TCustomMechanicalSensor.TProperty.SwitchArrayState:
        LValues := LValues + ToFormStr('SwitchArrayState', ls.SwitchArrayState);
      TCustomMechanicalSensor.TProperty.MultiValueState:
        LValues := LValues + ToFormStr('MultiValueState', ls.MultiValueState);
      TCustomMechanicalSensor.TProperty.Force:
        LValues := LValues + ToFormStr('Force', ls.Force);
      TCustomMechanicalSensor.TProperty.AbsPressure:
        LValues := LValues + ToFormStr('AbsPressure', ls.AbsPressure);
      TCustomMechanicalSensor.TProperty.GaugePressure:
        LValues := LValues + ToFormStr('GaugePressure', ls.GaugePressure);
      TCustomMechanicalSensor.TProperty.Strain:
        LValues := LValues + ToFormStr('Strain', ls.Strain);
      TCustomMechanicalSensor.TProperty.Weight:
        LValues := LValues + ToFormStr('Weight', ls.Weight);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameMech(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetInfoAboutMotion(ASensor: TCustomSensor): string;
var
  ls : TCustomMotionSensor;
  LValues : string;
  LProp : TCustomMotionSensor.TProperty;
begin
  LValues := '';
  ls := TCustomMotionSensor(ASensor);
  if not ls.Started then
    ls.Start;
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomMotionSensor.TProperty.AccelerationX:
        LValues := LValues + ToFormStr('AccelerationX', ls.AccelerationX);
      TCustomMotionSensor.TProperty.AccelerationY:
        LValues := LValues + ToFormStr('AccelerationY', ls.AccelerationY);
      TCustomMotionSensor.TProperty.AccelerationZ:
        LValues := LValues + ToFormStr('AccelerationZ', ls.AccelerationZ);
      TCustomMotionSensor.TProperty.AngleAccelX:
        LValues := LValues + ToFormStr('AngleAccelX', ls.AngleAccelX);
      TCustomMotionSensor.TProperty.AngleAccelY:
        LValues := LValues + ToFormStr('AngleAccelY', ls.AngleAccelY);
      TCustomMotionSensor.TProperty.AngleAccelZ:
        LValues := LValues + ToFormStr('AngleAccelZ', ls.AngleAccelZ);
      TCustomMotionSensor.TProperty.Motion:
        LValues := LValues + ToFormStr('Motion', ls.Motion);
      TCustomMotionSensor.TProperty.Speed:
        LValues := LValues + ToFormStr('Speed', ls.Speed);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameMotion(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetInfoAboutOrientation(
  ASensor: TCustomSensor): string;
var
  ls : TCustomOrientationSensor;
  LValues : string;
  LProp : TCustomOrientationSensor.TProperty;
begin
  LValues := '';
  ls := TCustomOrientationSensor(ASensor);
  if not ls.Started then
    ls.Start;
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomOrientationSensor.TProperty.TiltX:
        LValues := LValues + ToFormStr('TiltX', ls.TiltX);
      TCustomOrientationSensor.TProperty.TiltY:
        LValues := LValues + ToFormStr('TiltY', ls.TiltY);
      TCustomOrientationSensor.TProperty.TiltZ:
        LValues := LValues + ToFormStr('TiltZ', ls.TiltZ);
      TCustomOrientationSensor.TProperty.DistanceX:
        LValues := LValues + ToFormStr('DistanceX', ls.DistanceX);
      TCustomOrientationSensor.TProperty.DistanceY:
        LValues := LValues + ToFormStr('DistanceY', ls.DistanceY);
      TCustomOrientationSensor.TProperty.DistanceZ:
        LValues := LValues + ToFormStr('DistanceZ', ls.DistanceZ);
      TCustomOrientationSensor.TProperty.HeadingX:
        LValues := LValues + ToFormStr('HeadingX', ls.HeadingX);
      TCustomOrientationSensor.TProperty.HeadingY:
        LValues := LValues + ToFormStr('HeadingY', ls.HeadingY);
      TCustomOrientationSensor.TProperty.HeadingZ:
        LValues := LValues + ToFormStr('HeadingZ', ls.HeadingZ);
      TCustomOrientationSensor.TProperty.MagHeading:
        LValues := LValues + ToFormStr('MagHeading', ls.MagHeading);
      TCustomOrientationSensor.TProperty.TrueHeading:
        LValues := LValues + ToFormStr('TrueHeading', ls.TrueHeading);
      TCustomOrientationSensor.TProperty.CompMagHeading:
        LValues := LValues + ToFormStr('CompMagHeading', ls.CompMagHeading);
      TCustomOrientationSensor.TProperty.CompTrueHeading:
        LValues := LValues + ToFormStr('CompTrueHeading', ls.CompTrueHeading);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameOrientation(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetInfoAboutScanner(ASensor: TCustomSensor): string;
var
  ls : TCustomScannerSensor;
  LValues : string;
  LProp : TCustomScannerSensor.TProperty;
begin
  LValues := '';
  ls := TCustomScannerSensor(ASensor);
  for LProp in ls.AvailableProperties do
  begin
    case LProp of
      TCustomScannerSensor.TProperty.RFIDTag:
        LValues := LValues + ToFormStr('RFIDTag', ls.RFIDTag);
      TCustomScannerSensor.TProperty.BarcodeData:
        LValues := LValues + ToFormStrS('BarcodeData', ls.BarcodeData);
    end;
  end;
  Result := GetFullInfo(
    GetSensorCategoryName(ASensor.Category),
    GetTypeNameScanner(ls.SensorType),
    ls.ClassName,
    LValues
  ) ;
end;

function TfrmAboutSensors.GetTypeNameBio(AType: TBiometricSensorType): string;
begin
  Result := cND;
  case AType of
    TBiometricSensorType.HumanPresence: Result := 'HumanPresence';
    TBiometricSensorType.HumanProximity: Result := 'HumanProximity';
    TBiometricSensorType.Touch: Result := 'Touch';
  end;
end;

function TfrmAboutSensors.GetTypeNameElectro(
  AType: TElectricalSensorType): string;
begin
  Result := cND;
  case AType of
    TElectricalSensorType.Voltage: Result := 'Voltage';
    TElectricalSensorType.Current: Result := 'Current';
    TElectricalSensorType.Capacitance: Result := 'Capacitance';
    TElectricalSensorType.Resistance: Result := 'Resistance';
    TElectricalSensorType.Inductance: Result := 'Inductance';
    TElectricalSensorType.ElectricalPower: Result := 'ElectricalPower';
    TElectricalSensorType.Potentiometer: Result := 'Potentiometer';
  end;
end;

function TfrmAboutSensors.GetTypeNameEnv(
  AType: TEnvironmentalSensorType): string;
begin
  Result := cND;
  case AType of
    TEnvironmentalSensorType.Temperature: Result := 'Temperature';
    TEnvironmentalSensorType.AtmosphericPressure: Result := 'AtmosphericPressure';
    TEnvironmentalSensorType.Humidity: Result := 'Humidity';
    TEnvironmentalSensorType.WindSpeed: Result := 'WindSpeed';
    TEnvironmentalSensorType.WindDirection: Result := 'WindDirection';
  end;
end;

function TfrmAboutSensors.GetTypeNameLight(AType: TLightSensorType): string;
begin
  Result := cND;
  case AType of
    TLightSensorType.AmbientLight: Result := 'AmbientLight' ;
  end;
end;

function TfrmAboutSensors.GetTypeNameLocation(AType: TLocationSensorType): string;
begin
  case AType of
    TLocationSensorType.GPS: Result := 'GPS';
    TLocationSensorType.Static: Result := 'Static';
    TLocationSensorType.Lookup: Result := 'Lookup';
    TLocationSensorType.Triangulation: Result := 'Triangulation';
    TLocationSensorType.Broadcast: Result := 'Broadcast';
    TLocationSensorType.DeadReckoning: Result := 'DeadReckoning';
    TLocationSensorType.Other: Result := 'Other';
  else
    Result := cND
  end;
end;

function TfrmAboutSensors.GetTypeNameMech(AType: TMechanicalSensorType): string;
begin
  Result := cND;
  case AType of
    TMechanicalSensorType.BooleanSwitch: Result := 'BooleanSwitch';
    TMechanicalSensorType.BooleanSwitchArray: Result := 'BooleanSwitchArray';
    TMechanicalSensorType.MultiValueSwitch: Result := 'MultiValueSwitch';
    TMechanicalSensorType.Force: Result := 'Force';
    TMechanicalSensorType.Scale: Result := 'Scale';
    TMechanicalSensorType.Pressure: Result := 'Pressure';
    TMechanicalSensorType.Strain: Result := 'Strain';
  end;
end;

function TfrmAboutSensors.GetTypeNameMotion(AType: TMotionSensorType): string;
begin
  case AType of
    TMotionSensorType.Accelerometer1D: Result := 'Accelerometer1D';
    TMotionSensorType.Accelerometer2D: Result := 'Accelerometer2D';
    TMotionSensorType.Accelerometer3D: Result := 'Accelerometer3D';
    TMotionSensorType.MotionDetector: Result := 'MotionDetector';
    TMotionSensorType.Gyrometer1D: Result := 'Gyrometer1D';
    TMotionSensorType.Gyrometer2D: Result := 'Gyrometer2D';
    TMotionSensorType.Gyrometer3D: Result := 'Gyrometer3D';
    TMotionSensorType.Speedometer: Result := 'Speedometer';
    TMotionSensorType.LinearAccelerometer3D: Result := 'LinearAccelerometer3D';
    TMotionSensorType.GravityAccelerometer3D: Result := 'GravityAccelerometer3D';
  else
    Result := cND;
  end;
end;

function TfrmAboutSensors.GetTypeNameOrientation(
  AType: TOrientationSensorType): string;
begin
  case AType of
    TOrientationSensorType.Compass1D: Result := 'Compass1D';
    TOrientationSensorType.Compass2D: Result := 'Compass2D';
    TOrientationSensorType.Compass3D: Result := 'Compass3D';
    TOrientationSensorType.Inclinometer1D: Result := 'Inclinometer1D';
    TOrientationSensorType.Inclinometer2D: Result := 'Inclinometer2D';
    TOrientationSensorType.Inclinometer3D: Result := 'Inclinometer3D';
    TOrientationSensorType.Distance1D: Result := 'Distance1D';
    TOrientationSensorType.Distance2D: Result := 'Distance2D';
    TOrientationSensorType.Distance3D: Result := 'Distance3D';
  else
    Result := cND;
  end;
end;

function TfrmAboutSensors.GetTypeNameScanner(AType: TScannerSensorType): string;
begin
  Result := cND;
  case AType of
    TScannerSensorType.RFID: Result := 'RFID';
    TScannerSensorType.Barcode: Result := 'Barcode';
  end;
end;

procedure TfrmAboutSensors.lbMainItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  if Assigned(Item.OnClick) then
    Item.OnClick(Item);
end;

function TfrmAboutSensors.GetSensorCategoryName(ASensorCategory: TSensorCategory): string;
begin
  Result := cND ;
  case ASensorCategory of
    TSensorCategory.Location: Result := 'Location' ;
    TSensorCategory.Environmental: Result := 'Environmental' ;
    TSensorCategory.Motion: Result := 'Motion' ;
    TSensorCategory.Orientation: Result := 'Orientation' ;
    TSensorCategory.Mechanical: Result := 'Mechanical' ;
    TSensorCategory.Electrical: Result := 'Electrical' ;
    TSensorCategory.Biometric: Result := 'Biometric' ;
    TSensorCategory.Light: Result := 'Light' ;
    TSensorCategory.Scanner: Result := 'Scanner' ;
  end;
end;

function TfrmAboutSensors.GetSensorType(ASensor: TCustomSensor): string;
begin
  Result := cND;
  case ASensor.Category of
    TSensorCategory.Location: Result := GetTypeNameLocation(TCustomLocationSensor(ASensor).SensorType);
    TSensorCategory.Environmental:  Result := GetTypeNameEnv(TCustomEnvironmentalSensor(ASensor).SensorType);
    TSensorCategory.Motion: Result := GetTypeNameMotion(TCustomMotionSensor(ASensor).SensorType) ;
    TSensorCategory.Orientation: Result := GetTypeNameOrientation(TCustomOrientationSensor(ASensor).SensorType);
    TSensorCategory.Mechanical: Result := GetTypeNameMech(TCustomMechanicalSensor(ASensor).SensorType);
    TSensorCategory.Electrical: Result := GetTypeNameElectro(TCustomElectricalSensor(ASensor).SensorType);
    TSensorCategory.Biometric: Result := GetTypeNameBio(TCustomBiometricSensor(ASensor).SensorType);
    TSensorCategory.Light: Result := GetTypeNameLight(TCustomLightSensor(ASensor).SensorType);
    TSensorCategory.Scanner:  Result := GetTypeNameScanner(TCustomScannerSensor(ASensor).SensorType);
  end;
end;

procedure TfrmAboutSensors.ListBoxItemClick(Sender: TObject);
begin
  if Sender is TListBoxItem then
  begin
    FActiveSensor := TCustomSensor(TListBoxItem(Sender).Data);
    if (FActiveSensor <> nil) and (not FActiveSensor.Started) then
      FActiveSensor.Start;
  end;
  FShowInfo := True;
end;

procedure TfrmAboutSensors.ReAllignComponents;
begin
  FOnOneScreen := Width > Height;
  if FOnOneScreen then
  begin
    btnHide.Visible := False;
    lbMain.Position.Point := PointF(cBorder,cBorder);
    lbMain.Width := Width / 2 - cBorder * 2;
    lInfo.Position.Point := PointF(Width / 2 + cBorder,cBorder);
    lInfo.Width := Width / 2 - cBorder * 2;
  end
  else
  begin
    btnHide.Visible := True;
    lbMain.Position.Point := PointF(cBorder,cBorder);
    lbMain.Width := Width - cBorder * 2;
    lInfo.Position.Point := PointF(Width + cBorder,cBorder);
    lInfo.Width := Width - cBorder * 2;
  end;
  lbMain.Height := Height - cBorder * 2;
  lInfo.Height := Height - cBorder * 2;
end;

procedure TfrmAboutSensors.Timer1Timer(Sender: TObject);
var
  ResultText : string;
  LStep : Single;
begin
{
  SysDebug(
    'Assigned(FActiveSensor) = ' + BoolToStr(Assigned(FActiveSensor))
    + '| FShowInfo = ' + BoolToStr(FShowInfo)
  );
}
  if Assigned(FActiveSensor) then
  begin
    case FActiveSensor.Category of
      TSensorCategory.Location: ResultText := GetInfoAboutLocation(FActiveSensor);
      TSensorCategory.Environmental: ResultText := GetInfoAboutEnv(FActiveSensor);
      TSensorCategory.Motion: ResultText := GetInfoAboutMotion(FActiveSensor);
      TSensorCategory.Orientation: ResultText := GetInfoAboutOrientation(FActiveSensor);
      TSensorCategory.Mechanical: ResultText := GetInfoAboutMechanical(FActiveSensor);
      TSensorCategory.Electrical: ResultText := GetInfoAboutElectro(FActiveSensor);
      TSensorCategory.Biometric: ResultText := GetInfoAboutBiometric(FActiveSensor);
      TSensorCategory.Light: ResultText := GetInfoAboutLight(FActiveSensor);
      TSensorCategory.Scanner: ResultText := GetInfoAboutScanner(FActiveSensor);
    end;
    lInfo.Text := ResultText;
  end;
  if not FOnOneScreen then
  begin
    if FShowInfo then
    begin
      if lInfo.Position.Point.X > (cBorder*2) then
      begin
        LStep := Width / 5;
        lInfo.Position.Point := PointF(lInfo.Position.Point.X - LStep, cBorder);
        lbMain.Position.Point := PointF(lbMain.Position.Point.X - LStep, cBorder);
      end;
    end
    else
    begin
      if lbMain.Position.Point.X < cBorder then
      begin
        LStep := Width / 5;
        lInfo.Position.Point := PointF(lInfo.Position.Point.X + LStep, cBorder);
        lbMain.Position.Point := PointF(lbMain.Position.Point.X + LStep, cBorder);
      end;
    end;
  end;
end;

function TfrmAboutSensors.ToFormStr(AProp: string; AVal: Single): string;
begin
  Result := Format(cForm,[AProp,'', AVal]);
end;

function TfrmAboutSensors.ToFormStrB(AProp: string; AVal: Boolean): string;
begin
  if AVal then
    ToFormStrS(AProp,'True')
  else
    ToFormStrS(AProp, 'False');
end;

function TfrmAboutSensors.ToFormStrS(AProp, AVal: string): string;
begin
  Result := Format(cFormS,[AProp,'', AVal]);
end;

end.
