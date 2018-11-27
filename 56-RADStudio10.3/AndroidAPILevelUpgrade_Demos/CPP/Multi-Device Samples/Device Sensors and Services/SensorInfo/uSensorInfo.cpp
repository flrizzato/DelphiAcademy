//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#ifdef __ANDROID__
#include <Androidapi.Helpers.hpp>
#include <Androidapi.JNI.Os.hpp>
#endif
#include <FMX.DialogService.hpp>
#pragma hdrstop

#include "uSensorInfo.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"

TfrmAboutSensors *frmAboutSensors;
// ---------------------------------------------------------------------------
void __fastcall TfrmAboutSensors::ReAlignComponents() {
	FOnOneScreen = Width > Height;
	if (FOnOneScreen) {
		btnHide->Visible = false;
		lbMain->Position->Point = PointF(cBorder, cBorder);
		lbMain->Width = Width / 2 - cBorder * 2;
		lInfo->Position->Point = PointF(Width / 2 + cBorder, cBorder);
		lInfo->Width = Width / 2 - cBorder * 2;
	}
	else {
		btnHide->Visible = true;
		lbMain->Position->Point = PointF(cBorder, cBorder);
		lbMain->Width = Width - cBorder * 2;
		lInfo->Position->Point = PointF(Width + cBorder, cBorder);
		lInfo->Width = Width - cBorder * 2;
	}
	lbMain->Height = Height - cBorder * 2;
	lInfo->Height = Height - cBorder * 2;
}
// ---------------------------------------------------------------------------
void __fastcall TfrmAboutSensors::CreateIfExists(TSensorCategory ASensorCategory) {
	TSensorArray LSensorArray = TSensorManager::Current->GetSensorsByCategory(ASensorCategory);
	TListBoxGroupHeader *LHeader = new TListBoxGroupHeader(this->Owner);
	LHeader->Parent = lbMain;
	LHeader->Text = GetSensorCategoryName(ASensorCategory);
	LHeader->Height = LHeader->Height * 2;
	for (int i = 0; i < LSensorArray.Length; i++) {
		TListBoxItem *LItem = new TListBoxItem(this->Owner);
		LItem->Parent = lbMain;
		LItem->Text = GetSensorType(LSensorArray[i]);
		LItem->ItemData->Accessory = TListBoxItemData::TAccessory::aDetail;
		LItem->Data = LSensorArray[i];
		LItem->OnClick = ListBoxItemClick;
		LItem->Height = LItem->Height * 2;
		LItem->Font->Size = LItem->Font->Size * 2;
	}
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetSensorCategoryName(TSensorCategory ASensorCategory) {
	String _return = cND;
	switch (ASensorCategory) {
	case TSensorCategory::Location:
		_return = "Location";
		break;
	case TSensorCategory::Environmental:
		_return = "Environmental";
		break;
	case TSensorCategory::Motion:
		_return = "Motion";
		break;
	case TSensorCategory::Orientation:
		_return = "Orientation";
		break;
	case TSensorCategory::Mechanical:
		_return = "Mechanical";
		break;
	case TSensorCategory::Electrical:
		_return = "Electrical";
		break;
	case TSensorCategory::Biometric:
		_return = "Biometric";
		break;
	case TSensorCategory::Light:
		_return = "Light";
		break;
	case TSensorCategory::Scanner:
		_return = "Scanner";
		break;
	}
	return _return;
}
// ---------------------------------------------------------------------------
void __fastcall TfrmAboutSensors::ListBoxItemClick(TObject *Sender) {
	if (dynamic_cast<TListBoxItem *>(Sender)) {
		FActiveSensor = (TCustomSensor *)dynamic_cast<TListBoxItem *>(Sender)->Data;
		if (FActiveSensor != NULL && !FActiveSensor->Started) {
        #ifdef __ANDROID__
            if (FActiveSensor->Category == TSensorCategory::Location) {
                DynamicArray<String> permissions;
                permissions.Length = 1;
                permissions[0] = JStringToString(TJManifest_permission::JavaClass->ACCESS_FINE_LOCATION);

                PermissionsService()->RequestPermissions(permissions,
                    [this](const DynamicArray<String> APermissions, const DynamicArray<TPermissionStatus> AGrantResults)
                    {
                        if ((AGrantResults.Length == 1) && (AGrantResults[0] == TPermissionStatus::Granted))
                            this->FActiveSensor->Start();
                        else
                            TDialogService::ShowMessage("Location permission not granted");
                    });
            }
        #else
            FActiveSensor->Start();
        #endif
		}
	}
	FShowInfo = true;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetFullInfo(String ACategory, String AType, String AClass, String AAvailableProperties) {
	return Format("\nCategory:\n   %s\n\nSensor type:\n   %s\n\nBase class:\n   %s\n\nAvailable properties:\n%s", ARRAYOFCONST((ACategory, AType, AClass, AAvailableProperties)));
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameLocation(TLocationSensorType AType) {
	String Result;
	switch (AType) {
	case TLocationSensorType::GPS:
		Result = "GPS";
		break;
	case TLocationSensorType::Static:
		Result = "Static";
		break;
	case TLocationSensorType::Lookup:
		Result = "Lookup";
		break;
	case TLocationSensorType::Triangulation:
		Result = "Triangulation";
		break;
	case TLocationSensorType::Broadcast:
		Result = "Broadcast";
		break;
	case TLocationSensorType::DeadReckoning:
		Result = "DeadReckoning";
		break;
	case TLocationSensorType::Other:
		Result = "Other";
		break;
	default:
		Result = cND;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameEnv(TEnvironmentalSensorType AType) {
	String Result = cND;
	switch (AType) {
	case TEnvironmentalSensorType::Temperature:
		Result = "Temperature";
		break;
	case TEnvironmentalSensorType::AtmosphericPressure:
		Result = "AtmosphericPressure";
		break;
	case TEnvironmentalSensorType::Humidity:
		Result = "Humidity";
		break;
	case TEnvironmentalSensorType::WindSpeed:
		Result = "WindSpeed";
		break;
	case TEnvironmentalSensorType::WindDirection:
		Result = "WindDirection";
		break;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameMotion(TMotionSensorType AType) {
	String Result = cND;
	switch (AType) {
	case TMotionSensorType::Accelerometer1D:
		Result = "Accelerometer1D";
		break;
	case TMotionSensorType::Accelerometer2D:
		Result = "Accelerometer2D";
		break;
	case TMotionSensorType::Accelerometer3D:
		Result = "Accelerometer3D";
		break;
	case TMotionSensorType::MotionDetector:
		Result = "MotionDetector";
		break;
	case TMotionSensorType::Gyrometer1D:
		Result = "Gyrometer1D";
		break;
	case TMotionSensorType::Gyrometer2D:
		Result = "Gyrometer2D";
		break;
	case TMotionSensorType::Gyrometer3D:
		Result = "Gyrometer3D";
		break;
	case TMotionSensorType::Speedometer:
		Result = "Speedometer";
		break;
	case TMotionSensorType::LinearAccelerometer3D:
		Result = "LinearAccelerometer3D";
		break;
	case TMotionSensorType::GravityAccelerometer3D:
		Result = "GravityAccelerometer3D";
		break;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameOrientation(TOrientationSensorType AType) {
	String Result = cND;
	switch (AType) {
	case TOrientationSensorType::Compass1D:
		Result = "Compass1D";
		break;
	case TOrientationSensorType::Compass2D:
		Result = "Compass2D";
		break;
	case TOrientationSensorType::Compass3D:
		Result = "Compass3D";
		break;
	case TOrientationSensorType::Inclinometer1D:
		Result = "Inclinometer1D";
		break;
	case TOrientationSensorType::Inclinometer2D:
		Result = "Inclinometer2D";
		break;
	case TOrientationSensorType::Inclinometer3D:
		Result = "Inclinometer3D";
		break;
	case TOrientationSensorType::Distance1D:
		Result = "Distance1D";
		break;
	case TOrientationSensorType::Distance2D:
		Result = "Distance2D";
		break;
	case TOrientationSensorType::Distance3D:
		Result = "Distance3D";
		break;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameMech(TMechanicalSensorType AType) {
	String Result = cND;
	switch (AType) {
	case TMechanicalSensorType::BooleanSwitch:
		Result = "BooleanSwitch";
		break;
	case TMechanicalSensorType::BooleanSwitchArray:
		Result = "BooleanSwitchArray";
		break;
	case TMechanicalSensorType::MultiValueSwitch:
		Result = "MultiValueSwitch";
		break;
	case TMechanicalSensorType::Force:
		Result = "Force";
		break;
	case TMechanicalSensorType::Scale:
		Result = "Scale";
		break;
	case TMechanicalSensorType::Pressure:
		Result = "Pressure";
		break;
	case TMechanicalSensorType::Strain:
		Result = "Strain";
		break;
	default: ;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameElectro(TElectricalSensorType AType) {
	String Result = cND;
	switch (AType) {
	case TElectricalSensorType::Voltage:
		Result = "Voltage";
		break;
	case TElectricalSensorType::Current:
		Result = "Current";
		break;
	case TElectricalSensorType::Capacitance:
		Result = "Capacitance";
		break;
	case TElectricalSensorType::Resistance:
		Result = "Resistance";
		break;
	case TElectricalSensorType::Inductance:
		Result = "Inductance";
		break;
	case TElectricalSensorType::ElectricalPower:
		Result = "ElectricalPower";
		break;
	case TElectricalSensorType::Potentiometer:
		Result = "Potentiometer";
		break;
	default: ;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameBio(TBiometricSensorType AType) {
	String Result = cND;
	switch (AType) {
	case TBiometricSensorType::HumanPresence:
		Result = "HumanPresence";
		break;
	case TBiometricSensorType::HumanProximity:
		Result = "HumanProximity";
		break;
	case TBiometricSensorType::Touch:
		Result = "Touch";
		break;
	default: ;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameLight(TLightSensorType AType) {
	String Result = cND;
	switch (AType) {
	case TLightSensorType::AmbientLight:
		Result = "AmbientLight";
		break;
	default: ;
	}
	return Result;
}
// --------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetTypeNameScanner(TScannerSensorType AType) {
	String Result = cND;
	switch (AType) {
	case TScannerSensorType::RFID:
		Result = "RFID";
		break;
	case TScannerSensorType::Barcode:
		Result = "Barcode";
		break;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetSensorType(TCustomSensor *ASensor) {
	String Result = cND;
	switch (ASensor->Category) {
	case TSensorCategory::Location:
		Result = GetTypeNameLocation(static_cast<TCustomLocationSensor *>(ASensor)->SensorType);
		break;
	case TSensorCategory::Environmental:
		Result = GetTypeNameEnv(static_cast<TCustomEnvironmentalSensor *>(ASensor)->SensorType);
		break;
	case TSensorCategory::Motion:
		Result = GetTypeNameMotion(static_cast<TCustomMotionSensor *>(ASensor)->SensorType);
		break;
	case TSensorCategory::Orientation:
		Result = GetTypeNameOrientation(static_cast<TCustomOrientationSensor *>(ASensor)->SensorType);
		break;
	case TSensorCategory::Mechanical:
		Result = GetTypeNameMech(static_cast<TCustomMechanicalSensor *>(ASensor)->SensorType);
		break;
	case TSensorCategory::Electrical:
		Result = GetTypeNameElectro(static_cast<TCustomElectricalSensor *>(ASensor)->SensorType);
		break;
	case TSensorCategory::Biometric:
		Result = GetTypeNameBio(static_cast<TCustomBiometricSensor *>(ASensor)->SensorType);
		break;
	case TSensorCategory::Light:
		Result = GetTypeNameLight(static_cast<TCustomLightSensor *>(ASensor)->SensorType);
		break;
	case TSensorCategory::Scanner:
		Result = GetTypeNameScanner(static_cast<TCustomScannerSensor *>(ASensor)->SensorType);
		break;
	default: ;
	}
	return Result;
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::ToFormStr(String AProp, double AVal) {
	return Format(cForm, ARRAYOFCONST((AProp, "",  AVal)));
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::ToFormStrS(String AProp, String AVal) {
	return Format(cFormS, ARRAYOFCONST((AProp, "", AVal)));
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::ToFormStrB(String AProp, bool AVal) {
	if(AVal)
		return ToFormStrS(AProp, "True");
	else
		return ToFormStrS(AProp, "False");
}
// ---------------------------------------------------------------------------
__fastcall TfrmAboutSensors::TfrmAboutSensors(TComponent *Owner) : TForm(Owner), cBorder(10), cND("Not defined"),
	cForm(L"  %s =\n%30s        %3.5f \n"), cFormS(L"  %s =\n%30s        %s \n") {
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutLocation(TCustomSensor *ASensor) {
	String LValues = "";
	TCustomLocationSensor *ls = static_cast<TCustomLocationSensor *>(ASensor);
	if (!ls->Started) {
		ls->Start();
	}
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::Latitude))
		LValues += ToFormStr("Latitude", ls->Latitude);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::Longitude))
		LValues += ToFormStr("Longitude", ls->Longitude);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::ErrorRadius))
		LValues += ToFormStr("ErrorRadius", ls->ErrorRadius);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::Altitude))
		LValues += ToFormStr("Altitude", ls->Altitude);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::Speed))
		LValues += ToFormStr("Speed", ls->Speed);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::TrueHeading))
		LValues += ToFormStr("TrueHeading", ls->TrueHeading);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::MagneticHeading))
		LValues += ToFormStr("MagneticHeading", ls->MagneticHeading);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::Address1))
		LValues += ToFormStrS("Address1", ls->Address1);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::City))
		LValues += ToFormStrS("City", ls->City);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::StateProvince))
		LValues += ToFormStrS("StateProvince", ls->StateProvince);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::PostalCode))
		LValues += ToFormStrS("PostalCode", ls->PostalCode);
	if (ls->AvailableProperties.Contains(TCustomLocationSensor::TProperty::CountryRegion))
		LValues += ToFormStrS("CountryRegion", ls->CountryRegion);

	return GetFullInfo(GetSensorCategoryName(ASensor->Category), GetTypeNameLocation(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutEnv(TCustomSensor *ASensor) {
	String LValues = "";
	TCustomEnvironmentalSensor *ls = static_cast<TCustomEnvironmentalSensor *>(ASensor);
	if (ls->AvailableProperties.Contains(TCustomEnvironmentalSensor::TProperty::Temperature))
		LValues += ToFormStr("Temperature", ls->Temperature);
	if (ls->AvailableProperties.Contains(TCustomEnvironmentalSensor::TProperty::Pressure))
		LValues += ToFormStr("Pressure", ls->Pressure);
	if (ls->AvailableProperties.Contains(TCustomEnvironmentalSensor::TProperty::Humidity))
		LValues += ToFormStr("Humidity", ls->Humidity);
	if (ls->AvailableProperties.Contains(TCustomEnvironmentalSensor::TProperty::WindDirection))
		LValues += ToFormStr("WindDirection", ls->WindDirection);
	if (ls->AvailableProperties.Contains(TCustomEnvironmentalSensor::TProperty::WindSpeed))
		LValues += ToFormStr("WindSpeed", ls->WindSpeed);
	return GetFullInfo(GetSensorCategoryName(ASensor->Category), GetTypeNameEnv(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutMotion(TCustomSensor *ASensor) {
	String LValues = "";
	TCustomMotionSensor *ls = static_cast<TCustomMotionSensor *>(ASensor);
	if (!ls->Started)
		ls->Start();
	if (ls->AvailableProperties.Contains(TCustomMotionSensor::TProperty::AccelerationX))
		LValues += ToFormStr("AccelerationX", ls->AccelerationX);
	if (ls->AvailableProperties.Contains(TCustomMotionSensor::TProperty::AccelerationY))
		LValues += ToFormStr("AccelerationY", ls->AccelerationY);
	if (ls->AvailableProperties.Contains(TCustomMotionSensor::TProperty::AccelerationZ))
		LValues += ToFormStr("AccelerationZ", ls->AccelerationZ);
	if (ls->AvailableProperties.Contains(TCustomMotionSensor::TProperty::AngleAccelX))
		LValues += ToFormStr("AngleAccelX", ls->AngleAccelX);
	if (ls->AvailableProperties.Contains(TCustomMotionSensor::TProperty::AngleAccelY))
		LValues += ToFormStr("AngleAccelY", ls->AngleAccelY);
	if (ls->AvailableProperties.Contains(TCustomMotionSensor::TProperty::AngleAccelZ))
		LValues += ToFormStr("AngleAccelZ", ls->AngleAccelZ);
	if (ls->AvailableProperties.Contains(TCustomMotionSensor::TProperty::Motion))
		LValues += ToFormStr("Motion", ls->Motion);
	if (ls->AvailableProperties.Contains(TCustomMotionSensor::TProperty::Speed))
		LValues += ToFormStr("Speed", ls->Speed);
	return GetFullInfo(GetSensorCategoryName(ASensor->Category), GetTypeNameMotion(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutOrientation(TCustomSensor *ASensor) {
	 String LValues = "";
	 TCustomOrientationSensor *ls = static_cast<TCustomOrientationSensor *>(ASensor);
	 if(!ls->Started)
		ls->Start();
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::TiltX))
		LValues += ToFormStr("TiltX", ls->TiltX);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::TiltY))
		LValues += ToFormStr("TiltY", ls->TiltY);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::TiltZ))
		LValues += ToFormStr("TiltZ", ls->TiltZ);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::DistanceX))
		LValues += ToFormStr("DistanceX", ls->DistanceX);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::DistanceY))
		LValues += ToFormStr("DistanceY", ls->DistanceY);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::DistanceZ))
		LValues += ToFormStr("DistanceZ", ls->DistanceZ);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::HeadingX))
		LValues += ToFormStr("HeadingX", ls->HeadingX);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::HeadingY))
		LValues += ToFormStr("HeadingY", ls->HeadingY);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::HeadingZ))
		LValues += ToFormStr("HeadingZ", ls->HeadingZ);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::MagHeading))
		LValues += ToFormStr("MagHeading", ls->MagHeading);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::TrueHeading))
		LValues += ToFormStr("TrueHeading", ls->TrueHeading);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::CompMagHeading))
		LValues += ToFormStr("CompMagHeading", ls->CompMagHeading);
	 if(ls->AvailableProperties.Contains(TCustomOrientationSensor::TProperty::CompTrueHeading))
		LValues += ToFormStr("CompTrueHeading", ls->CompTrueHeading);

	 return GetFullInfo(GetSensorCategoryName(ASensor->Category),GetTypeNameOrientation(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutMechanical(TCustomSensor *ASensor) {
	 String LValues = "";
	 TCustomMechanicalSensor *ls = static_cast<TCustomMechanicalSensor *>(ASensor);
	 if(ls->AvailableProperties.Contains(TCustomMechanicalSensor::TProperty::SwitchState))
		LValues += ToFormStrB("SwitchState", ls->SwitchState);
	 if(ls->AvailableProperties.Contains(TCustomMechanicalSensor::TProperty::SwitchArrayState))
		LValues += ToFormStr("SwitchArrayState", ls->SwitchArrayState);
	 if(ls->AvailableProperties.Contains(TCustomMechanicalSensor::TProperty::MultiValueState))
		LValues += ToFormStr("MultiValueState", ls->MultiValueState);
	 if(ls->AvailableProperties.Contains(TCustomMechanicalSensor::TProperty::Force))
		LValues += ToFormStr("Force", ls->Force);
	 if(ls->AvailableProperties.Contains(TCustomMechanicalSensor::TProperty::AbsPressure))
		LValues += ToFormStr("AbsPressure", ls->AbsPressure);
	 if(ls->AvailableProperties.Contains(TCustomMechanicalSensor::TProperty::GaugePressure))
		LValues += ToFormStr("GaugePressure", ls->GaugePressure);
	 if(ls->AvailableProperties.Contains(TCustomMechanicalSensor::TProperty::Strain))
		LValues += ToFormStr("Strain", ls->Strain);
	 if(ls->AvailableProperties.Contains(TCustomMechanicalSensor::TProperty::Weight))
		LValues += ToFormStr("Weight", ls->Weight);

	 return GetFullInfo(GetSensorCategoryName(ASensor->Category), GetTypeNameMech(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutElectro(TCustomSensor *ASensor) {
	 String LValues = "";
	 TCustomElectricalSensor *ls = static_cast<TCustomElectricalSensor *>(ASensor);
	 if(ls->AvailableProperties.Contains(TCustomElectricalSensor::TProperty::Capacitance))
		LValues += ToFormStr("Capacitance", ls->Capacitance);
	 if(ls->AvailableProperties.Contains(TCustomElectricalSensor::TProperty::Resistance))
		LValues += ToFormStr("Resistance", ls->Resistance);
	 if(ls->AvailableProperties.Contains(TCustomElectricalSensor::TProperty::Inductance))
		LValues += ToFormStr("Inductance", ls->Inductance);
	 if(ls->AvailableProperties.Contains(TCustomElectricalSensor::TProperty::Current))
		LValues += ToFormStr("Current", ls->Current);
	 if(ls->AvailableProperties.Contains(TCustomElectricalSensor::TProperty::Voltage))
		LValues += ToFormStr("Voltage", ls->Voltage);
	 if(ls->AvailableProperties.Contains(TCustomElectricalSensor::TProperty::Power))
		LValues += ToFormStr("Power", ls->Power);

	 return GetFullInfo(GetSensorCategoryName(ASensor->Category), GetTypeNameElectro(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutBiometric(TCustomSensor *ASensor) {
	 String LValues = "";
	 TCustomBiometricSensor *ls = static_cast<TCustomBiometricSensor *>(ASensor);
	 if(ls->AvailableProperties.Contains(TCustomBiometricSensor::TProperty::HumanPresence))
		LValues += ToFormStrB("HumanPresence", ls->HumanPresence);
	 if(ls->AvailableProperties.Contains(TCustomBiometricSensor::TProperty::HumanProximity))
		LValues += ToFormStr("HumanProximity", ls->HumanProximity);
	 if(ls->AvailableProperties.Contains(TCustomBiometricSensor::TProperty::Touch))
		LValues += ToFormStrB("Touch", ls->Touch);

	 return GetFullInfo(GetSensorCategoryName(ASensor->Category), GetTypeNameBio(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutLight(TCustomSensor *ASensor) {
	 String LValues = "";
	 TCustomLightSensor *ls = static_cast<TCustomLightSensor *>(ASensor);
	 if(ls->AvailableProperties.Contains(TCustomLightSensor::TProperty::Lux))
		LValues += ToFormStr("Lux", ls->Lux);
	 if(ls->AvailableProperties.Contains(TCustomLightSensor::TProperty::Temperature))
		LValues += ToFormStr("Temperature", ls->Temperature);
	 if(ls->AvailableProperties.Contains(TCustomLightSensor::TProperty::Chromacity))
		LValues += ToFormStr("Chromacity", ls->Chromacity);

	 return GetFullInfo(GetSensorCategoryName(ASensor->Category),GetTypeNameLight(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
String __fastcall TfrmAboutSensors::GetInfoAboutScanner(TCustomSensor *ASensor) {
	 String LValues = "";
	 TCustomScannerSensor *ls = static_cast<TCustomScannerSensor *>(ASensor);
	 if(ls->AvailableProperties.Contains(TCustomScannerSensor::TProperty::RFIDTag))
		LValues += ToFormStr("RFIDTag", ls->RFIDTag);
	 if(ls->AvailableProperties.Contains(TCustomScannerSensor::TProperty::BarcodeData))
		LValues += ToFormStrS("BarcodeData", ls->BarcodeData);

	 return GetFullInfo(GetSensorCategoryName(ASensor->Category),GetTypeNameScanner(ls->SensorType), ls->ClassName(), LValues);
}
// ---------------------------------------------------------------------------
void __fastcall TfrmAboutSensors::Timer1Timer(TObject *Sender) {
	String ResultText;
	Single LStep;
	if (FActiveSensor != NULL) {
		switch (FActiveSensor->Category) {
		case TSensorCategory::Location:
			ResultText = GetInfoAboutLocation(FActiveSensor);
			break;
		case TSensorCategory::Environmental:
			ResultText = GetInfoAboutEnv(FActiveSensor);
			break;
		case TSensorCategory::Motion:
			ResultText = GetInfoAboutMotion(FActiveSensor);
			break;
		case TSensorCategory::Orientation:
			ResultText = GetInfoAboutOrientation(FActiveSensor);
			break;
		case TSensorCategory::Mechanical:
			ResultText = GetInfoAboutMechanical(FActiveSensor);
			break;
		case TSensorCategory::Electrical:
			ResultText = GetInfoAboutElectro(FActiveSensor);
			break;
		case TSensorCategory::Biometric:
			ResultText = GetInfoAboutBiometric(FActiveSensor);
			break;
		case TSensorCategory::Light:
			ResultText = GetInfoAboutLight(FActiveSensor);
			break;
		case TSensorCategory::Scanner:
			ResultText = GetInfoAboutScanner(FActiveSensor);
			break;
		}
		lInfo->Text = ResultText;
	}
	if (!FOnOneScreen) {
		if (FShowInfo) {
			if (lInfo->Position->Point.X > (cBorder * 2)) {
				LStep = Width / 5;
				lInfo->Position->Point = PointF(lInfo->Position->Point.X - LStep, cBorder);
				lbMain->Position->Point = PointF(lbMain->Position->Point.X - LStep, cBorder);
			}
		}
		else {
			if (lbMain->Position->Point.X < cBorder) {
				LStep = Width / 5;
				lInfo->Position->Point = PointF(lInfo->Position->Point.X + LStep, cBorder);
				lbMain->Position->Point = PointF(lbMain->Position->Point.X + LStep, cBorder);
			}
		}
	}
}
// ---------------------------------------------------------------------------
void __fastcall TfrmAboutSensors::btnHideClick(TObject *Sender) {
	FShowInfo = false;
	if (FActiveSensor != NULL && FActiveSensor->Started)
		FActiveSensor->Stop();
}
//---------------------------------------------------------------------------
void __fastcall TfrmAboutSensors::FormCreate(TObject *Sender) {
	FActiveSensor = NULL;
	FShowInfo = false;
	ReAlignComponents();
	TSensorManager::Current->Activate();
	for (TSensorCategory sCateg = TSensorCategory::Location; sCateg != TSensorCategory::Scanner;
    	sCateg = static_cast<TSensorCategory>(static_cast<int>(sCateg)+1)) {
		CreateIfExists(static_cast<TSensorCategory>(sCateg));
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmAboutSensors::lbMainItemClick(const TCustomListBox *Sender, const TListBoxItem *Item) {
	if (Item->OnClick) {
		TNotifyEvent MyOnClick = Item->OnClick;
		MyOnClick((TObject *)Item);
	}
}
//---------------------------------------------------------------------------
void __fastcall TfrmAboutSensors::FormResize(TObject *Sender) {
	ReAlignComponents();
}
//---------------------------------------------------------------------------
