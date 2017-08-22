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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Forms, FMX.Dialogs, System.Sensors,
  FMX.Controls3D, FMX.Objects3D, FMX.StdCtrls, FMX.Layers3D,
  FMX.MaterialSources, FMX.Types3D, System.Math.Vectors,
  FMX.Controls.Presentation;

type
  TGyroscopeForm = class(TForm3D)
    Rectangle3D1: TRectangle3D;
    Timer1: TTimer;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Layer3D1: TLayer3D;
    Label1: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
  private
    { Private declarations }
    FSensor: TCustomOrientationSensor;
    FSensors: TSensorArray;
    FSensorManager: TSensorManager;
  public
    { Public declarations }
  end;

var
  GyroscopeForm: TGyroscopeForm;

implementation

{$R *.fmx}

procedure TGyroscopeForm.Form3DCreate(Sender: TObject);
var
  Sensor: TCustomSensor;
begin
  { attempt to get and activate the sensor manager }
  FSensorManager := TSensorManager.Current;
  FSensorManager.Activate;

  { attempt to get an orientation sensor }
  FSensors := TSensorManager.Current.GetSensorsByCategory(TSensorCategory.Orientation);


  FSensor := nil;
  for Sensor in FSensors do
    if TCustomOrientationSensor(Sensor).SensorType = TOrientationSensorType.Inclinometer3D then
    begin
      FSensor := TCustomOrientationSensor(Sensor);
      Break;
    end;

  if not Assigned(FSensor) then
  begin
    Label1.Text := 'Gyro not found';
    Exit; { no orientation sensor is available }
  end;

  { start the sensor if it is not started }
  if not FSensor.Started then
  begin
    FSensor.Start;
    Timer1.Enabled := True;
  end;
end;

procedure TGyroscopeForm.Timer1Timer(Sender: TObject);
begin
  { check for sensor assignment }
  if Length(FSensors) > 0 then
    if Assigned(FSensor) then
    begin
      { and rotate the cube }

      {$IFDEF ANDROID} //In Android, Tilt property is returned as vector
        Rectangle3D1.RotationAngle.X := FSensor.TiltX * 360;
        Rectangle3D1.RotationAngle.Y := FSensor.TiltY * 360;
        Rectangle3D1.RotationAngle.Z := FSensor.TiltZ * 360;
      {$ELSE} //In other platforms, Tilt property is returned as degree
        Rectangle3D1.RotationAngle.X := FSensor.TiltX;
        Rectangle3D1.RotationAngle.Y := FSensor.TiltY;
        Rectangle3D1.RotationAngle.Z := FSensor.TiltZ;
      {$ENDIF}

      Label1.Text := Format('Gyro: %3.1f %3.1f %3.1f',[Rectangle3D1.RotationAngle.X,
        Rectangle3D1.RotationAngle.Y,
        Rectangle3D1.RotationAngle.Z]);
    end;
end;

end.
