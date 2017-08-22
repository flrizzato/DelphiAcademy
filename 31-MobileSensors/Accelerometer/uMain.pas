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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, System.Sensors, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, System.Sensors.Components, FMX.Controls.Presentation;

type
  TAccelerometerForm = class(TForm)
    swAccelerometerSensorActive: TSwitch;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListBox1: TListBox;
    lbAccelerometerSensor: TListBoxItem;
    lbAccelerationX: TListBoxItem;
    lbAccelerationY: TListBoxItem;
    lbAccelerationZ: TListBoxItem;
    lbAngleAccelX: TListBoxItem;
    lbAngleAccelY: TListBoxItem;
    lbAngleAccelZ: TListBoxItem;
    lbMotion: TListBoxItem;
    lbSpeed: TListBoxItem;
    MotionSensor1: TMotionSensor;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure swAccelerometerSensorActiveSwitch(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AccelerometerForm: TAccelerometerForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TAccelerometerForm.FormCreate(Sender: TObject);
begin
    // start with no sensors

  lbAccelerationX.Visible := False;
  lbAccelerationY.Visible := False;
  lbAccelerationZ.Visible := False;
  lbAngleAccelX.Visible := False;
  lbAngleAccelY.Visible := False;
  lbAngleAccelZ.Visible := False;
  lbMotion.Visible := False;
  lbSpeed.Visible := False;

{$ifdef IOS}
  {$ifndef CPUARM}
    lbAccelerometerSensor.Text := 'Simulator - no sensors';
    swAccelerometerSensorActive.Enabled := False;
  {$endif}
{$endif}

end;

procedure TAccelerometerForm.swAccelerometerSensorActiveSwitch(Sender: TObject);
begin
  { activate or deactivate the reading of the accelerometer sensor }
  MotionSensor1.Active := swAccelerometerSensorActive.IsChecked;
  Timer1.Enabled := swAccelerometerSensorActive.IsChecked;
end;

procedure TAccelerometerForm.Timer1Timer(Sender: TObject);
var
  LProp: TCustomMotionSensor.TProperty;

begin
  for LProp in MotionSensor1.Sensor.AvailableProperties do
  begin
    { get the data from the sensor }
    case LProp of
      TCustomMotionSensor.TProperty.AccelerationX:
      begin
        lbAccelerationX.Visible := True;
        lbAccelerationX.Text := Format('Acceleration X: %6.2f', [MotionSensor1.Sensor.AccelerationX]);
      end;
      TCustomMotionSensor.TProperty.AccelerationY:
      begin
        lbAccelerationY.Visible := True;
        lbAccelerationY.Text := Format('Acceleration Y: %6.2f', [MotionSensor1.Sensor.AccelerationY]);
      end;
      TCustomMotionSensor.TProperty.AccelerationZ:
      begin
        lbAccelerationZ.Visible := True;
        lbAccelerationZ.Text := Format('Acceleration Z: %6.2f', [MotionSensor1.Sensor.AccelerationZ]);
      end;
      TCustomMotionSensor.TProperty.AngleAccelX:
      begin
        lbAngleAccelX.Visible := True;
        lbAngleAccelX.Text := Format('Angle X: %6.2f', [MotionSensor1.Sensor.AngleAccelX]);
      end;
      TCustomMotionSensor.TProperty.AngleAccelY:
      begin
        lbAngleAccelY.Visible := True;
        lbAngleAccelY.Text := Format('Angle Y: %6.2f', [MotionSensor1.Sensor.AngleAccelY]);
      end;
      TCustomMotionSensor.TProperty.AngleAccelZ:
      begin
        lbAngleAccelZ.Visible := True;
        lbAngleAccelZ.Text := Format('Angle Z: %6.2f', [MotionSensor1.Sensor.AngleAccelZ]);
      end;
      TCustomMotionSensor.TProperty.Motion:
      begin
        lbMotion.Visible := True;
        lbMotion.Text := Format('Motion: %6.2f', [MotionSensor1.Sensor.Motion]);
      end;
      TCustomMotionSensor.TProperty.Speed:
      begin
        lbSpeed.Visible := True;
        lbSpeed.Text := Format('Speed: %6.2f', [MotionSensor1.Sensor.Speed]);
      end;
    end;
  end;
end;

end.
