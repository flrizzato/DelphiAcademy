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
  System.Sensors, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, System.Sensors.Components, FMX.Controls.Presentation;

type
  TOrientationSensorForm = class(TForm)
    OrientationSensor1: TOrientationSensor;
    swOrientationSensorActive: TSwitch;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListBox1: TListBox;
    lbOrientationSensor: TListBoxItem;
    lbTiltX: TListBoxItem;
    lbTiltY: TListBoxItem;
    lbTiltZ: TListBoxItem;
    lbHeadingX: TListBoxItem;
    lbHeadingY: TListBoxItem;
    lbHeadingZ: TListBoxItem;
    Layout1: TLayout;
    TiltButton: TSpeedButton;
    HeadingButton: TSpeedButton;
    Timer1: TTimer;
    procedure swOrientationSensorActiveSwitch(Sender: TObject);
    procedure OrientationSensor1SensorChoosing(Sender: TObject;
      const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
    procedure TiltButtonClick(Sender: TObject);
    procedure HeadingButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OrientationSensorForm: TOrientationSensorForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TOrientationSensorForm.OrientationSensor1SensorChoosing(
  Sender: TObject; const Sensors: TSensorArray; var ChoseSensorIndex: Integer);
var
  I: Integer;
  Found: Integer;
begin
  Found := -1;
  for I := 0 to High(Sensors) do
  begin
    if TiltButton.IsPressed and (TCustomOrientationSensor.TProperty.TiltX in TCustomOrientationSensor(Sensors[I]).AvailableProperties) then
    begin
        Found := I;
        Break;
    end
    else if HeadingButton.IsPressed and (TCustomOrientationSensor.TProperty.HeadingX in TCustomOrientationSensor(Sensors[I]).AvailableProperties) then
    begin
      Found := I;
      Break;
    end;
  end;

  if Found < 0 then
  begin
    Found := 0;
    if TiltButton.IsPressed then
      ShowMessage('Inclinometer not available');
    if HeadingButton.IsPressed then
      ShowMessage('Compass not available');
  end
  else
    ChoseSensorIndex := Found;
end;

procedure TOrientationSensorForm.TiltButtonClick(Sender: TObject);
begin
  OrientationSensor1.Active := False;
  HeadingButton.IsPressed := False;
  TiltButton.IsPressed := True;
  OrientationSensor1.Active := swOrientationSensorActive.IsChecked;
end;

procedure TOrientationSensorForm.Timer1Timer(Sender: TObject);
begin
  { get the data from the sensor }
  lbTiltX.Text := Format('Tilt X: %f', [OrientationSensor1.Sensor.TiltX]);
  lbTiltY.Text := Format('Tilt Y: %f', [OrientationSensor1.Sensor.TiltY]);
  lbTiltZ.Text := Format('Tilt Z: %f', [OrientationSensor1.Sensor.TiltZ]);
  lbHeadingX.Text := Format('Heading X: %f', [OrientationSensor1.Sensor.HeadingX]);
  lbHeadingY.Text := Format('Heading Y: %f', [OrientationSensor1.Sensor.HeadingY]);
  lbHeadingZ.Text := Format('Heading Z: %f', [OrientationSensor1.Sensor.HeadingZ]);
end;

procedure TOrientationSensorForm.FormActivate(Sender: TObject);
begin
{$ifdef IOS}
  {$ifndef CPUARM}
    lbOrientationSensor.Text := 'Simulator - no sensors';
    swOrientationSensorActive.Enabled := False;
  {$endif}
{$endif}
end;

procedure TOrientationSensorForm.HeadingButtonClick(Sender: TObject);
begin
  OrientationSensor1.Active := False;
  TiltButton.IsPressed := False;
  HeadingButton.IsPressed := True;
  OrientationSensor1.Active := swOrientationSensorActive.IsChecked;
end;

procedure TOrientationSensorForm.swOrientationSensorActiveSwitch(
  Sender: TObject);
begin
  { activate or deactivate the orientation sensor }
  OrientationSensor1.Active := swOrientationSensorActive.IsChecked;
  Timer1.Enabled := swOrientationSensorActive.IsChecked;
end;

end.
