unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Sensors, Vcl.StdCtrls,
  System.Sensors.Components;

type
  TForm1 = class(TForm)
    LocationSensor1: TLocationSensor;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.TypInfo, Winapi.ShellAPI;

const
  GoogleMapsURL: String = 'https://maps.google.com/maps?q=%s,%s&output=embed';

procedure TForm1.Button1Click(Sender: TObject);
var
  MyLocationSensorArray: TSensorArray;
  MyLocationSensor: TCustomLocationSensor;
begin
  Memo1.Lines.Clear;
  try
    TSensorManager.Current.Activate; // activate sensor manager
    MyLocationSensorArray := TSensorManager.Current.GetSensorsByCategory
      (TSensorCategory.Location);
    if MyLocationSensorArray <> nil then
    begin
      Memo1.Lines.Add('Location Sensor Found!');
      MyLocationSensor := MyLocationSensorArray[0] as TCustomLocationSensor;
      MyLocationSensor.Start;
      Memo1.Lines.Add('Latitude: ' + FloatToStr(MyLocationSensor.Latitude));
      Memo1.Lines.Add('Longitude: ' + FloatToStr(MyLocationSensor.Longitude));
      MyLocationSensor.Stop;
      ShellExecute(handle, 'open',
        PChar(Format(GoogleMapsURL, [MyLocationSensor.Latitude.ToString,
        MyLocationSensor.Longitude.ToString])), '', '', SW_SHOWNORMAL);
    end
    else
    begin
      Memo1.Lines.Add('Location Sensor Not Found!');
    end;
  finally
    TSensorManager.Current.DeActivate // deactivate sensor manager
  end;
end;

end.
