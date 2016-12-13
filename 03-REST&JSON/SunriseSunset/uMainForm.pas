unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Maps, FMX.Layouts, FMX.Controls.Presentation,
  System.Sensors, System.Sensors.Components, IPPeerClient, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope;

type
  TForm2 = class(TForm)
    TopToolBar: TToolBar;
    Label1: TLabel;
    BottomToolBar: TToolBar;
    GridPanelLayout1: TGridPanelLayout;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    MapView1: TMapView;
    Panel1: TPanel;
    Memo1: TMemo;
    LocationSensor1: TLocationSensor;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    procedure FormShow(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure MapView1MapClick(const Position: TMapCoordinate);
  private
    { Private declarations }
    procedure GetSunriseSunset(const Lat, Lng: string);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses System.Threading, REST.Types;

{$R *.fmx}

procedure TForm2.FormShow(Sender: TObject);
begin
  LocationSensor1.Active := True;
end;

procedure TForm2.GetSunriseSunset(const Lat, Lng: string);
var aTask: ITask;
begin
  aTask := TTask.Create(
    procedure ()
    begin
        TThread.Synchronize(nil,
            procedure
            begin
              Form2.Memo1.Lines.Clear;
              Form2.Memo1.Lines.Add('# Latitude: ' + Lat);
              Form2.Memo1.Lines.Add('# Longitude: ' + Lng);
            end
        );

      RESTRequest1.Params[0].Value := Lat;
      RESTRequest1.Params[1].Value := Lng;
      RESTRequest1.Params[2].Value := 'today';

      RESTRequest1.Execute;

      if RESTResponse1.StatusCode = 200 then
        TThread.Synchronize(nil,
            procedure
            begin
              Form2.Memo1.Lines.Add('JSONText: ');
              Form2.Memo1.Lines.Add(Form2.RESTResponse1.JSONText);
            end
        );
    end);
  aTask.Start;
end;

procedure TForm2.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
  mCenter: TMapCoordinate;
begin
  mCenter := TMapCoordinate.Create(NewLocation.Latitude, NewLocation.Longitude);
  MapView1.Location := mCenter;
end;

procedure TForm2.MapView1MapClick(const Position: TMapCoordinate);
var
  MyMarker: TMapMarkerDescriptor;
begin
  MyMarker := TMapMarkerDescriptor.Create(Position, 'MyMarker');
  MyMarker.Draggable := True;
  MyMarker.Visible := True;
  MapView1.AddMarker(MyMarker);

  GetSunriseSunset(Position.Latitude.ToString, Position.Longitude.ToString);
end;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  MapView1.MapType := TMapType.Normal;
end;

procedure TForm2.SpeedButton2Click(Sender: TObject);
begin
  MapView1.MapType := TMapType.Satellite;
end;

procedure TForm2.SpeedButton3Click(Sender: TObject);
begin
  MapView1.MapType := TMapType.Hybrid;
end;

end.
