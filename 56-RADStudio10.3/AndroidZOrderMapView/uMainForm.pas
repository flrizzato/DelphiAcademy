unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Maps,
  FMX.Objects, FMX.Layouts, System.ImageList, FMX.ImgList,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TMainForm = class(TForm)
    MapView1: TMapView;
    butLandmark1: TButton;
    butLandmark2: TButton;
    ImageList1: TImageList;
    butLandmark3: TButton;
    Layout1: TLayout;
    Layout2: TLayout;
    StyleBook1: TStyleBook;
    procedure butLandmark1Click(Sender: TObject);
    procedure butLandmark2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure butLandmark3Click(Sender: TObject);
  private
    { Private declarations }
    procedure Navigate(Latitude, Longitude: Double; Name: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.iPhone55in.fmx IOS}

procedure TMainForm.Navigate(Latitude, Longitude: Double; Name: string);
begin
  var mapNavigate := TMapCoordinate.Create(Latitude, Longitude);
  MapView1.Location := mapNavigate;

  var MyMarker := TMapMarkerDescriptor.Create(mapNavigate, Name);
  MyMarker.Draggable := False;
  MyMarker.Visible := True;
  MapView1.AddMarker(MyMarker);
end;

procedure TMainForm.butLandmark1Click(Sender: TObject);
begin
  Navigate(-22.951916, -43.2126759, 'Christ Redeemer');
end;

procedure TMainForm.butLandmark2Click(Sender: TObject);
begin
  Navigate(-22.9756488, -43.1907708, 'Copacabana Beach');
end;

procedure TMainForm.butLandmark3Click(Sender: TObject);
begin
  Navigate(38.4823868, 22.4987812, 'Delphi, Greece');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  butLandmark3Click(Self);
  MapView1.Zoom := 14;
end;

end.
