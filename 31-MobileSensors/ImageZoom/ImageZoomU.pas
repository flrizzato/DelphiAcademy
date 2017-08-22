//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ImageZoomU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects,
  FMX.Gestures, FMX.Controls.Presentation;

type
  TPinchZoom = class(TForm)
    Image1: TImage;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FLastDistance: Integer;
  PinchZoom: TPinchZoom;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TPinchZoom.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  LObj: IControl;
  LImage: TImage;
  LImageCenter: TPointF;
begin
  if EventInfo.GestureID = igiZoom then
  begin
    LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
    if LObj is TImage then
    begin
      if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and
        (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then
      begin
        { zoom the image }
        LImage := TImage(LObj.GetObject);
        LImageCenter := LImage.Position.Point + PointF(LImage.Width / 2,
          LImage.Height / 2);
        LImage.Width := LImage.Width + (EventInfo.Distance - FLastDistance);
        LImage.Height := LImage.Height + (EventInfo.Distance - FLastDistance);
        LImage.Position.X := LImageCenter.X - LImage.Width / 2;
        LImage.Position.Y := LImageCenter.Y - LImage.Height / 2;
      end;
      FLastDistance := EventInfo.Distance;
    end;
  end;
end;

end.
