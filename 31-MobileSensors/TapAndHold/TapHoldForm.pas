//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit TapHoldForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Gestures, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TTapHold = class(TForm)
    Image1: TImage;
    ToolBar1: TToolBar;
    Title: TLabel;
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TapHold: TTapHold;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TTapHold.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  { if a long tap gesture is detected }
  if EventInfo.GestureID = System.UITypes.igiLongTap then
    { show a message }
    Title.Text := Format('LongTap at %d, %d seen %s',
               [Round(EventInfo.Location.X), Round(EventInfo.Location.Y),
                FormatDateTime('nn:ss',Now)]);
end;

end.
