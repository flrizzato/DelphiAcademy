//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.TabControl,
  FMX.Gestures, FMX.StdCtrls, System.Actions, FMX.ActnList,
  FMX.Controls.Presentation;

type
  TTabSlidingForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Image1: TImage;
    Image2: TImage;
    GestureManager1: TGestureManager;
    ToolBar1: TToolBar;
    Label1: TLabel;
    TabActionList: TActionList;
    ChangeTabActionNext: TNextTabAction;
    ChangeTabActionPrev: TPreviousTabAction;
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TabSlidingForm: TTabSlidingForm;

implementation

{$R *.fmx}

{ Capture the back button; if there are tabs to the left of current,
  navigate back, otherwise "fall off." }

procedure TTabSlidingForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkHardwareBack) and (TabControl1.TabIndex > 0) then
  begin
    TabControl1.TabIndex := TabControl1.TabIndex - 1 mod TabControl1.TabCount;
    Key := 0;
  end;
end;

end.
