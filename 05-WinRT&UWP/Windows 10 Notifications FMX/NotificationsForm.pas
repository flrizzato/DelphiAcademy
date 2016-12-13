//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit NotificationsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  System.Notification, FMX.ScrollBox, FMX.Memo;

type
  TNotify = class(TForm)
    btnShow: TButton;
    NotificationCenter1: TNotificationCenter;
    btnCancelAll: TButton;
    btnShowAnother: TButton;
    btnCancelAnother: TButton;
    btnCancel: TButton;
    mmLog: TMemo;
    lblLog: TLabel;
    StyleBook1: TStyleBook;
    procedure btnShowClick(Sender: TObject);
    procedure NotificationCenter1ReceiveLocalNotification(Sender: TObject; ANotification: TNotification);
    procedure btnCancelAllClick(Sender: TObject);
    procedure btnShowAnotherClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCancelAnotherClick(Sender: TObject);
  private
    { Private declarations }
    //FNotificationCenter: TNotificationCenter;
  public
    { Public declarations }
  end;

var
  Notify: TNotify;

implementation

{$R *.fmx}

procedure TNotify.btnShowClick(Sender: TObject);
var
  MyNotification: TNotification;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Name := 'Windows10Notification';
    MyNotification.Title := 'Windows 10 Notification #1';
    MyNotification.AlertBody := 'RAD Studio 10 Seattle';

    NotificationCenter1.PresentNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
end;

procedure TNotify.btnCancelAllClick(Sender: TObject);
begin
  NotificationCenter1.CancelAll;
end;

procedure TNotify.btnShowAnotherClick(Sender: TObject);
var
  MyNotification: TNotification;
begin
  MyNotification := NotificationCenter1.CreateNotification;
  try
    MyNotification.Name := 'Windows10Notification2';
    MyNotification.Title := 'Windows 10 Notification #2';
    MyNotification.AlertBody := 'RAD Studio 10 Seattle';

    NotificationCenter1.PresentNotification(MyNotification);
  finally
    MyNotification.Free;
  end;
end;

procedure TNotify.btnCancelAnotherClick(Sender: TObject);
begin
  NotificationCenter1.CancelNotification('Windows10Notification2');
end;

procedure TNotify.btnCancelClick(Sender: TObject);
begin
  NotificationCenter1.CancelNotification('Windows10Notification');
end;

procedure TNotify.NotificationCenter1ReceiveLocalNotification(Sender: TObject; ANotification: TNotification);
begin
  mmLog.Lines.Add('Notification received: ' + ANotification.Name);
end;

end.
