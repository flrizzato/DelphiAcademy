unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, IPPeerClient, REST.Backend.PushTypes, System.JSON,
  REST.Backend.EMSPushDevice, System.PushNotification, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Backend.BindSource, REST.Backend.PushDevice,
  REST.Backend.EMSProvider, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    EMSProvider1: TEMSProvider;
    PushEvents1: TPushEvents;
    Memo1: TMemo;
    Switch1: TSwitch;
    procedure PushEvents1DeviceRegistered(Sender: TObject);
    procedure PushEvents1DeviceTokenReceived(Sender: TObject);
    procedure PushEvents1DeviceTokenRequestFailed(Sender: TObject;
      const AErrorMessage: string);
    procedure PushEvents1PushReceived(Sender: TObject; const AData: TPushData);
    procedure Switch1Switch(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses System.Threading;

{$R *.fmx}

procedure TMainForm.PushEvents1DeviceRegistered(Sender: TObject);
begin
  Memo1.Lines.Add('Device Registered');
  Memo1.Lines.Add('');
end;

procedure TMainForm.PushEvents1DeviceTokenReceived(Sender: TObject);
begin
  Memo1.Lines.Add('Device Token Received');
  Memo1.Lines.Add('');
end;

procedure TMainForm.PushEvents1DeviceTokenRequestFailed(Sender: TObject;
  const AErrorMessage: string);
begin
  Memo1.Lines.Add('Device Token Request Failed');
  Memo1.Lines.Add(AErrorMessage);
  Memo1.Lines.Add('');
end;

procedure TMainForm.PushEvents1PushReceived(Sender: TObject;
  const AData: TPushData);
begin
  Memo1.Lines.Add('Device push received');
  Memo1.Lines.Add(AData.Message);
  Memo1.Lines.Add('');
end;

procedure TMainForm.Switch1Switch(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      PushEvents1.Active := Switch1.IsChecked;
    end);
end;

end.
