unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Threading, System.Bluetooth;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    memLog: TMemo;
    butDevices: TButton;
    butStart: TButton;
    butSend: TButton;
    CBDevices: TComboBox;
    edtMessage: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure butDevicesClick(Sender: TObject);
    procedure butStartClick(Sender: TObject);
    procedure butSendClick(Sender: TObject);
  private
    { Private declarations }
    aTask: ITask;

    Client_UUID: TGUID;
    Server_UUID: TGUID;

    BTServerSocket: TBluetoothServerSocket;
    BTClientSocket: TBluetoothSocket;

    BTMyDevice: TBluetoothDevice;

    procedure Send2Log(sLog: string);
    function FindBTDevices(DeviceName: string): TBluetoothDevice;

    procedure ExecuteServer;
    procedure BTSendMessage;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

const
  cSleepTime: integer = 100;
  cWaitTime: integer = 300000;

implementation

{$R *.dfm}
{ TMainForm }

procedure TMainForm.BTSendMessage;
var
  BTMySocket: TBluetoothSocket;
  ToSend, Received: TBytes;
begin
  ToSend := TEncoding.UTF8.GetBytes(edtMessage.Text);

  if BTMyDevice = nil then
  begin
    BTMyDevice := FindBTDevices(CBDevices.Text);
    if BTMyDevice = nil then
    begin
      ShowMessage('Device not found!');
      Exit;
    end;
  end;

  BTMySocket := BTMyDevice.CreateClientSocket(Client_UUID, True);
  try
    try
      Send2Log('Sending: ' + edtMessage.Text);
      BTMySocket.Connect;
      BTMySocket.SendData(ToSend);
      Sleep(cSleepTime);
      Received := BTMySocket.ReceiveData;
      Send2Log('Received: ' + TEncoding.UTF8.GetString(Received));
    except
      on E: Exception do
        Send2Log('Exception raised sending data: ' + E.ClassName + ' ' +
          E.Message)
    end;
  finally
    BTMySocket.Close;
    FreeAndNil(BTMySocket);
  end;
end;

procedure TMainForm.butDevicesClick(Sender: TObject);
var
  i: Integer;
  BTDeviceList: TBluetoothDeviceList;
begin
  CBDevices.Items.Clear;
  BTDeviceList := TBluetoothManager.Current.CurrentAdapter.PairedDevices;
  for i := 0 to BTDeviceList.Count - 1 do
    CBDevices.Items.Add(BTDeviceList.Items[i].DeviceName);
end;

procedure TMainForm.butSendClick(Sender: TObject);
begin
  BTSendMessage;
end;

procedure TMainForm.butStartClick(Sender: TObject);
begin
  if butStart.Caption = 'Start' then
  begin
    ExecuteServer;
    butStart.Caption := 'Stop';
  end
  else
  begin
    aTask.Cancel;
    butStart.Caption := 'Start';
  end;
end;

procedure TMainForm.ExecuteServer;
var
  Buff: TBytes;
  ReceivedText: string;
begin
  BTServerSocket := TBluetoothManager.Current.CurrentAdapter.CreateServerSocket
    ('ATest Service', Server_UUID, False);

  aTask := TTask.Create(
    procedure()
    begin
      while True do
        try
          if aTask.Status = TTaskStatus.Canceled then
            Exit;
          Send2Log('Listening...');
          BTClientSocket := BTServerSocket.Accept(cWaitTime);
          Send2Log('Connection established or timeout...');
          if BTClientSocket <> nil then
          begin
            Buff := BTClientSocket.ReceiveData;
            ReceivedText := TEncoding.UTF8.GetString(Buff);
            Send2Log('Received: ' + ReceivedText);
            if ReceivedText.Length > 0 then
              BTClientSocket.SendData(TEncoding.UTF8.GetBytes('Echo ' +
                ReceivedText));
            Sleep(500);
            BTClientSocket.Close;
            Send2Log('End receive block...');
          end
          else
            Send2Log('Client Socket is nil');
        except
          on E: Exception do
          begin
            Send2Log('Exception: ' + E.Message);
          end;
        end;
    end);
  aTask.Start;
end;

function TMainForm.FindBTDevices(DeviceName: string): TBluetoothDevice;
var
  i: Integer;
  BTDeviceList: TBluetoothDeviceList;
begin
  BTDeviceList := TBluetoothManager.Current.CurrentAdapter.PairedDevices;
  for i := 0 to BTDeviceList.Count - 1 do
    if BTDeviceList.Items[i].DeviceName = DeviceName then
      Exit(BTDeviceList.Items[i]);
  Result := nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BTMyDevice := nil;

  BTServerSocket := nil;
  BTClientSocket := nil;

  Client_UUID := StringToGuid('{14800546-CF05-481F-BE41-4EC0246D862D}');
  Server_UUID := StringToGuid('{14800546-CF05-481F-BE41-4EC0246D862D}')
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(BTClientSocket);
  FreeAndNil(BTServerSocket);
end;

procedure TMainForm.Send2Log(sLog: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      memLog.Lines.Add(sLog);
      Application.ProcessMessages;
    end);
end;

end.
