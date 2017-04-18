unit TaskFutureUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  IPPeerClient, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  FMX.Layouts, FMX.Memo, FMX.Controls.Presentation, FMX.Edit,

  System.Threading, FMX.ScrollBox;

type
  TFormMain = class(TForm)
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    btnBlocking: TButton;
    editArtist: TEdit;
    memoResults: TMemo;
    btnTask: TButton;
    Clear: TButton;
    btnLaunch: TButton;
    btnRead: TButton;
    GroupBox1: TGroupBox;
    lblTask: TLabel;
    Label1: TLabel;
    lblFuture: TLabel;
    btnFutureCancel: TButton;
    btnTaskCancel: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure btnBlockingClick(Sender: TObject);
    procedure btnTaskClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnFutureCancelClick(Sender: TObject);
    procedure btnTaskCancelClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
  private
    function DoRESTCall(aArtist: string): string;
    procedure UpdateTaskStatus;
    procedure UpdateFutureStatus;
    procedure TaskCancel;
    procedure FutureCancel;
    { Private declarations }
  public
    { Public declarations }
    FutureString: IFuture<string>;
    TaskStatus: ITask;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

Uses TypInfo;

procedure TFormMain.btnLaunchClick(Sender: TObject);
begin
  memoResults.Lines.Clear;
  FutureString := TTask.Future<string>(
    function:string
    begin
      Result := DoRESTCall(editArtist.Text);
    end);
end;

procedure TFormMain.btnReadClick(Sender: TObject);
begin
  memoResults.Text := FutureString.Value;
end;

procedure TFormMain.btnTaskCancelClick(Sender: TObject);
begin
  TaskCancel;
end;

procedure TFormMain.btnTaskClick(Sender: TObject);
begin
  memoResults.Lines.Clear;
  TaskStatus := TTask.Create(procedure
  var
    Text: String;
  begin
    Text := DoRESTCall(editArtist.Text); // Do the work

    if TaskStatus.Status = TTaskStatus.Running then
    begin
      // Post back to the UI thread
      TThread.Queue(TThread.CurrentThread, procedure
      begin
        if Assigned(memoResults) then
          memoResults.Text := Text;
      end);
    end;
  end);

  UpdateTaskStatus;
  Application.ProcessMessages;
  //Sleep(500); // If you want to see the state before it starts

  TaskStatus.Start; // Start the task
end;

procedure TFormMain.ClearClick(Sender: TObject);
begin
  memoResults.Lines.Clear;
end;

procedure TFormMain.btnFutureCancelClick(Sender: TObject);
begin
  FutureCancel;
end;

function TFormMain.DoRESTCall(aArtist: string): string;
var
  lRESTClient: TRESTClient;
  lRESTRequest: TRESTRequest;
  lRESTResponse:TRESTResponse;
begin
  lRESTClient := TRESTClient.Create('http://api.discogs.com');
  lRESTRequest := TRESTRequest.Create(nil);
  lRESTResponse := TRESTResponse.Create(nil);
  lRESTRequest.Client := lRESTClient;
  lRESTRequest.Response := lRESTResponse;
  lRESTRequest.Resource := 'artists/{ID}';
  lRESTRequest.Params[0].Value := aArtist;
  Sleep(1000); // Cheating to make it longer for presentation purposes
  try
    lRESTRequest.Execute;
    Result := lRESTResponse.Content;
  finally {Free resources within thread execution}
    lRESTResponse.Free;
    lRESTRequest.Free;
    lRESTClient.Free;
  end;
end;

procedure TFormMain.UpdateFutureStatus;
begin
  if Assigned(FutureString) then
    lblFuture.Text := GetEnumName(TypeInfo(TTaskStatus),
                        integer(FutureString.Status))
  else
    lblFuture.Text := 'Unassigned';
end;

procedure TFormMain.TaskCancel;
begin
  if Assigned(TaskStatus) then
  begin
    TaskStatus.Cancel;
  end;
end;

procedure TFormMain.FutureCancel;
begin
  if Assigned(FutureString) then
  begin
    FutureString.Cancel;
  end;
end;

procedure TFormMain.UpdateTaskStatus;
begin
  if Assigned(TaskStatus) then
    lblTask.Text := GetEnumName(TypeInfo(TTaskStatus), integer(TaskStatus.Status))
  else
    lblTask.Text := 'Unassigned';
end;

procedure TFormMain.btnBlockingClick(Sender: TObject);
begin
  memoResults.Lines.Clear;
  Application.ProcessMessages;
  memoResults.Text := DoRESTCall(editArtist.Text);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  ProgressBar1.Value := ProgressBar1.Value + 1;
  if ProgressBar1.Value >= ProgressBar1.Max then
    ProgressBar1.Value := 0;
  UpdateTaskStatus;
  UpdateFutureStatus;
end;

end.
