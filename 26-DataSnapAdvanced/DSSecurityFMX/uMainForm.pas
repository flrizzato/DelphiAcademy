unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    ediUser: TEdit;
    ediPass: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    memResult: TMemo;
    Layout1: TLayout;
    ediAddress: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ediUserChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses ClientModuleUnit3, System.Threading, System.Net.HttpClient.Android;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Task: ITask;
begin
  ediUserChange(Self);
  Task := TTask.Create(
    procedure
    var
      CapturedException: TObject;
    begin
      try
        TThread.Synchronize(nil,
          procedure
          begin
            memResult.Text := ClientModule3.ServerMethods1Client.
              EchoString('123');
          end);
      except
        CapturedException := AcquireExceptionObject;
        TThread.Queue(TThread.CurrentThread,
          procedure
          begin
            raise CapturedException;
          end);
      end;
    end);
  Task.Start;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  Task: ITask;
begin
  ediUserChange(Self);
  Task := TTask.Create(
    procedure
    var
      CapturedException: TObject;
    begin
      try
        TThread.Synchronize(nil,
          procedure
          begin
            memResult.Text := ClientModule3.ServerMethods1Client.
              ReverseString('123');
          end);
      except
        CapturedException := AcquireExceptionObject;
        TThread.Queue(TThread.CurrentThread,
          procedure
          begin
            raise CapturedException;
          end);
      end;
    end);
  Task.Start;
end;

procedure TMainForm.Button3Click(Sender: TObject);
var
  Task: ITask;
begin
  ediUserChange(Self);
  Task := TTask.Create(
    procedure
    var
      CapturedException: TObject;
    begin
      try
        TThread.Synchronize(nil,
          procedure
          begin
            memResult.Text := ClientModule3.ServerMethods1Client.ServerDateTime;
          end);
      except
        CapturedException := AcquireExceptionObject;
        TThread.Queue(TThread.CurrentThread,
          procedure
          begin
            raise CapturedException;
          end);
      end;
    end);
  Task.Start;
end;

procedure TMainForm.ediUserChange(Sender: TObject);
begin
  ClientModule3.DSRestConnection1.UserName := ediUser.Text;
  ClientModule3.DSRestConnection1.Password := ediPass.Text;
  ClientModule3.DSRestConnection1.Host := ediAddress.Text;
end;

end.
