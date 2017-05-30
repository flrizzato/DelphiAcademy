unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    ediUser: TEdit;
    ediPass: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    memResult: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure ediUserChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses ClientModuleUnit1;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  memResult.Text := ClientModuleUnit1.ClientModule1.ServerMethods1Client.
    EchoString('123');
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  memResult.Text := ClientModuleUnit1.ClientModule1.ServerMethods1Client.
    ReverseString('123');
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  memResult.Text := ClientModuleUnit1.ClientModule1.ServerMethods1Client.
    ServerDateTime;
end;

procedure TMainForm.ediUserChange(Sender: TObject);
begin
  ClientModuleUnit1.ClientModule1.DSRestConnection1.UserName := ediUser.Text;
  ClientModuleUnit1.ClientModule1.DSRestConnection1.Password := ediPass.Text;
end;

end.
