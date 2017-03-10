unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, Vcl.StdCtrls,
  REST.Backend.EMSProvider, REST.Backend.ServiceTypes, System.JSON,
  REST.Backend.EMSServices, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Client, REST.Backend.EndPoint, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, REST.Backend.MetaTypes,
  REST.Backend.BindSource, REST.Backend.ServiceComponents;

type
  TForm1 = class(TForm)
    EMSProvider1: TEMSProvider;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Memo1: TMemo;
    BackendEndpoint1: TBackendEndpoint;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    BackendAuth1: TBackendAuth;
    Button7: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure BackendAuth1LoggedIn(Sender: TObject);
    procedure BackendAuth1LoggedOut(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Rest.Types;

{$R *.dfm}

procedure TForm1.BackendAuth1LoggedIn(Sender: TObject);
begin
  Memo1.Lines.Text := 'User successfully logged in!'
end;

procedure TForm1.BackendAuth1LoggedOut(Sender: TObject);
begin
  Memo1.Lines.Text := 'User successfully logged out!'
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BackendAuth1.UserName := Edit1.Text;
  BackendAuth1.Password := Edit2.Text;
  BackendAuth1.Login;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BackendEndpoint1.Method := TRESTRequestMethod.rmGET;
  BackendEndpoint1.ResourceSuffix := '';
  BackendEndpoint1.Execute;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  BackendEndpoint1.Method := TRESTRequestMethod.rmGET;
  BackendEndpoint1.ResourceSuffix := '100';
  BackendEndpoint1.Execute;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  BackendEndpoint1.Method := TRESTRequestMethod.rmPOST;
  BackendEndpoint1.ResourceSuffix := '';
  BackendEndpoint1.Execute;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  BackendEndpoint1.Method := TRESTRequestMethod.rmPUT;
  BackendEndpoint1.ResourceSuffix := '100';
  BackendEndpoint1.Execute;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  BackendEndpoint1.Method := TRESTRequestMethod.rmDELETE;
  BackendEndpoint1.ResourceSuffix := '100';
  BackendEndpoint1.Execute;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  BackendAuth1.Logout;
end;

end.
