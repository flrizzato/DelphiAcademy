unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  Data.Bind.Controls, Data.Bind.EngExt, FMX.Bind.DBEngExt, FMX.Bind.Grid,
  System.Bindings.Outputs, FMX.Bind.Editors, IPPeerClient, IPPeerServer,
  FMX.StdCtrls, System.Tether.Manager, System.Tether.AppProfile,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, Data.DB,
  Datasnap.DBClient, FMX.Layouts, FMX.Bind.Navigator, FMX.Grid,
  FMX.Controls.Presentation, FMX.Grid.Style, FMX.ScrollBox;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    BindNavigator1: TBindNavigator;
    ClientDataSet1: TClientDataSet;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    butSend: TButton;
    butLoad: TButton;
    butConnect: TButton;
    procedure butConnectClick(Sender: TObject);
    procedure TetheringManager1EndAutoConnect(Sender: TObject);
    procedure butLoadClick(Sender: TObject);
    procedure butSendClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.butConnectClick(Sender: TObject);
begin
  TetheringManager1.AutoConnect;
end;

procedure TForm2.butLoadClick(Sender: TObject);
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  st.LoadFromStream(TetheringAppProfile1.GetRemoteResourceValue
    (TetheringManager1.RemoteProfiles.First, 'MyDataSet').Value.AsStream);
  ClientDataSet1.LoadFromStream(st);
end;

procedure TForm2.butSendClick(Sender: TObject);
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  ClientDataSet1.SaveToStream(st, dfBinary);
  TetheringAppProfile1.SendStream(TetheringManager1.RemoteProfiles.First,
    'MyDataSet', st);
end;

procedure TForm2.TetheringManager1EndAutoConnect(Sender: TObject);
begin
  if TetheringAppProfile1.Connect(TetheringManager1.RemoteProfiles.First) then
  begin
    butLoad.Enabled := True;
    butSend.Enabled := True;
  end;
end;

end.
