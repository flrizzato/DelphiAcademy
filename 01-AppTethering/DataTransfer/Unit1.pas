unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  Data.Bind.Controls, Data.Bind.EngExt, FMX.Bind.DBEngExt, FMX.Bind.Grid,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, FMX.Layouts, FMX.Bind.Navigator, FMX.Grid,
  Data.DB, Datasnap.DBClient, IPPeerClient, IPPeerServer, System.Tether.Manager,
  System.Tether.AppProfile, System.IOUtils, FMX.Grid.Style,
  FMX.Controls.Presentation, FMX.ScrollBox;

type
  TForm1 = class(TForm)
    ClientDataSet1: TClientDataSet;
    StringGrid1: TStringGrid;
    BindNavigator1: TBindNavigator;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    procedure ClientDataSet1AfterPost(DataSet: TDataSet);
    procedure ClientDataSet1AfterOpen(DataSet: TDataSet);
    procedure FormShow(Sender: TObject);
    procedure TetheringAppProfile1ResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.ClientDataSet1AfterOpen(DataSet: TDataSet);
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  ClientDataSet1.SaveToStream(st, dfBinary);
  st.Position := 0;

  TetheringAppProfile1.Resources.FindByName('MyDataSet').Value := st;
end;

procedure TForm1.ClientDataSet1AfterPost(DataSet: TDataSet);
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  ClientDataSet1.SaveToStream(st, dfBinary);
  st.Position := 0;

  TetheringAppProfile1.Resources.FindByName('MyDataSet').Value := st;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  s: string;
begin
  s := TPath.GetDocumentsPath + PathDelim + 'MyDataSet.xml';
  if FileExists(s) then
    try
      ClientDataSet1.LoadFromFile(s);
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
end;

procedure TForm1.TetheringAppProfile1ResourceReceived(const Sender: TObject;
  const AResource: TRemoteResource);
var st: TMemoryStream;
begin
  if AResource.Hint = 'MyDataSet' then
  begin
    st := TMemoryStream.Create;
    st.LoadFromStream(AResource.Value.AsStream);
    ClientDataSet1.Close;
    ClientDataSet1.LoadFromStream(st);
  end;
end;

end.
