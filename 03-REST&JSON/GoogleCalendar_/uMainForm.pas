unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Calendar, FMX.WebBrowser,
  FMX.MultiView, System.Actions, FMX.ActnList, System.Rtti, FMX.Grid.Style,
  Data.Bind.EngExt, FMX.Bind.DBEngExt, FMX.Bind.Grid, System.Bindings.Outputs,
  FMX.Bind.Editors, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope,
  FMX.ScrollBox, FMX.Grid, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    MultiView1: TMultiView;
    butLogin: TButton;
    WebBrowser1: TWebBrowser;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    ListView1: TListView;
    LinkFillControlToField2: TLinkFillControlToField;
    procedure MultiView1Shown(Sender: TObject);
    procedure WebBrowser1DidFinishLoad(ASender: TObject);
  private
    { Private declarations }
    fAuthCode: string;
    fAuthToken: string;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses uMainDM;

procedure TMainForm.MultiView1Shown(Sender: TObject);
begin
  fAuthCode := '';
  fAuthToken := '';
  MultiView1.Width := 400;
  WebBrowser1.Navigate(MainDM.BuildLoginURL);
end;

procedure TMainForm.WebBrowser1DidFinishLoad(ASender: TObject);
begin
  fAuthCode := MainDM.ExtractAuthCode(WebBrowser1.URL);
  if fAuthCode.Length > 0 then
  begin
    fAuthToken := MainDM.RequestAuthToken(fAuthCode);
    if fAuthToken.Length > 0 then
    begin
      MainDM.OAuth2Authenticator1.AccessToken := fAuthToken;
      MainDM.RESTRequest1.Execute;
      MultiView1.HideMaster;
    end;
  end;
end;

end.
