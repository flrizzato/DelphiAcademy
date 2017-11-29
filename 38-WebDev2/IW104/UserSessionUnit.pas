unit UserSessionUnit;

{
  This is a DataModule where you can add components or declare fields that are specific to
  ONE user. Instead of creating global variables, it is better to use this datamodule. You can then
  access the it using UserSession.
}
interface

uses
  IWUserSessionBase, SysUtils, Classes, FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.Client, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Phys.IBBase, FireDAC.Comp.UI,
  IW.Content.Base, HTTPApp, IWApplication, IW.HTTP.Request,
  IW.HTTP.Reply, System.JSON;

type
  TIWUserSession = class(TIWUserSessionBase)
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Connection: TFDConnection;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    qryPeople: TFDQuery;
    qryPeopleID: TFDAutoIncField;
    qryPeopleFIRST_NAME: TStringField;
    qryPeopleLAST_NAME: TStringField;
    qryPeopleWORK_PHONE_NUMBER: TStringField;
    qryPeopleMOBILE_PHONE_NUMBER: TStringField;
    qryPeopleEMAIL: TStringField;
    cmdUpdatePerson: TFDCommand;
    cmdInsertPerson: TFDCommand;
    cmdDeletePerson: TFDCommand;
    procedure ConnectionBeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TIWUserSession.ConnectionBeforeConnect(Sender: TObject);
begin
  Connection.Params.Values['Database'] := gGetAppPath + '..\..\..\SAMPLES.IB';
end;

end.
