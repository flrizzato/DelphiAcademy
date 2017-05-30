unit ServerContainerUnit1;

interface

uses System.SysUtils, System.Classes,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  IPPeerServer, IPPeerAPI, Datasnap.DSAuth, FireDAC.Phys.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Intf, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.IBBase, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TServerContainer1 = class(TDataModule)
    DSServer1: TDSServer;
    DSServerClass1: TDSServerClass;
    DSServerClass2: TDSServerClass;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure DSServerClass2GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

function DSServer: TDSServer;

implementation

{$R *.dfm}

uses
  ServerMethodsUnit1, uDataCacheDM, ClientModuleUnit3;

var
  FModule: TComponent;
  FDSServer: TDSServer;

function DSServer: TDSServer;
begin
  Result := FDSServer;
end;

constructor TServerContainer1.Create(AOwner: TComponent);
begin
  inherited;
  FDSServer := DSServer1;
end;

procedure TServerContainer1.DataModuleCreate(Sender: TObject);
begin
  //FDEventAlerter1.Active := True;
end;

destructor TServerContainer1.Destroy;
begin
  inherited;
  FDSServer := nil;
end;

procedure TServerContainer1.DSServerClass1GetClass(DSServerClass
  : TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := ServerMethodsUnit1.TServerMethods1;
end;

procedure TServerContainer1.DSServerClass2GetClass(DSServerClass
  : TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := uDataCacheDM.TDataCacheDM;
end;

initialization

FModule := TServerContainer1.Create(nil);

finalization

FModule.Free;

end.
