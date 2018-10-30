unit MainDM;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Phys.IBBase, FireDAC.Phys.IB, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type
  TdmMain = class(TDataModule)
    Connection: TFDConnection;
    qryPeople: TFDQuery;
    qryPeopleID: TIntegerField;
    qryPeopleFIRST_NAME: TStringField;
    qryPeopleLAST_NAME: TStringField;
    qryPeopleWORK_PHONE_NUMBER: TStringField;
    qryPeopleMOBILE_PHONE_NUMBER: TStringField;
    qryPeopleEMAIL: TStringField;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

end.
