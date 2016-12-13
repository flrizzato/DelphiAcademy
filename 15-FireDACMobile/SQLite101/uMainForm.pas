unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, System.Rtti, System.Bindings.Outputs,
  FMX.Bind.Editors, Data.Bind.EngExt, FMX.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.DBScope, Data.DB, FMX.ListView, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Comp.UI, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    ToolBar1: TToolBar;
    Button1: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDQuery1: TFDQuery;
    ListView1: TListView;
    FDQuery1CustomerID: TStringField;
    FDQuery1CompanyName: TStringField;
    FDQuery1ContactName: TStringField;
    FDQuery1ContactTitle: TStringField;
    FDQuery1Address: TStringField;
    FDQuery1City: TStringField;
    FDQuery1Region: TStringField;
    FDQuery1PostalCode: TStringField;
    FDQuery1Country: TStringField;
    FDQuery1Phone: TStringField;
    FDQuery1Fax: TStringField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    procedure Button1Click(Sender: TObject);
    procedure FDConnection1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses System.IOUtils;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  FDQuery1.Open;
end;

procedure TForm1.FDConnection1BeforeConnect(Sender: TObject);
begin
{$IF DEFINED(MSWINDOWS)}
  FDConnection1.Params.Values['Database'] :=
    IncludeTrailingBackslash(ExtractFilePath(ParamStr(0))) + 'fddemo.sdb';
{$ELSE}
  FDConnection1.Params.Values['Database'] := TPath.GetDocumentsPath + PathDelim
    + 'fddemo.sdb';
{$ENDIF}
end;

end.
