unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.IB, FireDAC.Phys.IBLiteDef, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Phys.IBDef,
  FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.IBBase, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.ListView, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Rtti, System.Bindings.Outputs,
  FMX.Bind.Editors, Data.Bind.EngExt, FMX.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.DBScope;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    ListView1: TListView;
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDQuery1CUSTNO: TFloatField;
    FDQuery1COMPANY: TStringField;
    FDQuery1ADDR1: TStringField;
    FDQuery1ADDR2: TStringField;
    FDQuery1CITY: TStringField;
    FDQuery1STATE: TStringField;
    FDQuery1ZIP: TStringField;
    FDQuery1COUNTRY: TStringField;
    FDQuery1PHONE: TStringField;
    FDQuery1FAX: TStringField;
    FDQuery1TAXRATE: TFloatField;
    FDQuery1CONTACT: TStringField;
    FDQuery1LASTINVOICEDATE: TSQLTimeStampField;
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
    IncludeTrailingBackslash(ExtractFilePath(ParamStr(0))) + 'dbdemos.gdb';
{$ELSE}
  FDConnection1.Params.Values['Database'] := TPath.GetDocumentsPath + PathDelim
    + 'dbdemos.gdb';
{$ENDIF}
end;

end.
