//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainMongoSQL;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MongoDB,
  FireDAC.Phys.MongoDBDef, System.Rtti, System.JSON.Types, System.JSON.Readers,
  System.JSON.BSON, System.JSON.Builders, FireDAC.Phys.MongoDBWrapper,
  FireDAC.Comp.UI, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, FireDAC.Phys.MongoDBDataSet, Vcl.StdCtrls, Vcl.Grids,
  Vcl.DBGrids, FireDAC.Stan.Util, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.DApt, FireDAC.Phys.SQLiteVDataSet,
  FireDAC.VCLUI.Wait;

type
  TForm9 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditMatch: TEdit;
    DBGrid1: TDBGrid;
    ButtonOpenMongo: TButton;
    EditSort: TEdit;
    EditProjection: TEdit;
    FDMongoQuery1: TFDMongoQuery;
    FDConnection1: TFDConnection;
    dsQuery: TDataSource;
    EditSQL: TMemo;
    ButtonOpenSQL: TButton;
    FDLocalSQLConnection: TFDConnection;
    FDLocalSQL1: TFDLocalSQL;
    FDQuery1: TFDQuery;
    Label4: TLabel;
    EditDatabase: TEdit;
    Label5: TLabel;
    EditCollection: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonOpenMongoClick(Sender: TObject);
    procedure ButtonOpenSQLClick(Sender: TObject);
  private
    { Private declarations }
    FEnv: TMongoEnv;
    FCon: TMongoConnection;
    procedure OpenMongo;

  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

{procedure RemoveNestedDataSets(Fields: TFields);
var
  I: Integer;
begin
  for I := Fields.Count - 1 downto 0 do
  begin
    if Fields[I].DataType = ftADT then
    begin
      RemoveNestedDataSets(TADTField(Fields[I]).Fields);
    end;
    if (Fields[I].DataType = ftDataSet) or (Fields[I].DataType = ftADT) then
    begin
      Fields[I].Visible := False;
      Fields.Remove(Fields[I]);
    end;
  end;
end;}

procedure TForm9.ButtonOpenMongoClick(Sender: TObject);
begin
  OpenMongo;
end;

procedure TForm9.ButtonOpenSQLClick(Sender: TObject);
begin
  if not FDLocalSQL1.Active then OpenMongo;

  FDQuery1.Close;
  FDQuery1.FieldDefs.Clear();
  dsQuery.DataSet := FDQuery1;

  FDQuery1.Open(EditSQL.Text);
end;

procedure TForm9.OpenMongo;
var
  I: Integer;
begin
  // Close Everything
  FDLocalSQLConnection.Connected := False;
  FDLocalSQL1.Active := False;
  FDMongoQuery1.Close;
  FDMongoQuery1.FieldDefs.Clear;

  // Setup new connection
  FDMongoQuery1.DatabaseName := EditDatabase.Text;
  FDMongoQuery1.CollectionName := EditCollection.Text;
  FDMongoQuery1.QMatch := EditMatch.Text;
  FDMongoQuery1.QSort := EditSort.Text;
  FDMongoQuery1.QProject := EditProjection.Text;
  dsQuery.DataSet := FDMongoQuery1;
  FDMongoQuery1.Open;

  // Check for data types not allowed by SQLite
  for I := 0 to FDMongoQuery1.FieldDefs.Count - 1 do
    if (FDMongoQuery1.FieldDefs[I].DataType = ftADT) or (FDMongoQuery1.FieldDefs[I].DataType = ftDataSet) then
    begin
      ShowMessage('No nested objects or arrays allowed. [Field: ' + FDMongoQuery1.FieldDefs[i].Name + ']');
      Abort;
    end;

  // Open for business
  FDLocalSQLConnection.Connected := True;
  FDLocalSQL1.Active := True;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  FDConnection1.Connected := True;
  FCon := TMongoConnection(FDConnection1.CliObj);
  FEnv := FCon.Env;
end;

procedure TForm9.FormDestroy(Sender: TObject);
begin
  FDLocalSQLConnection.Connected := False;
  FDLocalSQL1.Active := False;
  FDQuery1.Close;

  FDConnection1.Close;
end;

end.
