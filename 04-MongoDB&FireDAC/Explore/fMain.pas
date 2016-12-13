//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MongoDB,
  FireDAC.Phys.MongoDBDef, System.Rtti, System.JSON.Types, System.JSON.Readers,
  System.JSON.BSON, System.JSON.Builders, FireDAC.Phys.MongoDBWrapper,
  FireDAC.VCLUI.Wait, Data.DB, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.Phys.MongoDBDataSet, FireDAC.Comp.DataSet, FireDAC.Comp.UI;

type
  TfrmMain = class(TForm)
    FDConnection1: TFDConnection;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    dsDatabases: TDataSource;
    dsCollections: TDataSource;
    dsData: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    mdDatabases: TFDMongoDataSet;
    mdCollections: TFDMongoDataSet;
    mqData: TFDMongoQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysMongoDriverLink1: TFDPhysMongoDriverLink;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure dsDatabasesDataChange(Sender: TObject; Field: TField);
    procedure dsCollectionsDataChange(Sender: TObject; Field: TField);
    procedure dsDataDataChange(Sender: TObject; Field: TField);
  private
    FMongoConn: TMongoConnection;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FDConnection1.Connected := True;
  FMongoConn := TMongoConnection(FDConnection1.CliObj);

  mdDatabases.Close;
  mdDatabases.Cursor := FMongoConn.ListDatabases;
  mdDatabases.Open;
end;

procedure TfrmMain.dsDatabasesDataChange(Sender: TObject; Field: TField);
begin
  mdCollections.Close;
  if not mdDatabases.IsEmpty then begin
    mdCollections.Cursor := FMongoConn.Databases[mdDatabases.Fields[0].AsString].ListCollections;
    mdCollections.Open;
  end;
end;

procedure TfrmMain.dsCollectionsDataChange(Sender: TObject; Field: TField);
begin
  mqData.Close;
  if not mdCollections.IsEmpty then begin
    mqData.DatabaseName := mdDatabases.Fields[0].AsString;
    mqData.CollectionName := mdCollections.Fields[0].AsString;
    mqData.Open;
  end;
end;

procedure TfrmMain.dsDataDataChange(Sender: TObject; Field: TField);
begin
  Label4.Caption := IntToStr(mqData.RecNo) + '/' + IntToStr(mqData.RecordCount);
end;

end.
