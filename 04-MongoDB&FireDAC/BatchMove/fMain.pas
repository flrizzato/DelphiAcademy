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
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Phys.MongoDBDataSet,
  FireDAC.Comp.BatchMove.DataSet, FireDAC.Comp.BatchMove,
  FireDAC.Comp.BatchMove.SQL, FireDAC.Phys.MongoDB, FireDAC.Phys.MongoDBDef,
  System.Rtti, System.JSON.Types, System.JSON.Readers, System.JSON.BSON,
  System.JSON.Builders, FireDAC.Phys.MongoDBWrapper, Vcl.StdCtrls, Vcl.Grids,
  Vcl.DBGrids, FireDAC.Comp.UI;

type
  TfrmMain = class(TForm)
    FDConnection1: TFDConnection;
    FDBatchMoveSQLReader1: TFDBatchMoveSQLReader;
    FDBatchMove1: TFDBatchMove;
    FDBatchMoveDataSetWriter1: TFDBatchMoveDataSetWriter;
    FDConnection2: TFDConnection;
    Button1: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    FDMongoQuery1: TFDMongoQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysMongoDriverLink1: TFDPhysMongoDriverLink;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
var
  sCat, sSch, sBase, sObj: string;
begin
  FDConnection1.Connected := True;
  FDConnection2.Connected := True;

  // Get MongoDB collection name from source DB table name
  FDConnection1.DecodeObjectName(FDBatchMoveSQLReader1.TableName, sCat, sSch, sBase, sObj);
  sObj := StringReplace(sObj, ' ', '_', [rfReplaceAll]);

  FDMongoQuery1.Active := False;
  // Specify MongoDB database and collection names
  FDMongoQuery1.DatabaseName := 'test';
  FDMongoQuery1.CollectionName := sObj;
  // Specify always-False MongoDB condition to avoid not needed documents fetching
  FDMongoQuery1.QMatch := '{"_id": ""}';
  // Delete all documents from specified MongoDB collection
  FDMongoQuery1.ServerDeleteAll();

  // Create and populate MongoDB collection
  FDBatchMove1.Execute;
  ShowMessage(Format('%d records inserted', [FDBatchMove1.InsertCount]));

  // Refresh MongoDB query
  FDMongoQuery1.Active := False;
  // Clear all definitions as field definitions may change
  FDMongoQuery1.IndexDefs.Clear;
  FDMongoQuery1.Indexes.Clear;
  FDMongoQuery1.FieldDefs.Clear;
  FDMongoQuery1.QMatch := '';
  FDMongoQuery1.Active := True;
end;

end.
