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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.MongoDB, FireDAC.Phys.MongoDBDef, System.JSON.Types,
  System.JSON.BSON, System.JSON.Builders, FireDAC.VCLUI.Wait, FireDAC.Comp.UI,
  FireDAC.Comp.Client, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, FireDAC.Phys.MongoDBDataSet,
  FireDAC.Phys.MongoDBWrapper, System.Rtti, System.JSON.Readers,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.Comp.DataSet;

type
  TfrmMain = class(TForm)
    FDConnection1: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysMongoDriverLink1: TFDPhysMongoDriverLink;
    Button1: TButton;
    dsRestaurants: TDataSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    dsCoords: TDataSource;
    dsGrades: TDataSource;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    Label3: TLabel;
    edtJSONQuery: TEdit;
    Button2: TButton;
    lblTimeElapsed: TLabel;
    FDMongoQuery1: TFDMongoQuery;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  System.Diagnostics;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Mode=fmAll helps to measure fetch performance
  FDMongoQuery1.FetchOptions.Mode := fmAll;
  // CachedUpdates=True helps to measure update performance
  FDMongoQuery1.CachedUpdates := True;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  rWatch: TStopwatch;
begin
  rWatch := TStopwatch.StartNew;
  try
    FDMongoQuery1.Close;
    FDMongoQuery1.QMatch := edtJSONQuery.Text;
    FDMongoQuery1.Open;

    // When query returned no documents or collection does not exist, then
    // dataset will have only single _id field.
    if FDMongoQuery1.IsEmpty and (FDMongoQuery1.FieldDefList.Count = 1) then begin
      ShowMessage('No documents returned by query or [' + FDMongoQuery1.CollectionName +
                  '] collection does not exist.'#13#10 +
                  'To load restaurants demo data use MongoDB\Restaurants demo.');
      Exit;
    end;

    // Attach datasources to nested datasets
    dsCoords.DataSet := (FDMongoQuery1.FieldByName('address.coord') as TDataSetField).NestedDataSet;
    dsGrades.DataSet := (FDMongoQuery1.FieldByName('grades') as TDataSetField).NestedDataSet;
  finally
    lblTimeElapsed.Caption := rWatch.Elapsed;
  end;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  rWatch: TStopwatch;
begin
  rWatch := TStopwatch.StartNew;
  try
    FDMongoQuery1.ApplyUpdates();
  finally
    lblTimeElapsed.Caption := rWatch.Elapsed;
  end;
end;

end.

