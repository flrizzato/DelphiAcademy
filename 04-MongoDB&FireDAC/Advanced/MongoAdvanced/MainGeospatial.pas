//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainGeospatial;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MongoDB,
  FireDAC.Phys.MongoDBDef, System.Rtti, System.JSON.Types, System.JSON.Readers,
  System.JSON.BSON, System.JSON.Builders, FireDAC.Phys.MongoDBWrapper,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  Data.DB, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Phys.MongoDBDataSet, FireDAC.Comp.UI,
  FireDAC.VCLUI.Wait;

type
  TForm10 = class(TForm)
    FDConnection1: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    EditOrigin: TEdit;
    Label1: TLabel;
    EditMin: TEdit;
    Label2: TLabel;
    EditMax: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    FDMongoQuery1: TFDMongoQuery;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    FDMongoDataSet1: TFDMongoDataSet;
    Button4: TButton;
    Button5: TButton;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    FEnv: TMongoEnv;
    FCon: TMongoConnection;
    { Private declarations }
    procedure BuildQuery;

  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

procedure TForm10.Button3Click(Sender: TObject);
begin
  BuildQuery;
  FDMongoQuery1.Query.Options.Explain := True;
  FDMongoQuery1.Open;
end;

procedure TForm10.Button4Click(Sender: TObject);
begin
  FCon['test']['restaurants'].DropIndex('address.coord_2dsphere');
end;

procedure TForm10.Button5Click(Sender: TObject);

var
  idx: TMongoIndex;
begin
  // http://docs.mongodb.org/manual/core/2dsphere/
  idx := TMongoIndex.Create(FEnv);
  try
    idx.Keys('{"address.coord" : "2dsphere"}');
    idx.Options.Name := 'address.coord_2dsphere';
    FCon['test']['restaurants'].CreateIndex(idx);
  finally
    idx.Free;
  end;

end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  // Connect to MongoDB and get CLI wrapping objects
  FDConnection1.Connected := True;
  FCon := TMongoConnection(FDConnection1.CliObj);
  FEnv := FCon.Env;
end;

procedure TForm10.BuildQuery;
begin
  // http://docs.mongodb.org/manual/reference/operator/query/nearSphere/#op._S_nearSphere
  FDMongoQuery1.Close;
  FDMongoQuery1.Query.Options.Explain := False;
  FDMongoQuery1.FieldDefs.Clear;
  FDMongoQuery1.QMatch := Format('{ "address.coord": { $nearSphere: { $geometry: { type: "Point",  coordinates: [ %s ] }, $minDistance: %s, $maxDistance: %s } } }',
  [EditOrigin.Text, EditMin.Text, EditMax.Text]);
  DataSource1.DataSet := FDMongoQuery1;
end;


procedure TForm10.Button1Click(Sender: TObject);
begin
  BuildQuery;
  FDMongoQuery1.Open;
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  FDMongoDataSet1.Close;
  FDMongoDataSet1.Cursor := FCon['test']['restaurants'].ListIndexes;
  DataSource1.DataSet := FDMongoDataSet1;
  FDMongoDataSet1.Open;
end;

end.
