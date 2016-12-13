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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.FMXUI.Wait, FireDAC.Phys.MongoDBDef,
  FireDAC.Phys.MongoDB, FireDAC.Comp.UI, Data.DB, FireDAC.Comp.Client,
  FMX.Controls.Presentation, FMX.StdCtrls, FireDAC.Phys.MongoDBWrapper,
  FMX.ScrollBox, FMX.Memo, System.JSON.Types, System.JSON.BSON,
  System.JSON.Builders, System.Rtti, System.JSON.Readers;

type
  TfrmMain = class(TForm)
    FDConnection1: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysMongoDriverLink1: TFDPhysMongoDriverLink;
    btnLoadData: TButton;
    Memo1: TMemo;
    btnAggregate: TButton;
    btnIndexes: TButton;
    btnInsert: TButton;
    btnRemove: TButton;
    btnUpdate: TButton;
    btnQuery: TButton;
    procedure btnLoadDataClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAggregateClick(Sender: TObject);
    procedure btnIndexesClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnQueryClick(Sender: TObject);
  private
    FEnv: TMongoEnv;
    FCon: TMongoConnection;
    procedure DumpCursor(ACrs: IMongoCursor);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.Diagnostics,
  FireDAC.Stan.Util;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FDConnection1.Connected := True;
  FCon := TMongoConnection(FDConnection1.CliObj);
  FEnv := FCon.Env;
end;

procedure TfrmMain.DumpCursor(ACrs: IMongoCursor);
begin
  while ACrs.Next do
    Memo1.Lines.Add(ACrs.Doc.AsJSON);
end;

procedure TfrmMain.btnLoadDataClick(Sender: TObject);
var
  oText: TFDTextFile;
  oDoc: TMongoDocument;
  s: String;
  t: TStopwatch;
  n: LongWord;
  oCol: TMongoCollection;
begin
  // 2.73 secs:
  // * 0.54 - file reading
  // * 1.78 - JSON parsing
  // * 0.41 - MongoDB bulk insert
  Memo1.Lines.Clear;

  oCol := FCon['test']['restaurants'];
  oCol.RemoveAll();

  oText := TFDTextFile.Create('..\..\..\..\DB\Data\restaurants.json', True, False, ecANSI, elWindows);
  oDoc := FEnv.NewDoc;
  try
    t := TStopwatch.StartNew;
    n := 0;
    oCol.BeginBulk;
    try
      while True do begin
        s := oText.ReadLine;
        if s = '' then
          Break;
        oDoc.AsJSON := s;
        oCol.Insert(oDoc);
        Inc(n);
      end;
      oCol.EndBulk;
    except
      oCol.CancelBulk;
      raise;
    end;
    Memo1.Lines.Add('Loaded [' + IntToStr(n) + '] records in [' + t.Elapsed + '] secs');
  finally
    oDoc.Free;
    oText.Free;
  end;
end;

procedure TfrmMain.btnQueryClick(Sender: TObject);
var
  oCrs: IMongoCursor;
begin
  Memo1.Lines.Clear;

  Memo1.Lines.Add('Collection restaurants has [' +
    FCon['test']['restaurants'].Count().Value().ToString() +
    '] documents');

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Restaurants where cuisine is Italian and zipcode is 10075');
  oCrs := FCon['test']['restaurants'].Find()
    .Match()
      .Add('cuisine', 'Italian')
      .Add('address.zipcode', '10075')
    .&End;
  DumpCursor(oCrs);
end;

procedure TfrmMain.btnIndexesClick(Sender: TObject);
var
  oInd: TMongoIndex;
  oCrs: IMongoCursor;
begin
  Memo1.Lines.Clear;

  // single field index
  oInd := TMongoIndex.Create(FEnv);
  try
    oInd.Keys.Field('cuisine');
    oInd.Options.Name := 'i_cuisine';
    FCon['test']['restaurants'].CreateIndex(oInd);
  finally
    oInd.Free;
  end;
  Memo1.Lines.Add('Index i_cuisine created');

  // compound index
  oInd := TMongoIndex.Create(FEnv);
  try
    oInd.Keys.Ascending(['cuisine', 'address.zipcode']);
    oInd.Options.Name := 'i_cuisine_zipcode';
    FCon['test']['restaurants'].CreateIndex(oInd);
  finally
    oInd.Free;
  end;
  Memo1.Lines.Add('Index i_cuisine_zipcode created');

  Memo1.Lines.Add('');
  Memo1.Lines.Add('List of restaurants collection indexes');
  oCrs := FCon['test']['restaurants'].ListIndexes;
  DumpCursor(oCrs);
end;

procedure TfrmMain.btnAggregateClick(Sender: TObject);
var
  oCrs: IMongoCursor;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Count of restaurants grouped by borough');

  oCrs := FCon['test']['restaurants'].Aggregate()
    .Group
      .Add('_id', '$cuisine')
      .BeginObject('count')
        .Add('$sum', 1)
      .EndObject
    .&End;
  DumpCursor(oCrs);

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Count of Mexican restaurants in Queens grouped by zip-code');

  oCrs := FCon['test']['restaurants'].Aggregate()
    .Match
      //.Add('borough', 'Queens')
      .Add('cuisine', 'Mexican')
    .&End
    .Group
      .Add('_id', '$address.zipcode')
      .BeginObject('count')
        .Add('$sum', 1)
      .EndObject
    .&End;
  DumpCursor(oCrs);
end;

procedure TfrmMain.btnInsertClick(Sender: TObject);
begin
  Memo1.Lines.Clear;

  FCon['test']['restaurants'].Insert()
    .Values()
      .BeginObject('address')
        .Add('street', '2 Avenue')
        .Add('zipcode', '10075')
        .Add('building', '1480')
        .BeginArray('coord')
          .Add('0', 73.9557413)
          .Add('1', 40.7720266)
        .EndArray
      .EndObject
      .Add('borough', 'Manhattan')
      .Add('cuisine', 'Italian')
      .BeginArray('grades')
        .BeginObject('0')
          .Add('date', EncodeDate(2014, 10, 1))
          .Add('grade', 'A')
          .Add('score', 11)
        .EndObject
        .BeginObject('1')
          .Add('date', EncodeDate(2014, 1, 6))
          .Add('grade', 'B')
          .Add('score', 17)
        .EndObject
      .EndArray
      .Add('name', 'Vella')
      .Add('restaurant_id', '41704620')
    .&End
  .Exec;

  Memo1.Lines.Add('Inserted [' + FCon['test']['restaurants'].DocsInserted.ToString + '] documents');
end;

procedure TfrmMain.btnUpdateClick(Sender: TObject);
begin
  Memo1.Lines.Clear;

  FCon['test']['restaurants'].Update()
    .Match()
      .Add('name', 'Juni')
    .&End
    .Modify()
      .&Set()
        .Field('cuisine', 'American (New)')
      .&End
      .CurrentDate()
        .AsDate('lastModified')
      .&End
    .&End
  .Exec;

  Memo1.Lines.Add('Updated [' + FCon['test']['restaurants'].DocsModified.ToString + '] documents');
end;

procedure TfrmMain.btnRemoveClick(Sender: TObject);
begin
  Memo1.Lines.Clear;

  FCon['test']['restaurants'].Remove()
    .Match()
      .Add('borough', 'Manhattan')
    .&End
  .Exec;

  Memo1.Lines.Add('Removed [' + FCon['test']['restaurants'].DocsRemoved.ToString + '] documents');
end;

end.
