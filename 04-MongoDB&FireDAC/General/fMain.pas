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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.StdCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
    FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
    FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
    FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.VCLUI.Error,
    FireDAC.Comp.UI, FireDAC.Moni.Base, FireDAC.Moni.FlatFile, FireDAC.VCLUI.Wait,
  FireDAC.Phys.MongoDBCli, FireDAC.Phys.MongoDBWrapper, FireDAC.Phys.MongoDB,
    FireDAC.Phys.MongoDBDef,
  System.JSON.Types, System.JSON.BSON, System.JSON.Builders, System.Rtti,
  System.JSON.Readers;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    btnInsert: TButton;
    btnPing: TButton;
    btnAggProj: TButton;
    btnAggRedact: TButton;
    btnInsFind: TButton;
    btnListCols: TButton;
    btnUpdInc: TButton;
    btnUpdPush: TButton;
    Button9: TButton;
    btnIterate: TButton;
    FDGUIxErrorDialog1: TFDGUIxErrorDialog;
    FDConnection1: TFDConnection;
    FDPhysMongoDriverLink1: TFDPhysMongoDriverLink;
    btnCurrentOp: TButton;
    FDMoniFlatFileClientLink1: TFDMoniFlatFileClientLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    procedure FormCreate(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnPingClick(Sender: TObject);
    procedure btnAggProjClick(Sender: TObject);
    procedure btnAggRedactClick(Sender: TObject);
    procedure btnInsFindClick(Sender: TObject);
    procedure btnListColsClick(Sender: TObject);
    procedure btnUpdIncClick(Sender: TObject);
    procedure btnUpdPushClick(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure btnIterateClick(Sender: TObject);
    procedure btnCurrentOpClick(Sender: TObject);
  private
    FEnv: TMongoEnv;
    FCon: TMongoConnection;
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
  // Connect to MongoDB and get CLI wrapping objects
  FDConnection1.Connected := True;
  FCon := TMongoConnection(FDConnection1.CliObj);
  FEnv := FCon.Env;
end;

procedure TfrmMain.btnInsertClick(Sender: TObject);
var
  oDoc: TMongoDocument;
  oCrs: IMongoCursor;
begin
  // For details see:
  // https://docs.mongodb.org/getting-started/shell/insert/
  oDoc := FEnv.NewDoc;
  try
    // Remove all documents from "restaurants" collection in "test" database
    FCon['test']['restaurants'].RemoveAll;

    // Build new document
    oDoc
      .BeginObject('address')
        .Add('street', '2 Avenue')
        .Add('zipcode', '10075')
        .Add('building', '1480')
        .BeginArray('coord')
          .Add('0', -73.9557413)
          .Add('1', 40.7720266)
        .EndArray
      .EndObject
      .Add('borough', 'Manhattan')
      .Add('cuisine', 'Italian')
      .BeginArray('grades')
        .BeginObject('0')
          .Add('date', EncodeDate(2000, 5, 25))
          .Add('grade', 'Add')
          .Add('score', 11)
        .EndObject
        .BeginObject('1')
          .Add('date', EncodeDate(2005, 6, 2))
          .Add('grade', 'B')
          .Add('score', 17)
        .EndObject
      .EndArray
      .Add('name', 'Vella')
      .Add('restaurant_id', '41704620');

    // Insert new document into "restaurants" collection in "test" database.
    // This may be done using "fluent" style.
    FCon['test']['restaurants'].Insert(oDoc);

    // Find, retrieve and show all documents
    // The query condition may be build using "fluent" style.
    oCrs := FCon['test']['restaurants'].Find();
    while oCrs.Next do
      Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;

    // Get number of documents in the collection
    Memo1.Text := Memo1.Text + #13#10'Record count ' +
      FCon['test']['restaurants'].Count().Value().ToString();

  finally
    oDoc.Free;
  end;
end;

procedure TfrmMain.btnPingClick(Sender: TObject);
begin
  // Ping server and get server version
  FCon.Ping;
  Memo1.Text := IntToStr(FCon.ServerVersion);
end;

procedure TfrmMain.btnAggProjClick(Sender: TObject);
var
  oDoc: TMongoDocument;
  oCrs: IMongoCursor;
begin
  // For details see:
  // http://docs.mongodb.org/manual/reference/operator/aggregation/project/#include-computed-fields
  oDoc := FEnv.NewDoc;
  try
    FCon['test']['books'].RemoveAll;

    oDoc
      .Add('_id', 1)
      .Add('title', 'abc123')
      .Add('isbn', '0001122223334')
      .BeginObject('author')
        .Add('last', 'zzz')
        .Add('first', 'aaa')
      .EndObject
      .Add('copies', 5);

    FCon['test']['books'].Insert(oDoc);

    oCrs := FCon['test']['books']
      .Aggregate()

      .Project
        .Field('title')
        .FieldBegin('isbn')
          .Exp('prefix',     '{ "$substr": [ "$isbn", 0, 3 ] }')
          .Exp('group',      '{ "$substr": [ "$isbn", 3, 2 ] }')
          .Exp('publisher',  '{ "$substr": [ "$isbn", 5, 4 ] }')
          .Exp('title',      '{ "$substr": [ "$isbn", 9, 3 ] }')
          .Exp('checkDigit', '{ "$substr": [ "$isbn", 12, 1] }')
        .FieldEnd
        .Exp('lastName',   '"$author.last"')
        .Exp('copiesSold', '"$copies"')
      .&End

      .Match
        .Exp('copiesSold', '{ "$gt" : 4, "$lte" : 6 }')
      .&End;

    while oCrs.Next do
      Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;

  finally
    oDoc.Free;
  end;
end;

procedure TfrmMain.btnAggRedactClick(Sender: TObject);
var
  oDoc: TMongoDocument;
  oCrs: IMongoCursor;
begin
  // For details see:
  // http://docs.mongodb.org/manual/reference/operator/aggregation/redact/
  oDoc := FEnv.NewDoc;
  try
    FCon['test']['forecasts'].RemoveAll;

    oDoc
      .Add('_id', 1)
      .Add('title', '123 Department Report')
      .BeginArray('tags')
        .Add('0', 'G')
        .Add('1', 'STLW')
      .EndArray
      .Add('year', 2014)
      .BeginArray('subsections')
        .BeginObject('0')
          .Add('subtitle', 'Section 1: Overview')
          .BeginArray('tags')
            .Add('0', 'SI')
            .Add('1', 'G')
          .EndArray
          .Add('content', 'Section 1: This is the content of section 1.')
        .EndObject
        .BeginObject('1')
          .Add('subtitle', 'Section 2: Analysis')
          .BeginArray('tags')
            .Add('0', 'STLW')
          .EndArray
          .Add('content', 'Section 2: This is the content of section 2.')
        .EndObject
        .BeginObject('2')
          .Add('subtitle', 'Section 3: Budgeting')
          .BeginArray('tags')
            .Add('0', 'TK')
          .EndArray
          .BeginObject('content')
            .Add('text', 'Section 3: This is the content of section3.')
            .BeginArray('tags')
              .Add('0', 'HCS')
            .EndArray
          .EndObject
        .EndObject
      .EndArray;

    FCon['test']['forecasts'].Insert(oDoc);

    oCrs := FCon['test']['forecasts']
      .Aggregate()

      .Match
        .Exp('year', '2014')
      .&End

      .Redact
        .BeginObject('$cond')
          .BeginObject('if')
            .BeginArray('$gt')
              .BeginObject('0')
                .BeginObject('$size')
                  .BeginArray('$setIntersection')
                    .Add('0', '$tags')
                    .BeginArray('1')
                      .Add('0', 'STLW')
                      .Add('1', 'G')
                    .EndArray
                  .EndArray
                .EndObject
              .EndObject
              .Add('1', 0)
            .EndArray
          .EndObject
          .Add('then', '$$DESCEND')
          .Add('else', '$$PRUNE')
        .EndObject
      .&End;

    while oCrs.Next do
      Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;

  finally
    oDoc.Free;
  end;
end;

procedure TfrmMain.btnInsFindClick(Sender: TObject);
var
  oDoc: TMongoDocument;
  oCrs: IMongoCursor;
  i: Integer;
begin
  // For details see:
  // http://docs.mongodb.org/manual/reference/operator/query/
  // http://docs.mongodb.org/manual/reference/operator/query-modifier/
  oDoc := FEnv.NewDoc;
  try
    FCon['test']['perf_test'].RemoveAll;

    for i := 1 to 100 do begin
      oDoc
        .Clear
        .Add('f1', i div 10)
        .Add('f2', i mod 10)
        .Add('f3', 'str' + IntToStr(i));
      FCon['test']['perf_test'].Insert(oDoc);
    end;

    oCrs := FCon['test']['perf_test']
      .Find()

      .Match
        .BeginObject('f1')
          .Add('$gt', 5)
        .EndObject
      .&End

      .Sort
        .Field('f1', False)
        .Field('f2', True)
      .&End

      .Limit(5);

    while oCrs.Next do
      Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;

  finally
    oDoc.Free;
  end;
end;

procedure TfrmMain.btnListColsClick(Sender: TObject);
var
  oCrs: IMongoCursor;
begin
  // For details see:
  // http://docs.mongodb.org/manual/reference/command/listCollections/
  oCrs := FCon['test'].ListCollections();
  while oCrs.Next do
    Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;
end;

procedure TfrmMain.btnUpdIncClick(Sender: TObject);
var
  oDoc: TMongoDocument;
  oCrs: IMongoCursor;
begin
  // For details see:
  // http://docs.mongodb.org/manual/reference/operator/update/inc/
  oDoc := FEnv.NewDoc;
  try
    FCon['test']['products'].RemoveAll;

    oDoc
      .Add('_id', 1)
      .Add('sku', 'abc123')
      .Add('quantity', 10)
      .BeginObject('metrics')
        .Add('orders', 2)
        .Add('ratings', 3.5)
      .EndObject;
    FCon['test']['products'].Insert(oDoc);

    FCon['test']['products']
      .Update()

      .Match
        .Add('sku', 'abc123')
      .&End

      .Modify
        .Inc
          .Field('quantity', -2)
          .Field('metrics.orders', 1)
        .&End
        .Mul
          .Field('metrics.ratings', 1.01)
        .&End
      .&End
      .Exec;

    oCrs := FCon['test']['products'].Find();
    while oCrs.Next do
      Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;

  finally
    oDoc.Free;
  end;
end;

procedure TfrmMain.btnUpdPushClick(Sender: TObject);
var
  oDoc: TMongoDocument;
  oCrs: IMongoCursor;
begin
  // For details see:
  // http://docs.mongodb.org/manual/reference/operator/update/push/
  oDoc := FEnv.NewDoc;
  try
    FCon['test']['students'].RemoveAll;

    oDoc
      .Add('_id', 5)
      .BeginArray('quizzes')
        .BeginObject('0')
          .Add('wk', 1)
          .Add('score', 10)
        .EndObject
        .BeginObject('1')
          .Add('wk', 2)
          .Add('score', 8)
        .EndObject
        .BeginObject('2')
          .Add('wk', 3)
          .Add('score', 5)
        .EndObject
        .BeginObject('3')
          .Add('wk', 4)
          .Add('score', 6)
        .EndObject
      .EndArray;
    FCon['test']['students'].Insert(oDoc);

    FCon['test']['students']
      .Update()

      .Match
        .Add('_id', 5)
      .&End

      .Modify
        .Push
          .Field('quizzes', ['{', 'wk', 5, 'score', 8, '}',
                             '{', 'wk', 6, 'score', 7, '}',
                             '{', 'wk', 7, 'score', 6, '}'],
            True, 3, '"score": -1')
        .&End
      .&End
      .Exec;

    oCrs := FCon['test']['students'].Find();
    while oCrs.Next do
      Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;

  finally
    oDoc.Free;
  end;
end;

procedure TfrmMain.Button9Click(Sender: TObject);
var
  oDoc: TMongoDocument;
  oCol: TMongoCollection;
  i: Integer;
  oCrs: IMongoCursor;
begin
  // For details see:
  // http://docs.mongodb.org/manual/reference/method/Bulk/
  oDoc := FEnv.NewDoc;
  try
    oCol := FCon['test']['testbulk'];
    oCol.RemoveAll;

    try
      oCol.BeginBulk(False);
      for i := 1 to 10 do begin
        oDoc
          .Clear
          .Add('_id', i div 2)
          .Add('name', 'rec' + IntToStr(i));
        oCol.Insert(oDoc);
      end;
      oCol.EndBulk;
    except
      ApplicationHandleException(nil);
    end;

    oCrs := oCol.Find();
    while oCrs.Next do
      Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;

  finally
    oDoc.Free;
  end;
end;

procedure TfrmMain.btnIterateClick(Sender: TObject);
var
  oDoc: TMongoDocument;
  oIter: TJSONIterator;
  sIdent: String;
begin
  oDoc := FEnv.NewDoc;
  try
    oDoc
      .BeginObject('address')
        .Add('street', '2 Avenue')
        .Add('zipcode', '10075')
        .Add('building', '1480')
        .BeginArray('coord')
          .Add('0', -73.9557413)
          .Add('1', 40.7720266)
        .EndArray
      .EndObject
      .Add('borough', 'Manhattan')
      .Add('cuisine', 'Italian')
      .BeginArray('grades')
        .BeginObject('0')
          .Add('date', EncodeDate(2000, 5, 25))
          .Add('grade', 'Add')
          .Add('score', 11)
        .EndObject
        .BeginObject('1')
          .Add('date', EncodeDate(2005, 6, 2))
          .Add('grade', 'B')
          .Add('score', 17)
        .EndObject
      .EndArray
      .Add('name', 'Vella')
      .Add('restaurant_id', '41704620');

    oIter := oDoc.Iterator;
    sIdent := '';
    try
      while True do begin
        while oIter.Next do begin
          Memo1.Lines.Add(sIdent + oIter.Key);
          if oIter.&Type in [TJsonToken.StartObject, TJsonToken.StartArray] then begin
            sIdent := sIdent + '  ';
            oIter.Recurse;
          end;
        end;
        if oIter.InRecurse then begin
          oIter.Return;
          sIdent := Copy(sIdent, 1, Length(sIdent) - 2);
        end
        else
          Break;
      end;

      if oIter.Find('grades[0].score') then
        Memo1.Lines.Add('found')
      else
        Memo1.Lines.Add('NOT found');

    finally
      oIter.Free;
    end;

  finally
    oDoc.Free;
  end;
end;

procedure TfrmMain.btnCurrentOpClick(Sender: TObject);
var
  oCrs: IMongoCursor;
begin
  // For details see:
  // http://docs.mongodb.org/manual/reference/method/db.currentOp/
  oCrs := FCon['admin']['$cmd.sys.inprog'].Command('{"query": {"$all": [true]}}');
  while oCrs.Next do
    Memo1.Text := Memo1.Text + #13#10 + oCrs.Doc.AsJSON;
end;

end.
