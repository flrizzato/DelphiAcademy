//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MainMongoIdx;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.FMXUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Comp.UI, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.ScrollBox, FMX.Memo, FireDAC.Phys.MongoDB, FireDAC.Phys.MongoDBDef,
  System.Rtti, System.JSON.Types, System.JSON.Readers, System.JSON.BSON,
  System.JSON.Builders, FireDAC.Phys.MongoDBWrapper;

type
  TForm9 = class(TForm)
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDConnection1: TFDConnection;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label1: TLabel;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FEnv: TMongoEnv;
    FCon: TMongoConnection;
    FCol: TMongoCollection;
    procedure OutputCursor(oCrs: IMongoCursor);
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.fmx}

procedure TForm9.OutputCursor(oCrs: IMongoCursor);
begin
  while oCrs.Next do
    Memo1.Lines.Add(oCrs.Doc.AsJSON);
end;


procedure TForm9.Button1Click(Sender: TObject);
var
  oDoc: TMongoDocument;
  oCrs: IMongoCursor;
  i: Integer;
begin
  oDoc := FEnv.NewDoc;
  try
    FCol.RemoveAll;

    for i := 1 to 100 do begin
      oDoc
        .Clear
        .Add('int', i)
        .Add('str', IntToHex(Random(MaxInt), 8));
      FCol.Insert(oDoc);
    end;
  finally
    oDoc.Free;
  end;

  oCrs := FCol.Find();

  Memo1.Lines.Clear;
  OutputCursor(oCrs);



end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  OutputCursor(FCol.ListIndexes);

end;

procedure TForm9.Button3Click(Sender: TObject);
var
  oCrs: IMongoCursor;
  oIter: TJSONIterator;
begin
  oCrs := FCol.ListIndexes;

  oIter := nil;
  while oCrs.Next do
  begin
    try
      oIter := oCrs.Doc.Iterator;
      if oIter.Find('name') and (oIter.AsString <> '_id_') then
        FCol.DropIndex(oIter.AsString);
    finally
      oIter.Free;
    end;
  end;

  Memo1.Lines.Clear;
  OutputCursor(FCol.ListIndexes);
end;

procedure TForm9.Button4Click(Sender: TObject);
var
  q: TMongoQuery;
begin

  Memo1.Lines.Clear;
  Memo1.Lines.Add('Indexes');
  OutputCursor(FCol.ListIndexes);
  Memo1.Lines.Add('');

  q := FCol
    .Find()
      .Match
        .BeginObject('int')
          .Add('$gt', 5)
          .Add('$lt', 10)
        .EndObject
      .&End;

  q.Options.Explain := True;
  OutputCursor(q);

end;

procedure TForm9.Button5Click(Sender: TObject);
var
  idx: TMongoIndex;
begin
  // http://docs.mongodb.org/manual/administration/indexes-creation/
  // http://docs.mongodb.org/manual/tutorial/create-an-index/
  idx := TMongoIndex.Create(FEnv);
  try
    idx.Keys('{int: 1}');
    idx.Options.Name := 'By int';
    FCol.CreateIndex(idx);
  finally
    idx.Free;
  end;
  Memo1.Lines.Clear;
  OutputCursor(FCol.ListIndexes);
end;

procedure TForm9.Button6Click(Sender: TObject);
var
  idx: TMongoIndex;
begin
  // http://docs.mongodb.org/manual/administration/indexes-creation/
  // http://docs.mongodb.org/manual/tutorial/create-an-index/
  idx := TMongoIndex.Create(FEnv);
  try
    idx.Keys('{str: 1}');
    idx.Options.Name := 'By str';
    FCol.CreateIndex(idx);
  finally
    idx.Free;
  end;

  Memo1.Lines.Clear;
  OutputCursor(FCol.ListIndexes);
end;

procedure TForm9.Button7Click(Sender: TObject);
var
  idx: TMongoIndex;
begin
  // http://docs.mongodb.org/manual/administration/indexes-creation/
  // http://docs.mongodb.org/manual/tutorial/create-a-compound-index/
  idx := TMongoIndex.Create(FEnv);
  try
    idx.Keys('{str: 1, int: 1}');
    idx.Options.Name := 'Compound';
    FCol.CreateIndex(idx);
  finally
    idx.Free;
  end;

  Memo1.Lines.Clear;
  OutputCursor(FCol.ListIndexes);
end;



procedure TForm9.FormCreate(Sender: TObject);
begin
  // Connect to MongoDB and get CLI wrapping objects
  FDConnection1.Connected := True;
  FCon := TMongoConnection(FDConnection1.CliObj);
  FEnv := FCon.Env;
  FCol := FCon['test']['advanced'];
end;

initialization


end.
