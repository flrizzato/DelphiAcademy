//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.ComCtrls, Vcl.StdCtrls, System.Rtti, Data.DB,
  System.JSON.Types, System.JSON.Readers, System.JSON.BSON, System.JSON.Builders,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Stan.Def,
    FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.MongoDB, FireDAC.Phys.MongoDBDef,
    FireDAC.Phys.MongoDBWrapper,
  FireDAC.Comp.Client;

type
  TfrmMain = class(TForm)
    FDConnection1: TFDConnection;
    ListView1: TListView;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FCon: TMongoConnection;
    FEnv: TMongoEnv;
    procedure AddColumn(const AName: String);
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

procedure TfrmMain.AddColumn(const AName: String);
var
  oCol: TListColumn;
begin
  oCol := ListView1.Columns.Add;
  oCol.Width := -1;
  oCol.Caption := AName;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  oCrs: IMongoCursor;
  oIter: TJSONIterator;
  oItem: TListItem;
begin
  // Select all Italian restaurants located in Manhattan (zipcode 10075)
  oCrs := FCon['test']['restaurants'].Find()
    .Match()
      .Add('cuisine', 'Italian')
      .Add('address.zipcode', '10075')
    .&End;

  ListView1.Items.Clear;
  ListView1.Columns.Clear;
  ListView1.ViewStyle := vsReport;
  AddColumn('Name');
  AddColumn('Cuisine');
  AddColumn('Street');
  AddColumn('Building');
  AddColumn('Borough');
  ListView1.Items.BeginUpdate;
  try

    while oCrs.Next do
    begin
      oItem := ListView1.Items.Add;

      // The use of the TJSONIterator.Find method is a simplest way to find a
      // JSON item. As an argument Find method accepts JSON Path-like expression.
      oIter := oCrs.Doc.Iterator;
      try
        if oIter.Find('name') then
          oItem.Caption := oIter.AsString;
        if oIter.Find('cuisine') then
          oItem.SubItems.Add(oIter.AsString)
        else
          oItem.SubItems.Add('');
        if oIter.Find('address.street') then
          oItem.SubItems.Add(oIter.AsString)
        else
          oItem.SubItems.Add('');
        if oIter.Find('address.building') then
          oItem.SubItems.Add(oIter.AsString)
        else
          oItem.SubItems.Add('');
        if oIter.Find('borough') then
          oItem.SubItems.Add(oIter.AsString)
        else
          oItem.SubItems.Add('');
      finally
        oIter.Free;
      end;
    end;

  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  oCrs: IMongoCursor;
  oIter: TJSONIterator;
  oItem: TListItem;
  i: Integer;
begin
  // Select all Italian restaurants located in Manhattan (zipcode 10075)
  oCrs := FCon['test']['restaurants'].Find()
    .Match()
      .Add('cuisine', 'Italian')
      .Add('address.zipcode', '10075')
    .&End;

  ListView1.Items.Clear;
  ListView1.Columns.Clear;
  ListView1.ViewStyle := vsReport;
  AddColumn('Name');
  AddColumn('Cuisine');
  AddColumn('Street');
  AddColumn('Building');
  AddColumn('Borough');
  ListView1.Items.BeginUpdate;
  try

    while oCrs.Next do
    begin
      oItem := ListView1.Items.Add;
      for i := 1 to ListView1.Columns.Count - 1 do
        oItem.SubItems.Add('');

      // The use of the TJSONIterator.Next/Recurse/Return methods is the most
      // flexible way to find a JSON item. But it is a more complicated way
      // than the use of the Find method.
      oIter := oCrs.Doc.Iterator;
      try
        while True do
        begin
          while oIter.Next do
            if oIter.&Type in [TJsonToken.StartObject, TJsonToken.StartArray] then
              oIter.Recurse
            else
              if oIter.Path = 'name' then
                oItem.Caption := oIter.AsString
              else if oIter.Path = 'cuisine' then
                oItem.SubItems[0] := oIter.AsString
              else if oIter.Path = 'address.street' then
                oItem.SubItems[1] := oIter.AsString
              else if oIter.Path = 'address.building' then
                oItem.SubItems[2] := oIter.AsString
              else if oIter.Path = 'borough' then
                oItem.SubItems[3] := oIter.AsString;
          if oIter.InRecurse then
            oIter.Return
          else
            Break;
        end;
      finally
        oIter.Free;
      end;
    end;

  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  oCrs: IMongoCursor;
  oItem: TListItem;
  i: Integer;
begin
  // Select all restaurants located in Brooklyn (zipcode 11214)
  oCrs := FCon['test']['restaurants'].Find()
    .Match()
      .Add('address.zipcode', '11214')
    .&End;

  ListView1.Items.Clear;
  ListView1.Columns.Clear;
  ListView1.ViewStyle := vsReport;
  AddColumn('Name');
  AddColumn('Cuisine');
  AddColumn('Street');
  AddColumn('Building');
  AddColumn('Borough');
  ListView1.Items.BeginUpdate;
  try

    while oCrs.Next do
    begin
      oItem := ListView1.Items.Add;
      for i := 1 to ListView1.Columns.Count - 1 do
        oItem.SubItems.Add('');

      // The use of the TJSONIterator.Iterate method is a less flexible way than
      // the use of TJSONIterator.Next/Recurse/Return methods. But it is more
      // simple and still flexible.
      oCrs.Doc.Iterate(
        function(AIter: TJSONIterator): Boolean
        begin
          if AIter.Path = 'name' then
            oItem.Caption := AIter.AsString
          else if AIter.Path = 'cuisine' then
            oItem.SubItems[0] := AIter.AsString
          else if AIter.Path = 'address.street' then
            oItem.SubItems[1] := AIter.AsString
          else if AIter.Path = 'address.building' then
            oItem.SubItems[2] := AIter.AsString
          else if AIter.Path = 'borough' then
            oItem.SubItems[3] := AIter.AsString;
          Result := True;
        end);
    end;

  finally
    ListView1.Items.EndUpdate;
  end;
end;

end.
