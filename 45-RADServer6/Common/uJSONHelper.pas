unit uJSONHelper;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.BatchMove, Data.DB,
  FireDAC.Comp.BatchMove.DataSet, FireDAC.Stan.Intf,
  FireDAC.Comp.BatchMove.JSON, REST.Response.Adapter, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  System.JSON, FireDAC.DApt, FireDAC.Stan.StorageBin;

type
  TJSONDM = class(TDataModule)
    FDBatchMoveJSONWriter1: TFDBatchMoveJSONWriter;
    FDBatchMoveDataSetReader1: TFDBatchMoveDataSetReader;
    FDBatchMove1: TFDBatchMove;
    JsonData: TFDMemTable;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TDataSetJSONHelper = class helper for TDataSet
  private
    procedure JsonToDataset(aJSON: TJSONObject; aDataset: TDataSet);
  public
    function AsJSONString: string;
    function AsJSONArray: TJSONArray;
    function AsJSONObject: TJSONObject;
    function AsJSONStream: TStream;
    procedure InsertFromJSON(aJSON: TJSONObject);
    procedure UpdateFromJSON(aJSON: TJSONObject);
  end;

function MemTableCreateDelta(const AFDMemTable: TFDMemTable): TFDMemTable;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}
{ TDataSetJSONHelper }

function TDataSetJSONHelper.AsJSONString: string;
var
  lDM: TJSONDM;
  lSTR: TStringStream;
begin
  lDM := TJSONDM.Create(nil);
  lSTR := TStringStream.Create;
  try
    Self.DisableControls;
    if not Self.Active then
      Self.Active := True;
    lDM.FDBatchMoveDataSetReader1.DataSet := Self;
    lDM.FDBatchMoveJSONWriter1.Stream := lSTR;
    lDM.FDBatchMove1.Execute;
    Result := lSTR.DataString;
  finally
    lDM.Free;
    lSTR.Free;
    Self.EnableControls;
  end;
end;

function TDataSetJSONHelper.AsJSONArray: TJSONArray;
var
  lDM: TJSONDM;
begin
  lDM := TJSONDM.Create(nil);
  Result := TJSONArray.Create;
  try
    Self.DisableControls;
    if not Self.Active then
      Self.Active := True;
    lDM.FDBatchMoveDataSetReader1.DataSet := Self;
    lDM.FDBatchMoveJSONWriter1.JsonArray := Result;
    lDM.FDBatchMove1.Execute;
  finally
    lDM.Free;
    Self.EnableControls;
  end;
end;

function TDataSetJSONHelper.AsJSONObject: TJSONObject;
begin
  Result := AsJSONArray.Items[0] as TJSONObject;
end;

function TDataSetJSONHelper.AsJSONStream: TStream;
var
  lDM: TJSONDM;
begin
  lDM := TJSONDM.Create(nil);
  Result := TStringStream.Create;
  try
    Self.DisableControls;
    if not Self.Active then
      Self.Active := True;
    lDM.FDBatchMoveDataSetReader1.DataSet := Self;
    lDM.FDBatchMoveJSONWriter1.Stream := Result;
    lDM.FDBatchMove1.Execute;
  finally
    lDM.Free;
    Self.EnableControls;
  end;
end;

procedure TDataSetJSONHelper.JsonToDataset(aJSON: TJSONObject;
  aDataset: TDataSet);
var
  vConv: TCustomJSONDataSetAdapter;
begin
  vConv := TCustomJSONDataSetAdapter.Create(Nil);
  try
    vConv.DataSet := aDataset;
    vConv.UpdateDataSet(aJSON);
  finally
    vConv.Free;
  end;
end;

procedure TDataSetJSONHelper.InsertFromJSON(aJSON: TJSONObject);
var
  lDM: TJSONDM;
  i: Integer;
begin
  Self.DisableControls;
  lDM := TJSONDM.Create(nil);
  try
    JsonToDataset(aJSON, lDM.JsonData);
    Self.Append;
    for i := 0 to lDM.JsonData.FieldCount - 1 do
      Self.FieldByName(lDM.JsonData.Fields[i].FieldName).Value :=
        lDM.JsonData.Fields[i].Value;
    Self.Post;
  finally
    lDM.Free;
    Self.EnableControls;
  end;
end;

procedure TDataSetJSONHelper.UpdateFromJSON(aJSON: TJSONObject);
var
  lDM: TJSONDM;
  i: Integer;
begin
  Self.DisableControls;
  lDM := TJSONDM.Create(nil);
  try
    JsonToDataset(aJSON, lDM.JsonData);
    Self.Edit;
    for i := 0 to lDM.JsonData.FieldCount - 1 do
      Self.FieldByName(lDM.JsonData.Fields[i].FieldName).Value :=
        lDM.JsonData.Fields[i].Value;
    Self.Post;
  finally
    lDM.Free;
    Self.EnableControls;
  end;
end;

function MemTableCreateDelta(const AFDMemTable: TFDMemTable): TFDMemTable;

  procedure CopyDataSet(const ASource, ADest: TFDAdaptedDataSet);
  var
    LStream: TStream;
  begin
    LStream := TMemoryStream.Create;
    try
      ASource.SaveToStream(LStream, TFDStorageFormat.sfBinary);
      LStream.Seek(0, TSeekOrigin.soBeginning);
      ADest.LoadFromStream(LStream, TFDStorageFormat.sfBinary);
    finally
      LStream.Free;
    end;
  end;

var
  LStoreItems: TFDStoreItems;
begin
  Result := TFDMemTable.Create(nil);
  Result.ResourceOptions.StoreItems := [siMeta, siDelta];
  LStoreItems := AFDMemTable.ResourceOptions.StoreItems;
  AFDMemTable.DisableControls;
  try
    AFDMemTable.ResourceOptions.StoreItems := [siMeta, siDelta];
    CopyDataSet(AFDMemTable, Result);
  finally
    AFDMemTable.ResourceOptions.StoreItems := LStoreItems;
    AFDMemTable.EnableControls;
  end;
end;

end.
