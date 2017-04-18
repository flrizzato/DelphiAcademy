unit JSONTreeView;

// ***********************************************************************
//
//   JSON TreeView Component
//
//   pawel.glowacki@embarcadero.com
//
//   July 2010 - version 1.0
//   February 2016 - version 1.1
//
// ***********************************************************************

interface

uses
  System.Classes, System.SysUtils, System.JSON, jsondoc, Vcl.ComCtrls;

type
  EUnknownJsonValueDescendant = class(Exception)
    constructor Create;
  end;

  TJSONTreeView = class(TTreeView)
  private
    FJSONDocument: TJSONDocument;
    FVisibleChildrenCounts: boolean;
    FVisibleByteSizes: boolean;
    procedure SetJSONDocument(const Value: TJSONDocument);
    procedure SetVisibleChildrenCounts(const Value: boolean);
    procedure SetVisibleByteSizes(const Value: boolean);
    procedure ProcessElement(currNode: TTreeNode; arr: TJSONArray;
      aIndex: integer);
    procedure ProcessPair(currNode: TTreeNode; obj: TJSONObject;
      aIndex: integer);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    class function IsSimpleJsonValue(v: TJSONValue): boolean; inline;
    class function UnQuote(s: string): string; inline;
    constructor Create(AOwner: TComponent); override;
    procedure ClearAll;
    procedure LoadJson;
  published
    property JSONDocument: TJSONDocument
      read FJSONDocument write SetJSONDocument;
    property VisibleChildrenCounts: boolean
      read FVisibleChildrenCounts write SetVisibleChildrenCounts;
    property VisibleByteSizes: boolean
      read FVisibleByteSizes write SetVisibleByteSizes;
  end;

implementation

{ TJSONTreeView }

procedure TJSONTreeView.ClearAll;
begin
  Items.Clear;
end;

constructor TJSONTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FVisibleChildrenCounts := true;
  FVisibleByteSizes := false;
end;

class function TJSONTreeView.IsSimpleJsonValue(v: TJSONValue): boolean;
begin
  Result := (v is TJSONNumber)
    or (v is TJSONString)
    or (v is TJSONTrue)
    or (v is TJSONFalse)
    or (v is TJSONNull);
end;

procedure TJSONTreeView.LoadJson;
var v: TJSONValue; currNode: TTreeNode; i, aCount: integer; s: string;
begin
  ClearAll;

  if (JSONDocument <> nil) and JSONDocument.IsActive then
  begin
    v := JSONDocument.RootValue;

    Items.BeginUpdate;
    try
      Items.Clear;

      if IsSimpleJsonValue(v) then
        Items.AddChild(nil, UnQuote(v.Value))

      else
      if v is TJSONObject then
      begin
        aCount := TJSONObject(v).Count;
        s := '{}';
        if VisibleChildrenCounts then
          s := s + ' (' + IntToStr(aCount) + ')';
        if VisibleByteSizes then
          s := s + ' (' + IntToStr(v.EstimatedByteSize) + ' bytes)';
        currNode := Items.AddChild(nil, s);
        for i := 0 to aCount - 1 do
          ProcessPair(currNode, TJSONObject(v), i)
      end

      else
      if v is TJSONArray then
      begin
        aCount := TJSONArray(v).Count;
        s := '[]';
        if VisibleChildrenCounts then
          s := s + ' (' + IntToStr(aCount) + ')';
        if VisibleByteSizes then
          s := s + ' (' + IntToStr(v.EstimatedByteSize) + ' bytes)';
        currNode := Items.AddChild(nil, s);
        for i := 0 to aCount - 1 do
          ProcessElement(currNode, TJSONArray(v), i)
      end

      else
        raise EUnknownJsonValueDescendant.Create;

    finally
      Items.EndUpdate;
    end;

    FullExpand;
  end;
end;

procedure TJSONTreeView.ProcessPair(currNode: TTreeNode; obj: TJSONObject; aIndex: integer);
var p: TJSONPair; s: string; n: TTreeNode; i, aCount: integer;
begin
  p := obj.Pairs[aIndex];

  s := UnQuote(p.JsonString.ToString) + ' : ';

  if IsSimpleJsonValue(p.JsonValue) then
  begin
    Items.AddChild(currNode, s + p.JsonValue.ToString);
    exit;
  end;

  if p.JsonValue is TJSONObject then
  begin
    aCount := TJSONObject(p.JsonValue).Count;
    s := s + ' {}';
    if VisibleChildrenCounts then
      s := s + ' (' + IntToStr(aCount) + ')';
    if VisibleByteSizes then
        s := s + ' (' + IntToStr(p.EstimatedByteSize) + ' bytes)';
    n := Items.AddChild(currNode, s);
    for i := 0 to aCount - 1 do
      ProcessPair(n, TJSONObject(p.JsonValue), i);
  end

  else if p.JsonValue is TJSONArray then
  begin
    aCount := TJSONArray(p.JsonValue).Count;
    s := s + ' []';
    if VisibleChildrenCounts then
      s := s + ' (' + IntToStr(aCount) + ')';
    if VisibleByteSizes then
        s := s + ' (' + IntToStr(p.EstimatedByteSize) + ' bytes)';
    n := Items.AddChild(currNode, s);
    for i := 0 to aCount - 1 do
      ProcessElement(n, TJSONArray(p.JsonValue), i);
  end
  else
    raise EUnknownJsonValueDescendant.Create;
end;

procedure TJSONTreeView.ProcessElement(currNode: TTreeNode; arr: TJSONArray; aIndex: integer);
var v: TJSONValue; s: string; n: TTreeNode; i, aCount: integer;
begin
  v := arr.Items[aIndex];
  s := '[' + IntToStr(aIndex) + '] ';

  if IsSimpleJsonValue(v) then
  begin
    Items.AddChild(currNode, s + v.ToString);
    exit;
  end;

  if v is TJSONObject then
  begin
    aCount := TJSONObject(v).Count;
    s := s + ' {}';
    if VisibleChildrenCounts then
      s := s + ' (' + IntToStr(aCount) + ')';
    if VisibleByteSizes then
        s := s + ' (' + IntToStr(v.EstimatedByteSize) + ' bytes)';
    n := Items.AddChild(currNode, s);
    for i := 0 to aCount - 1 do
      ProcessPair(n, TJSONObject(v), i);
  end

  else if v is TJSONArray then
  begin
    aCount := TJSONArray(v).Count;
    s := s + ' []';
    n := Items.AddChild(currNode, s);
    if VisibleChildrenCounts then
      s := s + ' (' + IntToStr(aCount) + ')';
    if VisibleByteSizes then
        s := s + ' (' + IntToStr(v.EstimatedByteSize) + ' bytes)';
    for i := 0 to aCount - 1 do
      ProcessElement(n, TJSONArray(v), i);
  end
  else
    raise EUnknownJsonValueDescendant.Create;

end;

procedure TJSONTreeView.SetJSONDocument(const Value: TJSONDocument);
begin
  if FJSONDocument <> Value then
  begin
    FJSONDocument := Value;
    ClearAll;
    if FJSONDocument <> nil then
    begin
      if FJSONDocument.IsActive then
        LoadJson;
    end;
  end;
end;

procedure TJSONTreeView.SetVisibleByteSizes(const Value: boolean);
begin
  if FVisibleByteSizes <> Value then
  begin
    FVisibleByteSizes := Value;
    LoadJson;
  end;
end;

procedure TJSONTreeView.SetVisibleChildrenCounts(const Value: boolean);
begin
  if FVisibleChildrenCounts <> Value then
  begin
    FVisibleChildrenCounts := Value;
    LoadJson;
  end;
end;

class function TJSONTreeView.UnQuote(s: string): string;
begin
  Result := Copy(s,2,Length(s)-2);
end;

procedure TJSONTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if FJSONDocument <> nil then
      if AComponent = FJSONDocument then
      begin
        FJSONDocument := nil;
        ClearAll;
      end;
end;

{ EUnknownJsonValueDescendant }

resourcestring
  StrUnknownTJSONValueDescendant = 'Unknown TJSONValue descendant';

constructor EUnknownJsonValueDescendant.Create;
begin
  inherited Create(StrUnknownTJSONValueDescendant);
end;

end.
