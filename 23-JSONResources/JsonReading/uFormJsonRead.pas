unit uFormJsonRead;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation,
  System.JSON, System.JSON.Types, System.JSON.Readers, uCustomTypes;

type
  TFormJsonRead = class(TForm)
    ToolBar1: TToolBar;
    ButtonReadTokens: TButton;
    ButtonReadReader: TButton;
    ButtonReadDOM: TButton;
    MemoSrc: TMemo;
    ListBoxTokens: TListBox;
    MemoLog: TMemo;
    procedure ButtonReadTokensClick(Sender: TObject);
    procedure ButtonReadReaderClick(Sender: TObject);
    procedure ButtonReadDOMClick(Sender: TObject);
  private
    function GetJsonText: string;
    procedure ProcessStockRead(stocks: TStocks; jtr: TJsonTextReader);
    procedure ProcessStockObj(stocks: TStocks; stockObj: TJSONObject);
    procedure DisplayStocks(stocks: TStocks);
    procedure Log(s: string);
  public
    { Public declarations }
  end;

var
  FormJsonRead: TFormJsonRead;

implementation

{$R *.fmx}

uses JsonUtils;

function TFormJsonRead.GetJsonText: string;
var s: string;
begin
  for s in MemoSrc.Lines do
    Result := Result + s;
end;

procedure TFormJsonRead.Log(s: string);
begin
  MemoLog.Lines.Add(s);
end;

procedure TFormJsonRead.DisplayStocks(stocks: TStocks);
var stock: TStock; i: integer;
begin
  MemoLog.Lines.Clear;

  Log(DateTimeToStr(Now));

  if stocks <> nil then
  begin
    Log('Stocks count = ' + stocks.Count.ToString);
    Log('==================');

    for i := 0 to stocks.Count-1 do
    begin
      stock := stocks[i];
      Log('Stock Nr: ' + i.ToString);
      Log('Symbol: ' + stock.Symbol);
      Log('Price: ' + stock.price.ToString);
      Log('==================');
    end;
  end
  else
    Log('No stocks found.');
end;

procedure TFormJsonRead.ButtonReadDOMClick(Sender: TObject);
var stocks: TStocks; valRoot: TJSONValue; objRoot: TJSONObject;
  valStocks: TJSONValue; arrStocks: TJSONArray; i: integer;
begin
  stocks := TStocks.Create;
  try
    valRoot := TJSONObject.ParseJSONValue(GetJsonText);
    if valRoot <> nil then
    begin
      if valRoot is TJSONObject then
      begin
        objRoot := TJSONObject(valRoot);
        if objRoot.Count > 0 then
        begin
          valStocks := objRoot.Values['Stocks'];
          if valStocks <> nil then
          begin
            if valStocks is TJSONArray then
            begin
              arrStocks := TJSONArray(valStocks);
              for i := 0 to arrStocks.Count-1 do
              begin
                if arrStocks.Items[i] is TJSONObject then
                  ProcessStockObj(stocks, TJSONObject(arrStocks.Items[i]));
              end;
            end;

          end;

        end;

      end;
    end;

    DisplayStocks(stocks);
  finally
    stocks.Free;
  end;
end;

procedure TFormJsonRead.ProcessStockObj(stocks: TStocks; stockObj: TJSONObject);
var stock: TStock; val: TJSONValue;
begin
  stock := TStock.Create;

  val := stockObj.Values['symbol'];
  if val <> nil then
    if val is TJSONString then
      stock.Symbol := TJSONString(val).Value;

  val := stockObj.Values['price'];
  if val <> nil then
    if val is TJSONNumber then
      stock.price := TJSONNumber(val).AsDouble;

  stocks.Add(stock);
end;

procedure TFormJsonRead.ButtonReadReaderClick(Sender: TObject);
var sr: TStringReader;
    jtr: TJsonTextReader;
    stocks: TStocks;
begin
  stocks := TStocks.Create;
  try
    sr := TStringReader.Create(GetJsonText);
    try
      jtr := TJsonTextReader.Create(sr);
      try
        while jtr.Read do
        begin
          if jtr.TokenType = TJsonToken.StartObject then
            ProcessStockRead(stocks, jtr);
        end;
      finally
        jtr.Free;
      end;
    finally
      sr.Free;
    end;

    DisplayStocks(stocks);
  finally
    stocks.Free;
  end;
end;

procedure TFormJsonRead.ProcessStockRead(stocks: TStocks; jtr: TJsonTextReader);
var stock: TStock;
begin
  stock := TStock.Create;

  while jtr.Read do
  begin
    if jtr.TokenType = TJsonToken.PropertyName then
    begin
      if jtr.Value.ToString = 'symbol' then
      begin
        jtr.Read;
        stock.Symbol := jtr.Value.AsString;
      end

      else if jtr.Value.ToString = 'price' then
      begin
        jtr.Read;
        stock.price := jtr.Value.AsExtended;
      end
    end

    else if jtr.TokenType = TJsonToken.EndObject then
    begin
      stocks.add(stock);
      exit;
    end;
  end;
end;

procedure TFormJsonRead.ButtonReadTokensClick(Sender: TObject);
var jsontext: string; sr: TStringReader; jtr: TJsonTextReader; s: string;
begin
  ListBoxTokens.Items.Clear;

  jsontext := GetJsonText;

  sr := TStringReader.Create(jsontext);
  try
    jtr := TJsonTextReader.Create(sr);
    try
      while jtr.Read do
      begin
        s := JsonTokenToString(jtr.TokenType);
        ListBoxTokens.Items.Add(s);
      end;
    finally
      jtr.Free;
    end;
  finally
    sr.Free;
  end;
end;

end.
