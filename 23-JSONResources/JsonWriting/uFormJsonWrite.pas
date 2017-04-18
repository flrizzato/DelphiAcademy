unit uFormJsonWrite;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation;

type
  TFormJsonWrite = class(TForm)
    ToolBar1: TToolBar;
    MemoLog: TMemo;
    ButtonWriter: TButton;
    ButtonDOM: TButton;
    ButtonBuilder: TButton;
    procedure ButtonWriterClick(Sender: TObject);
    procedure ButtonDOMClick(Sender: TObject);
    procedure ButtonBuilderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormJsonWrite: TFormJsonWrite;

implementation

{$R *.fmx}

uses
  System.JSON,
  System.JSON.Types,
  System.JSON.Writers,
  System.JSON.Builders;

procedure TFormJsonWrite.ButtonDOMClick(Sender: TObject);
var
  objStocks, objS1, objS2: TJSONObject;
  arrStocks: TJSONArray;
begin
  objStocks := TJSONObject.Create;
  try
    arrStocks := TJSONArray.Create;

    objS1 := TJSONObject.Create;
    objS1.AddPair('symbol', TJSONString.Create('ACME'));
    objS1.AddPair('price', TJSONNumber.Create(75.5));
    arrStocks.Add(objS1);

    objS2 := TJSONObject.Create;
    objS2.AddPair('symbol', TJSONString.Create('COOL'));
    objS2.AddPair('price', TJSONNumber.Create(21.7));
    arrStocks.Add(objS2);

    objStocks.AddPair('Stocks', arrStocks);

    MemoLog.Lines.Clear;
    MemoLog.Lines.Add(objStocks.ToString);

  finally
    objStocks.Free;
  end;
end;

procedure TFormJsonWrite.ButtonWriterClick(Sender: TObject);
var
  StringWriter: TStringWriter;
  Writer: TJsonTextWriter;
begin
  StringWriter := TStringWriter.Create();
  Writer := TJsonTextWriter.Create(StringWriter);
  try
    Writer.Formatting := TJsonFormatting.Indented;

    Writer.WriteStartObject;
    Writer.WritePropertyName('Stocks');
    Writer.WriteStartArray;

    Writer.WriteStartObject;
    Writer.WritePropertyName('symbol');
    Writer.WriteValue('ACME');
    Writer.WritePropertyName('price');
    Writer.WriteValue(75.5);
    Writer.WriteEndObject;

    Writer.WriteStartObject;
    Writer.WritePropertyName('symbol');
    Writer.WriteValue('COOL');
    Writer.WritePropertyName('price');
    Writer.WriteValue(21.7);
    Writer.WriteEndObject;

    Writer.WriteEndArray;
    Writer.WriteEndObject;

    MemoLog.Lines.Clear;
    MemoLog.Lines.Add(StringWriter.ToString);

  finally
    Writer.Free;
    StringWriter.Free;
  end;

end;

procedure TFormJsonWrite.ButtonBuilderClick(Sender: TObject);
var
  StringWriter: TStringWriter;
  Writer: TJsonTextWriter;
  Builder: TJSONObjectBuilder;

begin
  StringWriter := TStringWriter.Create();
  Writer := TJsonTextWriter.Create(StringWriter);
  Builder := TJSONObjectBuilder.Create(Writer);
  try
    Writer.Formatting := TJsonFormatting.Indented;

    Builder
    .BeginObject
      .BeginArray('Stocks')
        .BeginObject
          .Add('symbol', 'ACME')
          .Add('price', 75.5)
        .EndObject
        .BeginObject
          .Add('symbol', 'COOL')
          .Add('price', 21.7)
        .EndObject
      .EndArray
    .EndObject;

    MemoLog.Lines.Clear;
    MemoLog.Lines.Add(StringWriter.ToString);

  finally
    Builder.Free;
    Writer.Free;
    StringWriter.Free;
  end;
end;

end.
