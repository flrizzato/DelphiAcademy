unit uCustomTypes;

interface

uses
  System.Generics.Collections;

type
  TStock = class
  private
    FSymbol: string;
    FPrice: double;
    procedure SetPrice(const Value: double);
    procedure SetSymbol(const Value: string);
  public
    property Symbol: string read FSymbol write SetSymbol;
    property Price: double read FPrice write SetPrice;
  end;

  TStocks = TObjectList<TStock>;

implementation

{ TStock }

procedure TStock.SetPrice(const Value: double);
begin
  FPrice := Value;
end;

procedure TStock.SetSymbol(const Value: string);
begin
  FSymbol := Value;
end;

end.
