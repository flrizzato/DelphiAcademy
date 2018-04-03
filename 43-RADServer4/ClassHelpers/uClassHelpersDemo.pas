unit uClassHelpersDemo;

interface

uses System.Classes;

type
  TStringsHelper = class helper for TStrings
  public
    function Contains(const aString: string): boolean;
  end;

implementation

{ TStringsHelper }

function TStringsHelper.Contains(const aString: string): boolean;
begin
  result := -1 <> IndexOf(aString);
end;

end.
