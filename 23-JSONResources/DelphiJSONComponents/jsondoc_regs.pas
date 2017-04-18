unit jsondoc_regs;

// ***********************************************************************
//
//   JSON Document Component
//
//   pawel.glowacki@embarcadero.com
//
//   July 2010 - version 1.0
//   February 2016 - version 1.1
//
// ***********************************************************************


interface

{$R jsondoc.dcr}

procedure Register;

implementation

uses
  System.Classes, jsondoc;

procedure Register;
begin
  RegisterComponents('JSON', [TJSONDocument]);
end;

end.
