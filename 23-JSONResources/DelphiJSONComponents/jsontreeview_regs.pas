unit jsontreeview_regs;

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

procedure Register;

implementation

uses
  System.Classes, JSONTreeView;

procedure Register;
begin
  RegisterComponents('JSON', [TJSONTreeView]);
end;

end.
