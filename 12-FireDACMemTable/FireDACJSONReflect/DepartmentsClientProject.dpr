//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program DepartmentsClientProject;

uses
  FMX.Forms,
  DepartmentClientFormU in 'DepartmentClientFormU.pas' {DepartmentsClientForm},
  ClientClassesUnit2 in 'ClientClassesUnit2.pas',
  ClientModuleUnit2 in 'ClientModuleUnit2.pas' {ClientModule2: TDataModule};

{$R *.res}

begin
//  TObject.Create;
//  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TDepartmentsClientForm, DepartmentsClientForm);
  Application.CreateForm(TClientModule2, ClientModule2);
  Application.Run;
end.
