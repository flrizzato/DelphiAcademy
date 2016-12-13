//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program RESTDemos;

uses
  Vcl.Forms,
  uMain_frm in 'uMain_frm.pas' {frm_Main},
  REST.Authenticator.OAuth.WebForm.Win {frm_OAuthWebForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tfrm_Main, frm_Main);
  Application.CreateForm(Tfrm_OAuthWebForm, frm_OAuthWebForm);
  Application.Run;
end.
