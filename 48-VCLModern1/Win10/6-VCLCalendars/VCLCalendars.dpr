program VCLCalendars;

uses
  Vcl.Forms,
  uFormCals in 'uFormCals.pas' {Form3},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 SlateGray');
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
