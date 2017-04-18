program JSONViewer;

uses
  Vcl.Forms,
  uFormJsonViewer in 'uFormJsonViewer.pas' {FormJsonView},
  Vcl.Themes,
  Vcl.Styles,
  uDMHTTP in 'uDMHTTP.pas' {DMHTTP: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 SlateGray');
  Application.CreateForm(TFormJsonView, FormJsonView);
  Application.Run;
end.
