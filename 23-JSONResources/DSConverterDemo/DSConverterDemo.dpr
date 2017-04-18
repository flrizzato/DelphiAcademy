program DSConverterDemo;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form1},
  DataSetConverter4D.Helper in '..\DSConverter4Delphi\DataSetConverter4D.Helper.pas',
  DataSetConverter4D.Impl in '..\DSConverter4Delphi\DataSetConverter4D.Impl.pas',
  DataSetConverter4D in '..\DSConverter4Delphi\DataSetConverter4D.pas',
  DataSetConverter4D.Util in '..\DSConverter4Delphi\DataSetConverter4D.Util.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
