program AnimationSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  AnimationUnit1 in 'AnimationUnit1.pas' {Form29},
  AnimationUnit2 in 'AnimationUnit2.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm29, Form29);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
