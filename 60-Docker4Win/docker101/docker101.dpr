program docker101;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Writeln('Hello from docker!');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
