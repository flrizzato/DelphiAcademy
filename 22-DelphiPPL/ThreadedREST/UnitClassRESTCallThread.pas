unit UnitClassRESTCallThread;

interface

uses
  System.Classes, REST.Client, VCL.StdCtrls;

type
  TRESTCallThread = class(TThread)
  private
    FArtist: string;
    FMemo: TMemo;
    FElapsedMilliseconds: integer;
  protected
    procedure Execute; override;
  public
    property Artist:string read FArtist write FArtist;
    property ResponseMemo: TMemo read FMemo write FMemo;
  end;

function DoRESTCall(aArtist: string): string;

implementation

uses
  System.SysUtils, System.Diagnostics;

function DoRESTCall(aArtist: string): string;
var
  lRESTClient: TRESTClient;
  lRESTRequest: TRESTRequest;
  lRESTResponse:TRESTResponse;
begin
  lRESTClient := TRESTClient.Create('http://api.discogs.com');
  lRESTRequest := TRESTRequest.Create(nil);
  lRESTResponse := TRESTResponse.Create(nil);
  lRESTRequest.Client := lRESTClient;
  lRESTRequest.Response := lRESTResponse;
  lRESTRequest.Resource := 'database/search?artist={NAME}';
  lRESTRequest.Params[0].Value := aArtist;
  try
    lRESTRequest.Execute;
    Result := lRESTResponse.Content;
  finally {Free resources within thread execution}
    lRESTResponse.Free;
    lRESTRequest.Free;
    lRESTClient.Free;
  end;
end;

{ TRESTCallThread }

procedure TRESTCallThread.Execute;
var
  lStopWatch: TStopWatch;
  lRESTContent: string;
begin
  if not(FArtist.IsEmpty) then
  begin
    lStopWatch := TStopWatch.StartNew;
    lRESTContent := DoRESTCall(FArtist);
    lStopWatch.Stop;
    Synchronize(procedure
                begin
                  if Assigned(FMemo) then
                  begin
                    FMemo.Lines.Add(lStopWatch.ElapsedMilliseconds.ToString + ' ms.');
                    FMemo.Lines.Add(lRESTContent);
                  end;
                end);
  end
  else
  begin
    if Assigned(FMemo) then
    begin
      Synchronize(procedure
                  begin
                    FMemo.Clear;
                    FMemo.Lines.Add('FArtist was leeg.');
                  end);
    end;
  end;
end;

end.
