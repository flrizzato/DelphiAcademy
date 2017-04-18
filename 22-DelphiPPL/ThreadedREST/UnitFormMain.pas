unit UnitFormMain;

{ ===============================================================================
  CodeRage 9 - Demo responsive REST client using TTask

  This code shows how to do REST calls without blocking the user interface
  for the duration of the call. This code uses TTask to run the REST call
  in parallel.

  Please note that you could also use RESTRequest.ExecuteAsync.

  This demo works with RAD Studio XE7.
  The discogs API changes frequently, this REST request works with the current
  api of October 2014. Please update the RESTRequest code if the api has changed.

  Author: Danny Wind
  =============================================================================== }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, Vcl.ExtCtrls,
  Vcl.StdCtrls, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  Vcl.ComCtrls;

type
  TFormMain = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Memo5: TMemo;
    Memo6: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBoxUseThreading: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    function DoRESTCall(aArtist: string): string;
  public
    { Public declarations }
    procedure ExecuteRESTCall(aMemo: TMemo; aArtist: string);
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.Diagnostics, System.Threading;

{$R *.dfm}

procedure TFormMain.Button1Click(Sender: TObject);
begin
  ExecuteRESTCall(Memo1, (Sender as TButton).Tag.ToString);
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  ExecuteRESTCall(Memo2, (Sender as TButton).Tag.ToString);
end;

procedure TFormMain.Button3Click(Sender: TObject);
begin
  ExecuteRESTCall(Memo3, (Sender as TButton).Tag.ToString);
end;

procedure TFormMain.Button4Click(Sender: TObject);
begin
  ExecuteRESTCall(Memo4, (Sender as TButton).Tag.ToString);
end;

procedure TFormMain.Button5Click(Sender: TObject);
begin
  ExecuteRESTCall(Memo5, (Sender as TButton).Tag.ToString);
end;

procedure TFormMain.Button6Click(Sender: TObject);
begin
  ExecuteRESTCall(Memo6, (Sender as TButton).Tag.ToString);
end;

function TFormMain.DoRESTCall(aArtist: string): string;
var
  lRESTClient: TRESTClient;
  lRESTRequest: TRESTRequest;
  lRESTResponse: TRESTResponse;
begin
  lRESTClient := TRESTClient.Create('http://api.discogs.com');
  lRESTRequest := TRESTRequest.Create(nil);
  lRESTResponse := TRESTResponse.Create(nil);
  lRESTRequest.Client := lRESTClient;
  lRESTRequest.Response := lRESTResponse;
  lRESTRequest.Resource := 'artists/{ID}';
  lRESTRequest.Params[0].Value := aArtist;
  try
    lRESTRequest.Execute;
    Result := lRESTResponse.Content;
  finally { Free resources within thread execution }
    lRESTResponse.Free;
    lRESTRequest.Free;
    lRESTClient.Free;
  end;
end;

procedure TFormMain.ExecuteRESTCall(aMemo: TMemo; aArtist: string);
var
  lStopWatch: TStopWatch;
begin
  aMemo.Lines.Clear;
  aMemo.Repaint;
  if NOT CheckBoxUseThreading.Checked then
  begin { Blocking REST call }
    lStopWatch := TStopWatch.StartNew;
    RESTRequest1.Params[0].Value := aArtist;
    RESTRequest1.Execute;
    lStopWatch.Stop;
    aMemo.Lines.Add(RESTResponse1.Content);
    aMemo.Lines.Add(lStopWatch.ElapsedMilliseconds.ToString + ' ms.');
  end
  else
  begin
    { TTask parallel REST call }
    TTask.Run(
      procedure
      var
        lResult: string;
      begin
        lResult := DoRESTCall(aArtist);
        TThread.Synchronize(nil,
          procedure
          begin
            aMemo.Lines.Add(lResult);
          end);
      end);
  end;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  ProgressBar1.StepIt;
end;

end.
