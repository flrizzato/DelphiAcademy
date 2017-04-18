unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, IPPeerClient, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.Controls.Presentation, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors
{$IFDEF MACOS}, Macapi.CoreFoundation {$ENDIF};

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    butLeft: TSpeedButton;
    butRight: TSpeedButton;
    lblPersonagem: TLabel;
    imgPersonagem: TImage;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    AniIndicator1: TAniIndicator;
    lblDataHora: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure butLeftClick(Sender: TObject);
    procedure butRightClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FOffSet: integer;
    procedure ExecuteRequest;
    procedure TaskFinished;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses System.UIConsts, System.DateUtils, System.JSON, System.Threading,
  IdHashMessageDigest, IdHttp;

const
  PUBLIC_KEY: string = 'a058d39a512955bc78905eea7307f733';
  PRIVATE_KEY: string = 'c082618550a1a3937d9aacb4a484658fac81e30a';

procedure TMainForm.butLeftClick(Sender: TObject);
begin
  if FOffSet >= 1 then
    Dec(FOffSet);
  ExecuteRequest;
end;

procedure TMainForm.butRightClick(Sender: TObject);
begin
  Inc(FOffSet);
  ExecuteRequest;
end;

procedure TMainForm.ExecuteRequest;
var
  aTask: ITask;

  sHash: string;
  sTimeStamp: string;
  IdHashMessageDigest: TIdHashMessageDigest;

  aJSONObject: TJSONObject;
  aJS0NData: TJSONObject;
  aJSONResult: TJSONObject;
  aThumbnail: TJSONObject;

  aHttp: TIdHttp;
  aStream: TMemoryStream;
begin
  aTask := TTask.Create(
    procedure()
    begin
      AniIndicator1.Visible := True;
      try
        sTimeStamp := IntToStr(DateTimeToUnix(Now));
        IdHashMessageDigest := TIdHashMessageDigest5.Create;
        sHash := IdHashMessageDigest.HashStringAsHex(sTimeStamp + PRIVATE_KEY +
          PUBLIC_KEY).ToLower;

        RESTRequest1.Params.ParameterByName('OFFSET').Value := FOffSet.ToString;
        RESTRequest1.Params.ParameterByName('TS').Value := sTimeStamp;
        RESTRequest1.Params.ParameterByName('APIKEY').Value := PUBLIC_KEY;
        RESTRequest1.Params.ParameterByName('HASH').Value := sHash;
        RESTRequest1.Execute;

        aJSONObject := TJSONObject.ParseJSONValue(RESTResponse1.Content)
          as TJSONObject;
        aJS0NData := aJSONObject.GetValue('data') as TJSONObject;
        aJSONResult := (aJS0NData.GetValue('results') as TJSONArray)
          .Items[0] as TJSONObject;
        aThumbnail := aJSONResult.GetValue('thumbnail') as TJSONObject;

        aHttp := TIdHttp.Create(nil);
        aStream := TMemoryStream.Create;
        aHttp.Get(aThumbnail.GetValue('path').Value + '.' +
          aThumbnail.GetValue('extension').Value, aStream);

        if aStream.Size > 0 then
          TThread.Queue(nil,
            procedure()
            begin
              aStream.Position := 0;
              lblPersonagem.Text := aJSONResult.GetValue('name').Value;
              imgPersonagem.Bitmap.LoadFromStream(aStream);
            end);
      finally
        aHttp.Free;
        IdHashMessageDigest.Free;
        TThread.Queue(nil, TaskFinished);
      end;
    end);

  aTask.Start;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Timer1.Enabled := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FOffSet := 0;
  ExecuteRequest;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TMainForm.TaskFinished;
begin
  AniIndicator1.Visible := False;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  TThread.Queue(nil,
    procedure ()
     begin
       lblDataHora.Text := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
     end);
end;

end.
