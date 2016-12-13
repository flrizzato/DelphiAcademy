//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit fmWorkBench;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.TabControl, FMX.Edit;

type
  TWorkBenchForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    mInput: TMemo;
    mBson: TMemo;
    mJson: TMemo;
    btRight: TButton;
    btLeft: TButton;
    Label1: TLabel;
    Label2: TLabel;
    btBeautify: TButton;
    btMinify: TButton;
    gbButtons: TGroupBox;
    btToWriter: TButton;
    btToDelphi: TButton;
    lbUseBuilders: TCheckBox;
    WebBrowser1: TWebBrowser;
    procedure btBeautifyClick(Sender: TObject);
    procedure btMinifyClick(Sender: TObject);
    procedure btRightClick(Sender: TObject);
    procedure btLeftClick(Sender: TObject);
    procedure btToDelphiClick(Sender: TObject);
    procedure btToWriterClick(Sender: TObject);
    function BuildCommand(const ACode, AType: string; Wrapped: Boolean): string;
    procedure LoadCode(const ACode, AType: string; Wrapped: Boolean = False);
    procedure LoadError(const Msg: string);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WorkBenchForm: TWorkBenchForm;

implementation

uses System.NetEncoding, System.JSON.Types, System.IOUtils, System.DateUtils, Converters, Writers;

{$R *.fmx}

{ TForm1 }

procedure TWorkBenchForm.btBeautifyClick(Sender: TObject);
begin
  try
    LoadCode(TConverters.JsonReformat(mInput.Text, True), 'javascript');
  except
    on E: EJsonException do
      LoadError(E.Message);
  end;
end;

function TWorkBenchForm.BuildCommand(const ACode, AType: string; Wrapped: Boolean): string;
var
  LEncoder: TBase64Encoding;
begin
  LEncoder := TBase64Encoding.Create(0);
  Result := 'loadBase64Code("' + LEncoder.Encode(ACode) + '", "' + AType + '"';
  if Wrapped then
    Result := Result + ', true'
  else
    Result := Result + ', false';
  Result := Result + ');';
end;

procedure TWorkBenchForm.Button1Click(Sender: TObject);
begin
  ActiveControl := WebBrowser1;
end;

procedure TWorkBenchForm.FormShow(Sender: TObject);
begin
  WebBrowser1.URL := TPath.GetFullPath('highlight/index.html');
end;

procedure TWorkBenchForm.btRightClick(Sender: TObject);
begin
  try
    mBson.Text := TConverters.Json2BsonString(mJson.Text);
  except
    on E: EJsonException do
      mBson.Text := E.Message;
  end;
end;

procedure TWorkBenchForm.btMinifyClick(Sender: TObject);
begin
  try
    LoadCode(TConverters.JsonReformat(mInput.Text, False), 'javascript', True);
  except
    on E: EJsonException do
      LoadError(E.Message);
  end;
end;

procedure TWorkBenchForm.btToDelphiClick(Sender: TObject);
begin
  try
    LoadCode(TConverters.Json2DelphiCode(mInput.Text), 'javascript');
  except
    on E: EJsonException do
      LoadError(E.Message);
  end;
end;

procedure TWorkBenchForm.btToWriterClick(Sender: TObject);
var
  LCode: string;
begin
  try
    if lbUseBuilders.IsChecked then
    begin
      LCode := TConverters.Json2JsonBuilderCode(mInput.Text, 'Builder');
    end
    else
    begin
      LCode := TConverters.Json2JsonWriterCode(mInput.Text, 'Writer');
    end;
    LoadCode(LCode, 'pascal');
  except
    on E: EJsonException do
      LoadError(E.Message);
  end;
end;

procedure TWorkBenchForm.btLeftClick(Sender: TObject);
begin
  try
    mJson.Text := TConverters.BsonString2Json(mBson.Text, [Indented]);
  except
    on E: EJsonException do
      mJson.Text := E.Message;
  end;
end;

procedure TWorkBenchForm.LoadCode(const ACode, AType: string; Wrapped: Boolean);
begin
  WebBrowser1.EvaluateJavaScript(BuildCommand(ACode, AType, Wrapped));
end;

procedure TWorkBenchForm.LoadError(const Msg: string);
begin
  WebBrowser1.EvaluateJavaScript(BuildCommand(Msg, 'error', True));
end;

end.
