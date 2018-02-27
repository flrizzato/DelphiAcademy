unit uJSONHelper;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.BatchMove, Data.DB,
  FireDAC.Comp.BatchMove.DataSet, FireDAC.Stan.Intf, FireDAC.Comp.BatchMove.JSON;

type
  TJSONDM = class(TDataModule)
    FDBatchMoveJSONWriter1: TFDBatchMoveJSONWriter;
    FDBatchMoveDataSetReader1: TFDBatchMoveDataSetReader;
    FDBatchMove1: TFDBatchMove;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TDataSetJSONHelper = class helper for TDataSet
  public
    function AsJSONArray: string;
    function AsJSONStream: TStream;
  end;


implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TDataSetJSONHelper }

function TDataSetJSONHelper.AsJSONArray: string;
var
  lDM: TJSONDM;
  lSTR: TStringStream;
begin
  lDM := TJSONDM.Create(nil);
  lSTR := TStringStream.Create;
  try
    Self.DisableControls;
    if not Self.Active then
      Self.Active := True;
    lDM.FDBatchMoveDataSetReader1.DataSet := Self;
    lDM.FDBatchMoveJSONWriter1.Stream := lSTR;
    lDM.FDBatchMove1.Execute;
    Result := lSTR.DataString;
  finally
    lDM.Free;
    lSTR.Free;
    Self.EnableControls;
  end;
end;

function TDataSetJSONHelper.AsJSONStream: TStream;
var
  lDM: TJSONDM;
begin
  lDM := TJSONDM.Create(nil);
  Result := TStringStream.Create;
  try
    Self.DisableControls;
    if not Self.Active then
      Self.Active := True;
    lDM.FDBatchMoveDataSetReader1.DataSet := Self;
    lDM.FDBatchMoveJSONWriter1.Stream := Result;
    lDM.FDBatchMove1.Execute;
  finally
    lDM.Free;
    Self.EnableControls;
  end;
end;

end.
