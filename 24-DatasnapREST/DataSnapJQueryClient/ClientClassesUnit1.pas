// 
// Created by the DataSnap proxy generator.
// 4/17/2017 3:10:02 PM
// 

unit ClientClassesUnit1;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.DBXJSONReflect;

type
  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FSumCommand: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function Sum(A: Double; B: Double; const ARequestFilter: string = ''): Double;
  end;

const
  TServerMethods1_Sum: array [0..2] of TDSRestParameterMetaData =
  (
    (Name: 'A'; Direction: 1; DBXType: 7; TypeName: 'Double'),
    (Name: 'B'; Direction: 1; DBXType: 7; TypeName: 'Double'),
    (Name: ''; Direction: 4; DBXType: 7; TypeName: 'Double')
  );

implementation

function TServerMethods1Client.Sum(A: Double; B: Double; const ARequestFilter: string): Double;
begin
  if FSumCommand = nil then
  begin
    FSumCommand := FConnection.CreateCommand;
    FSumCommand.RequestType := 'GET';
    FSumCommand.Text := 'TServerMethods1.Sum';
    FSumCommand.Prepare(TServerMethods1_Sum);
  end;
  FSumCommand.Parameters[0].Value.SetDouble(A);
  FSumCommand.Parameters[1].Value.SetDouble(B);
  FSumCommand.Execute(ARequestFilter);
  Result := FSumCommand.Parameters[2].Value.GetDouble;
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection);
begin
  inherited Create(ARestConnection);
end;

constructor TServerMethods1Client.Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ARestConnection, AInstanceOwner);
end;

destructor TServerMethods1Client.Destroy;
begin
  FSumCommand.DisposeOf;
  inherited;
end;

end.
