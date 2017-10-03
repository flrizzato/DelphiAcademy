//
// Created by the DataSnap proxy generator.
// 10/3/2017 8:55:38 AM
//

unit ClientClassesUnit1;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.FireDACJSONReflect, Data.DBXJSONReflect;

type

  IDSRestCachedTFDJSONDataSets = interface;

  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FDataModuleCreateCommand: TDSRestCommand;
    FGetPartsCommand: TDSRestCommand;
    FGetPartsCommand_Cache: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    procedure DataModuleCreate(Sender: TObject);
    function GetParts(fLastUpdate: string; const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetParts_Cache(fLastUpdate: string; const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
  end;

  IDSRestCachedTFDJSONDataSets = interface(IDSRestCachedObject<TFDJSONDataSets>)
  end;

  TDSRestCachedTFDJSONDataSets = class(TDSRestCachedObject<TFDJSONDataSets>, IDSRestCachedTFDJSONDataSets, IDSRestCachedCommand)
  end;

const
  TServerMethods1_DataModuleCreate: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: 'Sender'; Direction: 1; DBXType: 37; TypeName: 'TObject')
  );

  TServerMethods1_GetParts: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'fLastUpdate'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TServerMethods1_GetParts_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'fLastUpdate'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

implementation

procedure TServerMethods1Client.DataModuleCreate(Sender: TObject);
begin
  if FDataModuleCreateCommand = nil then
  begin
    FDataModuleCreateCommand := FConnection.CreateCommand;
    FDataModuleCreateCommand.RequestType := 'POST';
    FDataModuleCreateCommand.Text := 'TServerMethods1."DataModuleCreate"';
    FDataModuleCreateCommand.Prepare(TServerMethods1_DataModuleCreate);
  end;
  if not Assigned(Sender) then
    FDataModuleCreateCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDSRestCommand(FDataModuleCreateCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FDataModuleCreateCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(Sender), True);
      if FInstanceOwner then
        Sender.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FDataModuleCreateCommand.Execute;
end;

function TServerMethods1Client.GetParts(fLastUpdate: string; const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetPartsCommand = nil then
  begin
    FGetPartsCommand := FConnection.CreateCommand;
    FGetPartsCommand.RequestType := 'GET';
    FGetPartsCommand.Text := 'TServerMethods1.GetParts';
    FGetPartsCommand.Prepare(TServerMethods1_GetParts);
  end;
  FGetPartsCommand.Parameters[0].Value.SetWideString(fLastUpdate);
  FGetPartsCommand.Execute(ARequestFilter);
  if not FGetPartsCommand.Parameters[1].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetPartsCommand.Parameters[1].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetPartsCommand.Parameters[1].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetPartsCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TServerMethods1Client.GetParts_Cache(fLastUpdate: string; const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetPartsCommand_Cache = nil then
  begin
    FGetPartsCommand_Cache := FConnection.CreateCommand;
    FGetPartsCommand_Cache.RequestType := 'GET';
    FGetPartsCommand_Cache.Text := 'TServerMethods1.GetParts';
    FGetPartsCommand_Cache.Prepare(TServerMethods1_GetParts_Cache);
  end;
  FGetPartsCommand_Cache.Parameters[0].Value.SetWideString(fLastUpdate);
  FGetPartsCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetPartsCommand_Cache.Parameters[1].Value.GetString);
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
  FDataModuleCreateCommand.DisposeOf;
  FGetPartsCommand.DisposeOf;
  FGetPartsCommand_Cache.DisposeOf;
  inherited;
end;

end.

