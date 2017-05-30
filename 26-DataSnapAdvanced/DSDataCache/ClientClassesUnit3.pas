// 
// Created by the DataSnap proxy generator.
// 5/30/2017 4:04:02 PM
// 

unit ClientClassesUnit3;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.FireDACJSONReflect, Data.DBXJSONReflect;

type

  IDSRestCachedTFDJSONDataSets = interface;

  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FEchoStringCommand: TDSRestCommand;
    FReverseStringCommand: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function EchoString(Value: string; const ARequestFilter: string = ''): string;
    function ReverseString(Value: string; const ARequestFilter: string = ''): string;
  end;

  TDataCacheDMClient = class(TDSAdminRestClient)
  private
    FDataModuleCreateCommand: TDSRestCommand;
    FDataCacheUpdateCommand: TDSRestCommand;
    FGetDepartmentTableCommand: TDSRestCommand;
    FGetDepartmentTableCommand_Cache: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataCacheUpdate;
    function GetDepartmentTable(const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetDepartmentTable_Cache(const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
  end;

  IDSRestCachedTFDJSONDataSets = interface(IDSRestCachedObject<TFDJSONDataSets>)
  end;

  TDSRestCachedTFDJSONDataSets = class(TDSRestCachedObject<TFDJSONDataSets>, IDSRestCachedTFDJSONDataSets, IDSRestCachedCommand)
  end;

const
  TServerMethods1_EchoString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TServerMethods1_ReverseString: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'Value'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'string')
  );

  TDataCacheDM_DataModuleCreate: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: 'Sender'; Direction: 1; DBXType: 37; TypeName: 'TObject')
  );

  TDataCacheDM_GetDepartmentTable: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TDataCacheDM_GetDepartmentTable_Cache: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

implementation

function TServerMethods1Client.EchoString(Value: string; const ARequestFilter: string): string;
begin
  if FEchoStringCommand = nil then
  begin
    FEchoStringCommand := FConnection.CreateCommand;
    FEchoStringCommand.RequestType := 'GET';
    FEchoStringCommand.Text := 'TServerMethods1.EchoString';
    FEchoStringCommand.Prepare(TServerMethods1_EchoString);
  end;
  FEchoStringCommand.Parameters[0].Value.SetWideString(Value);
  FEchoStringCommand.Execute(ARequestFilter);
  Result := FEchoStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.ReverseString(Value: string; const ARequestFilter: string): string;
begin
  if FReverseStringCommand = nil then
  begin
    FReverseStringCommand := FConnection.CreateCommand;
    FReverseStringCommand.RequestType := 'GET';
    FReverseStringCommand.Text := 'TServerMethods1.ReverseString';
    FReverseStringCommand.Prepare(TServerMethods1_ReverseString);
  end;
  FReverseStringCommand.Parameters[0].Value.SetWideString(Value);
  FReverseStringCommand.Execute(ARequestFilter);
  Result := FReverseStringCommand.Parameters[1].Value.GetWideString;
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
  FEchoStringCommand.DisposeOf;
  FReverseStringCommand.DisposeOf;
  inherited;
end;

procedure TDataCacheDMClient.DataModuleCreate(Sender: TObject);
begin
  if FDataModuleCreateCommand = nil then
  begin
    FDataModuleCreateCommand := FConnection.CreateCommand;
    FDataModuleCreateCommand.RequestType := 'POST';
    FDataModuleCreateCommand.Text := 'TDataCacheDM."DataModuleCreate"';
    FDataModuleCreateCommand.Prepare(TDataCacheDM_DataModuleCreate);
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

procedure TDataCacheDMClient.DataCacheUpdate;
begin
  if FDataCacheUpdateCommand = nil then
  begin
    FDataCacheUpdateCommand := FConnection.CreateCommand;
    FDataCacheUpdateCommand.RequestType := 'GET';
    FDataCacheUpdateCommand.Text := 'TDataCacheDM.DataCacheUpdate';
  end;
  FDataCacheUpdateCommand.Execute;
end;

function TDataCacheDMClient.GetDepartmentTable(const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetDepartmentTableCommand = nil then
  begin
    FGetDepartmentTableCommand := FConnection.CreateCommand;
    FGetDepartmentTableCommand.RequestType := 'GET';
    FGetDepartmentTableCommand.Text := 'TDataCacheDM.GetDepartmentTable';
    FGetDepartmentTableCommand.Prepare(TDataCacheDM_GetDepartmentTable);
  end;
  FGetDepartmentTableCommand.Execute(ARequestFilter);
  if not FGetDepartmentTableCommand.Parameters[0].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetDepartmentTableCommand.Parameters[0].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetDepartmentTableCommand.Parameters[0].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetDepartmentTableCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TDataCacheDMClient.GetDepartmentTable_Cache(const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetDepartmentTableCommand_Cache = nil then
  begin
    FGetDepartmentTableCommand_Cache := FConnection.CreateCommand;
    FGetDepartmentTableCommand_Cache.RequestType := 'GET';
    FGetDepartmentTableCommand_Cache.Text := 'TDataCacheDM.GetDepartmentTable';
    FGetDepartmentTableCommand_Cache.Prepare(TDataCacheDM_GetDepartmentTable_Cache);
  end;
  FGetDepartmentTableCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetDepartmentTableCommand_Cache.Parameters[0].Value.GetString);
end;

constructor TDataCacheDMClient.Create(ARestConnection: TDSRestConnection);
begin
  inherited Create(ARestConnection);
end;

constructor TDataCacheDMClient.Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ARestConnection, AInstanceOwner);
end;

destructor TDataCacheDMClient.Destroy;
begin
  FDataModuleCreateCommand.DisposeOf;
  FDataCacheUpdateCommand.DisposeOf;
  FGetDepartmentTableCommand.DisposeOf;
  FGetDepartmentTableCommand_Cache.DisposeOf;
  inherited;
end;

end.
