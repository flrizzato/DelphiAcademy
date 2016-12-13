//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit ClientClassesUnit2;

interface

uses Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, System.JSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.FireDACJSONReflect, Data.DBXJSONReflect;

type

  IDSRestCachedTFDJSONDataSets = interface;

  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FEchoStringCommand: TDSRestCommand;
    FReverseStringCommand: TDSRestCommand;
    FGetDepartmentNamesCommand: TDSRestCommand;
    FGetDepartmentNamesCommand_Cache: TDSRestCommand;
    FGetDepartmentNamesJSONCommand: TDSRestCommand;
    FGetDepartmentNamesJSONCommand_Cache: TDSRestCommand;
    FGetDepartmentEmployeesCommand: TDSRestCommand;
    FGetDepartmentEmployeesCommand_Cache: TDSRestCommand;
    FGetDepartmentEmployeesJSONCommand: TDSRestCommand;
    FGetDepartmentEmployeesJSONCommand_Cache: TDSRestCommand;
    FApplyChangesDepartmentEmployeesCommand: TDSRestCommand;
    FApplyChangesDepartmentEmployeesJSONCommand: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function EchoString(Value: string; const ARequestFilter: string = ''): string;
    function ReverseString(Value: string; const ARequestFilter: string = ''): string;
    function GetDepartmentNames(const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetDepartmentNames_Cache(const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
    function GetDepartmentNamesJSON(const ARequestFilter: string = ''): TJSONObject;
    function GetDepartmentNamesJSON_Cache(const ARequestFilter: string = ''): IDSRestCachedJSONObject;
    function GetDepartmentEmployees(AID: string; const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetDepartmentEmployees_Cache(AID: string; const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
    function GetDepartmentEmployeesJSON(AID: string; const ARequestFilter: string = ''): TJSONObject;
    function GetDepartmentEmployeesJSON_Cache(AID: string; const ARequestFilter: string = ''): IDSRestCachedJSONObject;
    procedure ApplyChangesDepartmentEmployees(ADeltaList: TFDJSONDeltas);
    procedure ApplyChangesDepartmentEmployeesJSON(AJSONObject: TJSONObject);
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

  TServerMethods1_GetDepartmentNames: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TServerMethods1_GetDepartmentNames_Cache: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_GetDepartmentNamesJSON: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TJSONObject')
  );

  TServerMethods1_GetDepartmentNamesJSON_Cache: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_GetDepartmentEmployees: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TServerMethods1_GetDepartmentEmployees_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_GetDepartmentEmployeesJSON: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TJSONObject')
  );

  TServerMethods1_GetDepartmentEmployeesJSON_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_ApplyChangesDepartmentEmployees: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: 'ADeltaList'; Direction: 1; DBXType: 37; TypeName: 'TFDJSONDeltas')
  );

  TServerMethods1_ApplyChangesDepartmentEmployeesJSON: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: 'AJSONObject'; Direction: 1; DBXType: 37; TypeName: 'TJSONObject')
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

function TServerMethods1Client.GetDepartmentNames(const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetDepartmentNamesCommand = nil then
  begin
    FGetDepartmentNamesCommand := FConnection.CreateCommand;
    FGetDepartmentNamesCommand.RequestType := 'GET';
    FGetDepartmentNamesCommand.Text := 'TServerMethods1.GetDepartmentNames';
    FGetDepartmentNamesCommand.Prepare(TServerMethods1_GetDepartmentNames);
  end;
  FGetDepartmentNamesCommand.Execute(ARequestFilter);
  if not FGetDepartmentNamesCommand.Parameters[0].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetDepartmentNamesCommand.Parameters[0].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetDepartmentNamesCommand.Parameters[0].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetDepartmentNamesCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TServerMethods1Client.GetDepartmentNames_Cache(const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetDepartmentNamesCommand_Cache = nil then
  begin
    FGetDepartmentNamesCommand_Cache := FConnection.CreateCommand;
    FGetDepartmentNamesCommand_Cache.RequestType := 'GET';
    FGetDepartmentNamesCommand_Cache.Text := 'TServerMethods1.GetDepartmentNames';
    FGetDepartmentNamesCommand_Cache.Prepare(TServerMethods1_GetDepartmentNames_Cache);
  end;
  FGetDepartmentNamesCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetDepartmentNamesCommand_Cache.Parameters[0].Value.GetString);
end;

function TServerMethods1Client.GetDepartmentNamesJSON(const ARequestFilter: string): TJSONObject;
begin
  if FGetDepartmentNamesJSONCommand = nil then
  begin
    FGetDepartmentNamesJSONCommand := FConnection.CreateCommand;
    FGetDepartmentNamesJSONCommand.RequestType := 'GET';
    FGetDepartmentNamesJSONCommand.Text := 'TServerMethods1.GetDepartmentNamesJSON';
    FGetDepartmentNamesJSONCommand.Prepare(TServerMethods1_GetDepartmentNamesJSON);
  end;
  FGetDepartmentNamesJSONCommand.Execute(ARequestFilter);
  Result := TJSONObject(FGetDepartmentNamesJSONCommand.Parameters[0].Value.GetJSONValue(FInstanceOwner));
end;

function TServerMethods1Client.GetDepartmentNamesJSON_Cache(const ARequestFilter: string): IDSRestCachedJSONObject;
begin
  if FGetDepartmentNamesJSONCommand_Cache = nil then
  begin
    FGetDepartmentNamesJSONCommand_Cache := FConnection.CreateCommand;
    FGetDepartmentNamesJSONCommand_Cache.RequestType := 'GET';
    FGetDepartmentNamesJSONCommand_Cache.Text := 'TServerMethods1.GetDepartmentNamesJSON';
    FGetDepartmentNamesJSONCommand_Cache.Prepare(TServerMethods1_GetDepartmentNamesJSON_Cache);
  end;
  FGetDepartmentNamesJSONCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedJSONObject.Create(FGetDepartmentNamesJSONCommand_Cache.Parameters[0].Value.GetString);
end;

function TServerMethods1Client.GetDepartmentEmployees(AID: string; const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetDepartmentEmployeesCommand = nil then
  begin
    FGetDepartmentEmployeesCommand := FConnection.CreateCommand;
    FGetDepartmentEmployeesCommand.RequestType := 'GET';
    FGetDepartmentEmployeesCommand.Text := 'TServerMethods1.GetDepartmentEmployees';
    FGetDepartmentEmployeesCommand.Prepare(TServerMethods1_GetDepartmentEmployees);
  end;
  FGetDepartmentEmployeesCommand.Parameters[0].Value.SetWideString(AID);
  FGetDepartmentEmployeesCommand.Execute(ARequestFilter);
  if not FGetDepartmentEmployeesCommand.Parameters[1].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetDepartmentEmployeesCommand.Parameters[1].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetDepartmentEmployeesCommand.Parameters[1].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetDepartmentEmployeesCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TServerMethods1Client.GetDepartmentEmployees_Cache(AID: string; const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetDepartmentEmployeesCommand_Cache = nil then
  begin
    FGetDepartmentEmployeesCommand_Cache := FConnection.CreateCommand;
    FGetDepartmentEmployeesCommand_Cache.RequestType := 'GET';
    FGetDepartmentEmployeesCommand_Cache.Text := 'TServerMethods1.GetDepartmentEmployees';
    FGetDepartmentEmployeesCommand_Cache.Prepare(TServerMethods1_GetDepartmentEmployees_Cache);
  end;
  FGetDepartmentEmployeesCommand_Cache.Parameters[0].Value.SetWideString(AID);
  FGetDepartmentEmployeesCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetDepartmentEmployeesCommand_Cache.Parameters[1].Value.GetString);
end;

function TServerMethods1Client.GetDepartmentEmployeesJSON(AID: string; const ARequestFilter: string): TJSONObject;
begin
  if FGetDepartmentEmployeesJSONCommand = nil then
  begin
    FGetDepartmentEmployeesJSONCommand := FConnection.CreateCommand;
    FGetDepartmentEmployeesJSONCommand.RequestType := 'GET';
    FGetDepartmentEmployeesJSONCommand.Text := 'TServerMethods1.GetDepartmentEmployeesJSON';
    FGetDepartmentEmployeesJSONCommand.Prepare(TServerMethods1_GetDepartmentEmployeesJSON);
  end;
  FGetDepartmentEmployeesJSONCommand.Parameters[0].Value.SetWideString(AID);
  FGetDepartmentEmployeesJSONCommand.Execute(ARequestFilter);
  Result := TJSONObject(FGetDepartmentEmployeesJSONCommand.Parameters[1].Value.GetJSONValue(FInstanceOwner));
end;

function TServerMethods1Client.GetDepartmentEmployeesJSON_Cache(AID: string; const ARequestFilter: string): IDSRestCachedJSONObject;
begin
  if FGetDepartmentEmployeesJSONCommand_Cache = nil then
  begin
    FGetDepartmentEmployeesJSONCommand_Cache := FConnection.CreateCommand;
    FGetDepartmentEmployeesJSONCommand_Cache.RequestType := 'GET';
    FGetDepartmentEmployeesJSONCommand_Cache.Text := 'TServerMethods1.GetDepartmentEmployeesJSON';
    FGetDepartmentEmployeesJSONCommand_Cache.Prepare(TServerMethods1_GetDepartmentEmployeesJSON_Cache);
  end;
  FGetDepartmentEmployeesJSONCommand_Cache.Parameters[0].Value.SetWideString(AID);
  FGetDepartmentEmployeesJSONCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedJSONObject.Create(FGetDepartmentEmployeesJSONCommand_Cache.Parameters[1].Value.GetString);
end;

procedure TServerMethods1Client.ApplyChangesDepartmentEmployees(ADeltaList: TFDJSONDeltas);
begin
  if FApplyChangesDepartmentEmployeesCommand = nil then
  begin
    FApplyChangesDepartmentEmployeesCommand := FConnection.CreateCommand;
    FApplyChangesDepartmentEmployeesCommand.RequestType := 'POST';
    FApplyChangesDepartmentEmployeesCommand.Text := 'TServerMethods1."ApplyChangesDepartmentEmployees"';
    FApplyChangesDepartmentEmployeesCommand.Prepare(TServerMethods1_ApplyChangesDepartmentEmployees);
  end;
  if not Assigned(ADeltaList) then
    FApplyChangesDepartmentEmployeesCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDSRestCommand(FApplyChangesDepartmentEmployeesCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FApplyChangesDepartmentEmployeesCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(ADeltaList), True);
      if FInstanceOwner then
        ADeltaList.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FApplyChangesDepartmentEmployeesCommand.Execute;
end;

procedure TServerMethods1Client.ApplyChangesDepartmentEmployeesJSON(AJSONObject: TJSONObject);
begin
  if FApplyChangesDepartmentEmployeesJSONCommand = nil then
  begin
    FApplyChangesDepartmentEmployeesJSONCommand := FConnection.CreateCommand;
    FApplyChangesDepartmentEmployeesJSONCommand.RequestType := 'POST';
    FApplyChangesDepartmentEmployeesJSONCommand.Text := 'TServerMethods1."ApplyChangesDepartmentEmployeesJSON"';
    FApplyChangesDepartmentEmployeesJSONCommand.Prepare(TServerMethods1_ApplyChangesDepartmentEmployeesJSON);
  end;
  FApplyChangesDepartmentEmployeesJSONCommand.Parameters[0].Value.SetJSONValue(AJSONObject, FInstanceOwner);
  FApplyChangesDepartmentEmployeesJSONCommand.Execute;
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
  FGetDepartmentNamesCommand.DisposeOf;
  FGetDepartmentNamesCommand_Cache.DisposeOf;
  FGetDepartmentNamesJSONCommand.DisposeOf;
  FGetDepartmentNamesJSONCommand_Cache.DisposeOf;
  FGetDepartmentEmployeesCommand.DisposeOf;
  FGetDepartmentEmployeesCommand_Cache.DisposeOf;
  FGetDepartmentEmployeesJSONCommand.DisposeOf;
  FGetDepartmentEmployeesJSONCommand_Cache.DisposeOf;
  FApplyChangesDepartmentEmployeesCommand.DisposeOf;
  FApplyChangesDepartmentEmployeesJSONCommand.DisposeOf;
  inherited;
end;

end.

