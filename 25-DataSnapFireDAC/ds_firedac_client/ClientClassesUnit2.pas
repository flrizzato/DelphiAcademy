// 
// Created by the DataSnap proxy generator.
// 5/15/2017 5:42:01 PM
// 

unit ClientClassesUnit2;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.FireDACJSONReflect, Data.DBXJSONReflect;

type

  IDSRestCachedTFDJSONDataSets = interface;

  TServerMethods1Client = class(TDSAdminRestClient)
  private
    FEmployeeConnectionBeforeConnectCommand: TDSRestCommand;
    FGetCustomersCommand: TDSRestCommand;
    FGetCustomersCommand_Cache: TDSRestCommand;
    FGetSalesCommand: TDSRestCommand;
    FGetSalesCommand_Cache: TDSRestCommand;
    FGetCustomersAndSalesCommand: TDSRestCommand;
    FGetCustomersAndSalesCommand_Cache: TDSRestCommand;
    FApplyCustomersAndSalesCommand: TDSRestCommand;
    FApplyCustomersAndSalesManualCommand: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    procedure EmployeeConnectionBeforeConnect(Sender: TObject);
    function GetCustomers(CustomerID: string; const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetCustomers_Cache(CustomerID: string; const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
    function GetSales(CustomerID: string; const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetSales_Cache(CustomerID: string; const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
    function GetCustomersAndSales(const ARequestFilter: string = ''): TFDJSONDataSets;
    function GetCustomersAndSales_Cache(const ARequestFilter: string = ''): IDSRestCachedTFDJSONDataSets;
    procedure ApplyCustomersAndSales(ADeltaList: TFDJSONDeltas);
    procedure ApplyCustomersAndSalesManual(ADeltaList: TFDJSONDeltas);
  end;

  IDSRestCachedTFDJSONDataSets = interface(IDSRestCachedObject<TFDJSONDataSets>)
  end;

  TDSRestCachedTFDJSONDataSets = class(TDSRestCachedObject<TFDJSONDataSets>, IDSRestCachedTFDJSONDataSets, IDSRestCachedCommand)
  end;

const
  TServerMethods1_EmployeeConnectionBeforeConnect: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: 'Sender'; Direction: 1; DBXType: 37; TypeName: 'TObject')
  );

  TServerMethods1_GetCustomers: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'CustomerID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TServerMethods1_GetCustomers_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'CustomerID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_GetSales: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'CustomerID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TServerMethods1_GetSales_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'CustomerID'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_GetCustomersAndSales: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TFDJSONDataSets')
  );

  TServerMethods1_GetCustomersAndSales_Cache: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TServerMethods1_ApplyCustomersAndSales: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: 'ADeltaList'; Direction: 1; DBXType: 37; TypeName: 'TFDJSONDeltas')
  );

  TServerMethods1_ApplyCustomersAndSalesManual: array [0..0] of TDSRestParameterMetaData =
  (
    (Name: 'ADeltaList'; Direction: 1; DBXType: 37; TypeName: 'TFDJSONDeltas')
  );

implementation

procedure TServerMethods1Client.EmployeeConnectionBeforeConnect(Sender: TObject);
begin
  if FEmployeeConnectionBeforeConnectCommand = nil then
  begin
    FEmployeeConnectionBeforeConnectCommand := FConnection.CreateCommand;
    FEmployeeConnectionBeforeConnectCommand.RequestType := 'POST';
    FEmployeeConnectionBeforeConnectCommand.Text := 'TServerMethods1."EmployeeConnectionBeforeConnect"';
    FEmployeeConnectionBeforeConnectCommand.Prepare(TServerMethods1_EmployeeConnectionBeforeConnect);
  end;
  if not Assigned(Sender) then
    FEmployeeConnectionBeforeConnectCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDSRestCommand(FEmployeeConnectionBeforeConnectCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FEmployeeConnectionBeforeConnectCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(Sender), True);
      if FInstanceOwner then
        Sender.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FEmployeeConnectionBeforeConnectCommand.Execute;
end;

function TServerMethods1Client.GetCustomers(CustomerID: string; const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetCustomersCommand = nil then
  begin
    FGetCustomersCommand := FConnection.CreateCommand;
    FGetCustomersCommand.RequestType := 'GET';
    FGetCustomersCommand.Text := 'TServerMethods1.GetCustomers';
    FGetCustomersCommand.Prepare(TServerMethods1_GetCustomers);
  end;
  FGetCustomersCommand.Parameters[0].Value.SetWideString(CustomerID);
  FGetCustomersCommand.Execute(ARequestFilter);
  if not FGetCustomersCommand.Parameters[1].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetCustomersCommand.Parameters[1].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetCustomersCommand.Parameters[1].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetCustomersCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TServerMethods1Client.GetCustomers_Cache(CustomerID: string; const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetCustomersCommand_Cache = nil then
  begin
    FGetCustomersCommand_Cache := FConnection.CreateCommand;
    FGetCustomersCommand_Cache.RequestType := 'GET';
    FGetCustomersCommand_Cache.Text := 'TServerMethods1.GetCustomers';
    FGetCustomersCommand_Cache.Prepare(TServerMethods1_GetCustomers_Cache);
  end;
  FGetCustomersCommand_Cache.Parameters[0].Value.SetWideString(CustomerID);
  FGetCustomersCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetCustomersCommand_Cache.Parameters[1].Value.GetString);
end;

function TServerMethods1Client.GetSales(CustomerID: string; const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetSalesCommand = nil then
  begin
    FGetSalesCommand := FConnection.CreateCommand;
    FGetSalesCommand.RequestType := 'GET';
    FGetSalesCommand.Text := 'TServerMethods1.GetSales';
    FGetSalesCommand.Prepare(TServerMethods1_GetSales);
  end;
  FGetSalesCommand.Parameters[0].Value.SetWideString(CustomerID);
  FGetSalesCommand.Execute(ARequestFilter);
  if not FGetSalesCommand.Parameters[1].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetSalesCommand.Parameters[1].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetSalesCommand.Parameters[1].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetSalesCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TServerMethods1Client.GetSales_Cache(CustomerID: string; const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetSalesCommand_Cache = nil then
  begin
    FGetSalesCommand_Cache := FConnection.CreateCommand;
    FGetSalesCommand_Cache.RequestType := 'GET';
    FGetSalesCommand_Cache.Text := 'TServerMethods1.GetSales';
    FGetSalesCommand_Cache.Prepare(TServerMethods1_GetSales_Cache);
  end;
  FGetSalesCommand_Cache.Parameters[0].Value.SetWideString(CustomerID);
  FGetSalesCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetSalesCommand_Cache.Parameters[1].Value.GetString);
end;

function TServerMethods1Client.GetCustomersAndSales(const ARequestFilter: string): TFDJSONDataSets;
begin
  if FGetCustomersAndSalesCommand = nil then
  begin
    FGetCustomersAndSalesCommand := FConnection.CreateCommand;
    FGetCustomersAndSalesCommand.RequestType := 'GET';
    FGetCustomersAndSalesCommand.Text := 'TServerMethods1.GetCustomersAndSales';
    FGetCustomersAndSalesCommand.Prepare(TServerMethods1_GetCustomersAndSales);
  end;
  FGetCustomersAndSalesCommand.Execute(ARequestFilter);
  if not FGetCustomersAndSalesCommand.Parameters[0].Value.IsNull then
  begin
    FUnMarshal := TDSRestCommand(FGetCustomersAndSalesCommand.Parameters[0].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TFDJSONDataSets(FUnMarshal.UnMarshal(FGetCustomersAndSalesCommand.Parameters[0].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetCustomersAndSalesCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TServerMethods1Client.GetCustomersAndSales_Cache(const ARequestFilter: string): IDSRestCachedTFDJSONDataSets;
begin
  if FGetCustomersAndSalesCommand_Cache = nil then
  begin
    FGetCustomersAndSalesCommand_Cache := FConnection.CreateCommand;
    FGetCustomersAndSalesCommand_Cache.RequestType := 'GET';
    FGetCustomersAndSalesCommand_Cache.Text := 'TServerMethods1.GetCustomersAndSales';
    FGetCustomersAndSalesCommand_Cache.Prepare(TServerMethods1_GetCustomersAndSales_Cache);
  end;
  FGetCustomersAndSalesCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedTFDJSONDataSets.Create(FGetCustomersAndSalesCommand_Cache.Parameters[0].Value.GetString);
end;

procedure TServerMethods1Client.ApplyCustomersAndSales(ADeltaList: TFDJSONDeltas);
begin
  if FApplyCustomersAndSalesCommand = nil then
  begin
    FApplyCustomersAndSalesCommand := FConnection.CreateCommand;
    FApplyCustomersAndSalesCommand.RequestType := 'POST';
    FApplyCustomersAndSalesCommand.Text := 'TServerMethods1."ApplyCustomersAndSales"';
    FApplyCustomersAndSalesCommand.Prepare(TServerMethods1_ApplyCustomersAndSales);
  end;
  if not Assigned(ADeltaList) then
    FApplyCustomersAndSalesCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDSRestCommand(FApplyCustomersAndSalesCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FApplyCustomersAndSalesCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(ADeltaList), True);
      if FInstanceOwner then
        ADeltaList.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FApplyCustomersAndSalesCommand.Execute;
end;

procedure TServerMethods1Client.ApplyCustomersAndSalesManual(ADeltaList: TFDJSONDeltas);
begin
  if FApplyCustomersAndSalesManualCommand = nil then
  begin
    FApplyCustomersAndSalesManualCommand := FConnection.CreateCommand;
    FApplyCustomersAndSalesManualCommand.RequestType := 'POST';
    FApplyCustomersAndSalesManualCommand.Text := 'TServerMethods1."ApplyCustomersAndSalesManual"';
    FApplyCustomersAndSalesManualCommand.Prepare(TServerMethods1_ApplyCustomersAndSalesManual);
  end;
  if not Assigned(ADeltaList) then
    FApplyCustomersAndSalesManualCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDSRestCommand(FApplyCustomersAndSalesManualCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FApplyCustomersAndSalesManualCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(ADeltaList), True);
      if FInstanceOwner then
        ADeltaList.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FApplyCustomersAndSalesManualCommand.Execute;
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
  FEmployeeConnectionBeforeConnectCommand.DisposeOf;
  FGetCustomersCommand.DisposeOf;
  FGetCustomersCommand_Cache.DisposeOf;
  FGetSalesCommand.DisposeOf;
  FGetSalesCommand_Cache.DisposeOf;
  FGetCustomersAndSalesCommand.DisposeOf;
  FGetCustomersAndSalesCommand_Cache.DisposeOf;
  FApplyCustomersAndSalesCommand.DisposeOf;
  FApplyCustomersAndSalesManualCommand.DisposeOf;
  inherited;
end;

end.
