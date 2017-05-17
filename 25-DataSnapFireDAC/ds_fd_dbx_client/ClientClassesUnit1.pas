// 
// Created by the DataSnap proxy generator.
// 5/15/2017 12:17:14 PM
// 

unit ClientClassesUnit1;

interface

uses System.JSON, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.DBXJSONReflect;

type
  TServerMethods1Client = class(TDSAdminClient)
  private
    FEmployeeConnectionBeforeConnectCommand: TDBXCommand;
    FEchoStringCommand: TDBXCommand;
    FReverseStringCommand: TDBXCommand;
  public
    constructor Create(ADBXConnection: TDBXConnection); overload;
    constructor Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    procedure EmployeeConnectionBeforeConnect(Sender: TObject);
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
  end;

implementation

procedure TServerMethods1Client.EmployeeConnectionBeforeConnect(Sender: TObject);
begin
  if FEmployeeConnectionBeforeConnectCommand = nil then
  begin
    FEmployeeConnectionBeforeConnectCommand := FDBXConnection.CreateCommand;
    FEmployeeConnectionBeforeConnectCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FEmployeeConnectionBeforeConnectCommand.Text := 'TServerMethods1.EmployeeConnectionBeforeConnect';
    FEmployeeConnectionBeforeConnectCommand.Prepare;
  end;
  if not Assigned(Sender) then
    FEmployeeConnectionBeforeConnectCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDBXClientCommand(FEmployeeConnectionBeforeConnectCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FEmployeeConnectionBeforeConnectCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(Sender), True);
      if FInstanceOwner then
        Sender.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FEmployeeConnectionBeforeConnectCommand.ExecuteUpdate;
end;

function TServerMethods1Client.EchoString(Value: string): string;
begin
  if FEchoStringCommand = nil then
  begin
    FEchoStringCommand := FDBXConnection.CreateCommand;
    FEchoStringCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FEchoStringCommand.Text := 'TServerMethods1.EchoString';
    FEchoStringCommand.Prepare;
  end;
  FEchoStringCommand.Parameters[0].Value.SetWideString(Value);
  FEchoStringCommand.ExecuteUpdate;
  Result := FEchoStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.ReverseString(Value: string): string;
begin
  if FReverseStringCommand = nil then
  begin
    FReverseStringCommand := FDBXConnection.CreateCommand;
    FReverseStringCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FReverseStringCommand.Text := 'TServerMethods1.ReverseString';
    FReverseStringCommand.Prepare;
  end;
  FReverseStringCommand.Parameters[0].Value.SetWideString(Value);
  FReverseStringCommand.ExecuteUpdate;
  Result := FReverseStringCommand.Parameters[1].Value.GetWideString;
end;


constructor TServerMethods1Client.Create(ADBXConnection: TDBXConnection);
begin
  inherited Create(ADBXConnection);
end;


constructor TServerMethods1Client.Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ADBXConnection, AInstanceOwner);
end;


destructor TServerMethods1Client.Destroy;
begin
  FEmployeeConnectionBeforeConnectCommand.DisposeOf;
  FEchoStringCommand.DisposeOf;
  FReverseStringCommand.DisposeOf;
  inherited;
end;

end.
