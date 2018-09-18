unit uRoomControl;

interface

uses
  Classes, uAbstractControl, uRoomDM;

type
  TRoomControl = class(TAbstractControl)
  strict private
    fRoomDM: TRoomDM;
    class var FInstance: TRoomControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateDefaultForm; override;
    procedure CreateDefaultDM; override;
    class function GetInstance: TRoomControl;
    function FindRoomName(RoomID: integer; Name: string): boolean;
  end;

implementation

uses
  SysUtils, Forms, uRoomForm, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.DBX.Migrate;

{ TRoomControl }

constructor TRoomControl.Create(AOwner: TComponent);
begin
  fRoomDM := nil;
  fRoomForm := nil;
  inherited Create(AOwner);
end;

procedure TRoomControl.CreateDefaultForm;
begin
  inherited;
  if fRoomForm = nil then
  begin
    fRoomForm := TRoomForm.Create(Self);
    fRoomForm.datControl.DataSet := fRoomDM.cdsControl;
    fRoomForm.datControl.DataSet.Open;
  end
  else
    fRoomForm.Show;
end;

procedure TRoomControl.CreateDefaultDM;
begin
  inherited;
  fRoomDM := TRoomDM.Create(Self);
  fRoomDM.DBConnection := DBConnection;
end;

destructor TRoomControl.Destroy;
begin
  inherited;
end;

class function TRoomControl.GetInstance: TRoomControl;
begin
  if FInstance = nil then
  begin
    FInstance := uRoomControl.TRoomControl.Create(Application);
  end;
  Result := FInstance;
end;

function TRoomControl.FindRoomName(RoomID: integer; Name: string): boolean;
var
  sqlFind: TFDQuery;
begin
  sqlFind := TFDQuery.Create(nil);
  try
    sqlFind.Connection := DBConnection;

    sqlFind.SQL.Add('SELECT * FROM ROOM');
    sqlFind.SQL.Add(' WHERE NAME = ' + QuotedStr(Name));
    sqlFind.SQL.Add('   AND ROOM_ID <> ' + IntToStr(RoomID));
    sqlFind.Open;

    Result := (sqlFind.RecordCount > 0);
  finally
    sqlFind.Free;
  end;
end;

end.
