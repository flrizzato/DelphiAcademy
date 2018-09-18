unit uAbstractControl;

interface

uses
  Forms, SysUtils, Classes, Variants, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  FireDAC.DBX.Migrate;

const
  MO_ExceptionMsg = 'Meeting Organizer Error: ' + #13;

type
  TAbstractControl = class(TComponent)
  private
    fDBConnection: TFDConnection;
    procedure SetDBConnection(value: TFDConnection);
    function GetDBConnection: TFDConnection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateDefaultDM; virtual; abstract;
    procedure CreateDefaultForm; virtual; abstract;
    property DBConnection: TFDConnection read GetDBConnection
      write SetDBConnection;
    function DataFormat(Data: Variant; DataType: Char): string;
  end;

implementation

uses
  uMainForm;

{ TAbstractControl }

constructor TAbstractControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DBConnection := TMainForm(Application.MainForm).DBConnection;
  CreateDefaultDM;
end;

destructor TAbstractControl.Destroy;
begin
  inherited Destroy;
end;

function TAbstractControl.GetDBConnection: TFDConnection;
begin
  Result := fDBConnection;
end;

procedure TAbstractControl.SetDBConnection(value: TFDConnection);
begin
  fDBConnection := value;
end;

function TAbstractControl.DataFormat(Data: Variant; DataType: Char): string;
begin
  if VarIsEmpty(Data) then
  begin
    Result := '';
  end
  else
  begin
    if DataType = 'C' then
    begin
      Result := #39 + Data + #39;
    end
    else if DataType = 'D' then
    begin
      Result := 'CAST(' + #39 + FormatDateTime('mm/dd/yyyy', VarToDateTime(Data)
        ) + #39 + ' AS DATE)'
    end
    else if DataType = 'T' then
    begin
      Result := 'CAST(' + #39 + FormatDateTime('mm/dd/yyyy hh:nn:ss',
        VarToDateTime(Data)) + #39 + ' AS TIMESTAMP)'
    end
    else if DataType = 'N' then
    begin
      Result := StringReplace(Result, '.', '', [rfReplaceAll]);
      Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
    end;
  end;
end;

end.
