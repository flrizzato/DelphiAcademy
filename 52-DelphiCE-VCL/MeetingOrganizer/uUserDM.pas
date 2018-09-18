unit uUserDM;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uAbstractDataModule, FMTBcd, DBClient, Provider, DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TUserDM = class(TAbstractDataModule)
    sqlControlUSER_ID: TIntegerField;
    sqlControlNAME: TStringField;
    sqlControlEMAIL: TStringField;
    sqlControlPHONE: TStringField;
    sqlControlLOGIN: TStringField;
    sqlControlPASSW: TStringField;
    sqlControlISADMIN: TStringField;
    cdsControlUSER_ID: TIntegerField;
    cdsControlNAME: TStringField;
    cdsControlEMAIL: TStringField;
    cdsControlPHONE: TStringField;
    cdsControlLOGIN: TStringField;
    cdsControlPASSW: TStringField;
    cdsControlISADMIN: TStringField;
    procedure dspControlBeforeUpdateRecord(Sender: TObject; SourceDS: TDataSet;
      DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind;
      var Applied: Boolean);
    procedure cdsControlISADMINGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure cdsControlBeforePost(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  uMainDM, uUserControl, uMsgControl;

{$R *.dfm}

procedure TUserDM.cdsControlBeforePost(DataSet: TDataSet);
begin
  inherited;
  CheckRequiredFields(DataSet);

  if TUserControl.GetInstance.FindLoginName(DataSet.FieldByName('USER_ID')
    .AsInteger, DataSet.FieldByName('LOGIN').AsString) then
    raise Exception.Create('Duplicated login name!');

  if DataSet.State = dsInsert then
    DataSet.FieldByName('USER_ID').AsInteger := GenerateID('GEN_USERS_ID');
end;

procedure TUserDM.cdsControlISADMINGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  inherited;
  if Sender.Value = 'Y' then
    Text := 'YES'
  else
    Text := 'NO';
end;

procedure TUserDM.dspControlBeforeUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind;
  var Applied: Boolean);
var
  msg: string;
begin
  inherited;
  if UpdateKind = ukInsert then
  begin
    msg := 'You have a new account on the Meeting Organizer!' + #13#10 +
      'Login Name: ' + DeltaDS.FieldByName('LOGIN').AsString + #13#10 +
      'Password: ' + DeltaDS.FieldByName('PASSW').AsString;
    TMsgControl.GetInstance.PostNotification(DeltaDS.FieldByName('EMAIL')
      .AsString, msg);
  end;
end;

end.
