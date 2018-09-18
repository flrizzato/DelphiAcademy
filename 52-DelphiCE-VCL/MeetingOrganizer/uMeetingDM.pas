unit uMeetingDM;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uAbstractDataModule, FMTBcd, DBClient, Provider, DB, DateUtils,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.DBX.Migrate;

type
  TMeetingDM = class(TAbstractDataModule)
    sqlControlMEETING_ID: TIntegerField;
    sqlControlTOPIC: TStringField;
    sqlControlDURATION: TIntegerField;
    sqlControlSTARTDATE: TDateField;
    sqlControlSTARTTIME: TTimeField;
    sqlControlUSER_ID: TIntegerField;
    sqlControlNAME_USER: TStringField;
    sqlControlROOM_ID: TIntegerField;
    sqlControlNAME_ROOM: TStringField;
    cdsControlMEETING_ID: TIntegerField;
    cdsControlTOPIC: TStringField;
    cdsControlDURATION: TIntegerField;
    cdsControlSTARTDATE: TDateField;
    cdsControlSTARTTIME: TTimeField;
    cdsControlUSER_ID: TIntegerField;
    cdsControlNAME_USER: TStringField;
    cdsControlROOM_ID: TIntegerField;
    cdsControlNAME_ROOM: TStringField;
    sqlParticipants: TFDQuery;
    cdsParticipants: TClientDataSet;
    cdsParticipantsUSER_ID: TIntegerField;
    cdsParticipantsNAME: TStringField;
    cdsParticipantsMEETING_ID: TIntegerField;
    sqlSearchPart: TFDQuery;
    cdsSearchPart: TClientDataSet;
    sqlSearchPartUSER_ID: TIntegerField;
    sqlSearchPartNAME: TStringField;
    cdsSearchPartUSER_ID: TIntegerField;
    cdsSearchPartNAME: TStringField;
    sqlParticipantsMEETING_ID: TIntegerField;
    sqlParticipantsUSER_ID: TIntegerField;
    sqlParticipantsUSER_NAME: TStringField;
    dspSearchPart: TDataSetProvider;
    dspParticipants: TDataSetProvider;
    sqlControlLASTCHANGE: TSQLTimeStampField;
    cdsControlLASTCHANGE: TSQLTimeStampField;
    procedure dspControlBeforeUpdateRecord(Sender: TObject; SourceDS: TDataSet;
      DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind;
      var Applied: Boolean);
    procedure cdsControlNewRecord(DataSet: TDataSet);
    procedure cdsControlBeforePost(DataSet: TDataSet);
    procedure cdsControlBeforeDelete(DataSet: TDataSet);
    procedure cdsControlBeforeEdit(DataSet: TDataSet);
    procedure cdsControlAfterScroll(DataSet: TDataSet);
    procedure cdsControlDURATIONChange(Sender: TField);
    procedure dspControlAfterUpdateRecord(Sender: TObject; SourceDS: TDataSet;
      DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  uMainDM, uUserControl, uMeetingControl, uMsgControl;

{$R *.dfm}

procedure TMeetingDM.cdsControlAfterScroll(DataSet: TDataSet);
begin
  inherited;
  cdsSearchPart.Close;
  cdsSearchPart.Params.ParamByName('MEETING_ID').AsInteger :=
    DataSet.FieldByName('MEETING_ID').AsInteger;
  cdsSearchPart.Open;

  cdsParticipants.Close;
  cdsParticipants.Params.ParamByName('MEETING_ID').AsInteger :=
    DataSet.FieldByName('MEETING_ID').AsInteger;
  cdsParticipants.Open;
end;

procedure TMeetingDM.cdsControlBeforeDelete(DataSet: TDataSet);
begin
  if not TMeetingControl.GetInstance.CheckMeetingOwner then
    raise Exception.Create
      ('This operation is granted to the meeting owner only!');
  inherited;
end;

procedure TMeetingDM.cdsControlBeforeEdit(DataSet: TDataSet);
begin
  if not TMeetingControl.GetInstance.CheckMeetingOwner then
    raise Exception.Create
      ('This operation is granted to the meeting owner only!');
  inherited;
end;

procedure TMeetingDM.cdsControlBeforePost(DataSet: TDataSet);
begin
  inherited;
  CheckRequiredFields(DataSet);

  if not TMeetingControl.GetInstance.CheckNumberOfParts then
    raise Exception.Create('At least two participants must be selected!');

  { Generating MeetingID }
  if DataSet.State = dsInsert then
  begin
    DataSet.FieldByName('MEETING_ID').AsInteger := GenerateID('GEN_MEETING_ID');
    cdsParticipants.First;
    While not cdsParticipants.Eof do
    begin
      cdsParticipants.Edit;
      cdsParticipants.FieldByName('MEETING_ID').AsInteger :=
        DataSet.FieldByName('MEETING_ID').AsInteger;
      cdsParticipants.Post;
      cdsParticipants.Next;
    end;
  end;

  { Logging date/time when meeting was changed }
  DataSet.FieldByName('LASTCHANGE').AsDateTime := Now;
end;

procedure TMeetingDM.cdsControlDURATIONChange(Sender: TField);
begin
  inherited;
  if cdsControl.State in [dsInsert, dsEdit] then
  begin
    cdsControl.FieldByName('STARTTIME').AsString := '';
    cdsControl.FieldByName('ROOM_ID').AsString := '';
    cdsControl.FieldByName('ROOM_NAME').AsString := '';
  end;
end;

procedure TMeetingDM.cdsControlNewRecord(DataSet: TDataSet);
begin
  inherited;
  { Meeting Owner }
  cdsControl.FieldByName('USER_ID').AsInteger :=
    TUserControl.GetInstance.fUserID;
  cdsControlSTARTDATE.AsDateTime := Date;
end;

procedure TMeetingDM.dspControlAfterUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind);
begin
  inherited;
  if UpdateKind = ukInsert then
    cdsParticipants.ApplyUpdates(0);
end;

procedure TMeetingDM.dspControlBeforeUpdateRecord(Sender: TObject;
  SourceDS: TDataSet; DeltaDS: TCustomClientDataSet; UpdateKind: TUpdateKind;
  var Applied: Boolean);
begin
  inherited;
  if SourceDS.Name = 'sqlControl' then
  begin
    { Validating meeting room }
    if not TMeetingControl.GetInstance.ValidateMeetingRoom
      (DeltaDS.FieldByName('MEETING_ID').AsInteger,
      DeltaDS.FieldByName('ROOM_ID').AsInteger, DeltaDS.FieldByName('STARTDATE')
      .AsDateTime, DeltaDS.FieldByName('STARTTIME').AsDateTime,
      DeltaDS.FieldByName('DURATION').AsInteger) then
      raise Exception.Create('There is no more availability for this room!');

    cdsParticipants.First;
    While not cdsParticipants.Eof do
    begin

      { Validating meeting participants }
      if (UpdateKind = ukInsert) or (UpdateKind = ukModify) then
        if not TMeetingControl.GetInstance.ValidateParticipants
          (DeltaDS.FieldByName('MEETING_ID').AsInteger,
          cdsParticipants.FieldByName('USER_ID').AsInteger,
          DeltaDS.FieldByName('STARTDATE').AsDateTime,
          DeltaDS.FieldByName('STARTTIME').AsDateTime,
          DeltaDS.FieldByName('DURATION').AsInteger) then
          raise Exception.Create('There is no more availability for user ' +
            cdsParticipants.FieldByName('USER_NAME').AsString + '!');

      { Sending meeting notification }
      TMeetingControl.GetInstance.SendMeetingNotification
        (cdsParticipants.FieldByName('USER_ID').AsInteger,
        DeltaDS.FieldByName('STARTDATE').AsDateTime,
        DeltaDS.FieldByName('STARTTIME').AsDateTime,
        DeltaDS.FieldByName('DURATION').AsInteger,
        DeltaDS.FieldByName('ROOM_NAME').AsString, DeltaDS.FieldByName('TOPIC')
        .AsString, Integer(UpdateKind));

      { If the meeting was canceled then remove participants }
      if UpdateKind = ukDelete then
        cdsParticipants.Delete
      else
        cdsParticipants.Next;
    end;

    if UpdateKind in [ukModify, ukDelete] then
      cdsParticipants.ApplyUpdates(0);
  end;
end;

end.
