inherited MeetingDM: TMeetingDM
  OldCreateOrder = True
  Height = 300
  Width = 415
  inherited sqlControl: TFDQuery
    Connection = MainDM.SQLConnection
    SQL.Strings = (
      'SELECT M.MEETING_ID,'
      '             M.TOPIC,'
      '             M.DURATION,'
      '             M.STARTDATE,'
      '             M.STARTTIME,'
      '             M.USER_ID,'
      '             U.NAME AS USER_NAME,'
      '             M.ROOM_ID,'
      '             R.NAME AS ROOM_NAME,'
      '             M.LASTCHANGE'
      '  FROM MEETING M'
      '  LEFT OUTER JOIN USERS U ON (M.USER_ID = U.USER_ID)'
      '  LEFT OUTER JOIN ROOM R ON (M.ROOM_ID = R.ROOM_ID)'
      ' WHERE M.STARTDATE >= '#39'TODAY'#39)
    Left = 32
    object sqlControlMEETING_ID: TIntegerField
      DefaultExpression = '0'
      FieldName = 'MEETING_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object sqlControlTOPIC: TStringField
      FieldName = 'TOPIC'
      Size = 100
    end
    object sqlControlDURATION: TIntegerField
      FieldName = 'DURATION'
      Required = True
    end
    object sqlControlSTARTDATE: TDateField
      FieldName = 'STARTDATE'
      Required = True
    end
    object sqlControlSTARTTIME: TTimeField
      FieldName = 'STARTTIME'
      Required = True
    end
    object sqlControlUSER_ID: TIntegerField
      FieldName = 'USER_ID'
      Required = True
    end
    object sqlControlNAME_USER: TStringField
      FieldName = 'USER_NAME'
      ProviderFlags = []
      Size = 40
    end
    object sqlControlROOM_ID: TIntegerField
      FieldName = 'ROOM_ID'
      Required = True
    end
    object sqlControlNAME_ROOM: TStringField
      FieldName = 'ROOM_NAME'
      ProviderFlags = []
      Size = 30
    end
    object sqlControlLASTCHANGE: TSQLTimeStampField
      FieldName = 'LASTCHANGE'
    end
  end
  inherited dspControl: TDataSetProvider
    AfterUpdateRecord = dspControlAfterUpdateRecord
    BeforeUpdateRecord = dspControlBeforeUpdateRecord
    Left = 32
  end
  inherited cdsControl: TClientDataSet
    IndexFieldNames = 'STARTDATE;STARTTIME'
    BeforeEdit = cdsControlBeforeEdit
    BeforePost = cdsControlBeforePost
    AfterScroll = cdsControlAfterScroll
    OnNewRecord = cdsControlNewRecord
    Left = 32
    object cdsControlMEETING_ID: TIntegerField
      DefaultExpression = '0'
      DisplayLabel = 'Meeting ID'
      FieldName = 'MEETING_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object cdsControlTOPIC: TStringField
      DisplayLabel = 'Topic'
      FieldName = 'TOPIC'
      Size = 100
    end
    object cdsControlDURATION: TIntegerField
      DisplayLabel = 'Duration'
      FieldName = 'DURATION'
      OnChange = cdsControlDURATIONChange
    end
    object cdsControlSTARTDATE: TDateField
      DisplayLabel = 'Date'
      FieldName = 'STARTDATE'
      OnChange = cdsControlDURATIONChange
      EditMask = '!99/99/0000;1;_'
    end
    object cdsControlSTARTTIME: TTimeField
      DisplayLabel = 'Start Time'
      FieldName = 'STARTTIME'
      EditMask = '!90:00:00;1;_'
    end
    object cdsControlUSER_ID: TIntegerField
      DisplayLabel = 'User ID'
      FieldName = 'USER_ID'
      Required = True
    end
    object cdsControlNAME_USER: TStringField
      DisplayLabel = 'User Name'
      FieldName = 'USER_NAME'
      ProviderFlags = []
      Size = 40
    end
    object cdsControlROOM_ID: TIntegerField
      DisplayLabel = 'Room ID'
      FieldName = 'ROOM_ID'
    end
    object cdsControlNAME_ROOM: TStringField
      DisplayLabel = 'Room Name'
      FieldName = 'ROOM_NAME'
      ProviderFlags = []
      Size = 30
    end
    object cdsControlLASTCHANGE: TSQLTimeStampField
      DisplayLabel = 'Last Change'
      FieldName = 'LASTCHANGE'
    end
  end
  object sqlParticipants: TFDQuery
    Connection = MainDM.SQLConnection
    SQL.Strings = (
      'SELECT PAR.MEETING_ID,'
      '       PAR.USER_ID,'
      '       USE.NAME AS USER_NAME'
      '  FROM PARTICIPANT PAR'
      '  LEFT OUTER JOIN USERS USE ON (PAR.USER_ID = USE.USER_ID)'
      '  WHERE PAR.MEETING_ID = :MEETING_ID')
    Left = 192
    Top = 16
    ParamData = <
      item
        Name = 'MEETING_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = '0'
      end>
    object sqlParticipantsMEETING_ID: TIntegerField
      FieldName = 'MEETING_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object sqlParticipantsUSER_ID: TIntegerField
      FieldName = 'USER_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object sqlParticipantsUSER_NAME: TStringField
      FieldName = 'USER_NAME'
      ProviderFlags = []
      Size = 40
    end
  end
  object cdsParticipants: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'USER_NAME'
    Params = <
      item
        DataType = ftInteger
        Name = 'MEETING_ID'
        ParamType = ptInput
        Value = '0'
      end>
    ProviderName = 'dspParticipants'
    Left = 192
    Top = 112
    object cdsParticipantsMEETING_ID: TIntegerField
      DisplayLabel = 'Meeting ID'
      FieldName = 'MEETING_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object cdsParticipantsUSER_ID: TIntegerField
      DisplayLabel = 'User ID'
      FieldName = 'USER_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object cdsParticipantsNAME: TStringField
      DisplayLabel = 'Selected Users'
      FieldName = 'USER_NAME'
      ProviderFlags = []
      Size = 40
    end
  end
  object sqlSearchPart: TFDQuery
    Connection = MainDM.SQLConnection
    SQL.Strings = (
      'SELECT U.USER_ID,'
      '             U.NAME AS USER_NAME'
      '  FROM USERS U'
      ' WHERE U.USER_ID NOT IN (SELECT P.USER_ID'
      '                           FROM PARTICIPANT P'
      '                          WHERE P.MEETING_ID = :MEETING_ID)')
    Left = 112
    Top = 16
    ParamData = <
      item
        Name = 'MEETING_ID'
        DataType = ftInteger
        ParamType = ptInput
        Value = '0'
      end>
    object sqlSearchPartUSER_ID: TIntegerField
      FieldName = 'USER_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object sqlSearchPartNAME: TStringField
      FieldName = 'USER_NAME'
      Size = 40
    end
  end
  object cdsSearchPart: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'USER_NAME'
    Params = <
      item
        DataType = ftInteger
        Name = 'MEETING_ID'
        ParamType = ptInput
        Value = '0'
      end>
    ProviderName = 'dspSearchPart'
    Left = 112
    Top = 112
    object cdsSearchPartUSER_ID: TIntegerField
      FieldName = 'USER_ID'
      Required = True
    end
    object cdsSearchPartNAME: TStringField
      DisplayLabel = 'Available Users'
      FieldName = 'USER_NAME'
      Size = 40
    end
  end
  object dspSearchPart: TDataSetProvider
    DataSet = sqlSearchPart
    ResolveToDataSet = True
    Options = [poIncFieldProps, poAllowMultiRecordUpdates, poAutoRefresh, poPropogateChanges, poAllowCommandText, poUseQuoteChar]
    UpdateMode = upWhereChanged
    Left = 112
    Top = 64
  end
  object dspParticipants: TDataSetProvider
    DataSet = sqlParticipants
    Options = [poIncFieldProps, poAllowMultiRecordUpdates, poAutoRefresh, poPropogateChanges, poAllowCommandText, poUseQuoteChar]
    UpdateMode = upWhereChanged
    Left = 192
    Top = 64
  end
end
