inherited RoomDM: TRoomDM
  OldCreateOrder = True
  Height = 299
  Width = 378
  inherited sqlControl: TFDQuery
    Connection = MainDM.SQLConnection
    SQL.Strings = (
      'select CAPACITY, LOCATION, NAME, ROOM_ID from ROOM')
    object sqlControlROOM_ID: TIntegerField
      DefaultExpression = '0'
      FieldName = 'ROOM_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object sqlControlCAPACITY: TIntegerField
      FieldName = 'CAPACITY'
    end
    object sqlControlLOCATION: TStringField
      FieldName = 'LOCATION'
      Size = 50
    end
    object sqlControlNAME: TStringField
      FieldName = 'NAME'
      Size = 30
    end
  end
  inherited cdsControl: TClientDataSet
    BeforePost = cdsControlBeforePost
    object cdsControlROOM_ID: TIntegerField
      DefaultExpression = '0'
      DisplayLabel = 'Room ID'
      FieldName = 'ROOM_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object cdsControlCAPACITY: TIntegerField
      DisplayLabel = 'Capacity'
      FieldName = 'CAPACITY'
    end
    object cdsControlLOCATION: TStringField
      DisplayLabel = 'Location'
      FieldName = 'LOCATION'
      Size = 50
    end
    object cdsControlNAME: TStringField
      DisplayLabel = 'Name'
      FieldName = 'NAME'
      Required = True
      Size = 30
    end
  end
end
