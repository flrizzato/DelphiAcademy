object dmMain: TdmMain
  OldCreateOrder = False
  Height = 252
  Width = 320
  object Connection: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      'Database=C:\Delphi Cookbook\BOOK\Chapter05\DATA\SAMPLES.IB'
      'DriverID=iB')
    ConnectedStoredUsage = [auDesignTime]
    Connected = True
    LoginPrompt = False
    Left = 200
    Top = 40
  end
  object qryPeople: TFDQuery
    Connection = Connection
    UpdateOptions.AssignedValues = [uvFetchGeneratorsPoint, uvGeneratorName]
    UpdateOptions.FetchGeneratorsPoint = gpImmediate
    UpdateOptions.GeneratorName = 'GEN_PEOPLE_ID'
    UpdateOptions.KeyFields = 'ID'
    UpdateOptions.AutoIncFields = 'ID'
    SQL.Strings = (
      'SELECT * FROM PEOPLE')
    Left = 200
    Top = 104
    object qryPeopleID: TIntegerField
      AutoGenerateValue = arAutoInc
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryPeopleFIRST_NAME: TStringField
      FieldName = 'FIRST_NAME'
      Origin = 'FIRST_NAME'
      Required = True
      Size = 50
    end
    object qryPeopleLAST_NAME: TStringField
      FieldName = 'LAST_NAME'
      Origin = 'LAST_NAME'
      Required = True
      Size = 50
    end
    object qryPeopleWORK_PHONE_NUMBER: TStringField
      FieldName = 'WORK_PHONE_NUMBER'
      Origin = 'WORK_PHONE_NUMBER'
      Size = 30
    end
    object qryPeopleMOBILE_PHONE_NUMBER: TStringField
      FieldName = 'MOBILE_PHONE_NUMBER'
      Origin = 'MOBILE_PHONE_NUMBER'
      Size = 30
    end
    object qryPeopleEMAIL: TStringField
      FieldName = 'EMAIL'
      Origin = 'EMAIL'
      Size = 60
    end
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 200
    Top = 176
  end
end
