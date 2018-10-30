object wmMain: TwmMain
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = wmMainDefaultHandlerAction
    end
    item
      MethodType = mtPost
      Name = 'waGetPeople'
      PathInfo = '/getpeople'
      OnAction = wmMainwaGetPeopleAction
    end
    item
      MethodType = mtPost
      Name = 'waSavePerson'
      PathInfo = '/saveperson'
      OnAction = wmMainwaSavePersonAction
    end
    item
      MethodType = mtPost
      Name = 'waDeletePerson'
      PathInfo = '/deleteperson'
      OnAction = wmMainwaDeletePersonAction
    end>
  Height = 262
  Width = 257
  object WebFileDispatcher1: TWebFileDispatcher
    WebFileExtensions = <
      item
        MimeType = 'text/css'
        Extensions = 'css'
      end
      item
        MimeType = 'text/html'
        Extensions = 'html;htm'
      end
      item
        MimeType = 'text/javascript'
        Extensions = 'js'
      end
      item
        MimeType = 'image/jpeg'
        Extensions = 'jpeg;jpg'
      end
      item
        MimeType = 'image/x-png'
        Extensions = 'png'
      end>
    WebDirectories = <
      item
        DirectoryAction = dirInclude
        DirectoryMask = '*'
      end
      item
        DirectoryAction = dirExclude
        DirectoryMask = '\templates\*'
      end>
    RootDirectory = 'www'
    VirtualPath = '/'
    Left = 48
    Top = 32
  end
  object Connection: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      
        'Database=C:\Users\IEUser\Documents\Embarcadero\Studio\Projects\W' +
        'ebBroker\SAMPLES.IB'
      'DriverID=IB')
    ConnectedStoredUsage = [auDesignTime]
    LoginPrompt = False
    BeforeConnect = ConnectionBeforeConnect
    Left = 48
    Top = 96
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 48
    Top = 152
  end
  object qryPeople: TFDQuery
    Connection = Connection
    UpdateOptions.AssignedValues = [uvFetchGeneratorsPoint, uvGeneratorName]
    UpdateOptions.FetchGeneratorsPoint = gpNone
    UpdateOptions.AutoIncFields = 'ID'
    SQL.Strings = (
      'SELECT * FROM PEOPLE')
    Left = 144
    Top = 152
    object qryPeopleID: TFDAutoIncField
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      IdentityInsert = True
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
  object cmdUpdatePerson: TFDCommand
    Connection = Connection
    CommandText.Strings = (
      'UPDATE PEOPLE'
      'SET FIRST_NAME = :FIRST_NAME,'
      '    LAST_NAME = :LAST_NAME,'
      '    WORK_PHONE_NUMBER = :WORK_PHONE_NUMBER,'
      '    MOBILE_PHONE_NUMBER = :MOBILE_PHONE_NUMBER,'
      '    EMAIL = :EMAIL'
      'WHERE (ID = :ID)')
    ParamData = <
      item
        Name = 'FIRST_NAME'
        ParamType = ptInput
      end
      item
        Name = 'LAST_NAME'
        ParamType = ptInput
      end
      item
        Name = 'WORK_PHONE_NUMBER'
        ParamType = ptInput
      end
      item
        Name = 'MOBILE_PHONE_NUMBER'
        ParamType = ptInput
      end
      item
        Name = 'EMAIL'
        ParamType = ptInput
      end
      item
        Name = 'ID'
        ParamType = ptInput
      end>
    Left = 144
    Top = 88
  end
  object cmdInsertPerson: TFDCommand
    Connection = Connection
    UpdateOptions.AssignedValues = [uvFetchGeneratorsPoint, uvGeneratorName]
    UpdateOptions.FetchGeneratorsPoint = gpImmediate
    UpdateOptions.GeneratorName = 'GEN_PEOPLE_ID'
    UpdateOptions.KeyFields = 'ID'
    UpdateOptions.AutoIncFields = 'ID'
    CommandKind = skInsert
    CommandText.Strings = (
      
        'INSERT INTO PEOPLE (ID, FIRST_NAME, LAST_NAME, WORK_PHONE_NUMBER' +
        ', MOBILE_PHONE_NUMBER, EMAIL) VALUES (:ID, :FIRST_NAME, :LAST_NA' +
        'ME, :WORK_PHONE_NUMBER, :MOBILE_PHONE_NUMBER, :EMAIL)')
    ParamData = <
      item
        Name = 'ID'
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'FIRST_NAME'
        DataType = ftString
        ParamType = ptInput
        Size = 50
      end
      item
        Name = 'LAST_NAME'
        DataType = ftString
        ParamType = ptInput
        Size = 50
      end
      item
        Name = 'WORK_PHONE_NUMBER'
        DataType = ftString
        ParamType = ptInput
        Size = 30
      end
      item
        Name = 'MOBILE_PHONE_NUMBER'
        DataType = ftString
        ParamType = ptInput
        Size = 30
      end
      item
        Name = 'EMAIL'
        DataType = ftString
        ParamType = ptInput
        Size = 60
      end>
    Left = 144
    Top = 32
  end
end
