object MainDM: TMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 377
  Width = 535
  object RESTClient1: TRESTClient
    Authenticator = OAuth2Authenticator1
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'UTF-8, *;q=0.8'
    BaseURL = 'https://www.googleapis.com/calendar/v3/calendars/primary/events'
    Params = <>
    HandleRedirects = True
    RaiseExceptionOn500 = False
    Left = 88
    Top = 16
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    SynchronizedEvents = False
    Left = 88
    Top = 128
  end
  object RESTResponse1: TRESTResponse
    ContentType = 'application/json'
    RootElement = 'items'
    Left = 88
    Top = 184
  end
  object RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter
    Dataset = FDMemTable1
    FieldDefs = <>
    Response = RESTResponse1
    NestedElements = True
    Left = 88
    Top = 240
  end
  object FDMemTable1: TFDMemTable
    FieldDefs = <>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    StoreDefs = True
    Left = 88
    Top = 296
    object FDMemTable1kind: TWideStringField
      FieldName = 'kind'
      Size = 255
    end
    object FDMemTable1etag: TWideStringField
      FieldName = 'etag'
      Size = 255
    end
    object FDMemTable1id: TWideStringField
      FieldName = 'id'
      Size = 255
    end
    object FDMemTable1status: TWideStringField
      FieldName = 'status'
      Size = 255
    end
    object FDMemTable1htmlLink: TWideStringField
      FieldName = 'htmlLink'
      Size = 255
    end
    object FDMemTable1created: TWideStringField
      FieldName = 'created'
      Size = 255
    end
    object FDMemTable1updated: TWideStringField
      FieldName = 'updated'
      Size = 255
    end
    object FDMemTable1summary: TWideStringField
      FieldName = 'summary'
      Size = 255
    end
    object FDMemTable1location: TWideStringField
      FieldName = 'location'
      Size = 255
    end
    object FDMemTable1creator: TWideStringField
      FieldName = 'creator'
      Size = 255
    end
    object FDMemTable1creatoremail: TWideStringField
      FieldName = 'creator.email'
      Size = 255
    end
    object FDMemTable1creatordisplayName: TWideStringField
      FieldName = 'creator.displayName'
      Size = 255
    end
    object FDMemTable1organizer: TWideStringField
      FieldName = 'organizer'
      Size = 255
    end
    object FDMemTable1organizeremail: TWideStringField
      FieldName = 'organizer.email'
      Size = 255
    end
    object FDMemTable1organizerdisplayName: TWideStringField
      FieldName = 'organizer.displayName'
      Size = 255
    end
    object FDMemTable1start: TWideStringField
      FieldName = 'start'
      Size = 255
    end
    object FDMemTable1startdateTime: TWideStringField
      FieldName = 'start.dateTime'
      Size = 255
    end
    object FDMemTable1end: TWideStringField
      FieldName = 'end'
      Size = 255
    end
    object FDMemTable1enddateTime: TWideStringField
      FieldName = 'end.dateTime'
      Size = 255
    end
    object FDMemTable1iCalUID: TWideStringField
      FieldName = 'iCalUID'
      Size = 255
    end
    object FDMemTable1sequence: TWideStringField
      FieldName = 'sequence'
      Size = 255
    end
    object FDMemTable1attendees: TWideStringField
      FieldName = 'attendees'
      Size = 255
    end
    object FDMemTable1reminders: TWideStringField
      FieldName = 'reminders'
      Size = 255
    end
    object FDMemTable1remindersuseDefault: TWideStringField
      FieldName = 'reminders.useDefault'
      Size = 255
    end
  end
  object OAuth2Authenticator1: TOAuth2Authenticator
    AccessToken = 
      'ya29.CjHgAmQ92HKZHXkxUnuGismHNusnJM9hSr1VIfcr8PkjfluQc2LjQBwB3As' +
      'XUXvquKmP'
    AccessTokenEndpoint = 'https://accounts.google.com/o/oauth2/token'
    AuthCode = '4/uJ02M2albyyTWqJJwqSoFCvIea2wFRQePyZMghkhD_U'
    AuthorizationEndpoint = 'https://accounts.google.com/o/oauth2/auth'
    ClientID = 
      '639890170320-vondtcucpakk4rpaorsu2bb0i8e3m60j.apps.googleusercon' +
      'tent.com'
    ClientSecret = '3dNbmdBT0Xo2g1X0dDk5Lymw'
    RedirectionEndpoint = 'http://localhost:9876'
    Scope = 'https://www.googleapis.com/auth/calendar'
    Left = 88
    Top = 72
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 9876
    Left = 184
    Top = 16
  end
end
