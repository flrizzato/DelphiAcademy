object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 253
  Width = 307
  object BackendAuth1: TBackendAuth
    Provider = EMSProvider
    LoginPrompt = False
    UserDetails = <>
    Authentication = Session
    Left = 135
    Top = 22
  end
  object EMSProvider: TEMSProvider
    ApiVersion = '1'
    URLPort = 0
    Left = 39
    Top = 22
  end
  object BackendUsers1: TBackendUsers
    Provider = EMSProvider
    Auth = BackendAuth1
    Left = 32
    Top = 184
  end
  object BackendGroups1: TBackendGroups
    Provider = EMSProvider
    Auth = BackendAuth1
    Left = 136
    Top = 184
  end
  object BackendEndpoint1: TBackendEndpoint
    Provider = EMSProvider
    Auth = BackendAuth1
    Params = <>
    Left = 136
    Top = 96
  end
  object BackendQuery1: TBackendQuery
    Provider = EMSProvider
    Auth = BackendAuth1
    Left = 40
    Top = 96
  end
  object BackendPush1: TBackendPush
    Provider = EMSProvider
    Auth = BackendAuth1
    Extras = <>
    Left = 232
    Top = 20
  end
end
