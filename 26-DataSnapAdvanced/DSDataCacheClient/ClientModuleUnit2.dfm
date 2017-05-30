object ClientModule2: TClientModule2
  OldCreateOrder = False
  Height = 271
  Width = 415
  object DSRestConnection1: TDSRestConnection
    Port = 8080
    LoginPrompt = False
    Left = 48
    Top = 40
    UniqueId = '{11126DB3-3CCB-4E61-BBC8-24A48FB1AD54}'
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 168
    Top = 40
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 296
    Top = 40
  end
  object FDDepartmentTable: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 48
    Top = 104
  end
end
