object dtmdlDelta: TdtmdlDelta
  OldCreateOrder = False
  Height = 442
  Width = 433
  object FDTransactionDelta: TFDTransaction
    Options.Isolation = xiSnapshot
    Options.DisconnectAction = xdRollback
    Left = 136
    Top = 80
  end
  object qrySubscriptionActive: TFDQuery
    Transaction = FDTransactionDelta
    SQL.Strings = (
      'set subscription sub_medicineupdates at '#39'P1'#39' active;'
      '')
    Left = 120
    Top = 144
  end
  object qryCategory: TFDQuery
    Left = 40
    Top = 16
  end
  object qryMedicine: TFDQuery
    Left = 136
    Top = 16
  end
  object qryMedicineCategories: TFDQuery
    Left = 248
    Top = 16
  end
  object qryCategoryDelta: TFDQuery
    Transaction = FDTransactionDelta
    Left = 40
    Top = 216
  end
  object qryMedicineDelta: TFDQuery
    Transaction = FDTransactionDelta
    Left = 136
    Top = 216
  end
  object qryMedicineCategoriesDelta: TFDQuery
    Transaction = FDTransactionDelta
    Left = 248
    Top = 216
  end
  object mtCategoryMerged: TFDMemTable
    FilterChanges = [rtModified, rtInserted, rtDeleted, rtUnmodified]
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 40
    Top = 296
  end
  object mtMedicineMerged: TFDMemTable
    FilterChanges = [rtModified, rtInserted, rtDeleted, rtUnmodified]
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 152
    Top = 280
  end
  object mtMedicineCategoriesMerged: TFDMemTable
    FilterChanges = [rtModified, rtInserted, rtDeleted, rtUnmodified]
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired]
    UpdateOptions.CheckRequired = False
    Left = 248
    Top = 304
  end
end
