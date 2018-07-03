object AbstractDataModule: TAbstractDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 291
  Width = 409
  object sqlControl: TFDQuery
    Left = 48
    Top = 16
  end
  object dspControl: TDataSetProvider
    DataSet = sqlControl
    Options = [poIncFieldProps, poAllowMultiRecordUpdates, poAutoRefresh, poPropogateChanges, poAllowCommandText, poUseQuoteChar]
    UpdateMode = upWhereChanged
    Left = 48
    Top = 64
  end
  object cdsControl: TClientDataSet
    Aggregates = <>
    PacketRecords = 1
    Params = <>
    ProviderName = 'dspControl'
    AfterPost = cdsApplyUpdates
    AfterCancel = cdsCancelUpdates
    BeforeDelete = cdsControlBeforeDelete
    AfterDelete = cdsApplyUpdates
    BeforeRefresh = cdsControlBeforeRefresh
    Left = 48
    Top = 112
  end
end
