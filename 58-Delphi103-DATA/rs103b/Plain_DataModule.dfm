object PlainResource1: TPlainResource1
  OldCreateOrder = False
  Height = 277
  Width = 475
  object FDQuery1: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'select * from employee')
    Left = 104
    Top = 112
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 231
    Top = 104
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 176
    Top = 166
  end
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 69
    Top = 57
  end
end
