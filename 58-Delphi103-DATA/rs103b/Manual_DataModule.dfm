object ManualResource1: TManualResource1
  OldCreateOrder = False
  Height = 229
  Width = 332
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    Connected = True
    LoginPrompt = False
    Left = 69
    Top = 57
  end
  object FDQuery1: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'select * from employee')
    Left = 128
    Top = 112
  end
end
