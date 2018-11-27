object RIOResource1: TRIOResource1
  OldCreateOrder = False
  Height = 420
  Width = 518
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 58
    Top = 26
  end
  object SalesTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM SALES')
    Left = 58
    Top = 74
  end
  object EMSDataSetResource1: TEMSDataSetResource
    AllowedActions = [List, Get]
    DataSet = SalesTable
    PageSize = 5
    Left = 56
    Top = 136
  end
end
