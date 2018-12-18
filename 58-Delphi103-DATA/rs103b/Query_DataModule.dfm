object TCustomerResource1: TTCustomerResource1
  OldCreateOrder = False
  Height = 275
  Width = 387
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    Connected = True
    LoginPrompt = False
    Left = 254
    Top = 35
  end
  object CustomerTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM CUSTOMERS'
      'ORDER BY {IIF(!SORT, !SORT, CustomerID)}')
    Left = 254
    Top = 83
    MacroData = <
      item
        Value = Null
        Name = 'SORT'
      end>
  end
  object CustomerResource: TEMSDataSetResource
    AllowedActions = [List, Get, Post, Put, Delete]
    DataSet = CustomerTable
    PageSize = 10
    Left = 152
    Top = 152
  end
  object FDQuery1: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM CUSTOMERS')
    Left = 262
    Top = 155
  end
end
