object WebModule1: TWebModule1
  OldCreateOrder = False
  Actions = <
    item
      Name = 'waTime'
      PathInfo = '/time'
      OnAction = TimeAction
    end
    item
      Name = 'waDate'
      PathInfo = '/date'
      OnAction = DateAction
    end
    item
      Default = True
      Name = 'waMenu'
      PathInfo = '/menu'
      OnAction = MenuAction
    end
    item
      Name = 'waStatus'
      PathInfo = '/status'
      OnAction = StatusAction
    end
    item
      Name = 'waTable'
      PathInfo = '/table'
      Producer = dsTableProcedure
      OnAction = TableAction
    end
    item
      Name = 'waRecord'
      PathInfo = '/record'
      Producer = dsPageProducer
      OnAction = RecordAction
    end>
  AfterDispatch = WebModuleAfterDispatch
  Height = 352
  Width = 674
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 50
    Top = 24
  end
  object EmployeeTable: TFDQuery
    IndexFieldNames = 'EMP_NO'
    Connection = EmployeeConnection
    SQL.Strings = (
      
        'SELECT EMP_NO, FIRST_NAME, LAST_NAME, PHONE_EXT, HIRE_DATE, SALA' +
        'RY '
      'FROM EMPLOYEE ORDER BY EMP_NO')
    Left = 50
    Top = 72
    object EmployeeTableEMP_NO: TSmallintField
      AutoGenerateValue = arAutoInc
      FieldName = 'EMP_NO'
      Origin = 'EMP_NO'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object EmployeeTableFIRST_NAME: TStringField
      FieldName = 'FIRST_NAME'
      Origin = 'FIRST_NAME'
      Required = True
      Size = 15
    end
    object EmployeeTableLAST_NAME: TStringField
      FieldName = 'LAST_NAME'
      Origin = 'LAST_NAME'
      Required = True
    end
    object EmployeeTablePHONE_EXT: TStringField
      FieldName = 'PHONE_EXT'
      Origin = 'PHONE_EXT'
      Size = 4
    end
    object EmployeeTableHIRE_DATE: TSQLTimeStampField
      AutoGenerateValue = arDefault
      FieldName = 'HIRE_DATE'
      Origin = 'HIRE_DATE'
    end
    object EmployeeTableSALARY: TBCDField
      FieldName = 'SALARY'
      Origin = 'SALARY'
      Required = True
      Precision = 18
      Size = 2
    end
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 160
    Top = 24
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 264
    Top = 24
  end
  object pageHead: TPageProducer
    HTMLDoc.Strings = (
      '<HTML><HEAD>'
      '<TITLE>WebBroker Demo</TITLE>'
      '</HEAD>'
      '<BODY>'
      '<H1>Web Broker Demo</H1>')
    Left = 408
    Top = 16
  end
  object pageFooter: TPageProducer
    HTMLDoc.Strings = (
      '<hr><I>Delphi Academy 2017</I>'
      '</BODY>'
      '</HTML>')
    Left = 408
    Top = 72
  end
  object dsTableProcedure: TDataSetTableProducer
    Columns = <
      item
        FieldName = 'EMP_NO'
      end
      item
        FieldName = 'FIRST_NAME'
      end
      item
        FieldName = 'LAST_NAME'
      end
      item
        FieldName = 'PHONE_EXT'
      end
      item
        FieldName = 'HIRE_DATE'
      end
      item
        FieldName = 'SALARY'
      end>
    DataSet = EmployeeTable
    TableAttributes.BgColor = 'White'
    TableAttributes.Border = 1
    TableAttributes.CellSpacing = 0
    TableAttributes.CellPadding = 4
    OnFormatCell = dsTableProcedureFormatCell
    Left = 520
    Top = 72
  end
  object dsPageProducer: TDataSetPageProducer
    HTMLDoc.Strings = (
      '<H3>Employee: <#LastName></H3>'
      '<ul>'
      '<li> Employee ID: <#Emp_No>'
      '<li> Name: <#First_Name> <#Last_Name>'
      '<li> Phone: <#Phone_Ext>'
      '<li> Hired On: <#Hire_Date>'
      '<li> Salary: <#Salary>'
      '</ul>')
    DataSet = EmployeeTable
    Left = 520
    Top = 16
  end
end
