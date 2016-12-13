object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 412
  ClientWidth = 802
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edtEmpNO: TEdit
    Left = 208
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edtProjID: TEdit
    Left = 208
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 335
    Top = 30
    Width = 75
    Height = 25
    Caption = 'Execute >>'
    TabOrder = 2
    OnClick = Button1Click
  end
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 79
    Top = 30
  end
  object Get_emp_projProc: TFDStoredProc
    Connection = EmployeeConnection
    StoredProcName = 'GET_EMP_PROJ'
    Left = 74
    Top = 91
    ParamData = <
      item
        Position = 1
        Name = 'EMP_NO'
        DataType = ftSmallint
        ParamType = ptInput
      end
      item
        Position = 2
        Name = 'PROJ_ID'
        DataType = ftFixedChar
        ParamType = ptOutput
        Size = 5
      end>
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 80
    Top = 160
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 72
    Top = 216
  end
end
