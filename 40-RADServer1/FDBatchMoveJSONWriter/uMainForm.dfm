object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'JSON Writer'
  ClientHeight = 443
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 8
    Top = 8
    Width = 628
    Height = 193
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PROJ_ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PROJ_NAME'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PROJ_DESC'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TEAM_LEADER'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PRODUCT'
        Visible = True
      end>
  end
  object Button1: TButton
    Left = 8
    Top = 207
    Width = 85
    Height = 25
    Caption = 'Open'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 99
    Top = 208
    Width = 85
    Height = 25
    Caption = 'JSON'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 239
    Width = 628
    Height = 196
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object Button3: TButton
    Left = 190
    Top = 207
    Width = 85
    Height = 25
    Caption = 'Class Helper'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = Button3Click
  end
  object EmployeeConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=EMPLOYEE')
    LoginPrompt = False
    Left = 80
    Top = 40
  end
  object ProjectTable: TFDQuery
    Connection = EmployeeConnection
    SQL.Strings = (
      'SELECT * FROM PROJECT')
    Left = 78
    Top = 91
    object ProjectTablePROJ_ID: TStringField
      FieldName = 'PROJ_ID'
      Origin = 'PROJ_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
      FixedChar = True
      Size = 5
    end
    object ProjectTablePROJ_NAME: TStringField
      FieldName = 'PROJ_NAME'
      Origin = 'PROJ_NAME'
      Required = True
    end
    object ProjectTablePROJ_DESC: TMemoField
      FieldName = 'PROJ_DESC'
      Origin = 'PROJ_DESC'
      BlobType = ftMemo
    end
    object ProjectTableTEAM_LEADER: TSmallintField
      FieldName = 'TEAM_LEADER'
      Origin = 'TEAM_LEADER'
    end
    object ProjectTablePRODUCT: TStringField
      FieldName = 'PRODUCT'
      Origin = 'PRODUCT'
      Required = True
      Size = 12
    end
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 184
    Top = 40
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 288
    Top = 40
  end
  object DataSource1: TDataSource
    DataSet = ProjectTable
    Left = 80
    Top = 144
  end
  object FDBatchMove1: TFDBatchMove
    Reader = FDBatchMoveDataSetReader1
    Writer = FDBatchMoveJSONWriter1
    Mappings = <
      item
        SourceFieldName = 'PROJ_ID'
        DestinationFieldName = 'ID'
      end
      item
        SourceFieldName = 'PROJ_NAME'
        SourceExpression = 'Lower(Name)'
        DestinationFieldName = 'Name'
      end
      item
        SourceFieldName = 'PROJ_DESC'
        DestinationFieldName = 'Description'
      end
      item
        SourceFieldName = 'TEAM_LEADER'
        DestinationFieldName = 'Leader'
      end
      item
        SourceFieldName = 'PRODUCT'
        SourceExpression = 'Upper(Product)'
        DestinationFieldName = 'Product'
      end>
    LogFileName = 'Data.log'
    Left = 88
    Top = 256
  end
  object FDBatchMoveDataSetReader1: TFDBatchMoveDataSetReader
    DataSet = ProjectTable
    Optimise = False
    Left = 88
    Top = 304
  end
  object FDBatchMoveJSONWriter1: TFDBatchMoveJSONWriter
    DataDef.Fields = <
      item
        FieldName = 'ID'
        DataType = jtString
      end
      item
        FieldName = 'Name'
        DataType = jtString
      end
      item
        FieldName = 'Description'
        DataType = jtString
      end
      item
        FieldName = 'Leader'
        DataType = jtString
      end
      item
        FieldName = 'Product'
        DataType = jtString
      end>
    DataDef.Formatting = Indented
    Encoding = ecANSI
    Left = 88
    Top = 352
  end
end
