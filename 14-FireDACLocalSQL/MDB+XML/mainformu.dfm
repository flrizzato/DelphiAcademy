object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MDB+XML'
  ClientHeight = 247
  ClientWidth = 664
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    664
    247)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 8
    Top = 39
    Width = 647
    Height = 200
    Anchors = [akLeft, akTop, akRight, akBottom]
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
        FieldName = 'Employee'
        Width = 155
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OrderID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CustomerID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'EmployeeID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'OrderDate'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'RequiredDate'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShippedDate'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipVia'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Freight'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipName'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipAddress'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipCity'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipRegion'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipPostalCode'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipCountry'
        Width = 64
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 2
    Top = 8
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object DataSource1: TDataSource
    DataSet = MainDM.FDQuery1
    Left = 60
    Top = 80
  end
end
