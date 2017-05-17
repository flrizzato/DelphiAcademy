object MainForm: TMainForm
  Left = 0
  Top = 0
  ActiveControl = DBGrid1
  Caption = 'DataSnap FireDAC DbExpress'
  ClientHeight = 449
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 0
    Width = 639
    Height = 40
    DataSource = DataSource1
    Align = alTop
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 40
    Width = 639
    Height = 219
    Align = alClient
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'CUST_NO'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CUSTOMER'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CONTACT_FIRST'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CONTACT_LAST'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PHONE_NO'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDRESS_LINE1'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDRESS_LINE2'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CITY'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'STATE_PROVINCE'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'COUNTRY'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'POSTAL_CODE'
        Width = 64
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ON_HOLD'
        Width = 64
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 409
    Width = 639
    Height = 40
    Align = alBottom
    TabOrder = 2
    object Button1: TButton
      AlignWithMargins = True
      Left = 216
      Top = 6
      Width = 75
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Open'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 301
      Top = 6
      Width = 75
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Apply'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 386
      Top = 6
      Width = 75
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = Button3Click
    end
    object edtHostName: TEdit
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 200
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Alignment = taCenter
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      Text = '127.0.0.1'
      TextHint = 'Hostname or IP Address'
      ExplicitHeight = 21
    end
  end
  object DBGrid2: TDBGrid
    Left = 0
    Top = 259
    Width = 639
    Height = 150
    Align = alBottom
    DataSource = DataSource2
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'PO_NUMBER'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CUST_NO'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SALES_REP'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ORDER_STATUS'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ORDER_DATE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SHIP_DATE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DATE_NEEDED'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PAID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'QTY_ORDERED'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'TOTAL_VALUE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'DISCOUNT'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ITEM_TYPE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'AGED'
        Visible = True
      end>
  end
  object DataSource1: TDataSource
    DataSet = ClientModule1.CustomerTable
    Left = 40
    Top = 72
  end
  object DataSource2: TDataSource
    DataSet = ClientModule1.SalesTable
    Left = 48
    Top = 320
  end
end
