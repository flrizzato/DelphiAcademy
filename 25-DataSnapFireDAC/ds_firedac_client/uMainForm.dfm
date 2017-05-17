object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'DataSnap FireDAC'
  ClientHeight = 493
  ClientWidth = 744
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
    Width = 744
    Height = 40
    DataSource = DataSource1
    Align = alTop
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 40
    Width = 744
    Height = 263
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
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CONTACT_LAST'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PHONE_NO'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDRESS_LINE1'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ADDRESS_LINE2'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CITY'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'STATE_PROVINCE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'COUNTRY'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'POSTAL_CODE'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ON_HOLD'
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 453
    Width = 744
    Height = 40
    Align = alBottom
    TabOrder = 2
    object Button1: TButton
      AlignWithMargins = True
      Left = 216
      Top = 6
      Width = 85
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
      Left = 311
      Top = 6
      Width = 85
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
      Left = 406
      Top = 6
      Width = 85
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
    object Button4: TButton
      AlignWithMargins = True
      Left = 501
      Top = 6
      Width = 85
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Apply Manual'
      TabOrder = 4
      OnClick = Button4Click
    end
  end
  object DBGrid2: TDBGrid
    Left = 0
    Top = 303
    Width = 744
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
    DataSet = ClientModule2.CustomerMemTable
    Left = 40
    Top = 72
  end
  object DataSource2: TDataSource
    DataSet = ClientModule2.SalesMemTable
    Left = 48
    Top = 320
  end
end
