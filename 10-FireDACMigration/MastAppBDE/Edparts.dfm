object EdPartsForm: TEdPartsForm
  Left = 274
  Top = 90
  HelpContext = 6
  ActiveControl = DBEdit2
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Edit Parts'
  ClientHeight = 264
  ClientWidth = 313
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 313
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object PrintBtn: TSpeedButton
      Left = 270
      Top = 5
      Width = 25
      Height = 25
      Hint = 'Print form image'
      Glyph.Data = {
        2A010000424D2A010000000000007600000028000000130000000F0000000100
        0400000000000000000000000000000000000000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333F0EFF3300000000000003333F00FF37877777777777703330000030F8
        8888888888870333003330F9988888888887703F11EE37FFFFFFFFFFFFF77039
        0910330888888888888F703865F03330870000000078F03765F03333000FFFFF
        F000033865F03333330F44448033333765F033333330FFFFFF03333865F03333
        3330F4444803333765F0333333330FFFFFF0333865F033333333000000003336
        66C0333333333333333333300000}
      OnClick = PrintBtnClick
    end
    object Bevel1: TBevel
      Left = 0
      Top = 34
      Width = 313
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object Navigator: TDBNavigator
      Left = 16
      Top = 5
      Width = 240
      Height = 25
      HelpContext = 6
      DataSource = PartsSource1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 36
    Width = 313
    Height = 193
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 11
      Width = 33
      Height = 13
      Caption = 'PartNo'
    end
    object Label2: TLabel
      Left = 8
      Top = 33
      Width = 53
      Height = 13
      Caption = 'Description'
    end
    object Label3: TLabel
      Left = 8
      Top = 55
      Width = 34
      Height = 13
      Caption = 'Vendor'
    end
    object Label4: TLabel
      Left = 8
      Top = 77
      Width = 40
      Height = 13
      Caption = 'OnHand'
    end
    object Label5: TLabel
      Left = 8
      Top = 100
      Width = 40
      Height = 13
      Caption = 'OnOrder'
    end
    object Label7: TLabel
      Left = 8
      Top = 144
      Width = 21
      Height = 13
      Caption = 'Cost'
    end
    object Label8: TLabel
      Left = 8
      Top = 166
      Width = 40
      Height = 13
      Caption = 'ListPrice'
    end
    object Label6: TLabel
      Left = 8
      Top = 122
      Width = 61
      Height = 13
      Caption = 'Backordered'
    end
    object DBEdit2: TDBEdit
      Left = 80
      Top = 29
      Width = 225
      Height = 21
      HelpContext = 6
      DataField = 'Description'
      DataSource = PartsSource1
      MaxLength = 30
      TabOrder = 1
    end
    object DBEdit4: TDBEdit
      Left = 80
      Top = 74
      Width = 82
      Height = 21
      HelpContext = 6
      DataField = 'OnHand'
      DataSource = PartsSource1
      MaxLength = 0
      TabOrder = 3
    end
    object DBEdit5: TDBEdit
      Left = 80
      Top = 97
      Width = 82
      Height = 21
      HelpContext = 6
      DataField = 'OnOrder'
      DataSource = PartsSource1
      MaxLength = 0
      TabOrder = 4
    end
    object DBEdit7: TDBEdit
      Left = 80
      Top = 141
      Width = 102
      Height = 21
      HelpContext = 6
      DataField = 'Cost'
      DataSource = PartsSource1
      MaxLength = 0
      TabOrder = 6
    end
    object DBEdit8: TDBEdit
      Left = 80
      Top = 163
      Width = 102
      Height = 21
      HelpContext = 6
      DataField = 'ListPrice'
      DataSource = PartsSource1
      MaxLength = 0
      TabOrder = 7
    end
    object DBEdPartNo: TDBEdit
      Left = 80
      Top = 6
      Width = 102
      Height = 21
      HelpContext = 6
      DataField = 'PartNo'
      DataSource = PartsSource1
      MaxLength = 0
      TabOrder = 0
    end
    object DBEdit3: TDBEdit
      Left = 80
      Top = 119
      Width = 45
      Height = 21
      HelpContext = 6
      DataField = 'BackOrd'
      DataSource = PartsSource1
      MaxLength = 0
      TabOrder = 5
    end
    object DataComboBox1: TDBLookupComboBox
      Left = 80
      Top = 51
      Width = 225
      Height = 21
      DataField = 'VendorNo'
      DataSource = PartsSource1
      KeyField = 'VendorNo'
      ListField = 'VendorName'
      ListSource = MastData.VendorSource
      TabOrder = 2
    end
  end
  object OKButton: TButton
    Left = 147
    Top = 235
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 232
    Top = 235
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object PartsSource1: TDataSource
    DataSet = MastData.Parts
    Left = 24
    Top = 232
  end
end
