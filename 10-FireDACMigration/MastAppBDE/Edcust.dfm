object EdCustForm: TEdCustForm
  Left = 344
  Top = 188
  HelpContext = 1
  ActiveControl = DBEdName
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Edit Customers'
  ClientHeight = 307
  ClientWidth = 376
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 39
    Width = 376
    Height = 228
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 248
      Top = 12
      Width = 35
      Height = 13
      Caption = 'CustNo'
    end
    object Label2: TLabel
      Left = 20
      Top = 12
      Width = 44
      Height = 13
      Caption = 'Company'
    end
    object Label3: TLabel
      Left = 20
      Top = 53
      Width = 25
      Height = 13
      Caption = 'Add1'
    end
    object Label4: TLabel
      Left = 20
      Top = 93
      Width = 25
      Height = 13
      Caption = 'Add2'
    end
    object Label5: TLabel
      Left = 20
      Top = 133
      Width = 17
      Height = 13
      Caption = 'City'
    end
    object Label6: TLabel
      Left = 124
      Top = 134
      Width = 25
      Height = 13
      Caption = 'State'
    end
    object Label7: TLabel
      Left = 192
      Top = 134
      Width = 43
      Height = 13
      Caption = 'Zip Code'
    end
    object Label8: TLabel
      Left = 289
      Top = 132
      Width = 36
      Height = 13
      Caption = 'Country'
    end
    object Label9: TLabel
      Left = 248
      Top = 53
      Width = 31
      Height = 13
      Caption = 'Phone'
    end
    object Label11: TLabel
      Left = 284
      Top = 186
      Width = 44
      Height = 13
      Caption = 'Tax Rate'
    end
    object Label13: TLabel
      Left = 144
      Top = 188
      Width = 58
      Height = 13
      Caption = 'Last Invoice'
    end
    object Label14: TLabel
      Left = 248
      Top = 92
      Width = 17
      Height = 13
      Caption = 'Fax'
    end
    object Bevel2: TBevel
      Left = 8
      Top = 178
      Width = 345
      Height = 5
      Shape = bsTopLine
    end
    object Label10: TLabel
      Left = 16
      Top = 188
      Width = 37
      Height = 13
      Caption = 'Contact'
    end
    object DBEdCustNo: TDBEdit
      Left = 247
      Top = 28
      Width = 69
      Height = 21
      Color = clSilver
      DataField = 'CustNo'
      DataSource = MastData.CustSource
      Enabled = False
      MaxLength = 0
      ReadOnly = True
      TabOrder = 12
    end
    object DBEdName: TDBEdit
      Left = 19
      Top = 28
      Width = 205
      Height = 21
      DataField = 'Company'
      DataSource = MastData.CustSource
      MaxLength = 30
      TabOrder = 0
    end
    object DBEdit3: TDBEdit
      Left = 19
      Top = 68
      Width = 205
      Height = 21
      DataField = 'Addr1'
      DataSource = MastData.CustSource
      MaxLength = 30
      TabOrder = 1
    end
    object DBEdit4: TDBEdit
      Left = 19
      Top = 108
      Width = 205
      Height = 21
      DataField = 'Addr2'
      DataSource = MastData.CustSource
      MaxLength = 30
      TabOrder = 2
    end
    object DBEdit5: TDBEdit
      Left = 19
      Top = 148
      Width = 98
      Height = 21
      DataField = 'City'
      DataSource = MastData.CustSource
      MaxLength = 15
      TabOrder = 3
    end
    object DBEdit6: TDBEdit
      Left = 123
      Top = 148
      Width = 62
      Height = 21
      DataField = 'State'
      DataSource = MastData.CustSource
      MaxLength = 20
      TabOrder = 4
    end
    object DBEdit7: TDBEdit
      Left = 191
      Top = 148
      Width = 90
      Height = 21
      DataField = 'Zip'
      DataSource = MastData.CustSource
      MaxLength = 10
      TabOrder = 5
    end
    object DBEdit8: TDBEdit
      Left = 288
      Top = 148
      Width = 66
      Height = 21
      DataField = 'Country'
      DataSource = MastData.CustSource
      MaxLength = 20
      TabOrder = 6
    end
    object DBEdit9: TDBEdit
      Left = 247
      Top = 68
      Width = 106
      Height = 21
      DataField = 'Phone'
      DataSource = MastData.CustSource
      MaxLength = 15
      TabOrder = 7
    end
    object DBEdit11: TDBEdit
      Left = 283
      Top = 203
      Width = 54
      Height = 21
      DataField = 'TaxRate'
      DataSource = MastData.CustSource
      MaxLength = 0
      TabOrder = 11
    end
    object DBEdit12: TDBEdit
      Left = 15
      Top = 203
      Width = 118
      Height = 21
      DataField = 'Contact'
      DataSource = MastData.CustSource
      MaxLength = 20
      TabOrder = 9
    end
    object DBEdInv: TDBEdit
      Left = 143
      Top = 203
      Width = 131
      Height = 21
      DataField = 'LastInvoiceDate'
      DataSource = MastData.CustSource
      MaxLength = 0
      TabOrder = 10
    end
    object DBEdit14: TDBEdit
      Left = 247
      Top = 106
      Width = 106
      Height = 21
      DataField = 'FAX'
      DataSource = MastData.CustSource
      MaxLength = 15
      TabOrder = 8
    end
  end
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 376
    Height = 39
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    object PrintBtn: TSpeedButton
      Left = 339
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
      Left = 2
      Top = 35
      Width = 372
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object DBNavigator: TDBNavigator
      Left = 19
      Top = 5
      Width = 250
      Height = 25
      DataSource = MastData.CustSource
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 267
    Width = 376
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object CancelButton: TButton
      Left = 292
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object OKButton: TButton
      Left = 204
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
end
