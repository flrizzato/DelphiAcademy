object BrCustOrdForm: TBrCustOrdForm
  Left = 235
  Top = 111
  HelpContext = 1
  ActiveControl = CustGrid
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Orders By Customer'
  ClientHeight = 307
  ClientWidth = 405
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CtrlsPanel: TPanel
    Left = 0
    Top = 0
    Width = 405
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ActivateBtn: TSpeedButton
      Left = 281
      Top = 4
      Width = 85
      Height = 25
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Activate Query'
      OnClick = ActivateQuery
    end
    object DefineBtn: TSpeedButton
      Left = 167
      Top = 5
      Width = 85
      Height = 25
      AllowAllUp = True
      Caption = '&Define Query'
      OnClick = SetQuery
    end
    object Bevel1: TBevel
      Left = 0
      Top = 34
      Width = 405
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object Navigator: TDBNavigator
      Left = 8
      Top = 5
      Width = 135
      Height = 25
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object BtnPanel: TPanel
    Left = 0
    Top = 271
    Width = 405
    Height = 36
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object EditBtn: TButton
      Left = 221
      Top = 6
      Width = 84
      Height = 25
      Caption = '&Edit'
      TabOrder = 0
      OnClick = EditBtnClick
    end
    object CloseBtn: TButton
      Left = 311
      Top = 6
      Width = 84
      Height = 25
      Cancel = True
      Caption = '&Close'
      Default = True
      TabOrder = 1
      OnClick = CloseBtnClick
    end
  end
  object CustPanel: TPanel
    Left = 0
    Top = 36
    Width = 405
    Height = 118
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object CustGrid: TDBGrid
      Left = 6
      Top = 3
      Width = 393
      Height = 111
      Columns = <
        item
          FieldName = 'CustNo'
        end
        item
          FieldName = 'Company'
        end
        item
          FieldName = 'Phone'
        end
        item
          FieldName = 'LastInvoiceDate'
        end>
      DataSource = MastData.CustMasterSrc
      Options = [dgTitles, dgIndicator, dgColLines, dgRowLines, dgRowSelect]
      ReadOnly = True
      TabOrder = 0
      TitleFont.Color = clBlack
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnDblClick = EditBtnClick
      OnEnter = CustGridEnter
    end
  end
  object OrdersPanel: TPanel
    Left = 0
    Top = 154
    Width = 405
    Height = 117
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object OrdersGrid: TDBGrid
      Left = 6
      Top = 3
      Width = 392
      Height = 111
      DataSource = MastData.OrdByCustSrc
      Options = [dgTitles, dgIndicator, dgColLines, dgRowLines, dgRowSelect]
      TabOrder = 0
      TitleFont.Color = clBlack
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnDblClick = EditBtnClick
      OnEnter = OrdersGridEnter
    end
  end
end
