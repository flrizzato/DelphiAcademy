object BrPartsForm: TBrPartsForm
  Left = 229
  Top = 151
  HelpContext = 4
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Browse Parts'
  ClientHeight = 235
  ClientWidth = 384
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 37
    HelpContext = 4
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object ActivateBtn: TSpeedButton
      Left = 284
      Top = 4
      Width = 85
      Height = 25
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Backorders'
      OnClick = ActivateQuery
    end
    object Bevel1: TBevel
      Left = 3
      Top = 32
      Width = 378
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object Navigator: TDBNavigator
      Left = 8
      Top = 4
      Width = 135
      Height = 25
      DataSource = MastData.PartsSource
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object EditBtn: TButton
      Left = 177
      Top = 4
      Width = 74
      Height = 25
      Caption = '&Edit'
      ModalResult = 1
      TabOrder = 1
      OnClick = EditBtnClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 196
    Width = 384
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object CloseBtn: TButton
      Left = 300
      Top = 9
      Width = 74
      Height = 25
      Cancel = True
      Caption = '&Close'
      Default = True
      ModalResult = 2
      TabOrder = 0
      OnClick = CloseBtnClick
    end
  end
  object PartsGrid: TDBGrid
    Left = 0
    Top = 37
    Width = 384
    Height = 159
    Align = alClient
    Columns = <
      item
        FieldName = 'PartNo'
      end
      item
        FieldName = 'Description'
      end
      item
        FieldName = 'OnHand'
      end
      item
        FieldName = 'OnOrder'
      end
      item
        FieldName = 'BackOrd'
      end>
    DataSource = MastData.PartsSource
    Options = [dgTitles, dgColLines, dgRowLines, dgRowSelect]
    TabOrder = 2
    TitleFont.Color = clBlack
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDblClick = EditBtnClick
  end
end
