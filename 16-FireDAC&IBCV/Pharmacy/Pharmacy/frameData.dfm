object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 857
  Height = 350
  TabOrder = 0
  OnResize = FrameResize
  object lblQry: TLabel
    Left = 0
    Top = 0
    Width = 857
    Height = 13
    Align = alTop
    Caption = 'Qry'
    ExplicitWidth = 18
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 213
    Width = 857
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = 4
    ExplicitTop = 26
    ExplicitWidth = 853
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 13
    Width = 857
    Height = 200
    Align = alClient
    Caption = 'Current Data'
    TabOrder = 0
    ExplicitHeight = 188
    object DBGrid1: TDBGrid
      Left = 2
      Top = 15
      Width = 853
      Height = 183
      Align = alClient
      DataSource = dsCurrent
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 216
    Width = 857
    Height = 134
    Align = alBottom
    Caption = 'Delta'
    TabOrder = 1
    object DBGrid2: TDBGrid
      Left = 2
      Top = 15
      Width = 853
      Height = 117
      Align = alClient
      DataSource = dsDelta
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      OnDrawColumnCell = DBGrid2DrawColumnCell
    end
  end
  object dsCurrent: TDataSource
    Left = 432
    Top = 62
  end
  object dsDelta: TDataSource
    Left = 440
    Top = 262
  end
end
