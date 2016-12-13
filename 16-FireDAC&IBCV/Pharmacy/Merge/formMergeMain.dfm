object frmMergeMain: TfrmMergeMain
  Left = 0
  Top = 0
  Caption = 'Update Medicine Data'
  ClientHeight = 291
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 742
    Height = 76
    Align = alTop
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 369
      Top = 1
      Height = 74
      ExplicitLeft = 436
      ExplicitTop = 6
    end
    object Panel2: TPanel
      Left = 372
      Top = 1
      Width = 369
      Height = 74
      Align = alClient
      TabOrder = 0
      object GroupBox2: TGroupBox
        Left = 1
        Top = 1
        Width = 367
        Height = 72
        Align = alClient
        Caption = 'Central MSSQL Database'
        TabOrder = 0
        DesignSize = (
          367
          72)
        object edtMSSQLDatabase: TEdit
          Left = 24
          Top = 47
          Width = 297
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'Pharmacy'
        end
        object edtMSSQLServer: TEdit
          Left = 24
          Top = 20
          Width = 297
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = 'lEnterYourServerName\MSSQL2014'
        end
      end
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 368
      Height = 74
      Align = alLeft
      TabOrder = 1
      object GroupBox1: TGroupBox
        Left = 1
        Top = 1
        Width = 366
        Height = 72
        Align = alClient
        Caption = 'Central InterBase Database'
        TabOrder = 0
        DesignSize = (
          366
          72)
        object sbInterBase: TSpeedButton
          Left = 311
          Top = 24
          Width = 23
          Height = 22
          Anchors = [akTop, akRight]
          Caption = '...'
          ExplicitLeft = 343
        end
        object edtIBDatabase: TEdit
          Left = 16
          Top = 24
          Width = 297
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'c:\data\MEDICINES.IB'
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 76
    Width = 742
    Height = 215
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 1
    DesignSize = (
      742
      215)
    object Label1: TLabel
      Left = 1
      Top = 103
      Width = 740
      Height = 39
      Align = alTop
      Alignment = taCenter
      Caption = 
        'This application was a sample used to get a MS-SQL database sync' +
        '-ed into a central InterBase database that then managed the chan' +
        'geviews to provide delta'#39's for remote databases to collect. This' +
        ' reduced network traffic, and speeded up the time it took to upd' +
        'ate a remote database. - This is provided as an example only as ' +
        'the MSSQL data is not shipped with the demo.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      ExplicitTop = 175
      ExplicitWidth = 734
    end
    object btnMerge: TButton
      Left = 334
      Top = 164
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'Merge'
      TabOrder = 0
    end
    object ProgressBar1: TProgressBar
      Left = 1
      Top = 1
      Width = 740
      Height = 17
      Align = alTop
      TabOrder = 1
    end
    object ProgressBar2: TProgressBar
      Left = 1
      Top = 18
      Width = 740
      Height = 17
      Align = alTop
      TabOrder = 2
    end
    object ProgressBar3: TProgressBar
      Left = 1
      Top = 35
      Width = 740
      Height = 17
      Align = alTop
      TabOrder = 3
    end
    object ProgressBar4: TProgressBar
      Left = 1
      Top = 52
      Width = 740
      Height = 17
      Align = alTop
      TabOrder = 4
    end
    object ProgressBar5: TProgressBar
      Left = 1
      Top = 69
      Width = 740
      Height = 17
      Align = alTop
      TabOrder = 5
    end
    object ProgressBar6: TProgressBar
      Left = 1
      Top = 86
      Width = 740
      Height = 17
      Align = alTop
      TabOrder = 6
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'InterBase'
        FileMask = '*.ib;*.gdb'
      end>
    Options = []
    Left = 768
  end
  object MainMenu1: TMainMenu
    Left = 368
    Top = 152
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Action = FileExit1
      end
    end
  end
  object ActionList1: TActionList
    Left = 448
    Top = 160
    object FileExit1: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
  end
end
