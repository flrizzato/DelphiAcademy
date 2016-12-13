object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 455
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 273
    Height = 21
    TabOrder = 0
    TextHint = 'Your text here...'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 56
    Width = 389
    Height = 391
    ItemHeight = 13
    TabOrder = 1
  end
  object Button1: TButton
    Left = 288
    Top = 20
    Width = 109
    Height = 25
    Action = Action1
    TabOrder = 2
  end
  object ActionList1: TActionList
    Left = 304
    Top = 280
    object Action1: TAction
      Caption = 'Add >>'
      OnExecute = Action1Execute
    end
  end
  object TetheringManager1: TTetheringManager
    Text = 'TetheringManager1'
    AllowedAdapters = 'Network'
    Left = 304
    Top = 152
  end
  object TetheringAppProfile1: TTetheringAppProfile
    Manager = TetheringManager1
    Text = 'TetheringAppProfile1'
    Group = 'MyActions'
    Actions = <
      item
        Name = 'MyAction'
        IsPublic = True
        Action = Action1
        NotifyUpdates = False
      end>
    Resources = <>
    OnResourceReceived = TetheringAppProfile1ResourceReceived
    Left = 304
    Top = 216
  end
end
