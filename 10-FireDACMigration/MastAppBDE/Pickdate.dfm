object BrDateForm: TBrDateForm
  Left = 221
  Top = 143
  BorderStyle = bsDialog
  Caption = 'Select a Date'
  ClientHeight = 220
  ClientWidth = 362
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 16
    Top = 8
    Width = 330
    Height = 177
    Shape = bsFrame
  end
  object TitleLabel: TLabel
    Left = 54
    Top = 18
    Width = 257
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'February, 1995'
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PrevMonthBtn: TSpeedButton
    Left = 22
    Top = 16
    Width = 20
    Height = 20
    Glyph.Data = {
      E6000000424DE60000000000000076000000280000000E0000000E0000000100
      0400000000007000000000000000000000000000000010000000000000000000
      BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3300333333333333330033333300033333003333330003333300333333000333
      3300333333000333330033333300033333003333330003333300333333000333
      3300333300000003330033333000003333003333330003333300333333303333
      33003333333333333300}
    OnClick = PrevMonthBtnClick
  end
  object NextMonthBtn: TSpeedButton
    Left = 320
    Top = 16
    Width = 20
    Height = 20
    Glyph.Data = {
      E6000000424DE60000000000000076000000280000000E0000000E0000000100
      0400000000007000000000000000000000000000000010000000000000000000
      BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3300333333303333330033333300033333003333300000333300333300000003
      3300333333000333330033333300033333003333330003333300333333000333
      3300333333000333330033333300033333003333330003333300333333000333
      33003333333333333300}
    OnClick = NextMonthBtnClick
  end
  object Calendar1: TCalendar
    Left = 22
    Top = 41
    Width = 318
    Height = 136
    Color = clInfoBk
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    StartOfWeek = 0
    TabOrder = 0
    UseCurrentDate = False
    OnChange = Calendar1Change
  end
  object OkBtn: TButton
    Left = 204
    Top = 189
    Width = 73
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 284
    Top = 189
    Width = 73
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
