object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 619
  ClientWidth = 730
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CalendarView1: TCalendarView
    Left = 40
    Top = 24
    Date = 42703.000000000000000000
    FirstDayOfWeek = dwMonday
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = []
    HeaderInfo.DaysOfWeekFont.Charset = DEFAULT_CHARSET
    HeaderInfo.DaysOfWeekFont.Color = clWindowText
    HeaderInfo.DaysOfWeekFont.Height = -13
    HeaderInfo.DaysOfWeekFont.Name = 'Segoe UI'
    HeaderInfo.DaysOfWeekFont.Style = []
    HeaderInfo.Font.Charset = DEFAULT_CHARSET
    HeaderInfo.Font.Color = clWindowText
    HeaderInfo.Font.Height = -20
    HeaderInfo.Font.Name = 'Segoe UI'
    HeaderInfo.Font.Style = []
    OnClick = CalendarView1Click
    OnDrawDayItem = CalendarView1DrawDayItem
    OnDrawMonthItem = CalendarView1DrawMonthItem
    OnDrawYearItem = CalendarView1DrawYearItem
    ParentFont = False
    ShowFirstOfGroupLabel = True
    TabOrder = 0
    TodayColor = clMedGray
  end
  object CalendarPicker1: TCalendarPicker
    Left = 45
    Top = 384
    Width = 289
    Height = 32
    CalendarHeaderInfo.DaysOfWeekFont.Charset = DEFAULT_CHARSET
    CalendarHeaderInfo.DaysOfWeekFont.Color = clWindowText
    CalendarHeaderInfo.DaysOfWeekFont.Height = -13
    CalendarHeaderInfo.DaysOfWeekFont.Name = 'Segoe UI'
    CalendarHeaderInfo.DaysOfWeekFont.Style = []
    CalendarHeaderInfo.Font.Charset = DEFAULT_CHARSET
    CalendarHeaderInfo.Font.Color = clWindowText
    CalendarHeaderInfo.Font.Height = -20
    CalendarHeaderInfo.Font.Name = 'Segoe UI'
    CalendarHeaderInfo.Font.Style = []
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ShowFirstOfGroupLabel = True
    TabOrder = 1
    TextHint = 'select a date'
  end
  object ToggleSwitch1: TToggleSwitch
    Left = 368
    Top = 24
    Width = 126
    Height = 20
    StateCaptions.CaptionOn = 'MultiSelect On'
    StateCaptions.CaptionOff = 'MultiSelect Off'
    TabOrder = 2
    OnClick = ToggleSwitch1Click
  end
  object memLog: TMemo
    Left = 368
    Top = 72
    Width = 321
    Height = 441
    Lines.Strings = (
      'memLog')
    ScrollBars = ssVertical
    TabOrder = 3
  end
end
