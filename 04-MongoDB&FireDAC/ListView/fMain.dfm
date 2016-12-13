object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'ListView'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    635
    300)
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 8
    Top = 39
    Width = 619
    Height = 253
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Read using Find'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 119
    Top = 8
    Width = 186
    Height = 25
    Caption = 'Read using Next / Recurse / Return'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 311
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Read using Iterate'
    TabOrder = 3
    OnClick = Button3Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=172.16.121.240'
      'User_Name=test'
      'Password=test'
      'DriverID=Mongo')
    Left = 72
    Top = 72
  end
end
