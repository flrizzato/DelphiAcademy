object MainDM: TMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 400
  Width = 562
  object SQLConnection: TFDConnection
    ConnectionName = 'MO'
    Params.Strings = (
      'Database=C:\DelphiAcademy\49-VCLModern2\MEETINGORGANIZER.IB'
      'User_Name=sysdba'
      'Password=masterkey'
      'DriverID=IBLite')
    UpdateOptions.AssignedValues = [uvAutoCommitUpdates]
    UpdateOptions.AutoCommitUpdates = True
    LoginPrompt = False
    BeforeConnect = SQLConnectionBeforeConnect
    Left = 48
    Top = 24
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 48
    Top = 136
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Lite = True
    Left = 48
    Top = 80
  end
end
