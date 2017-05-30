object ServerContainer1: TServerContainer1
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 271
  Width = 415
  object DSServer1: TDSServer
    Left = 48
    Top = 19
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    Left = 128
    Top = 19
  end
  object DSServerClass2: TDSServerClass
    OnGetClass = DSServerClass2GetClass
    Server = DSServer1
    LifeCycle = 'Server'
    Left = 208
    Top = 19
  end
end
