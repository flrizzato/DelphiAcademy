object ServerContainer1: TServerContainer1
  OldCreateOrder = False
  DisplayName = 'service101'
  OnStart = ServiceStart
  Height = 271
  Width = 415
  object DSServer1: TDSServer
    Left = 96
    Top = 11
  end
  object DSHTTPService1: TDSHTTPService
    HttpPort = 8080
    Active = True
    Server = DSServer1
    Filters = <>
    Left = 144
    Top = 71
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    Left = 200
    Top = 11
  end
end
