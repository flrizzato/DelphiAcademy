object DMHTTP: TDMHTTP
  OldCreateOrder = False
  Height = 185
  Width = 215
  object NetHTTPRequest1: TNetHTTPRequest
    Client = NetHTTPClient1
    Left = 56
    Top = 96
  end
  object NetHTTPClient1: TNetHTTPClient
    AllowCookies = True
    HandleRedirects = True
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 56
    Top = 24
  end
end
