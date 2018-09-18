inherited UserDM: TUserDM
  OldCreateOrder = True
  Height = 260
  Width = 342
  inherited sqlControl: TFDQuery
    Connection = MainDM.SQLConnection
    SQL.Strings = (
      'SELECT USR.USER_ID,'
      '       USR.NAME,'
      '       USR.EMAIL,'
      '       USR.PHONE,'
      '       USR.LOGIN,'
      '       USR.PASSW,'
      '       USR.ISADMIN'
      '  FROM USERS USR'
      'ORDER BY USR.USER_ID')
    object sqlControlUSER_ID: TIntegerField
      DefaultExpression = '0'
      FieldName = 'USER_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object sqlControlNAME: TStringField
      FieldName = 'NAME'
      Size = 40
    end
    object sqlControlEMAIL: TStringField
      FieldName = 'EMAIL'
      Size = 60
    end
    object sqlControlPHONE: TStringField
      FieldName = 'PHONE'
    end
    object sqlControlLOGIN: TStringField
      FieldName = 'LOGIN'
      Size = 10
    end
    object sqlControlPASSW: TStringField
      FieldName = 'PASSW'
      Size = 10
    end
    object sqlControlISADMIN: TStringField
      FieldName = 'ISADMIN'
      FixedChar = True
      Size = 1
    end
  end
  inherited dspControl: TDataSetProvider
    BeforeUpdateRecord = dspControlBeforeUpdateRecord
  end
  inherited cdsControl: TClientDataSet
    BeforePost = cdsControlBeforePost
    object cdsControlUSER_ID: TIntegerField
      DefaultExpression = '0'
      DisplayLabel = 'User ID'
      FieldName = 'USER_ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object cdsControlNAME: TStringField
      DisplayLabel = 'Name'
      FieldName = 'NAME'
      Required = True
      Size = 40
    end
    object cdsControlEMAIL: TStringField
      DisplayLabel = 'E-Mail'
      FieldName = 'EMAIL'
      Required = True
      Size = 60
    end
    object cdsControlPHONE: TStringField
      DisplayLabel = 'Phone'
      FieldName = 'PHONE'
    end
    object cdsControlLOGIN: TStringField
      DisplayLabel = 'Login Name'
      FieldName = 'LOGIN'
      Required = True
      Size = 10
    end
    object cdsControlPASSW: TStringField
      DisplayLabel = 'Password'
      FieldName = 'PASSW'
      Required = True
      Size = 10
    end
    object cdsControlISADMIN: TStringField
      DefaultExpression = #39'N'#39
      DisplayLabel = 'Is Admin?'
      FieldName = 'ISADMIN'
      Required = True
      OnGetText = cdsControlISADMINGetText
      FixedChar = True
      Size = 1
    end
  end
end
