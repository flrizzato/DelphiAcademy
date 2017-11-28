object IWForm1: TIWForm1
  Left = 0
  Top = 0
  Width = 555
  Height = 400
  RenderInvisibleControls = True
  AllowPageAccess = True
  ConnectionMode = cmAny
  Background.Fixed = False
  LayoutMgr = IWLayoutMgrHTML1
  HandleTabs = False
  LeftToRight = True
  LockUntilLoaded = True
  LockOnSubmit = True
  ShowHint = True
  XPTheme = True
  DesignLeft = 8
  DesignTop = 8
  object IWEdit1: TIWEdit
    Left = 32
    Top = 48
    Width = 185
    Height = 21
    Css = 'form-control'
    RenderSize = False
    StyleRenderOptions.RenderSize = False
    StyleRenderOptions.RenderPosition = False
    StyleRenderOptions.RenderFont = False
    StyleRenderOptions.RenderZIndex = False
    StyleRenderOptions.RenderVisibility = False
    StyleRenderOptions.RenderStatus = False
    StyleRenderOptions.RenderAbsolute = False
    StyleRenderOptions.RenderPadding = False
    StyleRenderOptions.RenderBorder = False
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWEdit1'
    SubmitOnAsyncEvent = True
    TabOrder = 0
  end
  object IWLabel1: TIWLabel
    Left = 32
    Top = 26
    Width = 64
    Height = 16
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    ForControl = IWEdit1
    HasTabOrder = False
    FriendlyName = 'IWLabel1'
    Caption = 'Seu Nome'
  end
  object IWListbox1: TIWListbox
    Left = 32
    Top = 88
    Width = 257
    Height = 249
    Css = 'list-group'
    StyleRenderOptions.RenderZIndex = False
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    TabOrder = 1
    FriendlyName = 'IWListbox1'
    NoSelectionText = '-- No Selection --'
  end
  object IWButton1: TIWButton
    Left = 223
    Top = 44
    Width = 66
    Height = 25
    Hint = 'Adicionar nome...'
    Css = 'btn btn-primary'
    Caption = 'Add'
    Color = clBtnFace
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWButton1'
    TabOrder = 2
    OnClick = IWButton1Click
  end
  object IWLayoutMgrHTML1: TIWLayoutMgrHTML
    HTML.Strings = (
      '<!DOCTYPE html>'
      '<html lang="en">'
      '   <head>'
      '      <title>Bootstrap Example 101</title>'
      '      <meta charset="utf-8">'
      
        '      <meta name="viewport" content="width=device-width, initial' +
        '-scale=1">'
      
        '      <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.c' +
        'om/bootstrap/3.3.7/css/bootstrap.min.css">'
      
        '      <script src="https://ajax.googleapis.com/ajax/libs/jquery/' +
        '3.2.0/jquery.min.js"></script>'
      
        '      <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3' +
        '.7/js/bootstrap.min.js"></script>'
      '   </head>'
      '   <body>'
      '      <div class="container">'
      '         <div class="panel panel-primary">'
      
        '            <div class="panel-heading">Hello from Bootstrap!!!</' +
        'div>'
      '            <div class="panel-body">'
      '               <div class="form-group">'
      '                  {%IWLabel1%}'
      '                  {%IWEdit1%}'
      '               </div>'
      '               <div class="form-group">'
      '                  {%IWButton1%}'
      '               </div>'
      '               {%IWListbox1%}'
      '            </div>'
      '         </div>'
      '      </div>'
      '   </body>'
      '</html>')
    Left = 376
    Top = 64
  end
end
