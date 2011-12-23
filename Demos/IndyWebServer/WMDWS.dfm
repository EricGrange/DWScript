object WebModuleDWS: TWebModuleDWS
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModuleDWSDefaultHandlerAction
    end>
  Left = 120
  Top = 178
  Height = 216
  Width = 335
  object dwsGlobalVarsFunctions: TdwsGlobalVarsFunctions
    Left = 208
    Top = 24
  end
  object RestrictedFileSystem: TdwsRestrictedFileSystem
    Left = 64
    Top = 24
  end
end
