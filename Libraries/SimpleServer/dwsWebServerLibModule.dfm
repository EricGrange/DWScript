object dwsWebServerLib: TdwsWebServerLib
  OldCreateOrder = False
  Left = 810
  Top = 86
  Height = 150
  Width = 215
  object dwsWebServer: TdwsUnit
    Classes = <
      item
        Name = 'WebServer'
        IsStatic = True
        Methods = <
          item
            Name = 'Name'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebServerClassesWebServerMethodsNameEval
            Kind = mkClassFunction
          end
          item
            Name = 'HttpPort'
            ResultType = 'Integer'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebServerClassesWebServerMethodsHttpPortEval
            Kind = mkClassFunction
          end
          item
            Name = 'HttpsPort'
            ResultType = 'Integer'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebServerClassesWebServerMethodsHttpsPortEval
            Kind = mkClassFunction
          end
          item
            Name = 'Authentications'
            ResultType = 'WebAuthentications'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebServerClassesWebServerMethodsAuthenticationsEval
            Kind = mkClassFunction
          end
          item
            Name = 'FlushCompiledPrograms'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebServerClassesWebServerMethodsFlushCompiledProgramsEval
            Kind = mkClassProcedure
          end
          item
            Name = 'LiveQueries'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebServerClassesWebServerMethodsLiveQueriesEval
            Kind = mkClassFunction
          end>
      end>
    Dependencies.Strings = (
      'System.Net')
    UnitName = 'System.WebServer'
    StaticSymbols = True
    Left = 48
    Top = 24
  end
end
