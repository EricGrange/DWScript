object dwsWebServerLib: TdwsWebServerLib
  OldCreateOrder = False
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
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsNameEval
          end
          item
            Name = 'HttpPort'
            ResultType = 'Integer'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsHttpPortEval
          end
          item
            Name = 'HttpsPort'
            ResultType = 'Integer'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsHttpsPortEval
          end
          item
            Name = 'Authentications'
            ResultType = 'WebAuthentications'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsAuthenticationsEval
          end
          item
            Name = 'FlushCompiledPrograms'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebServerClassesWebServerMethodsFlushCompiledProgramsEval
          end
          item
            Name = 'LiveQueries'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsLiveQueriesEval
          end
          item
            Name = 'CompilationInfoJSON'
            Parameters = <
              item
                Name = 'sourceFileName'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsCompilationInfoJSONEval
          end
          item
            Name = 'ExecutionInfoJSON'
            Parameters = <
              item
                Name = 'sourceFileName'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsExecutionInfoJSONEval
          end
          item
            Name = 'CompiledPrograms'
            ResultType = 'array of String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsCompiledProgramsEval
          end
          item
            Name = 'GetURLRewriteRulesJSON'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebServerClassesWebServerMethodsGetRewriteRulesJSONEval
          end
          item
            Name = 'SetURLRewriteRulesJSON'
            Parameters = <
              item
                Name = 'val'
                DataType = 'String'
              end>
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebServerClassesWebServerMethodsSetURLRewriteRulesJSONEval
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
