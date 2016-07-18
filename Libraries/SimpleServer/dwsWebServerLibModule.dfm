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
      end
      item
        Name = 'WebServerSentEvents'
        IsStatic = True
        Methods = <
          item
            Name = 'PostRaw'
            Parameters = <
              item
                Name = 'sourceName'
                DataType = 'String'
              end
              item
                Name = 'eventID'
                DataType = 'String'
              end
              item
                Name = 'eventName'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebServerClassesWebServerSentEventsMethodsPostRawEventEval
            Kind = mkClassProcedure
          end
          item
            Name = 'Close'
            Parameters = <
              item
                Name = 'sourceName'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebServerClassesWebServerSentEventsMethodsCloseEval
            Kind = mkClassProcedure
          end
          item
            Name = 'Connections'
            Parameters = <
              item
                Name = 'sourceName'
                DataType = 'String'
              end>
            ResultType = 'array of String'
            Attributes = [maStatic]
            OnEval = dwsWebServerClassesWebServerSentEventsMethodsConnectionsEval
            Kind = mkClassFunction
          end
          item
            Name = 'SourceNames'
            ResultType = 'array of String'
            Attributes = [maStatic]
            OnEval = dwsWebServerClassesWebServerSentEventsMethodsSourceNamesEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'WebServerSentEvent'
        Fields = <
          item
            Name = 'Retry'
            DataType = 'Integer'
          end
          item
            Name = 'ID'
            DataType = 'String'
          end
          item
            Name = 'Name'
            DataType = 'String'
          end
          item
            Name = 'Data'
            DataType = 'array of String'
          end>
        Methods = <
          item
            Name = 'Post'
            Parameters = <
              item
                Name = 'sourceName'
                DataType = 'String'
              end>
            OnEval = dwsWebServerClassesWebServerSentEventMethodsPostEval
            Kind = mkProcedure
          end
          item
            Name = 'ToRawData'
            ResultType = 'String'
            OnEval = dwsWebServerClassesWebServerSentEventMethodsToRawDataEval
            Kind = mkFunction
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
