object dwsWebLib: TdwsWebLib
  OldCreateOrder = False
  Left = 646
  Top = 86
  Height = 150
  Width = 215
  object dwsWeb: TdwsUnit
    Classes = <
      item
        Name = 'WebRequest'
        IsStatic = True
        Methods = <
          item
            Name = 'GetHeader'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsHeaderEval
            Visibility = cvProtected
            Kind = mkClassFunction
          end
          item
            Name = 'GetCookie'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsCookieEval
            Visibility = cvProtected
            Kind = mkClassFunction
          end
          item
            Name = 'GetQueryField'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsQueryFieldEval
            Visibility = cvProtected
            Kind = mkClassFunction
          end
          item
            Name = 'URL'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsURLEval
            Kind = mkClassFunction
          end
          item
            Name = 'Method'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsMethodEval
            Kind = mkClassFunction
          end
          item
            Name = 'PathInfo'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsPathInfoEval
            Kind = mkClassFunction
          end
          item
            Name = 'QueryString'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsQueryStringEval
            Kind = mkClassFunction
          end
          item
            Name = 'RemoteIP'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsRemoteIPEval
            Kind = mkClassFunction
          end
          item
            Name = 'Headers'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsHeadersEval
            Kind = mkClassFunction
          end
          item
            Name = 'Cookies'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsCookiesEval
            Kind = mkClassFunction
          end
          item
            Name = 'QueryFields'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsQueryFieldsEval
            Kind = mkClassFunction
          end>
        Properties = <
          item
            Name = 'Header'
            DataType = 'String'
            ReadAccess = 'GetHeader'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            IsDefault = False
          end
          item
            Name = 'Cookie'
            DataType = 'String'
            ReadAccess = 'GetCookie'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            IsDefault = False
          end
          item
            Name = 'QueryField'
            DataType = 'String'
            ReadAccess = 'GetQueryField'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            IsDefault = False
          end>
      end
      item
        Name = 'WebResponse'
        IsStatic = True
        Methods = <
          item
            Name = 'SetStatusCode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'Integer'
              end>
            Attributes = [maStatic]
            Visibility = cvProtected
            Kind = mkClassProcedure
          end
          item
            Name = 'SetContentData'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsContentDataEval
            Visibility = cvProtected
            Kind = mkClassProcedure
          end
          item
            Name = 'SetContentType'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsContentTypeEval
            Visibility = cvProtected
            Kind = mkClassProcedure
          end
          item
            Name = 'SetContentEncoding'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsContentEncodingEval
            Visibility = cvProtected
            Kind = mkClassProcedure
          end
          item
            Name = 'SetHeader'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'value'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Visibility = cvProtected
            Kind = mkClassProcedure
          end
          item
            Name = 'SetContentText'
            Parameters = <
              item
                Name = 'textType'
                DataType = 'String'
              end
              item
                Name = 'text'
                DataType = 'String'
              end>
            OnEval = dwsWebClassesWebResponseMethodsSetContentTextEval
            Kind = mkClassProcedure
          end>
        Properties = <
          item
            Name = 'StatusCode'
            DataType = 'Integer'
            WriteAccess = 'SetStatusCode'
            IsDefault = False
          end
          item
            Name = 'ContentData'
            DataType = 'String'
            WriteAccess = 'SetContentData'
            IsDefault = False
          end
          item
            Name = 'ContentType'
            DataType = 'String'
            WriteAccess = 'SetContentType'
            IsDefault = False
          end
          item
            Name = 'ContentText'
            DataType = 'String'
            WriteAccess = 'SetContentText'
            Parameters = <
              item
                Name = 'textType'
                DataType = 'String'
              end>
            IsDefault = False
          end
          item
            Name = 'ContentEncoding'
            DataType = 'String'
            WriteAccess = 'SetContentEncoding'
            IsDefault = False
          end
          item
            Name = 'Header'
            DataType = 'String'
            WriteAccess = 'SetHeader'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            IsDefault = False
          end>
      end>
    UnitName = 'System.Net'
    StaticSymbols = False
    Left = 24
    Top = 16
  end
end
