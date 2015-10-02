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
            Name = 'HasQueryField'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsHasQueryFieldEval
            Kind = mkClassFunction
          end
          item
            Name = 'GetContentField'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsGetContentFieldEval
            Visibility = cvProtected
            Kind = mkClassFunction
          end
          item
            Name = 'HasContentField'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsHasContentFieldEval
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
          end
          item
            Name = 'Security'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsSecureEval
            Kind = mkClassFunction
          end
          item
            Name = 'UserAgent'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsUserAgentEval
            Kind = mkClassFunction
          end
          item
            Name = 'AuthenticatedUser'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsAuthenticatedUserEval
            Kind = mkClassFunction
          end
          item
            Name = 'Authentication'
            ResultType = 'WebAuthentication'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsAuthenticationEval
            Kind = mkClassFunction
          end
          item
            Name = 'ContentType'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebClassesWebRequestMethodsContentTypeEval
            Kind = mkClassFunction
          end
          item
            Name = 'ContentData'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebClassesWebRequestMethodsContentDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'ContentLength'
            ResultType = 'Integer'
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebClassesWebRequestMethodsContentLengthEval
            Kind = mkClassFunction
          end
          item
            Name = 'IfModifiedSince'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebRequestMethodsIfModifiedSinceEval
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
          end
          item
            Name = 'ContentField'
            DataType = 'String'
            ReadAccess = 'GetContentField'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
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
            OnEval = dwsWebClassesWebResponseMethodsSetStatusCodeEval
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
            OnEval = dwsWebClassesWebResponseMethodsSetHeaderEval
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
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsSetContentTextEval
            Visibility = cvProtected
            Kind = mkClassProcedure
          end
          item
            Name = 'SetContentJSON'
            Parameters = <
              item
                Name = 'j'
                DataType = 'JSONVariant'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsSetContentJSONEval
            Kind = mkClassProcedure
          end
          item
            Name = 'RequestAuthentication'
            Parameters = <
              item
                Name = 'auth'
                DataType = 'WebAuthentication'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsRequestAuthenticationEval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetCookie'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'value'
                DataType = 'String'
              end
              item
                Name = 'expiresGMT'
                DataType = 'Float'
                HasDefaultValue = True
                DefaultValue = '0'
              end>
            Overloaded = True
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsSetCookieEval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetCookie'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'value'
                DataType = 'String'
              end
              item
                Name = 'expiresGMT'
                DataType = 'Float'
              end
              item
                Name = 'path'
                DataType = 'String'
              end
              item
                Name = 'domain'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end
              item
                Name = 'flags'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 0
              end>
            Overloaded = True
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsSetCookie2Eval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetCompression'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
              end>
            Attributes = [maClassMethod, maStatic]
            OnEval = dwsWebClassesWebResponseMethodsSetCompressionEval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetLastModified'
            Parameters = <
              item
                Name = 'v'
                DataType = 'Float'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebResponseMethodsSetLastModifiedEval
            Kind = mkClassProcedure
          end>
        Properties = <
          item
            Name = 'StatusCode'
            DataType = 'Integer'
            WriteAccess = 'SetStatusCode'
          end
          item
            Name = 'ContentData'
            DataType = 'String'
            WriteAccess = 'SetContentData'
          end
          item
            Name = 'ContentType'
            DataType = 'String'
            WriteAccess = 'SetContentType'
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
          end
          item
            Name = 'ContentEncoding'
            DataType = 'String'
            WriteAccess = 'SetContentEncoding'
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
          end
          item
            Name = 'Compression'
            DataType = 'Boolean'
            WriteAccess = 'SetCompression'
          end
          item
            Name = 'LastModified'
            DataType = 'Float'
            WriteAccess = 'SetLastModified'
          end>
      end
      item
        Name = 'HttpQuery'
        IsStatic = True
        Methods = <
          item
            Name = 'GetText'
            Parameters = <
              item
                Name = 'url'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
                IsVarParam = True
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsGetTextEval
            Kind = mkClassFunction
          end
          item
            Name = 'GetData'
            Parameters = <
              item
                Name = 'url'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
                IsVarParam = True
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsGetDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'PostData'
            Parameters = <
              item
                Name = 'url'
                DataType = 'String'
              end
              item
                Name = 'requestData'
                DataType = 'String'
              end
              item
                Name = 'requestContentType'
                DataType = 'String'
              end
              item
                Name = 'replyData'
                DataType = 'String'
                IsVarParam = True
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsPostDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'PutData'
            Parameters = <
              item
                Name = 'url'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'dataType'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsPutDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'Delete'
            Parameters = <
              item
                Name = 'url'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsDeleteEval
            Kind = mkClassFunction
          end
          item
            Name = 'SetCredentials'
            Parameters = <
              item
                Name = 'authScheme'
                DataType = 'WebAuthentication'
              end
              item
                Name = 'userName'
                DataType = 'String'
              end
              item
                Name = 'password'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsSetCredentialsEval
            Kind = mkClassProcedure
          end
          item
            Name = 'ClearCredentials'
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsClearCredentialsEval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetIgnoreSSLCertificateErrors'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Boolean'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsSetIgnoreSSLCertificateErrorsEval
            Kind = mkClassProcedure
          end
          item
            Name = 'GetIgnoreSSLCertificateErrors'
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsGetIgnoreSSLCertificateErrorsEval
            Kind = mkClassFunction
          end>
        Properties = <
          item
            Name = 'IgnoreSSLCertificateErrors'
            DataType = 'Boolean'
            ReadAccess = 'GetIgnoreSSLCertificateErrors'
            WriteAccess = 'SetIgnoreSSLCertificateErrors'
          end>
      end>
    Enumerations = <
      item
        Name = 'WebAuthentication'
        Elements = <
          item
            Name = 'None'
          end
          item
            Name = 'Failed'
          end
          item
            Name = 'Basic'
          end
          item
            Name = 'Digest'
          end
          item
            Name = 'NTLM'
          end
          item
            Name = 'Negotiate'
          end
          item
            Name = 'Kerberos'
          end
          item
            Name = 'Authorization'
          end>
        Style = enumScoped
      end
      item
        Name = 'WebCookieFlag'
        Elements = <
          item
            Name = 'Secure'
          end
          item
            Name = 'HttpOnly'
          end>
        Style = enumFlags
      end>
    Sets = <
      item
        Name = 'WebAuthentications'
        BaseType = 'WebAuthentication'
      end>
    Functions = <
      item
        Name = 'DeflateCompress'
        Parameters = <
          item
            Name = 'data'
            DataType = 'String'
          end
          item
            Name = 'level'
            DataType = 'Integer'
            HasDefaultValue = True
            DefaultValue = '1'
          end>
        ResultType = 'String'
        OnFastEval = dwsWebFunctionsDeflateCompressFastEval
      end
      item
        Name = 'DeflateDecompress'
        Parameters = <
          item
            Name = 'data'
            DataType = 'String'
          end>
        ResultType = 'String'
        OnFastEval = dwsWebFunctionsDeflateDecompressionFastEval
      end
      item
        Name = 'GetHostByAddr'
        Parameters = <
          item
            Name = 'addr'
            DataType = 'String'
          end>
        ResultType = 'String'
        OnEval = dwsWebFunctionsGetHostByAddrEval
      end>
    UnitName = 'System.Net'
    StaticSymbols = True
    Left = 56
    Top = 16
  end
end
