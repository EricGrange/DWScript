object dwsWebLib: TdwsWebLib
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
            Visibility = cvProtected
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsGetHeaderFastEvalString
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
            Visibility = cvProtected
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsGetCookieFastEvalString
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
            Visibility = cvProtected
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsGetQueryFieldFastEvalString
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
            Kind = mkClassFunction
            OnFastEvalBoolean = dwsWebClassesWebRequestMethodsHasQueryFieldFastEvalBoolean
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
            Visibility = cvProtected
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsGetContentFieldFastEvalString
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
            Kind = mkClassFunction
            OnFastEvalBoolean = dwsWebClassesWebRequestMethodsHasContentFieldFastEvalBoolean
          end
          item
            Name = 'FullURL'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsFullURLFastEvalString
          end
          item
            Name = 'URL'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsURLFastEvalString
          end
          item
            Name = 'RawURL'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsRawURLFastEvalString
          end
          item
            Name = 'Method'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsMethodFastEvalString
          end
          item
            Name = 'Host'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsHostFastEvalString
          end
          item
            Name = 'PathInfo'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsPathInfoFastEvalString
          end
          item
            Name = 'QueryString'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsQueryStringFastEvalString
          end
          item
            Name = 'RemoteIP'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsRemoteIPFastEvalString
          end
          item
            Name = 'Headers'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsHeadersFastEvalString
          end
          item
            Name = 'Cookies'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsCookiesFastEvalString
          end
          item
            Name = 'QueryFields'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsQueryFieldsFastEvalString
          end
          item
            Name = 'Security'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsSecurityFastEvalString
          end
          item
            Name = 'UserAgent'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsUserAgentFastEvalString
          end
          item
            Name = 'AuthenticatedUser'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsAuthenticatedUserFastEvalString
          end
          item
            Name = 'Authentication'
            ResultType = 'WebAuthentication'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalInteger = dwsWebClassesWebRequestMethodsAuthenticationFastEvalInteger
          end
          item
            Name = 'ContentType'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsContentTypeFastEvalString
          end
          item
            Name = 'ContentData'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsContentDataFastEvalString
          end
          item
            Name = 'ContentLength'
            ResultType = 'Integer'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnFastEvalInteger = dwsWebClassesWebRequestMethodsContentLengthFastEvalInteger
          end
          item
            Name = 'IfModifiedSince'
            ResultType = 'Float'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalFloat = dwsWebClassesWebRequestMethodsIfModifiedSinceFastEvalFloat
          end
          item
            Name = 'IfNoneMatch'
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebRequestMethodsIfNoneMatchFastEvalString
          end
          item
            Name = 'ContentFieldNames'
            ResultType = 'array of String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebClassesWebRequestMethodsContentFieldsEval
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
            Visibility = cvProtected
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetStatusCodeFastEvalNoResult
          end
          item
            Name = 'SetContentData'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Visibility = cvProtected
            Kind = mkClassProcedure
            OnFastEvalString = dwsWebClassesWebResponseMethodsSetContentDataFastEvalString
          end
          item
            Name = 'SetContentType'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Visibility = cvProtected
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentTypeFastEvalNoResult
          end
          item
            Name = 'SetContentEncoding'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Visibility = cvProtected
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentEncodingFastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetHeaderFastEvalNoResult
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
            Visibility = cvProtected
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentTextFastEvalNoResult
          end
          item
            Name = 'SetContentJSON'
            Parameters = <
              item
                Name = 'j'
                DataType = 'JSONVariant'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentJSONFastEvalNoResult
          end
          item
            Name = 'SetContentFile'
            Parameters = <
              item
                Name = 'fileName'
                DataType = 'String'
              end
              item
                Name = 'contentType'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentFileFastEvalNoResult
          end
          item
            Name = 'RequestAuthentication'
            Parameters = <
              item
                Name = 'auth'
                DataType = 'WebAuthentication'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsRequestAuthenticationFastEvalNoResult
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
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetCookie_StringStringFloat_FastEvalNoResult
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
              end
              item
                Name = 'sameSite'
                DataType = 'WebCookieSameSite'
                HasDefaultValue = True
                DefaultValue = 0
              end>
            Overloaded = True
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetCookie_StringStringFloatStringStringIntegerWebCookieSameSite_FastEvalNoResult
          end
          item
            Name = 'SetCompression'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
              end>
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetCompressionFastEvalNoResult
          end
          item
            Name = 'SetLastModified'
            Parameters = <
              item
                Name = 'v'
                DataType = 'Float'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetLastModifiedFastEvalNoResult
          end
          item
            Name = 'SetStatic'
            Parameters = <
              item
                Name = 'v'
                DataType = 'Boolean'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetStaticFastEvalNoResult
          end
          item
            Name = 'SetContentEventStream'
            Parameters = <
              item
                Name = 'sourceName'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsWebClassesWebResponseMethodsSetContentEventStreamFastEvalString
          end
          item
            Name = 'SetStatusPlainText'
            Parameters = <
              item
                Name = 'status'
                DataType = 'Integer'
              end
              item
                Name = 'text'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetStatusPlainTextFastEvalNoResult
          end
          item
            Name = 'SetStatusJSON'
            Parameters = <
              item
                Name = 'status'
                DataType = 'Integer'
              end
              item
                Name = 'utf16json'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEval = dwsWebClassesWebResponseMethodsSetStatusJSONFastEval
          end
          item
            Name = 'SetStatusRedirect'
            Parameters = <
              item
                Name = 'status'
                DataType = 'Integer'
              end
              item
                Name = 'location'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEval = dwsWebClassesWebResponseMethodsSetStatusRedirectFastEval
          end
          item
            Name = 'SetETag'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetETagFastEvalNoResult
          end
          item
            Name = 'SetCacheControl'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetCacheControlFastEvalNoResult
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
          end
          item
            Name = 'Static'
            DataType = 'Boolean'
            WriteAccess = 'SetStatic'
          end
          item
            Name = 'ETag'
            DataType = 'String'
            WriteAccess = 'SetETag'
          end
          item
            Name = 'CacheControl'
            DataType = 'String'
            WriteAccess = 'SetCacheControl'
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
            Kind = mkClassFunction
            OnEval = dwsWebClassesHttpQueryMethodsGetTextEval
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
            Kind = mkClassFunction
            OnEval = dwsWebClassesHttpQueryMethodsGetDataEval
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
            Kind = mkClassFunction
            OnEval = dwsWebClassesHttpQueryMethodsPostDataEval
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
            Kind = mkClassFunction
            OnEval = dwsWebClassesHttpQueryMethodsPutDataEval
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
            Kind = mkClassFunction
            OnEval = dwsWebClassesHttpQueryMethodsDeleteEval
          end
          item
            Name = 'Request'
            Parameters = <
              item
                Name = 'method'
                DataType = 'String'
              end
              item
                Name = 'url'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end
              item
                Name = 'dataType'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            ResultType = 'HttpRequest'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebClassesHttpQueryMethodsRequestEval
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
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetCredentialsEval
          end
          item
            Name = 'ClearCredentials'
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsClearCredentialsEval
          end
          item
            Name = 'SetIgnoreSSLCertificateErrors'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Boolean'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetIgnoreSSLCertificateErrorsEval
          end
          item
            Name = 'GetIgnoreSSLCertificateErrors'
            ResultType = 'Boolean'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebClassesHttpQueryMethodsGetIgnoreSSLCertificateErrorsEval
          end
          item
            Name = 'SetProxyName'
            Parameters = <
              item
                Name = 'val'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetProxyNameEval
          end
          item
            Name = 'SetConnectTimeoutMSec'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Integer'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetConnectTimeoutMSecEval
          end
          item
            Name = 'SetSendTimeoutMSec'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Integer'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetSendTimeoutMSecEval
          end
          item
            Name = 'SetReceiveTimeoutMSec'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Integer'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetReceiveTimeoutMSecEval
          end
          item
            Name = 'SetKeepAlive'
            Parameters = <
              item
                Name = 'keepAlive'
                DataType = 'Boolean'
              end>
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetKeepAliveEval
          end
          item
            Name = 'SetCustomHeaders'
            Parameters = <
              item
                Name = 'headers'
                DataType = 'array of String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetCustomHeadersEval
          end
          item
            Name = 'GetKeepAlive'
            ResultType = 'Boolean'
            Kind = mkClassFunction
            OnEval = dwsWebClassesHttpQueryMethodsGetKeepAliveEval
          end
          item
            Name = 'SetSynchronousRequests'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Boolean'
              end>
            Attributes = [maClassMethod, maStatic]
            Visibility = cvPrivate
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetSynchronousRequestEval
          end
          item
            Name = 'SetDisableRedirects'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Boolean'
              end>
            Attributes = [maClassMethod, maStatic]
            Visibility = cvPrivate
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetDisableRedirectsEval
          end
          item
            Name = 'SetConnectionPool'
            Parameters = <
              item
                Name = 'poolName'
                DataType = 'String'
              end>
            Attributes = [maClassMethod, maStatic]
            Visibility = cvPrivate
            Kind = mkClassProcedure
            OnEval = dwsWebClassesHttpQueryMethodsSetConnectionPoolEval
          end>
        Properties = <
          item
            Name = 'IgnoreSSLCertificateErrors'
            DataType = 'Boolean'
            ReadAccess = 'GetIgnoreSSLCertificateErrors'
            WriteAccess = 'SetIgnoreSSLCertificateErrors'
          end
          item
            Name = 'ConnectTimeoutMSec'
            DataType = 'Integer'
            WriteAccess = 'SetConnectTimeoutMSec'
          end
          item
            Name = 'KeepAlive'
            DataType = 'Boolean'
            ReadAccess = 'GetKeepAlive'
            WriteAccess = 'SetKeepAlive'
          end
          item
            Name = 'ConnectionPool'
            DataType = 'String'
            WriteAccess = 'SetConnectionPool'
          end
          item
            Name = 'SynchronousRequests'
            DataType = 'Boolean'
            WriteAccess = 'SetSynchronousRequests'
          end
          item
            Name = 'DisableRedirects'
            DataType = 'Boolean'
            WriteAccess = 'SetDisableRedirects'
          end>
      end
      item
        Name = 'HttpRequest'
        Methods = <
          item
            Name = 'Method'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsMethodEval
          end
          item
            Name = 'URL'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsURLEval
          end
          item
            Name = 'StatusCode'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsStatusCodeEval
          end
          item
            Name = 'Headers'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsHeadersEval
          end
          item
            Name = 'GetHeader'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsGetHeaderEval
          end
          item
            Name = 'ContentLength'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsContentLengthEval
          end
          item
            Name = 'ContentType'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsContentTypeEval
          end
          item
            Name = 'ContentData'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsContentDataEval
          end
          item
            Name = 'Completed'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsCompletedEval
          end
          item
            Name = 'Error'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsErrorEval
          end
          item
            Name = 'CurrentContentSize'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsCurrentContentSizeEval
          end
          item
            Name = 'ContentSubData'
            Parameters = <
              item
                Name = 'offset'
                DataType = 'Integer'
              end
              item
                Name = 'length'
                DataType = 'Integer'
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsContentSubDataEval
          end
          item
            Name = 'GetCertificateInfo'
            ResultType = 'HttpCertificateInfo'
            Kind = mkFunction
            OnEval = dwsWebClassesHttpRequestMethodsGetCertificateInfoEval
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
          end>
        OnCleanUp = dwsWebClassesHttpRequestCleanUp
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
                Name = 'data'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesWebServerSentEventsMethodsPostRawEval
          end
          item
            Name = 'Close'
            Parameters = <
              item
                Name = 'sourceName'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsWebClassesWebServerSentEventsMethodsCloseEval
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
            Kind = mkClassFunction
            OnEval = dwsWebClassesWebServerSentEventsMethodsConnectionsEval
          end
          item
            Name = 'SourceNames'
            ResultType = 'array of String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsWebClassesWebServerSentEventsMethodsSourceNamesEval
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
            Kind = mkProcedure
            OnEval = dwsWebClassesWebServerSentEventMethodsPostEval
          end
          item
            Name = 'ToRawData'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsWebClassesWebServerSentEventMethodsToRawDataEval
          end>
      end
      item
        Name = 'HttpCertificateInfo'
        Fields = <
          item
            Name = 'FExpiry'
            DataType = 'Integer'
            Visibility = cvProtected
          end
          item
            Name = 'FStart'
            DataType = 'Integer'
            Visibility = cvProtected
          end
          item
            Name = 'FSubjectInfo'
            DataType = 'String'
            Visibility = cvProtected
          end
          item
            Name = 'FIssuerInfo'
            DataType = 'String'
            Visibility = cvProtected
          end
          item
            Name = 'FKeySize'
            DataType = 'Integer'
            Visibility = cvProtected
          end>
        Properties = <
          item
            Name = 'ExpiryUnixTime'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'FExpiry'
          end
          item
            Name = 'StartUnixTime'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'FStart'
          end
          item
            Name = 'SubjectInfo'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'FSubjectInfo'
          end
          item
            Name = 'IssuerInfo'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'FIssuerInfo'
          end
          item
            Name = 'KeySize'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'FKeySize'
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
      end
      item
        Name = 'WebCookieSameSite'
        Elements = <
          item
            Name = 'Unspecified'
          end
          item
            Name = 'Strict'
          end
          item
            Name = 'Lax'
          end>
        Style = enumScoped
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
      end
      item
        Name = 'GetHostByName'
        Parameters = <
          item
            Name = 'host'
            DataType = 'String'
          end>
        ResultType = 'String'
        OnEval = dwsWebFunctionsGetHostByNameEval
      end
      item
        Name = 'PingIPv4'
        Parameters = <
          item
            Name = 'hostName'
            DataType = 'String'
          end
          item
            Name = 'timeOutMSec'
            DataType = 'Integer'
          end>
        ResultType = 'Integer'
        OnFastEval = dwsWebFunctionsPingIPv4FastEval
      end
      item
        Name = 'PingIPv6'
        Parameters = <
          item
            Name = 'hostName'
            DataType = 'String'
          end
          item
            Name = 'timeOutMSec'
            DataType = 'Integer'
          end>
        ResultType = 'Integer'
        OnFastEval = dwsWebFunctionsPingIPv6FastEval
      end>
    UnitName = 'System.Net'
    StaticSymbols = True
    Left = 56
    Top = 16
  end
end
