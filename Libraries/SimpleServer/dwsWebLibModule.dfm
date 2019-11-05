object dwsWebLib: TdwsWebLib
  OldCreateOrder = False
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
            OnFastEvalString = dwsWebClassesWebRequestMethodsGetHeaderFastEvalString
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
            OnFastEvalString = dwsWebClassesWebRequestMethodsGetCookieFastEvalString
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
            OnFastEvalString = dwsWebClassesWebRequestMethodsGetQueryFieldFastEvalString
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
            OnFastEvalBoolean = dwsWebClassesWebRequestMethodsHasQueryFieldFastEvalBoolean
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
            OnFastEvalString = dwsWebClassesWebRequestMethodsGetContentFieldFastEvalString
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
            OnFastEvalBoolean = dwsWebClassesWebRequestMethodsHasContentFieldFastEvalBoolean
            Kind = mkClassFunction
          end
          item
            Name = 'FullURL'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsFullURLFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'URL'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsURLFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'RawURL'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsRawURLFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'Method'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsMethodFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'Host'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsHostFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'PathInfo'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsPathInfoFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'QueryString'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsQueryStringFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'RemoteIP'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsRemoteIPFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'Headers'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsHeadersFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'Cookies'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsCookiesFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'QueryFields'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsQueryFieldsFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'Security'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsSecurityFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'UserAgent'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsUserAgentFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'AuthenticatedUser'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsAuthenticatedUserFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'Authentication'
            ResultType = 'WebAuthentication'
            Attributes = [maStatic]
            OnFastEvalInteger = dwsWebClassesWebRequestMethodsAuthenticationFastEvalInteger
            Kind = mkClassFunction
          end
          item
            Name = 'ContentType'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsContentTypeFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'ContentData'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsContentDataFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'ContentLength'
            ResultType = 'Integer'
            Attributes = [maClassMethod, maStatic]
            OnFastEvalInteger = dwsWebClassesWebRequestMethodsContentLengthFastEvalInteger
            Kind = mkClassFunction
          end
          item
            Name = 'IfModifiedSince'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnFastEvalFloat = dwsWebClassesWebRequestMethodsIfModifiedSinceFastEvalFloat
            Kind = mkClassFunction
          end
          item
            Name = 'IfNoneMatch'
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsWebClassesWebRequestMethodsIfNoneMatchFastEvalString
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetStatusCodeFastEvalNoResult
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
            OnFastEvalString = dwsWebClassesWebResponseMethodsSetContentDataFastEvalString
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentTypeFastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentEncodingFastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetHeaderFastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentTextFastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentJSONFastEvalNoResult
            Kind = mkClassProcedure
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetContentFileFastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsRequestAuthenticationFastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetCookie_StringStringFloat_FastEvalNoResult
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
              end
              item
                Name = 'sameSite'
                DataType = 'WebCookieSameSite'
                HasDefaultValue = True
                DefaultValue = 0
              end>
            Overloaded = True
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetCookie_StringStringFloatStringStringIntegerWebCookieSameSite_FastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetCompressionFastEvalNoResult
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetLastModifiedFastEvalNoResult
            Kind = mkClassProcedure
          end
          item
            Name = 'SetStatic'
            Parameters = <
              item
                Name = 'v'
                DataType = 'Boolean'
              end>
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetStaticFastEvalNoResult
            Kind = mkClassProcedure
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
            OnFastEvalString = dwsWebClassesWebResponseMethodsSetContentEventStreamFastEvalString
            Kind = mkClassFunction
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
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetStatusPlainTextFastEvalNoResult
            Kind = mkClassProcedure
          end
          item
            Name = 'SetETag'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetETagFastEvalNoResult
            Kind = mkClassProcedure
          end
          item
            Name = 'SetCacheControl'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsWebClassesWebResponseMethodsSetCacheControlFastEvalNoResult
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
            OnEval = dwsWebClassesHttpQueryMethodsRequestEval
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
          end
          item
            Name = 'SetProxyName'
            Parameters = <
              item
                Name = 'val'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsSetProxyNameEval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetConnectTimeoutMSec'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Integer'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsSetConnectTimeoutMSecEval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetSendTimeoutMSec'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Integer'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsSetSendTimeoutMSecEval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetReceiveTimeoutMSec'
            Parameters = <
              item
                Name = 'val'
                DataType = 'Integer'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsSetReceiveTimeoutMSecEval
            Kind = mkClassProcedure
          end
          item
            Name = 'SetCustomHeaders'
            Parameters = <
              item
                Name = 'headers'
                DataType = 'array of String'
              end>
            Attributes = [maStatic]
            OnEval = dwsWebClassesHttpQueryMethodsSetCustomHeadersEval
            Kind = mkClassProcedure
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
          end>
      end
      item
        Name = 'HttpRequest'
        Methods = <
          item
            Name = 'Method'
            ResultType = 'String'
            OnEval = dwsWebClassesHttpRequestMethodsMethodEval
            Kind = mkFunction
          end
          item
            Name = 'URL'
            ResultType = 'String'
            OnEval = dwsWebClassesHttpRequestMethodsURLEval
            Kind = mkFunction
          end
          item
            Name = 'StatusCode'
            ResultType = 'Integer'
            OnEval = dwsWebClassesHttpRequestMethodsStatusCodeEval
            Kind = mkFunction
          end
          item
            Name = 'Headers'
            ResultType = 'String'
            OnEval = dwsWebClassesHttpRequestMethodsHeadersEval
            Kind = mkFunction
          end
          item
            Name = 'GetHeader'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            OnEval = dwsWebClassesHttpRequestMethodsGetHeaderEval
            Kind = mkFunction
          end
          item
            Name = 'ContentLength'
            ResultType = 'Integer'
            OnEval = dwsWebClassesHttpRequestMethodsContentLengthEval
            Kind = mkFunction
          end
          item
            Name = 'ContentType'
            ResultType = 'String'
            OnEval = dwsWebClassesHttpRequestMethodsContentTypeEval
            Kind = mkFunction
          end
          item
            Name = 'ContentData'
            ResultType = 'String'
            OnEval = dwsWebClassesHttpRequestMethodsContentDataEval
            Kind = mkFunction
          end
          item
            Name = 'Completed'
            ResultType = 'Boolean'
            OnEval = dwsWebClassesHttpRequestMethodsCompletedEval
            Kind = mkFunction
          end
          item
            Name = 'Error'
            ResultType = 'String'
            OnEval = dwsWebClassesHttpRequestMethodsErrorEval
            Kind = mkFunction
          end
          item
            Name = 'CurrentContentSize'
            ResultType = 'Integer'
            OnEval = dwsWebClassesHttpRequestMethodsCurrentContentSizeEval
            Kind = mkFunction
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
            OnEval = dwsWebClassesHttpRequestMethodsContentSubDataEval
            Kind = mkFunction
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
            OnEval = dwsWebClassesWebServerSentEventsMethodsPostRawEval
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
            OnEval = dwsWebClassesWebServerSentEventsMethodsCloseEval
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
            OnEval = dwsWebClassesWebServerSentEventsMethodsConnectionsEval
            Kind = mkClassFunction
          end
          item
            Name = 'SourceNames'
            ResultType = 'array of String'
            Attributes = [maStatic]
            OnEval = dwsWebClassesWebServerSentEventsMethodsSourceNamesEval
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
            OnEval = dwsWebClassesWebServerSentEventMethodsPostEval
            Kind = mkProcedure
          end
          item
            Name = 'ToRawData'
            ResultType = 'String'
            OnEval = dwsWebClassesWebServerSentEventMethodsToRawDataEval
            Kind = mkFunction
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
      end>
    UnitName = 'System.Net'
    StaticSymbols = True
    Left = 56
    Top = 16
  end
end
