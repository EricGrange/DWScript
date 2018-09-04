{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{
  HTTP.sys API definitions

  This file is based on Synopse framework and is an attempt
  at supporting HTTP.SYS 2.0

  Synopse framework. Copyright (C) 2012 Arnaud Bouchez
    Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL
}
unit dwsHTTPSysAPI;

interface

uses
   Windows, SysUtils,
   SynWinSock;

{$MINENUMSIZE 4}
{$A8}

type
   ULONGLONG = Int64;
   HTTP_OPAQUE_ID = ULONGLONG;
   HTTP_URL_CONTEXT = HTTP_OPAQUE_ID;
   HTTP_REQUEST_ID = HTTP_OPAQUE_ID;
   HTTP_CONNECTION_ID = HTTP_OPAQUE_ID;
   HTTP_RAW_CONNECTION_ID = HTTP_OPAQUE_ID;

   HTTP_URL_GROUP_ID = HTTP_OPAQUE_ID;
   HTTP_SERVER_SESSION_ID = HTTP_OPAQUE_ID;

   // HTTP API version used
   HTTPAPI_VERSION = packed record
      MajorVersion : word;
      MinorVersion : word;
   end;

   // the req* values identify Request Headers, and resp* Response Headers
   THttpHeader = (
      reqCacheControl,
      reqConnection,
      reqDate,
      reqKeepAlive,
      reqPragma,
      reqTrailer,
      reqTransferEncoding,
      reqUpgrade,
      reqVia,
      reqWarning,
      reqAllow,
      reqContentLength,
      reqContentType,
      reqContentEncoding,
      reqContentLanguage,
      reqContentLocation,
      reqContentMd5,
      reqContentRange,
      reqExpires,
      reqLastModified,
      reqAccept,
      reqAcceptCharset,
      reqAcceptEncoding,
      reqAcceptLanguage,
      reqAuthorization,
      reqCookie,
      reqExpect,
      reqFrom,
      reqHost,
      reqIfMatch,
      reqIfModifiedSince,
      reqIfNoneMatch,
      reqIfRange,
      reqIfUnmodifiedSince,
      reqMaxForwards,
      reqProxyAuthorization,
      reqReferer,
      reqRange,
      reqTe,
      reqTranslate,
      reqUserAgent,
      respAcceptRanges = 20,
      respAge,
      respEtag,
      respLocation,
      respProxyAuthenticate,
      respRetryAfter,
      respServer,
      respSetCookie,
      respVary,
      respWwwAuthenticate
      );

   THttpVerb = (
      hvUnparsed,
      hvUnknown,
      hvInvalid,
      hvOPTIONS,
      hvGET,
      hvHEAD,
      hvPOST,
      hvPUT,
      hvDELETE,
      hvTRACE,
      hvCONNECT,
      hvTRACK,  // used by Microsoft Cluster Server for a non-logged trace
      hvMOVE,
      hvCOPY,
      hvPROPFIND,
      hvPROPPATCH,
      hvMKCOL,
      hvLOCK,
      hvUNLOCK,
      hvSEARCH,
      hvMaximum
      );

   THttpChunkType = (
      hctFromMemory,
      hctFromFileHandle,
      hctFromFragmentCache);

   THttpServiceConfigID = (
      hscIPListenList,
      hscSSLCertInfo,
      hscUrlAclInfo,
      hscMax
      );
   THttpServiceConfigQueryType = (
      hscQueryExact,
      hscQueryNext,
      hscQueryMax
      );

   // Pointers overlap and point into pFullUrl. nil if not present.
   HTTP_COOKED_URL = record
      FullUrlLength : word;     // in bytes not including the #0
      HostLength : word;        // in bytes not including the #0
      AbsPathLength : word;     // in bytes not including the #0
      QueryStringLength : word; // in bytes not including the #0
      pFullUrl : PWideChar;     // points to "http://hostname:port/abs/.../path?query"
      pHost : PWideChar;        // points to the first char in the hostname
      pAbsPath : PWideChar;     // Points to the 3rd '/' char
      pQueryString : PWideChar; // Points to the 1st '?' char or #0
   end;

   HTTP_TRANSPORT_ADDRESS = record
      pRemoteAddress : PSOCKADDR;
      pLocalAddress : PSOCKADDR;
   end;

   HTTP_UNKNOWN_HEADER = record
      NameLength : word;          // in bytes not including the #0
      RawValueLength : word;      // in bytes not including the n#0
      pName : PAnsiChar;          // The header name (minus the ':' character)
      pRawValue : PAnsiChar;      // The header value
   end;
   PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;

   HTTP_UNKNOWN_HEADER_ARRAY = array of HTTP_UNKNOWN_HEADER;

   HTTP_KNOWN_HEADER = record
      RawValueLength : word;     // in bytes not including the #0
      pRawValue : PAnsiChar;
   end;
   PHTTP_KNOWN_HEADER = ^HTTP_KNOWN_HEADER;

   HTTP_RESPONSE_HEADERS = record
      // number of entries in the unknown HTTP headers array
      UnknownHeaderCount : word;
      // array of unknown HTTP headers
      pUnknownHeaders : PHTTP_UNKNOWN_HEADER;
      // Reserved, must be 0
      TrailerCount : word;
      // Reserved, must be nil
      pTrailers : pointer;
      // Known headers
      KnownHeaders : array[low(THttpHeader)..respWwwAuthenticate] of HTTP_KNOWN_HEADER;
   end;

   HTTP_REQUEST_HEADERS = record
      // number of entries in the unknown HTTP headers array
      UnknownHeaderCount : word;
      // array of unknown HTTP headers
      pUnknownHeaders : PHTTP_UNKNOWN_HEADER;
      // Reserved, must be 0
      TrailerCount : word;
      // Reserved, must be nil
      pTrailers : pointer;
      // Known headers
      KnownHeaders : array[low(THttpHeader)..reqUserAgent] of HTTP_KNOWN_HEADER;
   end;
   PHTTP_REQUEST_HEADERS = ^HTTP_REQUEST_HEADERS;

   HTTP_BYTE_RANGE = record
      StartingOffset : ULARGE_INTEGER;
      Length : ULARGE_INTEGER;
   end;

   // we use 3 distinct HTTP_DATA_CHUNK_* records since variable records
   // alignment is buggy/non compatible under Delphi XE3
   HTTP_DATA_CHUNK_INMEMORY = record
      DataChunkType : THttpChunkType; // always hctFromMemory
      Reserved1 : ULONG;
      pBuffer : pointer;
      BufferLength : ULONG;
      Reserved2 : ULONG;
      Reserved3 : ULONG;
   end;
   PHTTP_DATA_CHUNK_INMEMORY = ^HTTP_DATA_CHUNK_INMEMORY;

   HTTP_DATA_CHUNK_FILEHANDLE = record
      DataChunkType : THttpChunkType; // always hctFromFileHandle
      ByteRange : HTTP_BYTE_RANGE;
      FileHandle : THandle;
   end;

   HTTP_DATA_CHUNK_FRAGMENTCACHE = record
      DataChunkType : THttpChunkType; // always hctFromFragmentCache
      FragmentNameLength : word;      // in bytes not including the #0
      pFragmentName : PWideChar;
   end;

   HTTP_SSL_CLIENT_CERT_INFO = record
      CertFlags : ULONG;
      CertEncodedSize : ULONG;
      pCertEncoded : PUCHAR;
      Token : THandle;
      CertDeniedByMapper : boolean;
   end;
   PHTTP_SSL_CLIENT_CERT_INFO = ^HTTP_SSL_CLIENT_CERT_INFO;

   HTTP_SSL_INFO = record
      ServerCertKeySize : word;
      ConnectionKeySize : word;
      ServerCertIssuerSize : ULONG;
      ServerCertSubjectSize : ULONG;
      pServerCertIssuer : PAnsiChar;
      pServerCertSubject : PAnsiChar;
      pClientCertInfo : PHTTP_SSL_CLIENT_CERT_INFO;
      SslClientCertNegotiated : ULONG;
   end;
   PHTTP_SSL_INFO = ^HTTP_SSL_INFO;

   HTTP_SERVICE_CONFIG_URLACL_KEY = record
      pUrlPrefix : PWideChar;
   end;

   HTTP_SERVICE_CONFIG_URLACL_PARAM = record
      pStringSecurityDescriptor : PWideChar;
   end;

   HTTP_SERVICE_CONFIG_URLACL_SET = record
      KeyDesc : HTTP_SERVICE_CONFIG_URLACL_KEY;
      ParamDesc : HTTP_SERVICE_CONFIG_URLACL_PARAM;
   end;

   HTTP_SERVICE_CONFIG_URLACL_QUERY = record
      QueryDesc : THttpServiceConfigQueryType;
      KeyDesc : HTTP_SERVICE_CONFIG_URLACL_KEY;
      dwToken : DWORD;
   end;

   HTTP_REQUEST_INFO_TYPE = (
      HttpRequestInfoTypeAuth,
      HttpRequestInfoTypeChannelBind,
      HttpRequestInfoTypeSslProtocol,
      HttpRequestInfoTypeSslTokenBindingDraft,
      HttpRequestInfoTypeSslTokenBinding
   );

   HTTP_AUTH_STATUS = (
      HttpAuthStatusSuccess,
      HttpAuthStatusNotAuthenticated,
      HttpAuthStatusFailure
      );

   HTTP_REQUEST_AUTH_TYPE = (
      HttpRequestAuthTypeNone,
      HttpRequestAuthTypeBasic,
      HttpRequestAuthTypeDigest,
      HttpRequestAuthTypeNTLM,
      HttpRequestAuthTypeNegotiate,
      HttpRequestAuthTypeKerberos
      );

   SECURITY_STATUS = ULONG;

   HTTP_REQUEST_AUTH_INFO = record
      AuthStatus : HTTP_AUTH_STATUS;
      SecStatus : SECURITY_STATUS;
      Flags : ULONG;
      AuthType : HTTP_REQUEST_AUTH_TYPE;
      AccessToken : THandle;
      ContextAttributes : ULONG;
      PackedContextLength : ULONG;
      PackedContextType : ULONG;
      PackedContext : PVOID;
      MutualAuthDataLength : ULONG;
      pMutualAuthData : PCHAR;
   end;
   PHTTP_REQUEST_AUTH_INFO = ^HTTP_REQUEST_AUTH_INFO;

   HTTP_REQUEST_INFO = record
      InfoType : HTTP_REQUEST_INFO_TYPE;
      InfoLength : ULONG;
      pInfo : PVOID;
   end;
   PHTTP_REQUEST_INFO = ^HTTP_REQUEST_INFO;

   /// structure used to handle data associated with a specific request
   HTTP_REQUEST_V2 = record
      // either 0 (Only Header), either HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY
      Flags : ULONG;
      // An identifier for the connection on which the request was received
      ConnectionId : HTTP_CONNECTION_ID;
      // A value used to identify the request when calling
      // HttpReceiveRequestEntityBody, HttpSendHttpResponse, and/or
      // HttpSendResponseEntityBody
      RequestId : HTTP_REQUEST_ID;
      // The context associated with the URL prefix
      UrlContext : HTTP_URL_CONTEXT;
      // The HTTP version number
      Version : HTTPAPI_VERSION;
      // An HTTP verb associated with this request
      Verb : THttpVerb;
      // The length of the verb string if the Verb field is hvUnknown
      // (in bytes not including the last #0)
      UnknownVerbLength : USHORT;
      // The length of the raw (uncooked) URL (in bytes not including the last #0)
      RawUrlLength : USHORT;
      // Pointer to the verb string if the Verb field is hvUnknown
      pUnknownVerb : PAnsiChar;
      // Pointer to the raw (uncooked) URL
      pRawUrl : PAnsiChar;
      // The canonicalized Unicode URL
      CookedUrl : HTTP_COOKED_URL;
      // Local and remote transport addresses for the connection
      Address : HTTP_TRANSPORT_ADDRESS;
      // The request headers.
      Headers : HTTP_REQUEST_HEADERS;
      // The total number of bytes received from network for this request
      BytesReceived : ULONGLONG;
      EntityChunkCount : USHORT;
      pEntityChunks : Pointer;
      RawConnectionId : HTTP_RAW_CONNECTION_ID;
      // SSL connection information
      pSslInfo : PHTTP_SSL_INFO;
      // V2 new fields
      {$ifndef WIN64}
      xxxPadding : DWORD;
      {$endif}
      RequestInfoCount : USHORT;
      pRequestInfo : PHTTP_REQUEST_INFO;
   end;
   PHTTP_REQUEST_V2 = ^HTTP_REQUEST_V2;

   HTTP_RESPONSE_INFO_TYPE = (
      HttpResponseInfoTypeMultipleKnownHeaders,
      HttpResponseInfoTypeAuthenticationProperty,
      HttpResponseInfoTypeQosProperty,
      HttpResponseInfoTypeChannelBind
      );

   HTTP_RESPONSE_INFO = record
      Typ : HTTP_RESPONSE_INFO_TYPE;
      Length : ULONG;
      pInfo : Pointer;
   end;
   PHTTP_RESPONSE_INFO = ^HTTP_RESPONSE_INFO;

   HTTP_RESPONSE_V2 = object
   public
      Flags : cardinal;
      // The raw HTTP protocol version number
      Version : HTTPAPI_VERSION;
      // The HTTP status code (e.g., 200)
      StatusCode : word;
      // in bytes not including the '\0'
      ReasonLength : word;
      // The HTTP reason (e.g., "OK"). This MUST not contain non-ASCII characters
      // (i.e., all chars must be in range 0x20-0x7E).
      pReason : PAnsiChar;
      // The response headers
      Headers : HTTP_RESPONSE_HEADERS;
      // number of elements in pEntityChunks[] array
      EntityChunkCount : word;
      // pEntityChunks points to an array of EntityChunkCount HTTP_DATA_CHUNK_*
      pEntityChunks : pointer;
      // V2 new fields
      ResponseInfoCount : USHORT;
      pResponseInfo : PHTTP_RESPONSE_INFO;

      // will set both StatusCode and Reason
      // - OutStatus is a temporary variable
      // - if DataChunkForErrorContent is set, it will be used to add a content
      // body in the response with the textual representation of the error code
      procedure SetStatus(code : integer; var outStatus : RawByteString);
      procedure SetErrorStatus(code : integer; var outStatus : RawByteString;
                               dataChunkForErrorContent : PHTTP_DATA_CHUNK_INMEMORY;
                               const errorMsg : String);
      // will set the content of the reponse, and ContentType header
      procedure SetContent(var DataChunk : HTTP_DATA_CHUNK_INMEMORY;
         const Content : RawByteString; const ContentType : RawByteString = 'text/html');
      /// will set all header values from lines
      // - Content-Type/Content-Encoding/Location will be set in KnownHeaders[]
      // - all other headers will be set in temp UnknownHeaders[0..MaxUnknownHeader]
      procedure SetHeaders(P : PAnsiChar; const UnknownHeaders : HTTP_UNKNOWN_HEADER_ARRAY);
   end;
   PHTTP_RESPONSE_V2 = ^HTTP_RESPONSE_V2;

   HTTP_PROPERTY_FLAGS = ULONG;

   HTTP_ENABLED_STATE = (
      HttpEnabledStateActive,
      HttpEnabledStateInactive
      );
   PHTTP_ENABLED_STATE = ^HTTP_ENABLED_STATE;

   HTTP_STATE_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      State : HTTP_ENABLED_STATE;
   end;
   PHTTP_STATE_INFO = ^HTTP_STATE_INFO;

   THTTP_503_RESPONSE_VERBOSITY = (
      Http503ResponseVerbosityBasic,
      Http503ResponseVerbosityLimited,
      Http503ResponseVerbosityFull
      );
   PHTTP_503_RESPONSE_VERBOSITY = ^ THTTP_503_RESPONSE_VERBOSITY;

   HTTP_QOS_SETTING_TYPE = (
      HttpQosSettingTypeBandwidth,
      HttpQosSettingTypeConnectionLimit,
      HttpQosSettingTypeFlowRate // Windows Server 2008 R2 and Windows 7 only.
      );
   PHTTP_QOS_SETTING_TYPE = ^HTTP_QOS_SETTING_TYPE;

   HTTP_QOS_SETTING_INFO = record
      QosType : HTTP_QOS_SETTING_TYPE;
      QosSetting : Pointer;
   end;
   PHTTP_QOS_SETTING_INFO = ^HTTP_QOS_SETTING_INFO;

   HTTP_CONNECTION_LIMIT_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      MaxConnections : ULONG;
   end;
   PHTTP_CONNECTION_LIMIT_INFO = ^HTTP_CONNECTION_LIMIT_INFO;

   HTTP_BANDWIDTH_LIMIT_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      MaxBandwidth : ULONG;
   end;
   PHTTP_BANDWIDTH_LIMIT_INFO = ^HTTP_BANDWIDTH_LIMIT_INFO;

const
   HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE {:ULONG} = 1024;
   HTTP_LIMIT_INFINITE {:ULONG} = ULONG(-1);

type
   HTTP_SERVICE_CONFIG_TIMEOUT_KEY = (
      IdleConnectionTimeout = 0,
      HeaderWaitTimeout
      );
   PHTTP_SERVICE_CONFIG_TIMEOUT_KEY = ^HTTP_SERVICE_CONFIG_TIMEOUT_KEY;

   HTTP_SERVICE_CONFIG_TIMEOUT_PARAM = USHORT;
   PHTTP_SERVICE_CONFIG_TIMEOUT_PARAM = ^HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;

   HTTP_SERVICE_CONFIG_TIMEOUT_SET = record
      KeyDesc : HTTP_SERVICE_CONFIG_TIMEOUT_KEY;
      ParamDesc : HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;
   end;
   PHTTP_SERVICE_CONFIG_TIMEOUT_SET = ^HTTP_SERVICE_CONFIG_TIMEOUT_SET;

   HTTP_TIMEOUT_LIMIT_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      EntityBody : USHORT;
      DrainEntityBody : USHORT;
      RequestQueue : USHORT;
      IdleConnection : USHORT;
      HeaderWait : USHORT;
      MinSendRate : USHORT;
   end;
   PHTTP_TIMEOUT_LIMIT_INFO = ^HTTP_TIMEOUT_LIMIT_INFO;

   HTTP_LISTEN_ENDPOINT_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      EnableSharing : BOOLEAN;
   end;
   PHTTP_LISTEN_ENDPOINT_INFO = ^HTTP_LISTEN_ENDPOINT_INFO;

   HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = record
      DomainNameLength : USHORT;
      DomainName : PWideChar;
      RealmLength : USHORT;
      Realm : PWideChar;
   end;
   PHTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = ^HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;

   HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = record
      RealmLength : USHORT;
      Realm : PWideChar;
   end;
   PHTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = ^HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;

const
   HTTP_AUTH_ENABLE_BASIC        = $00000001;
   HTTP_AUTH_ENABLE_DIGEST       = $00000002;
   HTTP_AUTH_ENABLE_NTLM         = $00000004;
   HTTP_AUTH_ENABLE_NEGOTIATE    = $00000008;
   HTTP_AUTH_ENABLE_KERBEROS     = $00000010;
   HTTP_AUTH_ENABLE_ALL          = $0000001F;

   HTTP_AUTH_EX_FLAG_ENABLE_KERBEROS_CREDENTIAL_CACHING  = $01;
   HTTP_AUTH_EX_FLAG_CAPTURE_CREDENTIAL                  = $02;

type
   HTTP_SERVER_AUTHENTICATION_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      AuthSchemes : ULONG;
      ReceiveMutualAuth : BYTEBOOL;
      ReceiveContextHandle : BYTEBOOL;
      DisableNTLMCredentialCaching : BYTEBOOL;
      ExFlags : BYTE;
      DigestParams : HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;
      BasicParams : HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;
   end;
   PHTTP_SERVER_AUTHENTICATION_INFO = ^HTTP_SERVER_AUTHENTICATION_INFO;


   HTTP_SERVICE_BINDING_TYPE=(
      HttpServiceBindingTypeNone = 0,
      HttpServiceBindingTypeW,
      HttpServiceBindingTypeA
   );

   HTTP_SERVICE_BINDING_BASE = record
      BindingType : HTTP_SERVICE_BINDING_TYPE;
   end;
   PHTTP_SERVICE_BINDING_BASE = ^HTTP_SERVICE_BINDING_BASE;

   HTTP_SERVICE_BINDING_A = record
      Base : HTTP_SERVICE_BINDING_BASE;
      Buffer : PAnsiChar;
      BufferSize : ULONG;
   end;
   PHTTP_SERVICE_BINDING_A = HTTP_SERVICE_BINDING_A;

   HTTP_SERVICE_BINDING_W = record
      Base : HTTP_SERVICE_BINDING_BASE;
      Buffer : PWCHAR;
      BufferSize : ULONG;
   end;
   PHTTP_SERVICE_BINDING_W = ^HTTP_SERVICE_BINDING_W;

   HTTP_AUTHENTICATION_HARDENING_LEVELS = (
      HttpAuthenticationHardeningLegacy = 0,
      HttpAuthenticationHardeningMedium,
      HttpAuthenticationHardeningStrict
   );

const
   HTTP_CHANNEL_BIND_PROXY = $1;
   HTTP_CHANNEL_BIND_PROXY_COHOSTING = $20;

   HTTP_CHANNEL_BIND_NO_SERVICE_NAME_CHECK = $2;
   HTTP_CHANNEL_BIND_DOTLESS_SERVICE = $4;

   HTTP_CHANNEL_BIND_SECURE_CHANNEL_TOKEN = $8;
   HTTP_CHANNEL_BIND_CLIENT_SERVICE = $10;

type
   HTTP_CHANNEL_BIND_INFO = record
      Hardening : HTTP_AUTHENTICATION_HARDENING_LEVELS;
      Flags : ULONG;
      ServiceNames : PHTTP_SERVICE_BINDING_BASE;
      NumberOfServiceNames : ULONG;
   end;
   PHTTP_CHANNEL_BIND_INFO = ^HTTP_CHANNEL_BIND_INFO;

   HTTP_REQUEST_CHANNEL_BIND_STATUS = record
      ServiceName : PHTTP_SERVICE_BINDING_BASE;
      ChannelToken : PUCHAR;
      ChannelTokenSize : ULONG;
      Flags : ULONG;
   end;
   PHTTP_REQUEST_CHANNEL_BIND_STATUS = ^HTTP_REQUEST_CHANNEL_BIND_STATUS;

const
   // Logging option flags. When used in the logging configuration alters
   // some default logging behaviour.

   // HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER - This flag is used to change
   //      the log file rollover to happen by local time based. By default
   //      log file rollovers happen by GMT time.
   HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER = 1;

   // HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION - When set the unicode fields
   //      will be converted to UTF8 multibytes when writting to the log
   //      files. When this flag is not present, the local code page
   //      conversion happens.
   HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION = 2;

   // HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY -
   // HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY - These two flags are used to
   //      to do selective logging. If neither of them are present both
   //      types of requests will be logged. Only one these flags can be
   //      set at a time. They are mutually exclusive.
   HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY = 4;
   HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY = 8;

   //
   // The known log fields recognized/supported by HTTPAPI. Following fields
   // are used for W3C logging. Subset of them are also used for error
   // logging.
   //
   HTTP_LOG_FIELD_DATE              = $00000001;
   HTTP_LOG_FIELD_TIME              = $00000002;
   HTTP_LOG_FIELD_CLIENT_IP         = $00000004;
   HTTP_LOG_FIELD_USER_NAME         = $00000008;
   HTTP_LOG_FIELD_SITE_NAME         = $00000010;
   HTTP_LOG_FIELD_COMPUTER_NAME     = $00000020;
   HTTP_LOG_FIELD_SERVER_IP         = $00000040;
   HTTP_LOG_FIELD_METHOD            = $00000080;
   HTTP_LOG_FIELD_URI_STEM          = $00000100;
   HTTP_LOG_FIELD_URI_QUERY         = $00000200;
   HTTP_LOG_FIELD_STATUS            = $00000400;
   HTTP_LOG_FIELD_WIN32_STATUS      = $00000800;
   HTTP_LOG_FIELD_BYTES_SENT        = $00001000;
   HTTP_LOG_FIELD_BYTES_RECV        = $00002000;
   HTTP_LOG_FIELD_TIME_TAKEN        = $00004000;
   HTTP_LOG_FIELD_SERVER_PORT       = $00008000;
   HTTP_LOG_FIELD_USER_AGENT        = $00010000;
   HTTP_LOG_FIELD_COOKIE            = $00020000;
   HTTP_LOG_FIELD_REFERER           = $00040000;
   HTTP_LOG_FIELD_VERSION           = $00080000;
   HTTP_LOG_FIELD_HOST              = $00100000;
   HTTP_LOG_FIELD_SUB_STATUS        = $00200000;

   HTTP_ALL_NON_ERROR_LOG_FIELDS = HTTP_LOG_FIELD_SUB_STATUS*2-1;

   //
   // Fields that are used only for error logging.
   //
   HTTP_LOG_FIELD_CLIENT_PORT    = $00400000;
   HTTP_LOG_FIELD_URI            = $00800000;
   HTTP_LOG_FIELD_SITE_ID        = $01000000;
   HTTP_LOG_FIELD_REASON         = $02000000;
   HTTP_LOG_FIELD_QUEUE_NAME     = $04000000;

type
   HTTP_LOGGING_TYPE = (
      HttpLoggingTypeW3C,
      HttpLoggingTypeIIS,
      HttpLoggingTypeNCSA,
      HttpLoggingTypeRaw
      );

   HTTP_LOGGING_ROLLOVER_TYPE = (
      HttpLoggingRolloverSize,
      HttpLoggingRolloverDaily,
      HttpLoggingRolloverWeekly,
      HttpLoggingRolloverMonthly,
      HttpLoggingRolloverHourly
      );

   HTTP_LOGGING_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      LoggingFlags : ULONG;
      SoftwareName : PWideChar;
      SoftwareNameLength : USHORT;
      DirectoryNameLength : USHORT;
      DirectoryName : PWideChar;
      Format : HTTP_LOGGING_TYPE;
      Fields : ULONG;
      pExtFields : PVOID;
      NumOfExtFields : USHORT;
      MaxRecordSize : USHORT;
      RolloverType : HTTP_LOGGING_ROLLOVER_TYPE;
      RolloverSize : ULONG;
      pSecurityDescriptor : PSECURITY_DESCRIPTOR;
   end;
   PHTTP_LOGGING_INFO = ^HTTP_LOGGING_INFO;

   HTTP_LOG_DATA_TYPE = (
      HttpLogDataTypeFields
      );

   HTTP_LOG_DATA = record
      Typ : HTTP_LOG_DATA_TYPE
   end;
   PHTTP_LOG_DATA = ^HTTP_LOG_DATA;

   HTTP_LOG_FIELDS_DATA = record
      Base : HTTP_LOG_DATA;
      UserNameLength : USHORT;
      UriStemLength : USHORT;
      ClientIpLength : USHORT;
      ServerNameLength : USHORT;
      ServiceNameLength : USHORT;
      ServerIpLength : USHORT;
      MethodLength : USHORT;
      UriQueryLength : USHORT;
      HostLength : USHORT;
      UserAgentLength : USHORT;
      CookieLength : USHORT;
      ReferrerLength : USHORT;
      UserName : PWideChar;
      UriStem : PWideChar;
      ClientIp : PAnsiChar;
      ServerName : PAnsiChar;
      ServiceName : PAnsiChar;
      ServerIp : PAnsiChar;
      Method : PAnsiChar;
      UriQuery : PAnsiChar;
      Host : PAnsiChar;
      UserAgent : PAnsiChar;
      Cookie : PAnsiChar;
      Referrer : PAnsiChar;
      ServerPort : USHORT;
      ProtocolStatus : USHORT;
      Win32Status : ULONG;
      MethodNum : THttpVerb;
      SubStatus : USHORT;
   end;
   PHTTP_LOG_FIELDS_DATA = ^HTTP_LOG_FIELDS_DATA;

   HTTP_BINDING_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      RequestQueueHandle : THandle;
   end;

   HTTP_PROTECTION_LEVEL_TYPE=(
      HttpProtectionLevelUnrestricted,
      HttpProtectionLevelEdgeRestricted,
      HttpProtectionLevelRestricted
   );

   HTTP_PROTECTION_LEVEL_INFO = record
      Flags : HTTP_PROPERTY_FLAGS;
      Level : HTTP_PROTECTION_LEVEL_TYPE;
   end;
   PHTTP_PROTECTION_LEVEL_INFO = ^HTTP_PROTECTION_LEVEL_INFO;

const
   //   HTTP_VERSION_UNKNOWN : HTTPAPI_VERSION = (MajorVersion : 0; MinorVersion : 0);
   //   HTTP_VERSION_0_9 : HTTPAPI_VERSION = (MajorVersion : 0; MinorVersion : 9);
   //   HTTP_VERSION_1_0 : HTTPAPI_VERSION = (MajorVersion : 1; MinorVersion : 0);
   //   HTTP_VERSION_1_1 : HTTPAPI_VERSION = (MajorVersion : 1; MinorVersion : 1);
   //   HTTPAPI_VERSION_1 : HTTPAPI_VERSION = (MajorVersion : 1; MinorVersion : 0);
   HTTPAPI_VERSION_2 : HTTPAPI_VERSION = (MajorVersion : 2; MinorVersion : 0);

   // if set, available entity body is copied along with the request headers
   // into pEntityChunks
   HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = 1;
   HTTP_RECEIVE_REQUEST_FLAG_FLUSH_BODY = 2;
   // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364496
   HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER = 1;

   // there is more entity body to be read for this request
   HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = 1;
   // initialization for applications that use the HTTP Server API
   HTTP_INITIALIZE_SERVER = 1;
   // initialization for applications that use the HTTP configuration functions
   HTTP_INITIALIZE_CONFIG = 2;

   HTTP_URL_FLAG_REMOVE_ALL = 1;

   HTTP_CREATE_REQUEST_QUEUE_FLAG_OPEN_EXISTING = 1;
   HTTP_CREATE_REQUEST_QUEUE_FLAG_CONTROLLER = 2;

   // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364499
   HTTP_SEND_RESPONSE_FLAG_DISCONNECT        = $00000001;
   HTTP_SEND_RESPONSE_FLAG_MORE_DATA         = $00000002;
   HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA       = $00000004;
   HTTP_SEND_RESPONSE_FLAG_ENABLE_NAGLING    = $00000008;
   HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES    = $00000020;
   HTTP_SEND_RESPONSE_FLAG_OPAQUE            = $00000040;

   HTTP_FLUSH_RESPONSE_FLAG_RECURSIVE = 1;

type
   HTTP_SERVER_PROPERTY = (
      HttpServerAuthenticationProperty,
      HttpServerLoggingProperty,
      HttpServerQosProperty,
      HttpServerTimeoutsProperty,
      HttpServerQueueLengthProperty,
      HttpServerStateProperty,
      HttpServer503VerbosityProperty,
      HttpServerBindingProperty,
      HttpServerExtendedAuthenticationProperty,
      HttpServerListenEndpointProperty,
      HttpServerChannelBindProperty,
      HttpServerProtectionLevelProperty
      );

   THttpAPIs = (
      hInitialize, hTerminate, hCreateHttpHandle,
      hAddUrl, hRemoveUrl, hReceiveHttpRequest,
      hSendHttpResponse, hReceiveRequestEntityBody, hSendResponseEntityBody,
      hSetServiceConfiguration, hDeleteServiceConfiguration,

      hFlushResponseCache,

      hCancelHttpRequest,
      hCreateServerSession, hCloseServerSession,
      hCreateRequestQueue,
      hSetServerSessionProperty, hQueryServerSessionProperty,
      hCreateUrlGroup, hCloseUrlGroup,
      hAddUrlToUrlGroup, hRemoveUrlFromUrlGroup,
      hSetUrlGroupProperty, hQueryUrlGroupProperty,
      hSetRequestQueueProperty, hQueryRequestQueueProperty
      );

const
   HttpNames : array[THttpAPIs] of PChar = (
      'HttpInitialize', 'HttpTerminate', 'HttpCreateHttpHandle',
      'HttpAddUrl', 'HttpRemoveUrl', 'HttpReceiveHttpRequest',
      'HttpSendHttpResponse', 'HttpReceiveRequestEntityBody', 'HttpSendResponseEntityBody',
      'HttpSetServiceConfiguration', 'HttpDeleteServiceConfiguration',

      'HttpFlushResponseCache',

      'HttpCancelHttpRequest',
      'HttpCreateServerSession', 'HttpCloseServerSession',
      'HttpCreateRequestQueue',
      'HttpSetServerSessionProperty', 'HttpQueryServerSessionProperty',
      'HttpCreateUrlGroup', 'HttpCloseUrlGroup',
      'HttpAddUrlToUrlGroup', 'HttpRemoveUrlFromUrlGroup',
      'HttpSetUrlGroupProperty', 'HttpQueryUrlGroupProperty',
      'HttpSetRequestQueueProperty', 'HttpQueryRequestQueueProperty'
      );

type
   THttpAPI = packed record
      Module : THandle;
      {/ The HttpInitialize function initializes the HTTP Server API driver, starts it,
         if it has not already been started, and allocates data structures for the
         calling application to support response-queue creation and other operations.
         Call this function before calling any other functions in the HTTP Server API. }
      Initialize : function(Version : HTTPAPI_VERSION; Flags : cardinal;
            pReserved : pointer = nil) : HRESULT; stdcall;
      {/ The HttpTerminate function cleans up resources used by the HTTP Server API
         to process calls by an application. An application should call HttpTerminate
         once for every time it called HttpInitialize, with matching flag settings. }
      Terminate : function(Flags : cardinal;
            Reserved : integer = 0) : HRESULT; stdcall;
      {/ The HttpCreateHttpHandle function creates an HTTP request queue for the
         calling application and returns a handle to it. }
      CreateHttpHandle : function(var ReqQueueHandle : THandle;
            Reserved : integer = 0) : HRESULT; stdcall;
      {/ The HttpAddUrl function registers a given URL so that requests that match
         it are routed to a specified HTTP Server API request queue. An application
         can register multiple URLs to a single request queue using repeated calls to
         HttpAddUrl.
         - a typical url prefix is 'http://+:80/vroot/', 'https://+:80/vroot/' or
          'http://adatum.com:443/secure/database/' - here the '+' is called a
          Strong wildcard, i.e. will match every IP or server name }
      AddUrl : function(ReqQueueHandle : THandle; UrlPrefix : PWideChar;
            Reserved : integer = 0) : HRESULT; stdcall;
      {/ Unregisters a specified URL, so that requests for it are no longer
         routed to a specified queue. }
      RemoveUrl : function(ReqQueueHandle : THandle; UrlPrefix : PWideChar) : HRESULT; stdcall;
      {/ retrieves the next available HTTP request from the specified request queue }
      ReceiveHttpRequest : function(ReqQueueHandle : THandle; RequestId : HTTP_REQUEST_ID;
            Flags : cardinal; var pRequestBuffer : HTTP_REQUEST_V2; RequestBufferLength : ULONG;
            var pBytesReceived : ULONG; pOverlapped : pointer = nil) : HRESULT; stdcall;
      {/ sent the response to a specified HTTP request }
      SendHttpResponse : function(ReqQueueHandle : THandle; RequestId : HTTP_REQUEST_ID;
            Flags : integer; var pHttpResponse : HTTP_RESPONSE_V2; pReserved1 : pointer;
            var pBytesSent : cardinal; pReserved2 : pointer = nil; Reserved3 : ULONG = 0;
            pOverlapped : pointer = nil; pLogData : PHTTP_LOG_DATA = nil) : HRESULT; stdcall;
      {/ receives additional entity body data for a specified HTTP request }
      ReceiveRequestEntityBody : function(ReqQueueHandle : THandle; RequestId : HTTP_REQUEST_ID;
            Flags : ULONG; pBuffer : pointer; BufferLength : cardinal; var pBytesReceived : cardinal;
            pOverlapped : pointer = nil) : HRESULT; stdcall;
      {/ sends entity-body data associated with an HTTP response. }
      SendResponseEntityBody : function(ReqQueueHandle : THandle; RequestId : HTTP_REQUEST_ID;
            Flags : ULONG; EntityChunkCount: USHORT; pEntityChunks: pointer; var pBytesSent: ULONG;
            pReserved1 : pointer = nil; pReserved2 : ULONG = 0; pOverlapped : pointer = nil;
            pLogData : PHTTP_LOG_DATA = nil) : HRESULT; stdcall;
      {/ set specified data, such as IP addresses or SSL Certificates, from the
         HTTP Server API configuration store}
      SetServiceConfiguration : function(ServiceHandle : THandle;
            ConfigId : THttpServiceConfigID; pConfigInformation : pointer;
            ConfigInformationLength : ULONG; pOverlapped : pointer = nil) : HRESULT; stdcall;
      {/ deletes specified data, such as IP addresses or SSL Certificates, from the
         HTTP Server API configuration store}
      DeleteServiceConfiguration : function(ServiceHandle : THandle;
            ConfigId : THttpServiceConfigID; pConfigInformation : pointer;
            ConfigInformationLength : ULONG; pOverlapped : pointer = nil) : HRESULT; stdcall;

      FlushResponseCache : function(ReqQueueHandle: THandle; pUrlPrefix: PWideChar; Flags: ULONG;
            pOverlapped: POverlapped): ULONG; stdcall;

      CancelHttpRequest : function(ReqQueueHandle : THandle; RequestId : HTTP_REQUEST_ID;
            pOverlapped : pointer = nil) : HRESULT; stdcall;

      CreateServerSession : function(Version : HTTPAPI_VERSION;
            var ServerSessionId : HTTP_SERVER_SESSION_ID; Reserved : ULONG = 0) : HRESULT; stdcall;
      CloseServerSession : function(ServerSessionId : HTTP_SERVER_SESSION_ID) : HRESULT; stdcall;

      CreateRequestQueue : function(Version : HTTPAPI_VERSION;
            pName : PWideChar; pSecurityAttributes : Pointer;
            Flags : ULONG; var ReqQueueHandle : THandle) : HRESULT; stdcall;

      SetServerSessionProperty : function(ServerSessionId : HTTP_SERVER_SESSION_ID;
            aProperty : HTTP_SERVER_PROPERTY; pPropertyInformation : Pointer;
            PropertyInformationLength : ULONG) : HRESULT; stdcall;
      QueryServerSessionProperty : function(ServerSessionId : HTTP_SERVER_SESSION_ID;
            aProperty : HTTP_SERVER_PROPERTY; pPropertyInformation : Pointer;
            PropertyInformationLength : ULONG; pReturnLength : PULONG = nil) : HRESULT; stdcall;

      CreateUrlGroup : function(ServerSessionId : HTTP_SERVER_SESSION_ID;
            var UrlGroupId : HTTP_URL_GROUP_ID; Reserved : ULONG = 0) : HRESULT; stdcall;
      CloseUrlGroup : function(UrlGroupId : HTTP_URL_GROUP_ID) : HRESULT; stdcall;

      AddUrlToUrlGroup : function(UrlGroupId : HTTP_URL_GROUP_ID;
            pFullyQualifiedUrl : PWideChar; UrlContext : HTTP_URL_CONTEXT = 0;
            Reserved : ULONG = 0) : HRESULT; stdcall;
      RemoveUrlFromUrlGroup : function(UrlGroupId : HTTP_URL_GROUP_ID;
            pFullyQualifiedUrl : PWideChar; Flags : ULONG = HTTP_URL_FLAG_REMOVE_ALL) : HRESULT; stdcall;

      SetUrlGroupProperty : function(UrlGroupId : HTTP_URL_GROUP_ID;
            aProperty : HTTP_SERVER_PROPERTY; pPropertyInformation : Pointer;
            PropertyInformationLength : ULONG) : HRESULT; stdcall;
      QueryUrlGroupProperty : function(UrlGroupId : HTTP_URL_GROUP_ID;
            aProperty : HTTP_SERVER_PROPERTY; pPropertyInformation : Pointer;
            PropertyInformationLength : ULONG; pReturnLength : PULONG = nil) : HRESULT; stdcall;

      SetRequestQueueProperty: function(ReqQueueHandle: THandle;
            aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
            PropertyInformationLength: ULONG; Reserved: ULONG; pReserved: Pointer): HRESULT; stdcall;
      QueryRequestQueueProperty: function(ReqQueueHandle: THandle;
            aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
            PropertyInformationLength: ULONG; Reserved: ULONG; pReturnLength: PULONG;
            pReserved: Pointer) : HRESULT; stdcall;

      class procedure InitializeAPI; static;

      class procedure Check(error : HRESULT; api : THttpAPIs); static; inline;
   end;

   EHttpApiServer = class (Exception)
   public
      constructor Create(api : THttpAPIs; Error : integer);
   end;

var
   HttpAPI : THttpAPI;

/// retrieve the HTTP reason text from a code
// - e.g. StatusCodeToReason(200)='OK'
procedure StatusCodeToReason(code : integer; var result : RawByteString);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

class procedure THttpAPI.InitializeAPI;
var
   api : THttpAPIs;
   P : PPointer;
begin
   if HttpAPI.Module<>0 then
      exit; // already loaded
   try
      if HttpAPI.Module = 0 then begin
         HttpAPI.Module := LoadLibrary('httpapi.dll');
         if HttpAPI.Module<=255 then
            raise Exception.Create('Unable to find httpapi.dll');
      {$ifdef FPC}
         P := @Http.Initialize;
      {$else}
         P := @@HttpAPI.Initialize;
      {$endif}
         for api := low(api) to high(api) do begin
            P^ := GetProcAddress(HttpAPI.Module, HttpNames[api]);
            if P^ = nil then
               raise Exception.CreateFmt('Unable to find %s in httpapi.dll', [HttpNames[api]]);
            inc(P);
         end;
      end;
   except
      on E : Exception do begin
         if HttpAPI.Module>255 then begin
            FreeLibrary(HttpAPI.Module);
            HttpAPI.Module := 0;
         end;
         raise E;
      end;
   end;
end;

class procedure THttpAPI.Check(error : HRESULT; api : THttpAPIs);
begin
   case error of
      NO_ERROR : ;
      ERROR_NETNAME_DELETED : ; // ignored
   else
      raise EHttpApiServer.Create(api, error);
   end;
end;

// StatusCodeToReason
//
var
   vCode200 : RawByteString; // initialized in initialization (see there)
procedure StatusCodeToReason(code : integer; var result : RawByteString);
const
   cCodes200 : array [200..207] of RawByteString = (
      'OK', 'Created', 'Accepted', 'Non-Authoritative Information',  // 200-203
      'No Content', 'Reset Content', 'Partial Content',              // 204-206
      'Multi-Status' );                                              // 207
   cCodes300 : array [300..307] of RawByteString = (
      'Multiple Choices', 'Moved Permanently', 'Found', 'See Other', // 300-303
      'Not Modified', 'Use Proxy', 'Unused', 'Temporary Redirect' ); // 304-307
   cCodes400 : array [400..417] of RawByteString = (
      'Bad Request', 'Unauthorized', 'Payment Required', 'Forbidden',// 400-403
      'Not Found', 'Method Not Allowed', 'Not Acceptable',           // 404-406
      'Proxy Authentication Required', 'Request Timeout',            // 407-408
      'Conflict', 'Gone', 'Length Required', 'Precondition Failed',  // 409-412
      'Request Entity Too Large', 'Request-URI Too Long',            // 413-414
      'Unsupported Media Type', 'Requested Range Not Satisfiable',   // 415-416
      'Expectation Failed' );
   cCodes500 : array [500..503] of RawByteString = (
      'Internal Server Error', 'Not Implemented', 'Bad Gateway',     // 500-502
      'Service Unavailable' );
begin
   if code = 200 then
      result := vCode200
   else case code of
      100 : result := 'Continue';
      101 : result := 'Switching Protocols';
      200..High(cCodes200) :
         result := cCodes200[code];
      300..High(cCodes300) :
         result := cCodes300[code];
      400..High(cCodes400) :
         result := cCodes400[code];
      423: result:='Locked';
      500..High(cCodes500) :
         result := cCodes500[code];
      507: result := 'Insufficient Storage';
   else
      result := 'Unknown';
   end;
end;

function IdemPChar(p, up : pAnsiChar) : boolean;
   // if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
var
   c : AnsiChar;
begin
   result := false;
   if (p = nil) or (up = nil) then
      exit;
   while up^<>#0 do begin
      c := p^;
      if up^<>c then
         if c in ['a'..'z'] then begin
            dec(c, 32);
            if up^<>c then
               exit;
         end else
            exit;
      inc(up);
      inc(p);
   end;
   result := true;
end;

constructor EHttpApiServer.Create(api : THttpAPIs; Error : integer);
begin
   inherited CreateFmt('%s failed: %s (%d)',
      [HttpNames[api], SysErrorMessage(Error), Error]);
end;

{ HTTP_RESPONSE_V2 }

procedure HTTP_RESPONSE_V2.SetContent(var dataChunk : HTTP_DATA_CHUNK_INMEMORY;
   const Content, ContentType : RawByteString);
begin
   FillChar(dataChunk, SizeOf(dataChunk), 0);
   if Content <> '' then begin
      dataChunk.DataChunkType := hctFromMemory;
      dataChunk.pBuffer := pointer(Content);
      dataChunk.BufferLength := length(Content);
      EntityChunkCount := 1;
      pEntityChunks := @dataChunk;
   end;
   if ContentType<>'' then begin
      Headers.KnownHeaders[reqContentType].RawValueLength := Length(ContentType);
      Headers.KnownHeaders[reqContentType].pRawValue := Pointer(ContentType);
   end;
end;

procedure HTTP_RESPONSE_V2.SetHeaders(P : PAnsiChar;
   const UnknownHeaders : HTTP_UNKNOWN_HEADER_ARRAY);
var
   knownHeader : THttpHeader;
   current : PHTTP_UNKNOWN_HEADER;
   pKnown : PHTTP_KNOWN_HEADER;
   setCookieOnce : Boolean;
begin
   current := @UnknownHeaders[0];
   Headers.pUnknownHeaders := current;
   Headers.UnknownHeaderCount := 0;

   if P=nil then Exit;

   setCookieOnce := False;

   while True do begin
      while P^ in [#13, #10] do
         Inc(P);
      if P^ = #0 then
         break;
      if IdemPChar(P, 'CONTENT-TYPE:') then
         knownHeader := reqContentType
      else if IdemPChar(P, 'CONTENT-ENCODING:') then
         knownHeader := reqContentEncoding
      else if IdemPChar(P, 'LOCATION:') then
         knownHeader := respLocation
      else if IdemPChar(P, 'WWW-AUTHENTICATE:') then
         knownHeader := respWwwAuthenticate
      else if IdemPChar(P, 'SET-COOKIE:') then
         if setCookieOnce then
            knownHeader := reqCacheControl
         else begin
            knownHeader := respSetCookie;
            setCookieOnce := True;
         end
      else if IdemPChar(P, 'UPGRADE:') then
         knownHeader := reqUpgrade
      else knownHeader := reqCacheControl; // mark not found

      if knownHeader<>reqCacheControl then begin
         pKnown := @Headers.KnownHeaders[knownHeader];
         while P^<>':' do
            Inc(P);
         Inc(P); // jump ':'
         while P^ = ' ' do
            Inc(P);
         pKnown^.pRawValue := P;
         while P^>=' ' do
            Inc(P);
         pKnown^.RawValueLength := P-pKnown^.pRawValue;
      end else begin
         current^.pName := P;
         while (P^>=' ') and (P^<>':') do
            Inc(P);
         if P^ = ':' then begin
            current^.NameLength := P-current^.pName;
            repeat
               Inc(P)
            until P^<>' ';
            current^.pRawValue := P;
            repeat
               Inc(P)
            until P^<' ';
            current^.RawValueLength := P-current^.pRawValue;
            if Headers.UnknownHeaderCount<Length(UnknownHeaders) then begin
               Inc(current);
               Inc(Headers.UnknownHeaderCount);
            end;
         end else begin
            while P^>=' ' do
               Inc(P);
         end;
      end;
   end;
end;

procedure HTTP_RESPONSE_V2.SetStatus(code : integer; var outStatus : RawByteString);
begin
   StatusCode := code;
   StatusCodeToReason(code, outStatus);
   ReasonLength := Length(outStatus);
   pReason := Pointer(outStatus);
end;

// SetErrorStatus
//
procedure HTTP_RESPONSE_V2.SetErrorStatus(code : integer; var outStatus : RawByteString;
                         dataChunkForErrorContent : PHTTP_DATA_CHUNK_INMEMORY;
                         const errorMsg : String);
begin
   SetStatus(code, outStatus);
   if dataChunkForErrorContent<>nil then
      SetContent(dataChunkForErrorContent^,
                 '<h1>'+outStatus+'</h1>'+UTF8Encode(errorMsg),
                 'text/html; charset=utf-8');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   // we initialize the variable so that reference counting is effective
   vCode200 := 'OK';

end.
