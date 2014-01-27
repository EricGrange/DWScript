object dwsIndyLib: TdwsIndyLib
  OldCreateOrder = False
  Left = 769
  Top = 189
  Height = 129
  Width = 183
  object dwsUnitIndy: TdwsUnit
    Classes = <
      item
        Name = 'TIdFtpProxySettings'
        IsSealed = True
        Methods = <
          item
            Name = 'GetProxyType'
            ResultType = 'TFtpProxyType'
            OnEval = dwsTIdFtpProxySettingsMethodsGetProxyTypeEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetProxyType'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TFtpProxyType'
              end>
            OnEval = dwsTIdFtpProxySettingsMethodsSetProxyTypeEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetHost'
            ResultType = 'String'
            OnEval = dwsTIdFtpProxySettingsMethodsGetHostEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetHost'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpProxySettingsMethodsSetHostEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUsername'
            ResultType = 'String'
            OnEval = dwsTIdFtpProxySettingsMethodsGetUsernameEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUsername'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpProxySettingsMethodsSetUsernameEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPassword'
            ResultType = 'String'
            OnEval = dwsTIdFtpProxySettingsMethodsGetPasswordEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPassword'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpProxySettingsMethodsSetPasswordEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPort'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpProxySettingsMethodsGetPortEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPort'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
              end>
            OnEval = dwsTIdFtpProxySettingsMethodsSetPortEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'ProxyType'
            DataType = 'TFtpProxyType'
            ReadAccess = 'GetProxyType'
            WriteAccess = 'SetProxyType'
          end
          item
            Name = 'Host'
            DataType = 'String'
            ReadAccess = 'GetHost'
            WriteAccess = 'SetHost'
          end
          item
            Name = 'Username'
            DataType = 'String'
            ReadAccess = 'GetUsername'
            WriteAccess = 'SetUsername'
          end
          item
            Name = 'Password'
            DataType = 'String'
            ReadAccess = 'GetPassword'
            WriteAccess = 'SetPassword'
          end
          item
            Name = 'Port'
            DataType = 'Integer'
            ReadAccess = 'GetPort'
            WriteAccess = 'SetPort'
          end>
      end
      item
        Name = 'TIdFTP'
        IsSealed = True
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsTIdFtpConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'GetBoundIP'
            ResultType = 'String'
            OnEval = dwsTIdFtpMethodsGetBoundIPEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetBoundIP'
            Parameters = <
              item
                Name = 'value'
                DataType = 'String'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetBoundIPEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetBoundPort'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetBoundPortEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetBoundPort'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetBoundPortEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetBoundPortMax'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetBoundPortMaxEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetBoundPortMax'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetBoundPortMaxEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetBoundPortMin'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetBoundPortMinEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'GetConnectionTimeOut'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetConnectionTimeOutEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetConnectionTimeOut'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetConnectionTimeOutEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetReuseSocket'
            ResultType = 'TReuseSocket'
            OnEval = dwsTIdFtpMethodsGetReuseSocketEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetReuseSocket'
            Parameters = <
              item
                Name = 'value'
                DataType = 'TReuseSocket'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetReuseSocketEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseNagle'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetUseNagleEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseNagle'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetUseNagleEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetSupportsTLS'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetSupportsTLSEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'GetCapabilities'
            ResultType = 'String'
            OnEval = dwsTIdFtpMethodsGetCapabilitiesEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'GetIPVersion'
            ResultType = 'TIPVersion'
            OnEval = dwsTIdFtpMethodsGetIPVersionEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetIPVersion'
            Parameters = <
              item
                Name = 'value'
                DataType = 'TIPVersion'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetIPVersionEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetAutoIssueFEAT'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetAutoIssueFEATEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetAutoIssueFEAT'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetAutoIssueFEATEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetHost'
            ResultType = 'String'
            OnEval = dwsTIdFtpMethodsGetHostEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetHost'
            Parameters = <
              item
                Name = 'value'
                DataType = 'String'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetHostEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseCCC'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetUseCCCEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseCCC'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetUseCCCEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPassiveUseControlHost'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetPassiveUseControlHostEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPassiveUseControlHost'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetPassiveUseControlHostEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetDataPortProtection'
            ResultType = 'TFTPDataPortSecurity'
            OnEval = dwsTIdFtpMethodsGetDataPortProtectionEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetDataPortProtection'
            Parameters = <
              item
                Name = 'value'
                DataType = 'TFTPDataPortSecurity'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetDataPortProtectionEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetAUTHCmd'
            ResultType = 'TAuthCmd'
            OnEval = dwsTIdFtpMethodsGetAUTHCmdEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetAUTHCmd'
            Parameters = <
              item
                Name = 'value'
                DataType = 'TAuthCmd'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetAUTHCmdEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetDataPort'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetDataPortEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetDataPort'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetDataPortEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetDataPortMax'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetDataPortMaxEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetDataPortMax'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetDataPortMaxEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetDataPortMin'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetDataPortMinEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetDataPortMin'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetDataPortMinEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetExternalIP'
            ResultType = 'String'
            OnEval = dwsTIdFtpMethodsGetExternalIPEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetExternalIP'
            Parameters = <
              item
                Name = 'value'
                DataType = 'String'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetExternalIPEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetTransferType'
            ResultType = 'TFTPTransferType'
            OnEval = dwsTIdFtpMethodsGetTransferTypeEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetTransferType'
            Parameters = <
              item
                Name = 'value'
                DataType = 'TFTPTransferType'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetTransferTypeEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetTransferTimeout'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetTransferTimeoutEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetTransferTimeout'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetTransferTimeoutEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetListenTimeout'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetListenTimeoutEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetListenTimeout'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetListenTimeoutEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseExtensionDataPort'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetUseExtensionDataPortEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseExtensionDataPort'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetUseExtensionDataPortEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseMLIS'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetUseMLISEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseMLIS'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetUseMLISEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetTryNATFastTrack'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetTryNATFastTrackEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetTryNATFastTrack'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetTryNATFastTrackEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetProxySettings'
            ResultType = 'TIdFtpProxySettings'
            OnEval = dwsTIdFtpMethodsGetProxySettingsEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetProxySettings'
            Parameters = <
              item
                Name = 'value'
                DataType = 'TIdFtpProxySettings'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetProxySettingsEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetAccount'
            ResultType = 'String'
            OnEval = dwsTIdFtpMethodsGetAccountEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetAccount'
            Parameters = <
              item
                Name = 'value'
                DataType = 'String'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetAccountEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseHOST'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetUseHOSTEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseHOST'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetUseHOSTEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseTLS'
            ResultType = 'TUseTls'
            OnEval = dwsTIdFtpMethodsGetUseTLSEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseTLS'
            Parameters = <
              item
                Name = 'value'
                DataType = 'TUseTls'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetUseTLSEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetReadTimeout'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetReadTimeoutEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetReadTimeout'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdFtpMethodsSetReadTimeoutEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUserName'
            ResultType = 'String'
            OnEval = dwsTIdFtpMethodsGetUserNameEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUserName'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpMethodsSetUserNameEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPassword'
            ResultType = 'String'
            OnEval = dwsTIdFtpMethodsGetPasswordEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPassword'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpMethodsSetPasswordEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPort'
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsGetPortEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPort'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
              end>
            OnEval = dwsTIdFtpMethodsSetPortEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPassive'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetPassiveEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPassive'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdFtpMethodsSetPassiveEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetResumeSupported'
            ResultType = 'Boolean'
            OnEval = dwsTIdFtpMethodsGetResumeSupportedEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'Abort'
            OnEval = dwsTIdFtpMethodsAbortEval
            Kind = mkProcedure
          end
          item
            Name = 'ChangeDir'
            Parameters = <
              item
                Name = 'DirName'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpMethodsChangeDirEval
            Kind = mkProcedure
          end
          item
            Name = 'ChangeDirUp'
            OnEval = dwsTIdFtpMethodsChangeDirUpEval
            Kind = mkProcedure
          end
          item
            Name = 'Connect'
            OnEval = dwsTIdFtpMethodsConnectEval
            Kind = mkProcedure
          end
          item
            Name = 'Delete'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpMethodsDeleteEval
            Kind = mkProcedure
          end
          item
            Name = 'Disconnect'
            OnEval = dwsTIdFtpMethodsDisconnectEval
            Kind = mkProcedure
          end
          item
            Name = 'Get'
            Parameters = <
              item
                Name = 'SourceFileName'
                DataType = 'String'
              end
              item
                Name = 'DestFileName'
                DataType = 'String'
              end
              item
                Name = 'CanOverwrite'
                DataType = 'Boolean'
                HasDefaultValue = True
                DefaultValue = False
              end
              item
                Name = 'Resume'
                DataType = 'Boolean'
                HasDefaultValue = True
                DefaultValue = False
              end>
            OnEval = dwsTIdFtpMethodsGetEval
            Kind = mkProcedure
          end
          item
            Name = 'Login'
            OnEval = dwsTIdFtpMethodsLoginEval
            Kind = mkProcedure
          end
          item
            Name = 'MakeDir'
            Parameters = <
              item
                Name = 'DirName'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpMethodsMakeDirEval
            Kind = mkProcedure
          end
          item
            Name = 'Noop'
            OnEval = dwsTIdFtpMethodsNoopEval
            Kind = mkProcedure
          end
          item
            Name = 'Put'
            Parameters = <
              item
                Name = 'SourceFileName'
                DataType = 'String'
              end
              item
                Name = 'DestinationFileName'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end
              item
                Name = 'Append'
                DataType = 'Boolean'
                HasDefaultValue = True
                DefaultValue = False
              end>
            OnEval = dwsTIdFtpMethodsPutEval
            Kind = mkProcedure
          end
          item
            Name = 'RemoveDir'
            Parameters = <
              item
                Name = 'DirName'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpMethodsRemoveDirEval
            Kind = mkProcedure
          end
          item
            Name = 'Rename'
            Parameters = <
              item
                Name = 'SourceFileName'
                DataType = 'String'
              end
              item
                Name = 'DestFileName'
                DataType = 'String'
              end>
            OnEval = dwsTIdFtpMethodsRenameEval
            Kind = mkProcedure
          end
          item
            Name = 'RetrieveCurrentDir'
            ResultType = 'String'
            OnEval = dwsTIdFtpMethodsRetrieveCurrentDirEval
            Kind = mkFunction
          end
          item
            Name = 'Size'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsSizeEval
            Kind = mkFunction
          end
          item
            Name = 'CRC'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
              end
              item
                Name = 'StartPoint'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 0
              end
              item
                Name = 'EndPoint'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 0
              end>
            ResultType = 'Integer'
            OnEval = dwsTIdFtpMethodsCRCEval
            Kind = mkFunction
          end>
        Properties = <
          item
            Name = 'BoundIP'
            DataType = 'String'
            ReadAccess = 'GetBoundIP'
            WriteAccess = 'SetBoundIP'
          end
          item
            Name = 'BoundPort'
            DataType = 'Integer'
            ReadAccess = 'GetBoundPort'
            WriteAccess = 'SetBoundPort'
          end
          item
            Name = 'BoundPortMax'
            DataType = 'Integer'
            ReadAccess = 'GetBoundPortMax'
            WriteAccess = 'SetBoundPortMax'
          end
          item
            Name = 'BoundPortMin'
            DataType = 'Integer'
            ReadAccess = 'GetBoundPortMin'
            WriteAccess = 'SetBoundPortMax'
          end
          item
            Name = 'ConnectionTimeOut'
            DataType = 'Integer'
            ReadAccess = 'GetConnectionTimeOut'
            WriteAccess = 'SetConnectionTimeOut'
          end
          item
            Name = 'ReuseSocket'
            DataType = 'TReuseSocket'
            ReadAccess = 'GetReuseSocket'
            WriteAccess = 'SetReuseSocket'
          end
          item
            Name = 'UseNagle'
            DataType = 'Boolean'
            ReadAccess = 'GetUseNagle'
            WriteAccess = 'SetUseNagle'
          end
          item
            Name = 'SupportsTLS'
            DataType = 'Boolean'
            ReadAccess = 'GetSupportsTLS'
          end
          item
            Name = 'Capabilities'
            DataType = 'String'
            ReadAccess = 'GetCapabilities'
          end
          item
            Name = 'IPVersion'
            DataType = 'TIPVersion'
            Visibility = cvPublished
            ReadAccess = 'GetIPVersion'
            WriteAccess = 'SetIPVersion'
          end
          item
            Name = 'AutoIssueFEAT'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetAutoIssueFEAT'
            WriteAccess = 'SetAutoIssueFEAT'
          end
          item
            Name = 'Host'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetHost'
            WriteAccess = 'SetHost'
          end
          item
            Name = 'UseCCC'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetUseCCC'
            WriteAccess = 'SetUseCCC'
          end
          item
            Name = 'Passive'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetPassive'
            WriteAccess = 'SetPassive'
          end
          item
            Name = 'PassiveUseControlHost'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetPassiveUseControlHost'
            WriteAccess = 'SetPassiveUseControlHost'
          end
          item
            Name = 'DataPortProtection'
            DataType = 'TFTPDataPortSecurity'
            Visibility = cvPublished
            ReadAccess = 'GetDataPortProtection'
            WriteAccess = 'SetDataPortProtection'
          end
          item
            Name = 'AUTHCmd'
            DataType = 'TAuthCmd'
            Visibility = cvPublished
            ReadAccess = 'GetAUTHCmd'
            WriteAccess = 'SetAUTHCmd'
          end
          item
            Name = 'DataPort'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'GetDataPort'
            WriteAccess = 'SetDataPort'
          end
          item
            Name = 'DataPortMax'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'GetDataPortMax'
            WriteAccess = 'SetDataPortMax'
          end
          item
            Name = 'DataPortMin'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'GetDataPortMin'
            WriteAccess = 'SetDataPortMin'
          end
          item
            Name = 'ExternalIP'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetExternalIP'
            WriteAccess = 'SetExternalIP'
          end
          item
            Name = 'Password'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetPassword'
            WriteAccess = 'SetPassword'
          end
          item
            Name = 'TransferType'
            DataType = 'TFTPTransferType'
            Visibility = cvPublished
            ReadAccess = 'GetTransferType'
            WriteAccess = 'SetTransferType'
          end
          item
            Name = 'TransferTimeout'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'GetTransferTimeout'
            WriteAccess = 'SetTransferTimeout'
          end
          item
            Name = 'ListenTimeout'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'GetListenTimeout'
            WriteAccess = 'SetListenTimeout'
          end
          item
            Name = 'Username'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetUserName'
            WriteAccess = 'SetUserName'
          end
          item
            Name = 'Port'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'GetPort'
            WriteAccess = 'SetPort'
          end
          item
            Name = 'CanResume'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetResumeSupported'
          end
          item
            Name = 'UseExtensionDataPort'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetUseExtensionDataPort'
            WriteAccess = 'SetUseExtensionDataPort'
          end
          item
            Name = 'UseMLIS'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetUseMLIS'
            WriteAccess = 'SetUseMLIS'
          end
          item
            Name = 'TryNATFastTrack'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetTryNATFastTrack'
            WriteAccess = 'SetTryNATFastTrack'
          end
          item
            Name = 'ProxySettings'
            DataType = 'TIdFtpProxySettings'
            Visibility = cvPublished
            ReadAccess = 'GetProxySettings'
            WriteAccess = 'SetProxySettings'
          end
          item
            Name = 'Account'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetAccount'
            WriteAccess = 'SetAccount'
          end
          item
            Name = 'UseHOST'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetUseHOST'
            WriteAccess = 'SetUseHOST'
          end
          item
            Name = 'UseTLS'
            DataType = 'TUseTls'
            Visibility = cvPublished
            ReadAccess = 'GetUseTLS'
            WriteAccess = 'SetUseTLS'
          end
          item
            Name = 'ReadTimeout'
            DataType = 'Integer'
            Visibility = cvPublished
            ReadAccess = 'GetReadTimeout'
            WriteAccess = 'SetReadTimeout'
          end>
        OnCleanUp = dwsTIdFtpCleanUp
      end
      item
        Name = 'TIdEMailAddressItem'
        IsSealed = True
        Constructors = <
          item
            Name = 'Create'
            Overloaded = True
          end
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'Text'
                DataType = 'String'
              end>
            Overloaded = True
          end>
        Methods = <
          item
            Name = 'GetAddress'
            ResultType = 'String'
            OnEval = dwsTIdEMailAddressItemMethodsGetAddressEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetAddress'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdEMailAddressItemMethodsSetAddressEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetName'
            ResultType = 'String'
            OnEval = dwsTIdEMailAddressItemMethodsGetNameEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetName'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdEMailAddressItemMethodsSetNameEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetText'
            ResultType = 'String'
            OnEval = dwsTIdEMailAddressItemMethodsGetTextEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetText'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdEMailAddressItemMethodsSetTextEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetDomain'
            ResultType = 'String'
            OnEval = dwsTIdEMailAddressItemMethodsGetDomainEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetDomain'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdEMailAddressItemMethodsSetDomainEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUser'
            ResultType = 'String'
            OnEval = dwsTIdEMailAddressItemMethodsGetUserEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUser'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdEMailAddressItemMethodsSetUserEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'Address'
            DataType = 'String'
            ReadAccess = 'GetAddress'
            WriteAccess = 'SetAddress'
          end
          item
            Name = 'Name'
            DataType = 'String'
            ReadAccess = 'GetName'
            WriteAccess = 'SetName'
          end
          item
            Name = 'Text'
            DataType = 'String'
            ReadAccess = 'GetText'
            WriteAccess = 'SetText'
          end
          item
            Name = 'Domain'
            DataType = 'String'
            ReadAccess = 'GetDomain'
            WriteAccess = 'SetDomain'
          end
          item
            Name = 'User'
            DataType = 'String'
            ReadAccess = 'GetUser'
            WriteAccess = 'SetUser'
          end>
      end
      item
        Name = 'TIdEMailAddressList'
        IsSealed = True
        Methods = <
          item
            Name = 'GetEMailAddresses'
            ResultType = 'string'
            OnEval = dwsTIdEMailAddressListMethodsGetEMailAddressesEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetEMailAddresses'
            Parameters = <
              item
                Name = 'value'
                DataType = 'string'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdEMailAddressListMethodsSetEMailAddressesEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetItem'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end>
            ResultType = 'TIdEMailAddressItem'
            OnEval = dwsTIdEMailAddressListMethodsGetItemEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetItem'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsVarParam = True
                IsWritable = False
              end
              item
                Name = 'value'
                DataType = 'TIdEMailAddressItem'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsTIdEMailAddressListMethodsSetItemEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'SortByDomain'
            OnEval = dwsTIdEMailAddressListMethodsSortByDomainEval
            Kind = mkProcedure
          end
          item
            Name = 'Add'
            ResultType = 'TIdEMailAddressItem'
            OnEval = dwsTIdEMailAddressListMethodsAddEval
            Kind = mkFunction
          end>
        Properties = <
          item
            Name = 'EMailAddresses'
            DataType = 'String'
            ReadAccess = 'GetEMailAddresses'
            WriteAccess = 'SetEMailAddresses'
          end
          item
            Name = 'Items'
            DataType = 'TIdEMailAddressItem'
            ReadAccess = 'GetItem'
            WriteAccess = 'SetItem'
            IndexType = 'Integer'
            IndexValue = 'Index'
          end>
      end
      item
        Name = 'TIdMailMessage'
        IsSealed = True
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsTIdMailMessageConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'GetEncoding'
            ResultType = 'TMessageEncoding'
            OnEval = dwsTIdMailMessageMethodsGetEncodingEval
            Kind = mkFunction
          end
          item
            Name = 'SetEncoding'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TMessageEncoding'
              end>
            OnEval = dwsTIdMailMessageMethodsSetEncodingEval
            Kind = mkProcedure
          end
          item
            Name = 'GetFlags'
            ResultType = 'TMessageFlags'
            OnEval = dwsTIdMailMessageMethodsGetFlagsEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetFlags'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TMessageFlags'
              end>
            OnEval = dwsTIdMailMessageMethodsSetFlagsEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetIsEncoded'
            ResultType = 'Boolean'
            OnEval = dwsTIdMailMessageMethodsGetIsEncodedEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetIsEncoded'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdMailMessageMethodsSetIsEncodedEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetMsgID'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetMsgIDEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetMsgID'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetMsgIDEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetIsMsgSinglePartMime'
            ResultType = 'Boolean'
            OnEval = dwsTIdMailMessageMethodsGetIsMsgSinglePartMimeEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetIsMsgSinglePartMime'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdMailMessageMethodsSetIsMsgSinglePartMimeEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetAttachmentEncoding'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetAttachmentEncodingEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetAttachmentEncoding'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetAttachmentEncodingEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetBody'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetBodyEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetBody'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetBodyEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetCharset'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetCharsetEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetCharset'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetCharsetEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetContentType'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetContentTypeEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetContentType'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetContentTypeEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetContentTransferEncoding'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetContentTransferEncodingEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetContentTransferEncoding'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsetContentTransferEncodingEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetContentDisposition'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetContentDispositionEval
            Kind = mkFunction
          end
          item
            Name = 'SetContentDisposition'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetContentDispositionEval
            Kind = mkProcedure
          end
          item
            Name = 'GetNoEncode'
            ResultType = 'Boolean'
            OnEval = dwsTIdMailMessageMethodsGetNoEncodeEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetNoEncode'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdMailMessageMethodsSetNoEncodeEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetNoDecode'
            ResultType = 'Boolean'
            OnEval = dwsTIdMailMessageMethodsGetNoDecodeEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetNoDecode'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdMailMessageMethodsSetNoDecodeEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetOrganization'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetOrganizationEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetOrganization'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetOrganizationEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetReferences'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetReferencesEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetReferences'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetReferencesEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetInReplyTo'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetInReplyToEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetInReplyTo'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetInReplyToEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetSubject'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetSubjectEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetSubject'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetSubjectEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseNowForDate'
            ResultType = 'Boolean'
            OnEval = dwsTIdMailMessageMethodsGetUseNowForDateEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseNowForDate'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdMailMessageMethodsSetUseNowForDateEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetConvertPreamble'
            ResultType = 'Boolean'
            OnEval = dwsTIdMailMessageMethodsGetConvertPreambleEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetConvertPreamble'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdMailMessageMethodsSetConvertPreambleEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetAttachmentTempDir'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetAttachmentTempDirEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetAttachmentTempDir'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetAttachmentTempDirEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUID'
            ResultType = 'String'
            OnEval = dwsTIdMailMessageMethodsGetUIDEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUID'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSetUIDEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'AddHeader'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsAddHeaderEval
            Kind = mkProcedure
          end
          item
            Name = 'Clear'
            OnEval = dwsTIdMailMessageMethodsClearEval
            Kind = mkProcedure
          end
          item
            Name = 'ClearBody'
            OnEval = dwsTIdMailMessageMethodsClearBodyEval
            Kind = mkProcedure
          end
          item
            Name = 'ClearHeader'
            OnEval = dwsTIdMailMessageMethodsClearHeaderEval
            Kind = mkProcedure
          end
          item
            Name = 'GenerateHeader'
            OnEval = dwsTIdMailMessageMethodsGenerateHeaderEval
            Kind = mkProcedure
          end
          item
            Name = 'LoadFromFile'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
              end
              item
                Name = 'HeadersOnly'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdMailMessageMethodsLoadFromFileEval
            Kind = mkProcedure
          end
          item
            Name = 'ProcessHeaders'
            OnEval = dwsTIdMailMessageMethodsProcessHeadersEval
            Kind = mkProcedure
          end
          item
            Name = 'SaveToFile'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
              end
              item
                Name = 'HeadersOnly'
                DataType = 'String'
              end>
            OnEval = dwsTIdMailMessageMethodsSaveToFileEval
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'Flags'
            DataType = 'TMessageFlags'
            ReadAccess = 'GetFlags'
            WriteAccess = 'SetFlags'
          end
          item
            Name = 'IsEncoded'
            DataType = 'Boolean'
            ReadAccess = 'GetIsEncoded'
            WriteAccess = 'SetIsEncoded'
          end
          item
            Name = 'MsgId'
            DataType = 'String'
            ReadAccess = 'GetMsgId'
            WriteAccess = 'SetMsgId'
          end
          item
            Name = 'UID'
            DataType = 'String'
            ReadAccess = 'GetUID'
            WriteAccess = 'SetUID'
          end
          item
            Name = 'IsMsgSinglePartMime'
            DataType = 'Boolean'
            ReadAccess = 'GetIsMsgSinglePartMime'
            WriteAccess = 'SetIsMsgSinglePartMime'
          end
          item
            Name = 'AttachmentEncoding'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetAttachmentEncoding'
            WriteAccess = 'SetAttachmentEncoding'
          end
          item
            Name = 'Body'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetBody'
            WriteAccess = 'SetBody'
          end
          item
            Name = 'CharSet'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'SetCharSet'
            WriteAccess = 'GetCharSet'
          end
          item
            Name = 'ContentType'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetContentType'
            WriteAccess = 'SetContentType'
          end
          item
            Name = 'ContentTransferEncoding'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetContentTransferEncoding'
            WriteAccess = 'SetContentTransferEncoding'
          end
          item
            Name = 'ContentDisposition'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetContentDisposition'
            WriteAccess = 'SetContentDisposition'
          end
          item
            Name = 'NoEncode'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetNoEncode'
            WriteAccess = 'SetNoEncode'
          end
          item
            Name = 'NoDecode'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetNoDecode'
            WriteAccess = 'SetNoDecode'
          end
          item
            Name = 'Organization'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetOrganization'
            WriteAccess = 'SetOrganization'
          end
          item
            Name = 'References'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetReferences'
            WriteAccess = 'SetReferences'
          end
          item
            Name = 'InReplyTo'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetInReplyTo'
            WriteAccess = 'SetInReplyTo'
          end
          item
            Name = 'Subject'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetSubject'
            WriteAccess = 'SetSubject'
          end
          item
            Name = 'UseNowForDate'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetUseNowForDate'
            WriteAccess = 'SetUseNowForDate'
          end
          item
            Name = 'ConvertPreamble'
            DataType = 'Boolean'
            Visibility = cvPublished
            ReadAccess = 'GetConvertPreamble'
            WriteAccess = 'SetConvertPreamble'
          end
          item
            Name = 'AttachmentTempDirectory'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetAttachmentTempDir'
            WriteAccess = 'SetAttachmentTempDir'
          end
          item
            Name = 'Encoding'
            DataType = 'TMessageEncoding'
            ReadAccess = 'GetEncoding'
            WriteAccess = 'SetEncoding'
          end>
        OnCleanUp = dwsTIdMailMessageCleanUp
      end
      item
        Name = 'TIdSMTP'
        IsSealed = True
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsTIdSMTPConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'GetMailAgent'
            ResultType = 'String'
            OnEval = dwsTIdSMTPMethodsGetMailAgentEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetMailAgent'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdSMTPMethodsSetMailAgentEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetHeloName'
            ResultType = 'String'
            OnEval = dwsTIdSMTPMethodsGetHeloNameEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetHeloName'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdSMTPMethodsSetHeloNameEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseEhlo'
            ResultType = 'Boolean'
            OnEval = dwsTIdSMTPMethodsGetUseEhloEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseEhlo'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdSMTPMethodsSetUseEhloEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPipeLine'
            ResultType = 'Boolean'
            OnEval = dwsTIdSMTPMethodsGetPipeLineEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPipeLine'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdSMTPMethodsSetPipeLineEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseVerp'
            ResultType = 'Boolean'
            OnEval = dwsTIdSMTPMethodsGetUseVerpEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseVerp'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdSMTPMethodsSetUseVerpEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetVerpDelims'
            ResultType = 'String'
            OnEval = dwsTIdSMTPMethodsGetVerpDelimsEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetVerpDelims'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdSMTPMethodsSetVerpDelimsEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetDidAuthenticate'
            ResultType = 'Boolean'
            OnEval = dwsTIdSMTPMethodsGetDidAuthenticateEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUsername'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdSMTPMethodsSetUsernameEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUsername'
            ResultType = 'String'
            OnEval = dwsTIdSMTPMethodsGetUsernameEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPassword'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdSMTPMethodsSetPasswordEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPassword'
            ResultType = 'String'
            OnEval = dwsTIdSMTPMethodsGetPasswordEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetPort'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
              end>
            OnEval = dwsTIdSMTPMethodsSetPortEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetPort'
            ResultType = 'Integer'
            OnEval = dwsTIdSMTPMethodsGetPortEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetUseTLS'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TUseTls'
              end>
            OnEval = dwsTIdSMTPMethodsSetUseTLSEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetUseTLS'
            ResultType = 'TUseTls'
            OnEval = dwsTIdSMTPMethodsGetUseTLSEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetValidateAuthLoginCapability'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            OnEval = dwsTIdSMTPMethodsSetValidateAuthLoginCapabilityEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetValidateAuthLoginCapability'
            ResultType = 'Boolean'
            OnEval = dwsTIdSMTPMethodsGetValidateAuthLoginCapabilityEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetAuthType'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TSMTPAuthenticationType'
              end>
            OnEval = dwsTIdSMTPMethodsSetAuthTypeEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetAuthType'
            ResultType = 'TSMTPAuthenticationType'
            OnEval = dwsTIdSMTPMethodsGetAuthTypeEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'SetHost'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            OnEval = dwsTIdSMTPMethodsSetHostEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'GetHost'
            ResultType = 'String'
            OnEval = dwsTIdSMTPMethodsGetHostEval
            Visibility = cvPrivate
            Kind = mkFunction
          end
          item
            Name = 'Connect'
            OnEval = dwsTIdSMTPMethodsConnectEval
            Kind = mkProcedure
          end
          item
            Name = 'Disconnect'
            OnEval = dwsTIdSMTPMethodsDisconnectEval
            Kind = mkProcedure
          end
          item
            Name = 'Verify'
            Parameters = <
              item
                Name = 'Username'
                DataType = 'String'
              end>
            ResultType = 'String'
            OnEval = dwsTIdSMTPMethodsVerifyEval
            Kind = mkFunction
          end
          item
            Name = 'Authenticate'
            ResultType = 'Boolean'
            OnEval = dwsTIdSMTPMethodsAuthenticateEval
            Kind = mkFunction
          end
          item
            Name = 'Send'
            Parameters = <
              item
                Name = 'MailMessage'
                DataType = 'TIdMailMessage'
              end>
            OnEval = dwsTIdSMTPMethodsSendEval
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'Host'
            DataType = 'String'
            Visibility = cvPublished
            ReadAccess = 'GetHost'
            WriteAccess = 'SetHost'
          end
          item
            Name = 'Username'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'Password'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'Port'
            DataType = 'Integer'
            Visibility = cvPublished
          end
          item
            Name = 'UseTLS'
            DataType = 'TUseTls'
            Visibility = cvPublished
          end
          item
            Name = 'ValidateAuthLoginCapability'
            DataType = 'Boolean'
            Visibility = cvPublished
          end
          item
            Name = 'AuthType'
            DataType = 'TSMTPAuthenticationType'
            Visibility = cvPublished
          end
          item
            Name = 'DidAuthenticate'
            DataType = 'Boolean'
            ReadAccess = 'GetDidAuthenticate'
          end
          item
            Name = 'MailAgent'
            DataType = 'String'
            ReadAccess = 'GetMailAgent'
            WriteAccess = 'SetMailAgent'
          end
          item
            Name = 'HeloName'
            DataType = 'String'
            ReadAccess = 'GetHeloName'
            WriteAccess = 'SetHeloName'
          end
          item
            Name = 'UseEhlo'
            DataType = 'Boolean'
            ReadAccess = 'GetUseEhlo'
            WriteAccess = 'SetUseEhlo'
          end
          item
            Name = 'PipeLine'
            DataType = 'Boolean'
            ReadAccess = 'GetPipeLine'
            WriteAccess = 'SetPipeLine'
          end
          item
            Name = 'VerpDelims'
            DataType = 'String'
            ReadAccess = 'GetVerpDelims'
            WriteAccess = 'SetVerpDelims'
          end>
        OnCleanUp = dwsTIdSMTPCleanUp
      end>
    Enumerations = <
      item
        Name = 'TSMTPAuthenticationType'
        Elements = <
          item
            Name = 'None'
          end
          item
            Name = 'Default'
          end
          item
            Name = 'SASL'
          end>
        Style = enumScoped
      end
      item
        Name = 'TUseTls'
        Elements = <
          item
            Name = 'NoSupport'
          end
          item
            Name = 'UseImplicit'
          end
          item
            Name = 'UseRequire'
          end
          item
            Name = 'UseExplicit'
          end>
        Style = enumScoped
      end
      item
        Name = 'TMessageEncoding'
        Elements = <
          item
            Name = 'Default'
          end
          item
            Name = 'MIME'
          end
          item
            Name = 'PlainText'
          end>
        Style = enumScoped
      end
      item
        Name = 'TMessageFlags'
        Elements = <
          item
            Name = 'Answered'
          end
          item
            Name = 'Flagged'
          end
          item
            Name = 'Deleted'
          end
          item
            Name = 'Draft'
          end
          item
            Name = 'Seen'
          end
          item
            Name = 'Recent'
          end>
        Style = enumFlags
      end
      item
        Name = 'TFtpProxyType'
        Elements = <
          item
            Name = 'None'
          end
          item
            Name = 'UserSite'
          end
          item
            Name = 'Site'
          end
          item
            Name = 'Open'
          end
          item
            Name = 'UserPass'
          end
          item
            Name = 'Transparent'
          end
          item
            Name = 'UserHostFireWallID'
          end
          item
            Name = 'NovellBorder'
          end
          item
            Name = 'HttpProxyWithFtp'
          end
          item
            Name = 'CustomProxy'
          end>
        Style = enumScoped
      end
      item
        Name = 'TAuthCmd'
        Elements = <
          item
            Name = 'Auto'
          end
          item
            Name = 'TLS'
          end
          item
            Name = 'SSL'
          end
          item
            Name = 'TLSC'
          end
          item
            Name = 'TLSP'
          end>
        Style = enumScoped
      end
      item
        Name = 'TIPVersion'
        Elements = <
          item
            Name = 'IPv4'
          end
          item
            Name = 'IPv6'
          end>
        Style = enumScoped
      end
      item
        Name = 'TReuseSocket'
        Elements = <
          item
            Name = 'OSDependent'
          end
          item
            Name = 'rsTrue'
          end
          item
            Name = 'rsFalse'
          end>
        Style = enumScoped
      end
      item
        Name = 'TFTPDataPortSecurity'
        Elements = <
          item
            Name = 'Clear'
          end
          item
            Name = 'Private'
          end>
        Style = enumScoped
      end
      item
        Name = 'TFTPTransferType'
        Elements = <
          item
            Name = 'ASCII'
          end
          item
            Name = 'Binary'
          end>
        Style = enumScoped
      end
      item
        Name = 'TFTPDataStructure'
        Elements = <
          item
            Name = 'File'
          end
          item
            Name = 'Record'
          end
          item
            Name = 'Page'
          end>
        Style = enumScoped
      end
      item
        Name = 'TFTPTransferMode'
        Elements = <
          item
            Name = 'Stream'
          end
          item
            Name = 'Deflate'
          end>
        Style = enumScoped
      end>
    Functions = <
      item
        Name = 'Upload'
        Parameters = <
          item
            Name = 'Filename'
            DataType = 'String'
          end
          item
            Name = 'Host'
            DataType = 'String'
          end
          item
            Name = 'Port'
            DataType = 'Integer'
          end
          item
            Name = 'Username'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = 'anonymous'
          end
          item
            Name = 'Password'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = 'anonymous'
          end
          item
            Name = 'Path'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = '/'
          end>
        Overloaded = True
        OnEval = dwsUploadEval
      end
      item
        Name = 'Upload'
        Parameters = <
          item
            Name = 'Filename'
            DataType = 'String'
          end
          item
            Name = 'Host'
            DataType = 'String'
          end
          item
            Name = 'Username'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = 'anonymous'
          end
          item
            Name = 'Password'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = 'anonymous'
          end
          item
            Name = 'Path'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = '/'
          end>
        Overloaded = True
        OnEval = dwsUploadPortEval
      end
      item
        Name = 'SendEmail'
        Parameters = <
          item
            Name = 'Host'
            DataType = 'String'
          end
          item
            Name = 'Recipient'
            DataType = 'String'
          end
          item
            Name = 'Subject'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = ''
          end
          item
            Name = 'Body'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = ''
          end
          item
            Name = 'Attachment'
            DataType = 'String'
            HasDefaultValue = True
            DefaultValue = ''
          end>
        OnEval = dwsSendEmailEval
      end>
    UnitName = 'Networkink.Indy'
    StaticSymbols = True
    Left = 64
    Top = 24
  end
end
