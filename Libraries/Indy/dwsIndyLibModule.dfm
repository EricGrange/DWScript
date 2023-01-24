object dwsIndyLib: TdwsIndyLib
  OldCreateOrder = False
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
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpProxySettingsMethodsGetProxyTypeEval
          end
          item
            Name = 'SetProxyType'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TFtpProxyType'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpProxySettingsMethodsSetProxyTypeEval
          end
          item
            Name = 'GetHost'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpProxySettingsMethodsGetHostEval
          end
          item
            Name = 'SetHost'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpProxySettingsMethodsSetHostEval
          end
          item
            Name = 'GetUsername'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpProxySettingsMethodsGetUsernameEval
          end
          item
            Name = 'SetUsername'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpProxySettingsMethodsSetUsernameEval
          end
          item
            Name = 'GetPassword'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpProxySettingsMethodsGetPasswordEval
          end
          item
            Name = 'SetPassword'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpProxySettingsMethodsSetPasswordEval
          end
          item
            Name = 'GetPort'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpProxySettingsMethodsGetPortEval
          end
          item
            Name = 'SetPort'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpProxySettingsMethodsSetPortEval
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
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetBoundIPEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetBoundIPEval
          end
          item
            Name = 'GetBoundPort'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetBoundPortEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetBoundPortEval
          end
          item
            Name = 'GetBoundPortMax'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetBoundPortMaxEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetBoundPortMaxEval
          end
          item
            Name = 'GetBoundPortMin'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetBoundPortMinEval
          end
          item
            Name = 'GetConnectionTimeOut'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetConnectionTimeOutEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetConnectionTimeOutEval
          end
          item
            Name = 'GetReuseSocket'
            ResultType = 'TReuseSocket'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetReuseSocketEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetReuseSocketEval
          end
          item
            Name = 'GetUseNagle'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetUseNagleEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetUseNagleEval
          end
          item
            Name = 'GetSupportsTLS'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetSupportsTLSEval
          end
          item
            Name = 'GetCapabilities'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetCapabilitiesEval
          end
          item
            Name = 'GetIPVersion'
            ResultType = 'TIPVersion'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetIPVersionEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetIPVersionEval
          end
          item
            Name = 'GetAutoIssueFEAT'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetAutoIssueFEATEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetAutoIssueFEATEval
          end
          item
            Name = 'GetHost'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetHostEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetHostEval
          end
          item
            Name = 'GetUseCCC'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetUseCCCEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetUseCCCEval
          end
          item
            Name = 'GetPassiveUseControlHost'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetPassiveUseControlHostEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetPassiveUseControlHostEval
          end
          item
            Name = 'GetDataPortProtection'
            ResultType = 'TFTPDataPortSecurity'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetDataPortProtectionEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetDataPortProtectionEval
          end
          item
            Name = 'GetAUTHCmd'
            ResultType = 'TAuthCmd'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetAUTHCmdEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetAUTHCmdEval
          end
          item
            Name = 'GetDataPort'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetDataPortEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetDataPortEval
          end
          item
            Name = 'GetDataPortMax'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetDataPortMaxEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetDataPortMaxEval
          end
          item
            Name = 'GetDataPortMin'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetDataPortMinEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetDataPortMinEval
          end
          item
            Name = 'GetExternalIP'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetExternalIPEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetExternalIPEval
          end
          item
            Name = 'GetTransferType'
            ResultType = 'TFTPTransferType'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetTransferTypeEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetTransferTypeEval
          end
          item
            Name = 'GetTransferTimeout'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetTransferTimeoutEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetTransferTimeoutEval
          end
          item
            Name = 'GetListenTimeout'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetListenTimeoutEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetListenTimeoutEval
          end
          item
            Name = 'GetUseExtensionDataPort'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetUseExtensionDataPortEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetUseExtensionDataPortEval
          end
          item
            Name = 'GetUseMLIS'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetUseMLISEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetUseMLISEval
          end
          item
            Name = 'GetTryNATFastTrack'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetTryNATFastTrackEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetTryNATFastTrackEval
          end
          item
            Name = 'GetProxySettings'
            ResultType = 'TIdFtpProxySettings'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetProxySettingsEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetProxySettingsEval
          end
          item
            Name = 'GetAccount'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetAccountEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetAccountEval
          end
          item
            Name = 'GetUseHOST'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetUseHOSTEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetUseHOSTEval
          end
          item
            Name = 'GetUseTLS'
            ResultType = 'TUseTls'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetUseTLSEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetUseTLSEval
          end
          item
            Name = 'GetReadTimeout'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetReadTimeoutEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetReadTimeoutEval
          end
          item
            Name = 'GetUserName'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetUserNameEval
          end
          item
            Name = 'SetUserName'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetUserNameEval
          end
          item
            Name = 'GetPassword'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetPasswordEval
          end
          item
            Name = 'SetPassword'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetPasswordEval
          end
          item
            Name = 'GetPort'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetPortEval
          end
          item
            Name = 'SetPort'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetPortEval
          end
          item
            Name = 'GetPassive'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetPassiveEval
          end
          item
            Name = 'SetPassive'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsSetPassiveEval
          end
          item
            Name = 'GetResumeSupported'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsGetResumeSupportedEval
          end
          item
            Name = 'Abort'
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsAbortEval
          end
          item
            Name = 'ChangeDir'
            Parameters = <
              item
                Name = 'DirName'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsChangeDirEval
          end
          item
            Name = 'ChangeDirUp'
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsChangeDirUpEval
          end
          item
            Name = 'Connect'
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsConnectEval
          end
          item
            Name = 'Delete'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsDeleteEval
          end
          item
            Name = 'Disconnect'
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsDisconnectEval
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
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsGetEval
          end
          item
            Name = 'Login'
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsLoginEval
          end
          item
            Name = 'MakeDir'
            Parameters = <
              item
                Name = 'DirName'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsMakeDirEval
          end
          item
            Name = 'Noop'
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsNoopEval
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
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsPutEval
          end
          item
            Name = 'RemoveDir'
            Parameters = <
              item
                Name = 'DirName'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsRemoveDirEval
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
            Kind = mkProcedure
            OnEval = dwsTIdFtpMethodsRenameEval
          end
          item
            Name = 'RetrieveCurrentDir'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsRetrieveCurrentDirEval
          end
          item
            Name = 'Size'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsSizeEval
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
            Kind = mkFunction
            OnEval = dwsTIdFtpMethodsCRCEval
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
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdEMailAddressItemMethodsGetAddressEval
          end
          item
            Name = 'SetAddress'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdEMailAddressItemMethodsSetAddressEval
          end
          item
            Name = 'GetName'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdEMailAddressItemMethodsGetNameEval
          end
          item
            Name = 'SetName'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdEMailAddressItemMethodsSetNameEval
          end
          item
            Name = 'GetText'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdEMailAddressItemMethodsGetTextEval
          end
          item
            Name = 'SetText'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdEMailAddressItemMethodsSetTextEval
          end
          item
            Name = 'GetDomain'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdEMailAddressItemMethodsGetDomainEval
          end
          item
            Name = 'SetDomain'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdEMailAddressItemMethodsSetDomainEval
          end
          item
            Name = 'GetUser'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdEMailAddressItemMethodsGetUserEval
          end
          item
            Name = 'SetUser'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdEMailAddressItemMethodsSetUserEval
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
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdEMailAddressListMethodsGetEMailAddressesEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdEMailAddressListMethodsSetEMailAddressesEval
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
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdEMailAddressListMethodsGetItemEval
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
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdEMailAddressListMethodsSetItemEval
          end
          item
            Name = 'SortByDomain'
            Kind = mkProcedure
            OnEval = dwsTIdEMailAddressListMethodsSortByDomainEval
          end
          item
            Name = 'Add'
            ResultType = 'TIdEMailAddressItem'
            Kind = mkFunction
            OnEval = dwsTIdEMailAddressListMethodsAddEval
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
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetEncodingEval
          end
          item
            Name = 'SetEncoding'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TMessageEncoding'
              end>
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetEncodingEval
          end
          item
            Name = 'GetFlags'
            ResultType = 'TMessageFlags'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetFlagsEval
          end
          item
            Name = 'SetFlags'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TMessageFlags'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetFlagsEval
          end
          item
            Name = 'GetIsEncoded'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetIsEncodedEval
          end
          item
            Name = 'SetIsEncoded'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetIsEncodedEval
          end
          item
            Name = 'GetMsgID'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetMsgIDEval
          end
          item
            Name = 'SetMsgID'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetMsgIDEval
          end
          item
            Name = 'GetIsMsgSinglePartMime'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetIsMsgSinglePartMimeEval
          end
          item
            Name = 'SetIsMsgSinglePartMime'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetIsMsgSinglePartMimeEval
          end
          item
            Name = 'GetAttachmentEncoding'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetAttachmentEncodingEval
          end
          item
            Name = 'SetAttachmentEncoding'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetAttachmentEncodingEval
          end
          item
            Name = 'GetBody'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetBodyEval
          end
          item
            Name = 'SetBody'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetBodyEval
          end
          item
            Name = 'GetCharset'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetCharsetEval
          end
          item
            Name = 'SetCharset'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetCharsetEval
          end
          item
            Name = 'GetContentType'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetContentTypeEval
          end
          item
            Name = 'SetContentType'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetContentTypeEval
          end
          item
            Name = 'GetContentTransferEncoding'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetContentTransferEncodingEval
          end
          item
            Name = 'SetContentTransferEncoding'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsetContentTransferEncodingEval
          end
          item
            Name = 'GetContentDisposition'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetContentDispositionEval
          end
          item
            Name = 'SetContentDisposition'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetContentDispositionEval
          end
          item
            Name = 'GetNoEncode'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetNoEncodeEval
          end
          item
            Name = 'SetNoEncode'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetNoEncodeEval
          end
          item
            Name = 'GetNoDecode'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetNoDecodeEval
          end
          item
            Name = 'SetNoDecode'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetNoDecodeEval
          end
          item
            Name = 'GetOrganization'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetOrganizationEval
          end
          item
            Name = 'SetOrganization'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetOrganizationEval
          end
          item
            Name = 'GetReferences'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetReferencesEval
          end
          item
            Name = 'SetReferences'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetReferencesEval
          end
          item
            Name = 'GetInReplyTo'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetInReplyToEval
          end
          item
            Name = 'SetInReplyTo'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetInReplyToEval
          end
          item
            Name = 'GetSubject'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetSubjectEval
          end
          item
            Name = 'SetSubject'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetSubjectEval
          end
          item
            Name = 'GetUseNowForDate'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetUseNowForDateEval
          end
          item
            Name = 'SetUseNowForDate'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetUseNowForDateEval
          end
          item
            Name = 'GetConvertPreamble'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetConvertPreambleEval
          end
          item
            Name = 'SetConvertPreamble'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetConvertPreambleEval
          end
          item
            Name = 'GetAttachmentTempDir'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetAttachmentTempDirEval
          end
          item
            Name = 'SetAttachmentTempDir'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetAttachmentTempDirEval
          end
          item
            Name = 'GetUID'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdMailMessageMethodsGetUIDEval
          end
          item
            Name = 'SetUID'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSetUIDEval
          end
          item
            Name = 'AddHeader'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsAddHeaderEval
          end
          item
            Name = 'Clear'
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsClearEval
          end
          item
            Name = 'ClearBody'
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsClearBodyEval
          end
          item
            Name = 'ClearHeader'
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsClearHeaderEval
          end
          item
            Name = 'GenerateHeader'
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsGenerateHeaderEval
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
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsLoadFromFileEval
          end
          item
            Name = 'ProcessHeaders'
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsProcessHeadersEval
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
            Kind = mkProcedure
            OnEval = dwsTIdMailMessageMethodsSaveToFileEval
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
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetMailAgentEval
          end
          item
            Name = 'SetMailAgent'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetMailAgentEval
          end
          item
            Name = 'GetHeloName'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetHeloNameEval
          end
          item
            Name = 'SetHeloName'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetHeloNameEval
          end
          item
            Name = 'GetUseEhlo'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetUseEhloEval
          end
          item
            Name = 'SetUseEhlo'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetUseEhloEval
          end
          item
            Name = 'GetPipeLine'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetPipeLineEval
          end
          item
            Name = 'SetPipeLine'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetPipeLineEval
          end
          item
            Name = 'GetUseVerp'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetUseVerpEval
          end
          item
            Name = 'SetUseVerp'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetUseVerpEval
          end
          item
            Name = 'GetVerpDelims'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetVerpDelimsEval
          end
          item
            Name = 'SetVerpDelims'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetVerpDelimsEval
          end
          item
            Name = 'GetDidAuthenticate'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetDidAuthenticateEval
          end
          item
            Name = 'SetUsername'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetUsernameEval
          end
          item
            Name = 'GetUsername'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetUsernameEval
          end
          item
            Name = 'SetPassword'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetPasswordEval
          end
          item
            Name = 'GetPassword'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetPasswordEval
          end
          item
            Name = 'SetPort'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetPortEval
          end
          item
            Name = 'GetPort'
            ResultType = 'Integer'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetPortEval
          end
          item
            Name = 'SetUseTLS'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TUseTls'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetUseTLSEval
          end
          item
            Name = 'GetUseTLS'
            ResultType = 'TUseTls'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetUseTLSEval
          end
          item
            Name = 'SetValidateAuthLoginCapability'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetValidateAuthLoginCapabilityEval
          end
          item
            Name = 'GetValidateAuthLoginCapability'
            ResultType = 'Boolean'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetValidateAuthLoginCapabilityEval
          end
          item
            Name = 'SetAuthType'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'TSMTPAuthenticationType'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetAuthTypeEval
          end
          item
            Name = 'GetAuthType'
            ResultType = 'TSMTPAuthenticationType'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetAuthTypeEval
          end
          item
            Name = 'SetHost'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
              end>
            Visibility = cvPrivate
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSetHostEval
          end
          item
            Name = 'GetHost'
            ResultType = 'String'
            Visibility = cvPrivate
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsGetHostEval
          end
          item
            Name = 'Connect'
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsConnectEval
          end
          item
            Name = 'Disconnect'
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsDisconnectEval
          end
          item
            Name = 'Verify'
            Parameters = <
              item
                Name = 'Username'
                DataType = 'String'
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsVerifyEval
          end
          item
            Name = 'Authenticate'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsTIdSMTPMethodsAuthenticateEval
          end
          item
            Name = 'Send'
            Parameters = <
              item
                Name = 'MailMessage'
                DataType = 'TIdMailMessage'
              end>
            Kind = mkProcedure
            OnEval = dwsTIdSMTPMethodsSendEval
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
