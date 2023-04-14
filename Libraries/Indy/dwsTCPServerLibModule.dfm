object dwsSystemTCPServerLib: TdwsSystemTCPServerLib
  OldCreateOrder = False
  Height = 150
  Width = 215
  object dwsTCPServerModule: TdwsUnit
    Classes = <
      item
        Name = 'TCPServers'
        IsStatic = True
        Methods = <
          item
            Name = 'ServerNames'
            ResultType = 'array of String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsServerNamesEval
          end
          item
            Name = 'ConnectionsCount'
            Parameters = <
              item
                Name = 'serverName'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsConnectionsCountEval
          end
          item
            Name = 'CreateServer'
            Parameters = <
              item
                Name = 'serverName'
                DataType = 'String'
              end
              item
                Name = 'options'
                DataType = 'TCPServerOptions'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsCreateServerEval
          end
          item
            Name = 'ConnectionIDs'
            Parameters = <
              item
                Name = 'serverName'
                DataType = 'String'
              end>
            ResultType = 'array of String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsConnectionIDsEval
          end
          item
            Name = 'SendData'
            Parameters = <
              item
                Name = 'connectionID'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsSendDataEval
          end
          item
            Name = 'ReceiveData'
            Parameters = <
              item
                Name = 'connectionID'
                DataType = 'String'
              end
              item
                Name = 'maxBytes'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = -1
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsReceiveDataEval
          end
          item
            Name = 'DestroyServer'
            Parameters = <
              item
                Name = 'serverName'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsDestroyServerEval
          end
          item
            Name = 'Disconnect'
            Parameters = <
              item
                Name = 'connectionID'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsDisconnectEval
          end
          item
            Name = 'ConnectionStatus'
            Parameters = <
              item
                Name = 'connectionID'
                DataType = 'String'
              end>
            ResultType = 'TCPConnectionStatus'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsTCPServerModuleClassesTCPServersMethodsConnectionStatusEval
          end>
      end>
    Records = <
      item
        Name = 'TCPServerOptions'
        Members = <
          item
            Name = 'Port'
            DataType = 'Integer'
          end
          item
            Name = 'MaxConnections'
            DataType = 'Integer'
          end
          item
            Name = 'BindIPs'
            DataType = 'array of String'
          end
          item
            Name = 'AllowedRemoteIPs'
            DataType = 'array of String'
          end
          item
            Name = 'KeepAliveTime'
            DataType = 'Integer'
          end
          item
            Name = 'KeepAliveInterval'
            DataType = 'Integer'
          end
          item
            Name = 'DisableNagle'
            DataType = 'Boolean'
          end
          item
            Name = 'OnConnect'
            DataType = 'String'
          end
          item
            Name = 'OnDisconnect'
            DataType = 'String'
          end
          item
            Name = 'OnData'
            DataType = 'String'
          end>
        Properties = <>
      end
      item
        Name = 'TCPConnectionStatus'
        Members = <
          item
            Name = 'ID'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'Connected'
            DataType = 'Boolean'
            Visibility = cvPublished
          end
          item
            Name = 'InputSize'
            DataType = 'Integer'
          end>
        Properties = <>
      end>
    UnitName = 'System.TCPServer'
    StaticSymbols = True
    Left = 88
    Top = 24
  end
end
