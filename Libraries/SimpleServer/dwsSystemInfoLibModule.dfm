object dwsSystemInfoLibModule: TdwsSystemInfoLibModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 205
  Width = 165
  object dwsSystemInfo: TdwsUnit
    Classes = <
      item
        Name = 'MemoryStatus'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsSystemInfoClassesMemoryStatusConstructorsCreateEval
          end>
        Fields = <
          item
            Name = 'Physical'
            DataType = 'MemoryStatusDetail'
          end
          item
            Name = 'Virtual'
            DataType = 'MemoryStatusDetail'
          end
          item
            Name = 'PageFile'
            DataType = 'MemoryStatusDetail'
          end>
      end
      item
        Name = 'OSVersionInfo'
        IsStatic = True
        Methods = <
          item
            Name = 'Name'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesOSVersionInfoMethodsNameEval
            Kind = mkClassFunction
          end
          item
            Name = 'Version'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesOSVersionInfoMethodsVersionEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'CPUInfo'
        IsStatic = True
        Methods = <
          item
            Name = 'Name'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsNameEval
            Kind = mkClassFunction
          end
          item
            Name = 'Count'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsCountEval
            Kind = mkClassFunction
          end
          item
            Name = 'FrequencyMHz'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsFrequencyMHzEval
            Kind = mkClassFunction
          end
          item
            Name = 'SystemUsage'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsSystemUsageEval
            Kind = mkClassFunction
          end
          item
            Name = 'KernelUsage'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsKernelUsageEval
            Kind = mkClassFunction
          end
          item
            Name = 'ProcessUsage'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsProcessUsageEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'ApplicationInfo'
        IsStatic = True
        Methods = <
          item
            Name = 'Version'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsVersionEval
            Kind = mkClassFunction
          end
          item
            Name = 'ExeName'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsExeNameEval
            Kind = mkClassFunction
          end
          item
            Name = 'RunningAsService'
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsRunningAsServiceEval
            Kind = mkClassFunction
          end
          item
            Name = 'GetEnvironmentVariable'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsGetEnvironmentVariableEval
            Kind = mkClassFunction
          end
          item
            Name = 'SetEnvironmentVariable'
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
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsSetEnvironmentVariableEval
            Kind = mkClassProcedure
          end
          item
            Name = 'MemoryCounters'
            ResultType = 'ProcessMemoryCounters'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsMemoryCountersEval
            Kind = mkClassFunction
          end
          item
            Name = 'UserName'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsUserNameEval
            Kind = mkClassFunction
          end
          item
            Name = 'ExeLinkTimeStamp'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsExeLinkTimeEval
            Kind = mkClassFunction
          end
          item
            Name = 'IsDebuggerPresent'
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethods8Eval
            Kind = mkClassFunction
          end
          item
            Name = 'GetCurrentDirectory'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsGetCurrentDirectoryEval
            Kind = mkClassFunction
          end
          item
            Name = 'SetCurrentDirectory'
            Parameters = <
              item
                Name = 'newDirectory'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesApplicationInfoMethodsSetCurrentDirectoryEval
            Kind = mkClassProcedure
          end>
        Properties = <
          item
            Name = 'EnvironmentVariable'
            DataType = 'String'
            ReadAccess = 'GetEnvironmentVariable'
            WriteAccess = 'SetEnvironmentVariable'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
          end
          item
            Name = 'CurrentDirectory'
            DataType = 'String'
            ReadAccess = 'GetCurrentDirectory'
            WriteAccess = 'SetCurrentDirectory'
          end>
      end
      item
        Name = 'HostInfo'
        IsStatic = True
        Methods = <
          item
            Name = 'Name'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesHostInfoMethodsNameEval
            Kind = mkClassFunction
          end
          item
            Name = 'DNSDomain'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesHostInfoMethodsDNSDomainEval
            Kind = mkClassFunction
          end
          item
            Name = 'DNSFullyQualified'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesHostInfoMethodsDNSFullyQualifiedEval
            Kind = mkClassFunction
          end
          item
            Name = 'DNSName'
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesHostInfoMethodsDNSNameEval
            Kind = mkClassFunction
          end
          item
            Name = 'SystemTime'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsSystemTimeEval
            Kind = mkClassFunction
          end
          item
            Name = 'KernelTime'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsKernelTimeEval
            Kind = mkClassFunction
          end
          item
            Name = 'ProcessTime'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsProcessTimeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Uptime'
            ResultType = 'Float'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsUpTimeEval
            Kind = mkClassFunction
          end
          item
            Name = 'DomainControllerInfo'
            ResultType = 'DomainControllerInfo'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesHostInfoMethodsDomainControllerInfoEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'PerformanceCounter'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsSystemInfoClassesPerformanceCounterConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Now'
            ResultType = 'Float'
            Attributes = [maClassMethod, maStatic]
            OnFastEvalFloat = dwsSystemInfoClassesPerformanceCounterMethodsNowFastEvalFloat
            Kind = mkClassFunction
          end
          item
            Name = 'Restart'
            OnFastEvalNoResult = dwsSystemInfoClassesPerformanceCounterMethodsRestartFastEvalNoResult
            Kind = mkProcedure
          end
          item
            Name = 'Stop'
            OnFastEvalNoResult = dwsSystemInfoClassesPerformanceCounterMethodsStopFastEvalNoResult
            Kind = mkProcedure
          end
          item
            Name = 'Elapsed'
            ResultType = 'Float'
            OnFastEvalFloat = dwsSystemInfoClassesPerformanceCounterMethodsElapsedFastEvalFloat
            Kind = mkFunction
          end
          item
            Name = 'Running'
            ResultType = 'Boolean'
            OnFastEvalBoolean = dwsSystemInfoClassesPerformanceCounterMethodsRunningFastEvalBoolean
            Kind = mkFunction
          end>
        Constants = <
          item
            Name = 'Frequency'
            DataType = 'Float'
            Value = '0'
          end>
        OnCleanUp = dwsSystemInfoClassesPerformanceCounterCleanUp
      end
      item
        Name = 'ThreadInfo'
        IsStatic = True
        Methods = <
          item
            Name = 'ID'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesThreadInfoMethodsIDEval
            Kind = mkClassFunction
          end
          item
            Name = 'Priority'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesThreadInfoMethodsPriorityEval
            Kind = mkClassFunction
          end
          item
            Name = 'IsDebugging'
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesThreadInfoMethodsIsDebuggingEval
            Kind = mkClassFunction
          end>
      end>
    Functions = <
      item
        Name = 'SystemMilliseconds'
        ResultType = 'Integer'
        OnFastEval = dwsSystemInfoFunctionsSystemMillisecondsFastEval
      end>
    Records = <
      item
        Name = 'MemoryStatusDetail'
        Members = <
          item
            Name = 'Total'
            DataType = 'Integer'
            Visibility = cvPublished
          end
          item
            Name = 'Available'
            DataType = 'Integer'
            Visibility = cvPublished
          end>
        Properties = <>
      end
      item
        Name = 'ProcessMemoryCounters'
        Members = <
          item
            Name = 'WorkingSetSize'
            DataType = 'Integer'
            Visibility = cvPublished
          end
          item
            Name = 'PeakWorkingSetSize'
            DataType = 'Integer'
            Visibility = cvPublished
          end
          item
            Name = 'PagefileUsage'
            DataType = 'Integer'
            Visibility = cvPublished
          end
          item
            Name = 'PeakPagefileUsage'
            DataType = 'Integer'
            Visibility = cvPublished
          end>
        Properties = <>
      end
      item
        Name = 'DomainControllerInfo'
        Members = <
          item
            Name = 'DCName'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'DCAddress'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'DCAddressType'
            DataType = 'Integer'
            Visibility = cvPublished
          end
          item
            Name = 'GUID'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'Name'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'ForestName'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'DCSiteName'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'ClientSiteName'
            DataType = 'String'
            Visibility = cvPublished
          end
          item
            Name = 'Flags'
            DataType = 'Integer'
            Visibility = cvPublished
          end>
        Properties = <>
      end>
    UnitName = 'System.Info'
    StaticSymbols = True
    Left = 56
    Top = 24
  end
  object dwsSystemRegistry: TdwsUnit
    Classes = <
      item
        Name = 'Registry'
        IsStatic = True
        Methods = <
          item
            Name = 'ReadValue'
            Parameters = <
              item
                Name = 'root'
                DataType = 'HKEY'
              end
              item
                Name = 'path'
                DataType = 'String'
              end
              item
                Name = 'valueName'
                DataType = 'String'
              end
              item
                Name = 'default'
                DataType = 'Variant'
                HasDefaultValue = True
              end>
            ResultType = 'Variant'
            Attributes = [maStatic]
            OnEval = dwsSystemRegistryClassesRegistryMethodsReadValueEval
            Kind = mkClassFunction
          end
          item
            Name = 'WriteValue'
            Parameters = <
              item
                Name = 'root'
                DataType = 'HKEY'
              end
              item
                Name = 'path'
                DataType = 'String'
              end
              item
                Name = 'valueName'
                DataType = 'String'
              end
              item
                Name = 'value'
                DataType = 'Variant'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsSystemRegistryClassesRegistryMethodsWriteValueEval
            Kind = mkClassFunction
          end
          item
            Name = 'DeleteValue'
            Parameters = <
              item
                Name = 'root'
                DataType = 'HKEY'
              end
              item
                Name = 'path'
                DataType = 'String'
              end
              item
                Name = 'valueName'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsSystemRegistryClassesRegistryMethodsDeleteValueEval
            Kind = mkClassFunction
          end
          item
            Name = 'CreateKey'
            Parameters = <
              item
                Name = 'root'
                DataType = 'HKEY'
              end
              item
                Name = 'path'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsSystemRegistryClassesRegistryMethodsCreateKeyEval
            Kind = mkClassFunction
          end
          item
            Name = 'DeleteKey'
            Parameters = <
              item
                Name = 'root'
                DataType = 'HKEY'
              end
              item
                Name = 'path'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsSystemRegistryClassesRegistryMethodsDeleteKeyEval
            Kind = mkClassFunction
          end
          item
            Name = 'SubKeys'
            Parameters = <
              item
                Name = 'root'
                DataType = 'HKEY'
              end
              item
                Name = 'path'
                DataType = 'String'
              end>
            ResultType = 'array of String'
            Attributes = [maStatic]
            OnEval = dwsSystemRegistryClassesRegistryMethodsSubKeysEval
            Kind = mkClassFunction
          end
          item
            Name = 'ValueNames'
            Parameters = <
              item
                Name = 'root'
                DataType = 'HKEY'
              end
              item
                Name = 'path'
                DataType = 'String'
              end>
            ResultType = 'array of String'
            Attributes = [maStatic]
            OnEval = dwsSystemRegistryClassesRegistryMethodsValueNamesEval
            Kind = mkClassFunction
          end>
      end>
    Enumerations = <
      item
        Name = 'HKEY'
        Elements = <
          item
            Name = 'ClassesRoot'
            UserDefValue = -2147483648
            IsUserDef = True
          end
          item
            Name = 'CurrentUser'
            UserDefValue = -2147483647
            IsUserDef = True
          end
          item
            Name = 'LocalMachine'
            UserDefValue = -2147483646
            IsUserDef = True
          end
          item
            Name = 'Users'
            UserDefValue = -2147483645
            IsUserDef = True
          end
          item
            Name = 'PerformanceData'
            UserDefValue = -2147483644
            IsUserDef = True
          end
          item
            Name = 'CurrentConfig'
            UserDefValue = -2147483643
            IsUserDef = True
          end
          item
            Name = 'DynData'
            UserDefValue = -2147483642
            IsUserDef = True
          end>
        Style = enumScoped
      end>
    UnitName = 'System.Registry'
    StaticSymbols = True
    Left = 56
    Top = 88
  end
end
