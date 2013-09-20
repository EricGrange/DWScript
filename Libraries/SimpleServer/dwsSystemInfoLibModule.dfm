object dwsSystemInfoLibModule: TdwsSystemInfoLibModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 712
  Top = 139
  Height = 135
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
            Name = 'FrequencyMHz'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsFrequencyMHzEval
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
          end>
      end>
    Records = <
      item
        Name = 'MemoryStatusDetail'
        Members = <
          item
            Name = 'Total'
            DataType = 'Integer'
          end
          item
            Name = 'Available'
            DataType = 'Integer'
          end>
        Properties = <>
      end>
    UnitName = 'System.Info'
    StaticSymbols = True
    Left = 56
    Top = 24
  end
end
