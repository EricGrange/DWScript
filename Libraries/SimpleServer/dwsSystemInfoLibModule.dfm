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
            Name = 'Count'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsCountEval
            Kind = mkClassFunction
          end
          item
            Name = 'SystemUsage'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsSystemUsageEval
            Kind = mkClassFunction
          end
          item
            Name = 'KernelUsage'
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsSystemInfoClassesCPUInfoMethodsKernelUsageEval
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
    StaticSymbols = False
    Left = 56
    Top = 24
  end
end
