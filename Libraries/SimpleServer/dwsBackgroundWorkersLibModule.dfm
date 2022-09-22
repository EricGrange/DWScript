object dwsBackgroundWorkersLib: TdwsBackgroundWorkersLib
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 129
  Width = 220
  object dwsBackgroundWorkers: TdwsUnit
    Classes = <
      item
        Name = 'BackgroundWorkers'
        IsStatic = True
        Methods = <
          item
            Name = 'CreateWorkQueue'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsBackgroundWorkersClassesBackgroundWorkersMethodsCreateWorkQueueEval
          end
          item
            Name = 'DestroyWorkQueue'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsBackgroundWorkersClassesBackgroundWorkersMethodsDestroyWorkQueueEval
          end
          item
            Name = 'QueueWork'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'task'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueWorkEval
          end
          item
            Name = 'QueueSize'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueSizeEval
          end
          item
            Name = 'QueueDelayedWork'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'delaySeconds'
                DataType = 'Float'
              end
              item
                Name = 'task'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueDelayedWorkEval
          end
          item
            Name = 'GetWorkerCount'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsBackgroundWorkersClassesBackgroundWorkersMethodsGetWorkerCountEval
          end
          item
            Name = 'SetWorkerCount'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'n'
                DataType = 'Integer'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = dwsBackgroundWorkersClassesBackgroundWorkersMethodsSetWorkerCountEval
          end
          item
            Name = 'WorkQueueStatusAsJSON'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'resetPeakStats'
                DataType = 'Boolean'
                HasDefaultValue = True
                DefaultValue = False
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueStatusAsJSONEval
          end>
        Properties = <
          item
            Name = 'WorkerCount'
            DataType = 'Integer'
            ReadAccess = 'GetWorkerCount'
            WriteAccess = 'SetWorkerCount'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
          end>
      end>
    UnitName = 'System.Workers'
    StaticSymbols = True
    Left = 80
    Top = 16
  end
end
