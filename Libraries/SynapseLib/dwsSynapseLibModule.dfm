object dwsSynapseLib: TdwsSynapseLib
  OldCreateOrder = False
  Left = 646
  Top = 86
  Height = 150
  Width = 215
  object dwsSynapse: TdwsUnit
    Classes = <
      item
        Name = 'SMTPMail'
        IsStatic = True
        Methods = <
          item
            Name = 'Send'
            Parameters = <
              item
                Name = 'from'
                DataType = 'String'
              end
              item
                Name = 'sendTo'
                DataType = 'String'
              end
              item
                Name = 'subject'
                DataType = 'String'
              end
              item
                Name = 'body'
                DataType = 'String'
              end
              item
                Name = 'additionalHeaders'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            Attributes = [maStatic]
            OnEval = dwsSynapseClassesSMTPMailMethodsSendEval
            Kind = mkClassProcedure
          end>
      end
      item
        Name = 'ESynapseException'
        Ancestor = 'Exception'
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
            OnEval = dwsSynapseClassesHttpQueryMethodsGetTextEval
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
            OnEval = dwsSynapseClassesHttpQueryMethodsGetDataEval
            Kind = mkClassFunction
          end>
      end>
    UnitName = 'Networking.Synapse'
    StaticSymbols = True
    Left = 72
    Top = 24
  end
end
