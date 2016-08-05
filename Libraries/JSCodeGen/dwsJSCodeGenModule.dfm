object DMJSCodeGenModule: TDMJSCodeGenModule
  OldCreateOrder = False
  Left = 415
  Top = 200
  Height = 150
  Width = 215
  object CodeGenLibModule: TdwsUnit
    Arrays = <
      item
        Name = 'TVariantArray'
        DataType = 'Variant'
        IsDynamic = True
      end>
    Classes = <
      item
        Name = 'CodeGen'
        IsStatic = True
        Methods = <
          item
            Name = 'RTLDependency'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = CodeGenLibModuleClassesCodeGenMethodsDependencyEval
            Kind = mkClassProcedure
          end
          item
            Name = 'NewUniqueGlobalVar'
            Parameters = <
              item
                Name = 'uniqueReference'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = CodeGenLibModuleClassesCodeGenMethodsNewUniqueGlobalVarEval
            Kind = mkClassFunction
          end
          item
            Name = 'CustomDependency'
            Parameters = <
              item
                Name = 'code'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnEval = CodeGenLibModuleClassesCodeGenMethodsCustomDependencyEval
            Kind = mkClassProcedure
          end>
      end>
    Functions = <
      item
        Name = 'RegisterSimpleCodeGen'
        Parameters = <
          item
            Name = 'exprClass'
            DataType = 'String'
          end
          item
            Name = 'qualifiedName'
            DataType = 'String'
          end
          item
            Name = 'codeGenFunc'
            DataType = 'TSimpleCodeGenFunc'
          end>
        OnEval = CodeGenLibModuleFunctionsRegisterSimpleCodeGenEval
      end>
    Delegates = <
      item
        Name = 'TSimpleCodeGenFunc'
        Parameters = <
          item
            Name = 'exprClass'
            DataType = 'String'
          end
          item
            Name = 'qualifiedName'
            DataType = 'String'
          end
          item
            Name = 'parameters'
            DataType = 'TVariantArray'
          end>
        ResultType = 'String'
      end>
    UnitName = 'JSCodeGen'
    StaticSymbols = False
    Left = 56
    Top = 32
  end
end
