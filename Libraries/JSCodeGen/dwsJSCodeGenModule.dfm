object DMJSCodeGenModule: TDMJSCodeGenModule
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
            Kind = mkClassProcedure
            OnEval = CodeGenLibModuleClassesCodeGenMethodsDependencyEval
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
            Kind = mkClassFunction
            OnEval = CodeGenLibModuleClassesCodeGenMethodsNewUniqueGlobalVarEval
          end
          item
            Name = 'CustomDependency'
            Parameters = <
              item
                Name = 'code'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnEval = CodeGenLibModuleClassesCodeGenMethodsCustomDependencyEval
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
    StaticSymbols = True
    Left = 56
    Top = 32
  end
end
