{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{$I dws.inc}
unit dwsStrings;

interface

const
  // Constants of "System.pas"
  SYS_INTEGER = 'Integer';
  SYS_FLOAT = 'Float';
  SYS_STRING = 'String';
  SYS_BOOLEAN = 'Boolean';
  SYS_VARIANT = 'Variant';
  SYS_VOID = 'Void';
  SYS_RESULT = 'Result';
  SYS_SELF = 'Self';
  SYS_INTERNAL = 'Internal';
  SYS_SYSTEM = 'System';
  SYS_TCLASS = 'TClass';
  SYS_TOBJECT = 'TObject';
  SYS_TOBJECT_CREATE = 'Create';
  SYS_TOBJECT_DESTROY = 'Destroy';
  SYS_TOBJECT_FREE = 'Free';
  SYS_TOBJECT_CLASSNAME = 'ClassName';
  SYS_TOBJECT_CLASSTYPE = 'ClassType';
  SYS_EXCEPTION = 'Exception';
  SYS_EXCEPTION_MESSAGE = 'Message';
  SYS_EXCEPTION_MESSAGE_FIELD = 'FMessage';
  SYS_EXCEPTION_STACKTRACE = 'StackTrace';
  SYS_EASSERTIONFAILED = 'EAssertionFailed';
  SYS_EDELPHI = 'EDelphi';
  SYS_EDELPHI_EXCEPTIONCLASS = 'ExceptionClass';
  SYS_EDELPHI_EXCEPTIONCLASS_FIELD = 'FExceptionClass';

  // Compiler switches
  SWI_INCLUDE_LONG = 'INCLUDE';
  SWI_INCLUDE_SHORT = 'I';
  SWI_INCLUDE_ONCE = 'INCLUDE_ONCE';
  SWI_FILTER_LONG = 'FILTER';
  SWI_FILTER_SHORT = 'F';
  SWI_DEFINE = 'DEFINE';
  SWI_UNDEF = 'UNDEF';
  SWI_IFDEF = 'IFDEF';
  SWI_IFNDEF = 'IFNDEF';
  SWI_IF = 'IF';
  SWI_ENDIF = 'ENDIF';
  SWI_ELSE = 'ELSE';
  SWI_HINT = 'HINT';
  SWI_HINTS = 'HINTS';
  SWI_WARNING = 'WARNING';
  SWI_WARNINGS = 'WARNINGS';
  SWI_ERROR = 'ERROR';
  SWI_FATAL = 'FATAL';

  // COMPILER ERRORS
  //
  // These are currently implemented as constants for various considerations.
  // If you wish to localize those error messages, you may want to make them
  // into resourcestrings, though be aware that at this point you'll likely be
  // on your own as far as interpreting translated error messages goes.
  // So sticking to english-only compiler error message is encouraged ^_^

const

  // Missing Tokens
  CPE_SemiExpected = '";" expected.';
  CPE_BrackLeftExpected = '"(" expected.';
  CPE_BrackRightExpected = '")" expected.';
  CPE_ArrayBracketRightExpected = '"]" expected';
  CPE_ArrayBracketLeftExpected = '"[" expected';
  CPE_AtExpected = '"@" expected';
  CPE_CurlyRightExpected = '"}" expected';
  CPE_ColonExpected = 'Colon ":" expected';
  CPE_DotExpected = 'Dot "." expected';
  CPE_NameExpected = 'Name expected';
  CPE_ProcOrFuncExpected = 'PROCEDURE or FUNCTION expected';
  CPE_EqualityExpected = '"=" expected';
  CPE_InExpected = 'IN expected';
  CPE_ArrayExpected = 'Array expected';
  CPE_StringExpected = 'String expected';
  CPE_BeginExpected = 'BEGIN expected';
  CPE_VariableExpected = 'Variable expected';
  CPE_ToOrDowntoExpected = 'TO or DOWNTO expected';
  CPE_DoExpected = 'DO expected';
  CPE_ThenExpected = 'Then expected';
  CPE_OfExpected = 'OF expected';
  CPE_ExpressionExpected = 'Expression expected';
  CPE_DotDotExpected = '".." expected';
  CPE_EndExpected = 'END expected';
  CPE_AssignExpected = '":=" expected';
  CPE_ArrayBracketOrClassExpected = '"[" or class expected';
  CPE_OnOffExpected = 'ON/OFF expected';

  // ReadName
  CPE_UnknownName = 'Unknown name "%s"';
  CPE_UnknownNameDotName = 'Unknown name "%s.%s"';
  CPE_UnknownType = 'Unknown type "%s"';
  CPE_UnknownUnit = 'Unknown unit "%s"';

  // Class declaration errors
  CPE_CantOverrideNotInherited = 'No method* "%s" found in class: "override" not applicable';
  CPE_CantOverrideNotVirtual = 'Inherited method "%s" isn''t virtual. "override" not applicable';
  CPE_CantOverrideWrongParameterList = 'Parameter list doesn''t match the inherited method';
  CPE_CantOverrideWrongResultType = 'Result type doesn''t match the inherited method';
  CPE_CantOverrideWrongMethodType = '"override" not applicable between class and instance methods';
  CPE_CantOverrideWrongFuncKind = 'Can''t override a %s with a %s';
  CPE_CantFinalWithoutOverride = 'Can''t mark "final" without overriding';
  CPE_CantOverrideFinal = 'Method "%s" marked as final. "override" not applicable';
  CPE_CantReintroduce = 'Method "%s" isn''t overlapping a virtual method';
  CPE_ReintroduceWarning = 'Method "%s" overlaps a virtual method';
  CPE_FieldRedefined = 'There is already a field with name "%s"';
  CPE_PropertyRedefined = 'There is already a property with name "%s"';
  CPE_MethodRedefined = 'There is already a method with name "%s"';
  CPE_ClassOperatorRedefined = 'Class operator already defined for type "%s"';
  CPE_ImplClassNameExpected = 'Class name expected';
  CPE_ImplNotAMethod = 'Member is not a method';
  CPE_ImplInvalidClass = '"%s" is not a method of class "%s"';
  CPE_ImplAbstract = '"%s.%s" is declared abstract. No implementation allowed!';
  CPE_NonVirtualAbstract = 'abstract is only valid for virtual methods!';
  CPE_AbstractClassUsage = 'Trying to use an abstract class!';
  CPE_ImplClassExpected = 'Declaration should start with CLASS!';
  CPE_ImplNotClassExpected = 'Declaration shouldn''t start with "class"!';
  CPE_InheritedOnlyInMethodsAllowed = 'Inherited only in methods allowed';
  CPE_InheritedWithoutName = 'Name expected after INHERITED!';
  CPE_InheritedMethodNotFound = 'Method "%s" not found in ancestor class';
  CPE_StaticMethodExpected = 'Class method or constructor expected';
  CPE_UnexpectedConstructor = 'Constructor invoked on instance outside of constructor';
  CPE_WriteOnlyProperty = 'Can''t read a write only property!';
  CPE_ReadOnlyProperty = 'Can''t set a value for a read-only property!';
  CPE_ObjectReferenceExpected = 'Object reference needed to read/write an object field';
  CPE_StaticPropertyWriteExpected = 'Write access of property should be a static method';
  CPE_UnknownClass = 'Class "%s" not found';
  CPE_NotAClass = '"%s" is not a class';
  CPE_ClassForwardAlreadyExists = 'There is already a forward declaration of this class!';
  CPE_FuncForwardAlreadyExists = 'There is already a forward declaration of this function!';
  CPE_ClassNotImplementedYet = 'Class "%s" not fully implemented.';
  CPE_ForwardNotImplemented = 'The function "%s" was forward declared but not implemented!';
  CPE_ClassIsSealed = 'Class "%s" is sealed, inheriting is not allowed!';
  CPE_ClassIsStatic = 'Class "%s" is static, instantiation not allowed!';
  CPE_ClassAncestorNotStatic = 'Class "%s" is not static, can''t inherit as static!';
  CPE_MethodOrPropertyExpected = 'Method or property declaration expected';
  CPE_OverloadableOperatorExpected = 'Overloadable operator expected';
  CPE_SingleParameterExpected = 'Single parameter expected';
  CPE_UsesExpected = '"USES" expected';
  CPE_FieldMethodUnknown = 'Field/method "%s" not found!';
  CPE_IncompatibleType = 'Field/method "%s" has an incompatible type';
  CPE_ProcedureMethodExpected = 'Procedure expected';
  CPE_FunctionMethodExpected = 'Function expected';
  CPE_FunctionOrValueExpected = 'Function or value expected';
  CPE_InvalidNumberOfArguments = 'Method "%s" has a wrong number of arguments';
  CPE_InvalidParameterType = 'Method "%s" has an incompatible parameter type';
  CPE_ReadOrWriteExpected = 'Neither READ nor WRITE directive found';
  CPE_IncompatibleWriteSymbol = 'Field/method "%s" has an incompatible type';
  CPE_ClassNotCompletelyDefined = 'Class "%s" isn''t defined completely';
  CPE_MethodNotImplemented = 'Method "%s" of class "%s" not implemented';
  CPE_CantWriteProperty = 'Can''t write properties of complex type (record, array)';
  CPE_CantUseCombinedAssignmentOnProperty = 'Can''t use combined assignment on property';
  CPE_MultipleDefaultProperties = 'Class "%s" already has a default property';
  CPE_ParamsExpected = 'Parameters expected';

  // CompareFuncSymbols
  CPE_FunctionExpected = 'Declaration should be FUNCTION!';
  CPE_ProcedureExpected = 'Declaration should be PROCEDURE!';
  CPE_MethodExpected = 'Declaration should be METHOD!';
  CPE_ConstructorExpected = 'Declaration should be CONSTRUCTOR!';
  CPE_DestructorExpected = 'Declaration should be DESTRUCTOR!';
  CPE_BadResultType = 'Result type should be "%s"';
  CPE_BadNumberOfParameters = 'Expected %d parameters (instead of %d)';
  CPE_BadParameterName = 'Parameter %d - Name "%s" expected';
  CPE_BadParameterType = 'Parameter %d - Type "%s" expected (instead of "%s")';
  CPE_VarParameterExpected = 'Parameter %d - Var-parameter expected';
  CPE_ConstParameterExpected = 'Parameter %d - Const-parameter expected';
  CPE_ValueParameterExpected = 'Parameter %d - Value-parameter expected';

  // Arrays
  CPE_ArrayBoundNotAConstant = 'Bound isn''t a constant expression';
  CPE_ArrayBoundNotInteger = 'Bound isn''t of type integer';
  CPE_LowerBoundBiggerThanUpperBound = 'Lower bound is bigger than upper bound!';

  // Assign
  CPE_RightSideNeedsReturnType = 'Assignment''s right-side-argument has no return type!';
  CPE_CantWriteToLeftSide = 'Can''t assign a value to the left-side argument!';

  // Function/Procedures
  CPE_FunctionTypeExpected = 'Function type expected';
  CPE_IncompatibleParameterType = 'Type of parameter %d don''t match the declaration';
  CPE_InvalidResultType = 'Invalid type "%s" for function result';
  CPE_NoResultTypeExpected = 'No result type expected';

  CPE_NameAlreadyExists = 'Name "%s" already exists';
  CPE_NameIsReserved = 'Name "%s" is reserved';
  CPE_TypeExpected = 'Type expected';
  CPE_TypeUnknown = 'Type "%s" not found';
  CPE_InvalidType = '%s is not a Type!';
  CPE_UnknownMember = 'There''s no member with name "%s"!';
  CPE_NoMemberExpected = 'Neither a record nor an object!';
  CPE_NoArrayExpected = 'Not an array!';
  CPE_NoMethodExpected = 'Not a method!';
  CPE_InvalidInstruction = 'Invalid Instruction - function or assignment expected';
  CPE_ConstantInstruction = 'Constant Instruction - has no effect';
  CPE_EndOfBlockExpected = 'End of block expected';
  CPE_ContructorExpected = 'Constructor expected';
  CPE_TooManyArguments = 'Too many arguments!';
  CPE_TooFewArguments = 'More arguments expected!';
  CPE_NoArgumentsExpected = 'No arguments expected!';
  CPE_WrongArgumentType = 'Argument %d expects type "%s"';
  CPE_WrongArgumentType_Long = 'Argument %d expects type "%s" instead of "%s"';
  CPE_NoDefaultProperty = 'Class "%s" has no default property';
  CPE_ConstVarParam = 'Argument %d (%s) cannot be passed as Var-parameter';

  CPE_InvalidOperands = 'Invalid Operands';
  CPE_IncompatibleOperands = 'Incompatible operands';

  CPE_FunctionOptimizationFailed = 'Evaluation of "%s" failed: %s %s';

  CPH_VariableDeclaredButNotUsed = 'Variable "%s" declared but not used';
  CPH_VariableDeclaredButNotWrittenTo = 'Variable "%s" declared but never written to';
  CPH_ResultNotUsed = 'Result is never used';

  // TypeCheck
  CPE_BooleanExpected = 'Boolean expected';
  CPE_IntegerExpected = 'Integer expected';
  CPE_FloatExpected = 'Float expected';
  CPE_VariantExpected = 'Simple type expected';
  CPE_EnumerationExpected = 'Enumeration expected';
  CPE_NumericalExpected = 'Numerical operand expected';
  CPE_BooleanOrIntegerExpected = 'Boolean or integer operand expected';
  CPE_ObjectExpected = 'Object expected';
  CPE_ClassRefExpected = 'Class reference expected';
  CPE_ExceptionObjectExpected = 'Exception object expected';

  CPE_IntegerCastInvalid = 'Cannot cast this type to "Integer"';
  CPE_IncompatibleTypes = 'Incompatible types: "%s" and "%s"';
  CPE_AssignIncompatibleTypes = 'Incompatible types: Cannot assign "%s" to "%s"';
  CPE_RangeIncompatibleTypes = 'Range start and range stop are of incompatible types: %s and %s';

  // Connector
  CPE_ConnectorCall = 'Method "%s" not found in connector "%s"';
  CPE_ConnectorMember = 'Member "%s" readonly or not found in connector "%s"';
  CPE_ConnectorTypeMismatch = 'Type mismatch in connector';
  CPE_ConnectorIndex = 'No index access in connector "%s"';
  CPE_ConnectorTooManyArguments = 'Too many arguments for connector call (%d)';

  // Others
  CPE_ConstantExpressionExpected = 'Constant expression expected';
  CPE_IntegerExpressionExpected = 'Integer expression expected';
  CPE_InvalidConstType = 'Invalid const type "%s"';

  CPE_CompilerSwitchUnknown = 'Compiler switch "%s" unknown';

  CPE_IncludeFileNotFound = 'Couldn''t find file "%s" on input paths';
  CPE_IncludeFileExpected = 'Name of include file expected';
  CPE_IncludeItemUnknown = 'Include item "%s" unknown';
  CPE_IncludeItemExpected = 'Include item expected';

  CPE_BreakOutsideOfLoop = '"Break" outside of loop';
  CPE_ContinueOutsideOfLoop = '"Continue" outside of loop';
  CPE_ExitInFinally = '"Exit" not allowed in "Finally" block';
  CPE_BreakContinueInFinally = 'Direct "Break" or "Continue" not allowed in "Finally" block';

  CPE_UnbalancedConditionalDirective = 'Unbalanced conditional directive';
  CPE_UnfinishedConditionalDirective = 'Unfinished conditional directive';

  CPE_TypeIsUnknown = 'Type "%s" unknown';
  CPE_TypeForParamNotFound = 'Type "%s" not found for parameter "%s" of function "%s"';
  CPE_LazyParamCantBeVarOrConst = 'lazy parameter can''t be var or const';
  CPE_LazyParamCantHaveDefaultValue = 'lazy parameter can''t have a default value';
  CPE_VarParamCantHaveDefaultValue = 'var parameter can''t have a default value';
  CPE_ConstParamCantHaveDefaultValue = 'const parameter can''t have a default value';
  CPE_OpenArrayParamMustBeConst = 'open array parameter must be const';
  CPE_OpenArrayParamElementsMustBeConst = 'open array parameter elements must be "const"';
  CPE_FieldExists = 'There is already a field with name "%s"';
  CPE_PropertyExists = 'There is already a property with name "%s"';
  CPE_MethodExists = 'There is already a method with name "%s"';
  CPE_AssignementToFORLoopVariable = 'Assignment to FOR-Loop variable';
  CPE_FORLoopMustBeLocalVariable = 'For loop control variable must be simple local variable';

  CPW_Deprecated = '"%s" has been deprecated';
  CPW_DeprecatedWithMessage = '"%s" has been deprecated: %s';
  CPW_InfiniteLoop = 'Infinite loop';
  CPW_ConstantCondition = 'Constant condition';
  CPW_UnReachableCode = 'Unreachable code';
  CPW_IncludeOnceWithDifferentCase = 'Filename case does not match: "%s" already included as "%s"';

  CPE_NoResultTypeRequired = 'No result type required';
  CPE_ResultTypeExpected = 'Result type expected';
  CPE_NoResultRequired = 'No result required';

  CPE_CanNotOverride = 'Method %s not found in parent class. Can''t override!';
  CPE_InvalidArgCombination = 'Invalid argument combination';

  CPE_FieldAlreadySet = 'Field has already been set';

  // Contracts
  CPE_PreconditionsMustBeInRootMethod = 'Preconditions must be defined in the root method only.';

  // Units
  CPE_UnitNotFound = 'Unit "%s" referenced in unit "%s" not found';
  CPE_UnitCircularReference = 'Circular referencing units detected!';
  CPE_FilterDependsOnUnit = 'The filter "%s" depends on unit "%s" that is not available.';
  CPE_ResultTypeDependsOnUnit = 'The result-type "%s" depends on unit "%s" that is not available.';
  CPE_NoStaticSymbols = 'Invalid use of static symbols!';

  // Filter
  CPE_NoFilterAvailable = 'There is no filter assigned to TDelphiWebScriptII.Config.Filter';

  // TOKENIZER ERRORS
  TOK_InvalidChar = 'Invalid character';
  TOK_EqualityExpected = '"=" expected.';
  TOK_NumberExpected = 'Number expected';
  TOK_HexDigitExpected = 'Hexadecimal digit expected';
  TOK_NumberPointExponentExpected = 'Number, point or exponent expected';
  TOK_NumberExponentExpected = 'Number or exponent expected';
  TOK_NumberSignExpected = 'Number or minus expected';
  TOK_GreaterEqualityExpected = '">" or "=" expected';
  TOK_StringTerminationError = 'End of string constant not found (end of line)';
  TOK_InvalidHexConstant = 'Invalid hexadezimal constant "%s"';
  TOK_InvalidCharConstant = 'Invalid char constant "%s"';
  TOK_InvalidIntegerConstant = 'Invalid integer constant "%s"';
  TOK_InvalidFloatConstant = 'Invalid floating point constant "%s"';
  TOK_GreaterThanExpected = '> expected';
  TOK_NameOfSwitchExpected = 'Name of compiler switch expected';
  TOK_DotExpected = '"." expected';

  // Constants of TMsgs in dwsErrors.pas
  MSG_DatatypeMissing = 'Invalid type: %s!';
  MSG_MainModule = '*MainModule*';
  MSG_Info = 'Info: %s';
  MSG_Error = 'Error: %s';
  MSG_ScriptPosLine = 'line: %d';
  MSG_ScriptPosColumn = 'column: %d';
  MSG_ScriptPosFile = 'file: %s';
  MSG_Hint = 'Hint: %s';
  MSG_Warning = 'Warning: %s';
  MSG_CompileError = 'Compile Error: %s';
  MSG_SyntaxError = 'Syntax Error: %s';
  MSG_RuntimeError = 'Runtime Error: %s';

  // Runtime Errors, Exceptions
  // ==========================
  RTE_CantRunScript = 'Script compiled with errors. Can''t execute!';
  RTE_ScriptAlreadyRunning = 'Script is already running!';
  RTE_ScriptStillRunning = 'Script is still running!';
  RTE_ScriptStopped = 'Script was stopped.';
  RTE_ScriptHasLiveExecutions = 'Script has %d live executions!';
  RTE_StateInitializedExpected = 'ProgramState "psInitialized" expected.';
  RTE_StateReadyToRunExpected = 'ProgramState "psReadyToRun" expected.';

  RTE_InstanceOfAbstractClass = 'Trying to create an instance of an abstract class';
  RTE_ArrayUpperBoundExceeded = 'Upper bound exceeded! Index %d';
  RTE_ArrayLowerBoundExceeded = 'Lower bound exceeded! Index %d';
  RTE_ForLoopStepShouldBeStrictlyPositive = 'FOR loop STEP should be strictly positive: %d';
  RTE_InvalidBreak = 'break without for/while/repeat or case';
  RTE_InvalidContinue = 'continue without for/while/repeat or case';
  RTE_ClassCastFailed = 'Can''t cast instance of type "%s" to class "%s"';
  RTE_OrdinalExpected = 'Ordinal expected';

  RTE_ObjectNotInstantiated = 'Object not instantiated';
  RTE_ObjectAlreadyDestroyed = 'Object already destroyed';
  RTE_ClassTypeIsNil = 'ClassType is nil';

  RTE_ScriptException = 'Script exception: %s';
  RTE_UserDefinedException = 'User defined exception';
  RTE_UserDefinedException_Msg = 'User defined exception: %s';
  RTE_AssertionFailed = 'Assertion failed%s%s';
  RTE_PreConditionFailed = 'Pre-condition failed in %s%s, %s';
  RTE_PostConditionFailed = 'Post-condition failed in %s%s, %s';

  // Connectors
  RTE_ConnectorCallFailed = 'Connector Call "%s" failed';
  RTE_ConnectorReadError = 'ConnectorRead error';
  RTE_ConnectorWriteError = 'ConnectorWrite error';

  // Stack
  RTE_MaximalDatasizeExceeded = 'Maximal data size exceeded (%d Variants)';
  RTE_MaximalRecursionExceeded = 'Maximal recursion exceeded (%d calls)';

  // TProgramInfo/TInfo
  RTE_VariableNotFound = 'Variable "%s" not found';
  RTE_FunctionNotFound = 'Function/Method "%s" not found';
  RTE_DatatypeNotFound = 'DataType "%s" not found!';
  RTE_ClassMatchNotFound = 'Unable to register external object of type "%s". Class not found';

  RTE_OnlyVarSymbols = '.Vars[] can''t handle this symbol: "%s". Use .Func[] or .Method[] instead';
  RTE_OnlyFuncSymbols = '.Func[] can''t handle this symbol: "%s". Use .Vars[] instead';

  RTE_InvalidOp = 'Operation "IInfo.%s" not possible on a symbol of type "%s"!';

  RTE_NoMemberOfClass = '"%s" isn''t a member of class "%s"';
  RTE_NoClassNoMethod = '"%s" is not a class and has no method "%s"';
  RTE_MethodNotFoundInClass = 'Method "%s" not found in class "%s"';

  RTE_CanNotReadComplexType = 'To read a value of complex type "%s" use .Data!';
  RTE_CanNotSetValueForType = 'To write values of type "%s" use .Data!';
  RTE_CanOnlyWriteBlocks = 'Use the .Data property of the type "%s" instead of "%s"';

  RTE_InvalidInputDataSize = 'Input data of invalid size: %d instead of %d';
  RTE_InvalidNumberOfParams = 'Invalid number of parameters (%d instead of %d) to call function %s';
  RTE_UseParameter = 'Use ''Parameter'' property to set parameter "%s" of Function "%s"';
  RTE_NoParameterFound = 'No parameter "%s" found in function "%s"';
  RTE_IncorrectParameterIndex = 'Incorrect parameter index %d';
  RTE_NoIndexFound = 'No index parameter "%s" found for property "%s"';

  RTE_NoRecordMemberFound = 'No member "%s" found in record "%s"';
  RTE_NoArray = '"%s" is not an array!';
  RTE_TooManyIndices = 'Too many indices';

  // Compiler Strings

  UNT_CircularReference = 'Circular reference to symbol "%s"';
  UNT_NameIsEmpty = 'Name property mustn''t be empty!';
  UNT_NameAlreadyExists = 'Name "%s" already exists!';
  UNT_UnitNameNotDefined = 'Property %s.UnitName is undefined!';
  UNT_ParameterNameAlreadyExists = 'Parameter name "%s" already exists';
  UNT_DependencyError = 'Dependency check from unit "%s" to unit "%s" failed: %s';

  UNT_UnitGenerationError = 'TdwsUnit: "%s" -- %s';
  UNT_SymbolGenerationError = '%s: "%s" -- %s';

  UNT_DatatypeUnknown = 'DataType "%s" not found';
  UNT_DatatypeNotSpecified = 'DataType not specified for property "%s" of class "%s"';
  UNT_AutoInstantiateWithoutClass = 'AutoInstantiate is true but DataType "%s" isn''t a class';

  UNT_ClassAlreadyDefined = 'Class already defined';
  UNT_ClassNameAlreadyDefined = 'The class "%s" is already used for another symbol %s';
  UNT_SuperClassUnknwon = 'Superclass "%s" not found';
  UNT_ReadAccessNotFound = 'ReadAccess "%s" not found';
  UNT_WriteAccessNotFound = 'WriteAccess "%s" not found';
  UNT_UsesAccessNotFound = 'UsesAccess "%s" not found';

  UNT_InvalidArrayBounds = 'LowBound is higher than HighBound';

  UNT_CantChangeUnitname = 'Can''t change UnitName while property "Script" is assigned!';

  ADP_ChainIsFormingLoop = 'Adapter chain is forming a loop!';
  ADP_IncompatibleAdapters = 'Incompatible Adapters: %s -> %s';

implementation

end.
