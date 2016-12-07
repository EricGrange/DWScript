{**********************************************************************}
{                                                                      }
{    The contents of this file are subject to the GNU General          }
{    Public License version 3 (the "License") as published by          }
{    the Free Software Foundation. You may obtain a copy of            }
{    the License at https://www.gnu.org/licenses/gpl-3.0.txt           }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsJSCodeGen;

interface

uses
   Classes, SysUtils, Variants, Math,
   dwsUtils, dwsSymbols, dwsCodeGen, dwsTextCodeGen, dwsCoreExprs, dwsDataContext,
   dwsExprs, dwsRelExprs, dwsJSON, dwsMagicExprs, dwsStrings, dwsMethodExprs,
   dwsConnectorExprs, dwsConvExprs, dwsSetOfExprs, dwsCompilerUtils,
   dwsJSLibModule, dwsJSMin, dwsFunctions, dwsGlobalVarsFunctions, dwsErrors,
   dwsRTTIFunctions, dwsConstExprs, dwsInfo, dwsScriptSource, dwsSymbolDictionary;

type

   TDataSymbolList = class(TObjectList<TDataSymbol>)
      public
         destructor Destroy; override;
   end;

   TdwsCodeGenSymbolMapJSObfuscating = class (TdwsCodeGenSymbolMap)
      protected
         function DoNeedUniqueName(symbol : TSymbol; tryCount : Integer; canObfuscate : Boolean) : String; override;
   end;

   TSimpleSymbolHash = class (TSimpleObjectHash<TSymbol>)
   end;

   TSimpleProgramHash = class (TSimpleObjectHash<TdwsProgram>)
   end;

   TdwsJSCodeGen = class (TdwsTextCodeGen)
      private
         FLocalVarScannedProg : TSimpleProgramHash;
         FAllLocalVarSymbols : TSimpleSymbolHash;
         FAllLocalVarWithinTryExpr : Integer;
         FDeclaredLocalVars : TDataSymbolList;
         FDeclaredLocalVarsStack : TSimpleStack<TDataSymbolList>;
         FMainBodyName : String;
         FSelfSymbolName : String;
         FResultSymbolName : String;
         FUniqueGlobalVar : TFastCompareStringList;
         FCustomDependency : TFastCompareStringList;

      protected
         procedure CollectLocalVars(proc : TdwsProgram);
         procedure CollectFuncSymLocalVars(funcSym : TFuncSymbol);
         procedure CollectLocalVarParams(expr : TExprBase);
         procedure EnumerateLocalVarParams(parent, expr : TExprBase; var abort : Boolean);
         procedure CollectInitExprLocalVars(initExpr : TBlockExprBase);

         function CreateSymbolMap(parentMap : TdwsCodeGenSymbolMap; symbol : TSymbol) : TdwsCodeGenSymbolMap; override;

         procedure EnterContext(proc : TdwsProgram); override;
         procedure LeaveContext; override;

         function  SameDefaultValue(typ1, typ2 : TTypeSymbol) : Boolean; overload;
         function  SameDefaultValue(fld1, fld2 : TFieldSymbol) : Boolean; overload;

         procedure WriteDefaultValue(typ : TTypeSymbol; box : Boolean);
         procedure WriteVariant(typ : TTypeSymbol; const v : Variant);
         procedure WriteValue(typ : TTypeSymbol; const dataPtr : IDataContext);
         procedure WriteValueData(typ : TTypeSymbol; const data : TData);
         procedure WriteStringArray(destStream : TWriteOnlyBlockStream; strings : TStrings); overload;
         procedure WriteStringArray(strings : TStrings); overload;

         procedure WriteFuncParams(func : TFuncSymbol);
         procedure EnumerateAbortOnResultExpr(parent, expr : TExprBase; var abort : Boolean);

         procedure CompileFuncBody(func : TFuncSymbol);
         procedure CompileMethod(meth : TMethodSymbol);
         procedure CompileRecordMethod(meth : TMethodSymbol);
         procedure CompileHelperMethod(meth : TMethodSymbol);

         procedure DoCompileCompositeSymbolInternals(struct : TCompositeTypeSymbol); override;
         procedure DoCompileHelperSymbol(helper : THelperSymbol); override;
         procedure DoCompileRecordSymbol(rec : TRecordSymbol); override;
         procedure DoCompileClassSymbol(cls : TClassSymbol); override;
         procedure DoCompileFieldsInit(cls : TClassSymbol);
         procedure DoCompileInterfaceTable(cls : TClassSymbol);
         procedure DoCompileFuncSymbol(func : TFuncSymbol; deAnonymize : Boolean = False); override;
         procedure DoCompileExternalRootedClassSymbol(cls : TClassSymbol);

         procedure CompileRTTIRawAttributes(attributes : TdwsSymbolAttributes); override;
         procedure CompileRTTISymbolName(sym : TSymbol);

         property SelfSymbolName : String read FSelfSymbolName write FSelfSymbolName;
         property ResultSymbolName : String read FResultSymbolName write FResultSymbolName;

      public
         constructor Create; override;
         destructor Destroy; override;

         procedure Clear; override;

         function  SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : String; override;

         procedure CompileValue(expr : TTypedExpr); override;

         procedure CompileEnumerationSymbol(enum : TEnumerationSymbol); override;
         procedure CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                     preConds : Boolean); override;
         procedure CompileProgramBody(expr : TProgramExpr); override;
         procedure CompileSymbolTable(table : TSymbolTable); override;

         procedure ReserveSymbolNames; override;

         procedure CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); override;
         procedure CompileResourceStrings(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); override;

         function GetNewTempSymbol : String; override;

         procedure WriteSymbolVerbosity(sym : TSymbol); override;

         procedure WriteLiteralString(const s : String); override;
         procedure WriteFloat(const v : Double; const fmt : TFormatSettings); override;

         function MemberName(sym : TSymbol; cls : TCompositeTypeSymbol) : String;

         procedure WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram); override;

         procedure WriteSourceMap(dest : TWriteOnlyBlockStream;
                                  const srcFileName, outFileName : String;
                                  const srcRoot, srcSuffix : String);
         function CompiledSourceMap(const srcFileName, outFileName : String;
                                    const srcRoot : String = '';
                                    const srcSuffix : String = '.pas') : String;

         procedure WriteDebuggingSourceFile(dest : TWriteOnlyBlockStream;
                                            srcFile : TSourceFile);
         function DebuggingSourceFile(srcFile : TSourceFile) : String;

         function NewUniqueGlobalVar(const uniqueText : String) : String;
         procedure RegisterCustomDependency(const code : String);

         // returns all the RTL support JS functions
         class function All_RTL_JS : String;
         // removes all RTL dependencies (use in combination with All_RTL_JS)
         procedure IgnoreRTLDependencies;

         const cBoxFieldName = 'v';
         const cVirtualPostfix = '$';

         property MainBodyName : String read FMainBodyName write FMainBodyName;
   end;

   TdwsJSCodeGenEnvironment = class;

   TdwsJSCodeGenIntercept = class (TdwsExprCodeGen)
      public
         Environment : TdwsJSCodeGenEnvironment;
         CallBack : TFuncSymbol;

         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TdwsJSCodeGenIntercepts = class(TSimpleNameObjectHash<TdwsJSCodeGenIntercept>)
      public
         destructor Destroy; override;

         function Match(expr : TExprBase) : TdwsJSCodeGenIntercept;
   end;

   TSimpleClassHash = class(TSimpleHash<TClass>)
      protected
         function SameItem(const item1, item2 : TClass) : Boolean; override;
         function GetItemHashCode(const item1 : TClass) : Integer; override;
   end;

   TdwsJSCodeGenEnvironment = class(TInterfacedSelfObject, IdwsEnvironment)
      private
         FCodeGen : TdwsJSCodeGen;
         FIntercepts : TSimpleNameObjectHash<TdwsJSCodeGenIntercepts>;
         FMissedIntercepts : TSimpleClassHash;
         FExec : IdwsProgramExecution;

      protected
         function DoCustomCodeGen(expr : TExprBase) : TdwsExprCodeGen;

      public
         constructor Create(codeGen : TdwsJSCodeGen; const customExec : IdwsProgramExecution);
         destructor Destroy; override;

         procedure RegisterIntercept(const exprClassName, qualifiedName : String;
                                     const callBack : IInfo);

         property CodeGen : TdwsJSCodeGen read FCodeGen;
         property Exec : IdwsProgramExecution read FExec;
   end;

   TJSExprCodeGen = class (TdwsExprCodeGen)
      class function IsLocalVarParam(codeGen : TdwsCodeGen; sym : TDataSymbol) : Boolean; static;
      class procedure WriteLocationString(codeGen : TdwsCodeGen; expr : TExprBase);
   end;

   TJSBlockInitExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSBlockExprBase = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSBlockExpr = class (TJSBlockExprBase)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSRAWBlockExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSNoResultWrapperExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSExitExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSExitValueExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssignExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); virtual;
   end;
   TJSAssignDataExpr = class (TJSAssignExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignClassOfExpr = class (TJSAssignExpr)
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignFuncExpr = class (TJSAssignExpr)
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssignConstToIntegerVarExpr = class (TJSAssignExpr)
      procedure CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignConstToFloatVarExpr = class (TJSAssignExpr)
      procedure CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignConstToBoolVarExpr = class (TJSAssignExpr)
      procedure CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignConstToStringVarExpr = class (TJSAssignExpr)
      procedure CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignNilToVarExpr = class (TJSAssignExpr)
      procedure CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignNilAsResetExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignConstDataToVarExpr = class (TJSAssignExpr)
      procedure CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJAssignArrayConstantExpr = class (TJSAssignExpr)
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAppendConstStringVarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSCompoundExpr = class (TJSExprCodeGen)
      Op : String;
      DynCompound : String;
      constructor Create(const anOp, aDynCompound : String);
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConstExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstStringExpr = class (TJSConstExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstNumExpr = class (TJSConstExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstIntExpr = class (TJSConstNumExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;
   TJSConstFloatExpr = class (TJSConstNumExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;
   TJSConstBooleanExpr = class (TJSConstExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstArrayExpr = class (TJSConstExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayConstantExpr = class (TJSConstExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      class procedure CodeGenElements(codeGen : TdwsCodeGen; e : TArrayConstantExpr); static;
   end;

   TJSResourceStringExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSVarExpr = class (TJSExprCodeGen)
      class function CodeGenSymbol(codeGen : TdwsCodeGen; expr : TExprBase) : TDataSymbol; static;
      class function CodeGenName(codeGen : TdwsCodeGen; expr : TExprBase) : TDataSymbol; static;
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSVarParamExpr = class (TJSVarExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSLazyParamExpr = class (TJSVarExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstParamExpr = class (TJSVarExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSRecordExpr = class (TJSVarExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;

      class procedure CodeGenFieldName(codeGen : TdwsCodeGen; e : TRecordExpr);
   end;
   TJSFieldExpr = class (TJSVarExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;

      class procedure CodeGenObjectExpr(codeGen : TdwsCodeGen; e : TFieldExpr);
      class procedure CodeGenFieldName(codeGen : TdwsCodeGen; e : TFieldExpr);
   end;

   TJSInlineObject = class (TJSExprCodeGen)
      procedure CodeGenMembers(codeGen : TdwsCodeGen;
                               members : TMembersSymbolTable;
                               useExternal : Boolean);
   end;
   TJSDynamicRecordExpr = class (TJSInlineObject)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstructorAnonymousExpr = class (TJSInlineObject)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSSetOfExpr = class (TJSExprCodeGen)
      class procedure CodeGenFunc(codeGen : TdwsCodeGen; const funcName : String;
                                  base, operand : TExprBase; setType : TSetOfSymbol);
   end;
   TJSSetOfFunctionExpr = class (TJSSetOfExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSSetOfInExpr = class (TJSSetOfExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConvStaticArrayToSetOfExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConvSetOfToIntegerExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConvIntegerToSetOfExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSNewArrayExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayLengthExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArraySetLengthExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayAddExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      class procedure CodeGenPush(codeGen : TdwsCodeGen; e : TArrayAddExpr; skip : Integer); static;
   end;
   TJSArrayPeekExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayPopExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayDeleteExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayIndexOfExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArraySortExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayMapExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayRemoveExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayInsertExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayCopyExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArraySwapExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayConcatExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSArrayTypedFluentExpr = class (TJSExprCodeGen)
      private
         FCode, FDependency : String;
      public
         constructor Create(const code, dependency : String);
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSStaticArrayExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSStaticArrayBoolExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
//   TJSOpenArrayExpr = class (TJSExprCodeGen)
//      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
//   end;
   TJSDynamicArrayExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSDynamicArraySetExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSStringArrayOpExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      class procedure CodeGenCharXxxAt(codeGen : TdwsCodeGen; expr : TExprBase;
                                       const xxx : String); static;
   end;
   TJSVarStringArraySetExpr = class (TJSExprCodeGen)
      protected
         procedure CodeGenCall(codeGen : TdwsCodeGen; expr : TStringArraySetExpr); virtual;
         procedure CodeGenValue(codeGen : TdwsCodeGen; valueExpr : TTypedExpr); virtual;
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSVarStringArraySetChrExpr = class (TJSVarStringArraySetExpr)
      protected
         procedure CodeGenValue(codeGen : TdwsCodeGen; valueExpr : TTypedExpr); override;
   end;
   TJSStringArraySetExpr = class (TJSVarStringArraySetExpr)
      protected
         procedure CodeGenCall(codeGen : TdwsCodeGen; expr : TStringArraySetExpr); override;
   end;

   TJSAssociativeArrayGetExpr  = class (TJSExprCodeGen)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssociativeArraySetExpr  = class (TJSExprCodeGen)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSInOpExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSBitwiseInOpExpr = class (TJSExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSIfExpr = class (TJSExprCodeGen)
      procedure CodeGenCondition(codeGen : TdwsCodeGen; condExpr : TTypedExpr);
   end;

   TJSIfThenElseValueExpr = class (TJSIfExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSIfThenExpr = class (TJSIfExpr)
      function SubExprIsSafeStatement(sub : TExprBase) : Boolean;
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSIfThenElseExpr = class (TJSIfThenExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSCaseExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      class procedure CodeGenCondition(codeGen : TdwsCodeGen; cond : TCaseCondition;
                                       const writeOperand : TProc); static;
   end;

   TJSObjAsClassExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSIsOpExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSObjAsIntfExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSObjToClassTypeExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSIntfAsClassExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSIntfAsIntfExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSTImplementsIntfOpExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSTClassImplementsIntfOpExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSClassAsClassExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConvIntegerExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConvFloatExpr = class (TJSExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSConvStringExpr = class (TJSExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSConvStaticArrayToDynamicExpr = class (TJSExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSOrdExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSOrdIntExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSOrdStrExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSEnumerationElementNameExpr = class (TJSExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;
   TJSEnumerationElementQualifiedNameExpr = class (TJSEnumerationElementNameExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSIncDecVarFuncExpr = class (TJSExprCodeGen)
      procedure DoCodeGen(codeGen : TdwsCodeGen; expr : TMagicFuncExpr;
                          op : Char; noWrap : Boolean);
   end;

   TJSIncVarFuncExpr = class (TJSIncDecVarFuncExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;
   TJSDecVarFuncExpr = class (TJSIncDecVarFuncExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSSarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSFuncBaseExpr = class (TJSExprCodeGen)
      private
         FVirtualCall : Boolean;
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
         procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); virtual;
         procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); virtual;
         procedure CodeGenTypeOf(codeGen : TdwsCodeGen; expr : TFuncExprBase; wrap : Boolean);

         class function GetSignature(funcSym : TFuncSymbol) : String;
   end;

   TJSRecordMethodExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSHelperMethodExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSMethodStaticExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
      procedure CodeGenExternalArray(codeGen : TdwsCodeGen; expr : TMethodExpr);
   end;
   TJSMethodVirtualExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;

   TJSMethodInterfaceExpr = class (TJSFuncBaseExpr)
      procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); override;
   end;
   TJSMethodInterfaceAnonymousExpr = class (TJSFuncBaseExpr)
//      procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;

   TJSClassMethodStaticExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;
   TJSClassMethodVirtualExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;

   TJSConstructorStaticExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;
   TJSConstructorVirtualExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;

   TJSConnectorCallExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConnectorReadExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConnectorWriteExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConnectorForInExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConnectorCastExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSFuncPtrExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); override;
   end;
   TJSFuncRefExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      class procedure DoCodeGen(codeGen : TdwsCodeGen; funcExpr : TFuncExprBase); static;
   end;
   TJSAnonymousFuncRefExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSExceptExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSFinallyExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssertExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSDebugBreakExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSDeclaredExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConditionalDefinedExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSSwapExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSForExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure WriteCompare(codeGen : TdwsCodeGen); virtual; abstract;
      procedure WriteStep(codeGen : TdwsCodeGen); virtual; abstract;
   end;
   TJSForUpwardExpr = class (TJSForExpr)
      procedure WriteCompare(codeGen : TdwsCodeGen); override;
      procedure WriteStep(codeGen : TdwsCodeGen); override;
   end;
   TJSForDownwardExpr = class (TJSForExpr)
      procedure WriteCompare(codeGen : TdwsCodeGen); override;
      procedure WriteStep(codeGen : TdwsCodeGen); override;
   end;
   TJSForUpwardStepExpr = class (TJSForUpwardExpr)
      procedure WriteStep(codeGen : TdwsCodeGen); override;
   end;
   TJSForDownwardStepExpr = class (TJSForDownwardExpr)
      procedure WriteStep(codeGen : TdwsCodeGen); override;
   end;

   TJSForCharCodeInStrExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSForCharInStrExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSSqrExpr = class (TJSExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
      class procedure CodeGenSqr(codeGen : TdwsCodeGen; expr : TTypedExpr);
   end;

   TJSOpExpr = class (TJSExprCodeGen)
      class procedure WriteWrappedIfNeeded(codeGen : TdwsCodeGen; expr : TTypedExpr); static;
   end;
   TJBinOpAssociativity = (associativeLeft, associativeRight);
   TJBinOpAssociativities = set of TJBinOpAssociativity;

   TJSBinOpExpr = class (TJSOpExpr)
      protected
         FOp : String;
         FPrecedence : Integer;
         FAssociative : TJBinOpAssociativities;

         function WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean; virtual;

         function ExprJSPrecedence(expr : TTypedExpr) : Integer;

      public
         constructor Create(const op : String; aPrecedence : Integer; associative : TJBinOpAssociativities);
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSAddOpExpr = class(TJSBinOpExpr)
      protected
         function WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean; override;
      public
         constructor Create;
   end;

   TJSSubOpExpr = class(TJSBinOpExpr)
      protected
         function WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean; override;
      public
         constructor Create;
   end;

   TJSMultOpExpr = class(TJSBinOpExpr)
      public
         constructor Create;
   end;

   TJSDivExpr = class (TJSExprCodeGen)
      public
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSMultIntPow2Expr = class(TJSExprCodeGen)
      public
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSCoalesceExpr = class (TJSBinOpExpr)
      public
         constructor Create;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsJSRTL;

const
   cBoolToJSBool : array [False..True] of String = ('false', 'true');
   cFormatSettings : TFormatSettings = ( DecimalSeparator : '.' );
   cInlineStaticArrayLimit = 20;

const
   cJSReservedWords : array [1..203] of String = (
      // Main JS keywords
      // from https://developer.mozilla.org/en/JavaScript/Reference/Reserved_Words
      'break', 'case', 'catch', 'continue', 'debugger', 'default', 'delete',
      'do', 'else', 'finally', 'for', 'function', 'if', 'in', 'instanceof',
      'new', 'return', 'switch', 'this', 'throw', 'try', 'typeof', 'var',
      'void', 'while', 'with',

      'class', 'enum', 'export', 'extends', 'import', 'super',

      'implements', 'interface', 'let', 'package', 'private', 'protected',
      'public', 'static', 'yield',

      'null', 'true', 'false',

      // supplemental reservations for standard JS class names, instances, etc.
      // from http://javascript.about.com/library/blclassobj.htm
      'Anchor', 'anchors', 'Applet', 'applets', 'Area', 'Array', 'Body', 'Button',
      'Checkbox', 'Date', 'Error', 'EvalError', 'FileUpload', 'Form',
      'forms', 'frame', 'frames', 'Function', 'Hidden', 'History', 'history',
      'Image', 'images', 'Link', 'links', 'location', 'Math', 'MimeType',
      'mimetypes', 'navigator', 'Number', 'Object', 'Option', 'options',
      'Password', 'Plugin', 'plugins', 'Radio', 'RangeError', 'ReferenceError',
      'RegExp', 'Reset', 'screen', 'Script', 'Select', 'String', 'Style',
      'StyleSheet', 'Submit', 'SyntaxError', 'Text', 'Textarea', 'TypeError',
      'URIError', 'window',


      // global properties and method names
      // from http://javascript.about.com/library/blglobal.htm
      'Infinity', 'NaN', 'undefined',
      'decodeURI', 'decodeURIComponent', 'encodeURI', 'encodeURIComponent',
      'eval', 'isFinite', 'isNaN', 'parseFloat', 'parseInt',
      'closed', 'Components', 'content', 'controllers', 'crypto', 'defaultstatus',
      'directories', 'document', 'innerHeight', 'innerWidth',
      'length', 'locationbar', 'menubar', 'name',
      'opener', 'outerHeight', 'outerWidth', 'pageXOffset', 'pageYOffset',
      'parent', 'personalbar', 'pkcs11', 'prompter', 'screenX',
      'screenY', 'scrollbars', 'scrollX', 'scrollY', 'self', 'statusbar',
      'toolbar', 'top',
      'alert', 'back', 'blur', 'captureevents', 'clearinterval', 'cleartimeout',
      'close', 'confirm', 'dump', 'escape', 'focus', 'forward', 'getAttention',
      'getSelection', 'home', 'moveBy', 'moveTo', 'open', 'print', 'prompt',
      'releaseevents', 'resizeBy', 'resizeTo', 'scroll', 'scrollBy', 'scrollByLines',
      'scrollByPages', 'scrollTo', 'setCursor', 'setinterval', 'settimeout',
      'sizeToContents', 'stop', 'unescape', 'updateCommands',
      'onabort', 'onblur', 'onchange', 'onclick', 'onclose', 'ondragdrop',
      'onerror', 'onfocus', 'onkeydown', 'onkeypress', 'onkeyup', 'onload',
      'onmousedown', 'onmousemove', 'onmouseout', 'onmouseover',
      'onmouseup', 'onpaint', 'onreset', 'onresize', 'onscroll', 'onselect',
      'onsubmit', 'onunload',

      // supplemental names
      'JSON'

   );

function ShouldBoxParam(param : TParamSymbol) : Boolean;
begin
   Result:=    (param is TVarParamSymbol)
           and not (param.Typ.UnAliasedType is TRecordSymbol);
end;

function IntToSkewedBase62(i : Cardinal) : String;
var
   m : Cardinal;
begin
   m:=i mod 52;
   i:=i div 52;
   if m<26 then
      Result:=Char(Ord('A')+m)
   else Result:=Char(Ord('a')+m-26);
   while i>0 do begin
      m:=i mod 62;
      i:=i div 62;
      case m of
         0..9 : Result:=Result+Char(Ord('0')+m);
         10..35 : Result:=Result+Char(Ord('A')+m-10);
      else
         Result:=Result+Char(Ord('a')+m-36);
      end;
   end;
end;

type
   TCodeGenHelper = class helper for TdwsCodeGen
      procedure WriteDotBoxFieldName(expr : TExprBase);
   end;

procedure TCodeGenHelper.WriteDotBoxFieldName(expr : TExprBase);
begin
   if    (not (expr is TTypedExpr))
      or (TTypedExpr(expr).Typ.UnAliasedType.ClassType<>TRecordSymbol) then begin
      WriteString('.');
      WriteString(TdwsJSCodeGen.cBoxFieldName);
   end;
end;

function IsValidJSName(const name : String) : Boolean;
var
   i : Integer;
begin
   if name='' then Exit(False);
   case name[1] of
      'a'..'z', 'A'..'Z', '_', '$' : ;
   else
      Exit(False);
   end;
   for i:=2 to Length(name) do begin
      case name[i] of
         '0'..'9', 'a'..'z', 'A'..'Z', '_', '$' : ;
      else
         Exit(False);
      end;
   end;
   Result:=True;
end;

// ------------------
// ------------------ TdwsJSCodeGen ------------------
// ------------------

// Create
//
constructor TdwsJSCodeGen.Create;
begin
   inherited;
   FLocalVarScannedProg:=TSimpleProgramHash.Create;
   FAllLocalVarSymbols:=TSimpleSymbolHash.Create;

   FDeclaredLocalVars:=TDataSymbolList.Create;
   FDeclaredLocalVarsStack:=TSimpleStack<TDataSymbolList>.Create;

   FUniqueGlobalVar:=TFastCompareStringList.Create;
   FCustomDependency:=TFastCompareStringList.Create;

   FMainBodyName:='$dws';
   FSelfSymbolName:='Self';
   FResultSymbolName:='Result';

   RegisterCodeGen(TBlockInitExpr,        TJSBlockInitExpr.Create);
   RegisterCodeGen(TBlockFinalExpr,       TJSBlockExprBase.Create);

   RegisterCodeGen(TBlockExpr,            TJSBlockExpr.Create);
   RegisterCodeGen(TBlockExprNoTable,     TJSBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable2,    TJSBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable3,    TJSBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable4,    TJSBlockExprBase.Create);

   RegisterCodeGen(TdwsJSBlockExpr,       TJSRAWBlockExpr.Create);

   RegisterCodeGen(TNullExpr,             TdwsExprGenericCodeGen.Create(['/* null */'#13#10]));
   RegisterCodeGen(TNoResultWrapperExpr,  TJSNoResultWrapperExpr.Create);

   RegisterCodeGen(TConstExpr,            TJSConstExpr.Create);
   RegisterCodeGen(TConstNilExpr,         TdwsExprGenericCodeGen.Create(['null']));
   RegisterCodeGen(TConstIntExpr,         TJSConstIntExpr.Create);
   RegisterCodeGen(TConstStringExpr,      TJSConstStringExpr.Create);
   RegisterCodeGen(TConstFloatExpr,       TJSConstFloatExpr.Create);
   RegisterCodeGen(TConstBooleanExpr,     TJSConstBooleanExpr.Create);
   RegisterCodeGen(TConstArrayExpr,       TJSConstArrayExpr.Create);
   RegisterCodeGen(TResourceStringExpr,   TJSResourceStringExpr.Create);
   RegisterCodeGen(TArrayConstantExpr,    TJSArrayConstantExpr.Create);

   RegisterCodeGen(TAssignExpr,           TJSAssignExpr.Create);
   RegisterCodeGen(TAssignExternalExpr,   TJSAssignExpr.Create);
   RegisterCodeGen(TAssignClassOfExpr,    TJSAssignClassOfExpr.Create);
   RegisterCodeGen(TAssignDataExpr,       TJSAssignDataExpr.Create);
   RegisterCodeGen(TAssignFuncExpr,       TJSAssignFuncExpr.Create);

   RegisterCodeGen(TAssignConstToIntegerVarExpr,   TJSAssignConstToIntegerVarExpr.Create);
   RegisterCodeGen(TAssignConstToFloatVarExpr,     TJSAssignConstToFloatVarExpr.Create);
   RegisterCodeGen(TAssignConstToBoolVarExpr,      TJSAssignConstToBoolVarExpr.Create);
   RegisterCodeGen(TAssignConstToStringVarExpr,    TJSAssignConstToStringVarExpr.Create);
   RegisterCodeGen(TAssignConstToVariantVarExpr,   TJSAssignConstDataToVarExpr.Create);
   RegisterCodeGen(TAssignNilToVarExpr,            TJSAssignNilToVarExpr.Create);
   RegisterCodeGen(TAssignNilClassToVarExpr,       TJSAssignNilToVarExpr.Create);
   RegisterCodeGen(TAssignNilAsResetExpr,          TJSAssignNilAsResetExpr.Create);
   RegisterCodeGen(TAssignConstDataToVarExpr,      TJSAssignConstDataToVarExpr.Create);

   RegisterCodeGen(TAssignArrayConstantExpr,       TJAssignArrayConstantExpr.Create);

   RegisterCodeGen(TVarExpr,              TJSVarExpr.Create);
   RegisterCodeGen(TExternalVarExpr,      TJSVarExpr.Create);
   RegisterCodeGen(TBaseTypeVarExpr,      TJSVarExpr.Create);
   RegisterCodeGen(TSelfVarExpr,          TJSVarExpr.Create);
   RegisterCodeGen(TSelfObjectVarExpr,    TJSVarExpr.Create);
   RegisterCodeGen(TVarParentExpr,        TJSVarExpr.Create);
   RegisterCodeGen(TVarParamExpr,         TJSVarParamExpr.Create);
   RegisterCodeGen(TVarParamParentExpr,   TJSVarParamExpr.Create);
   RegisterCodeGen(TLazyParamExpr,        TJSLazyParamExpr.Create);
   RegisterCodeGen(TConstParamExpr,       TJSConstParamExpr.Create);
   RegisterCodeGen(TConstParamParentExpr, TJSConstParamExpr.Create);

   RegisterCodeGen(TIntVarExpr,           TJSVarExpr.Create);
   RegisterCodeGen(TFloatVarExpr,         TJSVarExpr.Create);
   RegisterCodeGen(TStrVarExpr,           TJSVarExpr.Create);
   RegisterCodeGen(TBoolVarExpr,          TJSVarExpr.Create);
   RegisterCodeGen(TObjectVarExpr,        TJSVarExpr.Create);

   RegisterCodeGen(TRecordExpr,           TJSRecordExpr.Create);
   RegisterCodeGen(TRecordVarExpr,        TJSRecordExpr.Create);
   RegisterCodeGen(TDynamicRecordExpr,    TJSDynamicRecordExpr.Create);

   RegisterCodeGen(TConvOrdToIntegerExpr, TJSConvIntegerExpr.Create);
   RegisterCodeGen(TConvVarToIntegerExpr, TJSConvIntegerExpr.Create);
   RegisterCodeGen(TConvIntToFloatExpr,   TJSConvFloatExpr.Create);
   RegisterCodeGen(TConvVarToFloatExpr,   TJSConvFloatExpr.Create);
   RegisterCodeGen(TConvIntToBoolExpr,    TdwsExprGenericCodeGen.Create(['(', 0, '?true:false)']));
   RegisterCodeGen(TConvFloatToBoolExpr,  TdwsExprGenericCodeGen.Create(['(', 0, '?true:false)']));
   RegisterCodeGen(TConvVarToBoolExpr,    TdwsExprGenericCodeGen.Create(['(', 0, '?true:false)']));
   RegisterCodeGen(TConvVarToStringExpr,  TJSConvStringExpr.Create);
   RegisterCodeGen(TConvStaticArrayToDynamicExpr, TJSConvStaticArrayToDynamicExpr.Create);
   RegisterCodeGen(TConvVariantExpr,      TdwsExprGenericCodeGen.Create([0]));
   RegisterCodeGen(TConvExternalExpr,     TdwsExprGenericCodeGen.Create([0]));

   RegisterCodeGen(TClassAsClassExpr,     TJSClassAsClassExpr.Create);
   RegisterCodeGen(TObjAsClassExpr,       TJSObjAsClassExpr.Create);
   RegisterCodeGen(TIsOpExpr,             TJSIsOpExpr.Create);

   RegisterCodeGen(TObjAsIntfExpr,        TJSObjAsIntfExpr.Create);
   RegisterCodeGen(TObjToClassTypeExpr,   TJSObjToClassTypeExpr.Create);
   RegisterCodeGen(TIntfAsClassExpr,      TJSIntfAsClassExpr.Create);
   RegisterCodeGen(TIntfAsIntfExpr,       TJSIntfAsIntfExpr.Create);
   RegisterCodeGen(TImplementsIntfOpExpr, TJSTImplementsIntfOpExpr.Create);
   RegisterCodeGen(TClassImplementsIntfOpExpr, TJSTClassImplementsIntfOpExpr.Create);
   RegisterCodeGen(TIntfCmpExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '===', 1, ')']));

   RegisterCodeGen(TAddStrExpr,           TJSBinOpExpr.Create('+', 13, [associativeLeft, associativeRight]));

   RegisterCodeGen(TAddIntExpr,           TJSAddOpExpr.Create);
   RegisterCodeGen(TAddFloatExpr,         TJSAddOpExpr.Create);
   RegisterCodeGen(TAddVariantExpr,       TJSAddOpExpr.Create);
   RegisterCodeGen(TSubIntExpr,           TJSSubOpExpr.Create);
   RegisterCodeGen(TSubFloatExpr,         TJSSubOpExpr.Create);
   RegisterCodeGen(TSubVariantExpr,       TJSSubOpExpr.Create);
   RegisterCodeGen(TMultIntExpr,          TJSMultOpExpr.Create);
   RegisterCodeGen(TMultIntPow2Expr,      TJSMultIntPow2Expr.Create);
   RegisterCodeGen(TMultFloatExpr,        TJSMultOpExpr.Create);
   RegisterCodeGen(TMultVariantExpr,      TJSMultOpExpr.Create);
   RegisterCodeGen(TDivideExpr,           TJSBinOpExpr.Create('/', 14, [associativeLeft]));
   RegisterCodeGen(TDivExpr,              TJSDivExpr.Create);
   RegisterCodeGen(TDivConstExpr,         TJSDivExpr.Create);
   RegisterCodeGen(TModExpr,              TJSBinOpExpr.Create('%', 14, [associativeLeft]));
   RegisterCodeGen(TModFloatExpr,         TJSBinOpExpr.Create('%', 14, [associativeLeft]));
   RegisterCodeGen(TModConstExpr,         TJSBinOpExpr.Create('%', 14, [associativeLeft]));
   RegisterCodeGen(TSqrFloatExpr,         TJSSqrExpr.Create);
   RegisterCodeGen(TSqrIntExpr,           TJSSqrExpr.Create);
   RegisterCodeGen(TNegIntExpr,           TdwsExprGenericCodeGen.Create(['(', '-', 0, ')']));
   RegisterCodeGen(TNegFloatExpr,         TdwsExprGenericCodeGen.Create(['(', '-', 0, ')']));
   RegisterCodeGen(TNegVariantExpr,       TdwsExprGenericCodeGen.Create(['(', '-', 0, ')']));

   RegisterCodeGen(TCoalesceExpr,         TJSCoalesceExpr.Create);
   RegisterCodeGen(TCoalesceStrExpr,      TJSCoalesceExpr.Create);
   RegisterCodeGen(TCoalesceClassExpr,    TJSCoalesceExpr.Create);
   RegisterCodeGen(TCoalesceDynArrayExpr, TJSCoalesceExpr.Create);

   RegisterCodeGen(TAppendStringVarExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', -1], gcgStatement));
   RegisterCodeGen(TAppendConstStringVarExpr,      TJSAppendConstStringVarExpr.Create);

   RegisterCodeGen(TPlusAssignIntExpr,    TJSCompoundExpr.Create('+=', '$DIdxAdd'));
   RegisterCodeGen(TPlusAssignFloatExpr,  TJSCompoundExpr.Create('+=', '$DIdxAdd'));
   RegisterCodeGen(TPlusAssignStrExpr,    TJSCompoundExpr.Create('+=', '$DIdxAdd'));
   RegisterCodeGen(TPlusAssignExpr,       TJSCompoundExpr.Create('+=', '$DIdxAdd'));
   RegisterCodeGen(TMinusAssignIntExpr,   TJSCompoundExpr.Create('-=', '$DIdxMinus'));
   RegisterCodeGen(TMinusAssignFloatExpr, TJSCompoundExpr.Create('-=', '$DIdxMinus'));
   RegisterCodeGen(TMinusAssignExpr,      TJSCompoundExpr.Create('-=', '$DIdxMinus'));
   RegisterCodeGen(TMultAssignIntExpr,    TJSCompoundExpr.Create('*=', '$DIdxMult'));
   RegisterCodeGen(TMultAssignFloatExpr,  TJSCompoundExpr.Create('*=', '$DIdxMult'));
   RegisterCodeGen(TMultAssignExpr,       TJSCompoundExpr.Create('*=', '$DIdxMult'));
   RegisterCodeGen(TDivideAssignExpr,     TJSCompoundExpr.Create('/=', '$DIdxDiv'));

   RegisterCodeGen(TIncIntVarExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', -1], gcgStatement));
   RegisterCodeGen(TDecIntVarExpr,
      TdwsExprGenericCodeGen.Create([0, '-=', -1], gcgStatement));

   RegisterCodeGen(TIncVarFuncExpr,    TJSIncVarFuncExpr.Create);
   RegisterCodeGen(TDecVarFuncExpr,    TJSDecVarFuncExpr.Create);
   RegisterCodeGen(TSuccFuncExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '+', 1, ')']));
   RegisterCodeGen(TPredFuncExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '-', 1, ')']));

   RegisterCodeGen(TAbsIntExpr,
      TdwsExprGenericCodeGen.Create(['Math.abs', '(', 0, ')']));
   RegisterCodeGen(TAbsFloatExpr,
      TdwsExprGenericCodeGen.Create(['Math.abs', '(', 0, ')']));
   RegisterCodeGen(TAbsVariantExpr,
      TdwsExprGenericCodeGen.Create(['Math.abs', '(', 0, ')']));

   RegisterCodeGen(TSarExpr,           TJSSarExpr.Create);
   RegisterCodeGen(TShrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>>>', 1, ')']));
   RegisterCodeGen(TShlExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<<', 1, ')']));
   RegisterCodeGen(TIntAndExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '&', 1, ')']));
   RegisterCodeGen(TIntOrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '|', 1, ')']));
   RegisterCodeGen(TIntXorExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '^', 1, ')']));
   RegisterCodeGen(TNotIntExpr,
      TdwsExprGenericCodeGen.Create(['(', '~', 0, ')']));

   RegisterCodeGen(TBoolAndExpr,       TJSBinOpExpr.Create('&&', 6, [associativeLeft, associativeRight]));
   RegisterCodeGen(TBoolOrExpr,        TJSBinOpExpr.Create('||', 5, [associativeLeft, associativeRight]));
   RegisterCodeGen(TBoolXorExpr,
      TdwsExprGenericCodeGen.Create(['(!', 0, ' != !', 1, ')']));
   RegisterCodeGen(TBoolImpliesExpr,
      TdwsExprGenericCodeGen.Create(['(!', 0, ' || ', 1, ')']));
   RegisterCodeGen(TNotBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', '!', 0, ')']));

   RegisterCodeGen(TVariantAndExpr,    TJSBinOpExpr.Create('&&', 6, [associativeLeft, associativeRight]));
   RegisterCodeGen(TVariantOrExpr,     TJSBinOpExpr.Create('||', 5, [associativeLeft, associativeRight]));
   RegisterCodeGen(TVariantXorExpr,    TJSBinOpExpr.Create('^', 8, [associativeLeft, associativeRight]));
   RegisterCodeGen(TNotVariantExpr,
      TdwsExprGenericCodeGen.Create(['(', '!', 0, ')']));

   RegisterCodeGen(TAssignedInstanceExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null', ')']));
   RegisterCodeGen(TAssignedInterfaceExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null', ')']));
   RegisterCodeGen(TAssignedMetaClassExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null', ')']));
   RegisterCodeGen(TAssignedFuncPtrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null', ')']));

   RegisterCodeGen(TDebugBreakExpr, TJSDebugBreakExpr.Create);

   RegisterCodeGen(TDefinedExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==undefined', ')']));

   RegisterCodeGen(TRelEqualBoolExpr,     TJSBinOpExpr.Create('==', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelNotEqualBoolExpr,  TJSBinOpExpr.Create('!=', 10, [associativeLeft, associativeRight]));

   RegisterCodeGen(TStringInStringExpr,
      TdwsExprGenericCodeGen.Create(['(', '(', 1, ')', '.indexOf', '(', 0, ')', '>=0)']));
   RegisterCodeGen(TVarStringInConstStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 1, '.indexOf', '(', 0, ')', '>=0)']));
   RegisterCodeGen(TConstStringInVarStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 1, '.indexOf', '(', 0, ')', '>=0)']));

   RegisterCodeGen(TObjCmpEqualExpr,         TJSBinOpExpr.Create('===', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TObjCmpNotEqualExpr,      TJSBinOpExpr.Create('!==', 10, [associativeLeft, associativeRight]));

   RegisterCodeGen(TRelEqualIntExpr,         TJSBinOpExpr.Create('==', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelNotEqualIntExpr,      TJSBinOpExpr.Create('!=', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelGreaterEqualIntExpr,  TJSBinOpExpr.Create('>=', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelLessEqualIntExpr,     TJSBinOpExpr.Create('<=', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelGreaterIntExpr,       TJSBinOpExpr.Create('>', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelLessIntExpr,          TJSBinOpExpr.Create('<', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelIntIsZeroExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '==0)']));
   RegisterCodeGen(TRelIntIsNotZeroExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!=0)']));

   RegisterCodeGen(TRelEqualStringExpr,         TJSBinOpExpr.Create('==', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelNotEqualStringExpr,      TJSBinOpExpr.Create('!=', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelGreaterEqualStringExpr,  TJSBinOpExpr.Create('>=', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelLessEqualStringExpr,     TJSBinOpExpr.Create('<=', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelGreaterStringExpr,       TJSBinOpExpr.Create('>', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelLessStringExpr,          TJSBinOpExpr.Create('<', 11, [associativeLeft, associativeRight]));

   RegisterCodeGen(TRelEqualFloatExpr,          TJSBinOpExpr.Create('==', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelNotEqualFloatExpr,       TJSBinOpExpr.Create('!=', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelGreaterEqualFloatExpr,   TJSBinOpExpr.Create('>=', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelLessEqualFloatExpr,      TJSBinOpExpr.Create('<=', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelGreaterFloatExpr,        TJSBinOpExpr.Create('>', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelLessFloatExpr,           TJSBinOpExpr.Create('<', 11, [associativeLeft, associativeRight]));

   RegisterCodeGen(TRelEqualVariantExpr,        TJSBinOpExpr.Create('==', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelNotEqualVariantExpr,     TJSBinOpExpr.Create('!=', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelGreaterEqualVariantExpr, TJSBinOpExpr.Create('>=', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelLessEqualVariantExpr,    TJSBinOpExpr.Create('<=', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelGreaterVariantExpr,      TJSBinOpExpr.Create('>', 11, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelLessVariantExpr,         TJSBinOpExpr.Create('<', 11, [associativeLeft, associativeRight]));

   RegisterCodeGen(TRelEqualMetaExpr,           TJSBinOpExpr.Create('==', 10, [associativeLeft, associativeRight]));
   RegisterCodeGen(TRelNotEqualMetaExpr,        TJSBinOpExpr.Create('!=', 10, [associativeLeft, associativeRight]));

   RegisterCodeGen(TRelVarEqualNilExpr,         TdwsExprGenericCodeGen.Create(['(', '!(', 0, ')', ')']));
   RegisterCodeGen(TRelVarNotEqualNilExpr,      TdwsExprGenericCodeGen.Create(['(', 0, ')']));

   RegisterCodeGen(TIfThenElseValueExpr,        TJSIfThenElseValueExpr.Create);

   RegisterCodeGen(TIfThenExpr,                 TJSIfThenExpr.Create);
   RegisterCodeGen(TIfThenElseExpr,             TJSIfThenElseExpr.Create);

   RegisterCodeGen(TInOpExpr,                   TJSInOpExpr.Create);
   RegisterCodeGen(TStringInOpExpr,             TJSInOpExpr.Create);
   RegisterCodeGen(TStringInOpStaticSetExpr,    TJSInOpExpr.Create);
   RegisterCodeGen(TIntegerInOpExpr,            TJSInOpExpr.Create);
   RegisterCodeGen(TBitwiseInOpExpr,            TJSBitwiseInOpExpr.Create);

   RegisterCodeGen(TCaseExpr,             TJSCaseExpr.Create);
   RegisterCodeGen(TCaseStringExpr,       TJSCaseExpr.Create);
   RegisterCodeGen(TCaseIntegerExpr,      TJSCaseExpr.Create);

   RegisterCodeGen(TForUpwardExpr,        TJSForUpwardExpr.Create);
   RegisterCodeGen(TForDownwardExpr,      TJSForDownwardExpr.Create);
   RegisterCodeGen(TForUpwardStepExpr,    TJSForUpwardStepExpr.Create);
   RegisterCodeGen(TForDownwardStepExpr,  TJSForDownwardStepExpr.Create);

   RegisterCodeGen(TForCharCodeInStrExpr, TJSForCharCodeInStrExpr.Create);
   RegisterCodeGen(TForCharInStrExpr,     TJSForCharInStrExpr.Create);

   RegisterCodeGen(TWhileExpr,
      TdwsExprGenericCodeGen.Create(['while ', '(', 0, ')', ' {', #9, 1, #8, '}'], gcgStatement));
   RegisterCodeGen(TRepeatExpr,
      TdwsExprGenericCodeGen.Create(['do {', #9, 1, #8, '} while (!', 0, ')'], gcgStatement));
   RegisterCodeGen(TLoopExpr,
      TdwsExprGenericCodeGen.Create(['while (1) {', #9, 1, #8, '}'], gcgStatement));

   RegisterCodeGen(TContinueExpr,         TdwsExprGenericCodeGen.Create(['continue'], gcgStatement));
   RegisterCodeGen(TBreakExpr,            TdwsExprGenericCodeGen.Create(['break'], gcgStatement));
   RegisterCodeGen(TExitValueExpr,        TJSExitValueExpr.Create);
   RegisterCodeGen(TExitExpr,             TJSExitExpr.Create);

   RegisterCodeGen(TRaiseExpr,            TdwsExprGenericCodeGen.Create(['throw ', 0], gcgStatement));
   RegisterCodeGen(TReRaiseExpr,          TdwsExprGenericCodeGen.Create(['throw $e'], gcgStatement));
   RegisterCodeGen(TExceptExpr,           TJSExceptExpr.Create);

   RegisterCodeGen(TFinallyExpr,          TJSFinallyExpr.Create);

   RegisterCodeGen(TSetOfIncludeExpr,     TJSSetOfFunctionExpr.Create);
   RegisterCodeGen(TSetOfExcludeExpr,     TJSSetOfFunctionExpr.Create);
   RegisterCodeGen(TSetOfInExpr,          TJSSetOfInExpr.Create);
   RegisterCodeGen(TSetOfSmallInExpr,     TJSSetOfInExpr.Create);

   RegisterCodeGen(TConvStaticArrayToSetOfExpr,    TJSConvStaticArrayToSetOfExpr.Create);
   RegisterCodeGen(TConvSetOfToIntegerExpr,        TJSConvSetOfToIntegerExpr.Create);
   RegisterCodeGen(TConvIntegerToSetOfExpr,        TJSConvIntegerToSetOfExpr.Create);

   RegisterCodeGen(TNewArrayExpr,                  TJSNewArrayExpr.Create);
   RegisterCodeGen(TArraySetLengthExpr,            TJSArraySetLengthExpr.Create);
   RegisterCodeGen(TArrayAddExpr,                  TJSArrayAddExpr.Create);
   RegisterCodeGen(TArrayPeekExpr,                 TJSArrayPeekExpr.Create);
   RegisterCodeGen(TArrayPopExpr,                  TJSArrayPopExpr.Create);
   RegisterCodeGen(TArrayDeleteExpr,               TJSArrayDeleteExpr.Create);
   RegisterCodeGen(TArrayIndexOfExpr,              TJSArrayIndexOfExpr.Create);
   RegisterCodeGen(TArrayRemoveExpr,               TJSArrayRemoveExpr.Create);
   RegisterCodeGen(TArrayInsertExpr,               TJSArrayInsertExpr.Create);
   RegisterCodeGen(TArrayCopyExpr,                 TJSArrayCopyExpr.Create);
   RegisterCodeGen(TArraySwapExpr,                 TJSArraySwapExpr.Create);
   RegisterCodeGen(TArrayReverseExpr,              TdwsExprGenericCodeGen.Create([0, '.reverse()'], gcgStatement));
   RegisterCodeGen(TArraySortExpr,                 TJSArraySortExpr.Create);
   RegisterCodeGen(TArrayMapExpr,                  TJSArrayMapExpr.Create);
   RegisterCodeGen(TArrayConcatExpr,               TJSArrayConcatExpr.Create);
   RegisterCodeGen(TArraySortNaturalStringExpr,    TJSArrayTypedFluentExpr.Create('.sort()', ''));
   RegisterCodeGen(TArraySortNaturalIntegerExpr,   TJSArrayTypedFluentExpr.Create('.sort($CmpNum)', '$CmpNum'));
   RegisterCodeGen(TArraySortNaturalFloatExpr,     TJSArrayTypedFluentExpr.Create('.sort($CmpNum)', '$CmpNum'));

   RegisterCodeGen(TStaticArrayExpr,         TJSStaticArrayExpr.Create);
   RegisterCodeGen(TStaticArrayBoolExpr,     TJSStaticArrayBoolExpr.Create);
   RegisterCodeGen(TDynamicArrayExpr,        TJSDynamicArrayExpr.Create);
   RegisterCodeGen(TDynamicArrayVarExpr,     TJSDynamicArrayExpr.Create);
   RegisterCodeGen(TDynamicArraySetExpr,     TJSDynamicArraySetExpr.Create);
   RegisterCodeGen(TDynamicArraySetVarExpr,  TJSDynamicArraySetExpr.Create);
   RegisterCodeGen(TDynamicArraySetDataExpr, TJSDynamicArraySetExpr.Create);
   RegisterCodeGen(TOpenArrayExpr,           TJSDynamicArrayExpr.Create);
   RegisterCodeGen(TStringArrayOpExpr,       TJSStringArrayOpExpr.Create);
   RegisterCodeGen(TStringArraySetExpr,      TJSStringArraySetExpr.Create);
   RegisterCodeGen(TVarStringArraySetExpr,   TJSVarStringArraySetExpr.Create);
   RegisterCodeGen(TVarStringArraySetChrExpr,TJSVarStringArraySetChrExpr.Create);

   RegisterCodeGen(TAssociativeArrayGetExpr, TJSAssociativeArrayGetExpr.Create);
   RegisterCodeGen(TAssociativeArraySetExpr, TJSAssociativeArraySetExpr.Create);

   RegisterCodeGen(TStringLengthExpr,
      TdwsExprGenericCodeGen.Create([0, '.length']));
   RegisterCodeGen(TArrayLengthExpr,         TJSArrayLengthExpr.Create);
   RegisterCodeGen(TOpenArrayLengthExpr,     TJSArrayLengthExpr.Create);

   RegisterCodeGen(TOrdExpr,              TJSOrdExpr.Create);

   RegisterCodeGen(TEnumerationElementNameExpr, TJSEnumerationElementNameExpr.Create);
   RegisterCodeGen(TEnumerationElementQualifiedNameExpr, TJSEnumerationElementQualifiedNameExpr.Create);

   RegisterCodeGen(TOrdIntExpr,           TJSOrdIntExpr.Create);
   RegisterCodeGen(TOrdBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '?1:0)']));
   RegisterCodeGen(TOrdStrExpr,           TJSOrdStrExpr.Create);

   RegisterCodeGen(TFuncExpr,             TJSFuncBaseExpr.Create);
   RegisterCodeGen(TFuncSimpleExpr,       TJSFuncBaseExpr.Create);

   RegisterCodeGen(TRecordMethodExpr,     TJSRecordMethodExpr.Create);
   RegisterCodeGen(THelperMethodExpr,     TJSHelperMethodExpr.Create);

   RegisterCodeGen(TMagicIntFuncExpr,     TJSMagicFuncExpr.Create(Self));
   RegisterCodeGen(TMagicStringFuncExpr,  TJSMagicFuncExpr.Create(Self));
   RegisterCodeGen(TMagicFloatFuncExpr,   TJSMagicFuncExpr.Create(Self));
   RegisterCodeGen(TMagicBoolFuncExpr,    TJSMagicFuncExpr.Create(Self));
   RegisterCodeGen(TMagicVariantFuncExpr, TJSMagicFuncExpr.Create(Self));
   RegisterCodeGen(TMagicProcedureExpr,   TJSMagicFuncExpr.Create(Self));

   RegisterCodeGen(TConstructorStaticExpr,         TJSConstructorStaticExpr.Create);
   RegisterCodeGen(TConstructorStaticDefaultExpr,  TJSConstructorStaticExpr.Create);
   RegisterCodeGen(TConstructorVirtualExpr,        TJSConstructorVirtualExpr.Create);
   RegisterCodeGen(TConstructorVirtualObjExpr,     TJSMethodVirtualExpr.Create);
   RegisterCodeGen(TConstructorStaticObjExpr,      TJSMethodStaticExpr.Create);

   RegisterCodeGen(TConstructorAnonymousExpr,      TJSConstructorAnonymousExpr.Create);

   RegisterCodeGen(TDestructorStaticExpr,       TJSMethodStaticExpr.Create);
   RegisterCodeGen(TDestructorVirtualExpr,      TJSMethodVirtualExpr.Create);

   RegisterCodeGen(TMethodStaticExpr,           TJSMethodStaticExpr.Create);
   RegisterCodeGen(TMethodVirtualExpr,          TJSMethodVirtualExpr.Create);

   RegisterCodeGen(TMethodInterfaceExpr,        TJSMethodInterfaceExpr.Create);
   RegisterCodeGen(TMethodInterfaceAnonymousExpr, TJSMethodInterfaceAnonymousExpr.Create);

   RegisterCodeGen(TClassMethodStaticExpr,      TJSClassMethodStaticExpr.Create);
   RegisterCodeGen(TClassMethodVirtualExpr,     TJSClassMethodVirtualExpr.Create);

   RegisterCodeGen(TConnectorCallExpr,          TJSConnectorCallExpr.Create);
   RegisterCodeGen(TConnectorReadExpr,          TJSConnectorReadExpr.Create);
   RegisterCodeGen(TConnectorWriteExpr,         TJSConnectorWriteExpr.Create);
   RegisterCodeGen(TConnectorForInExpr,         TJSConnectorForInExpr.Create);
   RegisterCodeGen(TConnectorCastExpr,          TJSConnectorCastExpr.Create);

   RegisterCodeGen(TFuncPtrExpr,                TJSFuncPtrExpr.Create);
   RegisterCodeGen(TFuncRefExpr,                TJSFuncRefExpr.Create);
   RegisterCodeGen(TAnonymousFuncRefExpr,       TJSAnonymousFuncRefExpr.Create);

   RegisterCodeGen(TFieldExpr,                  TJSFieldExpr.Create);
   RegisterCodeGen(TFieldVarExpr,               TJSFieldExpr.Create);
   RegisterCodeGen(TReadOnlyFieldExpr,          TJSFieldExpr.Create);

   RegisterCodeGen(TAssertExpr,                 TJSAssertExpr.Create);
   RegisterCodeGen(TDeclaredExpr,               TJSDeclaredExpr.Create);
   RegisterCodeGen(TConditionalDefinedExpr,     TJSConditionalDefinedExpr.Create);

   RegisterCodeGen(TSwapExpr,                   TJSSwapExpr.Create);
end;

// Destroy
//
destructor TdwsJSCodeGen.Destroy;
begin
   inherited;

   FreeAndNil(FUniqueGlobalVar);
   FreeAndNil(FCustomDependency);

   FreeAndNil(FLocalVarScannedProg);
   FreeAndNil(FAllLocalVarSymbols);

   while FDeclaredLocalVarsStack.Count>0 do begin
      FDeclaredLocalVarsStack.Peek.Free;
      FDeclaredLocalVarsStack.Pop;
   end;
   FreeAndNil(FDeclaredLocalVarsStack);
   FreeAndNil(FDeclaredLocalVars);
end;

// Clear
//
procedure TdwsJSCodeGen.Clear;
begin
   FLocalVarScannedProg.Clear;
   FAllLocalVarSymbols.Clear;
   FUniqueGlobalVar.Clear;
   FCustomDependency.Clear;
   inherited;
end;

// SymbolMappedName
//
function TdwsJSCodeGen.SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : String;
var
   ct : TClass;
begin
   ct:=sym.ClassType;
   if ct=TSelfSymbol then
      Result:=SelfSymbolName
   else if ct=TResultSymbol then
      Result:=ResultSymbolName
   else if ct=TJSConnectorSymbol then
      Result:='Object'
   else if ct=TMagicFuncSymbol then begin
      if TMagicFuncSymbol(sym).IsOverloaded then
         Result:=TJSFuncBaseExpr.GetSignature(TMagicFuncSymbol(sym))
      else Result:=sym.Name;
      Dependencies.Add(Result);
   end else begin
      Result:=inherited SymbolMappedName(sym, scope);
   end;
end;

// CompileValue
//
procedure TdwsJSCodeGen.CompileValue(expr : TTypedExpr);

   function NeedArrayCopy(paramExpr : TArrayConstantExpr) : Boolean;
   var
      i : Integer;
      sub : TExprBase;
   begin
      for i:=0 to paramExpr.SubExprCount-1 do begin
         sub:=paramExpr.SubExpr[i];
         if sub is TConstExpr then continue;
         if sub is TTypedExpr then begin
            if TTypedExpr(sub).Typ.UnAliasedType is TBaseSymbol then continue;
         end;
         Exit(True);
      end;
      Result:=False;
   end;

var
   exprTypClass : TClass;
   itemTyp : TTypeSymbol;
begin
   exprTypClass:=expr.Typ.ClassType;

   if not (expr is TFuncExprBase) then begin

      if exprTypClass=TRecordSymbol then begin

         if not (   (expr is TDynamicRecordExpr)
                 or TRecordSymbol(expr.Typ).IsImmutable) then begin
            WriteString('Clone$');
            WriteSymbolName(expr.Typ);
            WriteString('(');
            CompileNoWrap(expr);
            WriteString(')');
            Exit;
         end;

      end else if    (exprTypClass=TStaticArraySymbol)
                  or (exprTypClass=TOpenArraySymbol)
                  or (exprTypClass=TSetOfSymbol) then begin

         CompileNoWrap(expr);
         if not (expr is TConstExpr) then begin
            if    (not (expr is TArrayConstantExpr))
               or NeedArrayCopy(TArrayConstantExpr(expr)) then begin
               itemTyp:=expr.Typ.Typ.UnAliasedType;
               if itemTyp is TRecordSymbol then begin
                  WriteString('.map(Clone$');
                  WriteSymbolName(itemTyp);
                  WriteString(')');
               end else begin
                  WriteString('.slice(0)');
               end;
            end;
         end;
         Exit;

      end;

   end;

   CompileNoWrap(expr);
end;

// CompileEnumerationSymbol
//
procedure TdwsJSCodeGen.CompileEnumerationSymbol(enum : TEnumerationSymbol);
var
   i : Integer;
   elem : TElementSymbol;
begin
   if enum.Elements.Count=0 then Exit;

   if not SmartLink(enum) then Exit;

   WriteSymbolVerbosity(enum);

   WriteString('var ');
   WriteSymbolName(enum);
   WriteString(' = ');

   if enum.Continuous and (enum.LowBound=0) then begin

      WriteString('[ ');
      for i:=0 to enum.Elements.Count-1 do begin
         elem:=enum.Elements[i] as TElementSymbol;
         if i>0 then
            WriteString(', ');
         WriteJavaScriptString(Output, elem.Name);
      end;
      WriteString(' ]');

   end else begin

      WriteString('{ ');
      for i:=0 to enum.Elements.Count-1 do begin
         elem:=enum.Elements[i] as TElementSymbol;
         if i>0 then
            WriteString(', ');
         if elem.Value>=0 then
            WriteString(IntToStr(elem.Value))
         else WriteString('"'+IntToStr(elem.Value)+'"');
         WriteString(':');
         WriteJavaScriptString(Output, elem.Name);
      end;
      WriteString(' }');

   end;

   WriteStatementEnd;
end;

// CompileFuncSymbol
//
procedure TdwsJSCodeGen.DoCompileFuncSymbol(func : TFuncSymbol; deAnonymize : Boolean = False);
begin
   WriteString('function ');
   if deAnonymize or (func.Name<>'') then
      WriteSymbolName(func);
   WriteString('(');
   WriteFuncParams(func);
   WriteBlockBegin(') ');

   CompileFuncBody(func);

   UnIndent;
   WriteString('}');
   if func.Name<>'' then
      WriteStatementEnd;
   if func.IsExport then begin
      if func.ExternalName<>'' then
         WriteString(func.ExternalName)
      else begin
         WriteString('window.');
         WriteString(func.Name);
      end;
      WriteString(' = ');
      WriteSymbolName(func);
      WriteStatementEnd;
   end;
end;

// DoCompileExternalRootedClassSymbol
//
procedure TdwsJSCodeGen.DoCompileExternalRootedClassSymbol(cls : TClassSymbol);
var
   i : Integer;
   firstField, needsExtend : Boolean;
   sym : TSymbol;
   fieldSym : TFieldSymbol;
   methSym : TMethodSymbol;
   proc : TdwsProcedure;
   oldSelfSymbolName : String;
   constructorSymbol : TMethodSymbol;
   locData : IDataContext;
begin
   if cls.Name='' then Exit;

   constructorSymbol:=cls.FindDefaultConstructor(cvPrivate);

   WriteString('function ');
   WriteString(cls.ExternalName);
   WriteString('(');
   if constructorSymbol<>nil then
      WriteFuncParams(constructorSymbol);
   WriteBlockBegin(') ');

   // initialize fields that cannot live in the prototype
   needsExtend:=not cls.IsSealed;
   for sym in cls.Members do begin
      if sym is TFieldSymbol then begin
         fieldSym:=TFieldSymbol(sym);
         if cls.IsSealed or (fieldSym.Typ is TArraySymbol) or (fieldSym.Typ is TRecordSymbol) then begin
            WriteString('this.');
            WriteString(fieldSym.ExternalName);
            WriteString(' = ');
            if fieldSym.DefaultValue=nil then
               WriteDefaultValue(fieldSym.Typ, False)
            else begin
               CreateDataContext(fieldSym.DefaultValue, 0, locData);
               WriteValue(fieldSym.Typ, locData);
            end;
            WriteStatementEnd;
         end;
      end else begin
         needsExtend:=needsExtend or (sym is TMethodSymbol);
      end;
   end;

   if cls.Parent.ExternalName<>'Object' then begin
      WriteString(cls.Parent.ExternalName);
      WriteString('.call(this)');
      WriteStatementEnd;
   end;
   if constructorSymbol<>nil then begin
      oldSelfSymbolName:=SelfSymbolName;
      try
         SelfSymbolName:='this';
         CompileFuncBody(constructorSymbol);
      finally
         SelfSymbolName:=oldSelfSymbolName;
      end;
   end;
   WriteBlockEndLn;

   if needsExtend then begin
      Dependencies.Add('$Extend');

      WriteString('$Extend(');
      WriteString(cls.Parent.ExternalName);
      WriteString(',');
      WriteString(cls.ExternalName);
      WriteStringLn(',');
      Indent;
      WriteBlockBegin('');

      firstField:=True;

      for i:=0 to cls.Members.Count-1 do begin
         sym:=cls.Members[i];

         if (not cls.IsSealed) and (sym is TFieldSymbol) then begin

            fieldSym:=TFieldSymbol(sym);
            if (fieldSym.Typ is TArraySymbol) or (fieldSym.Typ is TRecordSymbol) then continue;

            if not firstField then
               WriteStringLn(',')
            else firstField:=False;

            WriteLiteralString(fieldSym.ExternalName);
            WriteString(' : ');
            if fieldSym.DefaultValue=nil then
               WriteDefaultValue(fieldSym.Typ, False)
            else begin
               CreateDataContext(fieldSym.DefaultValue, 0, locData);
               WriteValue(fieldSym.Typ, locData);
            end;

         end else if (sym is TMethodSymbol) and (TMethodSymbol(sym).Kind<>fkConstructor) then begin

            if not firstField then
               WriteStringLn(',')
            else firstField:=False;

            methSym:=TMethodSymbol(sym);
            proc:=(methSym.Executable as TdwsProcedure);

            WriteSymbolVerbosity(methSym);

            //EnterScope(methSym);
            EnterContext(proc);
            oldSelfSymbolName:=SelfSymbolName;
            try
               SelfSymbolName:='this';

               WriteLiteralString(methSym.ExternalName);
               WriteString(' : function(');
               WriteFuncParams(methSym);
               WriteBlockBegin(') ');

               CompileFuncBody(methSym);

               WriteBlockEnd;

            finally
               SelfSymbolName:=oldSelfSymbolName;
               LeaveContext;
               //LeaveScope;
            end;

         end;
      end;

      if not firstField then
         WriteLineEnd;

      WriteBlockEnd;
      WriteString(')');
      UnIndent;
      WriteStatementEnd;
      WriteLineEnd;

   end;
end;

// CompileRTTIRawAttributes
//
procedure TdwsJSCodeGen.CompileRTTIRawAttributes(attributes : TdwsSymbolAttributes);
var
   firstAttrib : Boolean;
   i : Integer;
   attrib : TdwsSymbolAttribute;
   publishedSymbols : TSimpleSymbolList;
   symbol : TSymbol;
   propSym : TPropertySymbol;
   fieldSym : TFieldSymbol;
   methSym : TMethodSymbol;
   typRTTITypeInfo : TRecordSymbol;
   typRTTIRawAttribute : TRecordSymbol;
begin
   if not (cgoNoRTTI in Options) then begin
      publishedSymbols:=Context.Root.CollectAllPublishedSymbols(cgoIgnorePublishedInImplementation in Options);
      try
         typRTTITypeInfo:=Context.RootTable.FindTypeSymbol(SYS_TRTTITYPEINFO, cvMagic) as TRecordSymbol;
         typRTTIRawAttribute:=Context.RootTable.FindTypeSymbol(SYS_TRTTIRAWATTRIBUTE, cvMagic) as TRecordSymbol;

         SuspendSmartLink;
         try
            if publishedSymbols.Count>0 then
               CompileClassSymbolIfNeeded(Context.Root.SystemTable.SymbolTable.TypCustomAttribute);

            DoCompileRecordSymbol(typRTTITypeInfo);
            DoCompileRecordSymbol(typRTTIRawAttribute);
         finally
            ResumeSmartLink;
         end;

         WriteString('var $RTTI = [');
         firstAttrib:=True;
         for i:=0 to attributes.Count-1 do begin
            attrib:=attributes[i];
            if firstAttrib then begin
               WriteString(#13#10#9'{ T: {ID:');
               firstAttrib:=False;
            end else WriteString(#9',{ T: {ID:');
            CompileRTTISymbolName(attrib.Symbol);
            WriteString('}, A: function () { return ');
            CompileNoWrap(attrib.AttributeConstructor);
            WriteStringLn(' } }');
         end;

         if publishedSymbols.Count>0 then begin
            Dependencies.Add(SYS_RTTIPROPERTYATTRIBUTE);
            Dependencies.Add(SYS_RTTIMETHODATTRIBUTE);
         end;

         for i:=0 to publishedSymbols.Count-1 do begin
            symbol:=publishedSymbols[i];
            if firstAttrib then begin
               WriteString(#13#10#9'{ T: {ID:');
               firstAttrib:=False;
            end else WriteString(#9',{ T: {ID:');

            if symbol.ClassType=TPropertySymbol then begin

               propSym:=TPropertySymbol(symbol);

               CompileRTTISymbolName(propSym.OwnerSymbol);
               WriteString('}, A: { ClassType: '+SYS_RTTIPROPERTYATTRIBUTE);

               WriteString(', N: ');
               WriteJavaScriptString(Output, propSym.Name);

               WriteString(', T: {ID:');
               CompileRTTISymbolName(propSym.Typ);
               WriteString('}');

               WriteString(', C: '+IntToStr(TRTTIPropertyCapabilitiesMethod.Capabilities(propSym)));

               if propSym.ReadSym<>nil then begin
                  WriteString(', G: ');
                  if propSym.ReadSym is TFieldSymbol then begin
                     WriteString('function (s) { return s.');
                     WriteSymbolName(propSym.ReadSym);
                     WriteString(' }');
                  end else begin
                     WriteSymbolName(propSym.OwnerSymbol);
                     WriteString('.');
                     WriteSymbolName(propSym.ReadSym);
                  end;
               end;
               if propSym.WriteSym<>nil then begin
                  WriteString(', S: ');
                  if propSym.ReadSym is TFieldSymbol then begin
                     WriteString('function (s,v) { s.');
                     WriteSymbolName(propSym.ReadSym);
                     WriteString('=v }');
                  end else begin
                     WriteSymbolName(propSym.OwnerSymbol);
                     WriteString('.');
                     WriteSymbolName(propSym.WriteSym);
                  end;
               end;

            end else if symbol.ClassType=TFieldSymbol then begin

               fieldSym:=TFieldSymbol(symbol);

               CompileRTTISymbolName(fieldSym.StructSymbol);
               WriteString('}, A: { ClassType: '+SYS_RTTIPROPERTYATTRIBUTE);

               WriteString(', N: ');
               WriteJavaScriptString(Output, fieldSym.Name);

               WriteString(', T: {ID:');
               CompileRTTISymbolName(fieldSym.Typ);
               WriteString('}');

               WriteString(', C: 3');

               WriteString(', G: function (s) { return s.');
               WriteSymbolName(fieldSym);
               WriteString(' }');

               WriteString(', S: function (s,v) { s.');
               WriteSymbolName(fieldSym);
               WriteString('=v }');

            end else if symbol is TMethodSymbol then begin

               methSym:=TMethodSymbol(symbol);

               CompileRTTISymbolName(methSym.StructSymbol);
               WriteString('}, A: { ClassType: '+SYS_RTTIMETHODATTRIBUTE);

               WriteString(', N: ');
               WriteJavaScriptString(Output, methSym.Name);

               if methSym.Typ<>nil then begin
                  WriteString(', T: {ID:');
                  CompileRTTISymbolName(methSym.Typ);
                  WriteString('}');
               end;

               WriteString(', I: ');
               WriteInteger(TRTTIMethodInfoMethod.Info(methSym));

               if methSym.VMTIndex>=0 then begin
                  WriteString(', V: ');
                  WriteInteger(methSym.VMTIndex);
               end;

               WriteString(', F: ');
               WriteSymbolName(methSym.StructSymbol);
               WriteString('.');
               WriteSymbolName(methSym);
               if methSym.IsVirtual then
                  WriteString('$');

            end;

            WriteStringLn(' } }');
         end;
         WriteStringLn('];');
      finally
         publishedSymbols.Free;
      end;
   end else WriteStringLn('var $RTTI = [];');
end;

// CompileRTTISymbolName
//
procedure TdwsJSCodeGen.CompileRTTISymbolName(sym : TSymbol);
begin
   if sym is TTypeSymbol then
      sym:=TTypeSymbol(sym).UnAliasedType;
   if sym.AsFuncSymbol<>nil then
      WriteString('Function')
   else if sym is TArraySymbol then
      WriteString('Array')
   else WriteSymbolName(sym);
end;

// CompileConditions
//
procedure TdwsJSCodeGen.CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                          preConds : Boolean);

   procedure CompileInheritedConditions;
   var
      iter : TMethodSymbol;
      iterProc : TdwsProcedure;
   begin
      if func is TMethodSymbol then begin
         iter:=TMethodSymbol(func);
         while iter.IsOverride and (iter.ParentMeth<>nil) do
            iter:=iter.ParentMeth;
         if (iter<>func) and (iter is TSourceMethodSymbol) then begin
            iterProc:=TSourceMethodSymbol(iter).Executable as TdwsProcedure;
            if iterProc<>nil then begin
               if preConds then
                  CompileConditions(iter, iterProc.PreConditions, preConds)
               else CompileConditions(iter, iterProc.PostConditions, preConds);
            end;
         end;
      end;
   end;

var
   i : Integer;
   cond : TSourceCondition;
   msgFmt : String;
begin
   if preConds then
      CompileInheritedConditions;

   if (conditions=nil) or (conditions.Count=0) then Exit;

   Dependencies.Add('$CondFailed');

   if preConds then
      msgFmt:=RTE_PreConditionFailed
   else msgFmt:=RTE_PostConditionFailed;

   for i:=0 to conditions.Count-1 do begin
      cond:=conditions[i];
      WriteString('if (!');
      Compile(cond.Test);
      WriteString(') $CondFailed(');
      WriteLiteralString(Format(msgFmt, [func.QualifiedName, cond.ScriptPos.AsInfo, '']));
      WriteString(',');
      Compile(cond.Msg);
      WriteString(')');
      WriteStatementEnd;
   end;

   if not preConds then
      CompileInheritedConditions;
end;

// DoCompileCompositeSymbolInternals
//
procedure TdwsJSCodeGen.DoCompileCompositeSymbolInternals(struct : TCompositeTypeSymbol);
var
   i : Integer;
   sym : TSymbol;
begin
   for i:=0 to struct.Members.Count-1 do begin
      sym:=struct.Members[i];
      if (sym is TConstSymbol) and (sym.Typ is TArraySymbol) then begin
         WriteString('var ');
         WriteSymbolName(sym);
         WriteString(' = ');
         WriteValueData(sym.Typ, TConstSymbol(sym).Data);
         WriteStatementEnd;
      end;
   end;
end;

// DoCompileHelperSymbol
//
procedure TdwsJSCodeGen.DoCompileHelperSymbol(helper : THelperSymbol);
var
   i : Integer;
   sym : TSymbol;
begin
   // compile methods

   for i:=0 to helper.Members.Count-1 do begin
      sym:=helper.Members[i];
      if sym is TMethodSymbol then
         CompileHelperMethod(TMethodSymbol(sym));
   end;
end;

// DoCompileRecordSymbol
//
procedure TdwsJSCodeGen.DoCompileRecordSymbol(rec : TRecordSymbol);

   procedure WriteFieldAccessor(field : TFieldSymbol);
   begin
      if field.HasExternalName then begin
         if IsValidJSName(field.ExternalName) then begin
            WriteString('.');
            WriteString(field.ExternalName);
         end else begin
            WriteString('[');
            WriteLiteralString(field.ExternalName);
            WriteString(']');
         end;
      end else begin
         WriteString('.');
         WriteSymbolName(field);
      end;
   end;

var
   i, k : Integer;
   sym : TSymbol;
   field : TFieldSymbol;
   fieldTyp : TTypeSymbol;
   prop : TPropertySymbol;
   firstField : Boolean;
   publishedName : String;
   locData : IDataContext;
   activeFields : array of TFieldSymbol;
begin
   if not SmartLink(rec) then Exit;

   WriteSymbolVerbosity(rec);

   SetLength(activeFields, rec.Members.Count);
   k:=0;
   for i:=0 to rec.Members.Count-1 do begin
      sym:=rec.Members[i];
      if sym is TFieldSymbol then begin
         field:=TFieldSymbol(sym);
         if SmartLink(field) then begin
            activeFields[k]:=TFieldSymbol(sym);
            Inc(k);
         end;
      end;
   end;
   SetLength(activeFields, k);

   WriteString('function Copy$');
   WriteSymbolName(rec);
   WriteBlockBegin('(s,d) ');
   for i:=0 to High(activeFields) do begin
      field:=activeFields[i];
      fieldTyp:=field.Typ.UnAliasedType;
      if    (fieldTyp is TBaseSymbol)
         or (fieldTyp is TClassSymbol)
         or (fieldTyp is TInterfaceSymbol)
         or (fieldTyp.AsFuncSymbol<>nil)
         or (fieldTyp is TDynamicArraySymbol)
         or (fieldTyp is TEnumerationSymbol)
         or (fieldTyp is TClassOfSymbol)
         or ((fieldTyp is TCompositeTypeSymbol) and TCompositeTypeSymbol(fieldTyp).IsImmutable) then begin
         WriteString('d');
         WriteFieldAccessor(field);
         WriteString('=s');
         WriteFieldAccessor(field);
      end else if fieldTyp is TRecordSymbol then begin
         WriteString('Copy$');
         WriteSymbolName(fieldTyp);
         WriteString('(s');
         WriteFieldAccessor(field);
         WriteString(',d');
         WriteFieldAccessor(field);
         WriteString(')');
      end else if fieldTyp is TStaticArraySymbol then begin
         WriteString('d');
         WriteFieldAccessor(field);
         WriteString('=s');
         WriteFieldAccessor(field);
         if fieldTyp.Typ is TRecordSymbol then begin
            WriteString('.map(Clone$');
            WriteSymbolName(fieldTyp.Typ);
            WriteString(')');
         end else begin
            WriteString('.slice(0)');
         end;
      end else if fieldTyp is TSetOfSymbol then begin
         WriteString('d');
         WriteFieldAccessor(field);
         WriteString('=s');
         WriteFieldAccessor(field);
         WriteString('.slice(0)');
      end else raise ECodeGenUnsupportedSymbol.CreateFmt('Copy record field type %s', [fieldTyp.ClassName]);
      WriteStringLn(';');
   end;
   WriteString('return d');
   WriteStatementEnd;
   WriteBlockEndLn;

   WriteString('function Clone$');
   WriteSymbolName(rec);
   WriteBlockBegin('($) ');
   WriteBlockBegin('return ');
   firstField:=True;
   for i:=0 to High(activeFields) do begin
      if firstField then
         firstField:=False
      else WriteStringLn(',');
      field:=activeFields[i];
      fieldTyp:=field.Typ.UnAliasedType;

      if field.HasExternalName and not IsValidJSName(field.ExternalName) then
         WriteLiteralString(field.ExternalName)
      else WriteSymbolName(field);

      WriteString(':');
      if    (fieldTyp is TBaseSymbol)
         or (fieldTyp is TClassSymbol)
         or (fieldTyp is TInterfaceSymbol)
         or (fieldTyp.AsFuncSymbol<>nil)
         or (fieldTyp is TDynamicArraySymbol)
         or (fieldTyp is TClassOfSymbol)
         or (fieldTyp is TEnumerationSymbol) then begin

         WriteString('$');
         WriteFieldAccessor(field);

      end else if fieldTyp is TRecordSymbol then begin

         WriteString('Clone$');
         WriteSymbolName(fieldTyp);
         WriteString('($');
         WriteFieldAccessor(field);
         WriteString(')');

      end else if fieldTyp is TStaticArraySymbol then begin

         WriteString('$.');
         WriteSymbolName(field);
         if fieldTyp.Typ is TRecordSymbol then begin
            WriteString('.map(Clone$');
            WriteSymbolName(fieldTyp.Typ);
            WriteString(')');
         end else begin
            WriteString('.slice(0)');
         end;

      end else if fieldTyp is TSetOfSymbol then begin

         WriteString('$.');
         WriteSymbolName(field);
         WriteString('.slice(0)');

      end else raise ECodeGenUnsupportedSymbol.CreateFmt('Clone record field type %s', [fieldTyp.ClassName]);
   end;
   WriteLineEnd;
   WriteBlockEndLn;
   WriteBlockEndLn;

   if cvPublished in rec.MembersVisibilities then begin
      WriteString('function Pub$');
      WriteSymbolName(rec);
      WriteBlockBegin('($) ');
      WriteBlockBegin('return ');
      firstField:=True;
      for i:=0 to rec.Members.Count-1 do begin
         sym:=rec.Members[i];

         if sym.ClassType=TFieldSymbol then begin

            field:=TFieldSymbol(sym);
            if field.Visibility<>cvPublished then continue;
            publishedName:=field.ExternalName;

         end else if sym is TPropertySymbol then begin

            prop:=TPropertySymbol(sym);
            if (prop.Visibility<>cvPublished) or prop.HasArrayIndices or (prop.ReadSym=nil) then continue;
            sym:=prop.ReadSym;
            publishedName:=prop.Name;

         end else continue;

         if firstField then
            firstField:=False
         else WriteString(',');
         WriteLiteralString(publishedName);
         WriteString(' : ');

         if sym.Typ is TRecordSymbol then begin
            WriteString('Pub$');
            WriteSymbolName(sym.Typ);
            WriteString('(');
         end;

         if sym.ClassType=TFieldSymbol then begin

            WriteString('$');
            WriteFieldAccessor(TFieldSymbol(sym));

         end else if sym is TMethodSymbol then begin

            if TMethodSymbol(sym).IsClassMethod then begin
               WriteSymbolName(sym);
               WriteString('()');
            end else begin
               WriteSymbolName(rec);
               WriteString('$');
               WriteSymbolName(sym);
               WriteString('($)');
            end;

         end else if sym is TClassVarSymbol then begin

            WriteSymbolName(sym);

         end else if sym is TClassConstSymbol then begin

            CreateDataContext(TClassConstSymbol(sym).Data, 0, locData);
            WriteValue(sym.Typ, locData);

         end else Assert(False);

         if sym.Typ is TRecordSymbol then
            WriteString(')');

         WriteStringLn('');
      end;
      WriteBlockEndLn;
      WriteBlockEndLn;
   end;

   // compile methods

   for i:=0 to rec.Members.Count-1 do begin
      sym:=rec.Members[i];
      if sym is TMethodSymbol then
         CompileRecordMethod(TMethodSymbol(sym));
   end;
end;

// DoCompileClassSymbol
//
procedure TdwsJSCodeGen.DoCompileClassSymbol(cls : TClassSymbol);
var
   i : Integer;
   sym : TSymbol;
   meth : TMethodSymbol;
   staticAndSealed : Boolean;
begin
   inherited;

   if not SmartLink(cls) then Exit;

   WriteSymbolVerbosity(cls);

   if cls.IsExternalRooted then begin
      DoCompileExternalRootedClassSymbol(cls);
      Exit;
   end;

   WriteString('var ');
   WriteSymbolName(cls);
   WriteBlockBegin(' = ');

   staticAndSealed:=cls.IsStatic and cls.IsSealed;

   WriteString('$ClassName:');
   WriteLiteralString(cls.Name);

   if not cls.IsPureStatic then begin

      WriteString(',$Parent:');
      WriteSymbolName(cls.Parent);
      WriteLineEnd;

      Dependencies.Add('TObject');
      if not cls.IsStatic then
         Dependencies.Add('$New');

      if not staticAndSealed then begin
         WriteBlockBegin(',$Init:function ($) ');
         WriteSymbolName(cls.Parent);
         WriteString('.$Init($)');
         WriteStatementEnd;
         DoCompileFieldsInit(cls);
         UnIndent;
         WriteStringLn('}');
      end;

   end;

   // Compile methods specified by the class

   for i:=0 to cls.Members.Count-1 do begin
      sym:=cls.Members[i];
      if sym is TMethodSymbol then
         CompileMethod(TMethodSymbol(sym));
   end;

   // VMT entries for methods not overridden here

   for i:=0 to cls.VMTCount-1 do begin
      meth:=cls.VMTMethod(i);
      if not SmartLinkMethod(meth) then continue;
      if not meth.IsVirtual then continue; // devirtualized
      if staticAndSealed and (meth.Kind in [fkConstructor, fkDestructor]) then
         continue;
      if meth.StructSymbol<>cls then begin
         WriteString(',');
         WriteString(MemberName(meth, meth.StructSymbol));
         WriteString(':');
         WriteSymbolName(meth.StructSymbol);
         WriteString('.');
         WriteString(MemberName(meth, meth.StructSymbol));
         WriteLineEnd;
      end;
      if meth.StructSymbol=cls then begin
         WriteString(',');
         WriteString(MemberName(meth, meth.StructSymbol));
         WriteString(cVirtualPostfix+':');
         if meth.Kind=fkConstructor then begin
            WriteString('function($){return $.ClassType.');
         end else if meth.IsClassMethod then begin
            WriteString('function($){return $.');
         end else begin
            WriteString('function($){return $.ClassType.');
         end;
         WriteString(MemberName(meth, meth.StructSymbol));
         if meth.Params.Count=0 then
            WriteStringLn('($)}')
         else WriteStringLn('.apply($.ClassType, arguments)}');
      end;
   end;

   UnIndent;
   WriteString('}');
   WriteStatementEnd;

   DoCompileInterfaceTable(cls);
end;

// DoCompileFieldsInit
//
procedure TdwsJSCodeGen.DoCompileFieldsInit(cls : TClassSymbol);
var
   i, j, n : Integer;
   sym : TSymbol;
   fld1, fld2 : TFieldSymbol;
   flds : array of TFieldSymbol;
   locData : IDataContext;
begin
   SetLength(flds, cls.Members.Count);
   n:=0;
   for i:=0 to cls.Members.Count-1 do begin
      sym:=cls.Members[i];
      if sym is TFieldSymbol then begin
         if (TFieldSymbol(sym).Visibility=cvPublished) or SmartLink(sym) then begin
            flds[n]:=TFieldSymbol(sym);
            Inc(n);
         end;
      end;
   end;

   // aggregate initializations by type
   for i:=0 to n-1 do begin
      fld1:=flds[i];
      if fld1=nil then continue;
      for j:=i to n-1 do begin
         fld2:=flds[j];
         if fld2=nil then continue;
         if SameDefaultValue(fld1, fld2) then begin
            WriteString('$.');
            WriteString(MemberName(fld2, cls));
            WriteString(' = ');
            flds[j]:=nil;
            // records, static arrays and other value types can't be assigned together
            if not ((fld1.Typ is TBaseSymbol) or (fld1.Typ is TClassSymbol) or (fld1.Typ is TInterfaceSymbol)) then Break;
         end;
      end;
      if fld1.DefaultValue=nil then
         WriteDefaultValue(fld1.Typ, False)
      else begin
         CreateDataContext(fld1.DefaultValue, 0, locData);
         WriteValue(fld1.Typ, locData);
      end;
      WriteStatementEnd;
   end;
end;

type
   TWrittenInterfaces = class (TList)
      CodeGen : TdwsJSCodeGen;
      ClassSymbol : TClassSymbol;
      function Callback(const item : TResolvedInterface) : TSimpleHashAction;
   end;

function TWrittenInterfaces.Callback(const item : TResolvedInterface) : TSimpleHashAction;
var
   i : Integer;
   meth : TMethodSymbol;
begin
   Result:=shaNone;
   if IndexOf(item.IntfSymbol)>=0 then Exit;
   if Count>0 then
      CodeGen.WriteString(',');
   Add(item.IntfSymbol);
   CodeGen.WriteSymbolName(item.IntfSymbol);
   CodeGen.WriteString(':[');
   for i:=0 to High(item.VMT) do begin
      if i>0 then
         CodeGen.WriteString(',');
      meth:=item.VMT[i];
      CodeGen.WriteSymbolName(meth.StructSymbol);
      CodeGen.WriteString('.');
      CodeGen.WriteSymbolName(meth);
   end;
   CodeGen.WriteStringLn(']');
end;

// DoCompileInterfaceTable
//
procedure TdwsJSCodeGen.DoCompileInterfaceTable(cls : TClassSymbol);
var
   needIntfTable : Boolean;
   iter : TClassSymbol;
   writtenInterfaces : TWrittenInterfaces;
begin
   needIntfTable:=False;
   iter:=cls;
   while iter<>nil do begin
      if iter.Interfaces<>nil then begin
         needIntfTable:=True;
         Break;
      end;
      iter:=iter.Parent;
   end;
   if not needIntfTable then Exit;

   WriteSymbolName(cls);
   WriteBlockBegin('.$Intf=');

   writtenInterfaces:=TWrittenInterfaces.Create;
   try
      writtenInterfaces.CodeGen:=Self;
      iter:=cls;
      while iter<>nil do begin
         if iter.Interfaces<>nil then begin
            writtenInterfaces.ClassSymbol:=iter;
            iter.Interfaces.Enumerate(writtenInterfaces.Callback);
         end;
         iter:=iter.Parent;
      end;
   finally
      writtenInterfaces.Free;
   end;

   WriteBlockEndLn;
end;

// CompileProgramBody
//
procedure TdwsJSCodeGen.CompileProgramBody(expr : TProgramExpr);
begin
   if expr is TNullExpr then Exit;

   if FMainBodyName<>'' then begin
      WriteString('var ');
      WriteString(FMainBodyName);
      WriteBlockBegin(' = function() ');
   end;
   inherited;
   if FMainBodyName<>'' then begin
      WriteBlockEndLn;
      WriteString(FMainBodyName);
      WriteString('()');
      WriteStatementEnd;
   end;
end;

// CompileSymbolTable
//
procedure TdwsJSCodeGen.CompileSymbolTable(table : TSymbolTable);
var
   declaredOne : Boolean;

   procedure IssueSeparator;
   begin
      if not declaredOne then begin
         WriteString('var ');
         declaredOne:=True;
      end else begin
         if cgoOptimizeForSize in Options then
            WriteString(',')
         else begin
            WriteStringLn(',');
            WriteString('    ');
         end;
      end;
   end;

var
   varSym : TDataSymbol;
   constSym : TConstSymbol;
   sym : TSymbol;
begin
   inherited;
   declaredOne:=False;
   for sym in table do begin
      if sym.ClassType=TDataSymbol then begin

         varSym:=TDataSymbol(sym);
         if FDeclaredLocalVars.IndexOf(varSym)<0 then begin
            FDeclaredLocalVars.Add(varSym);
            if varSym.HasExternalName then continue;
            IssueSeparator;
            WriteSymbolName(varSym);
            if varSym.Typ.ClassType=TBaseVariantSymbol then begin
               // undefined is JS default for unassigned var
            end else begin
               WriteString(' = ');
               WriteDefaultValue(varSym.Typ, TJSExprCodeGen.IsLocalVarParam(Self, varSym));
            end;
         end;

      end else if (sym.ClassType=TConstSymbol) and (sym.Typ is TArraySymbol) then begin

         constSym:=TConstSymbol(sym);

         IssueSeparator;
         WriteSymbolName(constSym);
         WriteString(' = ');
         WriteValueData(constSym.Typ, constSym.Data);

      end;
   end;
   if declaredOne then
      WriteStatementEnd;
end;

// ReserveSymbolNames
//
procedure TdwsJSCodeGen.ReserveSymbolNames;
var
   i : Integer;
begin
   for i:=Low(cJSReservedWords) to High(cJSReservedWords) do
      SymbolMap.ReserveName(cJSReservedWords[i]);

   SymbolMap.ReserveName(MainBodyName);

   if cgoOptimizeForSize in Options then begin
      SelfSymbolName:='S';
      ResultSymbolName:='R';
   end else begin
      SelfSymbolName:='Self';
      ResultSymbolName:='Result';
   end;

   SymbolMap.ReserveName(SelfSymbolName);
   SymbolMap.ReserveName(ResultSymbolName);
end;

// CompileDependencies
//
procedure TdwsJSCodeGen.CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram);
var
   processedDependencies : TStringList;

   procedure InsertResourceDependency(const depName : String);
   var
      rs : TResourceStream;
      buf : UTF8String;
   begin
      if FlushedDependencies.IndexOf(depName)>=0 then Exit;

      rs:=TResourceStream.Create(HInstance, depName, 'dwsjsrtl');
      try
         SetLength(buf, rs.Size);
         rs.Read(buf[1], rs.Size);
         destStream.WriteString(UTF8ToString(buf));
      finally
         rs.Free;
      end;

      FlushedDependencies.Add(depName);
   end;

   procedure InsertDependencyRef(const ref : String);
   begin
      if processedDependencies.IndexOf(ref)<0 then begin
         processedDependencies.Add(ref);
         if ref[1]='!' then
            InsertResourceDependency(Copy(ref, 2, MaxInt));
      end;
   end;

   procedure InsertDependency(dep : PJSRTLDependency);
   begin
      if FlushedDependencies.IndexOf(dep.Name)>=0 then Exit;

      if dep.Dependency<>'' then
         InsertDependencyRef(dep.Dependency);
      if dep.Dependency2<>'' then
         InsertDependencyRef(dep.Dependency2);

      if dep.Code<>'' then begin
         destStream.WriteString(dep.Code);
         case dep.Code[Length(dep.Code)] of
            ';', '}' : destStream.WriteString(#13#10);
         else
            destStream.WriteString(';'#13#10);
         end;
      end;
      FlushedDependencies.Add(dep.Name);
   end;

var
   i, n : Integer;
   dependency : String;
   jsRTL : PJSRTLDependency;
begin
   if FUniqueGlobalVar.Count>0 then begin
      destStream.WriteString('var ');
      for i:=0 to FUniqueGlobalVar.Count-1 do begin
         if i>0 then
            destStream.WriteChar(',');
         destStream.WriteString('u$'+IntToSkewedBase62(i));
      end;
      destStream.WriteChar(';');
      if not (cgoOptimizeForSize in Options) then
        destStream.WriteCRLF;
   end;

   for i:=0 to FCustomDependency.Count-1 do begin
      destStream.WriteString(FCustomDependency[i]);
      if not (cgoOptimizeForSize in Options) then
        destStream.WriteCRLF;
   end;

   processedDependencies:=TStringList.Create;
   processedDependencies.Sorted:=True;
   try
      // expand dependencies
      repeat
         n:=Dependencies.List.Count;
         for i:=0 to Dependencies.List.Count-1 do begin
            jsRTL:=FindJSRTLDependency(Dependencies.List[i]);
            if jsRTL<>nil then begin
               if jsRTL.Dependency<>'' then
                  Dependencies.Add(jsRTL.Dependency);
               if jsRTL.Dependency2<>'' then
                  Dependencies.Add(jsRTL.Dependency2);
            end;
         end;
      until Dependencies.List.Count=n;
      // stream dependencies
      for i:=Dependencies.List.Count-1 downto 0 do begin
         dependency:=Dependencies.List[i];
         if FlushedDependencies.IndexOf(dependency)>=0 then
            continue;
         jsRTL:=FindJSRTLDependency(dependency);
         processedDependencies.Add(dependency);
         if jsRTL<>nil then
            InsertDependency(jsRTL)
         else if dependency='$ConditionalDefines' then begin
            destStream.WriteString('var $ConditionalDefines=');
            WriteStringArray(destStream, (prog as TdwsProgram).Root.ConditionalDefines.Value);
            destStream.WriteString(';'#13#10);
         end;
         Dependencies.List.Delete(i);
      end;
   finally
      processedDependencies.Free;
   end;
end;

// CompileResourceStrings
//
procedure TdwsJSCodeGen.CompileResourceStrings(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram);
var
   i : Integer;
   resList : TResourceStringSymbolList;
begin
   if (cgoSmartLink in Options) and (Dependencies.List.IndexOf('$R')<0) then Exit;

   resList:=prog.ProgramObject.ResourceStringList;
   if resList.Count=0 then Exit;

   destStream.WriteString('var $R = [');
   for i:=0 to resList.Count-1 do begin
      if i>0 then
         destStream.WriteString(','#13#10#9)
      else destStream.WriteString(#13#10#9);
      dwsJSON.WriteJavaScriptString(destStream, resList[i].Value);
   end;
   destStream.WriteString('];'#13#10);
end;

// GetNewTempSymbol
//
function TdwsJSCodeGen.GetNewTempSymbol : String;

   function IntToBase62(i : Integer) : String;
   var
      n : Integer;
      c : Char;
   begin
      Result:='';
      repeat
         n:=(i mod 62);
         i:=(i div 62);
         case n of
            0..9 : c:=Char(Ord('0')+n);
            10..35 : c:=Char(Ord('A')+n-10);
         else
            c:=Char(Ord('a')+n-36);
         end;
         Result:=Result+c;
      until i=0;
   end;

begin
   if cgoOptimizeForSize in Options then begin
      Result:='$t'+IntToBase62(IncTempSymbolCounter)
   end else Result:='$temp'+inherited GetNewTempSymbol;
end;

// WriteSymbolVerbosity
//
procedure TdwsJSCodeGen.WriteSymbolVerbosity(sym : TSymbol);

   procedure DoWrite;
   var
      funcSym : TFuncSymbol;
      symPos : TSymbolPosition;
      paramsDescr : String;
   begin
      if sym.Name='' then begin
         WriteString('/// anonymous ');
         WriteStringLn(sym.ClassName);
      end else if sym is TClassSymbol then begin
         WriteString('/// ');
         WriteString(sym.QualifiedName);
         WriteString(' = class (');
         WriteString(TClassSymbol(sym).Parent.QualifiedName);
         WriteStringLn(')');
      end else if sym is TRecordSymbol then begin
         WriteString('/// ');
         WriteString(sym.QualifiedName);
         WriteStringLn(' = record');
      end else if sym.AsFuncSymbol(funcSym) then begin
         WriteString('/// ');
         WriteString(cFuncKindToString[funcSym.Kind]);
         WriteString(' ');
         WriteString(funcSym.QualifiedName);
         paramsDescr:=funcSym.ParamsDescription;
         WriteString(StringReplace(paramsDescr, #13#10, '', [rfReplaceAll, rfIgnoreCase]));
         if funcSym.Typ<>nil then begin
            WriteString(' : ');
            WriteString(funcSym.Typ.QualifiedName);
         end;
         WriteLineEnd;
      end else if sym is TEnumerationSymbol then begin
         WriteString('/// ');
         WriteString(sym.QualifiedName);
         WriteStringLn(' enumeration');
      end else Exit;
      if SymbolDictionary<>nil then begin
         symPos:=SymbolDictionary.FindSymbolUsage(sym, suImplementation);
         if symPos=nil then
            symPos:=SymbolDictionary.FindSymbolUsage(sym, suDeclaration);
         if symPos<>nil then begin
            WriteString('/// ');
            WriteString(symPos.ScriptPos.AsInfo);
            WriteLineEnd;
         end;
      end;
   end;

begin
   if Verbosity>cgovNone then
      DoWrite;
end;

// WriteLiteralString
//
procedure TdwsJSCodeGen.WriteLiteralString(const s : String);
begin
   WriteIndentIfNeeded;
   dwsJSON.WriteJavaScriptString(Output, s);
end;

// WriteFloat
//
procedure TdwsJSCodeGen.WriteFloat(const v : Double; const fmt : TFormatSettings);
begin
   if IsNan(v) then
      WriteString('NaN')
   else if IsInfinite(v) then
      if v>0 then
         WriteString('Infinity')
      else WriteString('(-Infinity)')
   else WriteString(FloatToStr(v, fmt));
end;

// CollectLocalVars
//
procedure TdwsJSCodeGen.CollectLocalVars(proc : TdwsProgram);
begin
   CollectLocalVarParams(proc.InitExpr);
   CollectLocalVarParams(proc.Expr);
end;

// CollectFuncSymLocalVars
//
procedure TdwsJSCodeGen.CollectFuncSymLocalVars(funcSym : TFuncSymbol);
var
   p : TdwsProgram;
   s : TObject;
   exec : IExecutable;
begin
   if (funcSym.ClassType=TSourceFuncSymbol) or (funcSym.ClassType=TSourceMethodSymbol) then begin
      exec:=funcSym.Executable;
      if exec<>nil then begin
         s:=exec.GetSelf;
         if s is TdwsProgram then begin
            p:=TdwsProgram(s);
            if FLocalVarScannedProg.Add(p) then begin
//               EnterScope(funcSym);
               CollectLocalVars(p);
//               LeaveScope;
            end;
         end;
      end;
   end;
end;

// CollectLocalVarParams
//
procedure TdwsJSCodeGen.CollectLocalVarParams(expr : TExprBase);
begin
   if expr=nil then Exit;
   FAllLocalVarWithinTryExpr:=0;
   expr.RecursiveEnumerateSubExprs(EnumerateLocalVarParams);
end;

// EnumerateLocalVarParams
//
procedure TdwsJSCodeGen.EnumerateLocalVarParams(parent, expr : TExprBase; var abort : Boolean);
var
   funcSym : TFuncSymbol;
   varSym : TDataSymbol;
   paramSym : TParamSymbol;
   i : Integer;
begin
   if expr is TFuncExprBase then begin
      funcSym:=TFuncExprBase(expr).FuncSym;
      if funcSym<>nil then
         CollectFuncSymLocalVars(funcSym);
   end;

   if expr is TExceptionExpr then begin
      // needs to be enhanced with a truly hierarchic approach
      // currently will tag as try expr even when result isn't assigned in a try expr
      // (might be corner-case in practice, but still)
      Inc(FAllLocalVarWithinTryExpr);
      Exit;
   end else if (expr is TVarExpr) and (parent is TFuncExprBase) then begin
      funcSym:=TFuncExprBase(parent).FuncSym;

      i:=parent.IndexOfSubExpr(expr);
      if parent is TFuncPtrExpr then begin
         if i=0 then
            Exit
         else Dec(i);
      end else if (parent is TMethodExpr) then begin
         if (i=0) then
            Exit
         else Dec(i);
      end else if (i>0) and (parent is TConstructorStaticExpr) then begin
         Dec(i);
      end;
      if (funcSym=nil) or (i>=funcSym.Params.Count) then begin
         if (parent.ClassType=TDecVarFuncExpr) or (parent.ClassType=TIncVarFuncExpr) then begin
            // special case handled via ++, --, += or -= no need to pass by ref
            Exit; // special case handled via ++ or --, no need to pass by ref
         end else begin
            // else not supported yet
            Exit;
         end;
      end else begin
         paramSym:=funcSym.Params[i] as TParamSymbol;
         if ShouldBoxParam(paramSym) then
            varSym:=TVarExpr(expr).DataSym
         else Exit;
      end;
   end else if (expr is TExitExpr) then begin
      // exit with a try.. clause that modifies the result can cause issues
      // with JS immutability, this is a heavy-handed solution
      if FAllLocalVarWithinTryExpr=0 then Exit;
      varSym:=LocalTable.FindSymbol(SYS_RESULT, cvMagic) as TDataSymbol;
   end else begin
      // else not supported yet
      Exit;
   end;
   FAllLocalVarSymbols.Add(varSym);
end;

// CollectInitExprLocalVars
//
procedure TdwsJSCodeGen.CollectInitExprLocalVars(initExpr : TBlockExprBase);
var
   i : Integer;
   curExpr, subExpr : TExprBase;
   expr : TVarExpr;
   varSym : TDataSymbol;
begin
   for i:=0 to initExpr.SubExprCount-1 do begin
      curExpr:=initExpr.SubExpr[i];
      if curExpr is TBlockExprBase then begin
         CollectInitExprLocalVars(TBlockExprBase(curExpr));
      end else if (curExpr is TAssignExpr) or (curExpr is TInitDataExpr) then begin
         subExpr:=curExpr.SubExpr[0];
         if subExpr is  TVarExpr then begin
            expr:=TVarExpr(subExpr);
            varSym:=expr.DataSym; // FindSymbolAtStackAddr(expr.StackAddr, Context.Level);
            FDeclaredLocalVars.Add(varSym);
         end;
      end;
   end;
end;

// CreateSymbolMap
//
function TdwsJSCodeGen.CreateSymbolMap(parentMap : TdwsCodeGenSymbolMap; symbol : TSymbol) : TdwsCodeGenSymbolMap;
begin
   if cgoObfuscate in Options then
      Result:=TdwsCodeGenSymbolMapJSObfuscating.Create(Self, parentMap, symbol)
   else Result:=TdwsCodeGenSymbolMap.Create(Self, parentMap, symbol);
end;

// EnterContext
//
procedure TdwsJSCodeGen.EnterContext(proc : TdwsProgram);
begin
   inherited;

   FDeclaredLocalVarsStack.Push(FDeclaredLocalVars);
   FDeclaredLocalVars:=TDataSymbolList.Create;

   CollectInitExprLocalVars(proc.InitExpr);
   CollectLocalVarParams(proc.Expr);
end;

// LeaveContext
//
procedure TdwsJSCodeGen.LeaveContext;
begin
   FDeclaredLocalVars.Free;
   FDeclaredLocalVars:=FDeclaredLocalVarsStack.Peek;
   FDeclaredLocalVarsStack.Pop;

   inherited;
end;

// SameDefaultValue
//
function TdwsJSCodeGen.SameDefaultValue(typ1, typ2 : TTypeSymbol) : Boolean;
begin
   Result:=   (typ1=typ2)
           or (    ((typ1 is TClassSymbol) or (typ1.AsFuncSymbol<>nil) or (typ1 is TInterfaceSymbol))
               and ((typ2 is TClassSymbol) or (typ2.AsFuncSymbol<>nil) or (typ2 is TInterfaceSymbol)) );
end;

// SameDefaultValue
//
function TdwsJSCodeGen.SameDefaultValue(fld1, fld2 : TFieldSymbol) : Boolean;
begin
   Result:=SameDefaultValue(fld1.Typ, fld2.Typ);
   if not Result then Exit;

   if (fld1.DefaultValue=nil) and (fld2.DefaultValue=nil) then Exit;
   if (fld1.DefaultValue=nil) or (fld2.DefaultValue=nil) then Exit(False);

   Result:=DWSSameData(fld1.DefaultValue, fld2.DefaultValue, 0, 0, fld1.Typ.Size);
end;

// WriteDefaultValue
//
procedure TdwsJSCodeGen.WriteDefaultValue(typ : TTypeSymbol; box : Boolean);
var
   i : Integer;
   comma : Boolean;
   sas : TStaticArraySymbol;
   setOfSym : TSetOfSymbol;
   recSym : TRecordSymbol;
   member : TFieldSymbol;
begin
   typ:=typ.UnAliasedType;

   if box then
      WriteString('{'+cBoxFieldName+':');
   if typ is TBaseIntegerSymbol then
      WriteString('0')
   else if typ is TBaseFloatSymbol then
      WriteString('0')
   else if typ is TBaseStringSymbol then
      WriteString('""')
   else if typ is TBaseBooleanSymbol then
      WriteString(cBoolToJSBool[false])
   else if typ is TBaseVariantSymbol then
      WriteString('undefined')
   else if typ is TClassSymbol then
      WriteString('null')
   else if typ is TClassOfSymbol then
      WriteString('null')
   else if typ.AsFuncSymbol<>nil then
      WriteString('null')
   else if typ is TInterfaceSymbol then
      WriteString('null')
   else if typ is TEnumerationSymbol then
      WriteInteger(TEnumerationSymbol(typ).DefaultValue)
   else if typ is TStaticArraySymbol then begin
      sas:=TStaticArraySymbol(typ);
      if sas.ElementCount<cInlineStaticArrayLimit then begin
         // initialize "small" static arrays inline
         WriteString('[');
         for i:=0 to sas.ElementCount-1 do begin
            if i>0 then
               WriteString(',');
            WriteDefaultValue(sas.Typ, False);
         end;
         WriteString(']');
      end else begin
         // use a function for larger ones
         WriteBlockBegin('function () ');
         WriteString('for (var r=[],i=0; i<'+IntToStr(sas.ElementCount)+'; i++) r.push(');
         WriteDefaultValue(sas.Typ, False);
         WriteStringLn(');');
         WriteStringLn('return r');
         WriteBlockEnd;
         WriteString('()');
      end;
   end else if typ is TDynamicArraySymbol then begin
      WriteString('[]');
   end else if typ is TAssociativeArraySymbol then begin
      WriteString('{}');
   end else if typ is TSetOfSymbol then begin
      setOfSym:=TSetOfSymbol(typ);
      WriteString('[');
      for i:=0 to (setOfSym.CountValue div 32) do begin
         if i>0 then
            WriteString(',');
         WriteString('0');
      end;
      WriteString(']');
   end else if typ is TRecordSymbol then begin
      recSym:=TRecordSymbol(typ);
      WriteString('{');
      comma:=False;
      for i:=0 to recSym.Members.Count-1 do begin
         if not (recSym.Members[i] is TFieldSymbol) then continue;
         if comma then
            WriteString(',')
         else comma:=True;
         member:=TFieldSymbol(recSym.Members[i]);
         WriteSymbolName(member);
         WriteString(':');
         WriteDefaultValue(member.Typ, False);
      end;
      WriteString('}');
   end else raise ECodeGenUnsupportedSymbol.CreateFmt('Default value of type %s', [typ.ClassName]);
   if box then
      WriteString('}');
end;

// WriteVariant
//
procedure TdwsJSCodeGen.WriteVariant(typ : TTypeSymbol; const v : Variant);
begin
   case VarType(v) of
      varEmpty :
         WriteString('undefined');
      varNull :
         WriteString('null');
      varInteger, varSmallint, varShortInt, varInt64, varByte, varWord, varUInt64 :
         WriteInteger(v);
      varSingle, varDouble, varCurrency :
         WriteFloat(v, cFormatSettings);
      varString, varUString, varOleStr :
         WriteLiteralString(v);
      varBoolean :
         WriteString(cBoolToJSBool[Boolean(v)]);
      varUnknown :
         if TVarData(v).VUnknown=nil then
            WriteString('null')
         else raise ECodeGenUnsupportedSymbol.CreateFmt('Non nil varUnknown value (%s)',
                                                        [typ.ClassName]);
   else
      raise ECodeGenUnsupportedSymbol.CreateFmt('Value of type %s (VarType = %d)',
                                                [typ.ClassName, VarType(v)]);
   end;
end;

// WriteValue
//
procedure TdwsJSCodeGen.WriteValue(typ : TTypeSymbol; const dataPtr : IDataContext);
var
   i : Integer;
   recSym : TRecordSymbol;
   member : TFieldSymbol;
   sas : TStaticArraySymbol;
   setOfSym : TSetOfSymbol;
   intf : IUnknown;
   comma : Boolean;
   locData : IDataContext;
   v : Variant;
   buf : String;
begin
   typ:=typ.UnAliasedType;

   if (typ is TBaseIntegerSymbol) or (typ is TEnumerationSymbol) then
      WriteInteger(dataPtr.AsInteger[0])
   else if typ is TBaseFloatSymbol then
      WriteFloat(dataPtr.AsFloat[0], cFormatSettings)
   else if typ is TBaseStringSymbol then begin
      dataPtr.EvalAsString(0, buf);
      WriteLiteralString(buf);
   end else if typ is TBaseBooleanSymbol then begin
      WriteString(cBoolToJSBool[dataPtr.AsBoolean[0]])
   end else if typ is TBaseVariantSymbol then begin
      dataPtr.EvalAsVariant(0, v);
      WriteVariant(typ, v);
   end else if typ is TNilSymbol then begin
      WriteString('null')
   end else if typ is TClassOfSymbol then begin
      dataPtr.EvalAsVariant(0, v);
      case VarType(v) of
         varNull, varEmpty :
            WriteString('null');
         varUnknown : begin
            Assert(TVarData(v).VUnknown=nil);
            WriteString('null');
         end;
         varInt64 : begin
            if TVarData(v).VInt64=0 then
               WriteString('null')
            else begin
               WriteSymbolName(TObject(TVarData(v).VInt64) as TClassSymbol);
            end;
         end;
      else
         Assert(False, 'Unsupported VarType '+IntToStr(VarType(v)));
      end;
   end else if typ is TSetOfSymbol then begin
      setOfSym:=TSetOfSymbol(typ);
      WriteString('[');
      for i:=0 to (setOfSym.CountValue div 32) do begin
         if i>0 then
            WriteString(',');
         if (i and 1)=0 then
            WriteInteger(dataPtr.AsInteger[i shr 1] and $FFFFFFFF)
         else WriteInteger(dataPtr.AsInteger[i shr 1] shr 32);
      end;
      WriteString(']');
   end else if typ is TStaticArraySymbol then begin
      sas:=TStaticArraySymbol(typ);
      WriteString('[');
      for i:=0 to sas.ElementCount-1 do begin
         if i>0 then
            WriteString(',');
         dataPtr.CreateOffset(i*sas.Typ.Size, locData);
         WriteValue(sas.Typ, locData);
      end;
      WriteString(']');
   end else if typ is TRecordSymbol then begin
      recSym:=TRecordSymbol(typ);
      WriteString('{');
      comma:=False;
      for i:=0 to recSym.Members.Count-1 do begin
         if recSym.Members[i].ClassType<>TFieldSymbol then continue;
         member:=TFieldSymbol(recSym.Members[i]);
         if (recSym.Name='') and (member.Visibility<>cvPublished) then continue;
         if comma then
            WriteString(',')
         else comma:=True;
         WriteSymbolName(member);
         WriteString(':');
         dataPtr.CreateOffset(member.Offset, locData);
         WriteValue(member.Typ, locData);
      end;
      WriteString('}');
   end else if typ is TClassSymbol then begin
      intf:=dataPtr.AsInterface[0];
      if intf=nil then
         WriteString('null')
      else raise ECodeGenUnsupportedSymbol.Create('Non nil class symbol');
   end else if typ is TDynamicArraySymbol then begin
      intf:=dataPtr.AsInterface[0];
      if IScriptDynArray(intf).ArrayLength=0 then
         WriteString('[]')
      else raise ECodeGenUnsupportedSymbol.Create('Non empty dynamic array symbol');
   end else if typ is TAssociativeArraySymbol then begin
      intf:=dataPtr.AsInterface[0];
      if IScriptAssociativeArray(intf).Count=0 then
         WriteString('{}')
      else raise ECodeGenUnsupportedSymbol.Create('Non empty assiocative array symbol');
   end else if typ is TInterfaceSymbol then begin
      intf:=dataPtr.AsInterface[0];
      if intf=nil then
         WriteString('null')
      else raise ECodeGenUnsupportedSymbol.Create('Non nil interface symbol');
   end else if typ is TSourceFuncSymbol then begin
      intf:=dataPtr.AsInterface[0];
      if intf=nil then
         WriteString('null')
      else raise ECodeGenUnsupportedSymbol.Create('Non nil function pointer symbol');
   end else begin
      raise ECodeGenUnsupportedSymbol.CreateFmt('Value of type %s',
                                                [typ.ClassName]);
   end;
end;

// WriteValueData
//
procedure TdwsJSCodeGen.WriteValueData(typ : TTypeSymbol; const data : TData);
var
   dc : TDataContext;
   idc : IDataContext;
begin
   dc:=TDataContext.Create;
   dc.ReplaceData(data);
   idc:=dc;
   WriteValue(typ, idc);
end;

// WriteStringArray
//
procedure TdwsJSCodeGen.WriteStringArray(destStream  : TWriteOnlyBlockStream; strings : TStrings);
var
   i : Integer;
begin
   destStream.WriteString('[');
   for i:=0 to strings.Count-1 do begin
      if i<>0 then
         destStream.WriteString(',');
      dwsJSON.WriteJavaScriptString(destStream, strings[i]);
   end;
   destStream.WriteString(']');
end;

// WriteStringArray
//
procedure TdwsJSCodeGen.WriteStringArray(strings : TStrings);
begin
   WriteStringArray(Output, strings);
end;

// WriteFuncParams
//
procedure TdwsJSCodeGen.WriteFuncParams(func : TFuncSymbol);
var
   i : Integer;
   needComma : Boolean;
begin
   if     (func is TMethodSymbol)
      and (not TMethodSymbol(func).StructSymbol.IsExternalRooted)
      and not (   TMethodSymbol(func).IsStatic
               or (TMethodSymbol(func).StructSymbol is TRecordSymbol)
               or (TMethodSymbol(func).StructSymbol is THelperSymbol)) then begin
      WriteString(SelfSymbolName);
      needComma:=True;
   end else needComma:=False;

   for i:=0 to func.Params.Count-1 do begin
      if needComma then
         WriteString(', ');
      WriteSymbolName(func.Params[i]);
      needComma:=True;
   end;
end;

// EnumerateAbortOnResultExpr
//
procedure TdwsJSCodeGen.EnumerateAbortOnResultExpr(parent, expr : TExprBase; var abort : Boolean);
begin
   abort:=    (expr is TVarExpr)
          and (TVarExpr(expr).DataSym is TResultSymbol);
end;

// CompileFuncBody
//
procedure TdwsJSCodeGen.CompileFuncBody(func : TFuncSymbol);

   function ResultIsNotUsedInExpr(anExpr : TExprBase) : Boolean;
   begin
      Result:=not anExpr.RecursiveEnumerateSubExprs(EnumerateAbortOnResultExpr);
   end;

var
   resultTyp : TTypeSymbol;
   proc : TdwsProcedure;
   resultIsBoxed : Boolean;
   param : TParamSymbol;
   i : Integer;
   cg : TdwsExprCodeGen;
   assignExpr : TAssignExpr;
   exec : IExecutable;
begin
   exec:=func.Executable;
   if (exec=nil) or (exec.GetSelf.ClassType=TEmptyFunc) then Exit;
   proc:=(exec.GetSelf as TdwsProcedure);

   // box params that the function will pass as var
   for i:=0 to proc.Func.Params.Count-1 do begin
      param:=proc.Func.Params[i] as TParamSymbol;
      if     (not ShouldBoxParam(param)) and TJSExprCodeGen.IsLocalVarParam(Self, param) then begin
         WriteSymbolName(param);
         WriteString('={'+cBoxFieldName+':');
         WriteSymbolName(param);
         WriteString('}');
         WriteStatementEnd;
      end;
   end;

   resultTyp:=func.Typ;
   if resultTyp<>nil then begin
      resultIsBoxed:=TJSExprCodeGen.IsLocalVarParam(Self, func.Result);
      resultTyp:=resultTyp.UnAliasedType;

      // optimize to a straight "return" statement for trivial functions
      if     (not resultIsBoxed) and (proc.Table.Count=0)
         and ((proc.InitExpr=nil) or (proc.InitExpr.SubExprCount=0))
         and (proc.Expr is TAssignExpr) then begin

         assignExpr:=TAssignExpr(proc.Expr);

         if     (assignExpr.Left is TVarExpr)
            and (TVarExpr(assignExpr.Left).DataSym is TResultSymbol) then begin

            cg:=FindCodeGen(assignExpr);
            if (cg is TJSAssignExpr) and ResultIsNotUsedInExpr(assignExpr.Right) then begin

               WriteString('return ');
               TJSAssignExpr(cg).CodeGenRight(Self, assignExpr);
               WriteStatementEnd;
               Exit;

            end

         end;

      end;

      WriteString('var ');
      WriteString(ResultSymbolName);
      WriteString(' = ');
      WriteDefaultValue(resultTyp, resultIsBoxed);
      WriteStatementEnd;
   end else resultIsBoxed:=False;

   if resultIsBoxed then
      WriteBlockBegin('try ');

   if not (cgoNoConditions in Options) then
      CompileConditions(func, proc.PreConditions, True);

   Compile(proc.InitExpr);

   CompileSymbolTable(proc.Table);

   CompileStatement(proc.Expr);

   if not (cgoNoConditions in Options) then
      CompileConditions(func, proc.PostConditions, False);

   if resultTyp<>nil then begin
      if resultIsBoxed then begin
         WriteBlockEnd;
         WriteString(' finally {return ');
         WriteString(ResultSymbolName);
         WriteString('.');
         WriteString(cBoxFieldName);
         WriteStringLn('}')
      end else begin
         WriteString('return ');
         WriteStringLn(ResultSymbolName);
      end;
   end;
end;

// CompileMethod
//
procedure TdwsJSCodeGen.CompileMethod(meth : TMethodSymbol);
var
   proc : TdwsProcedure;
begin
   if not (meth.Executable is TdwsProcedure) then Exit;
   proc:=(meth.Executable as TdwsProcedure);

   if not SmartLinkMethod(meth) then Exit;

   if not (meth.IsVirtual or meth.IsInterfaced) then
      if meth.Kind in [fkProcedure, fkFunction, fkMethod] then
         if (meth.Visibility<>cvPublished) or (cgoNoRTTI in Options) then
            if not SmartLink(meth) then Exit;

   WriteSymbolVerbosity(meth);

   WriteString(',');
   WriteString(MemberName(meth, meth.StructSymbol));

//   EnterScope(meth);
   EnterContext(proc);
   try

      WriteString(':function(');
      WriteFuncParams(meth);
      WriteBlockBegin(') ');

      CompileFuncBody(meth);

      if meth.Kind=fkConstructor then begin
         WriteString('return ');
         WriteStringLn(SelfSymbolName);
      end;

      WriteBlockEndLn;

   finally
      LeaveContext;
//      LeaveScope;
   end;
end;

// CompileRecordMethod
//
procedure TdwsJSCodeGen.CompileRecordMethod(meth : TMethodSymbol);
var
   proc : TdwsProcedure;
begin
   if not (meth.Executable is TdwsProcedure) then Exit;
   proc:=(meth.Executable as TdwsProcedure);

   if not SmartLink(meth) then Exit;

   WriteSymbolVerbosity(meth);

   WriteString('function ');
   if not meth.IsClassMethod then begin
      WriteSymbolName(meth.StructSymbol);
      WriteString('$');
   end;
   WriteSymbolName(meth);

//   EnterScope(meth);
   EnterContext(proc);
   try

      WriteString('(');
      WriteFuncParams(meth);
      WriteBlockBegin(') ');

      CompileFuncBody(meth);

      if meth.Kind=fkConstructor then begin
         WriteString('return ');
         WriteStringLn(SelfSymbolName);
      end;

      WriteBlockEndLn;

   finally
      LeaveContext;
//      LeaveScope;
   end;
end;

// CompileHelperMethod
//
procedure TdwsJSCodeGen.CompileHelperMethod(meth : TMethodSymbol);
var
   proc : TdwsProcedure;
begin
   if not (meth.Executable is TdwsProcedure) then Exit;
   proc:=(meth.Executable as TdwsProcedure);

   if not SmartLink(meth) then Exit;

   WriteSymbolVerbosity(meth);

   WriteString('function ');
   if not meth.IsClassMethod then begin
      WriteSymbolName(meth.StructSymbol);
      WriteString('$');
   end;
   WriteSymbolName(meth);

//   EnterScope(meth);
   EnterContext(proc);
   try

      WriteString('(');
      WriteFuncParams(meth);
      WriteBlockBegin(') ');

      CompileFuncBody(meth);

      if meth.Kind=fkConstructor then begin
         WriteString('return ');
         WriteStringLn(SelfSymbolName);
      end;

      WriteBlockEndLn;

   finally
      LeaveContext;
//      LeaveScope;
   end;
end;

// MemberName
//
function TdwsJSCodeGen.MemberName(sym : TSymbol; cls : TCompositeTypeSymbol) : String;
//var
//   n : Integer;
//   match : TSymbol;
begin
   Result:=SymbolMappedName(sym, cgssGlobal);
//   n:=0;
//   cls:=cls.Parent;
//   while cls<>nil do begin
//      match:=cls.Members.FindSymbol(sym.Name, cvMagic);
//      if match<>nil then begin
//         if     (   (sym.ClassType=match.ClassType)
//                 or ((sym.ClassType=TSourceMethodSymbol) and (match.ClassType=TMethodSymbol)))
//            and (sym is TMethodSymbol)
//            and (TMethodSymbol(sym).IsVirtual)
//            and (TMethodSymbol(sym).VMTIndex=TMethodSymbol(match).VMTIndex) then begin
//            // method override
//         end else Inc(n);
//      end;
//      cls:=cls.Parent;
//   end;
//   Result:=SymbolMappedName(sym, False);
//   if n>0 then
//      Result:=Format('%s$%d', [Result, n]);
end;

// WriteCompiledOutput
//
procedure TdwsJSCodeGen.WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram);
var
   buf : TWriteOnlyBlockStream;
begin
   if cgoOptimizeForSize in Options then begin
      buf:=TWriteOnlyBlockStream.Create;
      try
         inherited WriteCompiledOutput(buf, prog);
         JavaScriptMinify(buf.ToString, dest);
      finally
         buf.Free;
      end;
   end else inherited WriteCompiledOutput(dest, prog);
end;

// WriteSourceMap
//
procedure TdwsJSCodeGen.WriteSourceMap(dest : TWriteOnlyBlockStream;
                                       const srcFileName, outFileName : String;
                                       const srcRoot, srcSuffix : String);

const
   cBase64 : String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

   procedure WriteVLQ(mappings : TWriteOnlyBlockStream; v : Integer; var previous : Integer);
   var
      vlq, digit : Integer;
   begin
      vlq:=v-previous;
      previous:=v;
      if vlq=0 then begin
         mappings.WriteChar('A');
         Exit;
      end;
      if vlq<0 then
         vlq:=1-2*vlq
      else vlq:=2*vlq;

      repeat
         digit := vlq and 31;
         vlq := vlq shr 5;
         if vlq > 0 then
            digit := digit or 32;
         mappings.WriteChar(cBase64[digit+1]);
      until vlq = 0;
   end;

var
   i, k : Integer;
   prevOutColumn, prevSrcFile, prevSrcLine, prevSrcColumn : Integer;
   sources, names : TStringList;
   map : array of Integer;
   info : PdwsMapInfo;
   json : TdwsJSONWriter;
   mappings : TWriteOnlyBlockStream;
begin
   json:=TdwsJSONBeautifiedWriter.Create(dest, 0, 1);
   sources:=TStringList.Create;
   names:=TStringList.Create;
   mappings:=TWriteOnlyBlockStream.Create;
   try
      json.BeginObject;

      json.WriteName('version');
      json.WriteInteger(3);
      json.WriteName('file');
      json.WriteString(outFileName);
      json.WriteName('sourceRoot');
      json.WriteString(srcRoot);

      // prepare map array
      k:=0;
      for i:=0 to SourceMap.Count-1 do begin
         info:=@SourceMap.Infos[i];
         if info.OutputLine>k then
            k:=info.OutputLine;
      end;
      SetLength(map, k+1+OutputLineOffset);
      for i:=0 to High(map) do
         map[i]:=-1;

      // fill map array
      for i:=0 to SourceMap.Count-1 do begin
         info:=@SourceMap.Infos[i];
         sources.Add(info.ScriptPos.SourceFile.Name);
         k:=info.OutputLine+OutputLineOffset;
         if map[k]<0 then
            map[k]:=i;
      end;

      // compute mappings
      prevOutColumn:=0;
      prevSrcFile:=0;
      prevSrcLine:=0;
      prevSrcColumn:=0;
      for i:=1 to High(map) do begin
         if map[i]>=0 then begin
            info:=@SourceMap.Infos[map[i]];
            // zero-based column in output line
            WriteVLQ(mappings, 0, prevOutColumn);
            // source file index
            WriteVLQ(mappings, sources.IndexOf(info.ScriptPos.SourceFile.Name), prevSrcFile);
            // zero-based line in source file
            WriteVLQ(mappings, info.ScriptPos.Line-1, prevSrcLine);
            // zero-based column in source file
            WriteVLQ(mappings, info.ScriptPos.Col-1, prevSrcColumn);
            // name associated to segment
         end else begin
            // unmapped lines go straight to js
            WriteVLQ(mappings, 0, prevOutColumn);
            WriteVLQ(mappings, sources.Count, prevSrcFile);
            WriteVLQ(mappings, i-1, prevSrcLine);
            WriteVLQ(mappings, 0, prevSrcColumn);
         end;
         prevOutColumn:=0;
         mappings.WriteChar(';');
      end;

      // translate main module name
      i:=sources.IndexOf(MSG_MainModule);
      if i>=0 then
         sources[i]:=srcFileName;
      if srcSuffix<>'' then begin
         for i:=0 to sources.Count-1 do
            sources[i]:=sources[i]+srcSuffix;
      end;
      sources.Add(outFileName);

      // write names and mappings
      json.WriteName('sources');
      json.WriteStrings(sources);
      json.WriteName('names');
      json.WriteStrings(names);
      json.WriteName('mappings');
      json.WriteString(mappings.ToString);

      json.EndObject;
   finally
      mappings.Free;
      names.Free;
      sources.Free;
      json.Free;
   end;
end;

// CompiledSourceMap
//
function TdwsJSCodeGen.CompiledSourceMap(const srcFileName, outFileName : String;
                                         const srcRoot : String = '';
                                         const srcSuffix : String = '.pas') : String;
var
   dest : TWriteOnlyBlockStream;
begin
   dest:=TWriteOnlyBlockStream.Create;
   try
      WriteSourceMap(dest, srcFileName, outFileName, srcRoot, srcSuffix);
      Result:=dest.ToString;
   finally
      dest.Free;
   end;
end;

// WriteDebuggingSourceFile
//
type
   TDebugSymbol = class (TRefCountedObject)
      Symbol : TSymbol;
      Column : Integer;
   end;

   TSortedDebugSymbols = class(TSortedList<TDebugSymbol>)
      function Compare(const item1, item2 : TDebugSymbol) : Integer; override;
   end;

function TSortedDebugSymbols.Compare(const item1, item2 : TDebugSymbol) : Integer;
begin
   Result:=item1.Column-item2.Column;
end;

procedure TdwsJSCodeGen.WriteDebuggingSourceFile(dest : TWriteOnlyBlockStream;
                                                 srcFile : TSourceFile);
var
   i, j, line, p : Integer;
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
   debugSymbols : TSortedDebugSymbols;
   debugSym : TDebugSymbol;
   sourceLines : array of TSortedDebugSymbols;
   source : TStringList;
   lineBuf : String;
   checkNoMap : Boolean;
begin
   source:=TStringList.Create;
   try
      source.Text:=srcFile.Code;
      SetLength(sourceLines, source.Count);

      // collect all symbols and locations relevant to the sourcefile
      for i:=0 to SymbolDictionary.Count-1 do begin
         symPosList:=SymbolDictionary[i];
         checkNoMap:=True;
         for j:=0 to symPosList.Count-1 do begin
            symPos:=symPosList.Items[j];
            if suImplicit in symPos.SymbolUsages then continue;
            if symPos.ScriptPos.SourceFile=srcFile then begin
               if checkNoMap then begin
                  if SymbolMappedName(symPosList.Symbol, cgssNoMap)='' then
                     break;
                  checkNoMap:=False;
               end;
               line:=symPos.ScriptPos.Line-1;
               Assert(line<source.Count);
               debugSymbols:=sourceLines[line];
               if debugSymbols=nil then begin
                  debugSymbols:=TSortedDebugSymbols.Create;
                  sourceLines[line]:=debugSymbols;
               end;
               debugSym:=TDebugSymbol.Create;
               debugSym.Symbol:=symPosList.Symbol;
               debugSym.Column:=symPos.ScriptPos.Col;
               debugSymbols.Add(debugSym);
            end;
         end;
      end;

      // write all source lines, substituting mapped symbols
      for i:=0 to source.Count-1 do begin
         debugSymbols:=sourceLines[i];
         lineBuf:=source[i];
         if debugSymbols=nil then
            dest.WriteString(lineBuf)
         else begin
            p:=1;
            for j:=0 to debugSymbols.Count-1 do begin
               debugSym:=debugSymbols[j];
               dest.WriteSubString(lineBuf, p, debugSym.Column-p);
               dest.WriteString(SymbolMappedName(debugSym.Symbol, cgssNoMap));
               p:=debugSym.Column+Length(debugSym.Symbol.Name);
            end;
            dest.WriteSubString(lineBuf, p, Length(lineBuf)-p+1);
         end;
         dest.WriteString(#13#10);
      end;
   finally
      for i:=0 to High(sourceLines) do begin
         if sourceLines[i]<>nil then begin
            sourceLines[i].Clean;
            sourceLines[i].Destroy;
         end;
      end;
      source.Free;
   end;
end;

// DebuggingSourceFile
//
function TdwsJSCodeGen.DebuggingSourceFile(srcFile : TSourceFile) : String;
var
   wobs : TWriteOnlyBlockStream;
begin
   wobs:=TWriteOnlyBlockStream.Create;
   try
      WriteDebuggingSourceFile(wobs, srcFile);
      Result:=wobs.ToString;
   finally
      wobs.Free;
   end;
end;

// NewUniqueGlobalVar
//
function TdwsJSCodeGen.NewUniqueGlobalVar(const uniqueText : String) : String;
var
   i : Integer;
begin
   if uniqueText='' then begin
      i:=FUniqueGlobalVar.Add('');
   end else begin
      i:=FUniqueGlobalVar.IndexOf(uniqueText);
      if i<0 then
         i:=FUniqueGlobalVar.Add(uniqueText);
   end;
   Result:='u$'+IntToSkewedBase62(i);
end;

// RegisterCustomDependency
//
procedure TdwsJSCodeGen.RegisterCustomDependency(const code : String);
var
   i : Integer;
begin
   i:=FCustomDependency.IndexOf(code);
   if i<0 then
      FCustomDependency.Add(code);
end;

// All_RTL_JS
//
class function TdwsJSCodeGen.All_RTL_JS : String;
begin
   Result:=All_RTL_JS;
end;

// IgnoreRTLDependencies
//
procedure TdwsJSCodeGen.IgnoreRTLDependencies;
begin
   IgnoreJSRTLDependencies(Dependencies.List);
end;

// ------------------
// ------------------ TJSBlockInitExpr ------------------
// ------------------

// CodeGen
//
procedure TJSBlockInitExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   inVar : Boolean;

   procedure WriteVar;
   begin
      if inVar then begin
         codeGen.WriteString(',');
         codeGen.WriteLineEnd;
      end else begin
         codeGen.WriteString('var ');
         codeGen.Indent(False);
         inVar:=True;
      end;
   end;

   procedure AbortVar;
   begin
      if inVar then begin
         codeGen.UnIndent(True);
         inVar:=False;
      end;
   end;

   procedure EndVar;
   begin
      if inVar then begin
         codeGen.WriteStatementEnd;
         codeGen.UnIndent(True);
         inVar:=False;
      end;
   end;

var
   i : Integer;
   blockInit : TBlockExprBase;
   initExpr : TExprBase;
   nextExpr : TExprBase;
   sym : TDataSymbol;
   oldTable : TSymbolTable;
   vpm : Boolean;
begin
   inVar:=False;
   blockInit:=TBlockExprBase(expr);
   for i:=0 to blockInit.SubExprCount-1 do begin
      initExpr:=blockInit.SubExpr[i];
      if initExpr is TBlockExprBase then begin

         EndVar;
         oldTable:=codeGen.LocalTable;
         if initExpr is TBlockExpr then
            codeGen.LocalTable:=TBlockExpr(initExpr).Table;
         try
            Self.CodeGen(codeGen, initExpr);
         finally
            codeGen.LocalTable:=oldTable;
         end;

      end else if     (initExpr.SubExprCount>=1)
                  and (initExpr.SubExpr[0] is TVarExpr)
                  and (   (initExpr.ClassType=TInitDataExpr)
                       or (initExpr is TAssignExpr)) then begin

         sym:=TJSVarExpr.CodeGenSymbol(codeGen, initExpr.SubExpr[0] as TVarExpr);

         if not sym.HasExternalName then begin

            if initExpr is TInitDataExpr then begin

               // skip default initialization of variable if it is followed
               // by an assignment of user default value for the variable
               if i<blockInit.SubExprCount-1 then begin
                  nextExpr:=blockInit.SubExpr[i+1];
                  if     (nextExpr is TAssignConstExpr)
                     and (TAssignConstExpr(nextExpr).Left.SameDataExpr(TInitDataExpr(initExpr).Expr)) then begin
                     continue;
                  end;
               end;

               WriteVar;
               codeGen.WriteSymbolName(sym);
               if sym.Typ<>codeGen.Context.TypVariant then begin
                  codeGen.WriteString(' = ');
                  TdwsJSCodeGen(codeGen).WriteDefaultValue(sym.Typ, IsLocalVarParam(codeGen, sym));
               end;

            end else begin

               (codeGen as TdwsJSCodeGen).FDeclaredLocalVars.Add(sym);
               WriteVar;
               vpm := IsLocalVarParam(codeGen, sym);
               if vpm then begin
                  codeGen.WriteSymbolName(sym);
                  codeGen.WriteString(' = {}');
                  EndVar;
               end;
               if     sym.Typ.IsOfType(codeGen.Context.TypVariant)
                  and (initExpr is TAssignConstToVariantVarExpr)
                  and (TAssignConstToVariantVarExpr(initExpr).Right=Unassigned) then begin

                  if not vpm then
                     codeGen.WriteSymbolName(sym);

               end else begin

                  codeGen.Compile(initExpr as TAssignExpr);
                  AbortVar;

               end;

            end;

         end;

      end else begin

         if initExpr is TAssignExpr then begin
            codeGen.Compile(TAssignExpr(initExpr));
            AbortVar;
         end else begin
            EndVar;
            codeGen.Compile(initExpr);
            codeGen.WriteStatementEnd;
         end;

      end;
   end;
   EndVar;
end;

// ------------------
// ------------------ TJSBlockExpr ------------------
// ------------------

// CodeGen
//
procedure TJSBlockExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   block : TBlockExpr;
begin
   block:=TBlockExpr(expr);
   codeGen.CompileSymbolTable(block.Table);
   inherited;
end;

// ------------------
// ------------------ TJSBlockExprBase ------------------
// ------------------

// CodeGen
//
procedure TJSBlockExprBase.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   block : TBlockExprNoTable;
   sub : TExprBase;
begin
   block:=TBlockExprNoTable(expr);
   for i:=0 to block.SubExprCount-1 do begin
      sub:=block.SubExpr[i];
      codeGen.CompileStatement(sub);
   end;
end;

// ------------------
// ------------------ TJSRAWBlockExpr ------------------
// ------------------

// CodeGen
//
procedure TJSRAWBlockExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TdwsJSBlockExpr;
   i : Integer;
   jsCode, symCode, prefCode : String;
   sym, prefix : TSymbol;
begin
   e:=TdwsJSBlockExpr(expr);
   jsCode:=e.Code;

   for i:=e.SymbolsCount-1 downto 0 do begin
      sym:=e.Symbols[i];
      symCode:=codeGen.SymbolMappedName(sym, cgssGlobal);
      prefix:=e.PrefixSymbols[i];
      if prefix<>nil then begin
         prefCode:=codeGen.SymbolMappedName(prefix, cgssGlobal);
         symCode:=prefCode+'.'+symCode;
      end;
      Insert(symCode, jsCode, e.Offsets[i]);
   end;

   codeGen.WriteStringLn(Trim(jsCode));
end;

// ------------------
// ------------------ TJSNoResultWrapperExpr ------------------
// ------------------

// CodeGen
//
procedure TJSNoResultWrapperExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TNoResultWrapperExpr;
begin
   e:=TNoResultWrapperExpr(expr);
   if e.Expr<>nil then
      codeGen.CompileNoWrap(e.Expr);
   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   sym : TDataSymbol;
begin
   sym:=CodeGenName(codeGen, expr);
   if IsLocalVarParam(codeGen, sym) then
      codeGen.WriteDotBoxFieldName(expr);
end;

// CodeGenName
//
class function TJSVarExpr.CodeGenName(codeGen : TdwsCodeGen; expr : TExprBase) : TDataSymbol;
begin
   Result:=CodeGenSymbol(codeGen, expr);
   codeGen.WriteSymbolName(Result);
end;

// CodeGenSymbol
//
class function TJSVarExpr.CodeGenSymbol(codeGen : TdwsCodeGen; expr : TExprBase) : TDataSymbol;
var
   varExpr : TVarExpr;
begin
   varExpr:=TVarExpr(expr);
   Result:=varExpr.DataSym;
   if Result=nil then
      raise ECodeGenUnsupportedSymbol.CreateFmt('Var not found at StackAddr %d', [varExpr.StackAddr]);
end;

// ------------------
// ------------------ TJSVarParamExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarParamExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   CodeGenName(codeGen, expr);
   codeGen.WriteDotBoxFieldName(expr);
end;

// ------------------
// ------------------ TJSLazyParamExpr ------------------
// ------------------

// CodeGen
//
procedure TJSLazyParamExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   sym : TDataSymbol;
begin
   sym:=TLazyParamExpr(expr).DataSym;
   codeGen.WriteSymbolName(sym);
   if IsLocalVarParam(codeGen, sym) then
      codeGen.WriteDotBoxFieldName(expr);
   codeGen.WriteString('()');
end;

// ------------------
// ------------------ TJSConstParamExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstParamExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   sym : TDataSymbol;
begin
   sym:=CodeGenName(codeGen, expr);
   if (sym.Typ.UnAliasedType is TRecordSymbol) or IsLocalVarParam(codeGen, sym) then
      codeGen.WriteDotBoxFieldName(expr);
end;

// ------------------
// ------------------ TJSAssignConstToIntegerVarExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignConstToIntegerVarExpr.CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToIntegerVarExpr;
begin
   e:=TAssignConstToIntegerVarExpr(expr);
   CodeGenRight.WriteInteger(e.Right);
end;

// ------------------
// ------------------ TJSAssignConstToStringVarExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignConstToStringVarExpr.CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToStringVarExpr;
begin
   e:=TAssignConstToStringVarExpr(expr);
   CodeGenRight.WriteLiteralString(e.Right);
end;

// ------------------
// ------------------ TJSAssignConstToFloatVarExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignConstToFloatVarExpr.CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToFloatVarExpr;
begin
   e:=TAssignConstToFloatVarExpr(expr);
   CodeGenRight.WriteFloat(e.Right, cFormatSettings);
end;

// ------------------
// ------------------ TJSAssignConstToBoolVarExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignConstToBoolVarExpr.CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToBoolVarExpr;
begin
   e:=TAssignConstToBoolVarExpr(expr);
   CodeGenRight.WriteString(cBoolToJSBool[e.Right]);
end;

// ------------------
// ------------------ TJSAssignNilToVarExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignNilToVarExpr.CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase);
begin
   CodeGenRight.WriteString('null');
end;

// ------------------
// ------------------ TJSAssignNilAsResetExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignNilAsResetExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignNilAsResetExpr;
   leftTyp : TClass;
begin
   e:=TAssignNilAsResetExpr(expr);

   leftTyp:=e.Left.Typ.UnAliasedType.ClassType;


   if leftTyp=TDynamicArraySymbol then begin

      codeGen.Compile(e.Left);
      codeGen.WriteString(' = []');

   end else if leftTyp.InheritsFrom(TAssociativeArraySymbol) then begin

      codeGen.Dependencies.Add('$Delete');
      codeGen.WriteString('$Delete(');
      codeGen.Compile(e.Left);
      codeGen.WriteString(')');

   end else Assert(False);

   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSAssignConstDataToVarExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignConstDataToVarExpr.CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstDataToVarExpr;
begin
   e:=TAssignConstDataToVarExpr(expr);
   CodeGenRight.CompileNoWrap(e.Right);
end;

// ------------------
// ------------------ TJAssignArrayConstantExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJAssignArrayConstantExpr.CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignArrayConstantExpr;
begin
   e:=TAssignArrayConstantExpr(expr);
   codeGen.CompileNoWrap(e.Right);
end;

// ------------------
// ------------------ TJSAppendConstStringVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAppendConstStringVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAppendConstStringVarExpr;
begin
   e:=TAppendConstStringVarExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString('+=');
   codeGen.WriteLiteralString(e.AppendString);
   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSCompoundExpr ------------------
// ------------------

// Create
//
constructor TJSCompoundExpr.Create(const anOp, aDynCompound : String);
begin
   inherited Create;
   Op:=anOp;
   DynCompound:=aDynCompound;
end;

// CodeGen
//
procedure TJSCompoundExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TOpAssignExpr;
   eDyn : TDynamicArrayExpr;
begin
   e:=TOpAssignExpr(expr);

   if (cgoNoRangeChecks in codeGen.Options) or not (e.Left is TDynamicArrayExpr) then begin

      codeGen.CompileNoWrap(e.Left);
      codeGen.WriteString(Op);
      codeGen.CompileNoWrap(e.Right);

   end else begin

      eDyn:=TDynamicArrayExpr(e.Left);

      codeGen.Dependencies.Add(DynCompound);

      codeGen.WriteString(DynCompound);
      codeGen.WriteString('(');
      codeGen.Compile(eDyn.BaseExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(eDyn.IndexExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Right);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');

   end;

   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSConstExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstExpr;
   locData : IDataContext;
begin
   e:=TConstExpr(expr);
   codeGen.CreateDataContext(e.Data, 0, locData);
   TdwsJSCodeGen(codeGen).WriteValue(e.Typ, locData);
end;

// ------------------
// ------------------ TJSConstStringExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstStringExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstStringExpr;
begin
   e:=TConstStringExpr(expr);
   codeGen.WriteLiteralString(e.Value);
end;

// ------------------
// ------------------ TJSConstNumExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstNumExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstExpr;
begin
   e:=TConstExpr(expr);
   if e.EvalAsFloat(nil)<0 then begin
      codeGen.WriteString('(');
      CodeGenNoWrap(codeGen, e);
      codeGen.WriteString(')');
   end else CodeGenNoWrap(codeGen, e);
end;

// ------------------
// ------------------ TJSConstIntExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSConstIntExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TConstIntExpr;
begin
   e:=TConstIntExpr(expr);
   codeGen.WriteInteger(e.Value);
end;

// ------------------
// ------------------ TJSConstFloatExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSConstFloatExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TConstFloatExpr;
begin
   e:=TConstFloatExpr(expr);
   codeGen.WriteFloat(e.Value, cFormatSettings);
end;

// ------------------
// ------------------ TJSConstBooleanExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstBooleanExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstBooleanExpr;
begin
   e:=TConstBooleanExpr(expr);
   codeGen.WriteString(cBoolToJSBool[e.Value]);
end;

// ------------------
// ------------------ TJSConstArrayExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstArrayExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   codeGen.WriteSymbolName(TConstArrayExpr(expr).Symbol);
end;

// ------------------
// ------------------ TJSArrayConstantExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayConstantExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayConstantExpr;
begin
   e:=TArrayConstantExpr(expr);
   codeGen.WriteString('[');
   CodeGenElements(codeGen, e);
   codeGen.WriteString(']');
end;

// CodeGenElements
//
class procedure TJSArrayConstantExpr.CodeGenElements(codeGen : TdwsCodeGen; e : TArrayConstantExpr);
var
   i : Integer;
begin
   for i:=0 to e.ElementCount-1 do begin
      if i>0 then
         codeGen.WriteString(', ');
      codeGen.CompileNoWrap(e.Elements[i]);
   end;
end;

// ------------------
// ------------------ TJSResourceStringExpr ------------------
// ------------------

// CodeGen
//
procedure TJSResourceStringExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TResourceStringExpr;
begin
   e:=TResourceStringExpr(expr);

   codeGen.Dependencies.Add('$R');
   codeGen.WriteString('$R[');
   codeGen.WriteInteger(e.ResSymbol.Index);
   codeGen.WriteString(']');
end;

// ------------------
// ------------------ TJSAssignExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignExpr;
begin
   e:=TAssignExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString(' = ');
   CodeGenRight(codeGen, expr);
   codeGen.WriteStatementEnd;
end;

// CodeGenRight
//
procedure TJSAssignExpr.CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignExpr;
begin
   e:=TAssignExpr(expr);
   codeGen.CompileNoWrap(e.Right);
end;

// ------------------
// ------------------ TJSAssignDataExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignDataExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignDataExpr;
   leftTyp : TTypeSymbol;
begin
   e:=TAssignDataExpr(expr);

   leftTyp:=e.Left.Typ.UnAliasedType;
   if     (leftTyp.ClassType=TRecordSymbol)
      and (   (e.Left is TVarParamExpr)
           or not (e.Right is TFuncExprBase))
      and not TRecordSymbol(leftTyp).IsImmutable then begin

      codeGen.WriteString('Copy$');
      codeGen.WriteSymbolName(leftTyp);
      codeGen.WriteString('(');
      codeGen.Compile(e.Right);
      codeGen.WriteString(',');
      codeGen.Compile(e.Left);
      codeGen.WriteString(')');
      codeGen.WriteStatementEnd;

   end else inherited CodeGen(codeGen, expr);
end;

// CodeGenRight
//
procedure TJSAssignDataExpr.CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignDataExpr;
begin
   e:=TAssignDataExpr(expr);

   codeGen.CompileValue(e.Right);
end;

// ------------------
// ------------------ TJSAssignClassOfExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignClassOfExpr.CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignClassOfExpr;
begin
   // TODO: deep copy of records & static arrays
   e:=TAssignClassOfExpr(expr);
   codeGen.CompileNoWrap(e.Right);
   if e.Right.Typ is TClassSymbol then
      codeGen.WriteStringLn('.ClassType');
end;

// ------------------
// ------------------ TJSAssignFuncExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignFuncExpr.CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignFuncExpr;
   funcExpr : TFuncExprBase;
begin
   e:=TAssignFuncExpr(expr);

   funcExpr:=(e.Right as TFuncExprBase);

   TJSFuncRefExpr.DoCodeGen(codeGen, funcExpr);
end;

// ------------------
// ------------------ TJSFuncBaseExpr ------------------
// ------------------

// CodeGen
//
procedure TJSFuncBaseExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TFuncExprBase;
   i : Integer;
   funcSym : TFuncSymbol;
   paramExpr : TTypedExpr;
   paramSymbol : TParamSymbol;
   name : String;
begin
   // TODO: handle deep copy of records, lazy params
   e:=TFuncExprBase(expr);
   funcSym:=e.FuncSym;

   if funcSym.ClassType=TAliasMethodSymbol then
      funcSym:=TAliasMethodSymbol(funcSym).Alias;

   if funcSym.ClassType=TTypeOfSymbol then begin
      CodeGenTypeOf(codeGen, e, True);
      Exit;
   end;

   if     (funcSym.Executable<>nil)
      and (funcSym.Executable.GetSelf is TInternalFunction) then begin

      if funcSym.IsOverloaded then
         name:=GetSignature(funcSym)
      else name:=funcSym.QualifiedName;
      codeGen.Dependencies.Add(name);
      codeGen.WriteString(name);

   end else begin

      CodeGenFunctionName(codeGen, e, funcSym);

   end;

   if not funcSym.IsProperty then begin

      codeGen.WriteString('(');
      CodeGenBeginParams(codeGen, e);
      for i:=0 to e.Args.Count-1 do begin
         if i>0 then
            codeGen.WriteString(',');
         paramExpr:=e.Args.ExprBase[i] as TTypedExpr;
         paramSymbol:=funcSym.Params[i] as TParamSymbol;
         if ShouldBoxParam(paramSymbol) then begin
            if paramExpr is TVarExpr then
               TJSVarExpr.CodeGenName(codeGen, TVarExpr(paramExpr))
            else begin
               codeGen.WriteString('{'+TdwsJSCodeGen.cBoxFieldName+':');
               codeGen.Compile(paramExpr);
               codeGen.WriteString('}');
            end;
         end else if paramSymbol is TLazyParamSymbol then begin
            codeGen.WriteString('function () { return ');
            codeGen.Compile(paramExpr);
            codeGen.WriteString('}');
         end else if paramSymbol is TByRefParamSymbol then begin
            codeGen.Compile(paramExpr);
         end else begin
            codeGen.CompileValue(paramExpr);
         end;
      end;
      codeGen.WriteString(')');

   end;
end;

// CodeGenNoWrap
//
procedure TJSFuncBaseExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   if TFuncExprBase(expr).FuncSym.ClassType=TTypeOfSymbol then
      CodeGenTypeOf(codeGen, TFuncExprBase(expr), False)
   else Self.CodeGen(codeGen, expr);
end;

// CodeGenFunctionName
//
procedure TJSFuncBaseExpr.CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol);
var
   meth : TMethodSymbol;
begin
   if funcSym is TMethodSymbol then begin
      meth:=TMethodSymbol(funcSym);
      if meth.IsStatic and (meth.StructSymbol is TClassSymbol) then begin
         codeGen.WriteSymbolName(meth.StructSymbol);
         codeGen.WriteString('.');
      end;
      codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(funcSym, meth.StructSymbol))
   end else codeGen.WriteSymbolName(funcSym);
   if FVirtualCall then
      codeGen.WriteString(TdwsJSCodeGen.cVirtualPostfix);
end;

// CodeGenBeginParams
//
procedure TJSFuncBaseExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
begin
   // nothing here
end;

// CodeGenTypeOf
//
procedure TJSFuncBaseExpr.CodeGenTypeOf(codeGen : TdwsCodeGen; expr : TFuncExprBase; wrap : Boolean);
begin
   if wrap then
      codeGen.WriteString('(typeof ')
   else codeGen.WriteString('typeof ');
   codeGen.Compile(expr.Args[0]);
   if wrap then
      codeGen.WriteString(')');
end;

// GetSignature
//
class function TJSFuncBaseExpr.GetSignature(funcSym : TFuncSymbol) : String;
var
   i : Integer;
begin
   Result:=funcSym.QualifiedName+'$_';
   for i:=0 to funcSym.Params.Count-1 do
      Result:=Result+funcSym.GetParamType(i).Name+'_';
end;

// ------------------
// ------------------ TJSRecordMethodExpr ------------------
// ------------------

// CodeGen
//
procedure TJSRecordMethodExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TRecordMethodExpr;
   methSym : TMethodSymbol;
begin
   e:=TRecordMethodExpr(expr);

   methSym:=(e.FuncSym as TMethodSymbol);
   if not methSym.IsClassMethod then begin
      codeGen.WriteSymbolName(methSym.StructSymbol);
      codeGen.WriteString('$');
   end;

   inherited;
end;

// ------------------
// ------------------ TJSHelperMethodExpr ------------------
// ------------------

// CodeGen
//
procedure TJSHelperMethodExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : THelperMethodExpr;
   funcSym : TFuncSymbol;
   methSym : TMethodSymbol;
begin
   e:=THelperMethodExpr(expr);

   funcSym:=e.FuncSym;
   if funcSym is TMethodSymbol then begin
      methSym:=TMethodSymbol(funcSym);
      if not methSym.IsClassMethod then begin
         codeGen.WriteSymbolName(methSym.StructSymbol);
         codeGen.WriteString('$');
      end;
   end;

   inherited;
end;

// ------------------
// ------------------ TJSMethodStaticExpr ------------------
// ------------------

// CodeGen
//
procedure TJSMethodStaticExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMethodStaticExpr;
begin
   e:=TMethodStaticExpr(expr);

   if e.MethSym.StructSymbol.IsExternalRooted then begin
      codeGen.Compile(e.BaseExpr);
   end else begin
      codeGen.Dependencies.Add('TObject');
      codeGen.WriteSymbolName((e.FuncSym as TMethodSymbol).StructSymbol);
   end;

   if e.MethSym.IsExternal and (e.MethSym.ExternalName=SYS_EXTERNAL_ARRAY) then
      CodeGenExternalArray(codeGen, e)
   else begin
      codeGen.WriteString('.');
      inherited;
   end;
end;

// CodeGenBeginParams
//
procedure TJSMethodStaticExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TMethodStaticExpr;
begin
   e:=TMethodStaticExpr(expr);
   if not e.MethSym.StructSymbol.IsExternalRooted then begin
      codeGen.Compile(e.BaseExpr);
      if e.FuncSym.Params.Count>0 then
         codeGen.WriteString(',');
   end;
end;

// CodeGenExternalArray
//
procedure TJSMethodStaticExpr.CodeGenExternalArray(codeGen : TdwsCodeGen; expr : TMethodExpr);
var
   i : Integer;
begin
   codeGen.WriteString('[');

   for i:=0 to expr.Args.Count-2 do begin
      if i>0 then
         codeGen.WriteString(',');
      codeGen.Compile(expr.Args[i]);
   end;

   if expr.MethSym.Typ=nil then begin
      // setter array
      codeGen.WriteString(']=');
      codeGen.Compile(expr.Args[expr.Args.Count-1]);
   end else begin
      // getter array
      codeGen.Compile(expr.Args[expr.Args.Count-1]);
      codeGen.WriteString(']');
   end;
end;

// ------------------
// ------------------ TJSMethodVirtualExpr ------------------
// ------------------

// CodeGen
//
procedure TJSMethodVirtualExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMethodVirtualExpr;
begin
   codeGen.Dependencies.Add('TObject');

   e:=TMethodVirtualExpr(expr);
   FVirtualCall:=e.MethSym.IsVirtual;
   codeGen.WriteSymbolName(e.MethSym.RootParentMeth.StructSymbol);
   codeGen.WriteString('.');
   inherited;
end;

// CodeGenBeginParams
//
procedure TJSMethodVirtualExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TMethodVirtualExpr;
begin
   e:=TMethodVirtualExpr(expr);

   if cgoNoCheckInstantiated in codeGen.Options then begin
      codeGen.Compile(e.BaseExpr);
   end else begin
      codeGen.Dependencies.Add('$Check');
      codeGen.WriteString('$Check(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');
   end;

   if e.FuncSym.Params.Count>0 then
      codeGen.WriteString(',');
end;

// ------------------
// ------------------ TJSMethodInterfaceExpr ------------------
// ------------------

// CodeGenFunctionName
//
procedure TJSMethodInterfaceExpr.CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol);
var
   e : TMethodInterfaceExpr;
begin
   e:=TMethodInterfaceExpr(expr);

   if cgoNoCheckInstantiated in codeGen.Options then begin
      codeGen.Compile(e.BaseExpr);
   end else begin
      codeGen.Dependencies.Add('$CheckIntf');
      codeGen.WriteString('$CheckIntf(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');
   end;

   codeGen.WriteString('[');
   codeGen.WriteInteger(e.MethSym.VMTIndex);
   codeGen.WriteString(']');
end;

// ------------------
// ------------------ TJSMethodInterfaceAnonymousExpr ------------------
// ------------------

// CodeGenFunctionName
//
//procedure TJSMethodInterfaceAnonymousExpr.CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol);
//var
//   e : TMethodInterfaceAnonymousExpr;
//begin
//   e:=TMethodInterfaceAnonymousExpr(expr);
//
//   codeGen.WriteSymbolName(e.FuncSym);
//end;

// CodeGenBeginParams
//
procedure TJSMethodInterfaceAnonymousExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TMethodInterfaceAnonymousExpr;
begin
   e:=TMethodInterfaceAnonymousExpr(expr);

   codeGen.Compile(e.BaseExpr);

   if e.FuncSym.Params.Count>0 then
      codeGen.WriteString(',');
end;

// ------------------
// ------------------ TJSClassMethodStaticExpr ------------------
// ------------------

// CodeGen
//
procedure TJSClassMethodStaticExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TClassMethodStaticExpr;
   meth : TMethodSymbol;
begin
   e:=TClassMethodStaticExpr(expr);

   meth:=(e.FuncSym as TMethodSymbol);

   if not meth.StructSymbol.IsExternal then
      codeGen.Dependencies.Add('TObject');

   codeGen.WriteSymbolName(meth.StructSymbol);
   codeGen.WriteString('.');
   inherited;
end;

// CodeGenBeginParams
//
procedure TJSClassMethodStaticExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TClassMethodStaticExpr;
   baseType : TStructuredTypeSymbol;
begin
   e:=TClassMethodStaticExpr(expr);

   if e.BaseExpr.Typ is TStructuredTypeMetaSymbol then
      baseType:=TStructuredTypeMetaSymbol(e.BaseExpr.Typ).StructSymbol
   else if e.BaseExpr.Typ is TStructuredTypeSymbol then
      baseType:=TStructuredTypeSymbol(e.BaseExpr.Typ)
   else baseType:=nil;
   if (baseType<>nil) and baseType.IsExternal then Exit;

   if (cgoNoCheckInstantiated in codeGen.Options) or (e.BaseExpr is TConstExpr) then begin
      codeGen.Compile(e.BaseExpr);
   end else begin
      codeGen.Dependencies.Add('$Check');
      codeGen.WriteString('$Check(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');
   end;
   if e.BaseExpr.Typ is TClassSymbol then
      codeGen.WriteString('.ClassType');

   if e.FuncSym.Params.Count>0 then
      codeGen.WriteString(',');
end;

// ------------------
// ------------------ TJSClassMethodVirtualExpr ------------------
// ------------------

// CodeGen
//
procedure TJSClassMethodVirtualExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TClassMethodVirtualExpr;
begin
   codeGen.Dependencies.Add('TObject');

   e:=TClassMethodVirtualExpr(expr);
   FVirtualCall:=e.MethSym.IsVirtual;
   codeGen.WriteSymbolName(e.MethSym.RootParentMeth.StructSymbol);
   codeGen.WriteString('.');
   inherited;
end;

// CodeGenBeginParams
//
procedure TJSClassMethodVirtualExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TClassMethodVirtualExpr;
begin
   e:=TClassMethodVirtualExpr(expr);

   if cgoNoCheckInstantiated in codeGen.Options then begin
      codeGen.Compile(e.BaseExpr);
   end else begin
      codeGen.Dependencies.Add('$Check');
      codeGen.WriteString('$Check(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');
   end;
   if e.BaseExpr.Typ is TClassSymbol then
      codeGen.WriteString('.ClassType');

   if e.FuncSym.Params.Count>0 then
      codeGen.WriteString(',');
end;

// ------------------
// ------------------ TJSConstructorStaticExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstructorStaticExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstructorStaticExpr;
   structSymbol : TCompositeTypeSymbol;
   newClass : TClassSymbol;
begin
   e:=TConstructorStaticExpr(expr);

   structSymbol:=(e.FuncSym as TMethodSymbol).StructSymbol;

   if structSymbol.IsExternalRooted then begin

      if not e.BaseExpr.IsConstant then begin

         codeGen.WriteString('new ');
         codeGen.Compile(e.BaseExpr);

      end else begin

         newClass := (e.BaseExpr.Typ.Typ as TClassSymbol);

         if (newClass.Parent<>nil) and (not newClass.Parent.IsExternal) then begin

            codeGen.WriteString('{}');
            Exit;

         end else begin

            codeGen.WriteString('new ');
            codeGen.WriteString(newClass.ExternalName);

         end;

      end;

   end else begin

      codeGen.Dependencies.Add('TObject');
      if structSymbol=codeGen.Context.TypException then
         codeGen.Dependencies.Add('Exception');

      codeGen.WriteSymbolName(structSymbol);
      codeGen.WriteString('.');

   end;

   inherited;
end;

// CodeGenFunctionName
//
procedure TJSConstructorStaticExpr.CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol);
var
   e : TConstructorStaticExpr;
begin
   e:=TConstructorStaticExpr(expr);
   if not (e.FuncSym as TMethodSymbol).StructSymbol.IsExternalRooted then
      inherited;
end;

// CodeGenBeginParams
//
procedure TJSConstructorStaticExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TConstructorStaticExpr;
begin
   e:=TConstructorStaticExpr(expr);

   if not (e.FuncSym as TMethodSymbol).StructSymbol.IsExternalRooted then begin

      if e.BaseExpr is TConstExpr then begin
         codeGen.WriteString('$New(');
         codeGen.Compile(e.BaseExpr);
      end else begin
         codeGen.Dependencies.Add('$NewDyn');
         codeGen.WriteString('$NewDyn(');
         codeGen.Compile(e.BaseExpr);
         codeGen.WriteString(',');
         WriteLocationString(codeGen, expr);
      end;
      codeGen.WriteString(')');
      if e.FuncSym.Params.Count>0 then
         codeGen.WriteString(',');

   end;
end;

// ------------------
// ------------------ TJSConstructorVirtualExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstructorVirtualExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstructorVirtualExpr;
begin
   codeGen.Dependencies.Add('TObject');

   e:=TConstructorVirtualExpr(expr);
   FVirtualCall:=e.MethSym.IsVirtual;
   codeGen.WriteSymbolName(e.MethSym.RootParentMeth.StructSymbol);
   codeGen.WriteString('.');
   inherited;
end;

// CodeGenBeginParams
//
procedure TJSConstructorVirtualExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TConstructorVirtualExpr;
begin
   e:=TConstructorVirtualExpr(expr);
   if e.BaseExpr is TConstExpr then begin
      codeGen.WriteString('$New(');
   end else begin
      codeGen.Dependencies.Add('$NewDyn');
      codeGen.WriteString('$NewDyn(');
   end;
   codeGen.Compile(e.BaseExpr);
   if e.BaseExpr.Typ is TClassSymbol then
      codeGen.WriteString('.ClassType');
   if not (e.BaseExpr is TConstExpr) then begin
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
   end;
   codeGen.WriteString(')');
   if e.FuncSym.Params.Count>0 then
      codeGen.WriteString(',');
end;

// ------------------
// ------------------ TJSConnectorCallExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConnectorCallExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConnectorCallExpr;
   jsCall : TdwsJSConnectorCall;
   isWrite : Boolean;
   i, n : Integer;
begin
   e:=TConnectorCallExpr(Expr);
   jsCall:=(e.ConnectorCall.GetSelf as TdwsJSConnectorCall);

   n:=e.SubExprCount-1;
   isWrite:=False;

   codeGen.Compile(e.BaseExpr);
   if e.IsIndex then begin
      if jsCall.CallMethodName<>'' then begin
         codeGen.WriteString('.');
         codeGen.WriteString(jsCall.CallMethodName);
      end;
      codeGen.WriteString('[');
      isWrite:=(jsCall as TdwsJSIndexCall).IsWrite;
      if isWrite then
         Dec(n);
   end else begin
      codeGen.WriteString('.');
      codeGen.WriteString(jsCall.CallMethodName);
      codeGen.WriteString('(');
   end;
   for i:=1 to n do begin
      if i>1 then
         codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.SubExpr[i] as TTypedExpr);
   end;
   if e.IsIndex then begin
      codeGen.WriteString(']');
      if isWrite then begin
         codeGen.WriteString(' = ');
         codeGen.CompileNoWrap(e.SubExpr[n+1] as TTypedExpr);
      end;
   end else codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSConnectorReadExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConnectorReadExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConnectorReadExpr;
   jsMember : TdwsJSConnectorMember;
begin
   e:=TConnectorReadExpr(Expr);
   jsMember:=(e.ConnectorMember.GetSelf as TdwsJSConnectorMember);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('.');
   codeGen.WriteString(jsMember.MemberName);
end;

// ------------------
// ------------------ TJSConnectorWriteExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConnectorWriteExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConnectorWriteExpr;
   jsMember : TdwsJSConnectorMember;
   valueTyp : TTypeSymbol;
begin
   e:=TConnectorWriteExpr(Expr);
   jsMember:=(e.ConnectorMember.GetSelf as TdwsJSConnectorMember);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('.');
   codeGen.WriteString(jsMember.MemberName);
   codeGen.WriteString(' = ');
   valueTyp:=e.ValueExpr.Typ;
   if valueTyp is TRecordSymbol then begin
      if cvPublished in TRecordSymbol(valueTyp).MembersVisibilities then begin
         codeGen.WriteString('Pub$');
         codeGen.WriteSymbolName(valueTyp);
         codeGen.WriteString('(');
         codeGen.Compile(e.ValueExpr);
         codeGen.WriteString(')');
      end else begin
         codeGen.WriteString('{}');
      end;
   end else if valueTyp is TStaticArraySymbol then begin
      codeGen.Compile(e.ValueExpr);
      if not e.ValueExpr.IsConstant then
         codeGen.WriteString('.slice()');
   end else codeGen.Compile(e.ValueExpr);
end;

// ------------------
// ------------------ TJSConnectorForInExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConnectorForInExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConnectorForInExpr;
begin
   e:=TConnectorForInExpr(expr);

   codeGen.WriteString('for (');
   codeGen.Compile(e.LoopVarExpr);
   codeGen.WriteString(' in ');
   codeGen.Compile(e.InExpr);
   codeGen.WriteBlockBegin(') ');
   codeGen.Compile(e.DoExpr);
   codeGen.WriteBlockEndLn;
end;

// ------------------
// ------------------ TJSConnectorCastExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConnectorCastExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConnectorCastExpr;
begin
   e:=TConnectorCastExpr(expr);

   codeGen.Compile(e.Expr);
end;

// ------------------
// ------------------ TJSFuncPtrExpr ------------------
// ------------------

// CodeGen
//
procedure TJSFuncPtrExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   inherited;
end;

// CodeGenFunctionName
//
procedure TJSFuncPtrExpr.CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol);
var
   e : TFuncPtrExpr;
begin
   e:=TFuncPtrExpr(expr);

   if cgoNoCheckInstantiated in codeGen.Options then begin
      codeGen.Compile(e.CodeExpr);
   end else begin
      codeGen.Dependencies.Add('$CheckFunc');
      codeGen.WriteString('$CheckFunc(');
      codeGen.Compile(e.CodeExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSFuncRefExpr ------------------
// ------------------

// CodeGen
//
procedure TJSFuncRefExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TFuncRefExpr;
begin
   e:=TFuncRefExpr(expr);
   if e.FuncExpr is TFuncPtrExpr then
      codeGen.Compile(TFuncPtrExpr(e.FuncExpr).CodeExpr)
   else DoCodeGen(codeGen, e.FuncExpr);
end;

// DoCodeGen
//
class procedure TJSFuncRefExpr.DoCodeGen(codeGen : TdwsCodeGen; funcExpr : TFuncExprBase);
var
   methExpr : TMethodExpr;
   funcSym : TFuncSymbol;
   methSym : TMethodSymbol;
   eventFunc : String;
begin
   if funcExpr is TMethodExpr then begin

      methExpr:=TMethodExpr(funcExpr);
      methSym:=TMethodSymbol(methExpr.funcSym);

      case methSym.Params.Count of
         0 : eventFunc:='$Event0';
         1 : eventFunc:='$Event1';
         2 : eventFunc:='$Event2';
         3 : eventFunc:='$Event3';
      else
         eventFunc:='$Event';
      end;
      codeGen.Dependencies.Add(eventFunc);
      codeGen.WriteString(eventFunc);
      codeGen.WriteString('(');

      codeGen.Compile(methExpr.BaseExpr);
      if methExpr is TMethodVirtualExpr then begin
         codeGen.WriteString(',');
         codeGen.WriteSymbolName(methExpr.MethSym.RootParentMeth.StructSymbol);
         codeGen.WriteString('.');

         codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(methSym, methSym.StructSymbol));
         if methSym.IsVirtual then
            codeGen.WriteString(TdwsJSCodeGen.cVirtualPostfix);
      end else if methExpr is TMethodInterfaceExpr then begin
         codeGen.WriteString('.O,');
         codeGen.Compile(methExpr.BaseExpr);
         codeGen.WriteString('[');
         codeGen.WriteInteger(methSym.VMTIndex);
         codeGen.WriteString(']');
      end else if methExpr is TMethodStaticExpr then begin
         if methSym.IsClassMethod and (methExpr.BaseExpr.Typ.UnAliasedType is TClassSymbol) then
            codeGen.WriteString('.ClassType');
         codeGen.WriteString(',');
         codeGen.WriteSymbolName(methSym.StructSymbol);
         codeGen.WriteString('.');
         codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(methSym, methSym.StructSymbol))
      end else begin
         raise ECodeGenUnknownExpression.CreateFmt('Unsupported AssignFuncExpr for %s', [methExpr.ClassName]);
      end;
      codeGen.WriteString(')');

   end else begin

      funcSym:=funcExpr.FuncSym;

      if funcSym is TMethodSymbol then begin
         methSym:=TMethodSymbol(funcSym);
         if not (methSym.StructSymbol is TRecordSymbol) then begin
            codeGen.WriteSymbolName(methSym.StructSymbol);
            codeGen.WriteString('.');
         end;
      end;

      codeGen.WriteSymbolName(funcSym);

      if funcExpr is TMagicFuncExpr then
         codeGen.Dependencies.Add(funcSym.QualifiedName);

   end;
end;

// ------------------
// ------------------ TJSAnonymousFuncRefExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAnonymousFuncRefExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAnonymousFuncRefExpr;
begin
   e:=TAnonymousFuncRefExpr(expr);
   codeGen.CompileFuncSymbol(e.FuncExpr.FuncSym as TSourceFuncSymbol);
end;

// ------------------
// ------------------ TJSAssociativeArrayGetExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssociativeArrayGetExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssociativeArrayGetExpr;
begin
   e:=TAssociativeArrayGetExpr(expr);

   Assert(e.KeyExpr.Typ.UnAliasedTypeIs(TBaseStringSymbol), 'String key required');
   Assert(e.Typ.UnAliasedTypeIs(TBaseSymbol), 'Base type value required');

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('[');
   codeGen.CompileNoWrap(e.KeyExpr);
   codeGen.WriteString(']');
end;

// ------------------
// ------------------ TJSAssociativeArraySetExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssociativeArraySetExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssociativeArraySetExpr;
begin
   e:=TAssociativeArraySetExpr(expr);

   Assert(e.KeyExpr.Typ.UnAliasedTypeIs(TBaseStringSymbol), 'String key required');
   Assert(e.ValueExpr.Typ.UnAliasedTypeIs(TBaseSymbol), 'Base type value required');

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('[');
   codeGen.CompileNoWrap(e.KeyExpr);
   codeGen.WriteString(']=');
   codeGen.CompileNoWrap(e.ValueExpr);
   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSInOpExpr ------------------
// ------------------

// CodeGen
//
procedure TJSInOpExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TInOpExpr;
   i : Integer;
   cond : TCaseCondition;
   wrapped, allConstantCompares : Boolean;
   writeOperand : TProc;
begin
   e:=TInOpExpr(expr);

   if e.Count=0 then begin
      codeGen.WriteString(cBoolToJSBool[false]);
      Exit;
   end;

   wrapped:=(e.Count>1) and not ((e.Left is TVarExpr) or (e.Left is TConstExpr) or (e.Left is TFieldExpr));

   if wrapped then begin

      allConstantCompares:=True;
      for i:=0 to e.Count-1 do begin
         if not ((e[i] is TCompareCaseCondition) and e[i].IsConstant) then begin
            allConstantCompares:=False;
            Break;
         end;
      end;

      if allConstantCompares then begin
         codeGen.WriteString('([');
         for i:=0 to e.Count-1 do begin
            if i>0 then
               codeGen.WriteString(',');
            codegen.Compile(TCompareCaseCondition(e[i]).CompareExpr);
         end;
         codeGen.WriteString('].indexOf(');
         codegen.CompileNoWrap(e.Left);
         codeGen.WriteString(')>=0)');
         Exit;
      end;

      codeGen.WriteString('function(v$){return ');
      writeOperand:=procedure begin codegen.WriteString('v$') end;
   end else begin
      writeOperand:=procedure begin codegen.Compile(e.Left) end;
   end;

   if e.Count>1 then
      codeGen.WriteString('(');

   for i:=0 to e.Count-1 do begin
      if i>0 then
         codeGen.WriteString('||');
      cond:=e[i];
      if (cond is TCompareCaseCondition) and (cond.IsConstant) then
         TJSCaseExpr.CodeGenCondition(codeGen, cond, writeOperand)
      else begin
         codeGen.WriteString('(');
         TJSCaseExpr.CodeGenCondition(codeGen, cond, writeOperand);
         codeGen.WriteString(')');
      end;
   end;

   if e.Count>1 then
      codeGen.WriteString(')');

   if wrapped then begin
      codeGen.WriteString('}(');
      codeGen.CompileNoWrap(e.Left);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSBitwiseInOpExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSBitwiseInOpExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TBitwiseInOpExpr;
begin
   e:=TBitwiseInOpExpr(expr);

   // JavaScript << has a higher precedence than &, which is lower than !=
   codeGen.WriteString('(1<<');
   codeGen.Compile(e.Expr);
   codeGen.WriteString('&');
   codeGen.WriteInteger(e.Mask);
   codeGen.WriteString(')!=0');
end;

// ------------------
// ------------------ TJSIfExpr ------------------
// ------------------

// CodeGenCondition
//
procedure TJSIfExpr.CodeGenCondition(codeGen : TdwsCodeGen; condExpr : TTypedExpr);
begin
   codeGen.WriteString('(');
   if condExpr is TAssignedExpr then
      codeGen.CompileNoWrap(TAssignedExpr(condExpr).Expr)
   else if (condExpr is TNotBoolExpr) and (TNotBoolExpr(condExpr).Expr is TAssignedExpr) then begin
      codeGen.WriteString('!');
      codeGen.CompileNoWrap(TNotBoolExpr(TAssignedExpr(condExpr).Expr).Expr);
   end else if condExpr is TRelIntIsNotZeroExpr then
      codeGen.CompileNoWrap(TRelIntIsNotZeroExpr(condExpr).Expr)
   else if condExpr is TRelIntIsZeroExpr then begin
      codeGen.WriteString('!');
      codeGen.Compile(TRelIntIsZeroExpr(condExpr).Expr)
   end else codeGen.CompileNoWrap(condExpr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSIfThenElseValueExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSIfThenElseValueExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TIfThenElseValueExpr;
begin
   e:=TIfThenElseValueExpr(expr);

   CodeGenCondition(codeGen, e.CondExpr);
   CodeGen.WriteString('?');
   CodeGen.CompileNoWrap(e.TrueExpr);
   CodeGen.WriteString(':');
   CodeGen.CompileNoWrap(e.FalseExpr);
end;

// ------------------
// ------------------ TJSIfThenExpr ------------------
// ------------------

// SubExprIsSafeStatement
//
function TJSIfThenExpr.SubExprIsSafeStatement(sub : TExprBase) : Boolean;
begin
   Result:=   (sub is TFuncExprBase)
           or (sub is TNoResultWrapperExpr)
           or (sub is TAssignExpr)
           or (sub is TFlowControlExpr);
end;

// CodeGen
//
procedure TJSIfThenExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TIfThenExpr;
begin
   e:=TIfThenExpr(expr);

   codeGen.WriteString('if ');
   CodeGenCondition(codeGen, e.CondExpr);
   codeGen.WriteString(' ');

   if (cgoOptimizeForSize in codeGen.Options) and SubExprIsSafeStatement(e.ThenExpr) then begin
      codeGen.CompileStatement(e.ThenExpr);
   end else begin
      codeGen.WriteBlockBegin('');
      codeGen.CompileStatement(e.ThenExpr);
      codeGen.WriteBlockEndLn;
   end;
end;

// ------------------
// ------------------ TJSIfThenElseExpr ------------------
// ------------------

// CodeGen
//
procedure TJSIfThenElseExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TIfThenElseExpr;
begin
   e:=TIfThenElseExpr(expr);

   codeGen.WriteString('if ');
   CodeGenCondition(codeGen, e.CondExpr);
   codeGen.WriteBlockBegin(' ');

   codeGen.CompileStatement(e.ThenExpr);
   codeGen.WriteBlockEnd;
   codeGen.WriteString(' else ');

   if    (e.ElseExpr is TIfThenExpr)
      or (   (cgoOptimizeForSize in codeGen.Options)
          and SubExprIsSafeStatement(e.ElseExpr)) then begin
      codeGen.CompileStatement(e.ElseExpr);
   end else begin
      codeGen.WriteBlockBegin('');
      codeGen.CompileStatement(e.ElseExpr);
      codeGen.WriteBlockEndLn;
   end;
end;

// ------------------
// ------------------ TJSCaseExpr ------------------
// ------------------

// CodeGen
//
procedure TJSCaseExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i, j : Integer;
   e : TCaseExpr;
   cond : TCaseCondition;
   compCond, compCondOther : TCompareCaseCondition;
   mark : array of Boolean;
   tmp : String;
   valType : TTypeSymbol;
   switchable : Boolean;
   checkBreak : IRecursiveHasSubExprClass;
begin
   e:=TCaseExpr(expr);

   valType:=e.ValueExpr.Typ.UnAliasedType;
   switchable:=   (valType is TBaseBooleanSymbol)
               or (valType is TBaseIntegerSymbol)
               or (valType is TBaseStringSymbol)
               or (valType is TEnumerationSymbol);

   if switchable then begin
      checkBreak:=TRecursiveHasSubExprClass.Create(TBreakExpr);
      if     (e.ElseExpr<>nil)
         and (   (e.ElseExpr is TBreakExpr)
              or checkBreak.Check(e.ElseExpr)) then begin
         switchable:=False
      end else begin
         for i:=0 to e.CaseConditions.Count-1 do begin
            cond:=TCaseCondition(e.CaseConditions.List[i]);
            switchable:=    (cond is TCompareCaseCondition)
                        and (TCompareCaseCondition(cond).CompareExpr is TConstExpr);
            if not switchable then break;
            if (cond.TrueExpr is TBreakExpr) or checkBreak.Check(cond.TrueExpr) then begin
               switchable:=False;
               break;
            end;
         end;
      end;
   end;

   if switchable then begin

      SetLength(mark, e.CaseConditions.Count);
      codeGen.WriteString('switch (');
      codeGen.Compile(e.ValueExpr);
      codeGen.WriteBlockBegin(') ');
      for i:=0 to e.CaseConditions.Count-1 do begin
         if mark[i] then continue;
         compCond:=TCompareCaseCondition(e.CaseConditions.List[i]);
         for j:=i to e.CaseConditions.Count-1 do begin
            compCondOther:=TCompareCaseCondition(e.CaseConditions.List[j]);
            if compCond.TrueExpr=compCondOther.TrueExpr then begin
               codeGen.WriteString('case ');
               codeGen.Compile(compCondOther.CompareExpr);
               codeGen.WriteStringLn(' :');
               mark[j]:=True;
            end;
         end;
         codeGen.Indent;
         codeGen.CompileStatement(compCond.TrueExpr);
         codeGen.WriteStringLn('break;');
         codeGen.UnIndent;
      end;
      if e.ElseExpr<>nil then begin
         codeGen.WriteStringLn('default :');
         codeGen.Indent;
         codeGen.Compile(e.ElseExpr);
         codeGen.UnIndent;
      end;
      codeGen.WriteBlockEndLn;

   end else begin

      tmp:=codeGen.GetNewTempSymbol;
      codeGen.WriteString('{var ');
      codeGen.WriteString(tmp);
      codeGen.WriteString(' = ');
      codeGen.Compile(e.ValueExpr);
      codeGen.WriteStatementEnd;
      codeGen.Indent;
      for i:=0 to e.CaseConditions.Count-1 do begin
         if i>0 then
            codeGen.WriteString(' else ');
         codeGen.WriteString('if (');
         cond:=TCaseCondition(e.CaseConditions.List[i]);
         CodeGenCondition(codeGen, cond, procedure begin codeGen.WriteString(tmp) end);
         codeGen.WriteBlockBegin(') ');
         codeGen.Compile(cond.TrueExpr);
         codeGen.WriteBlockEndLn;
      end;
      if e.ElseExpr<>nil then begin
         codeGen.WriteBlockBegin(' else ');
         codeGen.Compile(e.ElseExpr);
         codeGen.WriteBlockEndLn;
      end;
      codeGen.WriteBlockEndLn;

   end;
end;

// CodeGenCondition
//
class procedure TJSCaseExpr.CodeGenCondition(codeGen : TdwsCodeGen; cond : TCaseCondition;
                                             const writeOperand : TProc);
begin
   if cond is TCompareConstStringCaseCondition then begin
      writeOperand();
      codeGen.WriteString('==');
      codeGen.WriteLiteralString(TCompareConstStringCaseCondition(cond).Value);
   end else if cond is TCompareCaseCondition then begin
      writeOperand();
      codeGen.WriteString('==');
      codeGen.Compile(TCompareCaseCondition(cond).CompareExpr);
   end else if cond is TRangeCaseCondition then begin
      codeGen.WriteString('(');
      writeOperand();
      codeGen.WriteString('>=');
      codeGen.Compile(TRangeCaseCondition(cond).FromExpr);
      codeGen.WriteString(')&&(');
      writeOperand();
      codeGen.WriteString('<=');
      codeGen.Compile(TRangeCaseCondition(cond).ToExpr);
      codeGen.WriteString(')');
   end else raise ECodeGenUnknownExpression.Create(cond.ClassName);
end;

// ------------------
// ------------------ TJSExitExpr ------------------
// ------------------

// CodeGen
//
procedure TJSExitExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   func : TFuncSymbol;
begin
   if codeGen.Context is TdwsProcedure then
      func:=TdwsProcedure(codeGen.Context).Func
   else func:=nil;

   if (func<>nil) and (func.Typ<>nil) then begin

      // exit from a function or method with result
      codeGen.WriteString('return ');
      codeGen.WriteString(TdwsJSCodeGen(codeGen).ResultSymbolName);
      if IsLocalVarParam(codeGen, func.Result) then
         codeGen.WriteString('.'+TdwsJSCodeGen.cBoxFieldName);

   end else if (func<>nil) and (func.Kind=fkConstructor) then begin

      // exit from a constructor
      codeGen.WriteString('return ');
      codeGen.WriteString(TdwsJSCodegen(codeGen).SelfSymbolName);

   end else codeGen.WriteString('return');

   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSExitValueExpr ------------------
// ------------------

// CodeGen
//
procedure TJSExitValueExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TExitValueExpr;
begin
   e:=TExitValueExpr(expr);

   codeGen.WriteString('return ');

   if codeGen.TryDepth=0 then begin
      if e.AssignExpr is TAssignConstExpr then begin
         TdwsJSCodeGen(codeGen).WriteVariant(e.AssignExpr.Left.Typ, TAssignConstExpr(e.AssignExpr).RightValue);
         codeGen.WriteStatementEnd;
      end else if e.AssignExpr.ClassType=TAssignExpr then begin
         codeGen.CompileValue(e.AssignExpr.Right);
         codeGen.WriteStatementEnd;
      end else codeGen.Compile(e.AssignExpr);
   end else codeGen.Compile(e.AssignExpr);
end;

// ------------------
// ------------------ TJSIncDecVarFuncExpr ------------------
// ------------------

// DoCodeGen
//
procedure TJSIncDecVarFuncExpr.DoCodeGen(codeGen : TdwsCodeGen; expr : TMagicFuncExpr;
                                         op : Char; noWrap : Boolean);
var
   e : TIncVarFuncExpr;
   left, right : TExprBase;
begin
   e:=TIncVarFuncExpr(expr);
   left:=e.Args[0];
   right:=e.Args[1];
   if ExprIsConstantInteger(right, 1) then begin

      codeGen.WriteString(op);
      codeGen.WriteString(op);
      codeGen.Compile(left);

   end else begin

      if not noWrap then
         codeGen.WriteString('(');
      codeGen.Compile(left);
      codeGen.WriteString(op);
      codeGen.WriteString('= ');
      codeGen.Compile(right);
      if not noWrap then
         codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSIncVarFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSIncVarFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   DoCodeGen(codeGen, TIncVarFuncExpr(expr), '+', False);
end;

// CodeGenNoWrap
//
procedure TJSIncVarFuncExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   DoCodeGen(codeGen, TIncVarFuncExpr(expr), '+', True);
end;

// ------------------
// ------------------ TJSDecVarFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSDecVarFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   DoCodeGen(codeGen, TIncVarFuncExpr(expr), '-', False);
end;

// CodeGenNoWrap
//
procedure TJSDecVarFuncExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   DoCodeGen(codeGen, TIncVarFuncExpr(expr), '-', True);
end;

// ------------------
// ------------------ TJSSarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSSarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TSarExpr;
   d : Int64;
begin
   e:=TSarExpr(expr);

   codeGen.WriteString('(');
   if e.Right is TConstIntExpr then begin

      d:=e.Right.EvalAsInteger(nil);
      if d=0 then
         codeGen.CompileNoWrap(e.Left)
      else begin
         codeGen.Compile(e.Left);
         if d>31 then
            codeGen.WriteString('<0?-1:0')
         else begin
            codeGen.WriteString('>>');
            codeGen.Compile(e.Right);
         end;
      end;

   end else begin

      codeGen.Compile(e.Left);
      codeGen.WriteString('>>');
      codeGen.Compile(e.Right);

   end;
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSConvIntegerExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConvIntegerExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TUnaryOpExpr;
   etyp : TTypeSymbol;
begin
   e:=TUnaryOpExpr(expr);
   etyp:=e.Expr.Typ.UnAliasedType;
   if etyp is TBaseBooleanSymbol then begin
      codeGen.WriteString('(');
      codeGen.Compile(e.Expr);
      codeGen.WriteString('?1:0)');
   end else if etyp is TBaseFloatSymbol then begin
      codeGen.WriteString('Math.round(');
      codeGen.Compile(e.Expr);
      codeGen.WriteString(')');
   end else if etyp is TBaseIntegerSymbol then
      codeGen.Compile(e.Expr)
   else begin
      codeGen.WriteString('parseInt(');
      codeGen.Compile(e.Expr);
      codeGen.WriteString(',10)');
   end;
end;

// ------------------
// ------------------ TJSConvFloatExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSConvFloatExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TUnaryOpExpr;
   etyp : TTypeSymbol;
begin
   e:=TUnaryOpExpr(expr);
   etyp:=e.Expr.Typ.UnAliasedType;
   if etyp is TBaseBooleanSymbol then begin
      codeGen.WriteString('(');
      codeGen.Compile(e.Expr);
      codeGen.WriteString('?1:0)');
   end else if (etyp is TBaseIntegerSymbol) or (etyp is TBaseFloatSymbol) then
      codeGen.CompileNoWrap(e.Expr)
   else begin
      codeGen.WriteString('Number(');
      codeGen.CompileNoWrap(e.Expr);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSConvStringExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSConvStringExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TUnaryOpExpr;
   etyp : TTypeSymbol;
begin
   e:=TUnaryOpExpr(expr);
   etyp:=e.Expr.Typ.UnAliasedType;
   if etyp is TBaseStringSymbol then begin
      codeGen.Compile(e.Expr);
   end else if (etyp is TBaseIntegerSymbol) or (etyp is TBaseFloatSymbol) or (etyp is TBaseBooleanSymbol) then begin
      codeGen.CompileNoWrap(e.Expr);
      codeGen.WriteString('.toString()');
   end else begin
      codeGen.WriteString('String(');
      codeGen.CompileNoWrap(e.Expr);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSConvStaticArrayToDynamicExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSConvStaticArrayToDynamicExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TConvStaticArrayToDynamicExpr;
begin
   e:=TConvStaticArrayToDynamicExpr(expr);

   codeGen.Compile(e.Expr);
   if (e.Expr as TArrayConstantExpr).Size>0 then
      codeGen.WriteString('.slice()');
end;

// ------------------
// ------------------ TJSOrdExpr ------------------
// ------------------

// CodeGen
//
procedure TJSOrdExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TOrdExpr;
begin
   e:=TOrdExpr(expr);

   codeGen.Dependencies.Add('$Ord');
   codeGen.WriteString('$Ord(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(',');
   WriteLocationString(codeGen, expr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSOrdIntExpr ------------------
// ------------------

// CodeGen
//
procedure TJSOrdIntExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TOrdIntExpr;
begin
   e:=TOrdIntExpr(expr);

   if e.Expr is TStringArrayOpExpr then begin
      TJSStringArrayOpExpr.CodeGenCharXxxAt(codeGen, e.Expr, 'Code')
   end else codeGen.Compile(e.Expr);
end;

// ------------------
// ------------------ TJSOrdStrExpr ------------------
// ------------------

// CodeGen
//
procedure TJSOrdStrExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TOrdExpr;
begin
   e:=TOrdExpr(expr);

   if e.Expr is TStringArrayOpExpr then begin
      TJSStringArrayOpExpr.CodeGenCharXxxAt(codeGen, e.Expr, 'Code')
   end else begin
      codeGen.Dependencies.Add('$OrdS');
      codeGen.WriteString('$OrdS(');
      codeGen.CompileNoWrap(e.Expr);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSClassAsClassExpr ------------------
// ------------------

// CodeGen
//
procedure TJSClassAsClassExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TClassAsClassExpr;
begin
   codeGen.Dependencies.Add('$AsClass');

   e:=TClassAsClassExpr(expr);
   codeGen.WriteString('$AsClass(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(',');
   codeGen.WriteSymbolName(TClassOfSymbol(e.Typ).TypClassSymbol.UnAliasedType);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSObjAsClassExpr ------------------
// ------------------

// CodeGen
//
procedure TJSObjAsClassExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TObjAsClassExpr;
begin
   e:=TObjAsClassExpr(expr);

   if e.Expr.Typ.IsOfType(e.Typ) then begin

      codeGen.Compile(e.Expr);

   end else begin

      codeGen.Dependencies.Add('$As');

      codeGen.WriteString('$As(');
      codeGen.Compile(e.Expr);
      codeGen.WriteString(',');
      codeGen.WriteSymbolName(e.Typ.UnAliasedType);
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSIsOpExpr ------------------
// ------------------

// CodeGen
//
procedure TJSIsOpExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TIsOpExpr;
begin
   e:=TIsOpExpr(expr);

   if not (e.Left.Typ as TClassSymbol).IsExternalRooted then begin

      codeGen.Dependencies.Add('$Is');

      codeGen.WriteString('$Is(');
      codeGen.Compile(e.Left);
      codeGen.WriteString(',');
      codeGen.WriteSymbolName(e.Right.Typ.UnAliasedType.Typ);
      codeGen.WriteString(')');

   end else begin

      codeGen.Compile(e.Left);
      codeGen.WriteString(' instanceof ');
      codeGen.WriteSymbolName(e.Right.Typ.UnAliasedType.Typ);

   end;
end;

// ------------------
// ------------------ TJSObjAsIntfExpr ------------------
// ------------------

// CodeGen
//
procedure TJSObjAsIntfExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TObjAsIntfExpr;
begin
   codeGen.Dependencies.Add('$AsIntf');

   e:=TObjAsIntfExpr(expr);
   codeGen.WriteString('$AsIntf(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(',"');
   codeGen.WriteSymbolName(e.Typ.UnAliasedType);
   codeGen.WriteString('")');
end;

// ------------------
// ------------------ TJSObjToClassTypeExpr ------------------
// ------------------

// CodeGen
//
procedure TJSObjToClassTypeExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TObjToClassTypeExpr;
begin
   e:=TObjToClassTypeExpr(expr);

   codeGen.Dependencies.Add('$ToClassType');

   codeGen.WriteString('$ToClassType(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSIntfAsClassExpr ------------------
// ------------------

// CodeGen
//
procedure TJSIntfAsClassExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TIntfAsClassExpr;
begin
   codeGen.Dependencies.Add('$IntfAsClass');

   e:=TIntfAsClassExpr(expr);
   codeGen.WriteString('$IntfAsClass(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(',');
   codeGen.WriteSymbolName(e.Typ.UnAliasedType);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSIntfAsIntfExpr ------------------
// ------------------

// CodeGen
//
procedure TJSIntfAsIntfExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TIntfAsIntfExpr;
begin
   codeGen.Dependencies.Add('$AsIntf');
   codeGen.Dependencies.Add('$IntfAsClass');

   e:=TIntfAsIntfExpr(expr);
   codeGen.WriteString('$AsIntf($IntfAsClass(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(',TObject),"');
   codeGen.WriteSymbolName(e.Typ.UnAliasedType);
   codeGen.WriteString('")');
end;

// ------------------
// ------------------ TJSTImplementsIntfOpExpr ------------------
// ------------------

// CodeGen
//
procedure TJSTImplementsIntfOpExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TImplementsIntfOpExpr;
begin
   codeGen.Dependencies.Add('$Implements');

   e:=TImplementsIntfOpExpr(expr);
   codeGen.WriteString('$Implements(');
   codeGen.Compile(e.Left);
   codeGen.WriteString(',"');
   codeGen.WriteSymbolName(e.Right.Typ);
   codeGen.WriteString('")');
end;

// ------------------
// ------------------ TJSTClassImplementsIntfOpExpr ------------------
// ------------------

// CodeGen
//
procedure TJSTClassImplementsIntfOpExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TClassImplementsIntfOpExpr;
begin
   codeGen.Dependencies.Add('$ClassImplements');

   e:=TClassImplementsIntfOpExpr(expr);
   codeGen.WriteString('$ClassImplements(');
   codeGen.Compile(e.Left);
   codeGen.WriteString(',"');
   codeGen.WriteSymbolName(e.Right.Typ);
   codeGen.WriteString('")');
end;

// ------------------
// ------------------ TDataSymbolList ------------------
// ------------------

// Destroy
//
destructor TDataSymbolList.Destroy;
begin
   ExtractAll;
   inherited;
end;

// ------------------
// ------------------ TJSExprCodeGen ------------------
// ------------------

// IsLocalVarParam
//
class function TJSExprCodeGen.IsLocalVarParam(codeGen : TdwsCodeGen; sym : TDataSymbol) : Boolean;
begin
   if sym.Typ.UnAliasedType.ClassType=TRecordSymbol then
      Result:=False
   else Result:=(TdwsJSCodeGen(codeGen).FAllLocalVarSymbols.Contains(sym));
end;

// WriteLocationString
//
class procedure TJSExprCodeGen.WriteLocationString(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   if cgoNoSourceLocations in codeGen.Options then
      codeGen.WriteString('""')
   else codeGen.WriteLiteralString(codeGen.LocationString(expr));
end;

// ------------------
// ------------------ TJSRecordExpr ------------------
// ------------------

// CodeGen
//
procedure TJSRecordExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TRecordExpr;
begin
   e:=TRecordExpr(expr);
   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('.');
   CodeGenFieldName(codeGen, e);
end;

// CodeGenFieldName
//
class procedure TJSRecordExpr.CodeGenFieldName(codeGen : TdwsCodeGen; e : TRecordExpr);
var
   member : TFieldSymbol;
begin
   member:=(e.BaseExpr.Typ.UnAliasedType as TRecordSymbol).FieldAtOffset(e.MemberOffset);
   codeGen.WriteSymbolName(member);
end;

// ------------------
// ------------------ TJSFieldExpr ------------------
// ------------------

// CodeGen
//
procedure TJSFieldExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TFieldExpr;
begin
   e:=TFieldExpr(expr);

   CodeGenObjectExpr(codeGen, e);

   codeGen.WriteString('.');

   CodeGenFieldName(codeGen, e);
end;

// CodeGenObjectExpr
//
class procedure TJSFieldExpr.CodeGenObjectExpr(codeGen : TdwsCodeGen; e : TFieldExpr);
begin
   if cgoNoCheckInstantiated in codeGen.Options then begin
      codeGen.Compile(e.ObjectExpr);
   end else begin
      codeGen.Dependencies.Add('$Check');
      codeGen.WriteString('$Check(');
      codeGen.Compile(e.ObjectExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, e);
      codeGen.WriteString(')');
   end;
end;

// CodeGenFieldName
//
class procedure TJSFieldExpr.CodeGenFieldName(codeGen : TdwsCodeGen; e : TFieldExpr);
var
   field : TFieldSymbol;
begin
   field:=e.FieldSym;
   codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(field, field.StructSymbol));
end;

// ------------------
// ------------------ TJSInlineObject ------------------
// ------------------

// CodeGenMembers
//
procedure TJSInlineObject.CodeGenMembers(codeGen : TdwsCodeGen; members : TMembersSymbolTable;
                                         useExternal : Boolean);
var
   firstField : Boolean;
   sym : TSymbol;
   fieldSym : TFieldSymbol;
   locData : IDataContext;
   externalName : String;
begin
   codeGen.WriteBlockBegin('');

   firstField:=True;
   for sym in members do begin
      if sym.ClassType<>TFieldSymbol then continue;

      fieldSym:=TFieldSymbol(sym);
      if firstField then
         firstField:=False
      else codeGen.WriteString(',');

      if useExternal or fieldSym.HasExternalName then begin

         externalName:=fieldSym.ExternalName;

         if     (cgoOptimizeForSize in codeGen.Options)
            and IsValidJSName(externalName)
            and not codeGen.SymbolMap.IsReserved(externalName) then begin

            codeGen.WriteString(externalName);

         end else begin

            codeGen.WriteLiteralString(externalName);

         end;

      end else begin

         codeGen.WriteSymbolName(fieldSym);

      end;

      if cgoOptimizeForSize in codeGen.Options then
         codeGen.WriteString(':')
      else codeGen.WriteString(' : ');

      if fieldSym.DefaultExpr<>nil then
         codeGen.CompileNoWrap(fieldSym.DefaultExpr as TTypedExpr)
      else if fieldSym.DefaultValue<>nil then begin
         codeGen.CreateDataContext(fieldSym.DefaultValue, 0, locData);
         TdwsJSCodeGen(codeGen).WriteValue(fieldSym.Typ, locData);
      end else TdwsJSCodeGen(codeGen).WriteDefaultValue(fieldSym.Typ, False);
      codeGen.WriteStringLn('');
   end;

   codeGen.WriteBlockEnd;
end;

// ------------------
// ------------------ TJSDynamicRecordExpr ------------------
// ------------------

// CodeGen
//
procedure TJSDynamicRecordExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TDynamicRecordExpr;
   recordTyp : TRecordSymbol;
begin
   e:=TDynamicRecordExpr(expr);

   recordTyp:=e.Typ as TRecordSymbol;

   CodeGenMembers(codeGen, recordTyp.Members, False);
end;

// ------------------
// ------------------ TJSConstructorAnonymousExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstructorAnonymousExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstructorAnonymousExpr;
   c : TClassSymbol;
begin
   e := TConstructorAnonymousExpr(expr);
   c := e.Typ as TClassSymbol;

   CodeGenMembers(codeGen, c.Members, True);
end;

// ------------------
// ------------------ TJSExceptExpr ------------------
// ------------------

// CodeGen
//
procedure TJSExceptExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TExceptExpr;
   de : TExceptDoExpr;
   i : Integer;
begin
   e:=TExceptExpr(expr);
   codeGen.WriteBlockBegin('try ');
   codeGen.EnterTry;
   codeGen.CompileStatement(e.TryExpr);
   codeGen.LeaveTry;
   codeGen.WriteBlockEnd;
   codeGen.WriteBlockBegin(' catch ($e) ');

   if e.DoExprCount=0 then

      codeGen.CompileStatement(e.HandlerExpr)

   else begin

      codeGen.Dependencies.Add('$W');

      codeGen.Dependencies.Add('$Is');
      codeGen.Dependencies.Add('Exception');


      if (e.DoExprCount=1) and (e.DoExpr[0].ExceptionVar.Typ.UnAliasedType=codeGen.Context.TypException) then begin

         // special case with only "on E: Exception"

         de:=e.DoExpr[0];
         codeGen.LocalTable.AddSymbolDirect(de.ExceptionVar);
         try
            codeGen.WriteString('var ');
            codeGen.WriteSymbolName(de.ExceptionVar);
            codeGen.WriteStringLn(' = $W($e);');
            codeGen.Compile(de.DoBlockExpr);
         finally
            codeGen.LocalTable.Remove(de.ExceptionVar);
         end;

      end else begin

         // normal case, multiple exception or filtered exceptions

         for i:=0 to e.DoExprCount-1 do begin
            de:=e.DoExpr[i];
            if i>0 then
               codeGen.WriteString('else ');
            codeGen.WriteString('if ($Is($e,');
            codeGen.WriteSymbolName(de.ExceptionVar.Typ.UnAliasedType);
            codeGen.WriteBlockBegin(')) ');

            codeGen.LocalTable.AddSymbolDirect(de.ExceptionVar);
            try
               codeGen.WriteString('var ');
               codeGen.WriteSymbolName(de.ExceptionVar);
               codeGen.WriteStringLn(' = $W($e);');
               codeGen.Compile(de.DoBlockExpr);
            finally
               codeGen.LocalTable.Remove(de.ExceptionVar);
            end;

            codeGen.WriteBlockEndLn;
         end;

         if e.ElseExpr<>nil then begin
            codeGen.WriteBlockBegin('else ');

            codeGen.Compile(e.ElseExpr);

            codeGen.WriteBlockEndLn;
         end else codeGen.WriteStringLn('else throw $e');

      end;
   end;
   codeGen.WriteBlockEndLn;
end;

// ------------------
// ------------------ TJSFinallyExpr ------------------
// ------------------

// CodeGen
//
procedure TJSFinallyExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TFinallyExpr;
begin
   e:=TFinallyExpr(expr);

   codeGen.WriteBlockBegin('try ');
   codeGen.EnterTry;
   codeGen.CompileStatement(e.TryExpr);
   codeGen.LeaveTry;
   codeGen.WriteBlockEnd;
   codeGen.WriteBlockBegin(' finally ');
   codeGen.CompileStatement(e.HandlerExpr);
   codeGen.WriteBlockEndLn;
end;

// ------------------
// ------------------ TJSNewArrayExpr ------------------
// ------------------

// CodeGen
//
procedure TJSNewArrayExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TNewArrayExpr;
   elemTyp : TTypeSymbol;
   i : Integer;
begin
   e:=TNewArrayExpr(expr);

   elemTyp:=e.Typ.Typ;

   if e.LengthExprCount>1 then begin

      codeGen.Dependencies.Add('$NewArrayFn');

      for i:=0 to e.LengthExprCount-2 do begin
         codeGen.WriteString('$NewArrayFn(');
         codeGen.Compile(e.LengthExpr[i]);
         codeGen.WriteString(',function (){return ');
         elemTyp:=elemTyp.Typ;
      end;

   end else if (e.LengthExprCount=1) and (e.LengthExpr[0] is TConstIntExpr) then begin

      // special case of a new single-dimensional array with a constant length

      i:=TConstIntExpr(e.LengthExpr[0]).Value;
      if i=0 then begin

         codeGen.WriteString('[]');
         exit;

      end else if (i in [1..5]) and elemTyp.IsBaseType then begin

         codeGen.WriteString('[');
         repeat
            (codeGen as TdwsJSCodeGen).WriteDefaultValue(elemTyp, False);
            Dec(i);
            if i>0 then
               codeGen.WriteString(',')
            else Break;
         until False;
         codeGen.WriteString(']');
         exit;

      end;

   end;

   if elemTyp.IsBaseType then begin

      codeGen.Dependencies.Add('$NewArray');

      codeGen.WriteString('$NewArray(');
      codeGen.Compile(e.LengthExpr[e.LengthExprCount-1]);
      codeGen.WriteString(',');
      (codeGen as TdwsJSCodeGen).WriteDefaultValue(elemTyp, False);
      codeGen.WriteString(')');

   end else begin

      codeGen.Dependencies.Add('$NewArrayFn');

      codeGen.WriteString('$NewArrayFn(');
      codeGen.Compile(e.LengthExpr[e.LengthExprCount-1]);
      codeGen.WriteString(',function (){return ');
      (codeGen as TdwsJSCodeGen).WriteDefaultValue(elemTyp, False);
      codeGen.WriteString('})');

   end;

   for i:=0 to e.LengthExprCount-2 do
      codeGen.WriteString('})');
end;

// ------------------
// ------------------ TJSArrayLengthExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayLengthExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayLengthExpr;
begin
   e:=TArrayLengthExpr(expr);

   if e.Delta<>0 then
      codeGen.WriteString('(');

   codeGen.Compile(e.Expr);
   codeGen.WriteString('.length');

   if e.Delta<>0 then begin
      if e.Delta>0 then
         codeGen.WriteString('+');
      codeGen.WriteInteger(e.Delta);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSArraySetLengthExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArraySetLengthExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArraySetLengthExpr;
   elemTyp : TTypeSymbol;
begin
   e:=TArraySetLengthExpr(expr);

   if ExprIsConstantInteger(e.LengthExpr, 0) then begin

      codeGen.Compile(e.BaseExpr);
      codeGen.WriteStringLn('.length=0;');

   end else begin

      elemTyp:=e.BaseExpr.Typ.Typ;

      if elemTyp.IsBaseType then begin

         codeGen.Dependencies.Add('$ArraySetLen');

         codeGen.WriteString('$ArraySetLen(');
         codeGen.Compile(e.BaseExpr);
         codeGen.WriteString(',');
         codeGen.Compile(e.LengthExpr);
         codeGen.WriteString(',');
         (codeGen as TdwsJSCodeGen).WriteDefaultValue(elemTyp, False);
         codeGen.WriteStringLn(');');

      end else begin

         codeGen.Dependencies.Add('$ArraySetLenC');

         codeGen.WriteString('$ArraySetLenC(');
         codeGen.Compile(e.BaseExpr);
         codeGen.WriteString(',');
         codeGen.Compile(e.LengthExpr);
         codeGen.WriteString(',function (){return ');
         (codeGen as TdwsJSCodeGen).WriteDefaultValue(e.BaseExpr.Typ.Typ, False);
         codeGen.WriteStringLn('});');

      end;

   end;
end;

// ------------------
// ------------------ TJSArrayAddExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayAddExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayAddExpr;
begin
   e:=TArrayAddExpr(expr);

   codeGen.Compile(e.BaseExpr);

   CodeGenPush(codeGen, e, 0);

   codeGen.WriteStatementEnd;
end;

// CodeGenPush
//
class procedure TJSArrayAddExpr.CodeGenPush(codeGen : TdwsCodeGen; e : TArrayAddExpr; skip : Integer);
var
   arg : TTypedExpr;
   i : Integer;
   elementTyp : TTypeSymbol;
   pushType : Integer;
   inPushElems : Boolean;
begin
   elementTyp:=(e.BaseExpr.Typ as TDynamicArraySymbol).Typ;

   pushType:=0;
   for i:=skip to e.ArgCount-1 do begin
      arg:=e.ArgExpr[i];
      if    elementTyp.IsCompatible(arg.Typ)
         or (    (arg is TArrayConstantExpr)
             and elementTyp.IsCompatible(arg.Typ.Typ)) then
         pushType:=pushType or 1
      else pushType:=pushType or 2;
   end;

   if pushType=1 then begin

      // only elements

      codeGen.WriteString('.push(');
      for i:=skip to e.ArgCount-1 do begin
         if i>skip then
            codeGen.WriteString(', ');
         arg := e.ArgExpr[i];
         if arg is TArrayConstantExpr then
            TJSArrayConstantExpr.CodeGenElements(codeGen, TArrayConstantExpr(arg))
         else codeGen.CompileValue(e.ArgExpr[i]);
      end;
      codeGen.WriteString(')');

   end else begin

      // a mix of elements and arrays

      codeGen.Dependencies.Add('$Pusha');

      inPushElems:=False;
      for i:=skip to e.ArgCount-1 do begin
         arg:=e.ArgExpr[i];

         if elementTyp.IsCompatible(arg.Typ) then begin

            if not inPushElems then begin
               codeGen.WriteString('.pusha([');
               inPushElems:=True;
            end else codeGen.WriteString(', ');
            codeGen.CompileValue(arg);

         end else begin


            if inPushElems then begin
               codeGen.WriteString('])');
               inPushElems:=False;
            end;
            codeGen.WriteString('.pusha(');
            codeGen.CompileValue(arg);
            codeGen.WriteString(')');

         end;
      end;

      if inPushElems then
         codeGen.WriteString('])');

   end;
end;

// ------------------
// ------------------ TJSArrayPeekExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayPeekExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayPeekExpr;
begin
   e:=TArrayPeekExpr(expr);

   codeGen.Dependencies.Add('$Peek');

   codeGen.WriteString('$Peek(');
   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString(',');
   WriteLocationString(codeGen, expr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSArrayPopExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayPopExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayPopExpr;
begin
   e:=TArrayPopExpr(expr);

   if cgoNoRangeChecks in codeGen.Options then begin

      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString('.pop()');

   end else begin

      codeGen.Dependencies.Add('$Pop');

      codeGen.WriteString('$Pop(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSArrayDeleteExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayDeleteExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayDeleteExpr;
begin
   e:=TArrayDeleteExpr(expr);

   codeGen.Compile(e.BaseExpr);

   if     ExprIsConstantInteger(e.IndexExpr, 0)
      and ((e.CountExpr=nil) or ExprIsConstantInteger(e.CountExpr, 1)) then begin

      // optimize to shift for Delete(0, 1)
      codeGen.WriteString('.shift()');

   end else begin

      codeGen.WriteString('.splice(');
      codeGen.Compile(e.IndexExpr);
      codeGen.WriteString(',');
      if e.CountExpr<>nil then
         codeGen.Compile(e.CountExpr)
      else codeGen.WriteString('1');
      codeGen.WriteStringLn(')');

   end;

   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSArrayIndexOfExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayIndexOfExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayIndexOfExpr;
begin
   e:=TArrayIndexOfExpr(expr);

   if    (e.ItemExpr.Typ is TRecordSymbol)
      or (e.ItemExpr.Typ is TStaticArraySymbol) then begin

      codeGen.Dependencies.Add('$IndexOfRecord');

      codeGen.WriteString('$IndexOfRecord(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.ItemExpr);
      codeGen.WriteString(',');
      if e.FromIndexExpr<>nil then
         codeGen.CompileNoWrap(e.FromIndexExpr)
      else codeGen.WriteString('0');
      codeGen.WriteString(')');

   end else begin

      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString('.indexOf(');
      codeGen.CompileNoWrap(e.ItemExpr);
      if e.FromIndexExpr<>nil then begin
         codeGen.WriteString(',');
         codeGen.CompileNoWrap(e.FromIndexExpr);
      end;
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSArraySortExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArraySortExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArraySortExpr;
begin
   e:=TArraySortExpr(expr);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('.sort(');
   codeGen.CompileNoWrap((e.CompareExpr as TFuncPtrExpr).CodeExpr);
   codeGen.WriteString(')');
   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSArraySortExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayMapExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayMapExpr;
begin
   e:=TArrayMapExpr(expr);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('.map(');
   codeGen.CompileNoWrap((e.MapFuncExpr as TFuncPtrExpr).CodeExpr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSArrayRemoveExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayRemoveExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayRemoveExpr;
   removeFunc : String;
begin
   e:=TArrayRemoveExpr(expr);

   if    (e.ItemExpr.Typ is TRecordSymbol)
      or (e.ItemExpr.Typ is TStaticArraySymbol) then
      removeFunc:='$RemoveRecord'
   else removeFunc:='$Remove';

   codeGen.Dependencies.Add(removeFunc);

   codeGen.WriteString(removeFunc);
   codeGen.WriteString('(');
   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString(',');
   codeGen.CompileNoWrap(e.ItemExpr);
   codeGen.WriteString(',');
   if e.FromIndexExpr<>nil then
      codeGen.CompileNoWrap(e.FromIndexExpr)
   else codeGen.WriteString('0');
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSArrayInsertExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayInsertExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayInsertExpr;
   noRangeCheck : Boolean;
begin
   e:=TArrayInsertExpr(expr);

   noRangeCheck:=   (cgoNoRangeChecks in codeGen.Options)
                 or ExprIsConstantInteger(e.IndexExpr, 0);

   if noRangeCheck then begin

      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString('.splice(');
      codeGen.Compile(e.IndexExpr);
      codeGen.WriteString(',0,');
      codeGen.CompileValue(e.ItemExpr);
      codeGen.WriteStringLn(');');

   end else begin

      codeGen.Dependencies.Add('$ArrayInsert');

      codeGen.WriteString('$ArrayInsert(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      codeGen.Compile(e.IndexExpr);
      codeGen.WriteString(',');
      codeGen.CompileValue(e.ItemExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteStringLn(');');

   end;
end;

// ------------------
// ------------------ TJSArrayCopyExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayCopyExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayCopyExpr;
   rangeCheckFunc : String;
   noRangeCheck : Boolean;
begin
   e:=TArrayCopyExpr(expr);

   noRangeCheck:=(cgoNoRangeChecks in codeGen.Options) or (e.IndexExpr=nil);

   if noRangeCheck then begin

      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString('.slice(');
      if e.IndexExpr=nil then
         codeGen.WriteString('0')
      else begin
         codeGen.Compile(e.IndexExpr);
         if e.CountExpr<>nil then begin
            codeGen.WriteString(',');
            codeGen.Compile(e.CountExpr)
         end;
      end;
      codeGen.WriteString(')');

   end else begin

      if e.CountExpr=nil then
         rangeCheckFunc:='$ArrayCopy'
      else rangeCheckFunc:='$ArrayCopyLen';

      codeGen.Dependencies.Add(rangeCheckFunc);

      codeGen.WriteString(rangeCheckFunc);
      codeGen.WriteString('(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      codeGen.Compile(e.IndexExpr);
      if e.CountExpr<>nil then begin
         codeGen.WriteString(',');
         codeGen.Compile(e.CountExpr);
      end;
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSArraySwapExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArraySwapExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArraySwapExpr;
   noRangeCheck : Boolean;
begin
   e:=TArraySwapExpr(expr);

   noRangeCheck:=(cgoNoRangeChecks in codeGen.Options);

   if noRangeCheck then begin
      codeGen.Dependencies.Add('$ArraySwap');
      codeGen.WriteString('$ArraySwap(');
   end else begin
      codeGen.Dependencies.Add('$ArraySwapChk');
      codeGen.WriteString('$ArraySwapChk(');
   end;

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString(',');
   codeGen.Compile(e.Index1Expr);
   codeGen.WriteString(',');
   codeGen.Compile(e.Index2Expr);

   if not noRangeCheck then begin
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
   end;

   codeGen.WriteStringLn(');');
end;

// ------------------
// ------------------ TJSArrayConcatExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayConcatExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayConcatExpr;
   i : Integer;
begin
   e:=TArrayConcatExpr(expr);

   codeGen.Compile(e.ArgExpr[0]);
   codeGen.WriteString('.concat(');
   for i:=1 to e.ArgCount-1 do begin
      if i>1 then
         codeGen.WriteString(', ');
      codeGen.CompileNoWrap(e.ArgExpr[i]);
   end;
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSArrayTypedFluentExpr ------------------
// ------------------

// Create
//
constructor TJSArrayTypedFluentExpr.Create(const code, dependency : String);
begin
   inherited Create;
   FCode := code;
   FDependency := dependency;
end;

// CodeGen
//
procedure TJSArrayTypedFluentExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayTypedFluentExpr;
begin
   e := TArrayTypedFluentExpr(expr);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString(FCode);

   if FDependency <> '' then
      codeGen.Dependencies.Add(FDependency);
end;

// ------------------
// ------------------ TJSStaticArrayExpr ------------------
// ------------------

// CodeGen
//
procedure TJSStaticArrayExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TStaticArrayExpr;
   noRangeCheck : Boolean;
   typ : TStaticArraySymbol;
begin
   e:=TStaticArrayExpr(expr);

   noRangeCheck:=(cgoNoRangeChecks in codeGen.Options) or e.IndexExpr.IsConstant;
   typ:=(e.BaseExpr.Typ as TStaticArraySymbol);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('[');

   if noRangeCheck then begin

      if typ.LowBound=0 then
         codeGen.CompileNoWrap(e.IndexExpr)
      else begin
         codeGen.WriteString('(');
         codeGen.CompileNoWrap(e.IndexExpr);
         if typ.LowBound>0 then begin
            codeGen.WriteString(')-');
            codeGen.WriteInteger(typ.LowBound);
         end else begin
            codeGen.WriteString(')+');
            codeGen.WriteInteger(-typ.LowBound);
         end;
      end;

   end else begin

      codeGen.Dependencies.Add('$Idx');

      codeGen.WriteString('$Idx(');
      codeGen.CompileNoWrap(e.IndexExpr);
      codeGen.WriteString(',');
      codeGen.WriteInteger(typ.LowBound);
      codeGen.WriteString(',');
      codeGen.WriteInteger(typ.HighBound);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');

   end;

   codeGen.WriteString(']');

end;

// ------------------
// ------------------ TJSStaticArrayBoolExpr ------------------
// ------------------

// CodeGen
//
procedure TJSStaticArrayBoolExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TStaticArrayBoolExpr;
begin
   e:=TStaticArrayBoolExpr(expr);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('[');

   codeGen.Compile(e.IndexExpr);

   codeGen.WriteString('?1:0]');
end;

// ------------------
// ------------------ TJSDynamicArrayExpr ------------------
// ------------------

// CodeGen
//
procedure TJSDynamicArrayExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TDynamicArrayExpr;
   noRangeCheck : Boolean;
begin
   e:=TDynamicArrayExpr(expr);

   noRangeCheck:=(cgoNoRangeChecks in codeGen.Options);

   if noRangeCheck then begin

      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString('[');
      codeGen.CompileNoWrap(e.IndexExpr);
      codeGen.WriteString(']');

   end else begin

      codeGen.Dependencies.Add('$DIdxR');

      codeGen.WriteString('$DIdxR(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.IndexExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');

   end;

end;

// ------------------
// ------------------ TJSDynamicArraySetExpr ------------------
// ------------------

// CodeGen
//
procedure TJSDynamicArraySetExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TDynamicArraySetExpr;
   noRangeCheck : Boolean;
begin
   e:=TDynamicArraySetExpr(expr);

   noRangeCheck:=(cgoNoRangeChecks in codeGen.Options);

   if noRangeCheck then begin

      codeGen.Compile(e.ArrayExpr);
      codeGen.WriteString('[');
      codeGen.CompileNoWrap(e.IndexExpr);
      codeGen.WriteString(']=');
      codeGen.CompileNoWrap(e.ValueExpr);
      codeGen.WriteStatementEnd;

   end else begin

      codeGen.Dependencies.Add('$DIdxW');

      codeGen.WriteString('$DIdxW(');
      codeGen.CompileNoWrap(e.ArrayExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.IndexExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.ValueExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteStringLn(');');

   end;

end;

// ------------------
// ------------------ TJSStringArrayOpExpr ------------------
// ------------------

// CodeGen
//
procedure TJSStringArrayOpExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   CodeGenCharXxxAt(codeGen, expr, '');
end;

// CodeGenCharXxxAt
//
class procedure TJSStringArrayOpExpr.CodeGenCharXxxAt(
   codeGen : TdwsCodeGen; expr : TExprBase; const xxx : String);
var
   e : TStringArrayOpExpr;
   noRangeCheck : Boolean;
   funcName : String;
   delta : Int64;
begin
   e:=TStringArrayOpExpr(expr);

   noRangeCheck:=(cgoNoRangeChecks in codeGen.Options);

   if noRangeCheck then begin

      codeGen.Compile(e.Left);
      codeGen.WriteString('.char');
      codeGen.WriteString(xxx);
      codeGen.WriteString('At(');
      if e.Right is TConstIntExpr then begin
         codeGen.WriteInteger(TConstIntExpr(e.Right).Value-1);
      end else if     (e.Right is TAddIntExpr)
                  and (TAddIntExpr(e.Right).Right is TConstIntExpr) then begin
         delta:=TConstIntExpr(TAddIntExpr(e.Right).Right).Value-1;
         if delta=0 then
            codeGen.CompileNoWrap(TAddIntExpr(e.Right).Left)
         else begin
            codeGen.Compile(TAddIntExpr(e.Right).Left);
            if delta>0 then
               codeGen.WriteString('+');
            codeGen.WriteInteger(delta);
         end;
      end else begin
         codeGen.Compile(e.Right);
         codeGen.WriteString('-1');
      end;
      codeGen.WriteString(')');

   end else begin

      funcName:='$S'+xxx+'Idx';
      codeGen.Dependencies.Add(funcName);

      codeGen.WriteString(funcName);
      codeGen.WriteString('(');
      codeGen.CompileNoWrap(e.Left);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Right);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSVarStringArraySetExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarStringArraySetExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TVarStringArraySetExpr;
begin
   e:=TVarStringArraySetExpr(expr);

   CodeGenCall(codeGen, e);
   codeGen.WriteString(',');
   codeGen.CompileNoWrap(e.IndexExpr);
   codeGen.WriteString(',');
   CodeGenValue(codeGen, e.ValueExpr);
   if not (cgoNoRangeChecks in codeGen.Options) then begin
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
   end;
   codeGen.WriteStringLn(');');
end;

// CodeGenValue
//
procedure TJSVarStringArraySetExpr.CodeGenValue(codeGen : TdwsCodeGen; valueExpr : TTypedExpr);
begin
   codeGen.CompileNoWrap(valueExpr);
end;

// CodeGenCall
//
procedure TJSVarStringArraySetExpr.CodeGenCall(codeGen : TdwsCodeGen; expr : TStringArraySetExpr);
begin
   codeGen.Dependencies.Add('$StrSet');

   CodeGen.Compile(expr.StringExpr);
   codeGen.WriteString(' = $StrSet(');
   CodeGen.Compile(expr.StringExpr);
end;

// ------------------
// ------------------ TJSVarStringArraySetChrExpr ------------------
// ------------------

// CodeGenValue
//
procedure TJSVarStringArraySetChrExpr.CodeGenValue(codeGen : TdwsCodeGen; valueExpr : TTypedExpr);
begin
   codeGen.WriteString('String.fromCharCode(');
   codeGen.CompileNoWrap(valueExpr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSStringArraySetExpr ------------------
// ------------------

// CodeGenCall
//
procedure TJSStringArraySetExpr.CodeGenCall(codeGen : TdwsCodeGen; expr : TStringArraySetExpr);
begin
   if (expr.StringExpr is TFieldVarExpr) or (expr.StringExpr is TRecordVarExpr) then

      inherited CodeGenCall(codeGen, expr)

   else if expr.StringExpr is TFieldExpr then begin

      codeGen.Dependencies.Add('$StrSetF');

      codeGen.WriteString('$StrSetF(');
      TJSFieldExpr.CodeGenObjectExpr(codeGen, TFieldExpr(expr.StringExpr));
      codeGen.WriteString(',"');
      TJSFieldExpr.CodeGenFieldName(codeGen, TFieldExpr(expr.StringExpr));
      codeGen.WriteString('"');

   end else if expr.StringExpr is TRecordExpr then begin

      codeGen.Dependencies.Add('$StrSetF');

      codeGen.WriteString('$StrSetF(');
      codeGen.Compile(TRecordExpr(expr.StringExpr).BaseExpr);
      codeGen.WriteString(',"');
      TJSRecordExpr.CodeGenFieldName(codeGen, TRecordExpr(expr.StringExpr));
      codeGen.WriteString('"');

   end else if expr.StringExpr is TArrayExpr then begin

      codeGen.Dependencies.Add('$StrSetF');

      codeGen.WriteString('$StrSetF(');
      codeGen.Compile(TArrayExpr(expr.StringExpr).BaseExpr);
      codeGen.WriteString(',');
      codeGen.Compile(TArrayExpr(expr.StringExpr).IndexExpr);

   end else Assert(False, 'Unsupported '+expr.StringExpr.ClassName+' in '+ClassName);
end;

// ------------------
// ------------------ TJSAssertExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssertExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssertExpr;
begin
   e:=TAssertExpr(expr);

   codeGen.Dependencies.Add('$Assert');

   codeGen.WriteString('$Assert(');
   codeGen.CompileNoWrap(e.Cond);
   codeGen.WriteString(',');
   if e.Message<>nil then
      codeGen.CompileNoWrap(e.Message)
   else codeGen.WriteString('""');
   codeGen.WriteString(',');
   WriteLocationString(codeGen, expr);
   codeGen.WriteStringLn(');');
end;

// ------------------
// ------------------ TJSDebugBreakExpr ------------------
// ------------------

// CodeGen
//
procedure TJSDebugBreakExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   codeGen.WriteString('debugger');
   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSForExpr ------------------
// ------------------

// CodeGen
//
procedure TJSForExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   tmpTo, tmpStep : String;
   e : TForExpr;
   nonIncludedEnd : Boolean;
begin
   e:=TForExpr(expr);

   // allocate temporary variables to hold bounds
   // in Pascal bounds and step are evaluated before the loop is entered
   if not e.ToExpr.IsConstant then begin
      tmpTo:=codeGen.GetNewTempSymbol;
      codeGen.WriteString('var ');
      codeGen.WriteString(tmpTo);
      codeGen.WriteStatementEnd;
   end else tmpTo:='';
   if (e is TForStepExpr) and not (TForStepExpr(e).StepExpr.IsConstant) then begin
      tmpStep:=codeGen.GetNewTempSymbol;
      codeGen.WriteString('var ');
      codeGen.WriteString(tmpStep);
      codeGen.WriteStatementEnd;
   end else tmpStep:='';

   // trigger special codegen in case of
   // "for i := whatever to something-1 do"
   nonIncludedEnd:=    (e.ClassType=TForUpwardExpr)
                   and (tmpTo<>'')
                   and (   ((e.ToExpr is TArrayLengthExpr) and (TArrayLengthExpr(e.ToExpr).Delta=-1))
                        or ((e.ToExpr is TSubIntExpr) and ExprIsConstantInteger(TSubIntExpr(e.ToExpr).Right, 1))
                       );

   codeGen.WriteString('for(');

   // initialize loop variable
   codeGen.Compile(e.VarExpr);
   codeGen.WriteString('=');
   codeGen.CompileNoWrap(e.FromExpr);

   // initialize bound end variable
   if tmpTo<>'' then begin
      codeGen.WriteString(',');
      codeGen.WriteString(tmpTo);
      codeGen.WriteString('=');
      if nonIncludedEnd then begin
         if e.ToExpr is TArrayLengthExpr then begin
            codeGen.Compile(TArrayLengthExpr(e.ToExpr).Expr);
            codeGen.WriteString('.length');
         end else begin
            codeGen.Compile(TSubIntExpr(e.ToExpr).Left);
         end;
      end else codeGen.CompileNoWrap(e.ToExpr);
   end;

   // initialize step variable
   if tmpStep<>'' then begin
      codeGen.WriteString(',');
      codeGen.WriteString(tmpStep);
      if cgoNoCheckLoopStep in codeGen.Options then begin
         codeGen.WriteString('=');
         codeGen.CompileNoWrap(TForStepExpr(e).StepExpr);
      end else begin
         codeGen.Dependencies.Add('$CheckStep');
         codeGen.WriteString('=$CheckStep(');
         codeGen.CompileNoWrap(TForStepExpr(e).StepExpr);
         codeGen.WriteString(',');
         WriteLocationString(codeGen, e);
         codeGen.WriteString(')');
      end;
   end;
   codeGen.WriteString(';');

   // comparison sub-expression
   codeGen.Compile(e.VarExpr);
   if nonIncludedEnd then
      codeGen.WriteString('<')
   else WriteCompare(codeGen);
   if tmpTo<>'' then
      codeGen.WriteString(tmpTo)
   else codeGen.Compile(e.ToExpr);
   codeGen.WriteString(';');

   // step sub-expression
   codeGen.Compile(e.VarExpr);
   WriteStep(codeGen);
   if tmpStep<>'' then
      codeGen.WriteString(tmpStep)
   else if e is TForStepExpr then begin
      codeGen.Compile(TForStepExpr(e).StepExpr);
   end;

   // loop block
   codeGen.WriteBlockBegin(') ');
   codeGen.CompileStatement(e.DoExpr);
   codeGen.WriteBlockEndLn;
end;

// ------------------
// ------------------ TJSForUpwardExpr ------------------
// ------------------

// WriteCompare
//
procedure TJSForUpwardExpr.WriteCompare(codeGen : TdwsCodeGen);
begin
   codeGen.WriteString('<=');
end;

// WriteStep
//
procedure TJSForUpwardExpr.WriteStep(codeGen : TdwsCodeGen);
begin
   codeGen.WriteString('++');
end;

// ------------------
// ------------------ TJSForDownwardExpr ------------------
// ------------------

// WriteCompare
//
procedure TJSForDownwardExpr.WriteCompare(codeGen : TdwsCodeGen);
begin
   codeGen.WriteString('>=');
end;

// WriteStep
//
procedure TJSForDownwardExpr.WriteStep(codeGen : TdwsCodeGen);
begin
   codeGen.WriteString('--');
end;

// ------------------
// ------------------ TJSForUpwardStepExpr ------------------
// ------------------

// WriteStep
//
procedure TJSForUpwardStepExpr.WriteStep(codeGen : TdwsCodeGen);
begin
   codeGen.WriteString('+=');
end;

// ------------------
// ------------------ TJSForDownwardStepExpr ------------------
// ------------------

// WriteStep
//
procedure TJSForDownwardStepExpr.WriteStep(codeGen : TdwsCodeGen);
begin
   codeGen.WriteString('-=');
end;

// ------------------
// ------------------ TJSForCharCodeInStrExpr ------------------
// ------------------

// CodeGen
//
procedure TJSForCharCodeInStrExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   tmpLoop, tmpIn : String;
   e : TForCharCodeInStrExpr;
begin
   e:=TForCharCodeInStrExpr(expr);

   codeGen.Dependencies.Add('$uniCharCodeAt');

   // allocate temporary variables to operand and charcode
   if e.InExpr is TVarExpr then
      tmpIn:=''
   else begin
      tmpIn:=codeGen.GetNewTempSymbol;
      codeGen.WriteString('var ');
      codeGen.WriteString(tmpIn);
      codeGen.WriteString(' = ');
      codeGen.CompileNoWrap(e.InExpr);
      codeGen.WriteStatementEnd;
   end;

   tmpLoop:=codeGen.GetNewTempSymbol;

   codeGen.WriteString('for (var ');
   codeGen.WriteString(tmpLoop);
   codeGen.WriteString(' = 0;');
   codeGen.WriteString(tmpLoop);
   codeGen.WriteString('<');
   if tmpIn<>'' then
      codeGen.WriteString(tmpIn)
   else codeGen.Compile(e.InExpr);
   codeGen.WriteString('.length;');
   codeGen.WriteString(tmpLoop);

   codeGen.WriteBlockBegin('++) ');

   codeGen.Compile(e.VarExpr);
   codeGen.WriteString('=$uniCharCodeAt(');
   if tmpIn<>'' then
      codeGen.WriteString(tmpIn)
   else codeGen.Compile(e.InExpr);
   codeGen.WriteString(',');
   codeGen.WriteString(tmpLoop);
   codeGen.WriteString(')');
   codeGen.WriteStatementEnd;

   codeGen.WriteString('if (');
   codeGen.Compile(e.VarExpr);
   codeGen.WriteString('<0) continue');
   codeGen.WriteStatementEnd;

   codeGen.Compile(e.DoExpr);

   codeGen.WriteBlockEndLn;
end;

// ------------------
// ------------------ TJSForCharInStrExpr ------------------
// ------------------

// CodeGen
//
procedure TJSForCharInStrExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   tmpLoop, tmpIn : String;
   e : TForCharInStrExpr;
begin
   e:=TForCharInStrExpr(expr);

   codeGen.Dependencies.Add('$uniCharAt');

   // allocate temporary variables to operand and charcode
   if e.InExpr is TVarExpr then
      tmpIn:=''
   else begin
      tmpIn:=codeGen.GetNewTempSymbol;
      codeGen.WriteString('var ');
      codeGen.WriteString(tmpIn);
      codeGen.WriteString(' = ');
      codeGen.CompileNoWrap(e.InExpr);
      codeGen.WriteStatementEnd;
   end;

   tmpLoop:=codeGen.GetNewTempSymbol;

   codeGen.WriteString('for (var ');
   codeGen.WriteString(tmpLoop);
   codeGen.WriteString('=0;');
   codeGen.WriteString(tmpLoop);
   codeGen.WriteString('<');
   if tmpIn<>'' then
      codeGen.WriteString(tmpIn)
   else codeGen.Compile(e.InExpr);
   codeGen.WriteString('.length;');
   codeGen.WriteString(tmpLoop);

   codeGen.WriteBlockBegin('++) ');

   codeGen.Compile(e.VarExpr);
   codeGen.WriteString('=$uniCharAt(');
   if tmpIn<>'' then
      codeGen.WriteString(tmpIn)
   else codeGen.Compile(e.InExpr);
   codeGen.WriteString(',');
   codeGen.WriteString(tmpLoop);
   codeGen.WriteString(')');
   codeGen.WriteStatementEnd;

   codeGen.WriteString('if (!');
   codeGen.Compile(e.VarExpr);
   codeGen.WriteString(') continue');
   codeGen.WriteStatementEnd;

   codeGen.Compile(e.DoExpr);

   codeGen.WriteBlockEndLn;
end;

// ------------------
// ------------------ TJSSqrExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSSqrExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   expr:=expr.SubExpr[0] as TTypedExpr;
   CodeGenSqr(codeGen, expr);
end;

// CodeGenSqr
//
class procedure TJSSqrExpr.CodeGenSqr(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   if expr is TVarExpr then begin
      codeGen.Compile(expr);
      codeGen.WriteString('*');
      codeGen.Compile(expr);
   end else begin
      codeGen.WriteString('Math.pow(');
      codeGen.CompileNoWrap(expr);
      codeGen.WriteString(',2)');
   end;
end;

// ------------------
// ------------------ TJSOpExpr ------------------
// ------------------

// WriteWrappedIfNeeded
//
class procedure TJSOpExpr.WriteWrappedIfNeeded(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   if    (expr is TDataExpr)
      or (expr is TConstExpr) then begin
      codeGen.CompileNoWrap(expr);
   end else begin
      codeGen.Compile(expr);
   end;
end;

// ------------------
// ------------------ TJSBinOpExpr ------------------
// ------------------

// Create
//
constructor TJSBinOpExpr.Create(const op : String; aPrecedence : Integer; associative : TJBinOpAssociativities);
begin
   inherited Create;
   FOp:=op;
   FPrecedence:=aPrecedence;
   FAssociative:=associative;
end;

// CodeGen
//
procedure TJSBinOpExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TBinaryOpExpr;
   p : Integer;
begin
   e:=TBinaryOpExpr(expr);

   p:=ExprJSPrecedence(e.Left);
   if (p>FPrecedence) or ((p=FPrecedence) and (associativeLeft in FAssociative)) then
      codeGen.CompileNoWrap(e.Left)
   else WriteWrappedIfNeeded(codeGen, e.Left);

   if WriteOp(codeGen, e.Right) then Exit;

   p:=ExprJSPrecedence(e.Right);
   if (p>FPrecedence) or ((p=FPrecedence) and (associativeRight in FAssociative)) then
      codeGen.CompileNoWrap(e.Right)
   else WriteWrappedIfNeeded(codeGen, e.Right);
end;

// WriteOp
//
function TJSBinOpExpr.WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean;
begin
   codeGen.WriteString(FOp);
   Result:=False;
end;

// ExprJSPrecedence
//
function TJSBinOpExpr.ExprJSPrecedence(expr : TTypedExpr) : Integer;
var
   ebc : TExprBaseClass;
   cg : TdwsExprCodeGen;
begin
   ebc:=TExprBaseClass(expr.ClassType);
   if (ebc=TSqrIntExpr) or (ebc=TSqrFloatExpr) then
      Result:=14
   else begin
      cg:=Owner.FindCodeGen(ebc);
      if cg is TJSBinOpExpr then
         Result:=TJSBinOpExpr(cg).FPrecedence
      else Result:=0;
   end;
end;

// ------------------
// ------------------ TJSAddOpExpr ------------------
// ------------------

// Create
//
constructor TJSAddOpExpr.Create;
begin
   inherited Create('+', 13, [associativeLeft, associativeRight]);
end;

// WriteOp
//
function TJSAddOpExpr.WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean;
var
   v : Variant;
begin
   Result:=False;
   if rightExpr is TConstExpr then begin
      rightExpr.EvalAsVariant(nil, v);
      // right operand will write a minus
      if v<0 then Exit;
   end;
   codeGen.WriteString(FOp);
end;

// ------------------
// ------------------ TJSSubOpExpr ------------------
// ------------------

// Create
//
constructor TJSSubOpExpr.Create;
begin
   inherited Create('-', 13, [associativeLeft]);
end;

// WriteOp
//
function TJSSubOpExpr.WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean;
begin
   if (rightExpr is TConstExpr) and (rightExpr.EvalAsFloat(nil)<0) then begin
      // right operand would write a minus
      codeGen.WriteString('+');
      if rightExpr is TConstIntExpr then
         codeGen.WriteInteger(-TConstIntExpr(rightExpr).Value)
      else codeGen.WriteFloat(-rightExpr.EvalAsFloat(nil), cFormatSettings);
      Result:=True;
   end else begin
      codeGen.WriteString(FOp);
      Result:=False;
   end;
end;

// ------------------
// ------------------ TJSMultOpExpr ------------------
// ------------------

// Create
//
constructor TJSMultOpExpr.Create;
begin
   inherited Create('*', 14, [associativeLeft, associativeRight]);
end;

// ------------------
// ------------------ TJSDeclaredExpr ------------------
// ------------------

// CodeGen
//
procedure TJSDeclaredExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TDeclaredExpr;
   name : String;
   sym : TSymbol;
begin
   e:=TDeclaredExpr(expr);
   if not (e.Expr is TConstExpr) then
      raise ECodeGenUnknownExpression.Create('Declared Expr with non-constant parameter');
   e.Expr.EvalAsString(nil, name);
   sym:=TDeclaredExpr.FindSymbol(codeGen.Context.Table, name);
   codeGen.WriteString(cBoolToJSBool[sym<>nil]);
end;

// ------------------
// ------------------ TJSConditionalDefinedExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConditionalDefinedExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TDefinedExpr;
begin
   e:=TDefinedExpr(expr);
   codeGen.Dependencies.Add('$ConditionalDefines');
   codeGen.WriteString('($ConditionalDefines.indexOf(');
   codeGen.CompileNoWrap(e.Expr);
   codeGen.WriteString(')!=-1)');
end;

// ------------------
// ------------------ TJSSwapExpr ------------------
// ------------------

// CodeGen
//
procedure TJSSwapExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TSwapExpr;
   argTyp : TTypeSymbol;
   ct : TClass;
   tmp : String;
begin
   e:=TSwapExpr(expr);

   tmp:=codeGen.GetNewTempSymbol;

   codeGen.WriteString('var ');
   codeGen.WriteString(tmp);

   argTyp:=e.Arg0.Typ;
   ct:=argTyp.ClassType;
   if (ct=TRecordSymbol) and not TRecordSymbol(argTyp).IsImmutable then begin

      codeGen.WriteString(' = {}; Copy$');
      codeGen.WriteSymbolName(argTyp);
      codeGen.WriteString('(');
      codeGen.CompileNoWrap(e.Arg0);
      codeGen.WriteString(',');
      codeGen.WriteString(tmp);
      codeGen.WriteString('); Copy$');
      codeGen.WriteSymbolName(argTyp);
      codeGen.WriteString('(');
      codeGen.CompileNoWrap(e.Arg1);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Arg0);
      codeGen.WriteString('); Copy$');
      codeGen.WriteSymbolName(argTyp);
      codeGen.WriteString('(');
      codeGen.WriteString(tmp);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Arg1);
      codeGen.WriteString(')');
      codeGen.WriteStatementEnd;

   end else if ct.InheritsFrom(TStaticArraySymbol) then begin

      codeGen.WriteString(' = ');
      codeGen.CompileNoWrap(e.Arg0);
      codeGen.WriteString('.slice(0); ');

      codeGen.CompileNoWrap(e.Arg0);
      codeGen.WriteString(' = ');
      codeGen.CompileNoWrap(e.Arg1);
      codeGen.WriteString('.slice(0); ');

      codeGen.CompileNoWrap(e.Arg1);
      codeGen.WriteString(' = ');
      codeGen.WriteString(tmp);
      codeGen.WriteString('.slice(0)');
      codeGen.WriteStatementEnd;

   end else begin

      codeGen.WriteString(' = ');
      codeGen.CompileNoWrap(e.Arg0);
      codeGen.WriteString('; ');

      codeGen.CompileNoWrap(e.Arg0);
      codeGen.WriteString(' = ');
      codeGen.CompileNoWrap(e.Arg1);
      codeGen.WriteString('; ');

      codeGen.CompileNoWrap(e.Arg1);
      codeGen.WriteString(' = ');
      codeGen.WriteString(tmp);
      codeGen.WriteStatementEnd;
   end;

end;

// ------------------
// ------------------ TdwsCodeGenSymbolMapJSObfuscating ------------------
// ------------------

// DoNeedUniqueName
//
function TdwsCodeGenSymbolMapJSObfuscating.DoNeedUniqueName(symbol : TSymbol; tryCount : Integer; canObfuscate : Boolean) : String;
var
   h : Integer;
begin
   if not canObfuscate then
      Exit(inherited DoNeedUniqueName(symbol, tryCount, canObfuscate));
   h:=Random(MaxInt);
   case tryCount of
      0..4 : h:=h mod 52;
      5..15 : h:=h mod (52*62);
      16..30 : h:=h mod (52*62*62);
   else
      h:=h and $7FFFF;
   end;
   Result:=Prefix+IntToSkewedBase62(h);
end;

// ------------------
// ------------------ TJSDivExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSDivExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TDivExpr;
begin
   e:=TDivExpr(expr);

   codeGen.Dependencies.Add('$Div');

   codeGen.WriteString('$Div(');
   codeGen.CompileNoWrap(e.Left);
   codeGen.WriteString(',');
   codeGen.CompileNoWrap(e.Right);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSMultIntPow2Expr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSMultIntPow2Expr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TMultIntPow2Expr;
begin
   e:=TMultIntPow2Expr(expr);

   codeGen.Compile(e.Expr);
   codeGen.WriteString('*');
   codeGen.WriteInteger(Int64(2) shl e.Shift);
end;

// ------------------
// ------------------ TJSCoalesceExpr ------------------
// ------------------

// Create
//
constructor TJSCoalesceExpr.Create;
begin
   inherited Create('||', 5, [associativeLeft, associativeRight]);
end;

// ------------------
// ------------------ TJSSetOfExpr ------------------
// ------------------

// CodeGenFunc
//
class procedure TJSSetOfExpr.CodeGenFunc(codeGen : TdwsCodeGen; const funcName : String;
                                           base, operand : TExprBase; setType : TSetOfSymbol);
begin
   codeGen.Dependencies.Add(funcName);

   codeGen.WriteString(funcName);
   codeGen.WriteString('(');
   codeGen.Compile(base);
   codeGen.WriteString(',');
   codeGen.Compile(operand);
   codeGen.WriteString(',');
   codeGen.WriteInteger(setType.MinValue);
   codeGen.WriteString(',');
   codeGen.WriteInteger(setType.CountValue);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSSetOfFunctionExpr ------------------
// ------------------

// CodeGen
//
procedure TJSSetOfFunctionExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TSetOfFunctionExpr;
   funcName : String;
begin
   e:=TSetOfFunctionExpr(expr);

   if e is TSetOfIncludeExpr then
      funcName:='$SetInc'
   else funcName:='$SetExc';

   CodeGenFunc(codeGen, funcName, e.BaseExpr, e.Operand, e.SetType);
   codeGen.WriteStatementEnd;
end;

// ------------------
// ------------------ TJSSetOfInExpr ------------------
// ------------------

// CodeGen
//
procedure TJSSetOfInExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TSetOfInExpr;
begin
   e:=TSetOfInExpr(expr);

   CodeGenFunc(codeGen, '$SetIn', e.Right, e.Left, e.SetType);
end;

// ------------------
// ------------------ TJSConvStaticArrayToSetOfExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConvStaticArrayToSetOfExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConvStaticArrayToSetOfExpr;
   data : TData;
   dc : IDataContext;
begin
   e:=TConvStaticArrayToSetOfExpr(expr);

   e.EvalAsTData(nil, data);
   (codeGen as TdwsJSCodeGen).CreateDataContext(data, 0, dc);
   (codeGen as TdwsJSCodeGen).WriteValue(e.Typ, dc);
end;

// ------------------
// ------------------ TJSConvSetOfToIntegerExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConvSetOfToIntegerExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConvSetOfToIntegerExpr;
begin
   e:=TConvSetOfToIntegerExpr(expr);

   codeGen.CompileNoWrap(e.Expr);
   codeGen.WriteString('[0]');
end;

// ------------------
// ------------------ TJSConvIntegerToSetOfExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConvIntegerToSetOfExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConvIntegerToSetOfExpr;
begin
   e:=TConvIntegerToSetOfExpr(expr);

   codeGen.WriteString('[');
   codeGen.CompileNoWrap(e.Expr);
   codeGen.WriteString(']');
end;

// ------------------
// ------------------ TJSEnumerationElementNameExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSEnumerationElementNameExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TEnumerationElementNameExpr;
begin
   e:=TEnumerationElementNameExpr(expr);

   codeGen.Dependencies.Add('$EnumToName');

   codeGen.WriteString('$EnumToName(');
   codeGen.WriteSymbolName(e.Expr.Typ);
   codeGen.WriteString(',');
   codeGen.CompileNoWrap(e.Expr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSEnumerationElementQualifiedNameExpr ------------------
// ------------------

// CodeGenNoWrap
//
procedure TJSEnumerationElementQualifiedNameExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   codeGen.WriteString('(');
   codeGen.WriteLiteralString(TEnumerationElementQualifiedNameExpr(expr).Expr.Typ.UnAliasedType.Name+'.');
   codeGen.WriteString('+');
   inherited;
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TdwsJSCodeGenEnvironment ------------------
// ------------------

// Create
//
constructor TdwsJSCodeGenEnvironment.Create(codeGen : TdwsJSCodeGen; const customExec : IdwsProgramExecution);
begin
   FExec:=customExec;
   FCodeGen:=codeGen;
   FIntercepts:=TSimpleNameObjectHash<TdwsJSCodeGenIntercepts>.Create;
   FCodeGen.OnCustomCodeGen:=DoCustomCodeGen;
   FMissedIntercepts:=TSimpleClassHash.Create;
end;

// Destroy
//
destructor TdwsJSCodeGenEnvironment.Destroy;
begin
   FCodeGen.OnCustomCodeGen:=nil;
   FIntercepts.Clean;
   FIntercepts.Free;
   FMissedIntercepts.Free;
   inherited;
end;

// RegisterIntercept
//
procedure TdwsJSCodeGenEnvironment.RegisterIntercept(const exprClassName, qualifiedName : String;
                                                     const callBack : IInfo);
var
   intercept : TdwsJSCodeGenIntercept;
   intercepts : TdwsJSCodeGenIntercepts;
begin
   intercepts:=FIntercepts[exprClassName];
   if intercepts=nil then begin
      intercepts:=TdwsJSCodeGenIntercepts.Create;
      FIntercepts[exprClassName]:=intercepts;
   end;

   intercept:=TdwsJSCodeGenIntercept.Create;
   intercept.Environment:=Self;
   intercept.CallBack:=IFuncPointer(IUnknown(callBack.Value)).GetFuncExpr.FuncSym;
   intercepts[qualifiedName]:=intercept;

   if FMissedIntercepts.Count>0 then
      FMissedIntercepts.Clear;
end;

// DoCustomCodeGen
//
function TdwsJSCodeGenEnvironment.DoCustomCodeGen(expr : TExprBase) : TdwsExprCodeGen;

   function TryHarder(env : TdwsJSCodeGenEnvironment; expr : TExprBase) : TdwsJSCodeGenIntercepts;
   begin
      Result:=env.FIntercepts[expr.ClassName];
      if Result=nil then
         env.FMissedIntercepts.Add(expr.ClassType);
   end;

   function AllConstant(expr : TExprBase) : Boolean;
   var
      i : Integer;
   begin
      for i:=0 to expr.SubExprCount-1 do
         if not expr.SubExpr[i].IsConstant then
            Exit(False);
      Result:=True;
   end;

var
   intercepts : TdwsJSCodeGenIntercepts;
begin
   Result:=nil;
   if FMissedIntercepts.Contains(expr.ClassType) then
      Exit;

   intercepts:=TryHarder(Self, expr);
   if intercepts<>nil then begin

      if AllConstant(expr) then
         Result:=intercepts.Match(expr);

   end;
end;

// ------------------
// ------------------ TdwsJSCodeGenIntercept ------------------
// ------------------

// CodeGen
//
procedure TdwsJSCodeGenIntercept.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   info : IInfo;
   s : String;
   dynArray : TScriptDynamicArray;
   exec : TdwsExecution;
begin
   info := Environment.Exec.Info.FuncBySym[CallBack];

   info.Parameter['exprClass'].Value := expr.ClassName;
   info.Parameter['qualifiedName'].Value := expr.FuncSymQualifiedName;

   dynArray := TScriptDynamicArray.CreateNew(Environment.CodeGen.Context.TypVariant);
   dynArray.ArrayLength := expr.SubExprCount;

   exec := Environment.Exec.ExecutionObject;

   for i := 0 to expr.SubExprCount-1 do
      expr.SubExpr[i].EvalAsVariant(exec, dynArray.AsPVariant(i)^);

   info.Parameter['params'].Value := (dynArray as IScriptDynArray);

   try
      s:=info.Call.ValueAsString;

      codeGen.WriteString(s);
   except
      on E: Exception do begin
         codeGen.Context.CompileMsgs.AddCompilerErrorFmt(
            expr.ScriptPos, 'CodeGen: %s', [e.Message]);
      end;
   end;
end;

// ------------------
// ------------------ TdwsJSCodeGenIntercepts ------------------
// ------------------

// Destroy
//
destructor TdwsJSCodeGenIntercepts.Destroy;
begin
   Clean;
   inherited;
end;

// Match
//
function TdwsJSCodeGenIntercepts.Match(expr : TExprBase) : TdwsJSCodeGenIntercept;
begin
   Result:=Objects[expr.FuncSymQualifiedName];
end;

// ------------------
// ------------------ TSimpleClassHash ------------------
// ------------------

// SameItem
//
function TSimpleClassHash.SameItem(const item1, item2 : TClass) : Boolean;
begin
   Result:=(item1=item2);
end;

// GetItemHashCode
//
function TSimpleClassHash.GetItemHashCode(const item1 : TClass) : Integer;
begin
   Result := NativeInt(item1) * 16777619;
end;

end.
