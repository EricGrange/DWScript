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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsJSCodeGen;

interface

uses Classes, SysUtils, dwsUtils, dwsSymbols, dwsCodeGen, dwsCoreExprs,
   dwsExprs, dwsRelExprs, dwsJSON, dwsMagicExprs, dwsStack, Variants, dwsStrings,
   dwsJSLibModule;

type

   TDataSymbolList = class(TObjectList<TDataSymbol>)
      public
         destructor Destroy; override;
   end;

   TdwsJSCodeGen = class (TdwsCodeGen)
      private
         FLocalVarParams : TDataSymbolList;
         FLocalVarParamsStack : TSimpleStack<TDataSymbolList>;
         FDeclaredLocalVars : TDataSymbolList;
         FDeclaredLocalVarsStack : TSimpleStack<TDataSymbolList>;
         FMainBodyName : String;

      protected
         procedure CollectLocalVarParams(expr : TExprBase);
         procedure CollectInitExprLocalVars(initExpr : TBlockExprBase);

         procedure EnterContext(proc : TdwsProgram); override;
         procedure LeaveContext; override;

         procedure WriteDefaultValue(typ : TTypeSymbol; box : Boolean);
         procedure WriteValue(typ : TTypeSymbol; const data : TData; addr : Integer);
         procedure WriteStringArray(destStream : TWriteOnlyBlockStream; strings : TStrings); overload;
         procedure WriteStringArray(strings : TStrings); overload;

         procedure WriteFuncParams(func : TFuncSymbol);
         procedure CompileFuncBody(func : TFuncSymbol);
         procedure CompileMethod(meth : TMethodSymbol);

         procedure DoCompileClassSymbol(cls : TClassSymbol); override;
         procedure DoCompileInterfaceTable(cls : TClassSymbol);
         procedure DoCompileFuncSymbol(func : TSourceFuncSymbol); override;

      public
         constructor Create; override;
         destructor Destroy; override;

         function  SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : String; override;

         procedure CompileEnumerationSymbol(enum : TEnumerationSymbol); override;
         procedure CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                     preConds : Boolean); override;
         procedure CompileRecordSymbol(rec : TRecordSymbol); override;
         procedure CompileProgramBody(expr : TNoResultExpr); override;
         procedure CompileSymbolTable(table : TSymbolTable); override;

         procedure ReserveSymbolNames; override;

         procedure CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); override;

         function GetNewTempSymbol : String; override;

         procedure WriteJavaScriptString(const s : String);

         function MemberName(sym : TSymbol; cls : TStructuredTypeSymbol) : String;

         // returns all the RTL support JS functions
         class function All_RTL_JS : String;
         // removes all RTL dependencies (use in combination with All_RTL_JS)
         procedure IgnoreRTLDependencies;

         property MainBodyName : String read FMainBodyName write FMainBodyName;
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

   TJSExitExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssignExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignDataExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignClassOfExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignFuncExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSInitDataExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssignConstToIntegerVarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignConstToFloatVarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignConstToBoolVarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignConstToStringVarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignNilToVarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignConstDataToVarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAppendConstStringVarExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConstExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstIntExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstStringExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstFloatExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConstBooleanExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayConstantExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSVarExpr = class (TJSExprCodeGen)
      class function CodeGenSymbol(codeGen : TdwsCodeGen; expr : TExprBase) : TDataSymbol; static;
      class function CodeGenName(codeGen : TdwsCodeGen; expr : TExprBase) : TDataSymbol; static;
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSVarParentExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSVarParamExpr = class (TJSVarExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSVarParamParentExpr = class (TJSVarParentExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSLazyParamExpr = class (TJSVarExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSRecordExpr = class (TJSVarExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSFieldExpr = class (TJSVarExpr)
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
   end;
   TJSArrayDeleteExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArrayCopyExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSArraySwapExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSStaticArrayExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSDynamicArrayExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSDynamicArraySetExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSStringArrayOpExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSVarStringArraySetExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSInOpExpr = class (TJSExprCodeGen)
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

   TJSConvClassExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConvIntegerExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSIncVarFuncExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSDecVarFuncExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSFuncBaseExpr = class (TJSExprCodeGen)
      private
         FVirtualCall : Boolean;
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); virtual;
         procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); virtual;
   end;

   TJSMagicFuncExpr = class (TJSFuncBaseExpr)
      private
         FMagicCodeGens : TStringList;
      public
         constructor Create;
         destructor Destroy; override;
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSMethodStaticExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;
   TJSMethodVirtualExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;

   TJSMethodInterfaceExpr = class (TJSFuncBaseExpr)
      procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); override;
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
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
   end;
   TJSConstructorVirtualExpr = class (TJSFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); override;
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

   TJSExceptExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssertExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSDeclaredExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSDefinedExpr = class (TJSExprCodeGen)
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

   TJSSqrExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSOpExpr = class (TJSExprCodeGen)
      class procedure WriteWrappedIfNeeded(codeGen : TdwsCodeGen; expr : TTypedExpr); static;
   end;
   TJSBinOpExpr = class (TJSOpExpr)
      protected
         FOp : String;
         FAssociative : Boolean;
      public
         constructor Create(const op : String; associative : Boolean);
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R dwsJSRTL.res dwsJSRTL.rc}

const
   cBoolToJSBool : array [False..True] of String = ('false', 'true');
   cFormatSettings : TFormatSettings = ( DecimalSeparator : '.' );

type
   TJSRTLDependency = record
      Name, Code, Dependency : String;
   end;
   PJSRTLDependency = ^TJSRTLDependency;
const
   cJSRTLDependencies : array [1..124] of TJSRTLDependency = (
      // codegen utility functions
      (Name : '$CheckStep';
       Code : 'function $CheckStep(s,z) { if (s>0) return s; throw Exception.Create$1($New(Exception),"FOR loop STEP should be strictly positive: "+s.toString()+z); }';
       Dependency : 'Exception' ),
      (Name : '$dwsRand';
       Code : 'var $dwsRand=0'),
      (Name : '$New';
       Code : 'function $New(c) { var i={ClassType:c}; c.$Init(i); return i }'),
      (Name : '$NewDyn';
       Code : 'function $NewDyn(c,z) {'#13#10
              +#9'if (c==null) throw Exception.Create$1($New(Exception),"ClassType is nil"+z);'#13#10
              +#9'var i={ClassType:c};'#13#10
              +#9'c.$Init(i);'#13#10
              +#9'return i'#13#10
              +'}';
       Dependency : 'Exception' ),
      (Name : '$NewArray';
       Code : 'function $NewArray(n,d) { var r=new Array(n); for(var i=0;i<n;i++) r[i]=d(); return r }'),
      (Name : '$ArraySetLength';
       Code : 'function $ArraySetLength(a,n,d) {'#13#10
              +#9'var o=a.length;'#13#10
              +#9'if (o==n) return;'#13#10
              +#9'if (o>n) { a.splice(n, o-n); return }'#13#10
              +#9'for (;o<n;o++) a.push(d());'#13#10
              +'}'),
      (Name : '$ArrayCopy';
       Code : 'function $ArrayCopy(a,i,z) { return a.slice($Idx(i,0,a.length-1,z)) }';
       Dependency : '$Idx' ),
      (Name : '$ArrayCopyLen';
       Code : 'function $ArrayCopyLen(a,i,l,z) {'#13#10
              +#9'if (l<0) throw Exception.Create$1($New(Exception),"Positive count expected (got "+l.toString()+")"+z);'#13#10
              +#9'return a.slice($Idx(i,0,a.length-1,z),$Idx(i+l-1,0,a.length-1,z)-i+1,z)'#13#10
              +'}';
       Dependency : '$Idx' ),
      (Name : '$ArraySwap';
       Code : 'function $ArraySwap(a,i1,i2) { var t=a[i1]; a[i1]=a[i2]; a[i2]=t }' ),
      (Name : '$ArraySwapChk';
       Code : 'function $ArraySwapChk(a,i1,i2,z) {'#13#10
              +#9'var n=a.length-1;'#13#10
              +#9'var t=a[$Idx(i1,0,n,z)];'#13#10
              +#9'a[i1]=a[$Idx(i2,0,n,z)]'#13#10
              +#9'a[i2]=t;'#13#10
              +'}';
       Dependency : '$Idx' ),
      (Name : '$Check';
       Code : 'function $Check(i,z) { if (i) return i; throw Exception.Create$1($New(Exception),"Object not instantiated"+z); }'),
      (Name : '$Assert';
       Code : 'function $Assert(b,m,z) { if (!b) throw Exception.Create$1($New(EAssertionFailed),"Assertion failed"+z+((m=="")?"":" : ")+m); }';
       Dependency : 'EAssertionFailed' ),
      (Name : '$CondFailed';
       Code : 'function $CondFailed(z,m) { throw Exception.Create$1($New(EAssertionFailed),z+m); }';
       Dependency : 'EAssertionFailed' ),
      (Name : '$Inc';
       Code : 'function $Inc(v,i) { v.value+=i; return v.value }'),
      (Name : '$Dec';
       Code : 'function $Dec(v,i) { v.value-=i; return v.value }'),
      (Name : '$Is';
       Code : 'function $Is(o,c) {'#13#10
               +#9'if (o===null) return false; var ct=o.ClassType;'#13#10
               +#9'while ((ct)&&(ct!==c)) ct=ct.$Parent;'#13#10
               +#9'return (ct)?true:false;'#13#10
               +'}'#13#10),
      (Name : '$Cast';
       Code : 'function $Cast(o,c) { if (o===null) return o; else return $As(o,c); }';
       Dependency : '$As' ),
      (Name : '$As';
       Code : 'function $As(o,c) { if ((o!==null)&&$Is(o,c)) return o; '#13#10
               +'else throw Exception.Create$1($New(Exception),"Can''t cast instance of type \""+o.ClassType.$ClassName+"\" to class \""+c.$ClassName+"\""); }';
       Dependency : '$Is' ),
      (Name : '$AsIntf';
       Code : 'function $AsIntf(o,i) {'#13#10
               +#9'if (o==null) return null;'#13#10
               +#9'return {O:o, I:o.ClassType.$Intf[i]};'#13#10
               +'};'#13#10),
      (Name : '$Implements';
       Code : 'function $Implements(o,i) {'#13#10
               +#9'if (o==null) return false;'#13#10
               +#9'var cti=o.ClassType.$Intf;'#13#10
               +#9'return ((cti!=undefined)&&(cti[i]!=undefined));'#13#10
               +'}'#13#10),
      (Name : '$ClassImplements';
       Code : 'function $ClassImplements(c,i) {'#13#10
               +#9'if (c==null) return false;'#13#10
               +#9'var cti=c.$Intf;'#13#10
               +#9'return ((cti!=undefined)&&(cti[i]!=undefined));'#13#10
               +'}'#13#10),
      (Name : '$IntfAsClass';
       Code : 'function $IntfAsClass(i,c) {'#13#10
               +#9'if (i==null) return null;'#13#10
               +#9'if ($Is(i.O,c)) return i.O;'#13#10
               +#9'else throw Exception.Create$1($New(Exception),"Can''t cast interface of \""+i.O.ClassType.$ClassName+"\" to class \""+c.$ClassName+"\"");'#13#10
               +'}'#13#10;
       Dependency : '$Is' ),
      (Name : '$Idx';
       Code : 'function $Idx(i,l,h,z) {'#13#10
               +#9'if (i<l) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>h) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return i-l;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxR';
       Code : 'function $DIdxR(a,i,z) {'#13#10
               +#9'if (i<0) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>a.length) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return a[i];'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxW';
       Code : 'function $DIdxW(a,i,v,z) {'#13#10
               +#9'if (i<0) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>a.length) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'a[i]=v;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$SIdx';
       Code : 'function $SIdx(s,i,z) {'#13#10
               +#9'if (i<1) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>s.length) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return s.charAt(i-1);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$StrSet';
       Code : 'function $StrSet(s,i,v,z) {'#13#10
               +#9'if (i<1) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>s.length) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return s.substring(0,i-1)+v+s.substring(i);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$Event';
       Code : 'function $Event(i,f) {'#13#10
               +#9'var li=i,lf=f;'#13#10
               +#9'return function(){'#13#10
                  +#9#9'var arg=Array.prototype.slice.call(arguments);'#13#10
                  +#9#9'arg.splice(0,0,li);'#13#10
                  +#9#9'return lf.apply(li,arg)'#13#10
               +#9'}'#13#10
               +'}'),
      (Name : '$Intf';
       Code : 'function $Intf(i,n) {'#13#10
               +#9'var arg=Array.prototype.slice.call(arguments);'#13#10
               +#9'arg.splice(0,2,i.O);'#13#10
               +#9'return i.I[n].apply(i.O, arg);'#13#10
               +'}'),
      // RTL functions
      (Name : 'Abs';
       Code : 'function Abs(v) { return Math.abs(v) }'),
      (Name : 'AnsiCompareStr';
       Code : 'function AnsiCompareStr(a,b) { return a.localeCompare(b) }'),
      (Name : 'AnsiCompareText';
       Code : 'function AnsiCompareText(a,b) { return AnsiCompareStr(a.toLocaleUpperCase(), b.toLocaleUpperCase()) }';
       Dependency : 'AnsiCompareStr'),
      (Name : 'AnsiLowerCase';
       Code : 'function AnsiLowerCase(v) { return v.toLocaleLowerCase() }'),
      (Name : 'AnsiUpperCase';
       Code : 'function AnsiUpperCase(v) { return v.toLocaleUpperCase() }'),
      (Name : 'ArcCos';
       Code : 'function ArcCos(v) { return Math.acos(v) }'),
      (Name : 'ArcCosh';
       Code : 'function ArcCosh(v) { return Math.log(v+Math.sqrt((v-1)/(v+1))*(v+1)) }'),
      (Name : 'ArcSin';
       Code : 'function ArcSin(v) { return Math.asin(v) }'),
      (Name : 'ArcSinh';
       Code : 'function ArcSinh(v) { return Math.log(v+Math.sqrt(v*v+1)) }'),
      (Name : 'ArcTan';
       Code : 'function ArcTan(v) { return Math.atan(v) }'),
      (Name : 'ArcTan2';
       Code : 'function ArcTan2(y,x) { return Math.atan2(y,x) }'),
      (Name : 'ArcTanh';
       Code : 'function ArcTanh(v) { return 0.5*Math.log((1+v)/(1-v)) }'),
      (Name : 'BoolToStr';
       Code : 'function BoolToStr(b) { return b?"True":"False" }'),
      (Name : 'Ceil';
       Code : 'function Ceil(v) { return Math.ceil(v) }'),
      (Name : 'CharAt';
       Code : 'function CharAt(s,p) { return s.charAt(p-1) }'),
      (Name : 'Chr';
       Code : 'function Chr(c) {'#13#10
              +#9'if (c<=0xFFFF)'#13#10
                 +#9#9'return String.fromCharCode(c);'#13#10
              +#9'c-=0x10000;'#13#10
              +#9'return String.fromCharCode(0xD800+(c>>10))+String.fromCharCode(0xDC00+(c&0x3FF));'#13#10
              +'}'),
      (Name : 'ClampInt';
       Code : 'function ClampInt(v,mi,ma) { if (v<mi) return mi; else if (v>ma) return ma; else return v }'),
      (Name : 'CompareStr';
       Code : 'function CompareStr(a,b) { if (a<b) return -1; else return (a==b)?0:1 }'),
      (Name : 'CompareText';
       Code : 'function CompareText(a,b) { return CompareStr(a.toUpperCase(), b.toUpperCase()) }';
       Dependency : 'CompareStr'),
      (Name : 'Copy';
       Code : 'function Copy(s,f,n) { return s.substr(f-1,n) }'),
      (Name : 'Cos';
       Code : 'function Cos(v) { return Math.cos(v) }'),
      (Name : 'Cosh';
       Code : 'function Cosh(v) { return (v==0)?1:0.5*(Math.exp(v)+Math.exp(-v)) }'),
      (Name : 'Cotan';
       Code : 'function Cotan(v) { return 1/Math.tan(v) }'),
      (Name : 'DegToRad';
       Code : 'function DegToRad(v) { return v*(Math.PI/180) }'),
      (Name : 'Delete';
       Code : 'function Delete(s,i,n) { var v=s.value; if ((i<=0)||(i>v.length)||(n<=0)) return; s.value=v.substr(0,i-1)+v.substr(i+n-1); }'),
      (Name : 'EncodeDate';
       Code : 'function EncodeDate(y,m,d) { return (new Date(y,m,d)).getTime()/864e5+25569 }'),
      (Name : 'Even';
       Code : 'function Even(v) { return (v&1)==0 }'),
      (Name : 'Exp';
       Code : 'function Exp(v) { return Math.exp(v) }'),
      (Name : 'Factorial';
       Code : 'function Factorial(i) { var r=1; while (i>1) { r*=i; i--; } return r }'),
      (Name : 'FloatToStr';
       Code : 'function FloatToStr(i,p) { return (p==99)?i.toString():i.toFixed(p) }'),
      (Name : 'Floor';
       Code : 'function Floor(v) { return Math.floor(v) }'),
      (Name : 'Format';
       Code : 'function Format(f,a) { a.unshift(f); return sprintf.apply(null,a) }';
       Dependency : '!sprintf_js'),
      (Name : 'FormatDateTime';
       Code : 'function FormatDateTime(f,a) { return formatDateTime(f,new Date((a-25569)*864e5)) }';
       Dependency : '!formatDateTime_js'),
      (Name : 'Frac';
       Code : 'function Frac(v) { return v-((v>0)?Math.floor(v):Math.ceil(v)) }'),
      (Name : 'HexToInt';
       Code : 'function HexToInt(v) { return parseInt(v,16) }'),
      (Name : 'Hypot';
       Code : 'function Hypot(x,y) { return Math.sqrt(x*x+y*y) }'),
      (Name : 'Insert';
       Code : 'function Insert(s,d,i) { var v=d.value; if (s=="") return; if (i<1) i=1; if (i>v.length) i=v.length+1;'
               +'d.value=v.substr(0,i-1)+s+v.substr(i-1); }'),
      (Name : 'Int';
       Code : 'function Int(v) { return (v>0)?Math.floor(v):Math.ceil(v) }'),
      (Name : 'IntPower';
       Code : 'function IntPower(x,y) { return Math.pow(x,y) }'),
      (Name : 'IntToBin';
       Code : 'function IntToBin(v,d) { var r=v.toString(2).toUpperCase(); while (r.length<d) r="0"+r; return r }'),
      (Name : 'IntToHex';
       Code : 'function IntToHex(v,d) { var hex=v.toString(16).toUpperCase(); return "00000000".substr(0, 8-d-hex.length)+hex }'),
      (Name : 'IntToStr';
       Code : 'function IntToStr(i) { return i.toString() }'),
      (Name : 'IsDelimiter';
       Code : 'function IsDelimiter(d,s,i) { if ((i<=0)||(i>s.length)) return false; else return d.indexOf(s.charAt(i-1))>=0; }'),
      (Name : 'Ln';
       Code : 'function Ln(v) { return Math.log(v) }'),
      (Name : 'LastDelimiter';
       Code : 'function LastDelimiter(d,s) { var r=-1,n=d.length,i,p; for (i=0;i<n;i++) { p=s.lastIndexOf(d.charAt(i)); if (p>r) r=p; } return r+1;}'),
      (Name : 'LeftStr';
       Code : 'function LeftStr(s,n) { return s.substr(0,n) }'),
      (Name : 'Log10';
       Code : 'function Log10(x) { return Math.log(x)/Math.LN10 }'),
      (Name : 'Log2';
       Code : 'function Log2(x) { return Math.log(x)/Math.LN2 }'),
      (Name : 'LogN';
       Code : 'function LogN(n,x) { return Math.log(x)/Math.log(n) }'),
      (Name : 'LowerCase';
       Code : 'function LowerCase(v) { return v.toLowerCase() }'),
      (Name : 'Max';
       Code : 'function Max(a,b) { return (a>b)?a:b }'),
      (Name : 'MaxInt';
       Code : 'function MaxInt(a,b) { return (a>b)?a:b }'),
      (Name : 'Min';
       Code : 'function Min(a,b) { return (a<b)?a:b }'),
      (Name : 'MinInt';
       Code : 'function MinInt(a,b) { return (a<b)?a:b }'),
      (Name : 'Now';
       Code : 'function Now() { var d=new Date(); return d.getTime()/8.64e7+25569-d.getTimezoneOffset()/1440 }'),
      (Name : 'Odd';
       Code : 'function Odd(v) { return (v&1)==1 }'),
      (Name : 'Pi';
       Code : 'function Pi() { return Math.PI }'),
      (Name : 'Pos';
       Code : 'function Pos(a,b) { return b.indexOf(a)+1 }'),
      (Name : 'PosEx';
       Code : 'function PosEx(a,b,o) { return b.indexOf(a,o-1)+1 }'),
      (Name : 'Power';
       Code : 'function Power(x,y) { return Math.pow(x,y) }'),
      (Name : 'QuotedStr';
       Code : 'function QuotedStr(s,q) { if (!q) q="''"; return q+s.replace(q, q+q)+q }'),
      (Name : 'RadToDeg';
       Code : 'function RadToDeg(v) { return v*(180/Math.PI) }'),
      (Name : 'Random';
       Code : 'function Random() { var tmp=Math.floor($dwsRand*0x08088405+1)%4294967296; $dwsRand=tmp; return tmp*Math.pow(2, -32) }';
       Dependency : '$dwsRand'),
      (Name : 'ReverseString';
       Code : 'function ReverseString(s) { return s.split("").reverse().join("") }'),
      (Name : 'RevPos';
       Code : 'function RevPos(a,b) { return (a=="")?0:(b.lastIndexOf(a)+1) }'),
      (Name : 'RightStr';
       Code : 'function RightStr(s,n) { return s.substr(s.length-n) }'),
      (Name : 'Round';
       Code : 'function Round(v) { return Math.round(v) }'),
      (Name : 'SameText';
       Code : 'function SameText(a,b) { return a.toUpperCase()==b.toUpperCase() }'),
      (Name : 'SetLength';
       Code : 'function SetLength(s,n) { if (s.value.length>n) s.value=s.value.substring(0,n); else while (s.value.length<n) s.value+=" "; }'),
      (Name : 'SetRandSeed';
       Code : 'function SetRandSeed(v) { $dwsRand = v }';
       Dependency : '$dwsRand'),
      (Name : 'Sin';
       Code : 'function Sin(v) { return Math.sin(v) }'),
      (Name : 'Sinh';
       Code : 'function Sinh(v) { return (v==0)?0:(0.5*(Math.exp(v)-Math.exp(-v))) }'),
      (Name : 'StrAfter';
       Code : 'function StrAfter(s,d) { if (!d) return ""; var p=s.indexOf(d); return (p<0)?"":s.substr(p+d.length) }'),
      (Name : 'StrBefore';
       Code : 'function StrBefore(s,d) { if (!d) return s; var p=s.indexOf(d); return (p<0)?s:s.substr(0, p) }'),
      (Name : 'StrBeginsWith';
       Code : 'function StrBeginsWith(s,b) { return s.substr(0, b.length)==b }'),
      (Name : 'StrEndsWith';
       Code : 'function StrEndsWith(s,e) { return s.substr(s.length-e.length)==e }'),
      (Name : 'StringOfChar';
       Code : 'function StringOfChar(c,n) { return stringRepeat(c?c.charAt(0):" ",n) }';
       Dependency : '!stringRepeat_js'),
      (Name : 'StringOfString';
       Code : 'function StringOfString(s,n) { return stringRepeat(s,n) }';
       Dependency : '!stringRepeat_js'),
      (Name : 'StrToFloat';
       Code : 'function StrToFloat(v) { return parseFloat(v) }'),
      (Name : 'StrToFloatDef';
       Code : 'function StrToFloatDef(v,d) { var r=parseFloat(v); return isNaN(r)?d:r }'),
      (Name : 'StrToInt';
       Code : 'function StrToInt(v) { return parseInt(v,10) }'),
      (Name : 'StrToIntDef';
       Code : 'function StrToIntDef(v,d) { var r=parseInt(v,10); return isNaN(r)?d:r }'),
      (Name : 'SubStr';
       Code : 'function SubStr(s,f) { return s.substr(f-1) }'),
      (Name : 'SubString';
       Code : 'function SubString(s,f,t) { return s.substr(f-1,t-2) }'),
      (Name : 'Sqrt';
       Code : 'function Sqrt(v) { return Math.sqrt(v) }'),
      (Name : 'Tan';
       Code : 'function Tan(v) { return Math.tan(v) }'),
      (Name : 'Tanh';
       Code : 'function Tanh(v) { return (v==0)?0:(Math.exp(v)-Math.exp(-v))/(Math.exp(v)+Math.exp(-v)) }'),
      (Name : 'Trim';
       Code : 'function Trim(s) { return s.replace(/^\s\s*/, "").replace(/\s\s*$/, "") }'),
      (Name : 'TrimLeft';
       Code : 'function TrimLeft(s) { return s.replace(/^\s\s*/, "") }'),
      (Name : 'TrimRight';
       Code : 'function TrimRight(s) { return s.replace(/\s\s*$/, "") }'),
      (Name : 'Trunc';
       Code : 'function Trunc(v) { return (v>=0)?Math.floor(v):Math.ceil(v) }'),
      (Name : 'UpperCase';
       Code : 'function UpperCase(v) { return v.toUpperCase() }'),
      // RTL classes
      (Name : 'TObject';
       Code : 'var TObject={'#13#10
               +#9'$ClassName:"TObject",'#13#10
               +#9'ClassName:function(Self){return Self.$ClassName},'#13#10
               +#9'ClassType:function(Self){return Self},'#13#10
               +#9'$Init:function () {},'#13#10
               +#9'Create:function (Self) { return Self; },'#13#10
               +#9'Destroy:function (Self) { for (prop in obj) { if (obj.hasOwnProperty(prop)) { delete obj.prop; } } },'#13#10
               +#9'Destroy$v:function(Self){return Self.ClassType.Destroy.apply(Self.ClassType, arguments)},'#13#10
               +#9'Free:function (Self) { if (Self!=null) Self.ClassType.Destroy$v(Self) }'#13#10
               +'}';
       Dependency : '$New'),
      (Name : 'Exception';
       Code : 'var Exception={'#13#10
               +#9'$ClassName:"Exception",'#13#10
               +#9'$Parent:TObject,'#13#10
               +#9'$Init:function () { FMessage=""; },'#13#10
               +#9'Create$1:function (Self,Msg) { Self.FMessage=Msg; return Self; }'#13#10
               +'}';
       Dependency : 'TObject'),
      (Name : 'EAssertionFailed';
       Code : 'var EAssertionFailed={'#13#10
               +#9'$ClassName:"EAssertionFailed",'#13#10
               +#9'$Parent:Exception,'#13#10
               +#9'$Init:Exception.$Init,'#13#10
               +'}';
       Dependency : 'Exception')
   );

// FindJSRTLDependency
//
function FindJSRTLDependency(const name : String) : PJSRTLDependency;
var
   i : Integer;
begin
   for i:=Low(cJSRTLDependencies) to High(cJSRTLDependencies) do begin
      Result:=@cJSRTLDependencies[i];
      if Result.Name=name then Exit;
   end;
   Result:=nil;
end;

// ------------------
// ------------------ TdwsJSCodeGen ------------------
// ------------------

// Create
//
constructor TdwsJSCodeGen.Create;
begin
   inherited;
   FLocalVarParams:=TDataSymbolList.Create;
   FLocalVarParamsStack:=TSimpleStack<TDataSymbolList>.Create;

   FDeclaredLocalVars:=TDataSymbolList.Create;
   FDeclaredLocalVarsStack:=TSimpleStack<TDataSymbolList>.Create;

   FMainBodyName:='$dws';

   RegisterCodeGen(TBlockInitExpr, TJSBlockInitExpr.Create);

   RegisterCodeGen(TBlockExpr,            TJSBlockExpr.Create);
   RegisterCodeGen(TBlockExprNoTable,     TJSBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable2,    TJSBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable3,    TJSBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable4,    TJSBlockExprBase.Create);

   RegisterCodeGen(TdwsJSBlockExpr,       TJSRAWBlockExpr.Create);

   RegisterCodeGen(TNullExpr,             TdwsExprGenericCodeGen.Create(['/* null */'], True));
   RegisterCodeGen(TNoResultWrapperExpr,  TdwsExprGenericCodeGen.Create([0, ';'], True));

   RegisterCodeGen(TConstExpr,            TJSConstExpr.Create);
   RegisterCodeGen(TConstIntExpr,         TJSConstIntExpr.Create);
   RegisterCodeGen(TConstStringExpr,      TJSConstStringExpr.Create);
   RegisterCodeGen(TConstFloatExpr,       TJSConstFloatExpr.Create);
   RegisterCodeGen(TConstBooleanExpr,     TJSConstBooleanExpr.Create);
   RegisterCodeGen(TArrayConstantExpr,    TJSArrayConstantExpr.Create);

   RegisterCodeGen(TInitDataExpr,         TJSInitDataExpr.Create);

   RegisterCodeGen(TAssignExpr,           TJSAssignExpr.Create);
   RegisterCodeGen(TAssignClassOfExpr,    TJSAssignClassOfExpr.Create);
   RegisterCodeGen(TAssignDataExpr,       TJSAssignDataExpr.Create);
   RegisterCodeGen(TAssignFuncExpr,       TJSAssignFuncExpr.Create);

   RegisterCodeGen(TAssignConstToIntegerVarExpr,   TJSAssignConstToIntegerVarExpr.Create);
   RegisterCodeGen(TAssignConstToFloatVarExpr,     TJSAssignConstToFloatVarExpr.Create);
   RegisterCodeGen(TAssignConstToBoolVarExpr,      TJSAssignConstToBoolVarExpr.Create);
   RegisterCodeGen(TAssignConstToStringVarExpr,    TJSAssignConstToStringVarExpr.Create);
   RegisterCodeGen(TAssignNilToVarExpr,            TJSAssignNilToVarExpr.Create);
   RegisterCodeGen(TAssignNilClassToVarExpr,       TJSAssignNilToVarExpr.Create);
   RegisterCodeGen(TAssignConstDataToVarExpr,      TJSAssignConstDataToVarExpr.Create);

   RegisterCodeGen(TAssignArrayConstantExpr, TdwsExprGenericCodeGen.Create([0, '=', 1, ';'], True));

   RegisterCodeGen(TVarExpr,              TJSVarExpr.Create);
   RegisterCodeGen(TVarParentExpr,        TJSVarParentExpr.Create);
   RegisterCodeGen(TVarParamExpr,         TJSVarParamExpr.Create);
   RegisterCodeGen(TVarParamParentExpr,   TJSVarParamParentExpr.Create);
   RegisterCodeGen(TLazyParamExpr,        TJSLazyParamExpr.Create);

   RegisterCodeGen(TIntVarExpr,           TJSVarExpr.Create);
   RegisterCodeGen(TFloatVarExpr,         TJSVarExpr.Create);
   RegisterCodeGen(TStrVarExpr,           TJSVarExpr.Create);
   RegisterCodeGen(TBoolVarExpr,          TJSVarExpr.Create);
   RegisterCodeGen(TObjectVarExpr,        TJSVarExpr.Create);
   RegisterCodeGen(TConstParamExpr,       TJSVarExpr.Create);

   RegisterCodeGen(TRecordExpr,           TJSRecordExpr.Create);

   RegisterCodeGen(TConvIntegerExpr,      TJSConvIntegerExpr.Create);
   RegisterCodeGen(TConvFloatExpr,
      TdwsExprGenericCodeGen.Create(['Math.round(', 0, ')']));
   RegisterCodeGen(TConvBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '?true:false)']));
   RegisterCodeGen(TConvStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '.toString())']));

   RegisterCodeGen(TConvClassExpr,        TJSConvClassExpr.Create);
   RegisterCodeGen(TObjAsClassExpr,       TJSObjAsClassExpr.Create);
   RegisterCodeGen(TIsOpExpr,             TJSIsOpExpr.Create);

   RegisterCodeGen(TObjAsIntfExpr,        TJSObjAsIntfExpr.Create);
   RegisterCodeGen(TIntfAsClassExpr,      TJSIntfAsClassExpr.Create);
   RegisterCodeGen(TIntfAsIntfExpr,       TJSIntfAsIntfExpr.Create);
   RegisterCodeGen(TImplementsIntfOpExpr, TJSTImplementsIntfOpExpr.Create);
   RegisterCodeGen(TClassImplementsIntfOpExpr, TJSTClassImplementsIntfOpExpr.Create);
   RegisterCodeGen(TIntfCmpExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '===', 1, ')']));

   RegisterCodeGen(TAddStrExpr,           TJSBinOpExpr.Create('+', True));

   RegisterCodeGen(TAddIntExpr,           TJSBinOpExpr.Create('+', True));
   RegisterCodeGen(TAddFloatExpr,         TJSBinOpExpr.Create('+', True));
   RegisterCodeGen(TSubIntExpr,           TJSBinOpExpr.Create('-', False));
   RegisterCodeGen(TSubFloatExpr,         TJSBinOpExpr.Create('-', False));
   RegisterCodeGen(TMultIntExpr,          TJSBinOpExpr.Create('*', True));
   RegisterCodeGen(TMultFloatExpr,        TJSBinOpExpr.Create('*', True));
   RegisterCodeGen(TDivideExpr,           TJSBinOpExpr.Create('/', True));
   RegisterCodeGen(TDivExpr,
      TdwsExprGenericCodeGen.Create(['Math.floor(', 0, '/', 1, ')']));
   RegisterCodeGen(TModExpr,              TJSBinOpExpr.Create('%', True));
   RegisterCodeGen(TSqrFloatExpr,      TJSSqrExpr.Create);
   RegisterCodeGen(TSqrIntExpr,        TJSSqrExpr.Create);
   RegisterCodeGen(TNegIntExpr,
      TdwsExprGenericCodeGen.Create(['(', '-', 0, ')']));
   RegisterCodeGen(TNegFloatExpr,
      TdwsExprGenericCodeGen.Create(['(', '-', 0, ')']));

   RegisterCodeGen(TAppendStringVarExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', 1, ';'], True));
   RegisterCodeGen(TAppendConstStringVarExpr,      TJSAppendConstStringVarExpr.Create);

   RegisterCodeGen(TPlusAssignIntExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', 1, ';'], True));
   RegisterCodeGen(TPlusAssignFloatExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', 1, ';'], True));
   RegisterCodeGen(TPlusAssignStrExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', 1, ';'], True));
   RegisterCodeGen(TMinusAssignIntExpr,
      TdwsExprGenericCodeGen.Create([0, '-=', 1, ';'], True));
   RegisterCodeGen(TMinusAssignFloatExpr,
      TdwsExprGenericCodeGen.Create([0, '-=', 1, ';'], True));
   RegisterCodeGen(TMultAssignIntExpr,
      TdwsExprGenericCodeGen.Create([0, '*=', 1, ';'], True));
   RegisterCodeGen(TMultAssignFloatExpr,
      TdwsExprGenericCodeGen.Create([0, '*=', 1, ';'], True));
   RegisterCodeGen(TDivideAssignExpr,
      TdwsExprGenericCodeGen.Create([0, '/=', 1, ';'], True));

   RegisterCodeGen(TIncIntVarExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', 1, ';'], True));
   RegisterCodeGen(TDecIntVarExpr,
      TdwsExprGenericCodeGen.Create([0, '-=', 1, ';'], True));

   RegisterCodeGen(TIncVarFuncExpr,    TJSIncVarFuncExpr.Create);
   RegisterCodeGen(TDecVarFuncExpr,    TJSDecVarFuncExpr.Create);
   RegisterCodeGen(TSuccFuncExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '+', 1, ')']));
   RegisterCodeGen(TPredFuncExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '-', 1, ')']));

   RegisterCodeGen(TShrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>>', 1, ')']));
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

   RegisterCodeGen(TBoolAndExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '&&', 1, ')']));
   RegisterCodeGen(TBoolOrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '||', 1, ')']));
   RegisterCodeGen(TBoolXorExpr,
      TdwsExprGenericCodeGen.Create(['(!', 0, ' != !', 1, ')']));
   RegisterCodeGen(TBoolImpliesExpr,
      TdwsExprGenericCodeGen.Create(['(!', 0, ' || ', 1, ')']));
   RegisterCodeGen(TNotBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', '!', 0, ')']));

   RegisterCodeGen(TAssignedInstanceExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null)']));
   RegisterCodeGen(TAssignedMetaClassExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null)']));
   RegisterCodeGen(TAssignedFuncPtrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null)']));

   RegisterCodeGen(TRelEqualBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '==', 1, ')']));
   RegisterCodeGen(TRelNotEqualBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!=', 1, ')']));

   RegisterCodeGen(TObjCmpExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '===', 1, ')']));

   RegisterCodeGen(TRelEqualIntExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '==', 1, ')']));
   RegisterCodeGen(TRelNotEqualIntExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!=', 1, ')']));
   RegisterCodeGen(TRelGreaterEqualIntExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>=', 1, ')']));
   RegisterCodeGen(TRelLessEqualIntExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<=', 1, ')']));
   RegisterCodeGen(TRelGreaterIntExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>', 1, ')']));
   RegisterCodeGen(TRelLessIntExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<', 1, ')']));

   RegisterCodeGen(TRelEqualStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '==', 1, ')']));
   RegisterCodeGen(TRelNotEqualStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!=', 1, ')']));
   RegisterCodeGen(TRelGreaterEqualStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>=', 1, ')']));
   RegisterCodeGen(TRelLessEqualStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<=', 1, ')']));
   RegisterCodeGen(TRelGreaterStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>', 1, ')']));
   RegisterCodeGen(TRelLessStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<', 1, ')']));

   RegisterCodeGen(TRelEqualFloatExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '==', 1, ')']));
   RegisterCodeGen(TRelNotEqualFloatExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!=', 1, ')']));
   RegisterCodeGen(TRelGreaterEqualFloatExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>=', 1, ')']));
   RegisterCodeGen(TRelLessEqualFloatExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<=', 1, ')']));
   RegisterCodeGen(TRelGreaterFloatExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>', 1, ')']));
   RegisterCodeGen(TRelLessFloatExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<', 1, ')']));


   RegisterCodeGen(TIfThenExpr,
      TdwsExprGenericCodeGen.Create(['if (', 0, ') {', #9, 1, #8, '};'], True));
   RegisterCodeGen(TIfThenElseExpr,
      TdwsExprGenericCodeGen.Create(['if (', 0, ') {', #9, 1, #8, '} else {', #9, 2, #8, '};'], True));

   RegisterCodeGen(TInOpExpr,             TJSInOpExpr.Create);
   RegisterCodeGen(TCaseExpr,             TJSCaseExpr.Create);

   RegisterCodeGen(TForUpwardExpr,        TJSForUpwardExpr.Create);
   RegisterCodeGen(TForDownwardExpr,      TJSForDownwardExpr.Create);
   RegisterCodeGen(TForUpwardStepExpr,    TJSForUpwardStepExpr.Create);
   RegisterCodeGen(TForDownwardStepExpr,  TJSForDownwardStepExpr.Create);

   RegisterCodeGen(TWhileExpr,
      TdwsExprGenericCodeGen.Create(['while (', 0, ') {', #9, 1, #8, '};'], True));
   RegisterCodeGen(TRepeatExpr,
      TdwsExprGenericCodeGen.Create(['do {', #9, 1, #8, '} while (!', 0, ');'], True));
   RegisterCodeGen(TLoopExpr,
      TdwsExprGenericCodeGen.Create(['while (true) {', #9, 1, #8, '};'], True));

   RegisterCodeGen(TContinueExpr,         TdwsExprGenericCodeGen.Create(['continue;'], True));
   RegisterCodeGen(TBreakExpr,            TdwsExprGenericCodeGen.Create(['break;'], True));
   RegisterCodeGen(TExitValueExpr,        TdwsExprGenericCodeGen.Create(['return ', 0, ';'], True));
   RegisterCodeGen(TExitExpr,             TJSExitExpr.Create);

   RegisterCodeGen(TRaiseExpr,            TdwsExprGenericCodeGen.Create(['throw ', 0, ';'], True));
   RegisterCodeGen(TReRaiseExpr,          TdwsExprGenericCodeGen.Create(['throw $e;'], True));
   RegisterCodeGen(TExceptExpr,           TJSExceptExpr.Create);

   RegisterCodeGen(TFinallyExpr,
      TdwsExprGenericCodeGen.Create(['try {', #9, 0, #8, '} finally {', #9, 1, #8, '};'], True));

   RegisterCodeGen(TNewArrayExpr,            TJSNewArrayExpr.Create);
   RegisterCodeGen(TArraySetLengthExpr,      TJSArraySetLengthExpr.Create);
   RegisterCodeGen(TArrayAddExpr,            TJSArrayAddExpr.Create);
   RegisterCodeGen(TArrayDeleteExpr,         TJSArrayDeleteExpr.Create);
   RegisterCodeGen(TArrayCopyExpr,           TJSArrayCopyExpr.Create);
   RegisterCodeGen(TArraySwapExpr,           TJSArraySwapExpr.Create);
   RegisterCodeGen(TArrayReverseExpr,        TdwsExprGenericCodeGen.Create([0, '.reverse();'], True));

   RegisterCodeGen(TStaticArrayExpr,         TJSStaticArrayExpr.Create);
   RegisterCodeGen(TDynamicArrayExpr,        TJSDynamicArrayExpr.Create);
   RegisterCodeGen(TDynamicArraySetExpr,     TJSDynamicArraySetExpr.Create);
   RegisterCodeGen(TStringArrayOpExpr,       TJSStringArrayOpExpr.Create);
   RegisterCodeGen(TVarStringArraySetExpr,   TJSVarStringArraySetExpr.Create);

   RegisterCodeGen(TStringLengthExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, ').length']));
   RegisterCodeGen(TArrayLengthExpr,         TJSArrayLengthExpr.Create);
   RegisterCodeGen(TOpenArrayLengthExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, ').length']));

   RegisterCodeGen(TOrdIntExpr,
      TdwsExprGenericCodeGen.Create([0]));
   RegisterCodeGen(TOrdBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '?1:0)']));
   RegisterCodeGen(TOrdStrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, ').charCodeAt(0)']));

   RegisterCodeGen(TFuncExpr,             TJSFuncBaseExpr.Create);

   RegisterCodeGen(TMagicIntFuncExpr,     TJSMagicFuncExpr.Create);
   RegisterCodeGen(TMagicStringFuncExpr,  TJSMagicFuncExpr.Create);
   RegisterCodeGen(TMagicFloatFuncExpr,   TJSMagicFuncExpr.Create);
   RegisterCodeGen(TMagicBoolFuncExpr,    TJSMagicFuncExpr.Create);
   RegisterCodeGen(TMagicProcedureExpr,   TJSMagicFuncExpr.Create);

   RegisterCodeGen(TConstructorStaticExpr,      TJSConstructorStaticExpr.Create);
   RegisterCodeGen(TConstructorVirtualExpr,     TJSConstructorVirtualExpr.Create);
   RegisterCodeGen(TConstructorVirtualObjExpr,  TJSMethodVirtualExpr.Create);
   RegisterCodeGen(TConstructorStaticObjExpr,   TJSMethodStaticExpr.Create);

   RegisterCodeGen(TDestructorStaticExpr,       TJSMethodStaticExpr.Create);
   RegisterCodeGen(TDestructorVirtualExpr,      TJSMethodVirtualExpr.Create);

   RegisterCodeGen(TMethodStaticExpr,           TJSMethodStaticExpr.Create);
   RegisterCodeGen(TMethodVirtualExpr,          TJSMethodVirtualExpr.Create);

   RegisterCodeGen(TMethodInterfaceExpr,        TJSMethodInterfaceExpr.Create);

   RegisterCodeGen(TClassMethodStaticExpr,      TJSClassMethodStaticExpr.Create);
   RegisterCodeGen(TClassMethodVirtualExpr,     TJSClassMethodVirtualExpr.Create);

   RegisterCodeGen(TFuncPtrExpr,                TJSFuncPtrExpr.Create);
   RegisterCodeGen(TFuncRefExpr,                TJSFuncRefExpr.Create);

   RegisterCodeGen(TFieldExpr,                  TJSFieldExpr.Create);
   RegisterCodeGen(TReadOnlyFieldExpr,          TJSFieldExpr.Create);

   RegisterCodeGen(TAssertExpr,                 TJSAssertExpr.Create);
   RegisterCodeGen(TDeclaredExpr,               TJSDeclaredExpr.Create);
   RegisterCodeGen(TDefinedExpr,                TJSDefinedExpr.Create);
end;

// Destroy
//
destructor TdwsJSCodeGen.Destroy;
begin
   while FLocalVarParamsStack.Count>0 do begin
      FLocalVarParamsStack.Peek.Free;
      FLocalVarParamsStack.Pop;
   end;
   FLocalVarParamsStack.Free;
   FLocalVarParams.Free;

   while FDeclaredLocalVarsStack.Count>0 do begin
      FDeclaredLocalVarsStack.Peek.Free;
      FDeclaredLocalVarsStack.Pop;
   end;
   FDeclaredLocalVarsStack.Free;
   FDeclaredLocalVars.Free;

   inherited;
end;

// SymbolMappedName
//
function TdwsJSCodeGen.SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : String;
var
   ct : TClass;
begin
   ct:=sym.ClassType;
   if (ct=TSelfSymbol) or (ct=TResultSymbol) then
      Result:=sym.Name
   else Result:=inherited SymbolMappedName(sym, scope);
end;

// CompileEnumerationSymbol
//
procedure TdwsJSCodeGen.CompileEnumerationSymbol(enum : TEnumerationSymbol);
var
   i : Integer;
   elem : TElementSymbol;
begin
   if enum.Elements.Count=0 then Exit;
   WriteString('/* ');
   WriteString(enum.QualifiedName);
   WriteStringLn('*/');
   for i:=0 to enum.Elements.Count-1 do begin
      elem:=enum.Elements[i] as TElementSymbol;
      WriteString(elem.Name);
      WriteString('=');
      WriteString(IntToStr(elem.UserDefValue));
      WriteStringLn(';');
   end;
end;

// CompileFuncSymbol
//
procedure TdwsJSCodeGen.DoCompileFuncSymbol(func : TSourceFuncSymbol);
begin
   WriteString('function ');
   WriteSymbolName(func);
   WriteString('(');
   WriteFuncParams(func);
   WriteStringLn(') {');
   Indent;

   CompileFuncBody(func);

   UnIndent;
   WriteStringLn('};');
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
      WriteJavaScriptString(Format(msgFmt, [func.QualifiedName, cond.Pos.AsInfo, '']));
      WriteString(',');
      Compile(cond.Msg);
      WriteStringLn(');');
   end;

   if not preConds then
      CompileInheritedConditions;
end;

// CompileRecordSymbol
//
procedure TdwsJSCodeGen.CompileRecordSymbol(rec : TRecordSymbol);
//var
//   i : Integer;
//   member : TMemberSymbol;
begin
//   WriteString('function $init');
//   WriteString(rec.Name);
//   WriteStringLn('(r) {');
//   for i:=0 to rec.Members.Count-1 do begin
//      member:=rec.Members[i] as TMemberSymbol;
//      WriteString('r.');
//      WriteString(member.Name);
//      WriteString('=');
//      WriteDefaultValue(member.Typ, False);
//      WriteStringLn(';');
//   end;
//   WriteStringLn('};');
end;

// DoCompileClassSymbol
//
procedure TdwsJSCodeGen.DoCompileClassSymbol(cls : TClassSymbol);
var
   i : Integer;
   sym : TSymbol;
   meth : TMethodSymbol;
begin
   inherited;

   WriteString('var ');
   WriteSymbolName(cls);
   WriteStringLn('={');
   Indent;

   WriteString('$ClassName:');
   WriteJavaScriptString(cls.Name);
   WriteStringLn(',');
   WriteString('$Parent:');
   WriteSymbolName(cls.Parent);
   WriteStringLn(',');

   Dependencies.Add('$New');
   Dependencies.Add('TObject');

   WriteStringLn('$Init:function (Self) {');
   Indent;
   WriteSymbolName(cls.Parent);
   WriteStringLn('.$Init(Self);');
   for i:=0 to cls.Members.Count-1 do begin
      sym:=cls.Members[i];
      if sym is TFieldSymbol then begin
         WriteString('Self.');
         WriteString(MemberName(sym, cls));
         WriteString('=');
         WriteDefaultValue(sym.Typ, False);
         WriteStringLn(';');
      end;
   end;
   UnIndent;
   WriteStringLn('}');

   // Compile methods specified by the class

   for i:=0 to cls.Members.Count-1 do begin
      sym:=cls.Members[i];
      if sym is TMethodSymbol then
         CompileMethod(TMethodSymbol(sym));
   end;

   // VMT entries for methods not overridden here

   for i:=0 to cls.VMTCount-1 do begin
      meth:=cls.VMTMethod(i);
//      if meth.Name='Destroy' then continue;
      if meth.StructSymbol<>cls then begin
         WriteString(',');
         WriteString(MemberName(meth, meth.StructSymbol));
         WriteString(':');
         WriteSymbolName(meth.StructSymbol);
         WriteString('.');
         WriteString(MemberName(meth, meth.StructSymbol));
         WriteLineEnd;
      end;
      WriteString(',');
      WriteString(MemberName(meth, meth.StructSymbol));
      WriteString('$v:');
      if meth.StructSymbol=cls then begin
         if meth.Kind=fkConstructor then begin
            WriteString('function(Self){return Self.ClassType.');
            WriteString(MemberName(meth, meth.StructSymbol));
            WriteStringLn('.apply(Self, arguments)}');
         end else if meth.IsClassMethod then begin
            WriteString('function(Self){return Self.');
            WriteString(MemberName(meth, meth.StructSymbol));
            WriteStringLn('.apply(Self, arguments)}');
         end else begin
            WriteString('function(Self){return Self.ClassType.');
            WriteString(MemberName(meth, meth.StructSymbol));
            WriteStringLn('.apply(Self.ClassType, arguments)}');
         end;
      end else begin
         WriteSymbolName(meth.StructSymbol);
         WriteString('.');
         WriteString(MemberName(meth, meth.StructSymbol)+'$v');
         WriteLineEnd;
      end;
   end;

   UnIndent;
   WriteStringLn('};');

   DoCompileInterfaceTable(cls);
end;

// DoCompileInterfaceTable
//
procedure TdwsJSCodeGen.DoCompileInterfaceTable(cls : TClassSymbol);
var
   needIntfTable : Boolean;
   iter : TClassSymbol;
   writtenInterfaces : TList;
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
   WriteString('.$Intf={');
   WriteLineEnd;
   Indent;

   writtenInterfaces:=TList.Create;
   try
      iter:=cls;
      while iter<>nil do begin
         if iter.Interfaces<>nil then begin
            iter.Interfaces.Enumerate(
               procedure (const item : TResolvedInterface)
               var
                  i : Integer;
               begin
                  if writtenInterfaces.IndexOf(item.IntfSymbol)>=0 then Exit;
                  if writtenInterfaces.Count>0 then
                     Self.WriteString(',');
                  writtenInterfaces.Add(item.IntfSymbol);
                  Self.WriteSymbolName(item.IntfSymbol);
                  Self.WriteString(':[');
                  for i:=0 to High(item.VMT) do begin
                     if i>0 then
                        Self.WriteString(',');
                     WriteSymbolName(iter);
                     Self.WriteString('.');
                     Self.WriteSymbolName(item.VMT[i]);
                  end;
                  Self.WriteStringLn(']');
               end);
         end;
         iter:=iter.Parent;
      end;
   finally
      writtenInterfaces.Free;
   end;

   UnIndent;
   WriteString('};');
   WriteLineEnd;
end;

// CompileProgramBody
//
procedure TdwsJSCodeGen.CompileProgramBody(expr : TNoResultExpr);
begin
   if expr is TNullExpr then Exit;
   if FMainBodyName<>'' then begin
      WriteString('var ');
      WriteString(FMainBodyName);
      WriteStringLn('=function() {');
      Indent;
   end;
   inherited;
   if FMainBodyName<>'' then begin
      UnIndent;
      WriteStringLn('};');
      WriteString(FMainBodyName);
      WriteStringLn('();');
   end;
end;

// CompileSymbolTable
//
procedure TdwsJSCodeGen.CompileSymbolTable(table : TSymbolTable);
var
   varSym : TDataSymbol;
   sym : TSymbol;
begin
   inherited;
   for sym in table do begin
      if sym.ClassType=TDataSymbol then begin
         varSym:=TDataSymbol(sym);
         if FDeclaredLocalVars.IndexOf(varSym)<0 then begin
            FDeclaredLocalVars.Add(varSym);
            WriteString('var ');
            WriteSymbolName(varSym);
            WriteString('=');
            WriteDefaultValue(varSym.Typ, TJSExprCodeGen.IsLocalVarParam(Self, varSym));
            WriteStringLn(';');
         end;
      end;
   end;
end;

// ReserveSymbolNames
//
procedure TdwsJSCodeGen.ReserveSymbolNames;
const
   cJSReservedWords : array [1..202] of String = (
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
      'onsubmit', 'onunload'
   );
var
   i : Integer;
begin
   for i:=Low(cJSReservedWords) to High(cJSReservedWords) do
      SymbolMap.ReserveName(cJSReservedWords[i]);
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

   procedure InsertDependency(dep : PJSRTLDependency);
   var
      sub : PJSRTLDependency;
   begin
      if FlushedDependencies.IndexOf(dep.Name)>=0 then Exit;

      if     (dep.Dependency<>'')
         and (processedDependencies.IndexOf(dep.Dependency)<0) then begin
         processedDependencies.Add(dep.Dependency);
         if dep.Dependency[1]='!' then begin
            InsertResourceDependency(Copy(dep.Dependency, 2, MaxInt));
         end else begin
            sub:=FindJSRTLDependency(dep.Dependency);
            if sub<>nil then
               InsertDependency(sub);
         end;
      end;
      destStream.WriteString(dep.Code);
      destStream.WriteString(';'#13#10);
      FlushedDependencies.Add(dep.Name);
   end;

var
   i : Integer;
   dependency : String;
   jsRTL : PJSRTLDependency;
begin
   processedDependencies:=TStringList.Create;
   processedDependencies.Sorted:=True;
   try
      for i:=Dependencies.Count-1 downto 0 do begin
         dependency:=Dependencies[i];
         if FlushedDependencies.IndexOf(dependency)>=0 then
            continue;
         jsRTL:=FindJSRTLDependency(dependency);
         processedDependencies.Add(dependency);
         if jsRTL<>nil then
            InsertDependency(jsRTL)
         else if dependency='$ConditionalDefines' then begin
            destStream.WriteString('var $ConditionalDefines=');
            WriteStringArray(destStream, (prog as TdwsProgram).Root.ConditionalDefines);
            destStream.WriteString(';'#13#10);
         end;
         Dependencies.Delete(i);
      end;
   finally
      processedDependencies.Free;
   end;
end;

// GetNewTempSymbol
//
function TdwsJSCodeGen.GetNewTempSymbol : String;
begin
   Result:='$tmp'+inherited GetNewTempSymbol;
end;

// WriteJavaScriptString
//
procedure TdwsJSCodeGen.WriteJavaScriptString(const s : String);
begin
   dwsJSON.WriteJavaScriptString(Output, s);
end;

// CollectLocalVarParams
//
procedure TdwsJSCodeGen.CollectLocalVarParams(expr : TExprBase);
begin
   if expr=nil then Exit;
   expr.RecursiveEnumerateSubExprs(
      procedure (parent, expr : TExprBase; var abort : Boolean)
      var
         funcSym : TFuncSymbol;
         varSym : TDataSymbol;
         i : Integer;
         right : TExprBase;
      begin
         if (expr is TVarExpr) and (parent is TFuncExprBase) then begin
            funcSym:=TFuncExprBase(parent).FuncSym;
            i:=parent.IndexOfSubExpr(expr);
            if (i>0) and (parent is TConstructorStaticExpr) then
               Dec(i);
            if (funcSym=nil) or (i>=funcSym.Params.Count) then begin
               if (parent.ClassType=TDecVarFuncExpr) or (parent.ClassType=TIncVarFuncExpr) then begin
                  right:=TMagicIteratorFuncExpr(parent).Args[1];
                  if (right is TConstIntExpr) and (TConstIntExpr(right).Value=1) then
                     Exit // special case handled via ++ or --, no need to pass by ref
                  else varSym:=FindSymbolAtStackAddr(TVarExpr(expr).StackAddr, Context.Level);
               end else begin
                  // else not supported yet
                  Exit;
               end;
            end else begin
               if funcSym.Params[i] is TVarParamSymbol then begin
                  varSym:=FindSymbolAtStackAddr(TVarExpr(expr).StackAddr, Context.Level);
               end else Exit;
            end;
         end else if (expr is TExitExpr) then begin
            // exit with a try.. clause that modifies the result can cause issues
            // with JS immutability, this is a heavy-handed solution
            varSym:=LocalTable.FindSymbol('Result', cvMagic) as TDataSymbol;
         end else begin
            // else not supported yet
            Exit;
         end;
         if FLocalVarParams.IndexOf(varSym)<0 then
            FLocalVarParams.Add(varSym);
      end);
end;

// CollectInitExprLocalVars
//
procedure TdwsJSCodeGen.CollectInitExprLocalVars(initExpr : TBlockExprBase);
var
   i : Integer;
   curExpr : TExprBase;
   expr : TVarExpr;
   varSym : TDataSymbol;
begin
   for i:=0 to initExpr.SubExprCount-1 do begin
      curExpr:=initExpr.SubExpr[i];
      Assert((curExpr is TAssignExpr) or (curExpr is TInitDataExpr));
      expr:=curExpr.SubExpr[0] as TVarExpr;
      varSym:=FindSymbolAtStackAddr(expr.StackAddr, Context.Level);
      FDeclaredLocalVars.Add(varSym);
   end;
end;

// EnterContext
//
procedure TdwsJSCodeGen.EnterContext(proc : TdwsProgram);
begin
   inherited;
   FLocalVarParamsStack.Push(FLocalVarParams);
   FLocalVarParams:=TDataSymbolList.Create;

   FDeclaredLocalVarsStack.Push(FDeclaredLocalVars);
   FDeclaredLocalVars:=TDataSymbolList.Create;

   CollectInitExprLocalVars(proc.InitExpr);

   CollectLocalVarParams(proc.InitExpr);
   CollectLocalVarParams(proc.Expr);
end;

// LeaveContext
//
procedure TdwsJSCodeGen.LeaveContext;
begin
   FDeclaredLocalVars.Free;
   FDeclaredLocalVars:=FDeclaredLocalVarsStack.Peek;
   FDeclaredLocalVarsStack.Pop;

   FLocalVarParams.Free;
   FLocalVarParams:=FLocalVarParamsStack.Peek;
   FLocalVarParamsStack.Pop;
   inherited;
end;

// WriteDefaultValue
//
procedure TdwsJSCodeGen.WriteDefaultValue(typ : TTypeSymbol; box : Boolean);
var
   i : Integer;
   sas : TStaticArraySymbol;
   recSym : TRecordSymbol;
   member : TFieldSymbol;
begin
   typ:=typ.UnAliasedType;

   if box then
      WriteString('{value:');
   if typ is TBaseIntegerSymbol then
      WriteString('0')
   else if typ is TBaseFloatSymbol then
      WriteString('0.0')
   else if typ is TBaseStringSymbol then
      WriteString('""')
   else if typ is TBaseBooleanSymbol then
      WriteString(cBoolToJSBool[false])
   else if typ is TClassSymbol then
      WriteString('null')
   else if typ is TClassOfSymbol then
      WriteString('null')
   else if typ is TFuncSymbol then
      WriteString('null')
   else if typ is TInterfaceSymbol then
      WriteString('null')
   else if typ is TEnumerationSymbol then
      WriteString(IntToStr(TEnumerationSymbol(typ).DefaultValue))
   else if typ is TStaticArraySymbol then begin
      sas:=TStaticArraySymbol(typ);
      WriteString('[');
      for i:=0 to sas.ElementCount-1 do begin
         if i>0 then
            WriteString(',');
         WriteDefaultValue(sas.Typ, False);
      end;
      WriteString(']');
   end else if typ is TDynamicArraySymbol then begin
      WriteString('[]');
   end else if typ is TRecordSymbol then begin
      recSym:=TRecordSymbol(typ);
      WriteString('{');
      for i:=0 to recSym.Members.Count-1 do begin
         if i>0 then
            WriteString(',');
         member:=(recSym.Members[i] as TFieldSymbol);
         WriteSymbolName(member);
         WriteString(':');
         WriteDefaultValue(member.Typ, False);
      end;
      WriteString('}');
   end else raise ECodeGenUnsupportedSymbol.CreateFmt('Default value of type %s', [typ.ClassName]);
   if box then
      WriteString('}');
end;

// WriteValue
//
procedure TdwsJSCodeGen.WriteValue(typ : TTypeSymbol; const data : TData; addr : Integer);
var
   i : Integer;
   recSym : TRecordSymbol;
   member : TFieldSymbol;
   sas : TStaticArraySymbol;
   intf : IUnknown;
begin
   typ:=typ.UnAliasedType;

   if typ is TBaseIntegerSymbol then
      WriteString(IntToStr(data[addr]))
   else if typ is TBaseFloatSymbol then
      WriteString(FloatToStr(data[addr], cFormatSettings))
   else if typ is TBaseStringSymbol then
      WriteJavaScriptString(VarToStr(data[addr]))
   else if typ is TBaseBooleanSymbol then begin
      WriteString(cBoolToJSBool[Boolean(data[addr])])
   end else if typ is TNilSymbol then begin
      WriteString('null')
   end else if typ is TClassOfSymbol then begin
      WriteSymbolName(typ.Typ)
   end else if typ is TStaticArraySymbol then begin
      sas:=TStaticArraySymbol(typ);
      WriteString('[');
      for i:=0 to sas.ElementCount-1 do begin
         if i>0 then
            WriteString(',');
         WriteValue(sas.Typ, data, addr+i*sas.Typ.Size);
      end;
      WriteString(']');
   end else if typ is TRecordSymbol then begin
      recSym:=TRecordSymbol(typ);
      WriteString('{');
      for i:=0 to recSym.Members.Count-1 do begin
         if i>0 then
            WriteString(',');
         member:=(recSym.Members[i] as TFieldSymbol);
         WriteSymbolName(member);
         WriteString(':');
         WriteValue(member.Typ, data, addr+member.Offset);
      end;
      WriteString('}');
   end else if typ is TClassSymbol then begin
      intf:=data[addr];
      if intf=nil then
         WriteString('null')
      else raise ECodeGenUnsupportedSymbol.Create('Non nil class symbol');
   end else if typ is TInterfaceSymbol then begin
      intf:=data[addr];
      if intf=nil then
         WriteString('null')
      else raise ECodeGenUnsupportedSymbol.Create('Non nil interface symbol');
   end else begin
      raise ECodeGenUnsupportedSymbol.CreateFmt('Value of type %s',
                                                [typ.ClassName]);
   end;

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
   if func is TMethodSymbol then begin
      WriteString('Self');
      needComma:=True;
   end else needComma:=False;

   for i:=0 to func.Params.Count-1 do begin
      if needComma then
         WriteString(', ');
      WriteSymbolName(func.Params[i]);
      needComma:=True;
   end;
end;

// CompileFuncBody
//
procedure TdwsJSCodeGen.CompileFuncBody(func : TFuncSymbol);
var
   resultTyp : TTypeSymbol;
   proc : TdwsProcedure;
   resultIsBoxed : Boolean;
   param : TParamSymbol;
   i : Integer;
begin
   proc:=(func.Executable as TdwsProcedure);
   if proc=nil then Exit;

   for i:=0 to proc.Func.Params.Count-1 do begin
      param:=proc.Func.Params[i] as TParamSymbol;
      if (not (param is TByRefParamSymbol)) and TJSExprCodeGen.IsLocalVarParam(Self, param) then begin
         WriteSymbolName(param);
         WriteString('={value:');
         WriteSymbolName(param);
         WriteStringLn('};');
      end;
   end;

   resultTyp:=func.Typ;
   if resultTyp<>nil then begin
      resultIsBoxed:=TJSExprCodeGen.IsLocalVarParam(Self, func.Result);
      resultTyp:=resultTyp.UnAliasedType;
      WriteString('var Result=');
      WriteDefaultValue(resultTyp, resultIsBoxed);
      WriteStringLn(';');
   end else resultIsBoxed:=False;

   if resultIsBoxed then begin
      WriteStringLn('try {');
      Indent;
   end;

   if not (cgoNoConditions in Options) then
      CompileConditions(func, proc.PreConditions, True);

   Compile(proc.InitExpr);

   CompileSymbolTable(proc.Table);

   Compile(proc.Expr);

   if not (cgoNoConditions in Options) then
      CompileConditions(func, proc.PostConditions, False);

   if resultTyp<>nil then begin
      if resultIsBoxed then begin
         UnIndent;
         WriteStringLn('} finally {return Result.value}')
      end else WriteStringLn('return Result');
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

   WriteString(',');
   WriteString(MemberName(meth, meth.StructSymbol));

   EnterContext(proc);
   try

      WriteString(':function(');
      WriteFuncParams(meth);
      WriteStringLn(') {');
      Indent;

      CompileFuncBody(meth);

      if meth.Kind=fkConstructor then
         WriteStringLn('return Self');

      UnIndent;
      WriteStringLn('}');

   finally
      LeaveContext;
   end;
end;

// MemberName
//
function TdwsJSCodeGen.MemberName(sym : TSymbol; cls : TStructuredTypeSymbol) : String;
//var
//   n : Integer;
//   match : TSymbol;
begin
   Result:=SymbolMappedName(sym, cgssClass);
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

// All_RTL_JS
//
class function TdwsJSCodeGen.All_RTL_JS : String;
var
   i : Integer;
   wobs : TWriteOnlyBlockStream;
begin
   wobs:=TWriteOnlyBlockStream.Create;
   try
      for i:=Low(cJSRTLDependencies) to High(cJSRTLDependencies) do begin
         wobs.WriteString(cJSRTLDependencies[i].Code);
         wobs.WriteString(#13#10);
      end;
      Result:=wobs.ToString;
   finally
      wobs.Free;
   end;
end;

// IgnoreRTLDependencies
//
procedure TdwsJSCodeGen.IgnoreRTLDependencies;
var
   i, k : Integer;
begin
   for i:=Low(cJSRTLDependencies) to High(cJSRTLDependencies) do begin
      k:=Dependencies.IndexOf(cJSRTLDependencies[i].Name);
      if k>=0 then
         Dependencies.Delete(k);
   end;
end;

// ------------------
// ------------------ TJSBlockInitExpr ------------------
// ------------------

// CodeGen
//
procedure TJSBlockInitExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   blockInit : TBlockInitExpr;
   initExpr : TExprBase;
   sym : TDataSymbol;
begin
   blockInit:=TBlockInitExpr(expr);
   for i:=0 to blockInit.SubExprCount-1 do begin
      initExpr:=blockInit.SubExpr[i];
      codeGen.WriteString('var ');
      Assert(initExpr.SubExprCount>=1);
      sym:=TJSVarExpr.CodeGenSymbol(codeGen, initExpr.SubExpr[0] as TVarExpr);
      if IsLocalVarParam(codeGen, sym) then begin
         codeGen.WriteSymbolName(sym);
         codeGen.WriteString('=new Object();');
      end;
      codeGen.Compile(blockInit.SubExpr[i]);
   end;
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
begin
   block:=TBlockExprNoTable(expr);
   for i:=0 to block.SubExprCount-1 do begin
      codeGen.Compile(block.SubExpr[i]);
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
begin
   e:=TdwsJSBlockExpr(expr);
   codeGen.WriteString(e.Code);
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
      codeGen.WriteString('.value');
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
   Result:=codeGen.FindSymbolAtStackAddr(varExpr.StackAddr, codeGen.Context.Level);
   if Result=nil then
      raise ECodeGenUnsupportedSymbol.CreateFmt('Var not found at StackAddr %d', [varExpr.StackAddr]);
end;

// ------------------
// ------------------ TJSVarParentExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarParentExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   e : TVarParentExpr;
   sym : TDataSymbol;
begin
   e:=TVarParentExpr(expr);
   sym:=nil;
   if codeGen.LocalTable<>nil then begin
      for i:=0 to codeGen.LocalTable.ParentCount-1 do begin
         sym:=codeGen.LocalTable.Parents[i].FindSymbolAtStackAddr(e.StackAddr, e.Level);
         if sym<>nil then Break;
      end;
   end;
   if sym=nil then
      raise ECodeGenUnsupportedSymbol.Create(IntToStr(e.StackAddr));
   codeGen.WriteSymbolName(sym);
   if IsLocalVarParam(codeGen, sym) then
      codeGen.WriteString('.value');
end;

// ------------------
// ------------------ TJSVarParamExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarParamExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   inherited;
   codeGen.WriteString('.value');
end;

// ------------------
// ------------------ TJSVarParamParentExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarParamParentExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   inherited;
   codeGen.WriteString('.value');
end;

// ------------------
// ------------------ TJSLazyParamExpr ------------------
// ------------------

// CodeGen
//
procedure TJSLazyParamExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   inherited;
   codeGen.WriteString('()');
end;

// ------------------
// ------------------ TJSInitDataExpr ------------------
// ------------------

// CodeGen
//
procedure TJSInitDataExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TInitDataExpr;
   t : TTypeSymbol;
   sas : TStaticArraySymbol;
begin
   e:=TInitDataExpr(expr);
   t:=e.Expr.Typ;

   codeGen.Compile(e.Expr);

   if t is TStaticArraySymbol then begin

      sas:=TStaticArraySymbol(t);

      codeGen.WriteString('=new Array('+IntToStr(sas.ElementCount)+');');

      codeGen.WriteString(' for (var i=0; i<');
      codeGen.WriteString(IntToStr(sas.ElementCount));
      codeGen.WriteString(';i++) ');
      codeGen.Compile(e.Expr);
      codeGen.WriteString('[i]=');

      (codeGen as TdwsJSCodeGen).WriteDefaultValue(sas.Typ.UnAliasedType, False);

      codeGen.WriteStringLn(';');

   end else if t is TDynamicArraySymbol then begin

      codeGen.WriteStringLn('=[];');

   end else raise ECodeGenUnsupportedSymbol.CreateFmt('InitData for %s', [t.ClassName]);
end;

// ------------------
// ------------------ TJSAssignConstToIntegerVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignConstToIntegerVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToIntegerVarExpr;
begin
   e:=TAssignConstToIntegerVarExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString('=');
   codeGen.WriteString(IntToStr(e.Right));
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignConstToStringVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignConstToStringVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToStringVarExpr;
begin
   e:=TAssignConstToStringVarExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString('=');
   WriteJavaScriptString(codeGen.Output, e.Right);
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignConstToFloatVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignConstToFloatVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToFloatVarExpr;
begin
   e:=TAssignConstToFloatVarExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString('=');
   codeGen.WriteString(FloatToStr(e.Right, cFormatSettings));
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignConstToBoolVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignConstToBoolVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToBoolVarExpr;
begin
   e:=TAssignConstToBoolVarExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString('=');
   codeGen.WriteString(cBoolToJSBool[e.Right]);
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignNilToVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignNilToVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignNilToVarExpr;
begin
   e:=TAssignNilToVarExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteStringLn('=null;');
end;

// ------------------
// ------------------ TJSAssignConstDataToVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignConstDataToVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstDataToVarExpr;
begin
   e:=TAssignConstDataToVarExpr(expr);
//   if e.Left.Typ is TRecordSymbol then begin
      codeGen.Compile(e.Left);
      codeGen.WriteString('=');
      codeGen.Compile(e.Right);
//   end else raise ECodeGenUnsupportedSymbol.CreateFmt('Unsupported %s on type %s', [e.ClassName, e.Left.Typ.ClassName]);
   codeGen.WriteStringLn(';');
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
   WriteJavaScriptString(codeGen.Output, e.AppendString);
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSConstExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstExpr;
begin
   e:=TConstExpr(expr);
   TdwsJSCodeGen(codeGen).WriteValue(e.Typ, e.Data[nil], 0);
end;

// ------------------
// ------------------ TJSConstIntExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstIntExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstIntExpr;
begin
   e:=TConstIntExpr(expr);
   codeGen.WriteString(IntToStr(e.Value));
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
   WriteJavaScriptString(codeGen.Output, e.Value);
end;

// ------------------
// ------------------ TJSConstFloatExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstFloatExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstFloatExpr;
begin
   e:=TConstFloatExpr(expr);
   codeGen.WriteString(FloatToStr(e.Value, cFormatSettings));
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
// ------------------ TJSArrayConstantExpr ------------------
// ------------------

// CodeGen
//
procedure TJSArrayConstantExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TArrayConstantExpr;
   i : Integer;
begin
   e:=TArrayConstantExpr(expr);
   codeGen.WriteString('[');
   for i:=0 to e.SubExprCount-1 do begin
      if i>0 then
         codeGen.WriteString(',');
      codeGen.Compile(e.SubExpr[i]);
   end;
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
   // TODO: deep copy of records & static arrays
   e:=TAssignExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString('=');
   codeGen.CompileNoWrap(e.Right);
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignDataExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignDataExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignDataExpr;
   lt : TTypeSymbol;
begin
   // TODO: deep copy of records & static arrays
   e:=TAssignDataExpr(expr);
   lt:=e.Left.Typ.UnAliasedType;
   if lt is TStaticArraySymbol then begin

      codeGen.Compile(e.Left);
      codeGen.WriteString('=');
      codeGen.CompileNoWrap(e.Right);
      if not (e.Right is TArrayConstantExpr) then
         codeGen.WriteString('.slice(0)');

   end else if lt is TRecordSymbol then begin

      codeGen.Compile(e.Left);
      codeGen.WriteString('=JSON.parse(JSON.stringify(');
      codeGen.CompileNoWrap(e.Right);
      codeGen.WriteString('))');

   end else if lt is TDynamicArraySymbol then begin

      codeGen.Compile(e.Left);
      codeGen.WriteString('=');
      codeGen.CompileNoWrap(e.Right);

   end else raise ECodeGenUnsupportedSymbol.CreateFmt('Unsupported %s on type %s',
                                                      [e.ClassName, e.Left.Typ.ClassName]);
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignClassOfExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignClassOfExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignClassOfExpr;
begin
   // TODO: deep copy of records & static arrays
   e:=TAssignClassOfExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString('=');
   codeGen.CompileNoWrap(e.Right);
   if e.Right.Typ is TClassSymbol then
      codeGen.WriteStringLn('.ClassType');
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignFuncExpr;
   funcExpr : TFuncExprBase;
begin
   e:=TAssignFuncExpr(expr);

   codeGen.Compile(e.Left);
   codeGen.WriteString('=');

   funcExpr:=(e.Right as TFuncExprBase);

   TJSFuncRefExpr.DoCodeGen(codeGen, funcExpr);

   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSFuncBaseExpr ------------------
// ------------------

// CodeGen
//
procedure TJSFuncBaseExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);

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
   e : TFuncExprBase;
   i : Integer;
   funcSym : TFuncSymbol;
   paramExpr : TTypedExpr;
   paramSymbol : TParamSymbol;
   readBack : TStringList;
begin
   // TODO: handle deep copy of records, lazy params
   e:=TFuncExprBase(expr);
   funcSym:=e.FuncSym;

   readBack:=TStringList.Create;
   try
      CodeGenFunctionName(codeGen, e, funcSym);
      codeGen.WriteString('(');
      CodeGenBeginParams(codeGen, e);
      for i:=0 to e.Args.Count-1 do begin
         if i>0 then
            codeGen.WriteString(',');
         paramExpr:=e.Args.ExprBase[i] as TTypedExpr;
         paramSymbol:=funcSym.Params[i] as TParamSymbol;
         if (paramSymbol is TVarParamSymbol) then begin
            if paramExpr is TVarExpr then
               TJSVarExpr.CodeGenName(codeGen, TVarExpr(paramExpr))
            else begin
               codeGen.WriteString('{value:');
               codeGen.Compile(paramExpr);
               codeGen.WriteString('}');
            end;
         end else if paramSymbol is TByRefParamSymbol then begin
            codeGen.Compile(paramExpr);
         end else if paramSymbol is TLazyParamSymbol then begin
            codeGen.WriteString('function () { return ');
            codeGen.Compile(paramExpr);
            codeGen.WriteString('}');
         end else begin
            if paramSymbol.Typ is TRecordSymbol then begin
               codeGen.WriteString('JSON.parse(JSON.stringify(');
               codeGen.Compile(paramExpr);
               codeGen.WriteString('))');
            end else if paramSymbol.Typ is TStaticArraySymbol then begin
               if (paramExpr is TArrayConstantExpr) and not NeedArrayCopy(TArrayConstantExpr(paramExpr)) then begin
                  codeGen.Compile(paramExpr);
               end else begin
                  codeGen.WriteString('JSON.parse(JSON.stringify(');
                  codeGen.Compile(paramExpr);
                  codeGen.WriteString('))');
               end;
            end else begin
               codeGen.CompileNoWrap(paramExpr);
            end;
         end;
      end;
      codeGen.WriteString(')');
   finally
      readBack.Free;
   end;
end;

// CodeGenFunctionName
//
procedure TJSFuncBaseExpr.CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol);
begin
   if funcSym is TMethodSymbol then
      codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(funcSym, TMethodSymbol(funcSym).StructSymbol))
   else codeGen.WriteSymbolName(funcSym);
   if FVirtualCall then
      codeGen.WriteString('$v');
end;

// CodeGenBeginParams
//
procedure TJSFuncBaseExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
begin
   // nothing here
end;

// ------------------
// ------------------ TJSMagicFuncExpr ------------------
// ------------------

// Create
//
constructor TJSMagicFuncExpr.Create;
begin
   inherited;
   FMagicCodeGens:=TStringList.Create;
   FMagicCodeGens.CaseSensitive:=False;
   FMagicCodeGens.Sorted:=True;
   FMagicCodeGens.Duplicates:=dupError;

   FMagicCodeGens.AddObject('Abs', TdwsExprGenericCodeGen.Create(['Math.abs(', 0, ')']));
   FMagicCodeGens.AddObject('AnsiLowerCase', TdwsExprGenericCodeGen.Create(['(', 0, ').toLocaleLowerCase()']));
   FMagicCodeGens.AddObject('AnsiUpperCase', TdwsExprGenericCodeGen.Create(['(', 0, ').toLocaleUpperCase()']));
   FMagicCodeGens.AddObject('ArcCos', TdwsExprGenericCodeGen.Create(['Math.acos(', 0, ')']));
   FMagicCodeGens.AddObject('ArcSin', TdwsExprGenericCodeGen.Create(['Math.asin(', 0, ')']));
   FMagicCodeGens.AddObject('ArcTan', TdwsExprGenericCodeGen.Create(['Math.atan(', 0, ')']));
   FMagicCodeGens.AddObject('ArcTan2', TdwsExprGenericCodeGen.Create(['Math.atan2(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Ceil', TdwsExprGenericCodeGen.Create(['Math.ceil(', 0, ')']));
   FMagicCodeGens.AddObject('Cos', TdwsExprGenericCodeGen.Create(['Math.cos(', 0, ')']));
   FMagicCodeGens.AddObject('Copy', TdwsExprGenericCodeGen.Create(['(', 0, ').substr((', 1, ')-1,', 2, ')']));
   FMagicCodeGens.AddObject('Exp', TdwsExprGenericCodeGen.Create(['Math.exp(', 0, ')']));
   FMagicCodeGens.AddObject('Floor', TdwsExprGenericCodeGen.Create(['Math.floor(', 0, ')']));
   FMagicCodeGens.AddObject('HexToInt', TdwsExprGenericCodeGen.Create(['parseInt(', 0, ',16)']));
   FMagicCodeGens.AddObject('IntPower', TdwsExprGenericCodeGen.Create(['Math.pow(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('IntToStr', TdwsExprGenericCodeGen.Create(['(', 0, ').toString()']));
   FMagicCodeGens.AddObject('LeftStr', TdwsExprGenericCodeGen.Create(['(', 0, ').substr(0,', 1, ')']));
   FMagicCodeGens.AddObject('Ln', TdwsExprGenericCodeGen.Create(['Math.log(', 0, ')']));
   FMagicCodeGens.AddObject('LowerCase', TdwsExprGenericCodeGen.Create(['(', 0, ').toLowerCase()']));
   FMagicCodeGens.AddObject('MaxInt', TdwsExprGenericCodeGen.Create(['Math.max(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('MinInt', TdwsExprGenericCodeGen.Create(['Math.min(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Pi', TdwsExprGenericCodeGen.Create(['Math.PI']));
   FMagicCodeGens.AddObject('Pos', TdwsExprGenericCodeGen.Create(['(', 1, '.indexOf(', 0, ')+1)']));
   FMagicCodeGens.AddObject('PosEx', TdwsExprGenericCodeGen.Create(['(', 1, '.indexOf(', 0, ',(', 2, ')-1)+1)']));
   FMagicCodeGens.AddObject('Power', TdwsExprGenericCodeGen.Create(['Math.pow(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Round', TdwsExprGenericCodeGen.Create(['Math.round(', 0, ')']));
   FMagicCodeGens.AddObject('Sin', TdwsExprGenericCodeGen.Create(['Math.sin(', 0, ')']));
   FMagicCodeGens.AddObject('Sqrt', TdwsExprGenericCodeGen.Create(['Math.sqrt(', 0, ')']));
   FMagicCodeGens.AddObject('StrToFloat', TdwsExprGenericCodeGen.Create(['parseFloat(', 0, ')']));
   FMagicCodeGens.AddObject('StrToInt', TdwsExprGenericCodeGen.Create(['parseInt(', 0, ',10)']));
   FMagicCodeGens.AddObject('SubStr', TdwsExprGenericCodeGen.Create(['(', 0, ').substr((', 1, ')-1)']));
   FMagicCodeGens.AddObject('SubString', TdwsExprGenericCodeGen.Create(['(', 0, ').substr((', 1, ')-1,(', 2, ')-2)']));
   FMagicCodeGens.AddObject('Tan', TdwsExprGenericCodeGen.Create(['Math.tan(', 0, ')']));
   FMagicCodeGens.AddObject('UpperCase', TdwsExprGenericCodeGen.Create(['(', 0, ').toUpperCase()']));
end;

// Destroy
//
destructor TJSMagicFuncExpr.Destroy;
var
   i : Integer;
begin
   inherited;
   for i:=0 to FMagicCodeGens.Count-1 do
      FMagicCodeGens.Objects[i].Free;
   FMagicCodeGens.Free;
end;

// CodeGen
//
procedure TJSMagicFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   name : String;
   i : Integer;
begin
   e:=TMagicFuncExpr(expr);
   name:=e.FuncSym.QualifiedName;
   if cgoNoInlineMagics in codeGen.Options then
      i:=-1
   else i:=FMagicCodeGens.IndexOf(name);
   if i>=0 then
      TdwsExprCodeGen(FMagicCodeGens.Objects[i]).CodeGen(codeGen, expr)
   else begin
      codeGen.Dependencies.Add(name);
      inherited;
   end;
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

   if e.MethSym.StructSymbol.IsExternal then begin
      codeGen.Compile(e.BaseExpr);
   end else begin
      codeGen.Dependencies.Add('TObject');
      codeGen.WriteSymbolName((e.FuncSym as TMethodSymbol).StructSymbol);
   end;

   codeGen.WriteString('.');
   inherited;
end;

// CodeGenBeginParams
//
procedure TJSMethodStaticExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TMethodStaticExpr;
begin
   e:=TMethodStaticExpr(expr);
   if not e.MethSym.StructSymbol.IsExternal then begin
      codeGen.Compile(e.BaseExpr);
      if e.FuncSym.Params.Count>0 then
         codeGen.WriteString(',');
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
   FVirtualCall:=True;
   codeGen.WriteSymbolName(e.BaseExpr.Typ.UnAliasedType);
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
begin
   codeGen.Dependencies.Add('$Intf');

   codeGen.WriteString('$Intf');
end;

// CodeGenBeginParams
//
procedure TJSMethodInterfaceExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TMethodInterfaceExpr;
begin
   e:=TMethodInterfaceExpr(expr);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString(',');
   codeGen.WriteString(IntToStr(e.MethSym.VMTIndex));

   if e.FuncSym.Params.Count>0 then
      codeGen.WriteString(',');

   inherited;
end;

// ------------------
// ------------------ TJSClassMethodStaticExpr ------------------
// ------------------

// CodeGen
//
procedure TJSClassMethodStaticExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TClassMethodStaticExpr;
begin
   codeGen.Dependencies.Add('TObject');

   e:=TClassMethodStaticExpr(expr);
   codeGen.WriteSymbolName((e.FuncSym as TMethodSymbol).StructSymbol);
   codeGen.WriteString('.');
   inherited;
end;

// CodeGenBeginParams
//
procedure TJSClassMethodStaticExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TClassMethodStaticExpr;
begin
   e:=TClassMethodStaticExpr(expr);

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
   FVirtualCall:=True;
   if e.BaseExpr.Typ is TClassSymbol then
      codeGen.WriteSymbolName(e.BaseExpr.Typ.UnAliasedType)
   else codeGen.WriteSymbolName(e.BaseExpr.Typ.UnAliasedType.Typ);
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
begin
   codeGen.Dependencies.Add('TObject');

   e:=TConstructorStaticExpr(expr);
   codeGen.WriteSymbolName((e.FuncSym as TMethodSymbol).StructSymbol);
   codeGen.WriteString('.');
   inherited;
end;

// CodeGenBeginParams
//
procedure TJSConstructorStaticExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
var
   e : TConstructorStaticExpr;
begin
   e:=TConstructorStaticExpr(expr);
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
   FVirtualCall:=True;
   if e.BaseExpr.Typ is TClassSymbol then
      codeGen.WriteSymbolName(e.BaseExpr.Typ.UnAliasedType)
   else codeGen.WriteSymbolName(e.BaseExpr.Typ.UnAliasedType.Typ);
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
   codeGen.Compile(e.CodeExpr);
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
   methSym : TMethodSymbol;
begin
   if funcExpr is TMethodExpr then begin

      methExpr:=TMethodExpr(funcExpr);
      methSym:=TMethodSymbol(methExpr.funcSym);

      codeGen.Dependencies.Add('$Event');
      codeGen.WriteString('$Event(');
      codeGen.Compile(methExpr.BaseExpr);
      codeGen.WriteString(',');
      if methExpr is TMethodVirtualExpr then begin
         codeGen.Compile(methExpr.BaseExpr);
         codeGen.WriteString('.ClassType.');
         codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(methSym, methSym.StructSymbol));
         codeGen.WriteString('$v');
      end else if methExpr is TMethodStaticExpr then begin
         codeGen.WriteSymbolName(methSym.StructSymbol);
         codeGen.WriteString('.');
         codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(methSym, methSym.StructSymbol))
      end else begin
         raise ECodeGenUnknownExpression.CreateFmt('Unsupported AssignFuncExpr for %s', [methExpr.ClassName]);
      end;
      codeGen.WriteString(')');

   end else begin

      codeGen.WriteSymbolName(funcExpr.FuncSym);

      if funcExpr is TMagicFuncExpr then
         codeGen.Dependencies.Add(funcExpr.FuncSym.QualifiedName);

   end;
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
   wrapped : Boolean;
   writeOperand : TProc;
begin
   e:=TInOpExpr(expr);

   if e.Count=0 then begin
      codeGen.WriteString(cBoolToJSBool[false]);
      Exit;
   end;

   wrapped:=not((e.Left is TVarExpr) or (e.Left is TConstExpr));

   if wrapped then begin
      codeGen.WriteString('{f:function(){var v$=');
      codeGen.Compile(e.Left);
      codeGen.WriteString(';return ');
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
      codeGen.WriteString('(');
      TJSCaseExpr.CodeGenCondition(codeGen, cond, writeOperand);
      codeGen.WriteString(')');
   end;

   if e.Count>1 then
      codeGen.WriteString(')');

   if wrapped then
      codeGen.WriteString('}}.f()');
end;

// ------------------
// ------------------ TJSCaseExpr ------------------
// ------------------

// CodeGen
//
procedure TJSCaseExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   e : TCaseExpr;
   cond : TCaseCondition;
   tmp : String;
begin
   e:=TCaseExpr(expr);
   tmp:=codeGen.GetNewTempSymbol;
   codeGen.WriteString('{var ');
   codeGen.WriteString(tmp);
   codeGen.WriteString('=');
   codeGen.Compile(e.ValueExpr);
   codeGen.WriteStringLn(';');
   codeGen.Indent;
   for i:=0 to e.CaseConditions.Count-1 do begin
      if i>0 then
         codeGen.WriteString(' else ');
      codeGen.WriteString('if (');
      cond:=TCaseCondition(e.CaseConditions.List[i]);
      CodeGenCondition(codeGen, cond, procedure begin codeGen.WriteString(tmp) end);
      codeGen.WriteStringLn(') {');
      codeGen.Indent;
      codeGen.Compile(cond.TrueExpr);
      codeGen.UnIndent;
      codeGen.WriteStringLn('}');
   end;
   if e.ElseExpr<>nil then begin
      codeGen.WriteStringLn(' else {');
      codeGen.Indent;
      codeGen.Compile(e.ElseExpr);
      codeGen.UnIndent;
      codeGen.WriteStringLn('};');
   end;
   codeGen.UnIndent;
   codeGen.WriteStringLn('};');
end;

// CodeGenCondition
//
class procedure TJSCaseExpr.CodeGenCondition(codeGen : TdwsCodeGen; cond : TCaseCondition;
                                             const writeOperand : TProc);
begin
   if cond is TCompareCaseCondition then begin
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
begin
   if     (codeGen.Context is TdwsProcedure)
      and (TdwsProcedure(codeGen.Context).Func.Typ<>nil) then
      codeGen.WriteStringLn('return Result;')
   else codeGen.WriteStringLn('return;');
end;

// ------------------
// ------------------ TJSIncVarFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSIncVarFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TIncVarFuncExpr;
   right : TExprBase;
begin
   e:=TIncVarFuncExpr(expr);
   right:=e.Args[1];
   if (right is TConstIntExpr) and (TConstIntExpr(right).Value=1) then begin
      codeGen.WriteString('++');
      codeGen.Compile(e.Args[0]);
   end else begin
      codeGen.Dependencies.Add('$Inc');
      codeGen.WriteString('$Inc(');
      TJSVarExpr.CodeGenName(codeGen, TVarExpr(e.Args[0]));
      codeGen.WriteString(',');
      codeGen.Compile(e.Args[1]);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSDecVarFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSDecVarFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TDecVarFuncExpr;
   right : TExprBase;
begin
   e:=TDecVarFuncExpr(expr);
   right:=e.Args[1];
   if (right is TConstIntExpr) and (TConstIntExpr(right).Value=1) then begin
      codeGen.WriteString('--');
      codeGen.Compile(e.Args[0]);
   end else begin
      codeGen.Dependencies.Add('$Dec');
      codeGen.WriteString('$Dec(');
      TJSVarExpr.CodeGenName(codeGen, TVarExpr(e.Args[0]));
      codeGen.WriteString(',');
      codeGen.Compile(e.Args[1]);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSConvIntegerExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConvIntegerExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConvIntegerExpr;
begin
   e:=TConvIntegerExpr(expr);
   if e.Expr.Typ.UnAliasedType is TBaseBooleanSymbol then begin
      codeGen.WriteString('(');
      codeGen.Compile(e.Expr);
      codeGen.WriteString('?1:0)');
   end else codeGen.Compile(e.Expr);
end;

// ------------------
// ------------------ TJSConvClassExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConvClassExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConvClassExpr;
begin
   codeGen.Dependencies.Add('$Cast');

   e:=TConvClassExpr(expr);
   codeGen.WriteString('$Cast(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(',');
   codeGen.WriteSymbolName(e.Typ.UnAliasedType);
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
   codeGen.Dependencies.Add('$As');

   e:=TObjAsClassExpr(expr);
   codeGen.WriteString('$As(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(',');
   codeGen.WriteSymbolName(e.Typ.UnAliasedType);
   codeGen.WriteString(')');
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
   codeGen.Dependencies.Add('$Is');

   e:=TIsOpExpr(expr);
   codeGen.WriteString('$Is(');
   codeGen.Compile(e.Left);
   codeGen.WriteString(',');
   codeGen.WriteSymbolName(e.Right.Typ.UnAliasedType.Typ);
   codeGen.WriteString(')');
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
   codeGen.WriteString(e.Typ.UnAliasedType.Name);
   codeGen.WriteString('")');
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
   codeGen.WriteString(e.Typ.UnAliasedType.Name);
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
var
   i : Integer;
begin
   Result:=(TdwsJSCodeGen(codeGen).FLocalVarParams.IndexOf(sym)>=0);
   if Result then Exit;
   for i:=0 to TdwsJSCodeGen(codeGen).FLocalVarParamsStack.Count-1 do begin
      Result:=(TdwsJSCodeGen(codeGen).FLocalVarParamsStack.Items[i].IndexOf(sym)>=0);
      if Result then Exit;
   end;
end;

// WriteLocationString
//
class procedure TJSExprCodeGen.WriteLocationString(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   WriteJavaScriptString(codeGen.Output, codeGen.LocationString(expr));
end;

// ------------------
// ------------------ TJSRecordExpr ------------------
// ------------------

// CodeGen
//
procedure TJSRecordExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TRecordExpr;
   member : TFieldSymbol;
begin
   e:=TRecordExpr(expr);
   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('.');
   member:=(e.BaseExpr.Typ as TRecordSymbol).FieldAtOffset(e.MemberOffset);
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
   field : TFieldSymbol;
begin
   e:=TFieldExpr(expr);

   if cgoNoCheckInstantiated in codeGen.Options then begin
      codeGen.Compile(e.ObjectExpr);
   end else begin
      codeGen.Dependencies.Add('$Check');
      codeGen.WriteString('$Check(');
      codeGen.Compile(e.ObjectExpr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');
   end;

   codeGen.WriteString('.');
   field:=(e.ObjectExpr.Typ as TClassSymbol).FieldAtOffset(e.FieldAddr);
   codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(field, field.StructSymbol));
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
   codeGen.Dependencies.Add('$Is');
   codeGen.Dependencies.Add('Exception');

   e:=TExceptExpr(expr);
   codeGen.WriteStringLn('try {');
   codeGen.Indent;
   codeGen.Compile(e.TryExpr);
   codeGen.UnIndent;
   codeGen.WriteStringLn('} catch ($e) {');
   codeGen.Indent;
   if e.DoExprCount=0 then
      codeGen.Compile(e.HandlerExpr)
   else begin
      for i:=0 to e.DoExprCount-1 do begin
         de:=e.DoExpr[i];
         if i>0 then
            codeGen.WriteString('else ');
         codeGen.WriteString('if ($Is($e,');
         codeGen.WriteSymbolName(de.ExceptionVar.Typ.UnAliasedType);
         codeGen.WriteStringLn(')) {');
         codeGen.Indent;

         codeGen.WriteString('var ');
         codeGen.WriteSymbolName(de.ExceptionVar);
         codeGen.WriteStringLn('=$e;');
         codeGen.LocalTable.AddSymbolDirect(de.ExceptionVar);
         try
            codeGen.Compile(de.DoBlockExpr);
         finally
            codeGen.LocalTable.Remove(de.ExceptionVar);
         end;

         codeGen.UnIndent;
         codeGen.WriteStringLn('}');
      end;
      if e.ElseExpr<>nil then begin
         codeGen.WriteStringLn('else {');
         codeGen.Indent;

         codeGen.Compile(e.ElseExpr);

         codeGen.UnIndent;
         codeGen.WriteStringLn('}');
      end else codeGen.WriteStringLn('else throw $e');
   end;
   codeGen.UnIndent;
   codeGen.WriteStringLn('}');
end;

// ------------------
// ------------------ TJSNewArrayExpr ------------------
// ------------------

// CodeGen
//
procedure TJSNewArrayExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TNewArrayExpr;
   i : Integer;
begin
   e:=TNewArrayExpr(expr);

   codeGen.Dependencies.Add('$NewArray');

   for i:=0 to e.LengthExprCount-2 do begin
      codeGen.WriteString('$NewArray(');
      codeGen.Compile(e.LengthExpr[i]);
      codeGen.WriteString(',function (){return ');
   end;

   codeGen.WriteString('$NewArray(');
   codeGen.Compile(e.LengthExpr[e.LengthExprCount-1]);
   codeGen.WriteString(',function (){return ');
   (codeGen as TdwsJSCodeGen).WriteDefaultValue(e.Typ.Typ, False);
   codeGen.WriteString('})');

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
      codeGen.WriteString('+');
      codeGen.WriteString(IntToStr(e.Delta));
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
begin
   e:=TArraySetLengthExpr(expr);

   codeGen.Dependencies.Add('$ArraySetLength');

   codeGen.WriteString('$ArraySetLength(');
   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString(',');
   codeGen.Compile(e.LengthExpr);
   codeGen.WriteString(',function (){return ');
   (codeGen as TdwsJSCodeGen).WriteDefaultValue(e.BaseExpr.Typ.Typ, False);
   codeGen.WriteStringLn('});');
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
   codeGen.WriteString('.push(');
   codeGen.Compile(e.ItemExpr);
   codeGen.WriteStringLn(');');
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
   codeGen.WriteString('.splice(');
   codeGen.Compile(e.IndexExpr);
   codeGen.WriteString(',');
   if e.CountExpr<>nil then
      codeGen.Compile(e.CountExpr)
   else codeGen.WriteString('1');
   codeGen.WriteStringLn(');');
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
         codeGen.Compile(e.IndexExpr)
      else begin
         codeGen.WriteString('(');
         codeGen.Compile(e.IndexExpr);
         codeGen.WriteString(')-');
         codeGen.WriteString(IntToStr(typ.LowBound));
      end;

   end else begin

      codeGen.Dependencies.Add('$Idx');

      codeGen.WriteString('$Idx(');
      codeGen.Compile(e.IndexExpr);
      codeGen.WriteString(',');
      codeGen.WriteString(IntToStr(typ.LowBound));
      codeGen.WriteString(',');
      codeGen.WriteString(IntToStr(typ.HighBound));
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
      codeGen.WriteString(')');

   end;

   codeGen.WriteString(']');

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
      codeGen.Compile(e.IndexExpr);
      codeGen.WriteString(']');

   end else begin

      codeGen.Dependencies.Add('$DIdxR');

      codeGen.WriteString('$DIdxR(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      codeGen.Compile(e.IndexExpr);
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
      codeGen.WriteStringLn(';');

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
var
   e : TStringArrayOpExpr;
   noRangeCheck : Boolean;
begin
   e:=TStringArrayOpExpr(expr);

   noRangeCheck:=(cgoNoRangeChecks in codeGen.Options);

   if noRangeCheck then begin

      codeGen.Compile(e.Left);
      codeGen.WriteString('.charAt((');
      codeGen.Compile(e.Right);
      codeGen.WriteString(')-1)');

   end else begin

      codeGen.Dependencies.Add('$SIdx');

      codeGen.WriteString('$SIdx(');
      codeGen.Compile(e.Left);
      codeGen.WriteString(',');
      codeGen.Compile(e.Right);
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

   codeGen.Dependencies.Add('$StrSet');

   codeGen.Compile(e.StringExpr);
   codeGen.WriteString('=$StrSet(');
   codeGen.Compile(e.StringExpr);
   codeGen.WriteString(',');
   codeGen.Compile(e.IndexExpr);
   codeGen.WriteString(',');
   codeGen.Compile(e.ValueExpr);
   if not (cgoNoRangeChecks in codeGen.Options) then begin
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
   end;
   codeGen.WriteStringLn(');');
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
   codeGen.Compile(e.Cond);
   codeGen.WriteString(',');
   if e.Message<>nil then
      codeGen.Compile(e.Message)
   else codeGen.WriteString('""');
   codeGen.WriteString(',');
   WriteLocationString(codeGen, expr);
   codeGen.WriteStringLn(');');
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
begin
   e:=TForExpr(expr);

   if not e.ToExpr.IsConstant then begin
      tmpTo:=codeGen.GetNewTempSymbol;
      codeGen.WriteString('var ');
      codeGen.WriteString(tmpTo);
      codeGen.WriteStringLn(';');
   end else tmpTo:='';
   if (e is TForStepExpr) and not (TForStepExpr(e).StepExpr.IsConstant) then begin
      tmpStep:=codeGen.GetNewTempSymbol;
      codeGen.WriteString('var ');
      codeGen.WriteString(tmpStep);
      codeGen.WriteStringLn(';');
   end else tmpStep:='';

   codeGen.WriteString('for(');
   codeGen.Compile(e.VarExpr);
   codeGen.WriteString('=');
   codeGen.Compile(e.FromExpr);
   if tmpTo<>'' then begin
      codeGen.WriteString(',');
      codeGen.WriteString(tmpTo);
      codeGen.WriteString('=');
      codeGen.Compile(e.ToExpr);
   end;
   if tmpStep<>'' then begin
      codeGen.WriteString(',');
      codeGen.WriteString(tmpStep);
      if cgoNoCheckLoopStep in codeGen.Options then begin
         codeGen.WriteString('=');
         codeGen.Compile(TForStepExpr(e).StepExpr);
      end else begin
         codeGen.Dependencies.Add('$CheckStep');
         codeGen.WriteString('=$CheckStep(');
         codeGen.Compile(TForStepExpr(e).StepExpr);
         codeGen.WriteString(',');
         WriteLocationString(codeGen, e);
         codeGen.WriteString(')');
      end;
   end;
   codeGen.WriteString(';');
   codeGen.Compile(e.VarExpr);
   WriteCompare(codeGen);
   if tmpTo<>'' then
      codeGen.WriteString(tmpTo)
   else codeGen.Compile(e.ToExpr);
   codeGen.WriteString(';');
   codeGen.Compile(e.VarExpr);
   WriteStep(codeGen);
   if tmpStep<>'' then
      codeGen.WriteString(tmpStep)
   else if e is TForStepExpr then begin
      codeGen.Compile(TForStepExpr(e).StepExpr);
   end;
   codeGen.WriteStringLn(') {');
   codeGen.Indent;
   codeGen.Compile(e.DoExpr);
   codeGen.UnIndent;
   codeGen.WriteStringLn('}');
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
// ------------------ TJSSqrExpr ------------------
// ------------------

// CodeGen
//
procedure TJSSqrExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   if not (expr.SubExpr[0] is TVarExpr) then
      inherited
   else CodeGenNoWrap(codeGen, expr as TTypedExpr);
end;

// CodeGenNoWrap
//
procedure TJSSqrExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   expr:=expr.SubExpr[0] as TTypedExpr;
   if expr is TVarExpr then begin
      codeGen.Compile(expr);
      codeGen.WriteString('*');
      codeGen.Compile(expr);
   end else begin
      codeGen.WriteString('Math.pow(');
      codeGen.Compile(expr);
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
constructor TJSBinOpExpr.Create(const op : String; associative : Boolean);
begin
   inherited Create;
   FOp:=op;
   FAssociative:=associative;
end;

// CodeGen
//
procedure TJSBinOpExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TBinaryOpExpr;
begin
   e:=TBinaryOpExpr(expr);
   if FAssociative and (e.Left.ClassType=e.ClassType) then
      codeGen.CompileNoWrap(e.Left)
   else WriteWrappedIfNeeded(codeGen, e.Left);
   codeGen.WriteString(FOp);
   if FAssociative and (e.Right.ClassType=e.ClassType) then
      codeGen.CompileNoWrap(e.Right)
   else WriteWrappedIfNeeded(codeGen, e.Right);
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
// ------------------ TJSDefinedExpr ------------------
// ------------------

// CodeGen
//
procedure TJSDefinedExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TDefinedExpr;
begin
   e:=TDefinedExpr(expr);
   codeGen.Dependencies.Add('$ConditionalDefines');
   codeGen.WriteString('($ConditionalDefines.indexOf(');
   codeGen.Compile(e.Expr);
   codeGen.WriteString(')!=-1)');
end;

end.
