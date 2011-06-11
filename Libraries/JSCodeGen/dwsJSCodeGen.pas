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
   dwsExprs, dwsRelExprs, dwsJSON, dwsMagicExprs, dwsStack, Variants;

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

      public
         constructor Create; override;
         destructor Destroy; override;

         procedure CompileEnumerationSymbol(enum : TEnumerationSymbol); override;
         procedure CompileFuncSymbol(func : TSourceFuncSymbol); override;
         procedure CompileRecordSymbol(rec : TRecordSymbol); override;
         procedure CompileClassSymbol(cls : TClassSymbol); override;
         procedure CompileProgram(const prog : IdwsProgram); override;
         procedure CompileProgramBody(expr : TNoResultExpr); override;
         procedure CompileSymbolTable(table : TSymbolTable); override;

         procedure CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); override;

         function GetNewTempSymbol : String; override;

         procedure WriteJavaScriptString(const s : String);

         class function MemberName(sym : TSymbol; cls : TClassSymbol) : String; static;
   end;

   TJSExprCodeGen = class (TdwsExprCodeGen)
      class function IsLocalVarParam(codeGen : TdwsCodeGen; sym : TDataSymbol) : Boolean; static;
      class procedure WriteLocationString(codeGen : TdwsCodeGen; expr : TExprBase);
   end;

   TJSBlockInitExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSBlockExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSBlockExprNoTable = class (TJSExprCodeGen)
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

   TJSStaticArrayExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSStringArrayOpExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSVarStringArraySetExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSCaseExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAsOpExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSIsOpExpr = class (TJSExprCodeGen)
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
         procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); virtual;
   end;

   TJSMagicFuncExpr = class (TJSFuncBaseExpr)
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
      public
         constructor Create(const op : String);
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cBoolToJSBool : array [False..True] of String = ('false', 'true');

type
   TJSRTLDependency = record
      Name, Code, Dependency : String;
   end;
   PJSRTLDependency = ^TJSRTLDependency;
const
   cJSRTLDependencies : array [1..28] of TJSRTLDependency = (
      // codegen utility functions
      (Name : '$CheckStep';
       Code : 'function $CheckStep(s,z) { if (s>0) return s; throw Exception.Create$1($New(Exception),"FOR loop STEP should be strictly positive: "+s.toString()+z); }';
       Dependency : 'Exception' ),
      (Name : '$dwsRand';
       Code : 'var $dwsRand=0'),
      (Name : '$New';
       Code : 'function $New(c) { var i={ClassType:c}; c.$Init(i); return i }'),
      (Name : '$Check';
       Code : 'function $Check(i,z) { if (i) return i; throw Exception.Create$1($New(Exception),"Object not instantiated"+z); }';
       Dependency : 'Exception' ),
      (Name : '$Assert';
       Code : 'function $Assert(b,m,z) { if (!b) throw Exception.Create$1($New(EAssertionFailed),"Assertion failed"+z+((m=="")?"":" : ")+m); }';
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
               +'else throw Exception.Create$1($New(Exception),"Can''t cast instance of type \""+o.ClassType.$ClassName+"\" to class \""+c.$ClassName+"\""); }'; // TODO: exception
       Dependency : '$Is' ),
      (Name : '$Idx';
       Code : 'function $Idx(i,l,h,z) {'#13#10
               +#9'if (i<l) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>h) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return i-l;'#13#10
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
      // RTL functions
      (Name : 'BoolToStr';
       Code : 'function BoolToStr(b) { return b?"True":"False" }'),
      (Name : 'Chr';
       Code : 'function Chr(c) { return String.fromCharCode(c) }'),
      (Name : 'FloatToStr';
       Code : 'function FloatToStr(i) { return i.toString() }'),
      (Name : 'IntToHex';
       Code : 'function IntToHex(v,d) { var hex=v.toString(16).toUpperCase(); return "00000000".substr(0, 8-d-hex.length)+hex; }'),
      (Name : 'IntToStr';
       Code : 'function IntToStr(i) { return i.toString() }'),
      (Name : 'MaxInt';
       Code : 'function MaxInt(a,b) { return (a>b)?a:b }'),
      (Name : 'MinInt';
       Code : 'function MinInt(a,b) { return (a<b)?a:b }'),
      (Name : 'Random';
       Code : 'function Random() { var tmp=Math.floor($dwsRand*0x08088405+1)%4294967296; $dwsRand=tmp; return tmp*Math.pow(2, -32) }';
       Dependency : '$dwsRand'),
      (Name : 'Round';
       Code : 'function Round(v) { return Math.round(v) }'),
      (Name : 'SetLength';
       Code : 'function SetLength(s,n) { if (s.value.length>n) s.value=s.value.substring(0,n); else while (s.value.length<n) s.value+=" "; }'),
      (Name : 'SetRandSeed';
       Code : 'function SetRandSeed(v) { $dwsRand = v }';
       Dependency : '$dwsRand'),
      (Name : 'Trunc';
       Code : 'function Trunc(v) { return (v>=0)?Math.floor(v):Math.ceil(v) }'),
      // RTL classes
      (Name : 'TObject';
       Code : 'var TObject={'#13#10
               +#9'$ClassName:"TObject",'#13#10
               +#9'ClassName:function(Self){return Self.$ClassName},'#13#10
               +#9'ClassType:function(Self){return Self},'#13#10
               +#9'$Init:function () {},'#13#10
               +#9'Create:function (Self) { return Self; }'#13#10
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

   RegisterCodeGen(TBlockInitExpr, TJSBlockInitExpr.Create);

   RegisterCodeGen(TBlockExpr,            TJSBlockExpr.Create);
   RegisterCodeGen(TBlockExprNoTable,     TJSBlockExprNoTable.Create);
   RegisterCodeGen(TBlockExprNoTable2,    TJSBlockExprNoTable.Create);
   RegisterCodeGen(TBlockExprNoTable3,    TJSBlockExprNoTable.Create);
   RegisterCodeGen(TBlockExprNoTable4,    TJSBlockExprNoTable.Create);

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
   RegisterCodeGen(TAsOpExpr,             TJSAsOpExpr.Create);
   RegisterCodeGen(TIsOpExpr,             TJSIsOpExpr.Create);

   RegisterCodeGen(TAddStrExpr,           TJSBinOpExpr.Create('+'));

   RegisterCodeGen(TAddIntExpr,           TJSBinOpExpr.Create('+'));
   RegisterCodeGen(TAddFloatExpr,         TJSBinOpExpr.Create('+'));
   RegisterCodeGen(TSubIntExpr,           TJSBinOpExpr.Create('-'));
   RegisterCodeGen(TSubFloatExpr,         TJSBinOpExpr.Create('-'));
   RegisterCodeGen(TMultIntExpr,          TJSBinOpExpr.Create('*'));
   RegisterCodeGen(TMultFloatExpr,        TJSBinOpExpr.Create('*'));
   RegisterCodeGen(TDivideExpr,           TJSBinOpExpr.Create('/'));
   RegisterCodeGen(TDivExpr,
      TdwsExprGenericCodeGen.Create(['Math.floor(', 0, '/', 1, ')']));
   RegisterCodeGen(TModExpr,              TJSBinOpExpr.Create('%'));
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
      TdwsExprGenericCodeGen.Create([0, '.value+=', 1, '.value;'], True));
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

   RegisterCodeGen(TStaticArrayExpr,         TJSStaticArrayExpr.Create);
   RegisterCodeGen(TStringArrayOpExpr,       TJSStringArrayOpExpr.Create);
   RegisterCodeGen(TVarStringArraySetExpr,   TJSVarStringArraySetExpr.Create);
   RegisterCodeGen(TStringLengthExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, ').length']));
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
   RegisterCodeGen(TMagicProcedureExpr,   TJSMagicFuncExpr.Create);

   RegisterCodeGen(TConstructorStaticExpr,      TJSConstructorStaticExpr.Create);
   RegisterCodeGen(TConstructorVirtualExpr,     TJSConstructorVirtualExpr.Create);
   RegisterCodeGen(TConstructorVirtualObjExpr,  TJSConstructorVirtualExpr.Create);

   RegisterCodeGen(TMethodStaticExpr,           TJSMethodStaticExpr.Create);
   RegisterCodeGen(TMethodVirtualExpr,          TJSMethodVirtualExpr.Create);

   RegisterCodeGen(TClassMethodStaticExpr,      TJSClassMethodStaticExpr.Create);
   RegisterCodeGen(TClassMethodVirtualExpr,     TJSClassMethodVirtualExpr.Create);

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

// CompileEnumerationSymbol
//
procedure TdwsJSCodeGen.CompileEnumerationSymbol(enum : TEnumerationSymbol);
var
   i : Integer;
   elem : TElementSymbol;
begin
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
procedure TdwsJSCodeGen.CompileFuncSymbol(func : TSourceFuncSymbol);
var
   proc : TdwsProcedure;
begin
   proc:=(func.Executable as TdwsProcedure);
   if proc=nil then Exit;

   EnterContext(proc);
   try

      WriteString('function ');
      WriteString(func.Name);
      WriteString('(');
      WriteFuncParams(func);
      WriteStringLn(') {');
      Indent;
      CompileFuncBody(func);
      UnIndent;
      WriteStringLn('};');

   finally
      LeaveContext;
   end;
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

// CompileClassSymbol
//
procedure TdwsJSCodeGen.CompileClassSymbol(cls : TClassSymbol);
var
   i : Integer;
   sym : TSymbol;
   meth : TMethodSymbol;
begin
   inherited;

   WriteString('var ');
   WriteString(cls.Name);
   WriteStringLn('={');
   Indent;

   WriteString('$ClassName:');
   WriteJavaScriptString(cls.Name);
   WriteStringLn(',');
   WriteString('$Parent:');
   WriteString(cls.Parent.Name);
   WriteStringLn(',');

   Dependencies.Add('$New');

   WriteStringLn('$Init:function (Self) {');
   Indent;
   WriteString(cls.Parent.Name);
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
      if meth.Name='Destroy' then continue;
      if meth.ClassSymbol<>cls then begin
         WriteString(',');
         WriteString(MemberName(meth, meth.ClassSymbol));
         WriteString(':');
         WriteString(meth.ClassSymbol.Name+'.'+MemberName(meth, meth.ClassSymbol));
         WriteLineEnd;
      end;
      WriteString(',');
      WriteString(MemberName(meth, meth.ClassSymbol));
      WriteString('$v:');
      if meth.ClassSymbol=cls then begin
         if meth.Kind=fkConstructor then begin
            WriteString('function(Self){return Self.ClassType.');
            WriteString(MemberName(meth, meth.ClassSymbol));
            WriteStringLn('.apply(Self, arguments)}');
         end else if meth.IsClassMethod then begin
            WriteString('function(Self){return Self.');
            WriteString(MemberName(meth, meth.ClassSymbol));
            WriteStringLn('.apply(Self, arguments)}');
         end else begin
            WriteString('function(Self){return Self.ClassType.');
            WriteString(MemberName(meth, meth.ClassSymbol));
            WriteStringLn('.apply(Self.ClassType, arguments)}');
         end;
      end else begin
         WriteString(meth.ClassSymbol.Name+'.'+MemberName(meth, meth.ClassSymbol)+'$v');
         WriteLineEnd;
      end;
   end;

   UnIndent;
   WriteStringLn('};');
end;

// CompileProgram
//
procedure TdwsJSCodeGen.CompileProgram(const prog : IdwsProgram);
begin
   inherited;
end;

// CompileProgramBody
//
procedure TdwsJSCodeGen.CompileProgramBody(expr : TNoResultExpr);
begin
   if expr is TNullExpr then Exit;
   WriteStringLn('var $dws = function() {');
   Indent;
   inherited;
   UnIndent;
   WriteStringLn('};');
   WriteStringLn('$dws();');
end;

// CompileSymbolTable
//
procedure TdwsJSCodeGen.CompileSymbolTable(table : TSymbolTable);
var
   i : Integer;
   varSym : TDataSymbol;
begin
   inherited;
   for i:=0 to table.Count-1 do begin
      if table[i].ClassType=TDataSymbol then begin
         varSym:=TDataSymbol(table[i]);
         if FDeclaredLocalVars.IndexOf(varSym)<0 then begin
            FDeclaredLocalVars.Add(varSym);
            WriteString('var ');
            WriteString(varSym.Name);
            WriteString('=');
            WriteDefaultValue(varSym.Typ, TJSExprCodeGen.IsLocalVarParam(Self, varSym));
            WriteStringLn(';');
         end;
      end;
   end;
end;

// CompileDependencies
//
procedure TdwsJSCodeGen.CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram);
var
   processedDependencies : TStringList;

   procedure InsertDependency(dep : PJSRTLDependency);
   var
      sub : PJSRTLDependency;
   begin
      if FlushedDependencies.IndexOf(dep.Name)>=0 then Exit;
      
      if     (dep.Dependency<>'')
         and (processedDependencies.IndexOf(dep.Dependency)<0) then begin
         processedDependencies.Add(dep.Dependency);
         sub:=FindJSRTLDependency(dep.Dependency);
         if sub<>nil then
            InsertDependency(sub);
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
      begin
         if (expr is TVarExpr) and (parent is TFuncExprBase) then begin
            funcSym:=TFuncExprBase(parent).FuncSym;
            i:=parent.IndexOfSubExpr(expr);
            if (i>0) and (parent is TConstructorStaticExpr) then
               Dec(i);
            if (funcSym=nil) or (i>=funcSym.Params.Count) then // not supported yet
               Exit;
            if funcSym.Params[i] is TVarParamSymbol then begin
               varSym:=FindSymbolAtStackAddr(TVarExpr(expr).StackAddr, Context.Level);
               if FLocalVarParams.IndexOf(varSym)<0 then
                  FLocalVarParams.Add(varSym);
            end;
         end else if (expr is TExitExpr) then begin
            // exit with a try.. clause that modifies the result can cause issues
            // with JS immutability, this is a heavy-handed solution
            varSym:=LocalTable.FindSymbol('Result', cvMagic) as TDataSymbol;
            if FLocalVarParams.IndexOf(varSym)<0 then
               FLocalVarParams.Add(varSym);
         end;
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
   member : TMemberSymbol;
begin
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
   end else if typ is TRecordSymbol then begin
      recSym:=TRecordSymbol(typ);
      WriteString('{');
      for i:=0 to recSym.Members.Count-1 do begin
         if i>0 then
            WriteString(',');
         member:=TMemberSymbol(recSym.Members[i]);
         WriteString(member.Name);
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
   member : TMemberSymbol;
   sas : TStaticArraySymbol;
begin
   if typ is TBaseIntegerSymbol then
      WriteString(IntToStr(data[addr]))
   else if typ is TBaseFloatSymbol then
      WriteString(FloatToStr(data[addr]))
   else if typ is TBaseStringSymbol then
      WriteJavaScriptString(VarToStr(data[addr]))
   else if typ is TBaseBooleanSymbol then begin
      WriteString(cBoolToJSBool[Boolean(data[addr])])
   end else if typ is TNilSymbol then begin
      WriteString('null')
   end else if typ is TClassOfSymbol then begin
      WriteString(typ.Typ.Name)
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
         member:=TMemberSymbol(recSym.Members[i]);
         WriteString(member.Name);
         WriteString(':');
         WriteValue(member.Typ, data, addr+member.Offset);
      end;
      WriteString('}');
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
      WriteString(func.Params[i].Name);
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
begin
   proc:=(func.Executable as TdwsProcedure);
   if proc=nil then Exit;

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

   Compile(proc.InitExpr);

   CompileSymbolTable(proc.Table);

   Compile(proc.Expr);

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
   WriteString(MemberName(meth, meth.ClassSymbol));

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
class function TdwsJSCodeGen.MemberName(sym : TSymbol; cls : TClassSymbol) : String;
var
   n : Integer;
   match : TSymbol;
begin
   n:=0;
   cls:=cls.Parent;
   while cls<>nil do begin
      match:=cls.Members.FindSymbol(sym.Name, cvMagic);
      if match<>nil then begin
         if     (sym.ClassType=match.ClassType)
            and (sym is TMethodSymbol)
            and (TMethodSymbol(sym).IsVirtual)
            and (TMethodSymbol(sym).VMTIndex=TMethodSymbol(match).VMTIndex) then begin
            // method override
         end else Inc(n);
      end;
      cls:=cls.Parent;
   end;
   if n=0 then
      Result:=sym.Name
   else Result:=Format('%s$%d', [sym.Name, n]);
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
         codeGen.WriteString(sym.Name);
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
   i : Integer;
   block : TBlockExpr;
begin
   block:=TBlockExpr(expr);

   codeGen.WriteString('var ');
   codeGen.WriteString(codeGen.GetNewTempSymbol);
   codeGen.WriteStringLn('=function (){');
   codeGen.Indent;
   codeGen.CompileSymbolTable(block.Table);
   for i:=0 to block.SubExprCount-1 do begin
      codeGen.Compile(block.SubExpr[i]);
   end;
   codeGen.UnIndent;
   codeGen.WriteStringLn('}();');
end;

// ------------------
// ------------------ TJSBlockExprNoTable ------------------
// ------------------

// CodeGen
//
procedure TJSBlockExprNoTable.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
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
   codeGen.WriteString(Result.Name);
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
   codeGen.WriteString(sym.Name);
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
   codeGen.WriteString(FloatToStr(e.Right));
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
   codeGen.WriteString(FloatToStr(e.Value));
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
begin
   // TODO: deep copy of records & static arrays
   e:=TAssignDataExpr(expr);
   if e.Left.Typ is TStaticArraySymbol then begin
      codeGen.Compile(e.Left);
      codeGen.WriteString('=');
      codeGen.CompileNoWrap(e.Right);
      if not (e.Right is TArrayConstantExpr) then
         codeGen.WriteString('.slice(0)');
   end else raise ECodeGenUnsupportedSymbol.CreateFmt('Unsupported %s on type %s', [e.ClassName, e.Left.Typ.ClassName]);
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
   readBack : TStringList;
begin
   // TODO: handle deep copy of records, lazy params
   e:=TFuncExprBase(expr);
   funcSym:=e.FuncSym;

   readBack:=TStringList.Create;
   try
      if funcSym is TMethodSymbol then
         codeGen.WriteString(TdwsJSCodeGen.MemberName(funcSym, TMethodSymbol(funcSym).ClassSymbol))
      else codeGen.WriteString(funcSym.Name);
      if FVirtualCall then
         codeGen.WriteString('$v');
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
            if (paramSymbol.Typ is TRecordSymbol) or (paramSymbol.Typ is TArraySymbol) then begin
               codeGen.WriteString('JSON.parse(JSON.stringify(');
               codeGen.Compile(paramExpr);
               codeGen.WriteString('))');
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

// CodeGenBeginParams
//
procedure TJSFuncBaseExpr.CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase);
begin
   // nothing here
end;

// ------------------
// ------------------ TJSMagicFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSMagicFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);
   codeGen.Dependencies.Add(e.FuncSym.QualifiedName);
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
   codeGen.Dependencies.Add('TObject');

   e:=TMethodStaticExpr(expr);
   codeGen.WriteString((e.FuncSym as TMethodSymbol).ClassSymbol.Name);
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
   codeGen.Compile(e.BaseExpr);
   if e.FuncSym.Params.Count>0 then
      codeGen.WriteString(',');
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
   codeGen.WriteString(e.BaseExpr.Typ.UnAliasedType.Name);
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
   codeGen.WriteString((e.FuncSym as TMethodSymbol).ClassSymbol.Name);
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
      codeGen.WriteString(e.BaseExpr.Typ.UnAliasedType.Name)
   else codeGen.WriteString(e.BaseExpr.Typ.UnAliasedType.Typ.Name);
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
   codeGen.WriteString((e.FuncSym as TMethodSymbol).ClassSymbol.Name);
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
   codeGen.WriteString('$New(');
   codeGen.WriteString(e.Typ.UnAliasedType.Name);
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
      codeGen.WriteString(e.BaseExpr.Typ.UnAliasedType.Name)
   else codeGen.WriteString(e.BaseExpr.Typ.UnAliasedType.Typ.Name);
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
   codeGen.WriteString('$New(');
   codeGen.Compile(e.BaseExpr);
   if e.BaseExpr.Typ is TClassSymbol then
      codeGen.WriteString('.ClassType');
   codeGen.WriteString(')');
   if e.FuncSym.Params.Count>0 then
      codeGen.WriteString(',');
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
begin
   e:=TCaseExpr(expr);
   codeGen.WriteString('{var $case=');
   codeGen.Compile(e.ValueExpr);
   codeGen.WriteStringLn(';');
   codeGen.Indent;
   for i:=0 to e.CaseConditions.Count-1 do begin
      if i>0 then
         codeGen.WriteString(' else ');
      codeGen.WriteString('if (');
      cond:=TCaseCondition(e.CaseConditions.List[i]);
      if cond is TCompareCaseCondition then begin
         codeGen.WriteString('$case==');
         codeGen.Compile(TCompareCaseCondition(cond).CompareExpr);
      end else if cond is TRangeCaseCondition then begin
         codeGen.WriteString('($case>=');
         codeGen.Compile(TRangeCaseCondition(cond).FromExpr);
         codeGen.WriteString(')&&($case<=');
         codeGen.Compile(TRangeCaseCondition(cond).ToExpr);
         codeGen.WriteString(')');
      end else raise ECodeGenUnknownExpression.Create(cond.ClassName);
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
   e : TIncVarFuncExpr;
   right : TExprBase;
begin
   e:=TIncVarFuncExpr(expr);
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
   codeGen.WriteString(e.Typ.UnAliasedType.Name);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSAsOpExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAsOpExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAsOpExpr;
begin
   codeGen.Dependencies.Add('$As');

   e:=TAsOpExpr(expr);
   codeGen.WriteString('$As(');
   codeGen.Compile(e.Left);
   codeGen.WriteString(',');
   codeGen.WriteString(e.Typ.UnAliasedType.Name);
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
   codeGen.WriteString(e.Right.Typ.UnAliasedType.Typ.Name);
   codeGen.WriteString(')');
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
   member : TMemberSymbol;
begin
   e:=TRecordExpr(expr);
   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('.');
   member:=(e.BaseExpr.Typ as TRecordSymbol).MemberAtOffset(e.MemberOffset);
   codeGen.WriteString(member.Name);
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
   codeGen.WriteString(TdwsJSCodeGen.MemberName(field, field.ClassSymbol));
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
         codeGen.WriteString(de.ExceptionVar.Typ.UnAliasedType.Name);
         codeGen.WriteStringLn(')) {');
         codeGen.Indent;

         codeGen.WriteString('var ');
         codeGen.WriteString(de.ExceptionVar.Name);
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
      end;
   end;
   codeGen.UnIndent;
   codeGen.WriteStringLn('}');
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
constructor TJSBinOpExpr.Create(const op : String);
begin
   inherited Create;
   FOp:=op;
end;

// CodeGen
//
procedure TJSBinOpExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TBinaryOpExpr;
begin
   e:=TBinaryOpExpr(expr);
   if e.Left.ClassType=e.ClassType then
      codeGen.CompileNoWrap(e.Left)
   else WriteWrappedIfNeeded(codeGen, e.Left);
   codeGen.WriteString(FOp);
   if e.Right.ClassType=e.ClassType then
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
