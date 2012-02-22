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
{    Copyright Creative IT.                                            }
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsJSCodeGen;

interface

uses Classes, SysUtils, dwsUtils, dwsSymbols, dwsCodeGen, dwsCoreExprs,
   dwsExprs, dwsRelExprs, dwsJSON, dwsMagicExprs, dwsStack, Variants, dwsStrings,
   dwsJSLibModule, dwsJSMin;

type

   TDataSymbolList = class(TObjectList<TDataSymbol>)
      public
         destructor Destroy; override;
   end;

   TdwsCodeGenSymbolMapJSObfuscating = class (TdwsCodeGenSymbolMap)
      protected
         function DoNeedUniqueName(symbol : TSymbol; tryCount : Integer; canObfuscate : Boolean) : String; override;
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

         function CreateSymbolMap(parentMap : TdwsCodeGenSymbolMap; symbol : TSymbol) : TdwsCodeGenSymbolMap; override;

         procedure EnterContext(proc : TdwsProgram); override;
         procedure LeaveContext; override;

         function  SameDefaultValue(typ1, typ2 : TTypeSymbol) : Boolean;
         procedure WriteDefaultValue(typ : TTypeSymbol; box : Boolean);
         procedure WriteValue(typ : TTypeSymbol; const data : TData; addr : Integer);
         procedure WriteStringArray(destStream : TWriteOnlyBlockStream; strings : TStrings); overload;
         procedure WriteStringArray(strings : TStrings); overload;

         procedure WriteFuncParams(func : TFuncSymbol);
         procedure CompileFuncBody(func : TFuncSymbol);
         procedure CompileMethod(meth : TMethodSymbol);
         procedure CompileRecordMethod(meth : TMethodSymbol);

         procedure DoCompileRecordSymbol(rec : TRecordSymbol); override;
         procedure DoCompileClassSymbol(cls : TClassSymbol); override;
         procedure DoCompileFieldsInit(cls : TClassSymbol);
         procedure DoCompileInterfaceTable(cls : TClassSymbol);
         procedure DoCompileFuncSymbol(func : TSourceFuncSymbol); override;

      public
         constructor Create; override;
         destructor Destroy; override;

         function  SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : String; override;

         procedure CompileValue(expr : TTypedExpr); override;

         procedure CompileEnumerationSymbol(enum : TEnumerationSymbol); override;
         procedure CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                     preConds : Boolean); override;
         procedure CompileProgramBody(expr : TNoResultExpr); override;
         procedure CompileSymbolTable(table : TSymbolTable); override;

         procedure ReserveSymbolNames; override;

         procedure CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); override;

         function GetNewTempSymbol : String; override;

         procedure WriteSymbolVerbosity(sym : TSymbol); override;

         procedure WriteJavaScriptString(const s : String);

         function MemberName(sym : TSymbol; cls : TStructuredTypeSymbol) : String;

         procedure WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram); override;

         // returns all the RTL support JS functions
         class function All_RTL_JS : String;
         // removes all RTL dependencies (use in combination with All_RTL_JS)
         procedure IgnoreRTLDependencies;

         const cBoxFieldName = 'v';

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

   TJSNoResultWrapperExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSExitExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssignExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); virtual;
   end;
   TJSAssignDataExpr = class (TJSAssignExpr)
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignClassOfExpr = class (TJSAssignExpr)
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSAssignFuncExpr = class (TJSAssignExpr)
      procedure CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSInitDataExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
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
   TJSAssignConstDataToVarExpr = class (TJSAssignExpr)
      procedure CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAppendConstStringVarExpr = class (TJSExprCodeGen)
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
   TJSArrayConstantExpr = class (TJSConstExpr)
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
   TJSArrayIndexOfExpr = class (TJSExprCodeGen)
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

   TJSBitwiseInOpExpr = class (TJSExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
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

   TJSClassAsClassExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConvIntegerExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConvFloatExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSOrdExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
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

   TJSFuncBaseExpr = class (TJSExprCodeGen)
      private
         FVirtualCall : Boolean;
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); virtual;
         procedure CodeGenBeginParams(codeGen : TdwsCodeGen; expr : TFuncExprBase); virtual;
   end;

   TJSRecordMethodExpr = class (TJSFuncBaseExpr)
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

   TJSConnectorCallExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConnectorReadExpr = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TJSConnectorWriteExpr = class (TJSExprCodeGen)
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
         procedure WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr); virtual;
      public
         constructor Create(const op : String; associative : Boolean);
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSAddOpExpr = class(TJSBinOpExpr)
      protected
         procedure WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr); override;
      public
         constructor Create;
   end;

   TJSSubOpExpr = class(TJSBinOpExpr)
      protected
      public
         constructor Create;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
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
   RegisterCodeGen(TNoResultWrapperExpr,  TJSNoResultWrapperExpr.Create);

   RegisterCodeGen(TConstExpr,            TJSConstExpr.Create);
   RegisterCodeGen(TUnifiedConstExpr,     TJSConstExpr.Create);
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

   RegisterCodeGen(TAssignArrayConstantExpr, TdwsExprGenericCodeGen.Create([0, '=', -1, ';'], True));

   RegisterCodeGen(TVarExpr,              TJSVarExpr.Create);
   RegisterCodeGen(TSelfVarExpr,          TJSVarExpr.Create);
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
   RegisterCodeGen(TConvFloatExpr,        TJSConvFloatExpr.Create);
   RegisterCodeGen(TConvBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '?true:false)']));
   RegisterCodeGen(TConvStringExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '.toString()', ')']));
   RegisterCodeGen(TConvStaticArrayToDynamicExpr,
      TdwsExprGenericCodeGen.Create([0, '.slice()']));

   RegisterCodeGen(TOrdExpr,              TJSOrdExpr.Create);

   RegisterCodeGen(TClassAsClassExpr,     TJSClassAsClassExpr.Create);
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

   RegisterCodeGen(TAddIntExpr,           TJSAddOpExpr.Create);
   RegisterCodeGen(TAddFloatExpr,         TJSAddOpExpr.Create);
   RegisterCodeGen(TAddVariantExpr,       TJSAddOpExpr.Create);
   RegisterCodeGen(TSubIntExpr,           TJSSubOpExpr.Create);
   RegisterCodeGen(TSubFloatExpr,         TJSSubOpExpr.Create);
   RegisterCodeGen(TSubVariantExpr,       TJSSubOpExpr.Create);
   RegisterCodeGen(TMultIntExpr,          TJSBinOpExpr.Create('*', True));
   RegisterCodeGen(TMultFloatExpr,        TJSBinOpExpr.Create('*', True));
   RegisterCodeGen(TMultVariantExpr,      TJSBinOpExpr.Create('*', True));
   RegisterCodeGen(TDivideExpr,           TJSBinOpExpr.Create('/', True));
   RegisterCodeGen(TDivExpr,
      TdwsExprGenericCodeGen.Create(['Math.floor(', 0, '/', 1, ')']));
   RegisterCodeGen(TModExpr,              TJSBinOpExpr.Create('%', True));
   RegisterCodeGen(TSqrFloatExpr,         TJSSqrExpr.Create);
   RegisterCodeGen(TSqrIntExpr,           TJSSqrExpr.Create);
   RegisterCodeGen(TNegIntExpr,           TdwsExprGenericCodeGen.Create(['(', '-', 0, ')']));
   RegisterCodeGen(TNegFloatExpr,         TdwsExprGenericCodeGen.Create(['(', '-', 0, ')']));
   RegisterCodeGen(TNegVariantExpr,       TdwsExprGenericCodeGen.Create(['(', '-', 0, ')']));

   RegisterCodeGen(TAppendStringVarExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', -1, ';'], True));
   RegisterCodeGen(TAppendConstStringVarExpr,      TJSAppendConstStringVarExpr.Create);

   RegisterCodeGen(TPlusAssignIntExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', -1, ';'], True));
   RegisterCodeGen(TPlusAssignFloatExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', -1, ';'], True));
   RegisterCodeGen(TPlusAssignStrExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', -1, ';'], True));
   RegisterCodeGen(TPlusAssignExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', -1, ';'], True));
   RegisterCodeGen(TMinusAssignIntExpr,
      TdwsExprGenericCodeGen.Create([0, '-=', -1, ';'], True));
   RegisterCodeGen(TMinusAssignFloatExpr,
      TdwsExprGenericCodeGen.Create([0, '-=', -1, ';'], True));
   RegisterCodeGen(TMinusAssignExpr,
      TdwsExprGenericCodeGen.Create([0, '-=', -1, ';'], True));
   RegisterCodeGen(TMultAssignIntExpr,
      TdwsExprGenericCodeGen.Create([0, '*=', -1, ';'], True));
   RegisterCodeGen(TMultAssignFloatExpr,
      TdwsExprGenericCodeGen.Create([0, '*=', -1, ';'], True));
   RegisterCodeGen(TMultAssignExpr,
      TdwsExprGenericCodeGen.Create([0, '*=', -1, ';'], True));
   RegisterCodeGen(TDivideAssignExpr,
      TdwsExprGenericCodeGen.Create([0, '/=', -1, ';'], True));

   RegisterCodeGen(TIncIntVarExpr,
      TdwsExprGenericCodeGen.Create([0, '+=', -1, ';'], True));
   RegisterCodeGen(TDecIntVarExpr,
      TdwsExprGenericCodeGen.Create([0, '-=', -1, ';'], True));

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

   RegisterCodeGen(TSarExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>>', 1, ')']));
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

   RegisterCodeGen(TVariantAndExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '&', 1, ')']));
   RegisterCodeGen(TVariantOrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '|', 1, ')']));
   RegisterCodeGen(TVariantXorExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '^', 1, ')']));
   RegisterCodeGen(TNotVariantExpr,
      TdwsExprGenericCodeGen.Create(['(', '!', 0, ')']));

   RegisterCodeGen(TAssignedInstanceExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null', ')']));
   RegisterCodeGen(TAssignedMetaClassExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null', ')']));
   RegisterCodeGen(TAssignedFuncPtrExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==null', ')']));

   RegisterCodeGen(TRelEqualBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '==', 1, ')']));
   RegisterCodeGen(TRelNotEqualBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!=', 1, ')']));

   RegisterCodeGen(TObjCmpEqualExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '===', 1, ')']));
   RegisterCodeGen(TObjCmpNotEqualExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!==', 1, ')']));

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

   RegisterCodeGen(TRelEqualVariantExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '==', 1, ')']));
   RegisterCodeGen(TRelNotEqualVariantExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '!=', 1, ')']));
   RegisterCodeGen(TRelGreaterEqualVariantExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>=', 1, ')']));
   RegisterCodeGen(TRelLessEqualVariantExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<=', 1, ')']));
   RegisterCodeGen(TRelGreaterVariantExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '>', 1, ')']));
   RegisterCodeGen(TRelLessVariantExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '<', 1, ')']));

   RegisterCodeGen(TIfThenExpr,
      TdwsExprGenericCodeGen.Create(['if ', '(', 0, ')', ' {', #9, 1, #8, '};'], True));
   RegisterCodeGen(TIfThenElseExpr,
      TdwsExprGenericCodeGen.Create(['if ', '(', 0, ')', ' {', #9, 1, #8, '} else {', #9, 2, #8, '};'], True));

   RegisterCodeGen(TInOpExpr,             TJSInOpExpr.Create);
   RegisterCodeGen(TBitwiseInOpExpr,      TJSBitwiseInOpExpr.Create);
   RegisterCodeGen(TCaseExpr,             TJSCaseExpr.Create);

   RegisterCodeGen(TForUpwardExpr,        TJSForUpwardExpr.Create);
   RegisterCodeGen(TForDownwardExpr,      TJSForDownwardExpr.Create);
   RegisterCodeGen(TForUpwardStepExpr,    TJSForUpwardStepExpr.Create);
   RegisterCodeGen(TForDownwardStepExpr,  TJSForDownwardStepExpr.Create);

   RegisterCodeGen(TWhileExpr,
      TdwsExprGenericCodeGen.Create(['while ', '(', 0, ')', ' {', #9, 1, #8, '};'], True));
   RegisterCodeGen(TRepeatExpr,
      TdwsExprGenericCodeGen.Create(['do {', #9, 1, #8, '} while (!', 0, ');'], True));
   RegisterCodeGen(TLoopExpr,
      TdwsExprGenericCodeGen.Create(['while (1) {', #9, 1, #8, '};'], True));

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
   RegisterCodeGen(TArrayIndexOfExpr,        TJSArrayIndexOfExpr.Create);
   RegisterCodeGen(TArrayInsertExpr,         TJSArrayInsertExpr.Create);
   RegisterCodeGen(TArrayCopyExpr,           TJSArrayCopyExpr.Create);
   RegisterCodeGen(TArraySwapExpr,           TJSArraySwapExpr.Create);
   RegisterCodeGen(TArrayReverseExpr,        TdwsExprGenericCodeGen.Create([0, '.reverse();'], True));

   RegisterCodeGen(TStaticArrayExpr,         TJSStaticArrayExpr.Create);
   RegisterCodeGen(TDynamicArrayExpr,        TJSDynamicArrayExpr.Create);
   RegisterCodeGen(TDynamicArraySetExpr,     TJSDynamicArraySetExpr.Create);
   RegisterCodeGen(TStringArrayOpExpr,       TJSStringArrayOpExpr.Create);
   RegisterCodeGen(TVarStringArraySetExpr,   TJSVarStringArraySetExpr.Create);

   RegisterCodeGen(TStringLengthExpr,
      TdwsExprGenericCodeGen.Create([0, '.length']));
   RegisterCodeGen(TArrayLengthExpr,         TJSArrayLengthExpr.Create);
   RegisterCodeGen(TOpenArrayLengthExpr,
      TdwsExprGenericCodeGen.Create([0, '.length']));

   RegisterCodeGen(TOrdIntExpr,
      TdwsExprGenericCodeGen.Create([0]));
   RegisterCodeGen(TOrdBoolExpr,
      TdwsExprGenericCodeGen.Create(['(', 0, '?1:0)']));
   RegisterCodeGen(TOrdStrExpr,
      TdwsExprGenericCodeGen.Create(['$OrdS', '(', 0, ')'], False, '$OrdS'));

   RegisterCodeGen(TFuncExpr,             TJSFuncBaseExpr.Create);

   RegisterCodeGen(TRecordMethodExpr,     TJSRecordMethodExpr.Create);

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

   RegisterCodeGen(TConnectorCallExpr,          TJSConnectorCallExpr.Create);
   RegisterCodeGen(TConnectorReadExpr,          TJSConnectorReadExpr.Create);
   RegisterCodeGen(TConnectorWriteExpr,         TJSConnectorWriteExpr.Create);

   RegisterCodeGen(TFuncPtrExpr,                TJSFuncPtrExpr.Create);
   RegisterCodeGen(TFuncRefExpr,                TJSFuncRefExpr.Create);
   RegisterCodeGen(TAnonymousFuncRefExpr,       TJSAnonymousFuncRefExpr.Create);

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
begin
   exprTypClass:=expr.Typ.ClassType;

   if not (expr is TFuncExprBase) then begin

      if exprTypClass=TRecordSymbol then begin

         WriteString('Copy$');
         WriteSymbolName(expr.Typ);
         WriteString('(');
         CompileNoWrap(expr);
         WriteString(')');
         Exit;

      end else if (exprTypClass=TStaticArraySymbol) or (exprTypClass=TOpenArraySymbol) then begin

         CompileNoWrap(expr);
         if    (not (expr is TArrayConstantExpr))
            or NeedArrayCopy(TArrayConstantExpr(expr)) then
            WriteString('.slice(0)');
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

   if cgoOptimizeForSize in Options then Exit;

   WriteSymbolVerbosity(enum);

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
   if func.Name<>'' then
      WriteSymbolName(func);
   WriteString('(');
   WriteFuncParams(func);
   WriteStringLn(') {');
   Indent;

   CompileFuncBody(func);

   UnIndent;
   if func.Name<>'' then
      WriteStringLn('};')
   else WriteString('}');
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

// DoCompileRecordSymbol
//
procedure TdwsJSCodeGen.DoCompileRecordSymbol(rec : TRecordSymbol);
var
   i : Integer;
   sym : TSymbol;
   field : TFieldSymbol;
   firstField : Boolean;
begin
   // compile record copier

   if not SmartLink(rec) then Exit;

   WriteSymbolVerbosity(rec);

   WriteString('function Copy$');
   WriteSymbolName(rec);
   WriteStringLn('($s) {');
   Indent;
   WriteStringLn('return {');
   Indent;
   firstField:=True;
   for i:=0 to rec.Members.Count-1 do begin
      sym:=rec.Members[i];
      if sym is TFieldSymbol then begin
         field:=TFieldSymbol(sym);
         if firstField then
            firstField:=False
         else WriteString(',');
         WriteSymbolName(field);
         WriteString(':');
         if    (field.Typ is TBaseSymbol)
            or (field.Typ is TClassSymbol)
            or (field.Typ is TInterfaceSymbol)
            or (field.Typ is TFuncSymbol)
            or (field.Typ is TDynamicArraySymbol) then begin
            WriteString('$s.');
            WriteSymbolName(field)
         end else if field.Typ is TRecordSymbol then begin
            WriteString('Copy$');
            WriteSymbolName(field.Typ);
            WriteString('($s.');
            WriteSymbolName(field);
            WriteString(')');
         end else if field.Typ is TStaticArraySymbol then begin
            WriteString('$s.');
            WriteSymbolName(field);
            WriteString('.slice(0)');
         end else raise ECodeGenUnsupportedSymbol.CreateFmt('Copy record field type %s', [field.Typ.ClassName]);
         WriteStringLn('');
      end;
   end;
   UnIndent;
   WriteStringLn('}');
   UnIndent;
   WriteStringLn('};');

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
begin
   inherited;

   if not SmartLink(cls) then Exit;

   WriteSymbolVerbosity(cls);

   WriteString('var ');
   WriteSymbolName(cls);
   WriteStringLn('= {');
   Indent;

   WriteString('$ClassName:');
   WriteJavaScriptString(cls.Name);
   WriteStringLn(',');
   WriteString('$Parent:');
   WriteSymbolName(cls.Parent);
   WriteStringLn(',');

   Dependencies.Add('$New');
   Dependencies.Add('TObject');

   WriteStringLn('$Init:function ($) {');
   Indent;
   WriteSymbolName(cls.Parent);
   WriteStringLn('.$Init($);');
   DoCompileFieldsInit(cls);
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
      if not SmartLinkMethod(meth) then continue;
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
      WriteString('$'+cBoxFieldName+':');
      if meth.StructSymbol=cls then begin
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

// DoCompileFieldsInit
//
procedure TdwsJSCodeGen.DoCompileFieldsInit(cls : TClassSymbol);
var
   i, j, n : Integer;
   sym, sym2 : TSymbol;
   flds : array of TFieldSymbol;
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
      sym:=flds[i];
      if sym=nil then continue;
      for j:=i to n-1 do begin
         sym2:=flds[j];
         if sym2=nil then continue;
         if SameDefaultValue(sym2.Typ, sym.Typ) then begin
            WriteString('$.');
            WriteString(MemberName(sym2, cls));
            WriteString('=');
            flds[j]:=nil;
            // records, static arrays and other value types can't be assigned together
            if not ((sym.Typ is TBaseSymbol) or (sym.Typ is TClassSymbol) or (sym.Typ is TInterfaceSymbol)) then Break;
         end;
      end;
      WriteDefaultValue(sym.Typ, False);
      WriteStringLn(';');
   end;
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
      WriteStringLn('= function() {');
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
   declaredOne : Boolean;
begin
   inherited;
   declaredOne:=False;
   for sym in table do begin
      if sym.ClassType=TDataSymbol then begin
         varSym:=TDataSymbol(sym);
         if FDeclaredLocalVars.IndexOf(varSym)<0 then begin
            FDeclaredLocalVars.Add(varSym);
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
            WriteSymbolName(varSym);
            if varSym.Typ.ClassType=TBaseVariantSymbol then begin
               // undefined is JS default for unassigned var
            end else begin
               WriteString('=');
               WriteDefaultValue(varSym.Typ, TJSExprCodeGen.IsLocalVarParam(Self, varSym));
            end;
         end;
      end;
   end;
   if declaredOne then
      WriteStringLn(';');
end;

// ReserveSymbolNames
//
procedure TdwsJSCodeGen.ReserveSymbolNames;
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
            WriteStringArray(destStream, (prog as TdwsProgram).Root.ConditionalDefines.Value);
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
      i : Integer;
      funcSym : TFuncSymbol;
      symPos : TSymbolPosition;
   begin
      if sym is TClassSymbol then begin
         WriteString('/// ');
         WriteString(sym.QualifiedName);
         WriteString(' = class (');
         WriteString(TClassSymbol(sym).Parent.QualifiedName);
         WriteStringLn(')');
      end else if sym is TRecordSymbol then begin
         WriteString('/// ');
         WriteString(sym.QualifiedName);
         WriteStringLn(' = record');
      end else if sym is TFuncSymbol then begin
         funcSym:=TFuncSymbol(sym);
         WriteString('/// ');
         WriteString(cFuncKindToString[funcSym.Kind]);
         WriteString(' ');
         WriteString(funcSym.QualifiedName);
         WriteString(funcSym.ParamsDescription);
         if funcSym.Typ<>nil then begin
            WriteString(' : ');
            WriteString(funcSym.Typ.QualifiedName);
         end;
         WriteLineEnd;
      end else if sym is TEnumerationSymbol then begin
         WriteString('/// ');
         WriteString(sym.QualifiedName);
         WriteString(' enumeration');
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
            if parent is TFuncPtrExpr then begin
               if i=0 then
                  Exit
               else Dec(i);
            end else if parent is TMethodExpr then begin
               if i=0 then
                  Exit
               else Dec(i);
            end else if (i>0) and (parent is TConstructorStaticExpr) then begin
               Dec(i);
            end;
            if (funcSym=nil) or (i>=funcSym.Params.Count) then begin
               if (parent.ClassType=TDecVarFuncExpr) or (parent.ClassType=TIncVarFuncExpr) then begin
                  right:=TMagicIteratorFuncExpr(parent).Args[1];
                  if (right is TConstIntExpr) and (TConstIntExpr(right).Value=1) then
                     Exit // special case handled via ++ or --, no need to pass by ref
                  else varSym:=TVarExpr(expr).DataSym; // FindSymbolAtStackAddr(TVarExpr(expr).StackAddr, Context.Level);
               end else begin
                  // else not supported yet
                  Exit;
               end;
            end else begin
               if funcSym.Params[i] is TVarParamSymbol then begin
                  varSym:=TVarExpr(expr).DataSym; // FindSymbolAtStackAddr(TVarExpr(expr).StackAddr, Context.Level);
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
      if curExpr is TBlockExprBase then begin
         CollectInitExprLocalVars(TBlockExprBase(curExpr));
      end else begin
         Assert((curExpr is TAssignExpr) or (curExpr is TInitDataExpr));
         expr:=curExpr.SubExpr[0] as TVarExpr;
         varSym:=expr.DataSym; // FindSymbolAtStackAddr(expr.StackAddr, Context.Level);
         FDeclaredLocalVars.Add(varSym);
      end;
   end;
end;

// CreateSymbolMap
//
function TdwsJSCodeGen.CreateSymbolMap(parentMap : TdwsCodeGenSymbolMap; symbol : TSymbol) : TdwsCodeGenSymbolMap;
begin
   if cgoObfuscate in Options then
      Result:=TdwsCodeGenSymbolMapJSObfuscating.Create(parentMap, symbol)
   else Result:=TdwsCodeGenSymbolMap.Create(parentMap, symbol);
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

// SameDefaultValue
//
function TdwsJSCodeGen.SameDefaultValue(typ1, typ2 : TTypeSymbol) : Boolean;
begin
   Result:=   (typ1=typ2)
           or (    ((typ1 is TClassSymbol) or (typ1 is TFuncSymbol) or (typ1 is TInterfaceSymbol))
               and ((typ2 is TClassSymbol) or (typ2 is TFuncSymbol) or (typ2 is TInterfaceSymbol)) );
end;

// WriteDefaultValue
//
procedure TdwsJSCodeGen.WriteDefaultValue(typ : TTypeSymbol; box : Boolean);
var
   i : Integer;
   comma : Boolean;
   sas : TStaticArraySymbol;
   recSym : TRecordSymbol;
   member : TFieldSymbol;
begin
   typ:=typ.UnAliasedType;

   if box then
      WriteString('{'+cBoxFieldName+':');
   if typ is TBaseIntegerSymbol then
      WriteString('0')
   else if typ is TBaseFloatSymbol then
      WriteString('0.0')
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
   else if typ is TFuncSymbol then
      WriteString('null')
   else if typ is TInterfaceSymbol then
      WriteString('null')
   else if typ is TEnumerationSymbol then
      WriteString(IntToStr(TEnumerationSymbol(typ).DefaultValue))
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
         WriteStringLn('function () {');
         Indent;
         WriteString('for (var r=[],i=0; i<'+IntToStr(sas.ElementCount)+'; i++) r.push(');
         WriteDefaultValue(sas.Typ, False);
         WriteStringLn(');');
         WriteStringLn('return r');
         UnIndent;
         WriteString('}()');
      end;
   end else if typ is TDynamicArraySymbol then begin
      WriteString('[]');
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

// WriteValue
//
procedure TdwsJSCodeGen.WriteValue(typ : TTypeSymbol; const data : TData; addr : Integer);
var
   i : Integer;
   recSym : TRecordSymbol;
   member : TFieldSymbol;
   sas : TStaticArraySymbol;
   intf : IUnknown;
   comma : Boolean;
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
   end else if typ is TBaseVariantSymbol then begin
      case VarType(data[addr]) of
         varEmpty :
            WriteString('undefined');
         varNull :
            WriteString('null');
         varInteger, varSmallint, varShortInt, varInt64, varByte, varWord, varUInt64 :
            WriteString(IntToStr(data[addr]));
         varSingle, varDouble, varCurrency :
            WriteString(FloatToStr(data[addr], cFormatSettings));
         varString, varUString, varOleStr :
            WriteJavaScriptString(VarToStr(data[addr]));
         varBoolean :
            WriteString(cBoolToJSBool[Boolean(data[addr])])
      else
         raise ECodeGenUnsupportedSymbol.CreateFmt('Value of type %s (VarType = %d)',
                                                   [typ.ClassName, VarType(data[addr])]);
      end;
   end else if typ is TNilSymbol then begin
      WriteString('null')
   end else if typ is TClassOfSymbol then begin
      WriteSymbolName(TClassOfSymbol(typ).TypClassSymbol);
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
      comma:=False;
      for i:=0 to recSym.Members.Count-1 do begin
         if not (recSym.Members[i] is TFieldSymbol) then continue;
         if comma then
            WriteString(',')
         else comma:=True;
         member:=TFieldSymbol(recSym.Members[i]);
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
   end else if typ is TDynamicArraySymbol then begin
      intf:=data[addr];
      if (IScriptObj(intf).InternalObject as TScriptDynamicArray).Length=0 then
         WriteString('[]')
      else raise ECodeGenUnsupportedSymbol.Create('Non empty dynamic array symbol');
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
   if (func is TMethodSymbol) and not (TMethodSymbol(func).StructSymbol is TRecordSymbol) then begin
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

   function ResultIsNotUsedInExpr(anExpr : TExprBase) : Boolean;
   var
      foundIt : Boolean;
   begin
      foundIt:=False;
      anExpr.RecursiveEnumerateSubExprs(
         procedure (parent, expr : TExprBase; var abort : Boolean)
         begin
            abort:=    (expr is TVarExpr)
                   and (TVarExpr(expr).DataSym is TResultSymbol);
            if abort then
               foundIt:=True;
         end);
      Result:=not foundIt;
   end;

var
   resultTyp : TTypeSymbol;
   proc : TdwsProcedure;
   resultIsBoxed : Boolean;
   param : TParamSymbol;
   i : Integer;
   cg : TdwsExprCodeGen;
   assignExpr : TAssignExpr;
begin
   proc:=(func.Executable as TdwsProcedure);
   if proc=nil then Exit;

   // box params that the function will pass as var
   for i:=0 to proc.Func.Params.Count-1 do begin
      param:=proc.Func.Params[i] as TParamSymbol;
      if (not (param is TByRefParamSymbol)) and TJSExprCodeGen.IsLocalVarParam(Self, param) then begin
         WriteSymbolName(param);
         WriteString('={'+cBoxFieldName+':');
         WriteSymbolName(param);
         WriteStringLn('};');
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
               Exit;

            end

         end;

      end;

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
         WriteStringLn('} finally {return Result.'+cBoxFieldName+'}')
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

   if not SmartLinkMethod(meth) then Exit;

   if not (meth.IsVirtual or meth.IsInterfaced) then
      if meth.Kind in [fkProcedure, fkFunction, fkMethod] then
         if not SmartLink(meth) then Exit;

   WriteSymbolVerbosity(meth);

   WriteString(',');
   WriteString(MemberName(meth, meth.StructSymbol));

   EnterScope(meth);
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
      LeaveScope;
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

   EnterScope(meth);
   EnterContext(proc);
   try

      WriteString('(');
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
      LeaveScope;
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
   IgnoreJSRTLDependencies(Dependencies);
end;

// ------------------
// ------------------ TJSBlockInitExpr ------------------
// ------------------

// CodeGen
//
procedure TJSBlockInitExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   blockInit : TBlockExprBase;
   initExpr : TExprBase;
   sym : TDataSymbol;
   oldTable : TSymbolTable;
begin
   blockInit:=TBlockExprBase(expr);
   for i:=0 to blockInit.SubExprCount-1 do begin
      initExpr:=blockInit.SubExpr[i];
      if initExpr is TBlockExprBase then begin
         oldTable:=codeGen.LocalTable;
         if initExpr is TBlockExpr then
            codeGen.LocalTable:=TBlockExpr(initExpr).Table;
         try
            Self.CodeGen(codeGen, initExpr);
         finally
            codeGen.LocalTable:=oldTable;
         end;
      end else begin
         codeGen.WriteString('var ');
         Assert(initExpr.SubExprCount>=1);
         sym:=TJSVarExpr.CodeGenSymbol(codeGen, initExpr.SubExpr[0] as TVarExpr);
         if IsLocalVarParam(codeGen, sym) then begin
            codeGen.WriteSymbolName(sym);
            codeGen.WriteString('={};');
         end;
         codeGen.Compile(blockInit.SubExpr[i]);
      end;
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
   i : Integer;
   jsCode : String;
begin
   e:=TdwsJSBlockExpr(expr);
   jsCode:=e.Code;

   for i:=e.SymbolsCount-1 downto 0 do
      Insert(codeGen.SymbolMappedName(e.Symbols[i], cgssGlobal), jsCode, e.SymbolOffsets[i]);

   codeGen.WriteString(jsCode);
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
   codeGen.WriteStringLn(';');
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
      codeGen.WriteString('.'+TdwsJSCodeGen.cBoxFieldName);
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
   Result:=varExpr.DataSym; // codeGen.FindSymbolAtStackAddr(varExpr.StackAddr, codeGen.Context.Level);
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
      codeGen.WriteString('.'+TdwsJSCodeGen.cBoxFieldName);
end;

// ------------------
// ------------------ TJSVarParamExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarParamExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   inherited;
   codeGen.WriteString('.'+TdwsJSCodeGen.cBoxFieldName);
end;

// ------------------
// ------------------ TJSVarParamParentExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarParamParentExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   inherited;
   codeGen.WriteString('.'+TdwsJSCodeGen.cBoxFieldName);
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
      codeGen.WriteString('.'+TdwsJSCodeGen.cBoxFieldName);
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

      if sas.ElementCount<cInlineStaticArrayLimit then begin

         codeGen.WriteString('=');
         (codeGen as TdwsJSCodeGen).WriteDefaultValue(t, False);

      end else begin

         codeGen.WriteString('=new Array('+IntToStr(sas.ElementCount)+');');

         codeGen.WriteString(' for (var $i=0; $i<');
         codeGen.WriteString(IntToStr(sas.ElementCount));
         codeGen.WriteString('; $i++) ');
         codeGen.Compile(e.Expr);
         codeGen.WriteString('[$i]=');

         (codeGen as TdwsJSCodeGen).WriteDefaultValue(sas.Typ.UnAliasedType, False);

      end;

      codeGen.WriteStringLn(';');

   end else if t is TDynamicArraySymbol then begin

      codeGen.WriteStringLn('=[];');

   end else raise ECodeGenUnsupportedSymbol.CreateFmt('InitData for %s', [t.ClassName]);
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
   CodeGenRight.WriteString(IntToStr(e.Right));
   CodeGenRight.WriteStringLn(';');
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
   WriteJavaScriptString(CodeGenRight.Output, e.Right);
   CodeGenRight.WriteStringLn(';');
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
   CodeGenRight.WriteString(FloatToStr(e.Right, cFormatSettings));
   CodeGenRight.WriteStringLn(';');
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
   CodeGenRight.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignNilToVarExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignNilToVarExpr.CodeGenRight(CodeGenRight : TdwsCodeGen; expr : TExprBase);
begin
   CodeGenRight.WriteStringLn('null;');
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
//   if e.Left.Typ is TRecordSymbol then begin
      CodeGenRight.CompileNoWrap(e.Right);
//   end else raise ECodeGenUnsupportedSymbol.CreateFmt('Unsupported %s on type %s', [e.ClassName, e.Left.Typ.ClassName]);
   CodeGenRight.WriteStringLn(';');
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
// ------------------ TJSConstNumExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstNumExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstExpr;
begin
   e:=TConstExpr(expr);
   if e.Eval(nil)<0 then begin
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
   codeGen.WriteString(IntToStr(e.Value));
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
   for i:=0 to e.ElementCount-1 do begin
      if i>0 then
         codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Elements[i]);
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
   e:=TAssignExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.WriteString('=');
   CodeGenRight(codeGen, expr);
end;

// CodeGenRight
//
procedure TJSAssignExpr.CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignExpr;
begin
   e:=TAssignExpr(expr);
   codeGen.CompileNoWrap(e.Right);
   codeGen.WriteStringLn(';');
end;

// ------------------
// ------------------ TJSAssignDataExpr ------------------
// ------------------

// CodeGenRight
//
procedure TJSAssignDataExpr.CodeGenRight(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignDataExpr;
begin
   // TODO: deep copy of records & static arrays
   e:=TAssignDataExpr(expr);

   codeGen.CompileValue(e.Right);
   codeGen.WriteStringLn(';');
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
   codeGen.WriteStringLn(';');
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
               codeGen.WriteString('{'+TdwsJSCodeGen.cBoxFieldName+':');
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
            codeGen.CompileValue(paramExpr);
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
   codeGen.WriteString(IntToStr(e.MethSym.VMTIndex));
   codeGen.WriteString(']');
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
   jsCall:=(e.ConnectorCall as TdwsJSConnectorCall);

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
      codeGen.Compile(e.SubExpr[i]);
   end;
   if e.IsIndex then begin
      codeGen.WriteString(']');
      if isWrite then begin
         codeGen.WriteString('=');
         codeGen.Compile(e.SubExpr[n+1]);
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
begin
   e:=TConnectorWriteExpr(Expr);
   jsMember:=(e.ConnectorMember.GetSelf as TdwsJSConnectorMember);

   codeGen.Compile(e.BaseExpr);
   codeGen.WriteString('.');
   codeGen.WriteString(jsMember.MemberName);
   codeGen.WriteString('=');
   codeGen.Compile(e.ValueExpr);
   codegen.WriteStringLn(';');
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
         codeGen.Compile(methExpr.BaseExpr);
         codeGen.WriteString('.ClassType.');
         codeGen.WriteString((codeGen as TdwsJSCodeGen).MemberName(methSym, methSym.StructSymbol));
         codeGen.WriteString('$v');
      end else if methExpr is TMethodInterfaceExpr then begin
         codeGen.WriteString('.O,');
         codeGen.Compile(methExpr.BaseExpr);
         codeGen.WriteString('[');
         codeGen.WriteString(IntToStr(methSym.VMTIndex));
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

      codeGen.WriteSymbolName(funcExpr.FuncSym);

      if funcExpr is TMagicFuncExpr then
         codeGen.Dependencies.Add(funcExpr.FuncSym.QualifiedName);

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

   wrapped:=not ((e.Left is TVarExpr) or (e.Left is TConstExpr) or (e.Left is TFieldExpr));

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
   codeGen.WriteString(IntToStr(e.Mask));
   codeGen.WriteString(')!=0');
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
begin
   e:=TCaseExpr(expr);

   valType:=e.ValueExpr.Typ.UnAliasedType;
   switchable:=   (valType is TBaseBooleanSymbol)
               or (valType is TBaseIntegerSymbol)
               or (valType is TBaseStringSymbol)
               or (valType is TEnumerationSymbol);

   for i:=0 to e.CaseConditions.Count-1 do begin
      if not switchable then break;
      cond:=TCaseCondition(e.CaseConditions.List[i]);
      switchable:=    (cond is TCompareCaseCondition)
                  and (TCompareCaseCondition(cond).CompareExpr is TConstExpr);
   end;

   if switchable then begin

      SetLength(mark, e.CaseConditions.Count);
      codeGen.WriteString('switch (');
      codeGen.Compile(e.ValueExpr);
      codeGen.WriteStringLn(') {');
      codeGen.Indent;
      for i:=0 to e.CaseConditions.Count-1 do begin
         if mark[i] then continue;
         compCond:=TCompareCaseCondition(e.CaseConditions.List[i]);
         for j:=i to e.CaseConditions.Count-1 do begin
            compCondOther:=TCompareCaseCondition(e.CaseConditions.List[j]);
            if compCond.TrueExpr=compCondOther.TrueExpr then begin
               if j>i then
                  codeGen.WriteLineEnd;
               codeGen.WriteString('case ');
               codeGen.Compile(compCondOther.CompareExpr);
               codeGen.WriteStringLn(' :');
               mark[j]:=True;
            end;
         end;
         codeGen.Indent;
         codeGen.Compile(compCond.TrueExpr);
         codeGen.WriteStringLn('break;');
         codeGen.UnIndent;
      end;
      if e.ElseExpr<>nil then begin
         codeGen.WriteStringLn('default :');
         codeGen.Indent;
         codeGen.Compile(e.ElseExpr);
         codeGen.UnIndent;
      end;
      codeGen.UnIndent;
      codeGen.WriteStringLn('};');

   end else begin

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
   if (right is TConstIntExpr) and (TConstIntExpr(right).Value=1) then begin
      codeGen.WriteString(op);
      codeGen.WriteString(op);
      codeGen.Compile(left);
   end else begin
      if not noWrap then
         codeGen.WriteString('(');
      codeGen.Compile(left);
      codeGen.WriteString(op);
      codeGen.WriteString('=');
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
// ------------------ TJSConvFloatExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConvFloatExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConvIntegerExpr;
begin
   e:=TConvIntegerExpr(expr);
   if e.Expr.Typ.UnAliasedType is TBaseIntegerSymbol then
      codeGen.Compile(e.Expr)
   else begin
      codeGen.WriteString('Number(');
      codeGen.Compile(e.Expr);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSOrdExpr ------------------
// ------------------

// CodeGen
//
procedure TJSOrdExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TOrdExpr;
   typ : TTypeSymbol;
begin
   e:=TOrdExpr(expr);
   typ:=e.Expr.Typ.UnAliasedType;
   if typ is TBaseIntegerSymbol then
      codeGen.Compile(e.Expr)
   else begin
      codeGen.Dependencies.Add('$Ord');
      codeGen.WriteString('$Ord(');
      codeGen.Compile(e.Expr);
      codeGen.WriteString(',');
      WriteLocationString(codeGen, expr);
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
   codeGen.WriteSymbolName(e.Typ.UnAliasedType);
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
   if cgoNoSourceLocations in codeGen.Options then
      codeGen.WriteString('""')
   else WriteJavaScriptString(codeGen.Output, codeGen.LocationString(expr));
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

         codeGen.LocalTable.AddSymbolDirect(de.ExceptionVar);
         try
            codeGen.WriteString('var ');
            codeGen.WriteSymbolName(de.ExceptionVar);
            codeGen.WriteStringLn('=$e;');
            codeGen.Compile(de.DoBlockExpr);
         finally
            codeGen.SymbolMap.ForgetSymbol(de.ExceptionVar);
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

   if e.LengthExprCount>1 then begin

      codeGen.Dependencies.Add('$NewArrayFn');

      for i:=0 to e.LengthExprCount-2 do begin
         codeGen.WriteString('$NewArrayFn(');
         codeGen.Compile(e.LengthExpr[i]);
         codeGen.WriteString(',function (){return ');
      end;

   end;

   if e.Typ.Typ.IsBaseType then begin

      codeGen.Dependencies.Add('$NewArray');

      codeGen.WriteString('$NewArray(');
      codeGen.Compile(e.LengthExpr[e.LengthExprCount-1]);
      codeGen.WriteString(',');
      (codeGen as TdwsJSCodeGen).WriteDefaultValue(e.Typ.Typ, False);
      codeGen.WriteString(')');

   end else begin

      codeGen.Dependencies.Add('$NewArrayFn');

      codeGen.WriteString('$NewArrayFn(');
      codeGen.Compile(e.LengthExpr[e.LengthExprCount-1]);
      codeGen.WriteString(',function (){return ');
      (codeGen as TdwsJSCodeGen).WriteDefaultValue(e.Typ.Typ, False);
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

   if (e.LengthExpr is TConstIntExpr) and (TConstIntExpr(e.LengthExpr).Value=0) then begin

      codeGen.Compile(e.BaseExpr);
      codeGen.WriteStringLn('.length=0;');

   end else begin

      codeGen.Dependencies.Add('$ArraySetLength');

      codeGen.WriteString('$ArraySetLength(');
      codeGen.Compile(e.BaseExpr);
      codeGen.WriteString(',');
      codeGen.Compile(e.LengthExpr);
      codeGen.WriteString(',function (){return ');
      (codeGen as TdwsJSCodeGen).WriteDefaultValue(e.BaseExpr.Typ.Typ, False);
      codeGen.WriteStringLn('});');

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
   codeGen.WriteString('.push(');
   codeGen.CompileValue(e.ItemExpr);
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
                 or (    (e.IndexExpr is TConstIntExpr)
                     and (TConstIntExpr(e.IndexExpr).Value=0));

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
         codeGen.WriteString(')-');
         codeGen.WriteString(IntToStr(typ.LowBound));
      end;

   end else begin

      codeGen.Dependencies.Add('$Idx');

      codeGen.WriteString('$Idx(');
      codeGen.CompileNoWrap(e.IndexExpr);
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
      codeGen.CompileNoWrap(e.Right);
      codeGen.WriteString(')-1)');

   end else begin

      codeGen.Dependencies.Add('$SIdx');

      codeGen.WriteString('$SIdx(');
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

   codeGen.Dependencies.Add('$StrSet');

   codeGen.Compile(e.StringExpr);
   codeGen.WriteString('=$StrSet(');
   codeGen.CompileNoWrap(e.StringExpr);
   codeGen.WriteString(',');
   codeGen.CompileNoWrap(e.IndexExpr);
   codeGen.WriteString(',');
   codeGen.CompileNoWrap(e.ValueExpr);
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
   codeGen.CompileNoWrap(e.FromExpr);
   if tmpTo<>'' then begin
      codeGen.WriteString(',');
      codeGen.WriteString(tmpTo);
      codeGen.WriteString('=');
      codeGen.CompileNoWrap(e.ToExpr);
   end;
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
   WriteOp(codeGen, e.Right);
   if FAssociative and (e.Right.ClassType=e.ClassType) then
      codeGen.CompileNoWrap(e.Right)
   else WriteWrappedIfNeeded(codeGen, e.Right);
end;

// WriteOp
//
procedure TJSBinOpExpr.WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr);
begin
   codeGen.WriteString(FOp);
end;

// ------------------
// ------------------ TJSAddOpExpr ------------------
// ------------------

// Create
//
constructor TJSAddOpExpr.Create;
begin
   inherited Create('+', True);
end;

// WriteOp
//
procedure TJSAddOpExpr.WriteOp(codeGen : TdwsCodeGen; rightExpr : TTypedExpr);
begin
   if (rightExpr is TConstExpr) and (rightExpr.Eval(nil)<0) then begin
      // right operand will write a minus
   end else codeGen.WriteString(FOp);
end;

// ------------------
// ------------------ TJSSubOpExpr ------------------
// ------------------

// Create
//
constructor TJSSubOpExpr.Create;
begin
   inherited Create('-', True);
end;

// CodeGenNoWrap
//
procedure TJSSubOpExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TBinaryOpExpr;
begin
   e:=TBinaryOpExpr(expr);
   if (e.Left.ClassType=e.ClassType) then
      codeGen.CompileNoWrap(e.Left)
   else WriteWrappedIfNeeded(codeGen, e.Left);
   WriteOp(codeGen, e.Right);
   if (e.Right is TConstExpr) and (e.Right.Eval(nil)<0) then begin
      codeGen.Compile(e.Right)
   end else WriteWrappedIfNeeded(codeGen, e.Right);
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
   codeGen.CompileNoWrap(e.Expr);
   codeGen.WriteString(')!=-1)');
end;

// ------------------
// ------------------ TdwsCodeGenSymbolMapJSObfuscating ------------------
// ------------------

// DoNeedUniqueName
//
function TdwsCodeGenSymbolMapJSObfuscating.DoNeedUniqueName(symbol : TSymbol; tryCount : Integer; canObfuscate : Boolean) : String;

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

end.
