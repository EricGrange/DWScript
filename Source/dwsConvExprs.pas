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
unit dwsConvExprs;

{$I dws.inc}

interface

uses
   Variants, SysUtils,
   dwsUtils, dwsDataContext, dwsStack, dwsXPlatform, dwsErrors, dwsStrings,
   dwsExprs, dwsExprList, dwsConstExprs, dwsSymbols, dwsUnitSymbols;

type

   // newType(x)
   TConvExpr = class(TUnaryOpExpr)
      public
         class function WrapWithConvCast(prog : TdwsProgram; const scriptPos : TScriptPos;
                                         toTyp : TTypeSymbol; expr : TTypedExpr;
                                         reportError : Boolean) : TTypedExpr; static;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // Just wraps with a typ for an invalid conversion expr
   // this is used only to keep compiling, the resulting program is not executable
   TConvInvalidExpr = class sealed (TConvExpr)
      protected
         function GetIsConstant : Boolean; override;

      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr; toTyp : TTypeSymbol); reintroduce;
   end;

   // Float(int x)
   TConvIntToFloatExpr = class (TUnaryOpFloatExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;
   // Float(variant x)
   TConvVarToFloatExpr = class (TUnaryOpFloatExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // Integer(variant x)
   TConvVarToIntegerExpr = class (TUnaryOpIntExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   // Integer(ordinal x)
   TConvOrdToIntegerExpr = class (TUnaryOpIntExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // String(variant x)
   TConvVarToStringExpr = class (TUnaryOpStringExpr)
     procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString); override;
   end;

   // Boolean(int x)
   TConvIntToBoolExpr = class (TUnaryOpBoolExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   // Boolean(float x)
   TConvFloatToBoolExpr = class (TUnaryOpBoolExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   // Boolean(variant x)
   TConvVarToBoolExpr = class (TUnaryOpBoolExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // Variant(simple)
   TConvVariantExpr = class (TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // Static Array to Dynamic Array
   TConvStaticArrayToDynamicExpr = class (TUnaryOpExpr)
      constructor Create(prog : TdwsProgram; expr : TArrayConstantExpr; toTyp : TDynamicArraySymbol); reintroduce;
      function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // Static Array to set of
   TConvStaticArrayToSetOfExpr = class (TPosDataExpr)
      private
         FExpr : TArrayConstantExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos;
                            expr : TArrayConstantExpr; toTyp : TSetOfSymbol);
         destructor Destroy; override;

         procedure EvalAsTData(exec : TdwsExecution; var data : TData);

         function IsWritable: Boolean; override;
         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property Expr : TArrayConstantExpr read FExpr write FExpr;
   end;

   // ExternalClass(x)
   TConvExternalExpr = class (TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // cast something as Typ
   TAsCastExpr = class(TUnaryOpExpr)
      private
         FPos : TScriptPos;

      public
         constructor Create(prog : TdwsProgram; const aPos : TScriptPos;
                            expr : TTypedExpr; toTyp : TTypeSymbol); reintroduce;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // class as TMyClass
   TClassAsClassExpr = class(TAsCastExpr)
      protected
         procedure RaiseMetaClassCastFailed(exec : TdwsExecution; classSym : TClassSymbol);

      public
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // obj as TMyClass
   TObjAsClassExpr = class(TAsCastExpr)
      protected
         procedure RaiseInstanceClassCastFailed(exec : TdwsExecution; classSym : TClassSymbol);

      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   // obj.ClassType
   TObjToClassTypeExpr = class(TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;

         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // obj as Interface
   TObjAsIntfExpr = class(TAsCastExpr)
      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   // interface as Interface
   TIntfAsIntfExpr = class(TAsCastExpr)
      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   // interface as class
   TIntfAsClassExpr = class(TAsCastExpr)
      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsCoreExprs;

// ------------------
// ------------------ TConvExpr ------------------
// ------------------

// WrapWithConvCast
//
class function TConvExpr.WrapWithConvCast(prog : TdwsProgram; const scriptPos : TScriptPos;
                                          toTyp : TTypeSymbol; expr : TTypedExpr;
                                          reportError : Boolean) : TTypedExpr;

   procedure ReportIncompatibleTypes;
   var
      cleft, cright: UnicodeString;
   begin
      if not reportError then Exit;
      if toTyp = nil then
         cleft := SYS_VOID
      else cleft := toTyp.Caption;
      if expr.Typ = nil then
         cright := SYS_VOID
      else cright := expr.Typ.Caption;
      prog.CompileMsgs.AddCompilerErrorFmt(scriptPos, CPE_AssignIncompatibleTypes, [cright, cleft]);
   end;

var
   arrayConst : TArrayConstantExpr;
begin
   Result:=expr;
   if (toTyp=nil) or (expr.Typ=nil) then begin
      ReportIncompatibleTypes;
      Exit;
   end;

   if expr.Typ=toTyp then Exit;

   if expr.ClassType=TArrayConstantExpr then begin
      arrayConst:=TArrayConstantExpr(expr);
      if toTyp is TDynamicArraySymbol then begin
         if    (toTyp.Typ.IsOfType(expr.Typ.Typ))
            or ((arrayConst.ElementCount=0) and (arrayConst.Typ.Typ.IsOfType(prog.TypVariant)))  then
            Result:=TConvStaticArrayToDynamicExpr.Create(prog, arrayConst,
                                                         TDynamicArraySymbol(toTyp))
      end else if toTyp is TSetOfSymbol then begin
         if (arrayConst.ElementCount=0) or arrayConst.Typ.Typ.IsOfType(toTyp.Typ) then
            Result:=TConvStaticArrayToSetOfExpr.Create(prog, scriptPos, arrayConst, TSetOfSymbol(toTyp));
      end;
   end else if expr.Typ.UnAliasedTypeIs(TBaseVariantSymbol) then begin
      if toTyp.IsOfType(prog.TypInteger) then
         Result:=TConvVarToIntegerExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypFloat) then
         Result:=TConvVarToFloatExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypString) then
         Result:=TConvVarToStringExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypBoolean) then
         Result:=TConvVarToBoolExpr.Create(prog, expr);
   end else if     (toTyp is TStructuredTypeMetaSymbol)
               and (expr.Typ.IsOfType(toTyp.Typ)) then begin
      if toTyp.ClassType=TClassOfSymbol then begin
         Result:=TObjToClassTypeExpr.Create(prog, expr);
         if toTyp.Typ<>expr.Typ then
            Result:=TClassAsClassExpr.Create(prog, scriptPos, Result, toTyp);
      end else begin
         Assert(False);
         Result:=nil;
      end;
   end else begin
      if     toTyp.IsOfType(prog.TypFloat)
         and expr.IsOfType(prog.TypInteger) then begin
         if expr is TConstIntExpr then begin
            Result:=TConstFloatExpr.CreateTypedVariantValue(prog, prog.TypFloat, TConstIntExpr(expr).Value);
            expr.Free;
         end else Result:=TConvIntToFloatExpr.Create(prog, expr);
      end;
   end;
   // Look if Types are compatible
   if not toTyp.IsCompatible(Result.Typ) then
      ReportIncompatibleTypes;
end;

// Eval
//
function TConvExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Assert(False);
end;

// ------------------
// ------------------ TConvInvalidExpr ------------------
// ------------------

// Create
//
constructor TConvInvalidExpr.Create(prog : TdwsProgram; expr : TTypedExpr; toTyp : TTypeSymbol);
begin
   inherited Create(prog, expr);
   Typ:=toTyp;
end;

// GetIsConstant
//
function TConvInvalidExpr.GetIsConstant : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TConvIntToFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TConvIntToFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TConvVarToFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TConvVarToFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
var
   v : Variant;
begin
   FExpr.EvalAsVariant(exec, v);
   Result:=v;
end;

// ------------------
// ------------------ TConvVarToIntegerExpr ------------------
// ------------------

// EvalAsInteger
//
function TConvVarToIntegerExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   v : Variant;
begin
   FExpr.EvalAsVariant(exec, v);
   Result:=v;
end;

// ------------------
// ------------------ TConvOrdToIntegerExpr ------------------
// ------------------

// EvalAsInteger
//
function TConvOrdToIntegerExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FExpr.EvalAsInteger(exec);
end;

// Optimize
//
function TConvOrdToIntegerExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   // this can happen when an integer was qualifed as a type
   if Expr.ClassType=TConstIntExpr then begin
      if Expr.Typ=Typ then begin
         Result:=Expr;
         Expr:=nil;
      end else begin
         Result:=TConstIntExpr.CreateUnified(prog, Typ, TConstIntExpr(Expr).Value);
      end;
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TConvVarToStringExpr ------------------
// ------------------

// EvalAsString
//
procedure TConvVarToStringExpr.EvalAsString(exec : TdwsExecution; var Result : UnicodeString);
var
   v : Variant;
begin
   FExpr.EvalAsVariant(exec, v);
   VariantToString(v, Result);
end;

// ------------------
// ------------------ TConvIntToBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConvIntToBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FExpr.EvalAsInteger(exec)<>0);
end;

// ------------------
// ------------------ TConvFloatToBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConvFloatToBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FExpr.EvalAsFloat(exec)<>0);
end;

// ------------------
// ------------------ TConvVarToBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConvVarToBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   v : Variant;
begin
   FExpr.EvalAsVariant(exec, v);
   Result:=v;
end;

// ------------------
// ------------------ TConvVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TConvVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   FExpr.EvalAsVariant(exec, Result);
end;

// ------------------
// ------------------ TConvStaticArrayToDynamicExpr ------------------
// ------------------

// Create
//
constructor TConvStaticArrayToDynamicExpr.Create(prog : TdwsProgram; expr : TArrayConstantExpr;
                                                 toTyp : TDynamicArraySymbol);
begin
   inherited Create(prog, expr);
   Typ:=toTyp;
end;

// Eval
//
function TConvStaticArrayToDynamicExpr.Eval(exec : TdwsExecution) : Variant;
var
   arr : TArrayConstantExpr;
   dynArray : TScriptDynamicArray;
begin
   arr:=TArrayConstantExpr(Expr);

   dynArray:=TScriptDynamicArray.CreateNew(TDynamicArraySymbol(Typ).Typ);
   dynArray.ReplaceData(arr.EvalAsTData(exec));

   Result:=IUnknown(IScriptObj(dynArray));
end;

// ------------------
// ------------------ TConvExternalExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TConvExternalExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Expr.EvalAsVariant(exec, Result);
end;

// ------------------
// ------------------ TAsCastExpr ------------------
// ------------------

// Create
//
constructor TAsCastExpr.Create(prog : TdwsProgram; const aPos : TScriptPos;
                               expr : TTypedExpr; toTyp : TTypeSymbol);
begin
   inherited Create(prog, expr);
   FPos:=aPos;
   FTyp:=toTyp;
end;

// Eval
//
function TAsCastExpr.Eval(exec : TdwsExecution) : Variant;
var
   scriptObj : IScriptObj;
begin
   EvalAsScriptObj(exec, scriptObj);
   Result:=scriptObj;
end;

// ------------------
// ------------------ TClassAsClassExpr ------------------
// ------------------

// RaiseMetaClassCastFailed
//
procedure TClassAsClassExpr.RaiseMetaClassCastFailed(exec : TdwsExecution; classSym : TClassSymbol);
begin
   RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_MetaClassCastFailed,
                                                  [classSym.Caption, FTyp.Name]))
end;

// Eval
//
function TClassAsClassExpr.Eval(exec : TdwsExecution) : Variant;
var
   ref : TClassSymbol;
begin
   ref:=TClassSymbol(Expr.EvalAsInteger(exec));
   Result:=Int64(ref);

   if ref<>nil then begin
      if not FTyp.IsCompatible(ref.MetaSymbol) then
         RaiseMetaClassCastFailed(exec, ref);
   end;
end;

// ------------------
// ------------------ TObjAsClassExpr ------------------
// ------------------

// RaiseInstanceClassCastFailed
//
procedure TObjAsClassExpr.RaiseInstanceClassCastFailed(exec : TdwsExecution; classSym : TClassSymbol);
begin
   RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_ClassInstanceCastFailed,
                                                  [classSym.Caption, FTyp.Caption]))
end;

// EvalAsScriptObj
//
procedure TObjAsClassExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
begin
   Expr.EvalAsScriptObj(exec, Result);

   if Assigned(Result) and not (FTyp.IsCompatible(Result.ClassSym)) then
      RaiseInstanceClassCastFailed(exec, Result.ClassSym);
end;

// ------------------
// ------------------ TObjToClassTypeExpr ------------------
// ------------------

// Create
//
constructor TObjToClassTypeExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
begin
   inherited Create(prog, expr);
   Typ:=(expr.Typ as TStructuredTypeSymbol).MetaSymbol;
end;

// Eval
//
function TObjToClassTypeExpr.Eval(exec : TdwsExecution) : Variant;
var
   obj : IScriptObj;
begin
   Expr.EvalAsScriptObj(exec, obj);
   if obj=nil then
      Result:=Int64(0)
   else Result:=Int64(obj.ClassSym);
end;

// ------------------
// ------------------ TObjAsIntfExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TObjAsIntfExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);

   procedure RaiseIntfCastFailed;
   begin
      RaiseScriptError(exec, EClassCast.CreatePosFmt(ScriptPos, RTE_ObjCastToIntfFailed,
                                                     [Result.ClassSym.Caption, FTyp.Caption]))
   end;

var
   intf : TScriptInterface;
   resolved : TResolvedInterface;
begin
   Expr.EvalAsScriptObj(exec, Result);

   if Assigned(Result) then begin
      if not Result.ClassSym.ResolveInterface(TInterfaceSymbol(Typ), resolved) then
         RaiseIntfCastFailed;
      intf:=TScriptInterface.Create(Result, resolved);
      Result:=intf;
   end;
end;

// ------------------
// ------------------ TIntfAsIntfExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TIntfAsIntfExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);

   procedure RaiseIntfCastFailed;
   begin
      RaiseScriptError(exec, EClassCast.CreatePosFmt(ScriptPos, RTE_IntfCastToIntfFailed,
                                                     [Result.ClassSym.Caption, FTyp.Caption]))
   end;

var
   intf : TScriptInterface;
   instance : IScriptObj;
   resolved : TResolvedInterface;
begin
   Expr.EvalAsScriptObj(exec, Result);

   if Assigned(Result) then begin
      instance:=TScriptInterface(Result.GetSelf).Instance;
      if not instance.ClassSym.ResolveInterface(TInterfaceSymbol(Typ), resolved) then
         RaiseIntfCastFailed;
      intf:=TScriptInterface.Create(instance, resolved);
      Result:=intf;
   end;
end;

// ------------------
// ------------------ TIntfAsClassExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TIntfAsClassExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);

   procedure RaiseIntfCastFailed;
   begin
      RaiseScriptError(exec, EClassCast.CreatePosFmt(ScriptPos, RTE_IntfCastToObjFailed,
                                                     [Result.ClassSym.Caption, FTyp.Caption]))
   end;

var
   intf : TScriptInterface;
begin
   Expr.EvalAsScriptObj(exec, Result);

   if Assigned(Result) then begin
      intf:=TScriptInterface(Result.GetSelf);
      Result:=intf.Instance;
      if not Result.ClassSym.IsCompatible(FTyp) then
         RaiseIntfCastFailed;
   end;
end;

// ------------------
// ------------------ TConvStaticArrayToSetOfExpr ------------------
// ------------------

// Create
//
constructor TConvStaticArrayToSetOfExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos;
                                               expr : TArrayConstantExpr; toTyp : TSetOfSymbol);
begin
   inherited Create(prog, scriptPos, toTyp);
   FExpr:=expr;
end;

// Destroy
//
destructor TConvStaticArrayToSetOfExpr.Destroy;
begin
   inherited;
   FExpr.Free;
end;

// EvalAsTData
//
procedure TConvStaticArrayToSetOfExpr.EvalAsTData(exec : TdwsExecution; var data : TData);
var
   i, v : Integer;
   setOfSym : TSetOfSymbol;
begin
   setOfSym:=TSetOfSymbol(Typ);

   SetLength(data, setOfSym.Size);
   Typ.InitData(data, 0);

   for i:=0 to Expr.ElementCount-1 do begin
      v:=Expr.Elements[i].EvalAsInteger(exec)-setOfSym.MinValue;
      if Cardinal(v)<Cardinal(setOfSym.CountValue) then
         data[v shr 6]:=data[v shr 6] or (Int64(1) shl (v and 63));
   end;
end;

// IsWritable
//
function TConvStaticArrayToSetOfExpr.IsWritable: Boolean;
begin
   Result:=False;
end;

// GetDataPtr
//
procedure TConvStaticArrayToSetOfExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
var
   data : TData;
begin
   EvalAsTData(exec, data);
   result:=exec.Stack.CreateDataContext(data, 0);
end;

// GetSubExpr
//
function TConvStaticArrayToSetOfExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FExpr;
end;

// GetSubExprCount
//
function TConvStaticArrayToSetOfExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

end.
