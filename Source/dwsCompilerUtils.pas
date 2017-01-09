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
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsCompilerUtils;

{$I dws.inc}

interface

uses
   SysUtils, TypInfo,
   dwsErrors, dwsStrings, dwsXPlatform, dwsUtils, dwsScriptSource,
   dwsSymbols, dwsUnitSymbols, dwsCompilerContext,
   dwsExprs, dwsCoreExprs, dwsConstExprs, dwsMethodExprs, dwsMagicExprs,
   dwsConvExprs;

type

   CompilerUtils = class
      public
         class procedure IncompatibleTypes(
                        context : TdwsCompilerContext; const scriptPos : TScriptPos;
                        const fmt : UnicodeString; typ1, typ2 : TTypeSymbol); static;

         class function WrapWithImplicitConversion(
                        context : TdwsCompilerContext; expr : TTypedExpr; toTyp : TTypeSymbol;
                        const hotPos : TScriptPos;
                        const msg : UnicodeString = CPE_IncompatibleTypes) : TTypedExpr; static;

         class procedure AddProcHelper(const name : UnicodeString;
                                       table : TSymbolTable; func : TFuncSymbol;
                                       unitSymbol : TUnitMainSymbol); static;

         class function DynamicArrayAdd(context : TdwsCompilerContext; baseExpr : TTypedExpr;
                                        const namePos : TScriptPos;
                                        argList : TTypedExprList; const argPosArray : TScriptPosArray) : TArrayAddExpr; overload; static;
         class function DynamicArrayAdd(context : TdwsCompilerContext; baseExpr : TTypedExpr;
                                        const scriptPos : TScriptPos; argExpr : TTypedExpr) : TArrayAddExpr; overload; static;

         class function ArrayConcat(context : TdwsCompilerContext; const hotPos : TScriptPos;
                                    left, right : TTypedExpr) : TArrayConcatExpr; static;
   end;

   IRecursiveHasSubExprClass = interface
      function Check(expr : TExprBase) : Boolean;
   end;

   TRecursiveHasSubExprClass = class (TInterfacedObject, IRecursiveHasSubExprClass)
      private
         FClass : TExprBaseClass;

      protected
         procedure Callback(parent, expr : TExprBase; var abort : Boolean);

      public
         constructor Create(aClass : TExprBaseClass);

         function Check(expr : TExprBase) : Boolean;
   end;

   TStringToEnum = class (TFastCompareTextList)
      public
         constructor Create(typ : PTypeInfo; low, high, prefixLength : Integer);
   end;

   TArrayMethodKind = (
      amkNone,
      amkAdd, amkPush, amkIndexOf, amkRemove, amkSort, amkMap, amkHigh, amkLow,
      amkLength, amkCount, amkPop, amkPeek, amkDelete, amkInsert, amkSetLength,
      amkClear, amkSwap, amkCopy, amkReverse, amkDimCount
   );


function NameToArrayMethod(const name : UnicodeString; msgs : TdwsCompileMessageList;
                           const namePos : TScriptPos) : TArrayMethodKind;

function CreateFuncExpr(context : TdwsCompilerContext; funcSym: TFuncSymbol;
                        const scriptObj : IScriptObj; structSym : TCompositeTypeSymbol;
                        options : TCreateFunctionOptions) : TFuncExprBase;
function CreateIntfExpr(context : TdwsCompilerContext; funcSym: TFuncSymbol;
                        const scriptObjIntf : IScriptObjInterface) : TFuncExprBase;

function CreateMethodExpr(context : TdwsCompilerContext; meth: TMethodSymbol; var expr : TTypedExpr; RefKind: TRefKind;
                          const scriptPos: TScriptPos; options : TCreateFunctionOptions) : TFuncExprBase;

function CreateConstParamSymbol(const name : UnicodeString; typ : TTypeSymbol) : TParamSymbol;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// NameToArrayMethod
//
var
   vArrayMethods : TStringToEnum;
function NameToArrayMethod(const name : UnicodeString; msgs : TdwsCompileMessageList;
                           const namePos : TScriptPos) : TArrayMethodKind;

   procedure CheckExactMatch(i : Integer);
   begin
      if vArrayMethods[i] <> name then begin
         msgs.AddCompilerHintFmt(namePos, CPH_CaseDoesNotMatchDeclaration,
                                 [name, vArrayMethods[i]], hlPedantic);
      end;
   end;

var
   i : Integer;
begin
   if vArrayMethods.Find(name, i) then begin
      Result := TArrayMethodKind(vArrayMethods.Objects[i]);
      if (msgs <> nil) and (msgs.HintsLevel >= hlPedantic) then
         CheckExactMatch(i);
   end else begin
      Result := amkNone;
   end;
end;

type
   TCheckAbstractClassConstruction = class (TErrorMessage)
      FClassSym : TClassSymbol;
      constructor Create(msgs: TdwsMessageList; const text : UnicodeString; const p : TScriptPos;
                         classSym : TClassSymbol); overload;
      function IsValid : Boolean; override;
   end;

// Create
//
constructor TCheckAbstractClassConstruction.Create(msgs: TdwsMessageList; const text : UnicodeString; const p : TScriptPos;
                                                   classSym : TClassSymbol);
begin
   FClassSym:=classSym;
   inherited Create(msgs, UnicodeFormat(MSG_Error, [text]), p);
end;

// IsValid
//
function TCheckAbstractClassConstruction.IsValid : Boolean;
begin
   Result:=FClassSym.IsAbstract and (MessageList.State=mlsCompleted);
end;

// CreateFuncExpr
//
function CreateFuncExpr(context : TdwsCompilerContext; funcSym: TFuncSymbol;
                        const scriptObj : IScriptObj; structSym : TCompositeTypeSymbol;
                        options : TCreateFunctionOptions) : TFuncExprBase;
var
   instanceExpr : TTypedExpr;
begin
   if FuncSym is TMethodSymbol then begin
      if Assigned(scriptObj) then begin
         instanceExpr:=TConstExpr.Create(structSym, scriptObj);
         Result:=CreateMethodExpr(context, TMethodSymbol(funcSym),
                                  instanceExpr, rkObjRef, cNullPos, options)
      end else if structSym<>nil then begin
         instanceExpr:=TConstExpr.Create(structSym.MetaSymbol, Int64(structSym));
         Result:=CreateMethodExpr(context, TMethodSymbol(funcSym),
                                  instanceExpr, rkClassOfRef, cNullPos, options)
      end else begin
         // static method
         structSym:=TMethodSymbol(funcSym).StructSymbol;
         if structSym is TStructuredTypeSymbol then begin
            instanceExpr:=TConstExpr.Create(structSym.MetaSymbol, Int64(structSym));
            Result:=CreateMethodExpr(context, TMethodSymbol(funcSym),
                                     instanceExpr, rkClassOfRef, cNullPos, options)
         end else begin
            Result:=nil;
            Assert(False, 'TODO');
         end;
      end;
   end else if funcSym is TMagicFuncSymbol then begin
      Result:=TMagicFuncExpr.CreateMagicFuncExpr(context, cNullPos, TMagicFuncSymbol(funcSym));
   end else begin
      Result:=TFuncSimpleExpr.Create(context, cNullPos, funcSym);
   end;
end;

// CreateIntfExpr
//
function CreateIntfExpr(context : TdwsCompilerContext; funcSym: TFuncSymbol;
                        const scriptObjIntf : IScriptObjInterface) : TFuncExprBase;
var
   instanceExpr : TTypedExpr;
   scriptIntf : TScriptInterface;
begin
   scriptIntf:=(scriptObjIntf.GetSelf as TScriptInterface);
   instanceExpr:=TConstExpr.Create(scriptIntf.Typ, scriptObjIntf);
   Result:=CreateMethodExpr(context, TMethodSymbol(funcSym),
                            instanceExpr, rkIntfRef, cNullPos, [])
end;

// CreateMethodExpr
//
function CreateMethodExpr(context : TdwsCompilerContext; meth: TMethodSymbol; var expr: TTypedExpr; RefKind: TRefKind;
                          const scriptPos: TScriptPos; options : TCreateFunctionOptions) : TFuncExprBase;
var
   helper : THelperSymbol;
   internalFunc : TInternalMagicFunction;
   classSymbol : TClassSymbol;
begin
   // Create the correct TExpr for a method symbol
   Result := nil;

   if meth is TMagicMethodSymbol then begin

      if meth is TMagicStaticMethodSymbol then begin
         dwsFreeAndNil(expr);
         internalFunc:=TMagicStaticMethodSymbol(meth).InternalFunction;
         Result:=internalFunc.MagicFuncExprClass.Create(context, scriptPos, meth, internalFunc);
      end else Assert(False, 'not supported yet');

   end else if meth.StructSymbol is TInterfaceSymbol then begin

      if meth.Name<>'' then
         Result:=TMethodInterfaceExpr.Create(context, scriptPos, meth, expr)
      else begin
         Result:=TMethodInterfaceAnonymousExpr.Create(context, scriptPos, meth, expr);
      end;

   end else if meth.StructSymbol is TClassSymbol then begin

      if meth.IsStatic then begin

         Result:=TFuncSimpleExpr.Create(context, scriptPos, meth);
         expr.Free;
         Exit;

      end else if (expr.Typ is TClassOfSymbol) then begin

         if expr.IsConstant then begin
            classSymbol:=TClassOfSymbol(expr.Typ).TypClassSymbol;
            if classSymbol.IsAbstract then begin
               if meth.Kind=fkConstructor then
                  TCheckAbstractClassConstruction.Create(context.Msgs, RTE_InstanceOfAbstractClass, scriptPos, classSymbol)
               else if meth.IsAbstract then
                  TCheckAbstractClassConstruction.Create(context.Msgs, CPE_AbstractClassUsage, scriptPos, classSymbol);
            end;
         end;

      end;
      if (not meth.IsClassMethod) and meth.StructSymbol.IsStatic then
         context.Msgs.AddCompilerErrorFmt(scriptPos, CPE_ClassIsStaticNoInstantiation, [meth.StructSymbol.Name]);

      // Return the right expression
      case meth.Kind of
         fkFunction, fkProcedure, fkMethod, fkLambda:
            if meth.IsClassMethod then begin
               if not (cfoForceStatic in options) and meth.IsVirtual then
                  Result := TClassMethodVirtualExpr.Create(context, scriptPos, meth, expr)
               else Result := TClassMethodStaticExpr.Create(context, scriptPos, meth, expr)
            end else begin
               if RefKind=rkClassOfRef then
                  context.Msgs.AddCompilerError(scriptPos, CPE_StaticMethodExpected)
               else if expr.Typ is TClassOfSymbol then
                  context.Msgs.AddCompilerError(scriptPos, CPE_ClassMethodExpected);
               if not (cfoForceStatic in options) and meth.IsVirtual then
                  Result := TMethodVirtualExpr.Create(context, scriptPos, meth, expr)
               else Result := TMethodStaticExpr.Create(context, scriptPos, meth, expr);
            end;
         fkConstructor:
            if RefKind = rkClassOfRef then begin
               if not (cfoForceStatic in options) and meth.IsVirtual then
                  Result := TConstructorVirtualExpr.Create(context, scriptPos, meth, expr)
               else if meth = context.TypDefaultConstructor then
                  Result := TConstructorStaticDefaultExpr.Create(context, scriptPos, meth, expr)
               else Result := TConstructorStaticExpr.Create(context, scriptPos, meth, expr);
            end else begin
               if not ((context.Prog is TdwsProcedure) and (TdwsProcedure(context.Prog).Func.Kind=fkConstructor)) then
                  context.Msgs.AddCompilerWarning(scriptPos, CPE_UnexpectedConstructor);
               if not (cfoForceStatic in options) and meth.IsVirtual then
                  Result := TConstructorVirtualObjExpr.Create(context, scriptPos, meth, expr)
               else Result := TConstructorStaticObjExpr.Create(context, scriptPos, meth, expr);
            end;
         fkDestructor:
            begin
               if RefKind=rkClassOfRef then
                  context.Msgs.AddCompilerError(scriptPos, CPE_UnexpectedDestructor);
               if not (cfoForceStatic in options) and meth.IsVirtual then
                  Result := TDestructorVirtualExpr.Create(context, scriptPos, meth, expr)
               else Result := TDestructorStaticExpr.Create(context, scriptPos, meth, expr)
            end;
      else
         Assert(False);
      end;

   end else if meth.StructSymbol is TRecordSymbol then begin

      if meth.IsClassMethod then begin

         Result:=TFuncSimpleExpr.Create(context, scriptPos, meth);
         expr.Free;

      end else begin

         Result:=TRecordMethodExpr.Create(context, scriptPos, meth);
         Result.AddArg(expr);

      end;

   end else if meth.StructSymbol is THelperSymbol then begin

      helper:=THelperSymbol(meth.StructSymbol);
      if     meth.IsClassMethod
         and (   (helper.ForType.ClassType=TInterfaceSymbol)
              or meth.IsStatic
              or not (   (helper.ForType is TStructuredTypeSymbol)
                      or (helper.ForType is TStructuredTypeMetaSymbol))) then begin

         Result:=TFuncSimpleExpr.Create(context, scriptPos, meth);
         expr.Free;

      end else begin

         Result:=THelperMethodExpr.Create(context, scriptPos, meth);
         if expr<>nil then
            Result.AddArg(expr);

      end;

   end else Assert(False);

   expr:=nil;
end;

// CreateConstParamSymbol
//
function CreateConstParamSymbol(const name : UnicodeString; typ : TTypeSymbol) : TParamSymbol;
var
   utyp : TTypeSymbol;
begin
   utyp := typ.UnAliasedType;
   if       (utyp.Size = 1)
      and (    (utyp is TBaseSymbol)
            or (utyp is TClassSymbol)
            or (utyp is TDynamicArraySymbol)
            or (utyp is TInterfaceSymbol)
            ) then
      Result := TConstByValueParamSymbol.Create(name, typ)
   else Result := TConstByRefParamSymbol.Create(name, typ);
end;

// ------------------
// ------------------ CompilerUtils ------------------
// ------------------

// IncompatibleTypes
//
class procedure CompilerUtils.IncompatibleTypes(
                        context : TdwsCompilerContext; const scriptPos : TScriptPos;
                        const fmt : UnicodeString; typ1, typ2 : TTypeSymbol);
begin
   context.Msgs.AddCompilerErrorFmt(scriptPos, fmt, [typ1.Caption, typ2.Caption]);
end;

// AddProcHelper
//
class procedure CompilerUtils.AddProcHelper(const name : UnicodeString;
                                            table : TSymbolTable; func : TFuncSymbol;
                                            unitSymbol : TUnitMainSymbol);
var
   helper : THelperSymbol;
   param : TParamSymbol;
   meth : TAliasMethodSymbol;
   i : Integer;
   sym : TSymbol;
begin
   param:=func.Params[0];

   // find a local anonymous helper for the 1st parameter's type
   helper:=nil;
   for sym in table do begin
      if     (sym.Name='')
         and (sym.ClassType=THelperSymbol)
         and (THelperSymbol(sym).ForType=param.Typ) then begin
         helper:=THelperSymbol(sym);
         Break;
      end;
   end;

   // create anonymous helper if necessary
   if helper=nil then begin
      helper:=THelperSymbol.Create('', unitSymbol, param.Typ, table.Count);
      table.AddSymbol(helper);
   end;

   // create helper method symbol
   meth:=TAliasMethodSymbol.Create(name, func.Kind, helper, cvPublic, False);
   meth.SetIsStatic;
   if func.IsOverloaded then
      meth.IsOverloaded:=True;
   meth.Typ:=func.Typ;
   for i:=0 to func.Params.Count-1 do
      meth.Params.AddSymbol(func.Params[i].Clone);
   meth.Alias:=func;
   helper.AddMethod(meth);
end;

// DynamicArrayAdd
//
class function CompilerUtils.DynamicArrayAdd(
      context : TdwsCompilerContext; baseExpr : TTypedExpr;
      const namePos : TScriptPos;
      argList : TTypedExprList; const argPosArray : TScriptPosArray) : TArrayAddExpr;
var
   i : Integer;
   arraySym : TDynamicArraySymbol;
begin
   arraySym:=(baseExpr.Typ.UnAliasedType as TDynamicArraySymbol);
   for i:=0 to argList.Count-1 do begin
      if    (argList[i].Typ=nil)
         or not (   arraySym.Typ.IsCompatible(argList[i].Typ)
                 or arraySym.IsCompatible(argList[i].Typ)
                 or (    (argList[i].Typ is TStaticArraySymbol)
                     and (   arraySym.Typ.IsCompatible(argList[i].Typ.Typ)
                          or (argList[i].Typ.Size=0)))) then begin
         argList[i]:=WrapWithImplicitConversion(context, argList[i], arraySym.Typ, argPosArray[i],
                                                CPE_IncompatibleParameterTypes);
         Break;
      end else if argList[i].ClassType=TArrayConstantExpr then begin
         TArrayConstantExpr(argList[i]).Prepare(context, arraySym.Typ);
      end;
   end;
   Result:=TArrayAddExpr.Create(namePos, baseExpr, argList);
   argList.Clear;
end;

// ArrayConcat
//
class function CompilerUtils.ArrayConcat(context : TdwsCompilerContext; const hotPos : TScriptPos;
                                         left, right : TTypedExpr) : TArrayConcatExpr;
var
   typ : TDynamicArraySymbol;
   leftTyp, rightTyp : TArraySymbol;
begin
   leftTyp:=left.Typ.UnAliasedType as TArraySymbol;
   rightTyp:=left.Typ.UnAliasedType as TArraySymbol;

   if leftTyp.Typ<>rightTyp.Typ then
      IncompatibleTypes(context, hotPos, CPE_IncompatibleTypes, left.Typ, right.Typ);

   typ:=TDynamicArraySymbol.Create('', leftTyp.Typ, context.TypInteger);
   context.Table.AddSymbol(typ);

   Result:=TArrayConcatExpr.Create(hotPos, typ);
   Result.AddArg(left);
   Result.AddArg(right);
end;

// DynamicArrayAdd
//
class function CompilerUtils.DynamicArrayAdd(context : TdwsCompilerContext; baseExpr : TTypedExpr;
      const scriptPos : TScriptPos; argExpr : TTypedExpr) : TArrayAddExpr;
var
   argList : TTypedExprList;
   argPosArray : TScriptPosArray;
begin
   argList:=TTypedExprList.Create;
   try
      argList.AddExpr(argExpr);
      SetLength(argPosArray, 1);
      argPosArray[0]:=scriptPos;
      Result:=DynamicArrayAdd(context, baseExpr, scriptPos, argList, argPosArray);
   finally
      argList.Free;
   end;
end;


// WrapWithImplicitConversion
//
class function CompilerUtils.WrapWithImplicitConversion(
      context : TdwsCompilerContext; expr : TTypedExpr; toTyp : TTypeSymbol;
      const hotPos : TScriptPos;
      const msg : UnicodeString = CPE_IncompatibleTypes) : TTypedExpr;
var
   exprTyp : TTypeSymbol;
begin
   if expr<>nil then
      exprTyp:=expr.Typ
   else exprTyp:=nil;

   if exprTyp.IsOfType(context.TypInteger) and toTyp.IsOfType(context.TypFloat) then begin

      if expr.ClassType=TConstIntExpr then begin
         Result := TConstFloatExpr.Create(context.TypFloat, TConstIntExpr(expr).Value);
         expr.Free;
      end else Result:=TConvIntToFloatExpr.Create(context, expr);

   end else if     (expr.ClassType=TArrayConstantExpr)
               and toTyp.UnAliasedTypeIs(TSetOfSymbol)
               and exprTyp.Typ.IsCompatible(toTyp.Typ) then begin

      Result := TConvStaticArrayToSetOfExpr.Create(hotPos, TArrayConstantExpr(expr), toTyp.UnAliasedType as TSetOfSymbol);

   end else begin
      // error & keep compiling
      IncompatibleTypes(context, hotPos, msg, toTyp, exprTyp);
      Result:=TConvInvalidExpr.Create(context, expr, toTyp);
      Exit;
   end;
end;

// ------------------
// ------------------ TRecursiveHasSubExprClass ------------------
// ------------------

// Create
//
constructor TRecursiveHasSubExprClass.Create(aClass : TExprBaseClass);
begin
   FClass:=aClass;
end;

// Check
//
function TRecursiveHasSubExprClass.Check(expr : TExprBase) : Boolean;
begin
   Result:=expr.RecursiveEnumerateSubExprs(CallBack);
end;

// Callback
//
procedure TRecursiveHasSubExprClass.Callback(parent, expr : TExprBase; var abort : Boolean);
begin
   abort:=abort or (expr is FClass);
end;

// ------------------
// ------------------ TStringToEnum ------------------
// ------------------

// Create
//
constructor TStringToEnum.Create(typ : PTypeInfo; low, high, prefixLength : Integer);
var
   i : Integer;
begin
   for i:=low to high do
      AddObject(Copy(GetEnumName(typ, i), prefixLength+1, 99), TObject(i));
   Sorted := True;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vArrayMethods := TStringToEnum.Create(
      TypeInfo(TArrayMethodKind),
      Ord(Low(TArrayMethodKind)), Ord(High(TArrayMethodKind)), 3
   );

finalization

   FreeAndNil(vArrayMethods);

end.
