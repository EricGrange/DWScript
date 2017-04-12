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
   dwsConvExprs, dwsTokenizer, dwsOperators, dwsConnectorSymbols;

type

   CompilerUtils = class
      public
         class procedure IncompatibleTypes(
                        context : TdwsCompilerContext; const scriptPos : TScriptPos;
                        const fmt : String; typ1, typ2 : TTypeSymbol); static;

         class function WrapWithImplicitConversion(
                        context : TdwsCompilerContext; expr : TTypedExpr; toTyp : TTypeSymbol;
                        const hotPos : TScriptPos;
                        const msg : String = CPE_IncompatibleTypes) : TTypedExpr; static;

         class procedure AddProcHelper(const name : String;
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
      amkClear, amkSwap, amkCopy, amkReverse, amkDimCount, amkKeys
   );


function NameToArrayMethod(const name : String; msgs : TdwsCompileMessageList;
                           const namePos : TScriptPos) : TArrayMethodKind;

function CreateAssignExpr(context : TdwsCompilerContext;
                          const scriptPos : TScriptPos; token : TTokenType;
                          left : TDataExpr; right : TTypedExpr) : TProgramExpr;

function CreateSimpleFuncExpr(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                              funcSym: TFuncSymbol) : TFuncExprBase;

function CreateFuncExpr(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
                        funcSym: TFuncSymbol;
                        const scriptObj : IScriptObj; structSym : TCompositeTypeSymbol;
                        options : TCreateFunctionOptions) : TFuncExprBase;
function CreateIntfExpr(context : TdwsCompilerContext; funcSym: TFuncSymbol;
                        const scriptObjIntf : IScriptObjInterface) : TFuncExprBase;

function CreateMethodExpr(context : TdwsCompilerContext; meth: TMethodSymbol; var expr : TTypedExpr; RefKind: TRefKind;
                          const scriptPos: TScriptPos; options : TCreateFunctionOptions) : TFuncExprBase;

procedure TypeCheckArguments(context : TdwsCompilerContext; funcExpr : TFuncExprBase;
                             const argPosArray : TScriptPosArray);

function CreateConstParamSymbol(const name : String; typ : TTypeSymbol) : TParamSymbol;

function ResolveOperatorFor(currentProg : TdwsProgram; token : TTokenType;
                            aLeftType, aRightType : TTypeSymbol) : TOperatorSymbol;
function CreateTypedOperatorExpr(context : TdwsCompilerContext; token : TTokenType;
                                 const scriptPos : TScriptPos;
                                 aLeft, aRight : TTypedExpr) : TTypedExpr;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsGenericExprs;

// NameToArrayMethod
//
var
   vArrayMethods : TStringToEnum;
function NameToArrayMethod(const name : String; msgs : TdwsCompileMessageList;
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

// CreateAssignOperatorExpr
//
function CreateAssignOperatorExpr(context : TdwsCompilerContext;
                                  token : TTokenType; const scriptPos : TScriptPos;
                                  aLeft, aRight : TTypedExpr) : TAssignExpr;
var
   opSym : TOperatorSymbol;
begin
   Result:=nil;
   if (aLeft=nil) or (aRight=nil) then Exit;
   opSym:=ResolveOperatorFor(context.Prog as TdwsProgram, token, aLeft.Typ, aRight.Typ);
   if opSym<>nil then begin
      if opSym.AssignExprClass<>nil then
         Result:=TAssignExprClass(opSym.AssignExprClass).Create(context, scriptPos, aLeft as TDataExpr, aRight);
   end;
end;

// CreateAssignExpr
//
function CreateAssignExpr(context : TdwsCompilerContext;
                          const scriptPos : TScriptPos; token : TTokenType;
                          left : TDataExpr; right : TTypedExpr) : TProgramExpr;
var
   classOpSymbol : TClassOperatorSymbol;
   classOpExpr : TFuncExprBase;
   assignOpExpr : TAssignExpr;
   classSymbol : TClassSymbol;
   intfSymbol : TInterfaceSymbol;
   leftTyp : TTypeSymbol;
begin
   if (right<>nil) and (right.Typ<>nil) then begin

      case token of
         ttASSIGN : begin
            leftTyp:=left.Typ;
            if leftTyp=nil then begin
               // error assumed to have already been reported
               left.Orphan(context);
               right.Orphan(context);
               Result:=TNullExpr.Create(scriptPos);
            end else begin
               leftTyp:=leftTyp.UnAliasedType;
               if leftTyp.ClassType=TClassOfSymbol then begin
                  Result:=TAssignClassOfExpr.Create(context, scriptPos, left, right);
               end else if leftTyp.ClassType=TInterfaceSymbol then begin
                  if right.Typ is TClassSymbol then begin
                     classSymbol:=TClassSymbol(right.Typ);
                     intfSymbol:=TInterfaceSymbol(left.Typ);
                     if not classSymbol.ImplementsInterface(intfSymbol) then
                        context.Msgs.AddCompilerErrorFmt(scriptPos, RTE_ObjCastToIntfFailed,
                                                         [classSymbol.Name, intfSymbol.Name]);
                     Result:=TAssignExpr.Create(context, scriptPos, left,
                                                TObjAsIntfExpr.Create(context, scriptPos, right, intfSymbol));
                  end else Result:=TAssignExpr.Create(context, scriptPos, left, right);
               end else if leftTyp.ClassType=TDynamicArraySymbol then begin
                  if right.ClassType=TConstNilExpr then begin
                     right.Orphan(context);
                     Result:=TAssignNilAsResetExpr.CreateVal(context, scriptPos, left);
                  end else if right.ClassType = TArrayConstantExpr then begin
                     if TArrayConstantExpr(right).ElementCount = 0 then begin
                        right.Orphan(context);
                        Result := TAssignNilAsResetExpr.CreateVal(context, scriptPos, left);
                     end else begin
                        Result := TAssignArrayConstantExpr.Create(context, scriptPos, left, TArrayConstantExpr(right));
                     end
                  end else begin
                     Result:=TAssignExpr.Create(context, scriptPos, left, right);
                  end;
               end else if leftTyp is TAssociativeArraySymbol then begin
                  if right.ClassType=TConstNilExpr then begin
                     right.Orphan(context);
                     Result:=TAssignNilAsResetExpr.CreateVal(context, scriptPos, left);
                  end else begin
                     Result:=TAssignExpr.Create(context, scriptPos, left, right);
                  end;
               end else if     right.InheritsFrom(TDataExpr)
                           and (   (right.Typ.Size<>1)
                                or (right.Typ is TArraySymbol)
                                or (right.Typ is TRecordSymbol)
                                or (right.Typ is TSetOfSymbol)) then begin
                  if right.InheritsFrom(TFuncExpr) then
                     TFuncExpr(right).SetResultAddr(context.Prog as TdwsProgram, nil);
                  if right.InheritsFrom(TArrayConstantExpr) and (left.Typ is TArraySymbol) then
                     Result:=TAssignArrayConstantExpr.Create(context, scriptPos, left, TArrayConstantExpr(right))
                  else Result:=TAssignDataExpr.Create(context, scriptPos, left, right)
               end else if leftTyp.AsFuncSymbol<>nil then begin
                  if (right.Typ.AsFuncSymbol<>nil) or (right.Typ is TNilSymbol) then begin
                     if right is TFuncRefExpr then begin
                        right:=TFuncRefExpr(right).Extract;
                        if right is TFuncPtrExpr then begin
                           right:=TFuncPtrExpr(right).Extract;
                           Result:=TAssignExpr.Create(context, scriptPos, left, right);
                        end else begin
                           Assert(right is TFuncExprBase);
                           Result:=TAssignFuncExpr.Create(context, scriptPos, left, right);
                        end;
                     end else begin
                        Result:=TAssignExpr.Create(context, scriptPos, left, right);
                     end;
                  end else begin
                     context.Msgs.AddCompilerError(scriptPos, CPE_IncompatibleOperands);
                     Result:=TAssignExpr.Create(context, scriptPos, left, right); // keep going
                  end;
               end else if leftTyp is TConnectorSymbol then begin
                  Result:=TConnectorSymbol(leftTyp).CreateAssignExpr(context, scriptPos, left, right);
               end else begin
                  if left.IsExternal then
                     Result:=TAssignExternalExpr.Create(context, scriptPos, left, right)
                  else Result:=TAssignExpr.Create(context, scriptPos, left, right);
               end;
            end;
         end;
         ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN, ttCARET_ASSIGN : begin
            if left.Typ is TClassSymbol then begin

               classOpSymbol:=(left.Typ as TClassSymbol).FindClassOperator(token, right.Typ);
               if classOpSymbol=nil then
                  context.Msgs.AddCompilerStop(scriptPos, CPE_IncompatibleOperands);
               classOpExpr := CreateMethodExpr(context, classOpSymbol.UsesSym, TTypedExpr(left), rkObjRef, scriptPos, []);
               classOpExpr.AddArg(right);
               TypeCheckArguments(context, classOpExpr, nil);
               Result:=classOpExpr;

            end else begin

               assignOpExpr := CreateAssignOperatorExpr(context, token, scriptPos, left, right);
               if assignOpExpr<>nil then

                  Result:=assignOpExpr

               else if left.Typ is TDynamicArraySymbol then begin

                  Result:=CompilerUtils.DynamicArrayAdd(context, left, scriptPos, right);

               end else if left.IsGeneric or right.IsGeneric then begin

                  Result := TGenericAssignExpr.Create(context, scriptPos, token, left, right);

               end else begin

                  context.Msgs.AddCompilerError(scriptPos, CPE_IncompatibleOperands);
                  Result:=TAssignExpr.Create(context, scriptPos, left, right);

               end;

            end;
         end;
      else
         Result:=nil;
         Assert(False);
      end;

      if context.Optimize then
         Result:=Result.Optimize(context);

   end else begin

      left.Orphan(context);
      right.Orphan(context);
      context.Msgs.AddCompilerError(scriptPos, CPE_RightSideNeedsReturnType);
      Result:=TNullExpr.Create(scriptPos);

   end;
end;

type
   TCheckAbstractClassConstruction = class (TErrorMessage)
      FClassSym : TClassSymbol;
      constructor Create(msgs: TdwsMessageList; const text : String; const p : TScriptPos;
                         classSym : TClassSymbol); overload;
      function IsValid : Boolean; override;
   end;

// Create
//
constructor TCheckAbstractClassConstruction.Create(msgs: TdwsMessageList; const text : String; const p : TScriptPos;
                                                   classSym : TClassSymbol);
begin
   FClassSym:=classSym;
   inherited Create(msgs, Format(MSG_Error, [text]), p);
end;

// IsValid
//
function TCheckAbstractClassConstruction.IsValid : Boolean;
begin
   Result:=FClassSym.IsAbstract and (MessageList.State=mlsCompleted);
end;

// CreateSimpleFuncExpr
//
function CreateSimpleFuncExpr(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                              funcSym: TFuncSymbol) : TFuncExprBase;
var
   magicFuncSym : TMagicFuncSymbol;
begin
   if funcSym.IsType then
      context.Msgs.AddCompilerError(aScriptPos, CPE_FunctionMethodExpected);

   if funcSym.InheritsFrom(TMagicFuncSymbol) then begin

      magicFuncSym := TMagicFuncSymbol(funcSym);
      Result := TMagicFuncExpr.CreateMagicFuncExpr(context, aScriptPos, magicFuncSym);

   end else Result := TFuncSimpleExpr.Create(context, aScriptPos, funcSym);
end;

// CreateFuncExpr
//
function CreateFuncExpr(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
                        funcSym: TFuncSymbol;
                        const scriptObj : IScriptObj; structSym : TCompositeTypeSymbol;
                        options : TCreateFunctionOptions) : TFuncExprBase;
var
   instanceExpr : TTypedExpr;
begin
   if funcSym is TMethodSymbol then begin
      if Assigned(scriptObj) then begin
         instanceExpr:=TConstExpr.Create(structSym, scriptObj);
         Result:=CreateMethodExpr(context, TMethodSymbol(funcSym),
                                  instanceExpr, rkObjRef, aScriptPos, options)
      end else if structSym<>nil then begin
         instanceExpr:=TConstExpr.Create(structSym.MetaSymbol, Int64(structSym));
         Result:=CreateMethodExpr(context, TMethodSymbol(funcSym),
                                  instanceExpr, rkClassOfRef, aScriptPos, options)
      end else begin
         // static method
         structSym:=TMethodSymbol(funcSym).StructSymbol;
         if structSym is TStructuredTypeSymbol then begin
            instanceExpr:=TConstExpr.Create(structSym.MetaSymbol, Int64(structSym));
            Result:=CreateMethodExpr(context, TMethodSymbol(funcSym),
                                     instanceExpr, rkClassOfRef, aScriptPos, options)
         end else begin
            Result:=nil;
            Assert(False, 'TODO');
         end;
      end;
   end else begin
      Result := CreateSimpleFuncExpr(context, aScriptPos, funcSym);
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
   if meth is TAliasMethodSymbol then begin

      Result:=CreateSimpleFuncExpr(context, scriptPos, TAliasMethodSymbol(meth).Alias);
      Result.AddArg(expr);
      Exit;

   end;


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
         if (meth.Typ<>nil) and (meth.Typ.Size>1) then
            Result.SetResultAddr(context.Prog as TdwsProgram, nil);
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

   if (meth.Typ<>nil) and (meth.Typ.Size>1) then
      Result.SetResultAddr(context.Prog as TdwsProgram, nil);
end;

// TypeCheckArguments
//
procedure TypeCheckArguments(context : TdwsCompilerContext; funcExpr : TFuncExprBase;
                             const argPosArray : TScriptPosArray);

   procedure WrongArgumentError(const argPos : TScriptPos; n : Integer; typ : TTypeSymbol);
   begin
      if not typ.IsGeneric then
         context.Msgs.AddCompilerErrorFmt(argPos, CPE_WrongArgumentType, [n, typ.Caption]);
   end;

   procedure WrongArgumentLongError(const argPos : TScriptPos; n : Integer; typ1, typ2 : TTypeSymbol);
   begin
      if not (typ1.IsGeneric or typ2.IsGeneric) then
         context.Msgs.AddCompilerErrorFmt(argPos, CPE_WrongArgumentType_Long,
                                          [n, typ1.Caption, typ2.Caption]);
   end;

var
   arg : TTypedExpr;
   i, paramCount, nbParamsToCheck : Integer;
   funcSym : TFuncSymbol;
   paramSymbol : TParamSymbol;
   argTyp : TTypeSymbol;
   initialErrorCount : Integer;
   tooManyArguments, tooFewArguments : Boolean;
   argPos : TScriptPos;
begin
   funcSym:=funcExpr.FuncSym;

   paramCount:=funcSym.Params.Count;

   initialErrorCount:=context.Msgs.Count;

   // Check number of arguments = number of parameters
   if funcExpr.Args.Count>paramCount then begin
      tooManyArguments:=True;
      while funcExpr.Args.Count>paramCount do begin
         context.OrphanObject(funcExpr.Args.ExprBase[funcExpr.Args.Count-1]);
         funcExpr.Args.Delete(funcExpr.Args.Count-1);
      end;
   end else tooManyArguments:=False;

   tooFewArguments:=False;
   while funcExpr.Args.Count<paramCount do begin
      // Complete missing args by default values
      paramSymbol:=TParamSymbol(funcSym.Params[funcExpr.Args.Count]);
      if paramSymbol is TParamSymbolWithDefaultValue then
         funcExpr.Args.Add(TConstExpr.CreateTyped(context, paramSymbol.Typ,
                                                  TParamSymbolWithDefaultValue(paramSymbol).DefaultValue))
      else begin
         tooFewArguments:=True;
         Break;
      end;
   end;

   if paramCount<funcExpr.Args.Count then
      nbParamsToCheck:=paramCount
   else nbParamsToCheck:=funcExpr.Args.Count;

   for i := 0 to nbParamsToCheck-1 do begin
      arg := TTypedExpr(funcExpr.Args.ExprBase[i]);
      if arg.ClassType = TErrorValueExpr then continue;

      paramSymbol:=TParamSymbol(funcSym.Params[i]);
      if i < Length(argPosArray) then
         argPos:=argPosArray[i]
      else argPos:=funcExpr.ScriptPos;

      if arg.ClassType=TArrayConstantExpr then
         TArrayConstantExpr(arg).Prepare(context, paramSymbol.Typ.Typ);

      argTyp:=arg.Typ;
      // Wrap-convert arguments if necessary and possible
      if paramSymbol.ClassType<>TVarParamSymbol then begin
         arg:=TConvExpr.WrapWithConvCast(context, argPos, paramSymbol.Typ, arg, '');
      end;
      funcExpr.Args.ExprBase[i] := arg;

      if argTyp=nil then
         WrongArgumentError(argPos, i, paramSymbol.Typ)
      else if not paramSymbol.Typ.IsCompatible(arg.Typ) then
         WrongArgumentLongError(argPos, i, paramSymbol.Typ, arg.Typ)
      else if paramSymbol.ClassType=TVarParamSymbol then begin
         if not paramSymbol.Typ.IsOfType(arg.Typ) then
            WrongArgumentLongError(argPos, i, paramSymbol.Typ, argTyp);
         if arg is TDataExpr then begin
            if     (coVariablesAsVarOnly in context.Options)
               and (not (arg is TVarExpr))
               and (not (argTyp.UnAliasedType.ClassType=TRecordSymbol))
               and (   (i > 0)
                    or (not (funcSym is TMethodSymbol))
                    or (not (TMethodSymbol(funcSym).StructSymbol is TRecordSymbol))
                    or TMethodSymbol(funcSym).IsClassMethod
                    ) then
               context.Msgs.AddCompilerError(argPos, CPE_OnlyVariablesAsVarParam)
            // Record methods ignore the IsWritable constraints, as in Delphi
            else if     (not TDataExpr(arg).IsWritable)
                    and (   (i > 0)
                         or (not (funcSym is TMethodSymbol))
                         or (not (TMethodSymbol(funcSym).StructSymbol is TRecordSymbol))) then
               context.Msgs.AddCompilerErrorFmt(argPos, CPE_ConstVarParam, [i, paramSymbol.Name]);
         end else context.Msgs.AddCompilerErrorFmt(argPos, CPE_ConstVarParam, [i, paramSymbol.Name]);
      end;

   end;

   if initialErrorCount=context.Msgs.Count then begin
      if tooManyArguments then
         context.Msgs.AddCompilerError(funcExpr.ScriptPos, CPE_TooManyArguments);
      if tooFewArguments then
         context.Msgs.AddCompilerError(funcExpr.ScriptPos, CPE_TooFewArguments);
   end;

   funcExpr.CompileTimeCheck(context);

   funcExpr.Initialize(context);
end;

// CreateConstParamSymbol
//
function CreateConstParamSymbol(const name : String; typ : TTypeSymbol) : TParamSymbol;
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

type
   TOperatorResolver = class
      private
         Resolved : TOperatorSymbol;
         LeftType, RightType : TTypeSymbol;
         function Callback(opSym : TOperatorSymbol) : Boolean;
   end;

// Callback
//
function TOperatorResolver.Callback(opSym : TOperatorSymbol) : Boolean;
begin
   Result:=(opSym.Params[0]=LeftType) and (opSym.Params[1]=RightType);
   if Result or (Resolved=nil) then
      Resolved:=opSym;
end;

// ResolveOperatorFor
//
function ResolveOperatorFor(currentProg : TdwsProgram; token : TTokenType;
                            aLeftType, aRightType : TTypeSymbol) : TOperatorSymbol;
var
   operatorResolver : TOperatorResolver;
begin
   operatorResolver := TOperatorResolver.Create;
   try
      operatorResolver.Resolved:=nil;
      operatorResolver.LeftType:=aLeftType;
      operatorResolver.RightType:=aRightType;

      if aLeftType=nil then begin
         if currentProg.Table.HasOperators then
            if currentProg.Table.EnumerateOperatorsFor(token, nil, aRightType, operatorResolver.Callback) then
               Exit(operatorResolver.Resolved);
         (currentProg.Root.Operators as TOperators).EnumerateUnaryOperatorsFor(token, aRightType, operatorResolver.Callback);
      end else begin
         if currentProg.Table.HasOperators then
            if currentProg.Table.EnumerateOperatorsFor(token, aLeftType, aRightType, operatorResolver.Callback) then
               Exit(operatorResolver.Resolved);
         (currentProg.Root.Operators as TOperators).EnumerateOperatorsFor(token, aLeftType, aRightType, operatorResolver.Callback);
      end;
      Result:=operatorResolver.Resolved;
   finally
      operatorResolver.Free;
   end;
end;

// CreateTypedOperatorExpr
//
function CreateTypedOperatorExpr(context : TdwsCompilerContext; token : TTokenType;
                                 const scriptPos : TScriptPos;
                                 aLeft, aRight : TTypedExpr) : TTypedExpr;
var
   opSym : TOperatorSymbol;
   funcExpr : TFuncExprBase;
begin
   Result := nil;
   if (aLeft=nil) or (aRight=nil) then Exit;
   opSym:=ResolveOperatorFor(context.Prog as TdwsProgram, token, aLeft.Typ, aRight.Typ);
   if opSym=nil then Exit;

   if opSym.OperatorExprClass <> nil then begin
      Result := TBinaryOpExprClass(opSym.OperatorExprClass).Create(context, scriptPos, token, aLeft, aRight);
   end else if opSym.UsesSym<>nil then begin
      if opSym.UsesSym is TMethodSymbol then
         funcExpr:=CreateMethodExpr(context, TMethodSymbol(opSym.UsesSym), aLeft, rkObjRef, scriptPos, [])
      else begin
         funcExpr := CreateFuncExpr(context, scriptPos, opSym.UsesSym, nil, nil, []);
         funcExpr.AddArg(aLeft);
      end;
      funcExpr.AddArg(aRight);
      TypeCheckArguments(context, funcExpr, nil);
      Result:=funcExpr;
   end;
end;

// ------------------
// ------------------ CompilerUtils ------------------
// ------------------

// IncompatibleTypes
//
class procedure CompilerUtils.IncompatibleTypes(
                        context : TdwsCompilerContext; const scriptPos : TScriptPos;
                        const fmt : String; typ1, typ2 : TTypeSymbol);
begin
   context.Msgs.AddCompilerErrorFmt(scriptPos, fmt, [typ1.Caption, typ2.Caption]);
end;

// AddProcHelper
//
class procedure CompilerUtils.AddProcHelper(const name : String;
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
      const msg : String = CPE_IncompatibleTypes) : TTypedExpr;
var
   exprTyp : TTypeSymbol;
begin
   if expr<>nil then begin
      if context.WrapWithImplicitCast(toTyp, hotPos, expr) then
         Exit(expr);
      exprTyp := expr.Typ
   end else exprTyp := nil;

   if     (expr.ClassType=TArrayConstantExpr)
      and toTyp.UnAliasedTypeIs(TSetOfSymbol)
      and exprTyp.Typ.IsCompatible(toTyp.Typ) then begin

      Result := TConvStaticArrayToSetOfExpr.Create(hotPos, TArrayConstantExpr(expr), toTyp.UnAliasedType as TSetOfSymbol);

   end else if expr.Typ.IsGeneric or toTyp.IsGeneric then begin

      Result := expr;

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
