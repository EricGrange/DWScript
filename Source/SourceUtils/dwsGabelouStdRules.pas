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
{    Copyright Eric Grange / Creative IT                               }
{                                                                      }
{**********************************************************************}
unit dwsGabelouStdRules;

{$I ../dws.inc}

interface

uses Classes, SysUtils, Character,
   dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsCoreExprs, dwsTokenizer,
   dwsStrings, dwsUnitSymbols, dwsSymbolDictionary,
   dwsGabelou, dwsGabelouStrings, dwsScriptSource;

type

   TGR_CamelCaseParameters = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_CamelCaseLocalVariables = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_PrefixedClassVariables = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_PrefixedPrivateFields = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_PrefixedPublicFields = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_ConstantNamingRules = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_PascalCaseFunctions = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_PascalCaseProperties = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_PascalCaseTypes = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_TypesNaming = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGR_AttributeClassNaming = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

   TGabelouFunctionUseMessage = class (TGabelouMessage)
      public
         constructor Create(msgs : TdwsMessageList; currentFunc : TFuncSymbol; const suggestedFunc : String;
                            const scriptPos : TScriptPos);
   end;

   TGSR_StronglyTypedVarFunctionAlternatives = class (TdwsFunctionUseGabelouSubRule)
      public
         class procedure Evaluate(const aProg : TdwsProgram; msgs : TdwsMessageList;
                                  funcExpr : TFuncExprBase); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsMagicExprs;

// ------------------
// ------------------ TGR_CamelCaseParameters ------------------
// ------------------

// Create
//
constructor TGR_CamelCaseParameters.Create;
begin
   Name:=GAB_CamelCaseParameters_Name;
   Description:=GAB_CamelCaseParameters_Description;
end;

// EvaluateSymbol
//
procedure TGR_CamelCaseParameters.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
var
   paramSymbol : TParamSymbol;
begin
   if not (aSymbolList.Symbol is TParamSymbol) then Exit;
   paramSymbol := TParamSymbol(aSymbolList.Symbol);
   if paramSymbol.IsInternal then Exit;

   if paramSymbol.Name[1].IsUpper() then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_CamelCaseLocalVariables ------------------
// ------------------

// Create
//
constructor TGR_CamelCaseLocalVariables.Create;
begin
   Name:=GAB_CamelCaseLocalVariables_Name;
   Description:=GAB_CamelCaseLocalVariables_Description;
end;

// EvaluateSymbol
//
procedure TGR_CamelCaseLocalVariables.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if    (aSymbolList.Symbol.ClassType<>TDataSymbol)
      or (TDataSymbol(aSymbolList.Symbol).Level<=0) then Exit;

   if aSymbolList.Symbol.Name[1].IsUpper() then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_PascalCaseFunctions ------------------
// ------------------

// Create
//
constructor TGR_PascalCaseFunctions.Create;
begin
   Name:=GAB_PascalCaseFunctions_Name;
   Description:=GAB_PascalCaseFunctions_Description;
end;

// EvaluateSymbol
//
procedure TGR_PascalCaseFunctions.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
var
   funcSym : TFuncSymbol;
begin
   funcSym:=aSymbolList.Symbol.AsFuncSymbol;
   if funcSym=nil then Exit;

   if funcSym.IsExport then Exit;
   if funcSym.IsExternal then Exit;

   if funcSym.Name[1].IsLower() then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_ConstantNamingRules ------------------
// ------------------

// Create
//
constructor TGR_ConstantNamingRules.Create;
begin
   Name:=GAB_ConstsNamingRules_Name;
   Description:=GAB_ConstsNamingRules_Description;
end;

// EvaluateSymbol
//
procedure TGR_ConstantNamingRules.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);

   function ChecksCPrefix(const s : UnicodeString) : Boolean;
   begin
      Result:=(Length(s)>=2) and (s[1]='c') and s[2].IsUpper();
   end;

   function ChecksAllCapsUpToUnderscore(const s : UnicodeString) : Boolean;
   var
      c : WideChar;
      i : Integer;
   begin
      for i:=1 to Length(s) do begin
         c:=s[i];
         if c='_' then
            break
         else if c.IsLower() then
            Exit(False);
      end;
      Result:=True;
   end;

begin
   if not (aSymbolList.Symbol is TConstSymbol) then Exit;
   if aSymbolList.Symbol is TElementSymbol then Exit;
   if aSymbolList.Symbol.Name='Null' then Exit;
   if aSymbolList.Symbol is TClassConstSymbol then Exit;

   if not (ChecksCPrefix(aSymbolList.Symbol.Name) or ChecksAllCapsUpToUnderscore(aSymbolList.Symbol.Name)) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_PrefixedClassVariables ------------------
// ------------------

// Create
//
constructor TGR_PrefixedClassVariables.Create;
begin
   Name:=GAB_PrefixedClassVariables_Name;
   Description:=GAB_PrefixedClassVariables_Description;
end;

// EvaluateSymbol
//
procedure TGR_PrefixedClassVariables.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
var
   sym : TClassVarSymbol;
begin
   if aSymbolList.Symbol.ClassType<>TClassVarSymbol then Exit;

   sym:=TClassVarSymbol(aSymbolList.Symbol);
   if sym.OwnerSymbol.IsStatic then Exit;

   if    (Length(sym.Name)<2)
      or (sym.Name[1]<>'v') or sym.Name[2].IsLower() then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_PrefixedPrivateFields ------------------
// ------------------

// Create
//
constructor TGR_PrefixedPrivateFields.Create;
begin
   Name:=GAB_PrefixedPrivateFields_Name;
   Description := GAB_PrefixedPrivateFields_Description;
end;

// EvaluateSymbol
//
procedure TGR_PrefixedPrivateFields.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
var
   fld : TFieldSymbol;
begin
   if aSymbolList.Symbol.ClassType<>TFieldSymbol then Exit;

   fld:=TFieldSymbol(aSymbolList.Symbol);
   if fld.StructSymbol.IsExternal or (fld.StructSymbol.Name='') then Exit;

   if not (fld.Visibility in [cvPublic, cvPublished]) then begin
      if    (Length(aSymbolList.Symbol.Name)<2)
         or (aSymbolList.Symbol.Name[1]<>'F') or aSymbolList.Symbol.Name[2].IsLower() then
         TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
   end;
end;

// ------------------
// ------------------ TGR_PrefixedPublicFields ------------------
// ------------------

// Create
//
constructor TGR_PrefixedPublicFields.Create;
begin
   Name:=GAB_PrefixedPublicFields_Name;
   Description := GAB_PrefixedPublicFields_Description;
end;

// EvaluateSymbol
//
procedure TGR_PrefixedPublicFields.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
var
   fld : TFieldSymbol;
begin
   if aSymbolList.Symbol.ClassType<>TFieldSymbol then Exit;

   fld:=TFieldSymbol(aSymbolList.Symbol);
   if fld.StructSymbol.IsExternal or (fld.StructSymbol.Name='') then Exit;

   if fld.Visibility in [cvPublic, cvPublished] then begin
      if aSymbolList.Symbol.Name[1].IsLower() then
         TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
   end;
end;

// ------------------
// ------------------ TGR_PascalCaseProperties ------------------
// ------------------

// Create
//
constructor TGR_PascalCaseProperties.Create;
begin
   Name:=GAB_PascalCaseProperties_Name;
   Description:=GAB_PascalCaseProperties_Description;
end;

// EvaluateSymbol
//
procedure TGR_PascalCaseProperties.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if aSymbolList.Symbol.ClassType<>TPropertySymbol then Exit;
   if TPropertySymbol(aSymbolList.Symbol).OwnerSymbol.IsExternal then Exit;

   if aSymbolList.Symbol.Name[1].IsLower() then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_PascalCaseTypes ------------------
// ------------------

// Create
//
constructor TGR_PascalCaseTypes.Create;
begin
   Name:=GAB_PascalCaseTypes_Name;
   Description:=GAB_PascalCaseTypes_Description;
end;

// EvaluateSymbol
//
procedure TGR_PascalCaseTypes.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
begin
   if not (aSymbolList.Symbol is TTypeSymbol) then Exit;
   if aSymbolList.Symbol.AsFuncSymbol<>nil then Exit;
   if aSymbolList.Symbol is TUnitSymbol then Exit;

   if aSymbolList.Symbol.Name[1].IsLower() then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_TypesNaming ------------------
// ------------------

// Create
//
constructor TGR_TypesNaming.Create;
begin
   Name := GAB_Types_Name;
   Description := GAB_Types_Description;
end;

// EvaluateSymbol
//
procedure TGR_TypesNaming.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
var
   symName : String;
   classSymbol : TClassSymbol;
   typeSymbol : TTypeSymbol;
   isException : Boolean;
begin
   if not aSymbolList.Symbol.IsType then Exit;

   symName := aSymbolList.Symbol.Name;
   if (symName[1] = 'E') and (Length(symName) > 1) and CharInSet(symName[2], ['A'..'Z']) then begin

      isException := False;

      typeSymbol := TTypeSymbol(aSymbolList.Symbol).UnAliasedType;
      if typeSymbol is TClassSymbol then begin
         classSymbol := TClassSymbol(typeSymbol);
         while classSymbol.Parent <> nil do begin
            classSymbol := classSymbol.Parent;
            if classSymbol.Name = 'Exception' then begin
               isException := True;
               Break;
            end;
         end;
      end;

      if not isException then
         TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description)
   end;
end;


// ------------------
// ------------------ TGR_AttributeClassNaming ------------------
// ------------------

// Create
//
constructor TGR_AttributeClassNaming.Create;
begin
   Name:=GAB_AttributeClassNaming_Name;
   Description:=GAB_AttributeClassNaming_Description;
end;

// EvaluateSymbol
//
procedure TGR_AttributeClassNaming.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
var
   cls : TClassSymbol;
begin
   if aSymbolList.Symbol.ClassType<>TClassSymbol then Exit;

   cls:=TClassSymbol(aSymbolList.Symbol);
   if not cls.IsAttribute then Exit;
   if cls.IsAbstract then Exit;
   if cls.Name=SYS_TCUSTOMATTRIBUTE then Exit;

   if    (not StrEndsWith(cls.Name, 'Attribute'))
      or (StrBeginsWith(cls.Name, 'T') and not (Copy(cls.Name, 2, 1)+'e')[1].IsLower()) then begin
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
   end;
end;

// ------------------
// ------------------ TGabelouFunctionUseMessage ------------------
// ------------------

// Create
//
constructor TGabelouFunctionUseMessage.Create(msgs : TdwsMessageList; currentFunc : TFuncSymbol; const suggestedFunc : String;
                                              const scriptPos : TScriptPos);
begin
   inherited Create(msgs, 'replace weakly typed %s with strongly typed function %s',
                    [ currentFunc.Name, suggestedFunc ], scriptPos);;

end;

// ------------------
// ------------------ TGSR_StronglyTypedVarFunctionAlternatives ------------------
// ------------------

// Evaluate
//
class procedure TGSR_StronglyTypedVarFunctionAlternatives.Evaluate(
   const aProg : TdwsProgram; msgs : TdwsMessageList; funcExpr : TFuncExprBase);
var
   magic : TMagicFuncSymbol;
begin
   if funcExpr.Args.Count = 0 then Exit;
   if funcExpr.FuncSym is TMagicFuncSymbol then begin
      magic := TMagicFuncSymbol(funcExpr.FuncSym);
      if (magic.Name = 'VarToIntDef') and funcExpr.ArgIsOfType(0, aProg.Root.CompilerContext.TypString) then
         TGabelouFunctionUseMessage.Create(msgs, magic, 'StrToIntDef', funcExpr.ScriptPos);
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsGabelou.RegisterRuleClasses([
      TGR_CamelCaseParameters, TGR_CamelCaseLocalVariables,

      TGR_ConstantNamingRules,

      TGR_PascalCaseFunctions, TGR_PascalCaseProperties, TGR_PascalCaseTypes,
      TGR_TypesNaming,
      TGR_AttributeClassNaming,

      TGR_PrefixedPrivateFields, TGR_PrefixedPublicFields, TGR_PrefixedClassVariables,

      TdwsFunctionUseGabelouRule
      ]);

   TdwsFunctionUseGabelouRule.RegisterSubRule(
      TGSR_StronglyTypedVarFunctionAlternatives
   );

end.
