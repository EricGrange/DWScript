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
   dwsGabelou, dwsGabelouStrings;

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

   TGR_PrefixedFields = class abstract (TdwsSymbolDictionaryGabelouRule)
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

   TGR_AttributeClassNaming = class abstract (TdwsSymbolDictionaryGabelouRule)
      public
         constructor Create; override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

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
begin
   if not (aSymbolList.Symbol is TParamSymbol) then Exit;
   if aSymbolList.Symbol.Name='Self' then Exit;

   if TCharacter.IsUpper(aSymbolList.Symbol.Name[1]) then
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

   if TCharacter.IsUpper(aSymbolList.Symbol.Name[1]) then
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

   if TCharacter.IsLower(funcSym.Name[1]) then
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
      Result:=(Length(s)>=2) and (s[1]='c') and TCharacter.IsUpper(s[2]);
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
         else if TCharacter.IsLower(c) then
            Exit(False);
      end;
      Result:=True;
   end;

begin
   if not (aSymbolList.Symbol is TConstSymbol) then Exit;
   if aSymbolList.Symbol is TElementSymbol then Exit;
   if aSymbolList.Symbol.Name='Null' then Exit;

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
      or (sym.Name[1]<>'v') or TCharacter.IsLower(sym.Name[2]) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
end;

// ------------------
// ------------------ TGR_PrefixedFields ------------------
// ------------------

// Create
//
constructor TGR_PrefixedFields.Create;
begin
   Name:=GAB_PrefixedFields_Name;
   Description:=GAB_PrefixedFields_Description;
end;

// EvaluateSymbol
//
procedure TGR_PrefixedFields.EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList);
var
   fld : TFieldSymbol;
begin
   if aSymbolList.Symbol.ClassType<>TFieldSymbol then Exit;

   fld:=TFieldSymbol(aSymbolList.Symbol);
   if fld.StructSymbol.IsExternal or (fld.StructSymbol.Name='') then Exit;

   if not (fld.Visibility in [cvPublic, cvPublished]) then begin
      if    (Length(aSymbolList.Symbol.Name)<2)
         or (aSymbolList.Symbol.Name[1]<>'F') or TCharacter.IsLower(aSymbolList.Symbol.Name[2]) then
         TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
   end else begin
      if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
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

   if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
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

   if TCharacter.IsLower(aSymbolList.Symbol.Name[1]) then
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
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
      or (StrBeginsWith(cls.Name, 'T') and not TCharacter.IsLower((Copy(cls.Name, 2, 1)+'e')[1])) then begin
      TGabelouMessage.CreateOnSymbolPosList(msgs, aSymbolList, Description);
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
      TGR_AttributeClassNaming,

      TGR_PrefixedFields, TGR_PrefixedClassVariables
      ]);

end.
