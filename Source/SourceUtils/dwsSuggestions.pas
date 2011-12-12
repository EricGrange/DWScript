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
unit dwsSuggestions;

{$I ../dws.inc}

interface

uses Classes, SysUtils, dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsTokenizer,
   dwsUnitSymbols, dwsPascalTokenizer;

type

   // code suggestions helper (ala ctrl+space in Delphi)
   // needs prog to have been compiler with coSymbolDictionary & coContextMap

   TdwsSuggestionCategory = (scUnknown,
                             scUnit, scType,
                             scClass, scRecord, scInterface, scDelegate,
                             scFunction, scProcedure, scMethod,
                             scConstructor, scDestructor,
                             scProperty,
                             scEnum, scElement,
                             scParameter,
                             scVariable, scConst);

   IdwsSuggestions = interface
      ['{09CA8BF2-AF3F-4B5A-B188-4B2FF574AC34}']
      function GetCode(i : Integer) : UnicodeString;
      function GetCategory(i : Integer) : TdwsSuggestionCategory;
      function GetCaption(i : Integer) : UnicodeString;
      function GetSymbols(i : Integer) : TSymbol;

      property Code[i : Integer] : UnicodeString read GetCode;
      property Category[i : Integer] : TdwsSuggestionCategory read GetCategory;
      property Caption[i : Integer] : UnicodeString read GetCaption;
      property Symbols[i : Integer] : TSymbol read GetSymbols;
      function Count : Integer;

      function PartialToken : UnicodeString;
   end;

   // Pseudo-symbol for suggestion purposes
   TReservedWordSymbol = class(TSymbol)
   end;

   TSimpleSymbolList = class;

   TProcAddToList = procedure (aList : TSimpleSymbolList) of object;

   TSimpleSymbolList = class(TSimpleList<TSymbol>)
      protected
         function ScopeStruct(symbol : TSymbol) : TStructuredTypeSymbol;
         // min visibility of struct members when seen from scope
         function ScopeVisiblity(scope, struct : TStructuredTypeSymbol) : TdwsVisibility;

      public
         procedure AddSymbolTable(table : TSymbolTable);
         procedure AddDirectSymbolTable(table : TSymbolTable);

         procedure AddMembers(struc : TStructuredTypeSymbol; from : TSymbol;
                              const addToList : TProcAddToList = nil);
         procedure AddMetaMembers(struc : TStructuredTypeSymbol; from : TSymbol;
                                  const addToList : TProcAddToList = nil);
   end;

   TdwsSuggestionsOption = (soNoReservedWords);
   TdwsSuggestionsOptions = set of TdwsSuggestionsOption;

   TdwsSuggestions = class (TInterfacedObject, IdwsSuggestions)
      private
         FProg : IdwsProgram;
         FSourcePos : TScriptPos;
         FSourceFile : TSourceFile;
         FList : TSimpleSymbolList;
         FCleanupList : TTightList;
         FListLookup : TObjectsLookup;
         FNamesLookup : TStringList;
         FPartialToken : UnicodeString;
         FPreviousSymbol : TSymbol;
         FPreviousTokenString : UnicodeString;
         FPreviousToken : TTokenType;
         FLocalContext : TContext;
         FContextSymbol : TSymbol;
         FSymbolClassFilter : TSymbolClass;

      protected
         function GetCode(i : Integer) : UnicodeString;
         function GetCategory(i : Integer) : TdwsSuggestionCategory;
         function GetCaption(i : Integer) : UnicodeString;
         function GetSymbols(i : Integer) : TSymbol;
         function Count : Integer;

         function PartialToken : UnicodeString;

         procedure AnalyzeLocalTokens;

         procedure AddToList(aList : TSimpleSymbolList);
         function IsContextSymbol(sym : TSymbol) : Boolean;

         procedure AddReservedWords;
         procedure AddImmediateSuggestions;
         procedure AddContextSuggestions;
         procedure AddUnitSuggestions;
         procedure AddGlobalSuggestions;

      public
         constructor Create(const prog : IdwsProgram; const sourcePos : TScriptPos;
                            const options : TdwsSuggestionsOptions = []);
         destructor Destroy; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsSuggestions ------------------
// ------------------

// Create
//
constructor TdwsSuggestions.Create(const prog : IdwsProgram; const sourcePos : TScriptPos;
                                   const options : TdwsSuggestionsOptions = []);
begin
   FProg:=prog;
   FSourcePos:=sourcePos;
   FSourceFile:=sourcePos.SourceFile;
   FList:=TSimpleSymbolList.Create;
   FListLookup:=TObjectsLookup.Create;
   FNamesLookup:=TFastCompareStringList.Create;
   FNamesLookup.Sorted:=True;

   AnalyzeLocalTokens;

   AddImmediateSuggestions;
   AddContextSuggestions;
   AddUnitSuggestions;
   AddGlobalSuggestions;

   if not (soNoReservedWords in options) then
      AddReservedWords;

   FNamesLookup.Clear;
   FListLookup.Clear;
end;

// Destroy
//
destructor TdwsSuggestions.Destroy;
begin
   FNamesLookup.Free;
   FListLookup.Free;
   FList.Free;
   FCleanupList.Clean;
   inherited;
end;

// AnalyzeLocalTokens
//
procedure TdwsSuggestions.AnalyzeLocalTokens;
var
   codeLine : UnicodeString;
   p, p2 : Integer;

   procedure MoveToTokenStart;
   begin
      if p>Length(codeLine)+1 then
         p:=Length(codeLine)+1;
      while p>1 do begin
         case codeLine[p-1] of
            '0'..'9', 'A'..'Z', 'a'..'z', '_', #127..#$FFFF : begin
               Dec(p);
            end;
         else
            Break;
         end;
      end;
   end;

var
   sl : TStringList;
   context : TContext;
begin
   FLocalContext:=FProg.ContextMap.FindContext(FSourcePos);
   context:=FLocalContext;
   while (context<>nil) and (context.ParentSym=nil) do
      context:=context.Parent;
   if context<>nil then
      FContextSymbol:=context.ParentSym;

   sl:=TStringList.Create;
   try
      sl.Text:=FSourceFile.Code;
      if Cardinal(FSourcePos.Line-1)<Cardinal(sl.Count) then
         codeLine:=sl[FSourcePos.Line-1]
      else Exit;
   finally
      sl.Free;
   end;

   p:=FSourcePos.Col;
   MoveToTokenStart;

   FPartialToken:=Copy(codeLine, p, FSourcePos.Col-p);

   if (p>1) and (codeLine[p-1]='.') then begin
      Dec(p, 2);
      MoveToTokenStart;
      FPreviousSymbol:=FProg.SymbolDictionary.FindSymbolAtPosition(p, FSourcePos.Line, FSourceFile.Name);
      if FPreviousSymbol is TAliasSymbol then
         FPreviousSymbol:=TAliasSymbol(FPreviousSymbol).UnAliasedType;
   end;

   Dec(p);
   while (p>1) do begin
      case codeLine[p] of
         ' ', #13, #10, #9 : Dec(p);
      else
         Break;
      end;
   end;

   p2:=p;
   MoveToTokenStart;
   FPreviousTokenString:=LowerCase(Copy(codeLine, p, p2-p+1));
   FPreviousToken:=TTokenBuffer.StringToTokenType(FPreviousTokenString);
end;

// AddToList
//
procedure TdwsSuggestions.AddToList(aList : TSimpleSymbolList);
var
   i : Integer;
   tmp : TStringList;
   sym : TSymbol;
begin
   tmp:=TFastCompareTextList.Create;
   try
      tmp.CaseSensitive:=False;
      tmp.Capacity:=aList.Count;
      for i:=0 to aList.Count-1 do begin
         sym:=aList[i];
         if FPartialToken<>'' then begin
            if not StrIBeginsWith(sym.Name, FPartialToken) then continue;
         end;
         if FSymbolClassFilter<>nil then begin
            if not ((sym is FSymbolClassFilter) or (sym.Typ is FSymbolClassFilter)) then continue;
         end else if sym is TOpenArraySymbol then
            continue;
         if FListLookup.IndexOf(sym)<0 then begin
            FListLookup.Add(sym);
            if FNamesLookup.IndexOf(sym.Name)<0 then begin
               tmp.AddObject(sym.Name, sym);
               FNamesLookup.Add(sym.Name);
            end;
         end;
      end;
      tmp.Sort;
      for i:=0 to tmp.Count-1 do
         FList.Add(TSymbol(tmp.Objects[i]));
   finally
      tmp.Free;
   end;
end;

// IsContextSymbol
//
function TdwsSuggestions.IsContextSymbol(sym : TSymbol) : Boolean;
var
   context : TContext;
begin
   context:=FLocalContext;
   while context<>nil do begin
      if context.ParentSym=sym then
         Exit(True);
      context:=context.Parent;
   end;
   Result:=False;
end;

// AddReservedWords
//
procedure TdwsSuggestions.AddReservedWords;

   function IsAlpha(const s : String) : Boolean;
   var
      i : Integer;
   begin
      for i:=1 to Length(s) do begin
         case s[i] of
            'a'..'z', 'A'..'Z' : ;
         else
            exit(False);
         end;
      end;
      Result:=True;
   end;

var
   t : TTokenType;
   rws : TReservedWordSymbol;
   list : TSimpleSymbolList;
begin
   list:=TSimpleSymbolList.Create;
   try
      for t in cPascalReservedNames do begin
         if IsAlpha(cTokenStrings[t]) then begin
            rws:=TReservedWordSymbol.Create(LowerCase(cTokenStrings[t]), nil);
            list.Add(rws);
            FCleanupList.Add(rws);
         end;
      end;
      AddToList(list);
   finally
      list.Free;
   end;
end;

// AddImmediateSuggestions
//
procedure TdwsSuggestions.AddImmediateSuggestions;
var
   list : TSimpleSymbolList;
begin
   list:=TSimpleSymbolList.Create;
   try
      if FPreviousSymbol<>nil then begin

         if FPreviousSymbol is TStructuredTypeMetaSymbol then begin

            // nothing

         end else if FPreviousSymbol is TStructuredTypeSymbol then begin

            if FPreviousToken in [ttPROCEDURE, ttFUNCTION, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR]  then begin
               FSymbolClassFilter:=TFuncSymbol;
               list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol), FContextSymbol);
            end else list.AddMetaMembers(TStructuredTypeSymbol(FPreviousSymbol), FContextSymbol);

         end else if     (FPreviousSymbol is TMethodSymbol)
                     and (TMethodSymbol(FPreviousSymbol).Kind=fkConstructor) then begin

            list.AddMembers(TMethodSymbol(FPreviousSymbol).StructSymbol, FContextSymbol);

         end else if FPreviousSymbol.Typ is TStructuredTypeSymbol then begin

            if     (FContextSymbol is TMethodSymbol)
               and (TMethodSymbol(FContextSymbol).StructSymbol=FPreviousSymbol.Typ) then
               list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol.Typ), FContextSymbol, AddToList)
            else list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol.Typ), FContextSymbol);

         end else if FPreviousSymbol.Typ is TStructuredTypeMetaSymbol then begin

            list.AddMetaMembers(TStructuredTypeMetaSymbol(FPreviousSymbol.Typ).StructSymbol, FContextSymbol);

         end else if FPreviousSymbol is TUnitSymbol then begin

            list.AddSymbolTable(TUnitSymbol(FPreviousSymbol).Main.Table);

         end else if FPreviousSymbol is TArraySymbol then begin

            if FPreviousSymbol is TDynamicArraySymbol then begin
               // todo
            end else begin
               // todo
            end;

         end else FPreviousSymbol:=nil;

      end;
      if FPreviousToken=ttNEW then
         FSymbolClassFilter:=TClassSymbol;

      AddToList(list);
   finally
      list.Free;
   end;
end;

// AddContextSuggestions
//
procedure TdwsSuggestions.AddContextSuggestions;
var
   list : TSimpleSymbolList;
   context : TContext;
   funcSym : TFuncSymbol;
   methSym : TMethodSymbol;
begin
   if FPreviousSymbol<>nil then Exit;

   context:=FLocalContext;

   list:=TSimpleSymbolList.Create;
   try
      while context<>nil do begin
         list.AddSymbolTable(context.LocalTable);

         if context.ParentSym is TFuncSymbol then begin
            funcSym:=TFuncSymbol(Context.ParentSym);
            list.AddDirectSymbolTable(funcSym.Params);
            list.AddDirectSymbolTable(funcSym.InternalParams);

            // if a method, add the object's members to the list
            if funcSym is TMethodSymbol then begin
               methSym:=TMethodSymbol(funcSym);
               if methSym.IsClassMethod then
                  list.AddMetaMembers(methSym.StructSymbol, methSym.StructSymbol, AddToList)
               else list.AddMembers(methSym.StructSymbol, methSym.StructSymbol, AddToList);
            end;
        end;

         context:=context.Parent;
      end;
      AddToList(list);
   finally
      list.Free;
   end;
end;

// AddUnitSuggestions
//
procedure TdwsSuggestions.AddUnitSuggestions;
var
   table : TSymbolTable;
   main : TUnitMainSymbol;
   list : TSimpleSymbolList;
begin
   if FPreviousSymbol<>nil then Exit;

   if FSourcePos.IsMainModule then
      table:=FProg.Table
   else begin
      main:=FProg.UnitMains.Find(FSourceFile.Name);
      if main=nil then Exit;
      table:=main.Table;
   end;

   list:=TSimpleSymbolList.Create;
   try
      list.AddSymbolTable(table);
      AddToList(list);
   finally
      list.Free;
   end;
end;

// AddGlobalSuggestions
//
procedure TdwsSuggestions.AddGlobalSuggestions;
var
   list : TSimpleSymbolList;
   unitMains : TUnitMainSymbols;
   table : TSymbolTable;
   i : Integer;
begin
   if FPreviousSymbol<>nil then Exit;

   unitMains:=FProg.UnitMains;

   list:=TSimpleSymbolList.Create;
   try
      for i:=0 to unitMains.Count-1 do begin
         table:=unitMains[i].Table;
         list.AddSymbolTable(table);
         if table is TLinkedSymbolTable then
            list.AddSymbolTable(TLinkedSymbolTable(table).ParentSymbolTable);
      end;
      AddToList(list);
   finally
      list.Free;
   end;
end;

// GetCode
//
function TdwsSuggestions.GetCode(i : Integer) : UnicodeString;
begin
   Result:=FList[i].Name;
end;

// GetCategory
//
function TdwsSuggestions.GetCategory(i : Integer) : TdwsSuggestionCategory;
var
   symbol : TSymbol;
   symbolClass : TClass;
begin
   Result:=scUnknown;
   symbol:=GetSymbols(i);
   symbolClass:=symbol.ClassType;

   if symbolClass.InheritsFrom(TTypeSymbol) then begin

      Result:=scType;
      if symbolClass.InheritsFrom(TStructuredTypeSymbol) then begin
         if symbolClass.InheritsFrom(TClassSymbol) then
            Result:=scClass
         else if symbolClass.InheritsFrom(TRecordSymbol) then
            Result:=scInterface
         else if symbolClass.InheritsFrom(TInterfaceSymbol) then
            Result:=scInterface
      end else if symbolClass.InheritsFrom(TMethodSymbol) then begin
         case TMethodSymbol(symbol).Kind of
            fkConstructor : Result:=scConstructor;
            fkDestructor : Result:=scDestructor;
            fkMethod : Result:=scMethod;
            fkProcedure : Result:=scProcedure;
            fkFunction : Result:=scFunction;
         end;
      end else if symbolClass.InheritsFrom(TFuncSymbol) then begin
         if TFuncSymbol(symbol).IsType then
            Result:=scDelegate
         else if symbol.Typ=nil then
            Result:=scProcedure
         else Result:=scFunction;
      end else if symbolClass.InheritsFrom(TEnumerationSymbol) then
         Result:=scEnum
      else if symbolClass.InheritsFrom(TUnitSymbol) then
         Result:=scUnit;

   end else if symbolClass.InheritsFrom(TValueSymbol) then begin

      if symbolClass.InheritsFrom(TParamSymbol) then
         Result:=scParameter
      else if symbolClass.InheritsFrom(TPropertySymbol) then
         Result:=scProperty
      else if symbolClass.InheritsFrom(TConstSymbol) then begin
         if symbolClass.InheritsFrom(TElementSymbol) then
            Result:=scElement
         else Result:=scConst
      end else Result:=scVariable;

   end;
end;

// GetCaption
//
function TdwsSuggestions.GetCaption(i : Integer) : UnicodeString;

   function SafeSymbolName(symbol : TSymbol) : UnicodeString;
   begin
      if symbol<>nil then
         Result:=symbol.Name
      else Result:='???';
   end;

var
   symbol : TSymbol;
   funcSym : TFuncSymbol;
   valueSym : TValueSymbol;
   enumSym : TEnumerationSymbol;
   propSymbol : TPropertySymbol;
begin
   symbol:=FList[i];
   if symbol is TFuncSymbol then begin

      funcSym:=TFuncSymbol(symbol);
      Result:=funcSym.Name+' '+funcSym.ParamsDescription;
      if funcSym.Result<>nil then
         Result:=Result+' : '+SafeSymbolName(funcSym.Result.Typ);

   end else if symbol is TEnumerationSymbol then begin

      enumSym:=TEnumerationSymbol(symbol);
      if enumSym.Elements.Count>0 then
         Result:=enumSym.Name+' : '+SafeSymbolName(enumSym.Elements[0])+'..'+SafeSymbolName(enumSym.Elements[enumSym.Elements.Count-1])
      else Result:=enumSym.Name;

   end else if symbol is TPropertySymbol then begin

      propSymbol:=TPropertySymbol(symbol);
      if propSymbol.ArrayIndices.Count>0 then
         Result:=propSymbol.Name+' '+propSymbol.GetArrayIndicesDescription+' : '+SafeSymbolName(propSymbol.Typ)
      else Result:=propSymbol.Name+' : '+SafeSymbolName(propSymbol.Typ);

   end else if symbol is TValueSymbol then begin

      valueSym:=TValueSymbol(symbol);
      Result:=valueSym.Name+' : '+SafeSymbolName(valueSym.Typ);

   end else Result:=symbol.Name;
end;

// GetSymbols
//
function TdwsSuggestions.GetSymbols(i : Integer) : TSymbol;
begin
   Result:=FList[i];
end;

// Count
//
function TdwsSuggestions.Count : Integer;
begin
   Result:=FList.Count;
end;

// PartialToken
//
function TdwsSuggestions.PartialToken : UnicodeString;
begin
   Result:=FPartialToken;
end;

// ------------------
// ------------------ TSimpleSymbolList ------------------
// ------------------

// ScopeStruct
//
function TSimpleSymbolList.ScopeStruct(symbol : TSymbol) : TStructuredTypeSymbol;
begin
   if symbol is TStructuredTypeSymbol then
      Result:=TStructuredTypeSymbol(symbol)
   else if symbol is TMethodSymbol then
      Result:=TMethodSymbol(symbol).StructSymbol
   else if symbol is TStructuredTypeMetaSymbol then
      Result:=(TStructuredTypeMetaSymbol(symbol).Typ as TStructuredTypeSymbol)
   else Result:=nil;
end;

// ScopeVisiblity
//
function TSimpleSymbolList.ScopeVisiblity(scope, struct : TStructuredTypeSymbol) : TdwsVisibility;
begin
   if scope=struct then
      Result:=cvPrivate
   else if (scope<>nil) and scope.IsOfType(struct) then
      Result:=cvProtected
   else Result:=cvPublic;
end;

// AddSymbolTable
//
procedure TSimpleSymbolList.AddSymbolTable(table : TSymbolTable);
var
   sym : TSymbol;
begin
   if table is TLinkedSymbolTable then
      table:=TLinkedSymbolTable(table).ParentSymbolTable;
   for sym in table do begin
      if sym is TUnitSymbol then continue;
      Add(sym);
      if sym is TEnumerationSymbol then
         AddDirectSymbolTable(TEnumerationSymbol(sym).Elements);
   end;
end;

// AddDirectSymbolTable
//
procedure TSimpleSymbolList.AddDirectSymbolTable(table : TSymbolTable);
var
   sym : TSymbol;
begin
   for sym in table do
      Add(sym);
end;

// AddMembers
//
procedure TSimpleSymbolList.AddMembers(struc : TStructuredTypeSymbol; from : TSymbol;
                                       const addToList : TProcAddToList = nil);
var
   sym : TSymbol;
   scope : TStructuredTypeSymbol;
   visibility : TdwsVisibility;
   first : Boolean;
begin
   scope:=ScopeStruct(from);
   visibility:=ScopeVisiblity(scope, struc);
   first:=True;

   repeat
      for sym in struc.Members do begin
         if sym is TClassOperatorSymbol then continue;
         if not sym.IsVisibleFor(visibility) then continue;
         Add(sym);
      end;
      if first then begin
         first:=False;
         if Assigned(addToList) then begin
            addToList(Self);
            Clear;
         end;
      end;
      if visibility=cvPrivate then
         visibility:=cvProtected;
      struc:=struc.Parent;
   until struc=nil;
end;

// AddMetaMembers
//
procedure TSimpleSymbolList.AddMetaMembers(struc : TStructuredTypeSymbol; from : TSymbol;
                                           const addToList : TProcAddToList = nil);
var
   sym : TSymbol;
   methSym : TMethodSymbol;
   scope : TStructuredTypeSymbol;
   visibility : TdwsVisibility;
   first : Boolean;
begin
   scope:=ScopeStruct(from);
   visibility:=ScopeVisiblity(scope, struc);
   first:=True;

   repeat
      for sym in struc.Members do begin
         if not sym.IsVisibleFor(visibility) then continue;
         if sym is TMethodSymbol then begin
            methSym:=TMethodSymbol(sym);
            if methSym.IsClassMethod or (methSym.Kind=fkConstructor) then
               Add(Sym);
         end else if sym is TClassConstSymbol then begin
            Add(sym);
         end;
      end;
      if first then begin
         first:=False;
         if Assigned(addToList) then begin
            addToList(Self);
            Clear;
         end;
      end;
      if visibility=cvPrivate then
         visibility:=cvProtected;
      struc:=struc.Parent;
   until struc=nil;
end;

end.
