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

uses Classes, SysUtils, dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsTokenizer;

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
      function GetCode(i : Integer) : String;
      function GetCategory(i : Integer) : TdwsSuggestionCategory;
      function GetCaption(i : Integer) : String;
      function GetSymbols(i : Integer) : TSymbol;

      property Code[i : Integer] : String read GetCode;
      property Category[i : Integer] : TdwsSuggestionCategory read GetCategory;
      property Caption[i : Integer] : String read GetCaption;
      property Symbols[i : Integer] : TSymbol read GetSymbols;
      function Count : Integer;

      function PartialToken : String;
   end;

   TSimpleSymbolList = class(TSimpleList<TSymbol>)
      protected
         function ScopeStruct(symbol : TSymbol) : TStructuredTypeSymbol;
         // min visibility of struct members when seen from scope
         function ScopeVisiblity(scope, struct : TStructuredTypeSymbol) : TdwsVisibility;

      public
         procedure AddSymbolTable(table : TSymbolTable);
         procedure AddDirectSymbolTable(table : TSymbolTable);

         procedure AddMembers(struc : TStructuredTypeSymbol; from : TSymbol);
         procedure AddMetaMembers(struc : TStructuredTypeSymbol; from : TSymbol); overload;
         procedure AddMetaMembers(struc : TStructuredTypeMetaSymbol; from : TSymbol); overload;
   end;

   TdwsSuggestions = class (TInterfacedObject, IdwsSuggestions)
      private
         FProg : IdwsProgram;
         FSourcePos : TScriptPos;
         FSourceFile : TSourceFile;
         FList : TSimpleSymbolList;
         FListLookup : TObjectsLookup;
         FPartialToken : String;
         FPreviousSymbol : TSymbol;
         FPreviousTokenString : String;
         FPreviousToken : TTokenType;
         FLocalContext : TContext;
         FContextSymbol : TSymbol;
         FSymbolClassFilter : TSymbolClass;

      protected
         function GetCode(i : Integer) : String;
         function GetCategory(i : Integer) : TdwsSuggestionCategory;
         function GetCaption(i : Integer) : String;
         function GetSymbols(i : Integer) : TSymbol;
         function Count : Integer;

         function PartialToken : String;

         procedure AnalyzeLocalTokens;

         procedure AddToList(aList : TSimpleSymbolList);
         function IsContextSymbol(sym : TSymbol) : Boolean;

         procedure AddImmediateSuggestions;
         procedure AddContextSuggestions;
         procedure AddUnitSuggestions;
         procedure AddGlobalSuggestions;

      public
         constructor Create(const prog : IdwsProgram; const sourcePos : TScriptPos);
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
constructor TdwsSuggestions.Create(const prog : IdwsProgram; const sourcePos : TScriptPos);
begin
   FProg:=prog;
   FSourcePos:=sourcePos;
   FSourceFile:=sourcePos.SourceFile;
   FList:=TSimpleSymbolList.Create;
   FListLookup:=TObjectsLookup.Create;

   AnalyzeLocalTokens;

   AddImmediateSuggestions;
   AddContextSuggestions;
   AddUnitSuggestions;
   AddGlobalSuggestions;

   FListLookup.Clear;
end;

// Destroy
//
destructor TdwsSuggestions.Destroy;
begin
   FListLookup.Free;
   FList.Free;
   inherited;
end;

// AnalyzeLocalTokens
//
procedure TdwsSuggestions.AnalyzeLocalTokens;
var
   codeLine : String;
   p, p2 : Integer;

   procedure MoveToTokenStart;
   begin
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
         end;
         if FListLookup.IndexOf(sym)<0 then begin
            tmp.AddObject(sym.Name, sym);
            FListLookup.Add(sym);
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

// AddImmediateSuggestions
//
procedure TdwsSuggestions.AddImmediateSuggestions;
var
   list : TSimpleSymbolList;
begin

   list:=TSimpleSymbolList.Create;
   try
      if FPreviousSymbol<>nil then begin

         if FPreviousSymbol.Typ is TStructuredTypeSymbol then begin
            list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol.Typ), FContextSymbol);
         end else if FPreviousSymbol is TStructuredTypeSymbol then begin
            if FPreviousToken in [ttPROCEDURE, ttFUNCTION, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR]  then begin
               FSymbolClassFilter:=TFuncSymbol;
               list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol), FContextSymbol);
            end else list.AddMetaMembers(TStructuredTypeSymbol(FPreviousSymbol), FContextSymbol);
         end else if FPreviousSymbol is TStructuredTypeMetaSymbol then begin
            list.AddMetaMembers(TStructuredTypeMetaSymbol(FPreviousSymbol), FContextSymbol);
         end else if FPreviousSymbol is TArraySymbol then begin
            if FPreviousSymbol is TDynamicArraySymbol then begin
               // todo
            end else begin
               // todo
            end;
         end;

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
                  list.AddMetaMembers(methSym.StructSymbol, methSym.StructSymbol)
               else list.AddMembers(methSym.StructSymbol, methSym.StructSymbol);
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
            list.AddSymbolTable(TLinkedSymbolTable(table).Parent);
      end;
      AddToList(list);
   finally
      list.Free;
   end;
end;

// GetCode
//
function TdwsSuggestions.GetCode(i : Integer) : String;
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
function TdwsSuggestions.GetCaption(i : Integer) : String;

   function SafeSymbolName(symbol : TSymbol) : String;
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
function TdwsSuggestions.PartialToken : String;
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
procedure TSimpleSymbolList.AddMembers(struc : TStructuredTypeSymbol; from : TSymbol);
var
   sym : TSymbol;
   scope : TStructuredTypeSymbol;
   visibility : TdwsVisibility;
begin
   scope:=ScopeStruct(from);
   visibility:=ScopeVisiblity(scope, struc);

   repeat
      for sym in struc.Members do begin
         if sym is TClassOperatorSymbol then continue;
         if not sym.IsVisibleFor(visibility) then continue;
         Add(sym);
      end;
      if visibility=cvPrivate then
         visibility:=cvProtected;
      struc:=struc.Parent;
   until struc=nil;
end;

// AddMetaMembers
//
procedure TSimpleSymbolList.AddMetaMembers(struc : TStructuredTypeSymbol; from : TSymbol);
var
   sym : TSymbol;
   methSym : TMethodSymbol;
   scope : TStructuredTypeSymbol;
   visibility : TdwsVisibility;
begin
   scope:=ScopeStruct(from);
   visibility:=ScopeVisiblity(scope, struc);

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
      if visibility=cvPrivate then
         visibility:=cvProtected;
      struc:=struc.Parent;
   until struc=nil;
end;

// AddMetaMembers
//
procedure TSimpleSymbolList.AddMetaMembers(struc : TStructuredTypeMetaSymbol; from : TSymbol);
begin
   AddMetaMembers(struc.Typ as TStructuredTypeSymbol, from);
end;

end.
