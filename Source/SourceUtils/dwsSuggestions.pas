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

uses
   Classes, SysUtils,
   dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsTokenizer, dwsScriptSource,
   dwsUnitSymbols, dwsPascalTokenizer, dwsCompiler, dwsContextMap, dwsCompilerContext,
   dwsSymbolDictionary;

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
                             scField, scVariable, scConst,
                             scReservedWord,
                             scSpecialFunction);

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
      function PreviousSymbol : TSymbol;
   end;

   // Pseudo-symbols for suggestion purposes
   TReservedWordSymbol = class(TSymbol);
   TSpecialFunctionSymbol = class(TSymbol);

   TSuggestionFuncSymbol = class (TFuncSymbol)
   end;
   TSuggestionForcedParenthesisFuncSymbol = class (TSuggestionFuncSymbol)
   end;

   TSimpleSymbolList = class;

   TProcAddToList = procedure (aList : TSimpleSymbolList) of object;

   TAddMembersOption = (amoMeta, amoInstance, amoTypedOnly);
   TAddMembersOptions = set of TAddMembersOption;

   TSimpleSymbolList = class(TSimpleList<TSymbol>)
      protected
         function ScopeStruct(symbol : TSymbol) : TCompositeTypeSymbol;
         // min visibility of struct members when seen from scope
         function ScopeVisiblity(scope, struct : TCompositeTypeSymbol) : TdwsVisibility;

      public
         procedure AddSymbolTable(table : TSymbolTable);
         procedure AddDirectSymbolTable(table : TSymbolTable);

         procedure AddEnumeration(enum : TEnumerationSymbol; const addToList : TProcAddToList = nil);
         procedure AddMembers(struc : TCompositeTypeSymbol; from : TSymbol;
                              options : TAddMembersOptions;
                              const addToList : TProcAddToList = nil;
                              restrictTo : TSymbolClass = nil);
         procedure AddNameSpace(unitSym : TUnitSymbol);

         procedure Remove(sym : TSymbol);
   end;

   TdwsSuggestionsOption = (
      soNoReservedWords,   // do not suggest reserved words
      soNoUnits,           // do not suggest units
      soUnifyOverloads     // for legacy behavior when overloads were not listed separately
      );
   TdwsSuggestionsOptions = set of TdwsSuggestionsOption;

   TNameSymbolHash = TSimpleNameObjectHash<TSymbol>;

   TdwsSuggestions = class (TInterfacedObject, IdwsSuggestions)
      private
         FProg : IdwsProgram;
         FSourcePos : TScriptPos;
         FSourceFile : TSourceFile;
         FList : TSimpleSymbolList;
         FCleanupList : TTightList;
         FListLookup : TObjectsLookup;
         FNamesLookup : TNameSymbolHash;
         FPartialToken : String;
         FFullToken : String;
         FPreviousSymbol : TSymbol;
         FPreviousSymbolIsMeta : Boolean;
         FPreviousTokenString : String;
         FPreviousToken : TTokenType;
         FAfterDot : Boolean;
         FLocalContext : TdwsSourceContext;
         FLocalTable : TSymbolTable;
         FContextSymbol : TSymbol;
         FSymbolClassFilter : TSymbolClass;
         FStaticArrayHelpers : TSymbolTable;
         FDynArrayHelpers : TSymbolTable;
         FAssocArrayHelpers : TSymbolTable;
         FEnumElementHelpers : TSymbolTable;
         FJSONVariantHelpers : TSymbolTable;
         FOptions: TdwsSuggestionsOptions;

      protected
         function GetCode(i : Integer) : String;
         function GetCategory(i : Integer) : TdwsSuggestionCategory;
         function GetCaption(i : Integer) : String;
         function GetSymbols(i : Integer) : TSymbol;
         function Count : Integer;

         function PartialToken : String;
         function PreviousSymbol : TSymbol;

         procedure AnalyzeLocalTokens;

         procedure AddToList(aList : TSimpleSymbolList);
         function IsContextSymbol(sym : TSymbol) : Boolean;

         function CreateHelper(const name : String; resultType : TTypeSymbol;
                               const args : array of const;
                               forceParenthesis : Boolean = False) : TFuncSymbol;
         procedure AddEnumerationElementHelpers(list : TSimpleSymbolList);
         procedure AddStaticArrayHelpers(list : TSimpleSymbolList);
         procedure AddDynamicArrayHelpers(dyn : TDynamicArraySymbol; list : TSimpleSymbolList);
         procedure AddAssociativeArrayHelpers(assoc : TAssociativeArraySymbol; list : TSimpleSymbolList);
         procedure AddJSONVariantHelpers(list : TSimpleSymbolList);
         procedure AddTypeHelpers(typ : TTypeSymbol; meta : Boolean; list : TSimpleSymbolList);
         procedure AddUnitSymbol(unitSym : TUnitSymbol; list : TSimpleSymbolList);

         procedure AddReservedWords;
         procedure AddSpecialFuncs;
         procedure AddImmediateSuggestions;
         procedure AddContextSuggestions;
         procedure AddUnitSuggestions;
         procedure AddGlobalSuggestions;

      public
         constructor Create(const prog : IdwsProgram; const sourcePos : TScriptPos;
                            const options : TdwsSuggestionsOptions = [];
                            customSuggestions : TSimpleSymbolList = nil);
         destructor Destroy; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   THelperFilter = class (TSimpleSymbolList)
      private
         List : TSimpleSymbolList;
         Meta : Boolean;
         function OnHelper(helper : THelperSymbol) : Boolean;
   end;

// OnHelper
//
function THelperFilter.OnHelper(helper : THelperSymbol) : Boolean;
var
   opt : TAddMembersOptions;
begin
   opt := [amoMeta];
   if not Meta then
      Include(opt, amoInstance);
   List.AddMembers(helper, nil, opt);
   Result:=False;
end;

// ------------------
// ------------------ TdwsSuggestions ------------------
// ------------------

// Create
//
constructor TdwsSuggestions.Create(const prog : IdwsProgram; const sourcePos : TScriptPos;
                                   const options : TdwsSuggestionsOptions = [];
                                   customSuggestions : TSimpleSymbolList = nil);
begin
   FProg:=prog;
   FSourcePos:=sourcePos;
   FSourceFile:=sourcePos.SourceFile;
   FOptions:=options;
   FList:=TSimpleSymbolList.Create;
   FListLookup:=TObjectsLookup.Create;
   FNamesLookup:=TNameSymbolHash.Create;

   AnalyzeLocalTokens;

   AddImmediateSuggestions;
   if customSuggestions<>nil then
      AddToList(customSuggestions);
   AddContextSuggestions;
   if not (soNoUnits in options) then
      AddUnitSuggestions;
   AddGlobalSuggestions;

   if not FAfterDot then begin
      if not (soNoReservedWords in options) then
         AddReservedWords;
      AddSpecialFuncs;
   end;

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
   FStaticArrayHelpers.Free;
   FDynArrayHelpers.Free;
   FAssocArrayHelpers.Free;
   FEnumElementHelpers.Free;
   FJSONVariantHelpers.Free;
   inherited;
end;

// AnalyzeLocalTokens
//
procedure TdwsSuggestions.AnalyzeLocalTokens;
var
   codeLine : String;
   p, p2 : Integer;
   sl : TStringList;
   lineNumber : Integer;

   function NeedCodeLine : Boolean;
   begin
      if lineNumber>0 then begin
         Dec(lineNumber);
         codeLine:=sl[lineNumber];
         p:=Length(codeLine);
         Result:=True;
      end else Result:=False;
   end;

   function SkipBrackets : Boolean;
   var
      n : Integer;
      inString : WideChar;
   begin
      while p<=0 do
         if not NeedCodeLine then
            Exit(False);
      n:=0;
      inString:=#0;
      Result:=(codeLine[p]=']') or (codeLine[p]=')');
      while p>=1 do begin
         if inString=#0 then begin
            case codeLine[p] of
               ')', ']' : Inc(n);
               '(', '[' : Dec(n);
               '''' : inString:='''';
               '"' : inString:='"';
            end;
            if n=0 then Exit;
         end else begin
            if codeLine[p]=inString then
               inString:=#0;
         end;
         Dec(p);
         while p<=0 do
            if not NeedCodeLine then
               Exit(False);
      end;
      Result:=Result and (p>=1);
   end;

   procedure MoveToTokenStart;
   begin
      if p>Length(codeLine)+1 then
         p:=Length(codeLine)+1;
      while p>1 do begin
         case codeLine[p-1] of
            {$ifdef FPC}
            '0'..'9', 'A'..'Z', 'a'..'z', '_', #127..#$FF : begin
            {$else}
            '0'..'9', 'A'..'Z', 'a'..'z', '_', #127..#$FFFF : begin
            {$endif}
               Dec(p);
            end;
         else
            Break;
         end;
      end;
   end;

var
   context : TdwsSourceContext;
   arrayItem : Boolean;
begin
   FLocalContext:=FProg.SourceContextMap.FindContext(FSourcePos);

   // find context symbol
   context:=FLocalContext;
   while (context<>nil) and (context.ParentSym=nil) do
      context:=context.Parent;
   if context<>nil then
      FContextSymbol:=context.ParentSym;

   // find context table
   context:=FLocalContext;
   while (context<>nil) and (context.LocalTable=nil) do
      context:=context.Parent;
   if context<>nil then
      FLocalTable:=context.LocalTable
   else FLocalTable:=FProg.Table;

   sl:=TStringList.Create;
   try
      if FSourceFile<>nil then
         sl.Text:=FSourceFile.Code;
      if Cardinal(FSourcePos.Line-1)>=Cardinal(sl.Count) then begin
         lineNumber:=0;
         codeLine:='';
      end else begin
         lineNumber:=FSourcePos.Line-1;
         codeLine:=sl[lineNumber]
      end;

      p:=FSourcePos.Col;
      MoveToTokenStart;

      FPartialToken:=Copy(codeLine, p, FSourcePos.Col-p);

      if (p>1) and (codeLine[p-1]='.') then begin
         FAfterDot:=True;
         Dec(p, 2);
         arrayItem:=SkipBrackets;
         MoveToTokenStart;
         FFullToken := Copy(codeLine, p, FSourcePos.Col-p);
         FPreviousSymbol:=FProg.SymbolDictionary.FindSymbolAtPosition(p, lineNumber+1, FSourceFile.Name);

         if FPreviousSymbol<>nil then begin
            if FPreviousSymbol is TAliasSymbol then
               FPreviousSymbol:=TAliasSymbol(FPreviousSymbol).UnAliasedType;
            if arrayItem then begin
               FPreviousSymbolIsMeta:=False;
               if FPreviousSymbol.Typ is TArraySymbol then
                  FPreviousSymbol:=TArraySymbol(FPreviousSymbol.Typ).Typ
               else if FPreviousSymbol is TPropertySymbol then
                  FPreviousSymbol:=TArraySymbol(FPreviousSymbol).Typ;
            end else begin
               FPreviousSymbolIsMeta:=FPreviousSymbol.IsType;
            end;
         end;
      end else FAfterDot:=False;
   finally
      sl.Free;
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
   nameStr : String;
begin
   if aList.Count = 0 then Exit;
   tmp:=TFastCompareTextList.Create;
   try
      tmp.CaseSensitive:=False;
      tmp.Capacity:=aList.Count;
      for i:=0 to aList.Count-1 do begin
         sym:=aList[i];
         if sym.Name='' then continue;
         if FPartialToken<>'' then begin
            if not StrIBeginsWith(sym.Name, FPartialToken) then continue;
         end;
         if FSymbolClassFilter<>nil then begin
            if not ((sym is FSymbolClassFilter) or (sym.Typ is FSymbolClassFilter)) then continue;
         end else if sym is TOpenArraySymbol then
            continue;
         if StrContains(sym.Name, ' ') then continue;

         if (FLocalContext<>nil) and (FLocalContext.Token=ttUSES) then begin
            if not (sym is TUnitSymbol) then continue;
         end else begin
            if (soNoUnits in FOptions) and (sym is TUnitSymbol) then continue;
         end;

         if FListLookup.IndexOf(sym)<0 then begin
            FListLookup.Add(sym);
            if soUnifyOverloads in FOptions then
               nameStr := sym.Name
            else nameStr := sym.Description;
            if FNamesLookup.Objects[nameStr]=nil then begin
               tmp.AddObject(sym.Name, sym);
               FNamesLookup.AddObject(nameStr, sym);
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
   context : TdwsSourceContext;
begin
   context:=FLocalContext;
   while context<>nil do begin
      if context.ParentSym=sym then
         Exit(True);
      context:=context.Parent;
   end;
   Result:=False;
end;

// CreateHelper
//
function TdwsSuggestions.CreateHelper(const name : String; resultType : TTypeSymbol;
                                      const args : array of const;
                                      forceParenthesis : Boolean = False) : TFuncSymbol;
var
   i : Integer;
   p : TParamSymbol;
   funcKind : TFuncKind;
begin
   if resultType = nil then
      funcKind := fkProcedure
   else funcKind := fkFunction;
   if forceParenthesis then
      Result := TSuggestionForcedParenthesisFuncSymbol.Create(name, funcKind, 0)
   else Result:=TSuggestionFuncSymbol.Create(name, funcKind, 0);
   if resultType <> nil then
      Result.Typ := resultType;

   for i:=0 to (Length(args) div 2)-1 do begin
      Assert(args[i*2].VType=vtUnicodeString);
      Assert(args[i*2+1].VType=vtObject);
      p:=TParamSymbol.Create(String(args[i*2].VUnicodeString), TObject(args[i*2+1].VObject) as TTypeSymbol);
      Result.Params.AddSymbol(p);
   end;
end;

// AddEnumerationElementHelpers
//
procedure TdwsSuggestions.AddEnumerationElementHelpers(list : TSimpleSymbolList);
var
   p : TdwsCompilerContext;
begin
   if FEnumElementHelpers=nil then begin
      FEnumElementHelpers:=TSymbolTable.Create;
      p:=FProg.ProgramObject.CompilerContext;
      FEnumElementHelpers.AddSymbol(CreateHelper('Name', p.TypString, []));
      FEnumElementHelpers.AddSymbol(CreateHelper('Value', p.TypInteger, []));
   end;

   list.AddSymbolTable(FEnumElementHelpers);
end;

// AddStaticArrayHelpers
//
procedure TdwsSuggestions.AddStaticArrayHelpers(list : TSimpleSymbolList);
var
   p : TdwsCompilerContext;
begin
   if FStaticArrayHelpers=nil then begin
      FStaticArrayHelpers:=TSymbolTable.Create;
      p:=FProg.ProgramObject.CompilerContext;
      FStaticArrayHelpers.AddSymbol(CreateHelper('Low', p.TypInteger, []));
      FStaticArrayHelpers.AddSymbol(CreateHelper('High', p.TypInteger, []));
      FStaticArrayHelpers.AddSymbol(CreateHelper('Length', p.TypInteger, []));
   end;

   list.AddSymbolTable(FStaticArrayHelpers);
end;

// AddDynamicArrayHelpers
//
procedure TdwsSuggestions.AddDynamicArrayHelpers(dyn : TDynamicArraySymbol; list : TSimpleSymbolList);
var
   p : TdwsCompilerContext;
begin
   if FDynArrayHelpers=nil then begin
      p:=FProg.ProgramObject.CompilerContext;
      FDynArrayHelpers:=TSystemSymbolTable.Create;
      FDynArrayHelpers.AddSymbol(CreateHelper('Count', p.TypInteger, []));
      FDynArrayHelpers.AddSymbol(CreateHelper('Add', nil, ['item', dyn.Typ]));
      FDynArrayHelpers.AddSymbol(CreateHelper('Push', nil, ['item', dyn.Typ]));
      FDynArrayHelpers.AddSymbol(CreateHelper('Pop', dyn.Typ, []));
      FDynArrayHelpers.AddSymbol(CreateHelper('Peek', dyn.Typ, []));
      FDynArrayHelpers.AddSymbol(CreateHelper('Delete', nil, ['index', dyn.Typ, 'count', p.TypInteger]));
      FDynArrayHelpers.AddSymbol(CreateHelper('IndexOf', p.TypInteger, ['item', dyn.Typ, 'fromIndex', p.TypInteger]));
      FDynArrayHelpers.AddSymbol(CreateHelper('Insert', nil, ['index', p.TypInteger, 'item', dyn.Typ]));
      FDynArrayHelpers.AddSymbol(CreateHelper('SetLength', nil, ['newLength', p.TypInteger]));
      FDynArrayHelpers.AddSymbol(CreateHelper('Clear', nil, []));
      FDynArrayHelpers.AddSymbol(CreateHelper('Remove', nil, []));
      FDynArrayHelpers.AddSymbol(CreateHelper('Reverse', nil, []));
      FDynArrayHelpers.AddSymbol(CreateHelper('Swap', nil, ['index1', p.TypInteger, 'index2', p.TypInteger]));
      FDynArrayHelpers.AddSymbol(CreateHelper('Copy', nil, ['startIndex', p.TypInteger, 'count', p.TypInteger]));
      FDynArrayHelpers.AddSymbol(CreateHelper('Sort', nil, ['comparer', dyn.SortFunctionType(p.TypInteger)]));
      FDynArrayHelpers.AddSymbol(CreateHelper('Map', nil, ['func', dyn.MapFunctionType(p.TypInteger)]));
   end;

   list.AddSymbolTable(FDynArrayHelpers);
end;

// AddAssociativeArrayHelpers
//
procedure TdwsSuggestions.AddAssociativeArrayHelpers(assoc : TAssociativeArraySymbol; list : TSimpleSymbolList);
var
   p : TdwsCompilerContext;
begin
   if FAssocArrayHelpers=nil then begin
      p:=FProg.ProgramObject.CompilerContext;
      FAssocArrayHelpers:=TSystemSymbolTable.Create;
      FAssocArrayHelpers.AddSymbol(CreateHelper('Count', p.TypInteger, []));
      FAssocArrayHelpers.AddSymbol(CreateHelper('Length', p.TypInteger, []));
      FAssocArrayHelpers.AddSymbol(CreateHelper('Clear', nil, []));
      FAssocArrayHelpers.AddSymbol(CreateHelper('Delete', nil, ['key', assoc.KeyType]));
      FAssocArrayHelpers.AddSymbol(CreateHelper('Keys', assoc.KeysArrayType(p.TypInteger), []));
   end;

   list.AddSymbolTable(FAssocArrayHelpers);
end;

// AddJSONVariantHelpers
//
procedure TdwsSuggestions.AddJSONVariantHelpers(list : TSimpleSymbolList);
var
   p : TdwsCompilerContext;
begin
   if FJSONVariantHelpers=nil then begin
      p:=FProg.ProgramObject.CompilerContext;
      FJSONVariantHelpers:=TSystemSymbolTable.Create;
      FJSONVariantHelpers.AddSymbol(CreateHelper('Add', p.TypInteger, ['item', p.TypAnyType]));
      FJSONVariantHelpers.AddSymbol(CreateHelper('Push', p.TypInteger, ['item', p.TypAnyType]));
      FJSONVariantHelpers.AddSymbol(CreateHelper('Length', p.TypInteger, [], True));
      FJSONVariantHelpers.AddSymbol(CreateHelper('Low', p.TypInteger, [], True));
      FJSONVariantHelpers.AddSymbol(CreateHelper('High', p.TypInteger, [], True));
      FJSONVariantHelpers.AddSymbol(CreateHelper('Clone', p.TypVariant, [], True));
      FJSONVariantHelpers.AddSymbol(CreateHelper('ToString', p.TypString, [], True));
      FJSONVariantHelpers.AddSymbol(CreateHelper('Extend', p.TypVariant, ['obj', p.TypVariant]));
      FJSONVariantHelpers.AddSymbol(CreateHelper('Delete', nil, ['item', p.TypAnyType]));
      FJSONVariantHelpers.AddSymbol(CreateHelper('Swap', nil, ['index1', p.TypInteger, 'index2', p.TypInteger]));
      FJSONVariantHelpers.AddSymbol(CreateHelper('TypeName', p.TypString, [], True));
      FJSONVariantHelpers.AddSymbol(CreateHelper('ElementName', p.TypString, ['index', p.TypInteger]));
   end;

   list.AddSymbolTable(FJSONVariantHelpers);
end;

// AddTypeHelpers
//
procedure TdwsSuggestions.AddTypeHelpers(typ : TTypeSymbol; meta : Boolean; list : TSimpleSymbolList);
var
   filter : THelperFilter;
begin
   if FLocalTable=nil then Exit;

   filter:=THelperFilter.Create;
   try
      filter.List:=list;
      filter.Meta:=meta;
      FLocalTable.EnumerateHelpers(typ, filter.OnHelper);
   finally
      filter.Free;
   end;
end;

// AddUnitSymbol
//
procedure TdwsSuggestions.AddUnitSymbol(unitSym : TUnitSymbol; list : TSimpleSymbolList);
begin
   if unitSym.Main<>nil then
      list.AddSymbolTable(unitSym.Table);
   if unitSym.HasNameSpace then
      list.AddNameSpace(unitSym);
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
            case t of
               ttTRUE : rws:=TReservedWordSymbol.Create('True', nil);
               ttFALSE : rws:=TReservedWordSymbol.Create('False', nil);
            else
               rws:=TReservedWordSymbol.Create(LowerCase(cTokenStrings[t]), nil);
            end;
            list.Add(rws);
            FCleanupList.Add(rws);
         end;
      end;
      AddToList(list);
   finally
      list.Free;
   end;
end;

// AddSpecialFuncs
//
procedure TdwsSuggestions.AddSpecialFuncs;
var
   skk : TSpecialKeywordKind;
   sfs : TSpecialFunctionSymbol;
   list : TSimpleSymbolList;
begin
   list:=TSimpleSymbolList.Create;
   try
      for skk:=Succ(skNone) to High(TSpecialKeywordKind) do begin
         sfs:=TSpecialFunctionSymbol.Create(cSpecialKeywords[skk], nil);
         list.Add(sfs);
         FCleanupList.Add(sfs);
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

         if FPreviousSymbolIsMeta then begin
            AddTypeHelpers(FPreviousSymbol as TTypeSymbol, True, list);
         end else if (FPreviousSymbol.Typ<>nil) and FPreviousSymbol.Typ.IsType then begin
            AddTypeHelpers(FPreviousSymbol.Typ, False, list);
         end else if FPreviousSymbol is TTypeSymbol then
            AddTypeHelpers(TTypeSymbol(FPreviousSymbol), False, list);

         if FPreviousSymbol is TStructuredTypeMetaSymbol then begin

            // nothing

         end else if FPreviousSymbol is TStructuredTypeSymbol then begin

            if not FPreviousSymbolIsMeta then
               list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol), FContextSymbol, [amoInstance, amoMeta])
            else begin
               if FPreviousToken in [ttPROCEDURE, ttFUNCTION, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR]  then begin
                  FSymbolClassFilter:=TFuncSymbol;
                  list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol), FContextSymbol, [amoInstance, amoMeta]);
               end else list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol), FContextSymbol, [amoMeta]);
            end;

         end else if     (FPreviousSymbol is TMethodSymbol)
                     and (TMethodSymbol(FPreviousSymbol).Kind=fkConstructor) then begin

            list.AddMembers(TMethodSymbol(FPreviousSymbol).StructSymbol, FContextSymbol, [amoInstance]);

         end else if FPreviousSymbol.Typ is TStructuredTypeSymbol then begin

            if     (FContextSymbol is TMethodSymbol)
               and (TMethodSymbol(FContextSymbol).StructSymbol=FPreviousSymbol.Typ) then
               list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol.Typ), FContextSymbol, [amoInstance, amoMeta], AddToList)
            else list.AddMembers(TStructuredTypeSymbol(FPreviousSymbol.Typ), FContextSymbol, [amoInstance, amoMeta]);

         end else if FPreviousSymbol.Typ is TStructuredTypeMetaSymbol then begin

            list.AddMembers(TStructuredTypeMetaSymbol(FPreviousSymbol.Typ).StructSymbol, FContextSymbol, [amoMeta]);

         end else if FPreviousSymbol is TUnitSymbol then begin

            AddUnitSymbol(TUnitSymbol(FPreviousSymbol), list);

         end else if FPreviousSymbol.Typ is TArraySymbol then begin

            AddStaticArrayHelpers(list);
            if FPreviousSymbol.Typ is TDynamicArraySymbol then begin
               AddDynamicArrayHelpers(TDynamicArraySymbol(FPreviousSymbol.Typ), list);
            end;

         end else if FPreviousSymbol.Typ is TAssociativeArraySymbol then begin

            AddAssociativeArrayHelpers(TAssociativeArraySymbol(FPreviousSymbol.Typ), list);

         end else if FPreviousSymbol.Typ is TBaseStringSymbol then begin

            AddStaticArrayHelpers(list);

         end else if FPreviousSymbol is TEnumerationSymbol then begin

            list.AddSymbolTable(TEnumerationSymbol(FPreviousSymbol).Elements);

         end else if    (FPreviousSymbol is TElementSymbol)
                     or (FPreviousSymbol.Typ is TEnumerationSymbol) then begin

            AddEnumerationElementHelpers(list);

         end else if (FPreviousSymbol.Typ is TBaseVariantSymbol) and (FPreviousSymbol.Typ.Name='JSONVariant') then begin

            AddJSONVariantHelpers(list);

         end else if list.Count=0 then

            FPreviousSymbol:=nil;

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
   context : TdwsSourceContext;
   funcSym : TFuncSymbol;
   methSym : TMethodSymbol;
begin
   if FPreviousSymbol<>nil then Exit;

   context:=FLocalContext;

   list:=TSimpleSymbolList.Create;
   try
      while context<>nil do begin
         list.AddSymbolTable(context.LocalTable);

         funcSym:=context.ParentSym.AsFuncSymbol;
         if funcSym<>nil then begin
            list.AddDirectSymbolTable(funcSym.Params);
            list.AddDirectSymbolTable(funcSym.InternalParams);

            // if a method, add the object's members to the list
            if funcSym is TMethodSymbol then begin
               methSym:=TMethodSymbol(funcSym);
               if methSym.IsClassMethod then
                  list.AddMembers(methSym.StructSymbol, methSym.StructSymbol, [amoMeta], AddToList)
               else list.AddMembers(methSym.StructSymbol, methSym.StructSymbol, [amoInstance, amoMeta], AddToList);
            end;
         end;

         case context.Token of
            ttARRAY : begin
               if context.ParentSym is TRecordSymbol then
                  list.AddMembers(TRecordSymbol(context.ParentSym), TRecordSymbol(context.ParentSym), [amoInstance, amoMeta], AddToList, TFieldSymbol);
            end;
            ttPROPERTY : if context.ParentSym is TPropertySymbol then begin
               list.AddMembers(TPropertySymbol(context.ParentSym).OwnerSymbol, nil, [amoInstance, amoMeta, amoTypedOnly], AddToList);
               FList.Remove(context.ParentSym);
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
function TdwsSuggestions.GetCode(i : Integer) : String;
begin
   Result := FList[i].Name;
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
            Result:=scRecord
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

      if symbolClass.InheritsFrom(TFieldSymbol) then
         Result:=scField
      else if symbolClass.InheritsFrom(TParamSymbol) then
         Result:=scParameter
      else if symbolClass.InheritsFrom(TPropertySymbol) then
         Result:=scProperty
      else if symbolClass.InheritsFrom(TConstSymbol) then begin
         if symbolClass.InheritsFrom(TElementSymbol) then
            Result:=scElement
         else Result:=scConst
      end else Result:=scVariable;

   end else if symbolClass.InheritsFrom(TReservedWordSymbol) then
      Result:=scReservedWord
   else if symbolClass.InheritsFrom(TSpecialFunctionSymbol) then
      Result:=scSpecialFunction;

 end;

// GetCaption
//
function TdwsSuggestions.GetCaption(i : Integer) : String;

   function SafeSymbolName(symbol : TSymbol) : String;
   begin
      if symbol<>nil then begin
         Result:=symbol.Name;
         if (Result='') and (symbol.Typ<>nil) then
            Result:=symbol.Caption;
      end else Result:='???';
   end;

var
   symbol : TSymbol;
   funcSym : TFuncSymbol;
   valueSym : TValueSymbol;
   enumSym : TEnumerationSymbol;
   propSymbol : TPropertySymbol;
   alias : TAliasSymbol;
begin
   symbol:=FList[i];
   funcSym:=symbol.AsFuncSymbol;
   if funcSym<>nil then begin

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

   end else if symbol is TAliasSymbol then begin

      alias:=TAliasSymbol(symbol);
      Result:=alias.Name+' = '+SafeSymbolName(alias.Typ);

   end else if symbol is TSpecialFunctionSymbol then begin

      if symbol.Name='DebugBreak' then
         Result:=symbol.Name
      else Result:=symbol.Name+' ( special )';

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

function TdwsSuggestions.PreviousSymbol: TSymbol;
begin
  Result := FPreviousSymbol;
end;

// ------------------
// ------------------ TSimpleSymbolList ------------------
// ------------------

// ScopeStruct
//
function TSimpleSymbolList.ScopeStruct(symbol : TSymbol) : TCompositeTypeSymbol;
begin
   if symbol is TCompositeTypeSymbol then
      Result:=TCompositeTypeSymbol(symbol)
   else if symbol is TMethodSymbol then
      Result:=TMethodSymbol(symbol).StructSymbol
   else if symbol is TStructuredTypeMetaSymbol then
      Result:=(TStructuredTypeMetaSymbol(symbol).Typ as TCompositeTypeSymbol)
   else Result:=nil;
end;

// ScopeVisiblity
//
function TSimpleSymbolList.ScopeVisiblity(scope, struct : TCompositeTypeSymbol) : TdwsVisibility;
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
   if table=nil then Exit;
   if table is TLinkedSymbolTable then
      table:=TLinkedSymbolTable(table).ParentSymbolTable;
   for sym in table do begin
      if sym is TUnitSymbol then
         AddNameSpace(TUnitSymbol(sym))
      else begin
         Add(sym);
         if sym.ClassType=TEnumerationSymbol  then
            AddEnumeration(TEnumerationSymbol(sym));
      end;
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

// AddEnumeration
//
procedure TSimpleSymbolList.AddEnumeration(enum : TEnumerationSymbol; const addToList : TProcAddToList = nil);
begin
   if enum.Style=enumClassic then
      AddDirectSymbolTable(enum.Elements);
end;

// AddMembers
//
procedure TSimpleSymbolList.AddMembers(struc : TCompositeTypeSymbol; from : TSymbol;
                                       options : TAddMembersOptions;
                                       const addToList : TProcAddToList = nil;
                                       restrictTo : TSymbolClass = nil);

   function IsMetaAccessor(sym : TSymbol) : Boolean;
   var
      symClass : TClass;
   begin
      if sym=nil then Exit(False);
      symClass:=sym.ClassType;
      Result:=   (symClass=TClassConstSymbol)
              or (symClass=TClassVarSymbol)
              or (symClass.InheritsFrom(TMethodSymbol) and TMethodSymbol(sym).IsClassMethod);
   end;

var
   sym : TSymbol;
   symClass : TClass;
   scope : TCompositeTypeSymbol;
   methSym : TMethodSymbol;
   visibility : TdwsVisibility;
   first, allowConstructors : Boolean;
begin
   scope:=ScopeStruct(from);
   visibility:=ScopeVisiblity(scope, struc);
   first:=True;
   allowConstructors:=(struc is TClassSymbol) and not TClassSymbol(struc).IsStatic;

   repeat
      for sym in struc.Members do begin
         if sym.Name = '' then continue;
         symClass:=sym.ClassType;
         if symClass=TClassOperatorSymbol then continue;
         if (restrictTo <> nil) and not symClass.InheritsFrom(restrictTo) then continue;
         if not sym.IsVisibleFor(visibility) then continue;

         if    (symClass=TClassConstSymbol)
            or (symClass=TClassVarSymbol) then begin

            if not (amoMeta in options) then continue;

         end else if sym is TMethodSymbol then begin

            methSym := TMethodSymbol(sym);

            if methSym.IsClassMethod then begin
               if not (amoMeta in options) then continue;
            end else if methSym.Kind=fkConstructor then begin
               if not allowConstructors then continue;
            end else if not (amoInstance in options) then continue;

         end;

         if (sym.Typ <> nil) or not (amoTypedOnly in options) then
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

// AddNameSpace
//
procedure TSimpleSymbolList.AddNameSpace(unitSym : TUnitSymbol);
begin
   if unitSym.Main<>nil then
      Add(unitSym)
   else unitSym.EnumerateNameSpaceUnits(AddNameSpace);
end;

// Remove
//
procedure TSimpleSymbolList.Remove(sym : TSymbol);
var
   i : Integer;
begin
   for i := Count-1 downto 0 do begin
      if Items[i] = sym then begin
         Extract(i);
         Break;
      end;
   end;
end;

end.
