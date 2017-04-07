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
unit dwsContextMap;

{$I dws.inc}

interface

uses dwsUtils, dwsScriptSource, dwsSymbols, dwsTokenizer, dwsJSON, TypInfo;

type

   TdwsSourceContext = class;
   TdwsSourceContextCallBack = procedure (context : TdwsSourceContext) of object;

   // Context within the script. (A block of code) Can be nested
   //
   TdwsSourceContext = class (TRefCountedObject)
      private
         FParentContext : TdwsSourceContext;
         FParentSymbol : TSymbol;     // a parent symbol would be a procedure/method, etc.
         FSubContexts : TTightList;   // contexts that are inside of this one
         FEndPos : TScriptPos;
         FStartPos : TScriptPos;
         FData : Pointer;             // pointer to some data element (for users)
         FLocalTable : TSymbolTable;  // symbol table associated with the context (begin..end blocks, TProcedures, etc)
         FToken : TTokenType;         // token associated with opening the context

      protected
         function GetSubContext(index : Integer) : TdwsSourceContext; inline;

      public
         constructor Create(aParent : TdwsSourceContext; const aStartPos : TScriptPos;
                            aParentSymbol : TSymbol; aToken : TTokenType);
         destructor Destroy; override;

         function IsPositionInContext(const aPos : TScriptPos) : Boolean; overload;
         function IsPositionInContext(aCol, aLine : Integer; const sourceName : String) : Boolean; overload;
         function HasParentSymbolOfClass(SymbolType: TSymbolClass; SearchParents: Boolean): Boolean;

         function FindContext(parentSymbol : TSymbol) : TdwsSourceContext;
         function FindContextByToken(aToken : TTokenType) : TdwsSourceContext;
         procedure EnumerateContextsOfSymbol(aParentSymbol : TSymbol; const callBack : TdwsSourceContextCallBack);

         procedure WriteToJSON(writer : TdwsJSONWriter);

         property Parent : TdwsSourceContext read FParentContext;
         property ParentSym : TSymbol read FParentSymbol write FParentSymbol;
         property Token : TTokenType read FToken write FToken;

         property SubContexts : TTightList read FSubContexts;
         property SubContext[index : Integer] : TdwsSourceContext read GetSubContext;
         property Count : Integer read FSubContexts.FCount;

         property StartPos : TScriptPos read FStartPos;
         property EndPos : TScriptPos read FEndPos;
         property Data : Pointer read FData write FData;
         property LocalTable : TSymbolTable read FLocalTable write FLocalTable;
   end;

   // Map the various script contexts. (Code blocks)
   TdwsSourceContextMap = class
      private
         FScriptContexts : TTightList; // list of top-level contexts
         FCurrentContext : TdwsSourceContext;   // current context (used when adding and leaving)

      protected
         function GetContext(index : Integer) : TdwsSourceContext; inline;

      public
         destructor Destroy; override;

         { Push a context on to the stack - procedures have a symbol context.
         Standard Begin..end blocks do not have a ParentSymbol. }
         procedure OpenContext(const startPos : TScriptPos; parentSymbol : TSymbol; token : TTokenType);
         { Pop a context off the stack }
         procedure CloseContext(const aEndPos : TScriptPos; onlyIfTokenType : TTokenType = ttNone);
         { Pops and close all opened contexts in the stack }
         procedure CloseAllContexts(const aEndPos : TScriptPos);

         { Suspends the current context and goes back to top level }
         function SuspendContext : TdwsSourceContext;
         { Sets specified context as current context }
         procedure ResumeContext(aContext : TdwsSourceContext);

         // return the first context group based on its parent
         function FindContext(AParentSymbol : TSymbol) : TdwsSourceContext; overload;
         function FindContext(aCol, aLine : Integer; sourceFile : TSourceFile) : TdwsSourceContext; overload;
         function FindContext(aCol, aLine : Integer; const sourceName : String) : TdwsSourceContext; overload;
         function FindContext(const ScriptPos : TScriptPos) : TdwsSourceContext; overload;
         function FindContextByToken(aToken : TTokenType) : TdwsSourceContext;
         procedure EnumerateContextsOfSymbol(aParentSymbol : TSymbol; const callBack : TdwsSourceContextCallBack);

         procedure WriteToJSON(writer : TdwsJSONWriter);

         property Contexts : TTightList read FScriptContexts;
         property Context[index : Integer] : TdwsSourceContext read GetContext;
         property Count : Integer read FScriptContexts.FCount;
         property Current : TdwsSourceContext read FCurrentContext;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsSourceContext ------------------
// ------------------

// Create
//
constructor TdwsSourceContext.Create(aParent : TdwsSourceContext; const aStartPos : TScriptPos;
                            aParentSymbol : TSymbol; aToken : TTokenType);
begin
   FParentContext := AParent;
   FParentSymbol  := AParentSymbol;
   FStartPos := AStartPos;
   FToken := aToken;
end;

// Destroy
//
destructor TdwsSourceContext.Destroy;
begin
   FSubContexts.Clean;
   inherited;
end;

// GetSubContext
//
function TdwsSourceContext.GetSubContext(index : Integer) : TdwsSourceContext;
begin
   Result:=TdwsSourceContext(FSubContexts.List[index]);
end;

// HasParentSymbolOfClass
//
function TdwsSourceContext.HasParentSymbolOfClass(SymbolType: TSymbolClass;
  SearchParents: Boolean): Boolean;
begin
  // Return if the context has a parent symbol of the specified type. Optionally
  // search up through other parent contexts
  Result := False;
  if Assigned(ParentSym) then
    Result := (ParentSym is SymbolType);
  // if not found and parents should be searched also, recurse until no more
  // parents or the symbol type is found.
  if (not Result) and SearchParents then
    if Assigned(Parent) then
      Result := Parent.HasParentSymbolOfClass(SymbolType, SearchParents);
end;

// FindContext
//
function TdwsSourceContext.FindContext(parentSymbol : TSymbol) : TdwsSourceContext;
var
   i : Integer;
begin
   if FParentSymbol=parentSymbol then Exit(Self);
   for i:=0 to Count-1 do begin
      Result:=SubContext[i].FindContext(parentSymbol);
      if Result<>nil then Exit;
   end;
   Result:=nil;
end;

// FindContextByToken
//
function TdwsSourceContext.FindContextByToken(aToken : TTokenType) : TdwsSourceContext;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      Result:=SubContext[i];
      if Result.Token=aToken then Exit;
   end;
   Result:=nil;
end;

// EnumerateContextsOfSymbol
//
procedure TdwsSourceContext.EnumerateContextsOfSymbol(aParentSymbol : TSymbol; const callBack : TdwsSourceContextCallBack);
var
   i : Integer;
begin
   if ParentSym=aParentSymbol then
      callBack(Self);
   for i:=0 to Count-1 do
      SubContext[i].EnumerateContextsOfSymbol(aParentSymbol, callBack);
end;

// WriteToJSON
//
procedure TdwsSourceContext.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginObject;

   writer.WriteName('Token');
   writer.WriteString(UnicodeString(GetEnumName(TypeInfo(TTokenType), Ord(Token))));

   writer.WriteName('Symbol');
   if ParentSym=nil then
      writer.WriteNull
   else begin
      writer.BeginObject;
      writer.WriteName('Class');
      writer.WriteString(UnicodeString(ParentSym.ClassName));
      writer.WriteName('Name');
      writer.WriteString(UnicodeString(ParentSym.Name));
      writer.EndObject;
   end;

   writer.WriteName('SubContexts');
   writer.BeginArray;
   for i:=0 to Count-1 do
      SubContext[i].WriteToJSON(writer);
   writer.EndArray;

   writer.EndObject;
end;

// IsPositionInContext
//
function TdwsSourceContext.IsPositionInContext(const aPos : TScriptPos) : Boolean;
begin
   Result := IsPositionInContext(aPos.Col, aPos.Line, aPos.SourceFile.Name);
end;

// IsPositionInContext
//
function TdwsSourceContext.IsPositionInContext(aCol, aLine : Integer; const sourceName : String) : Boolean;
begin
   // check if the position is in the same SourceFile
   if sourceName<>'' then begin // if empty, don't check it
      if not FStartPos.IsSourceFile(sourceName) then begin
         Result:=False;
         Exit;
      end;
   end;

   // if inside a multi-line context
   Result := (aLine > FStartPos.Line) and (aLine < FEndPos.Line);
   if not Result then begin
      // if not, check for a one-line context (inside the context begin and end cols)
      if FStartPos.Line = FEndPos.Line then
         Result:=    (aLine = FStartPos.Line)
                 and (aCol >= FStartPos.Col)
                 and (aCol <= FEndPos.Col)
      else begin
         // not a single-line context
         if FEndPos.SourceFile=nil then begin
            // unclosed context (compiled with errors)
            Result :=    (aLine > FStartPos.Line)
                      or ((aLine = FStartPos.Line) and (aCol >= FStartPos.Col)) ;    // after start
         end else begin
            Result :=    ((aLine = FStartPos.Line) and (aCol >= FStartPos.Col)) // on top line, inside start
                      or ((aLine = FEndPos.Line) and (aCol <= FEndPos.Col));    // on bottom line, inside end
         end;
      end;
   end;
end;

// ------------------
// ------------------ TdwsSourceContextMap ------------------
// ------------------

// Destroy
//
destructor TdwsSourceContextMap.Destroy;
begin
   FScriptContexts.Clean;
   inherited;
end;

// FindContext
//
function TdwsSourceContextMap.FindContext(aParentSymbol : TSymbol): TdwsSourceContext;
var
   x : Integer;
begin
   for x:=0 to FScriptContexts.Count-1 do begin
      Result:=TdwsSourceContext(FScriptContexts.List[x]);
      if Result.FParentSymbol=aParentSymbol then Exit;
   end;
   Result:=nil;
end;

// FindContextByToken
//
function TdwsSourceContextMap.FindContextByToken(aToken : TTokenType) : TdwsSourceContext;
var
   x : Integer;
begin
   for x:=0 to FScriptContexts.Count-1 do begin
      Result:=TdwsSourceContext(FScriptContexts.List[x]);
      if Result.Token=aToken then Exit;
   end;
   Result:=nil;
end;

// EnumerateContextsOfSymbol
//
procedure TdwsSourceContextMap.EnumerateContextsOfSymbol(aParentSymbol : TSymbol; const callBack : TdwsSourceContextCallBack);
var
   x : Integer;
begin
   for x:=0 to FScriptContexts.Count-1 do
      TdwsSourceContext(FScriptContexts.List[x]).EnumerateContextsOfSymbol(aParentSymbol, callBack);
end;

// WriteToJSON
//
procedure TdwsSourceContextMap.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i:=0 to FScriptContexts.Count-1 do
      TdwsSourceContext(FScriptContexts.List[i]).WriteToJSON(writer);
   writer.EndArray;
end;

// FindContext
//
function TdwsSourceContextMap.FindContext(aCol, aLine : Integer; sourceFile : TSourceFile) : TdwsSourceContext;
begin
   Result:=FindContext(aCol, aLine, sourceFile.Name);
end;

// FindContext
//
function TdwsSourceContextMap.FindContext(aCol, aLine : Integer; const sourceName : String) : TdwsSourceContext;
var
   returnContext : TdwsSourceContext;    // Gets set to the context found
   hitEnd : Boolean;            // Followed branch to end, stop searching

   function FoundContext(context : TdwsSourceContext) : Boolean;
   var
      x : Integer;
      subContext : TdwsSourceContext;
   begin
      Result := False;
      { Record that this context contains it and should be returned (provided it
        doesn't go deeper) }
      returnContext := context;
      { Search sub-contexts }
      for x := 0 to context.SubContexts.Count - 1 do begin
         subContext:=TdwsSourceContext(context.SubContexts.List[x]);
         if subContext.IsPositionInContext(aCol, aLine, sourceName) then
            Result := FoundContext(subContext)
      end;
      { We got here because it was found. After all subContexts were checked,
        it wasn't found so we've hit the end. }
      if not Result then
         hitEnd := True;
   end;

var
   i : Integer;
   context : TdwsSourceContext;
begin
   { If this position is not in the top level contexts then it won't be in
     subcontexts. Use a recursive search to find the lowest context at which the
     position can be found. }

   returnContext := nil;
   hitEnd        := False;
   { Cycle all top level contexts. Burrow into each to find the lowest level that
     matches the criteria. }
   for i := 0 to FScriptContexts.Count - 1 do begin
      if hitEnd then
         Break;
      { If in top-level context, burrow into subcontexts }
      context:=TdwsSourceContext(FScriptContexts.List[i]);
      if context.IsPositionInContext(aCol, aLine, sourceName) then
         if not FoundContext(context) then
            Break;
   end;
   Result := returnContext;
end;

// FindContext
//
function TdwsSourceContextMap.FindContext(const scriptPos : TScriptPos): TdwsSourceContext;
begin
   Result:=FindContext(scriptPos.Col, scriptPos.Line, scriptPos.SourceName);
end;

// OpenContext
//
procedure TdwsSourceContextMap.OpenContext(const startPos : TScriptPos; parentSymbol : TSymbol; token : TTokenType);
var
   newContext: TdwsSourceContext;
begin
   // Uses a simple 'stack' concept. If currently in a context and a new context
   // is opened then the new context is a sub context of the current context.
   // new context is owned by the current context
   newContext:=TdwsSourceContext.Create(FCurrentContext, startPos, parentSymbol, token);
   // Add new context to the appropriate 'parent' context
   if FCurrentContext=nil then           // if top-level,
      FScriptContexts.Add(newContext)      // Add to top-level contexts
   else FCurrentContext.SubContexts.Add(newContext);
   FCurrentContext:=newContext;
end;

// CloseContext
//
procedure TdwsSourceContextMap.CloseContext(const aEndPos : TScriptPos; onlyIfTokenType : TTokenType = ttNone);
begin
   if (onlyIfTokenType<>ttNone) and (FCurrentContext.Token<>onlyIfTokenType) then Exit;

   FCurrentContext.FEndPos := AEndPos;       // close the current context
   { if the CurrentContext is not a top-level one, then pop the stack and make
     the new context the closed one's parent }
   FCurrentContext := FCurrentContext.Parent;
end;

// CloseAllContexts
//
procedure TdwsSourceContextMap.CloseAllContexts(const aEndPos : TScriptPos);
begin
   while FCurrentContext<>nil do
      CloseContext(aEndPos);
end;

// SuspendContext
//
function TdwsSourceContextMap.SuspendContext : TdwsSourceContext;
begin
   Result:=FCurrentContext;
   FCurrentContext:=nil;
end;

// ResumeContext
//
procedure TdwsSourceContextMap.ResumeContext(aContext : TdwsSourceContext);
begin
   Assert((FCurrentContext=nil) or (FCurrentContext.Token in [ttUNIT, ttINTERFACE]));
   FCurrentContext:=aContext;
end;

// GetContext
//
function TdwsSourceContextMap.GetContext(index : Integer) : TdwsSourceContext;
begin
   Result:=TdwsSourceContext(FScriptContexts.List[index]);
end;

end.
