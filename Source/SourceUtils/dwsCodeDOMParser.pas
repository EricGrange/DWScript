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
unit dwsCodeDOMParser;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsTokenTypes, dwsTokenizer, dwsScriptSource,
   dwsCodeDOM, dwsCodeDOMNodes, dwsErrors;

type
   TdwsRuleItemFlag = (
      rifOptional,
      rifRestart, rifGoToStep1, rifRepeat,
      rifEndIfNotPresent,
      rifMergeChildren
   );
   TdwsRuleItemFlags = set of TdwsRuleItemFlag;

   TdwsParserRule = class;

   TdwsRuleItem = class (TRefCountedObject)
      private
         FFlags : TdwsRuleItemFlags;
         FRule : TdwsParserRule;

      public
         property Rule : TdwsParserRule read FRule;
         property Flags : TdwsRuleItemFlags read FFlags;
         function SetFlags(aFlags : TdwsRuleItemFlags) : TdwsRuleItem;

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; virtual; abstract;
         function StartTokens : TTokenTypes; virtual; abstract;
   end;

   TdwsRuleItems = class (TObjectList<TdwsRuleItem>);

   TdwsParserRuleFlag = (
      prfRoot,
      prfComment,
      prfReplaceBySingleChild
   );
   TdwsParserRuleFlags = set of TdwsParserRuleFlag;

   TdwsParser = class;

   TdwsParserRule = class (TRefCountedObject)
      private
         FName : String;
         FItems : TdwsRuleItems;
         FFlags : TdwsParserRuleFlags;
         FParser : TdwsParser;
         FStartTokens : TTokenTypes;

         FLastFailedAttemptAt : TScriptPos;

      protected
         function GetItem(index : Integer) : TdwsRuleItem; inline;

         procedure ResetState;
         function Prepare : Boolean; virtual; abstract;

      public
         constructor Create(const aName : String; aFlags : TdwsParserRuleFlags = []);
         destructor Destroy; override;

         property Name : String read FName;
         property Flags : TdwsParserRuleFlags read FFlags;
         property Parser : TdwsParser read FParser;
         property StartTokens : TTokenTypes read FStartTokens;

         property Items[index : Integer] : TdwsRuleItem read GetItem;
         function ItemCount : Integer; inline;
         function Add(anItem : TdwsRuleItem) : TdwsParserRule; inline;

         function AddMatchName(const aName : String = ''; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
         function AddMatchTokenType(aTokenType : TTokenType; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
         function AddMatchTokenTypes(aTokenTypes : TTokenTypes; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
         function AddMatchTokenTypePair(aTokenType1, aTokenType2 : TTokenType; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
         function AddMatchAnyExceptTokenTypes(aTokenTypes : TTokenTypes; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
         function AddMatchStatementEnd : TdwsParserRule;
         function AddSubRule(aRule : TdwsParserRule; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; virtual; abstract;
   end;

   TdwsParserRuleNode = class (TdwsParserRule)
      private
         FDOMNodeClass : TdwsCodeDOMNodeClass;

      protected
         function Prepare : Boolean; override;

      public
         constructor Create(const aName : String; aDOMNodeClass : TdwsCodeDOMNodeClass; aFlags : TdwsParserRuleFlags = []);

         property DOMNodeClass : TdwsCodeDOMNodeClass read FDOMNodeClass;

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
   end;

   TdwsParserRuleAlternative = class (TdwsParserRule)
      protected
         function Prepare : Boolean; override;

      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
   end;

   TdwsParserRuleWrap = class (TdwsParserRule)
      protected
         function Prepare : Boolean; override;

      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
   end;

   TdwsParserRuleArray = array of TdwsParserRule;

   TdwsParserRules = class (TObjectList<TdwsParserRule>)
      private
         FParser : TdwsParser;
         FSymbolTokens : TTokenTypes;
         FReservedTokens : TTokenTypes;

      protected
         procedure ResetStates;

      public
         property Parser : TdwsParser read FParser;

         property SymbolTokens : TTokenTypes read FSymbolTokens write FSymbolTokens;
         property ReservedTokens : TTokenTypes read FReservedTokens write FReservedTokens;

         function RuleByName(const aName : String) : TdwsParserRule;

         function NewRuleNode(const aName : String; aDOMNodeClass : TdwsCodeDOMNodeClass;
                              aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleNode;
         function NewRuleAlternative(const aName : String; aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleAlternative;
         function NewRuleWrap(const aName : String; aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleWrap;
   end;

   TdwsParser = class
      private
         FTokenizer : TTokenizer;
         FRules : TdwsParserRules;
         FRootRules : TdwsParserRuleArray;
         FCommentRules : TdwsParserRuleArray;
         FCommentStartTokens : TTokenTypes;
         FMessages : TdwsCompileMessageList;
         FSnippetPool : TdwsCodeDOMSnippetPool;
         FTokenPool : TdwsCodeDOMTokenPool;

      protected
         procedure PrepareRules;

      public
         constructor Create(aTokenizer : TTokenizer; aRules : TdwsParserRules);
         destructor Destroy; override;

         property Tokenizer : TTokenizer read FTokenizer;
         property Rules : TdwsParserRules read FRules;
         property Messages : TdwsCompileMessageList read FMessages;

         function Parse(const source : TSourceFile) : TdwsCodeDOM;
         function ParseComments(context : TdwsCodeDOMContext; base : TdwsCodeDOMNode) : Boolean;
   end;

   TdwsRuleItem_MatchTokenType = class (TdwsRuleItem)
      private
         FTokenTypes : TTokenTypes;

      public
         constructor Create(aTokenTypes : TTokenTypes);

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
         function StartTokens : TTokenTypes; override;

         property TokenTypes : TTokenTypes read FTokenTypes;
   end;

   TdwsRuleItem_MatchAnyExceptTokenType = class (TdwsRuleItem_MatchTokenType)
      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
         function StartTokens : TTokenTypes; override;
   end;

   TdwsRuleItem_MatchTokenTypePair = class (TdwsRuleItem)
      private
         FTokenType1 : TTokenType;
         FTokenType2 : TTokenType;

      public
         constructor Create(aTokenType1, aTokenType2 : TTokenType);

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
         function StartTokens : TTokenTypes; override;

         property TokenType1 : TTokenType read FTokenType1;
         property TokenType2 : TTokenType read FTokenType2;
   end;

   TdwsRuleItem_MatchStatementEnd = class (TdwsRuleItem)
      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
         function StartTokens : TTokenTypes; override;
   end;

   TdwsRuleItem_MatchName = class (TdwsRuleItem)
      private
         FName : String;

      public
         constructor Create(const aName : String);
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
         function StartTokens : TTokenTypes; override;
         property Name : String read FName;
   end;

   TdwsRuleItem_MatchAnyName = class (TdwsRuleItem)
      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
         function StartTokens : TTokenTypes; override;
   end;

   TdwsRuleItem_Rule = class (TdwsRuleItem)
      private
         FRule : TdwsParserRule;

      public
         constructor Create(aRule : TdwsParserRule);

         property Rule : TdwsParserRule read FRule;
   end;

   TdwsRuleItem_SubRule = class (TdwsRuleItem_Rule)
      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
         function StartTokens : TTokenTypes; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cAllValidTokens : TTokenTypes = [ Succ(ttNone)..High(TTokenType) ];
   cNameTokens = [
      ttNAME,
      ttDEFAULT, ttEMPTY, ttINDEX,
      ttPASCAL, ttOLD, ttSET
   ];

// ------------------
// ------------------ TdwsRuleItem ------------------
// ------------------

// SetFlags
//
function TdwsRuleItem.SetFlags(aFlags : TdwsRuleItemFlags) : TdwsRuleItem;
begin
   FFlags := aFlags;
   Result := Self;
end;

// ------------------
// ------------------ TdwsParserRule ------------------
// ------------------

// Create
//
constructor TdwsParserRule.Create(const aName : String; aFlags : TdwsParserRuleFlags = []);
begin
   inherited Create;
   FItems := TdwsRuleItems.Create;
   FName := aName;
   FFlags := aFlags;
end;

// Destroy
//
destructor TdwsParserRule.Destroy;
begin
   inherited;
   FItems.Free;
end;

// ItemCount
//
function TdwsParserRule.ItemCount : Integer;
begin
   Result := FItems.Count;
end;

// GetItem
//
function TdwsParserRule.GetItem(index : Integer) : TdwsRuleItem;
begin
   Result := FItems[index];
end;

// ResetState
//
procedure TdwsParserRule.ResetState;
begin
   FLastFailedAttemptAt.Clear;
end;

// Add
//
function TdwsParserRule.Add(anItem : TdwsRuleItem) : TdwsParserRule;
begin
   FItems.Add(anItem);
   anItem.FRule := Self;
   Result := Self;
end;

// AddMatchName
//
function TdwsParserRule.AddMatchName(const aName : String = ''; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
begin
   if aName = '' then
      Result := Add(TdwsRuleItem_MatchAnyName.Create.SetFlags(flags))
   else Result := Add(TdwsRuleItem_MatchName.Create(aName).SetFlags(flags));
end;

// AddMatchTokenType
//
function TdwsParserRule.AddMatchTokenType(aTokenType : TTokenType; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
begin
   Result := AddMatchTokenTypes([ aTokenType ], flags);
end;

// AddMatchTokenTypes
//
function TdwsParserRule.AddMatchTokenTypes(aTokenTypes : TTokenTypes; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
begin
   Result := Add(TdwsRuleItem_MatchTokenType.Create(aTokenTypes).SetFlags(flags));
end;

// AddMatchTokenTypePair
//
function TdwsParserRule.AddMatchTokenTypePair(aTokenType1, aTokenType2 : TTokenType; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
begin
   Result := Add(TdwsRuleItem_MatchTokenTypePair.Create(aTokenType1, aTokenType2).SetFlags(flags));
end;

// AddMatchAnyExceptTokenTypes
//
function TdwsParserRule.AddMatchAnyExceptTokenTypes(aTokenTypes : TTokenTypes; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
begin
   Result := Add(TdwsRuleItem_MatchAnyExceptTokenType.Create(aTokenTypes).SetFlags(flags));
end;

// AddMatchStatementEnd
//
function TdwsParserRule.AddMatchStatementEnd : TdwsParserRule;
begin
   Result := Add(TdwsRuleItem_MatchStatementEnd.Create);
end;

// AddSubRule
//
function TdwsParserRule.AddSubRule(aRule : TdwsParserRule; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
begin
   if aRule.Parser = nil then
      aRule.FParser := Parser
   else Assert(aRule.Parser = Parser);
   Result := Add(TdwsRuleItem_SubRule.Create(aRule).SetFlags(flags));
end;

// ------------------
// ------------------ TdwsParserRuleNode ------------------
// ------------------

// Create
//
constructor TdwsParserRuleNode.Create(const aName : String; aDOMNodeClass : TdwsCodeDOMNodeClass; aFlags : TdwsParserRuleFlags = []);
begin
   inherited Create(aName, aFlags);
   FDOMNodeClass := aDOMNodeClass;
end;

// Parse
//
function TdwsParserRuleNode.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
var
   subNode : TdwsCodeDOMNode;
   initialToken : TdwsCodeDOMToken;
   ruleItem : TdwsRuleItem;
   i : Integer;
begin
   Assert(ItemCount > 0, 'Rule ' + Name + ' undefined');

   if FLastFailedAttemptAt.SamePosAs(context.TokenBeginPos) then begin
      Exit(nil);
   end;

   initialToken := context.Token;

   Result := nil;
   i := 0;
   while i < ItemCount do begin
      ruleItem := Items[i];
      Inc(i);
      if context.TokenType in ruleItem.StartTokens then
         subNode := ruleItem.Parse(context)
      else subNode := nil;
      if subNode = nil then begin
         if rifOptional in ruleItem.Flags then continue;
         if (Result <> nil) and Parser.ParseComments(context, Result) then continue;
         if rifEndIfNotPresent in ruleItem.FFlags then break;
         FLastFailedAttemptAt := context.TokenBeginPos;
         if Result <> nil then begin
            context.ReleaseNode(Result);
            context.Token := initialToken;
         end;
         Exit;
      end;
      if rifRestart in ruleItem.Flags then
         i := 0
      else if rifGoToStep1 in ruleItem.Flags then
         i := 1
      else if rifRepeat in ruleItem.Flags then
         Dec(i);
      if Result = nil then
         Result := DOMNodeClass.Create;
      if rifMergeChildren in ruleItem.Flags then begin
         Result.AddChildrenFrom(subNode);
         FreeAndNil(subNode);
         if not (prfComment in Flags) then
            Parser.ParseComments(context, Result)
      end else begin
         Result.AddChild(subNode);
         if not (prfComment in Flags) then
            Parser.ParseComments(context, subNode);
      end;
   end;

   if Result <> nil then begin
      if (prfReplaceBySingleChild in Flags) and (Result.ChildCount = 1) then begin
         var child := Result.Child[0];
         Result.Extract(child);
         context.ReleaseNode(Result);
         Result := child;
      end;
   end else begin
      context.Token := initialToken;
   end;
end;

// Prepare
//
function TdwsParserRuleNode.Prepare : Boolean;
begin
   var oldTokens := FStartTokens;
   for var i := 0 to ItemCount-1 do begin
      var ruleItem := Items[i];
      FStartTokens := FStartTokens + ruleItem.StartTokens;
      if not (rifOptional in ruleItem.Flags) then Break;
   end;
   Result := (FStartTokens <> oldTokens);
end;

// ------------------
// ------------------ TdwsParserRuleAlternative ------------------
// ------------------

// Prepare
//
function TdwsParserRuleAlternative.Prepare : Boolean;
begin
   var oldTokens := FStartTokens;
   for var i := 0 to ItemCount-1 do begin
      var ruleItem := Items[i];
      FStartTokens := FStartTokens + ruleItem.StartTokens;
   end;
   Result := (FStartTokens <> oldTokens);
end;

// Parse
//
function TdwsParserRuleAlternative.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   Assert(ItemCount > 0, 'Rule ' + Name + ' undefined');
   Result := nil;

   if FLastFailedAttemptAt.SamePosAs(context.TokenBeginPos) then
      Exit;

   for var i := 0 to ItemCount-1 do begin
      var ruleItem := Items[i];
      if context.TokenType in ruleItem.StartTokens then begin
         Result := ruleItem.Parse(context);
         if Result <> nil then begin
            Exit;
         end;
      end;
   end;

   FLastFailedAttemptAt := context.TokenBeginPos;
end;

// ------------------
// ------------------ TdwsParserRuleWrap ------------------
// ------------------

// Prepare
//
function TdwsParserRuleWrap.Prepare : Boolean;
begin
   var oldTokens := FStartTokens;
   if ItemCount > 0 then
      FStartTokens := FStartTokens + Items[0].StartTokens;
   Result := (FStartTokens <> oldTokens);
end;

// Parse
//
function TdwsParserRuleWrap.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   Assert(ItemCount > 1, 'Rule ' + Name + ' incomplete');
   Result := nil;

   if FLastFailedAttemptAt.SamePosAs(context.TokenBeginPos) then
      Exit;

   var initialToken := context.Token;

   Result := Items[0].Parse(context);
   if Result = nil then begin
      FLastFailedAttemptAt := context.TokenBeginPos;
      context.Token := initialToken;
      Exit;
   end;

   var i := 1;
   while i < ItemCount do begin
      var ruleItem := Items[i];
      if context.TokenType in ruleItem.StartTokens then begin
         var sub := ruleItem.Parse(context);
         if sub <> nil then begin
            sub.InsertChild(0, Result);
            Result := sub;
            if rifRepeat in ruleItem.Flags then
               i := 0;
         end;
      end;
      Inc(i);
   end;
end;

// ------------------
// ------------------ TdwsRuleItem_MatchTokenType ------------------
// ------------------

// Create
//
constructor TdwsRuleItem_MatchTokenType.Create(aTokenTypes : TTokenTypes);
begin
   inherited Create;
   FTokenTypes := aTokenTypes;
end;

// StartTokens
//
function TdwsRuleItem_MatchTokenType.StartTokens : TTokenTypes;
begin
   Result := FTokenTypes;
end;

// Parse
//
function TdwsRuleItem_MatchTokenType.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   if context.TokenType in TokenTypes then
      Result := TdwsCodeDOMSnippet.ParseFromContext(context)
   else Result := nil;
end;

// ------------------
// ------------------ TdwsRuleItem_MatchAnyExceptTokenType ------------------
// ------------------

// Parse
//
function TdwsRuleItem_MatchAnyExceptTokenType.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   if context.TokenType in TokenTypes then
      Result := nil
   else Result := TdwsCodeDOMSnippet.ParseFromContext(context)
end;

// StartTokens
//
function TdwsRuleItem_MatchAnyExceptTokenType.StartTokens : TTokenTypes;
begin
   Result := cAllValidTokens - TokenTypes;
end;

// ------------------
// ------------------ TdwsRuleItem_MatchTokenTypePair ------------------
// ------------------

// Create
//
constructor TdwsRuleItem_MatchTokenTypePair.Create(aTokenType1, aTokenType2 : TTokenType);
begin
   inherited Create;
   FTokenType1 := aTokenType1;
   FTokenType2 := aTokenType2;
end;

// Parse
//
function TdwsRuleItem_MatchTokenTypePair.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   if     (context.TokenType = TokenType1)
      and (context.Token.Next <> nil)
      and (context.Token.Next.TokenType = TokenType2) then begin

      Result := TdwsCodeDOMSnippet.ParseFromContext(context);
      Result.AddChild(TdwsCodeDOMSnippet.ParseFromContext(context))

   end else Result := nil;
end;

// StartTokens
//
function TdwsRuleItem_MatchTokenTypePair.StartTokens : TTokenTypes;
begin
   Result := [ TokenType1 ];
end;

// ------------------
// ------------------ TdwsRuleItem_MatchStatementEnd ------------------
// ------------------

// Parse
//
function TdwsRuleItem_MatchStatementEnd.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   case context.TokenType of
      ttSEMI : begin
         Result := TdwsCodeDOMSnippet.ParseFromContext(context);
      end;
      ttELSE, ttEND, ttNone : begin
         Result := TdwsCodeDOMNop.Create;
      end;
   else
      Result := nil;
   end;
end;

// StartTokens
//
function TdwsRuleItem_MatchStatementEnd.StartTokens : TTokenTypes;
begin
   Result := [ ttSEMI, ttELSE, ttEND, ttNone ];
end;

// ------------------
// ------------------ TdwsRuleItem_Rule ------------------
// ------------------

// Create
//
constructor TdwsRuleItem_Rule.Create(aRule : TdwsParserRule);
begin
   inherited Create;
   FRule := aRule;
end;

// ------------------
// ------------------ TdwsRuleItem_SubRule ------------------
// ------------------

// Parse
//
function TdwsRuleItem_SubRule.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   Result := FRule.Parse(context);
end;

// StartTokens
//
function TdwsRuleItem_SubRule.StartTokens : TTokenTypes;
begin
   Result := FRule.StartTokens;
end;

// ------------------
// ------------------ TdwsParserRules ------------------
// ------------------

// ResetStates
//
procedure TdwsParserRules.ResetStates;
begin
   for var i := 0 to Count-1 do
      Items[i].ResetState;
end;

// RuleByName
//
function TdwsParserRules.RuleByName(const aName : String) : TdwsParserRule;
begin
   for var i := 0 to Count-1 do begin
      Result := Items[i];
      if Result.Name = aName then Exit;
   end;
   Result := nil;
end;

// NewRuleNode
//
function TdwsParserRules.NewRuleNode(const aName : String; aDOMNodeClass : TdwsCodeDOMNodeClass;
                              aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleNode;
begin
   Result := TdwsParserRuleNode.Create(aName, aDOMNodeClass, aFlags);
   Result.FParser := Parser;
   Add(Result);
end;

// NewRuleAlternative
//
function TdwsParserRules.NewRuleAlternative(const aName : String; aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleAlternative;
begin
   Result := TdwsParserRuleAlternative.Create(aName, aFlags);
   Result.FParser := Parser;
   Add(Result);
end;

// NewRuleWrap
//
function TdwsParserRules.NewRuleWrap(const aName : String; aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleWrap;
begin
   Result := TdwsParserRuleWrap.Create(aName, aFlags);
   Result.FParser := Parser;
   Add(Result);
end;

// ------------------
// ------------------ TdwsParserRules ------------------
// ------------------

// Create
//
constructor TdwsParser.Create(aTokenizer : TTokenizer; aRules : TdwsParserRules);
begin
   Assert(aRules.Parser = nil);

   inherited Create;
   FTokenizer := aTokenizer;
   FRules := aRules;
   FRules.FParser := Self;
   PrepareRules;
   FMessages := TdwsCompileMessageList.Create;
   FSnippetPool := TdwsCodeDOMSnippetPool.Create;
   FTokenPool := TdwsCodeDOMTokenPool.Create;
end;

// Destroy
//
destructor TdwsParser.Destroy;
begin
   inherited;
   FTokenizer.Free;
   FRules.Free;
   FMessages.Free;
   FSnippetPool.Free;
   FTokenPool.Free;
end;

// Parse
//
function TdwsParser.Parse(const source : TSourceFile) : TdwsCodeDOM;
begin
   Result := TdwsCodeDOM.Create;

   Messages.Clear;
   Rules.ResetStates;

   var context := TdwsCodeDOMContext.Create(Messages, FSnippetPool, FTokenPool);
   try
      FTokenizer.BeginSourceFile(source);
      FTokenizer.OnBeforeAction := context.DoBeforeTokenizerAction;
      try
         while FTokenizer.HasTokens do
            FTokenizer.KillToken;
      finally
         FTokenizer.OnBeforeAction := nil;
         FTokenizer.EndSourceFile;
      end;

      context.Token := context.HeadToken;

      Result.Root := TdwsCodeDOMMain.Create;

      while context.Token <> nil do begin
         var node : TdwsCodeDOMNode := nil;
         for var i := 0 to High(FRootRules) do begin
            var rule := FRootRules[i];
            if context.TokenType in rule.StartTokens then begin
               node := FRootRules[i].Parse(context);
               if node <> nil then Break;
            end;
         end;
         if node = nil then Break;
         Result.Root.AddChild(node);
      end;
      if context.Token <> nil then begin
         context.Messages.AddCompilerError(context.TokenBeginPos, 'No applicable rule');
         var previousSnippet := Result.Root.AddChildSnippet(context);
         while context.Token <> nil do begin
            if (context.TokenType = ttStrVal) and (previousSnippet.TokenType = ttStrVal) then begin
               previousSnippet.Snippet := previousSnippet.Snippet + context.Token.RawString;
               context.Next;
            end else previousSnippet := Result.Root.AddChildSnippet(context);
         end;
      end;

      Result.Prepare;
   finally
      context.Free;
   end;
end;

// ParseComments
//
function TdwsParser.ParseComments(context : TdwsCodeDOMContext; base : TdwsCodeDOMNode) : Boolean;
begin
   Result := False;
   if not (context.TokenType in FCommentStartTokens) then Exit;
   var prevLine := context.PreviousTokenEndPosLine;
   while context.Token <> nil  do begin
      var line := context.TokenBeginPosLine;
      var node : TdwsCodeDOMNode := nil;
      for var i := 0 to High(FCommentRules) do begin
         node := FCommentRules[i].Parse(context);
         if node <> nil then begin
            if (line = prevLine) or (base.Parent = nil) then
               base.AddChild(node)
            else base.Parent.AddChild(node);
            Result := True;
            Break;
         end;
      end;
      if node = nil then
         Break;
   end;
end;

// PrepareRules
//
procedure TdwsParser.PrepareRules;
var
   kRoot, kTail : Integer;
begin
   SetLength(FRootRules, Rules.Count);
   SetLength(FCommentRules, Rules.Count);
   FCommentStartTokens := [];
   kRoot := 0;
   kTail := 0;
   for var i := 0 to Rules.Count-1 do begin
      var rule := Rules[i];
      rule.FParser := Self;
      rule.Prepare;
      if prfRoot in rule.Flags then begin
         FRootRules[kRoot] := rule;
         Inc(kRoot);
      end;
      if prfComment in rule.Flags then begin
         FCommentRules[kTail] := rule;
         FCommentStartTokens := FCommentStartTokens + rule.StartTokens;
         Inc(kTail);
      end;
   end;
   SetLength(FRootRules, kRoot);
   SetLength(FCommentRules, kTail);

   var changed : Boolean;
   repeat
      changed := False;
      for var i := 0 to Rules.Count-1 do begin
         var rule := Rules[i];
         if rule.Prepare then
            changed := True;
      end;
   until not changed;
end;

// ------------------
// ------------------ TdwsRuleItem_MatchName ------------------
// ------------------

// Create
//
constructor TdwsRuleItem_MatchName.Create(const aName : String);
begin
   inherited Create;
   FName := aName;
end;

// Parse
//
function TdwsRuleItem_MatchName.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   if (context.TokenType = ttNAME) and SameText(context.Token.RawString, Name) then begin
      Result := TdwsCodeDOMSnippet.ParseFromContext(context);
   end else Result := nil;
end;

// StartTokens
//
function TdwsRuleItem_MatchName.StartTokens : TTokenTypes;
begin
   Result := [ ttNAME ];
end;

// ------------------
// ------------------ TdwsRuleItem_MatchAnyName ------------------
// ------------------

// Parse
//
function TdwsRuleItem_MatchAnyName.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   if context.TokenType in cNameTokens then begin
      Result := TdwsCodeDOMSnippet.ParseFromContext(context);
   end else Result := nil;
end;

// StartTokens
//
function TdwsRuleItem_MatchAnyName.StartTokens : TTokenTypes;
begin
   Result := cNameTokens;
end;

end.
