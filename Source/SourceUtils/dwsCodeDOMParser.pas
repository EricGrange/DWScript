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
   dwsCodeDOM, dwsCodeDOMNodes;

type
   TdwsRuleItemFlag = ( rifOptional, rifRestart, rifSkipSnippet, rifEndIfNotPresent );
   TdwsRuleItemFlags = set of TdwsRuleItemFlag;

   TdwsRuleItem = class (TRefCountedObject)
      private
         FFlags : TdwsRuleItemFlags;

      public
         property Flags : TdwsRuleItemFlags read FFlags;
         function SetFlags(aFlags : TdwsRuleItemFlags) : TdwsRuleItem;

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; virtual; abstract;
   end;

   TdwsRuleItems = class (TObjectList<TdwsRuleItem>);

   TdwsParserRuleFlag = ( prfRoot, prfReplaceBySingleChild );
   TdwsParserRuleFlags = set of TdwsParserRuleFlag;

   TdwsParserRule = class (TRefCountedObject)
      private
         FName : String;
         FItems : TdwsRuleItems;
         FFlags : TdwsParserRuleFlags;

         FLastFailedAttemptAt : TScriptPos;

      protected
         function GetItem(index : Integer) : TdwsRuleItem; inline;

         procedure ResetState;

      public
         constructor Create(const aName : String; aFlags : TdwsParserRuleFlags = []);
         destructor Destroy; override;

         property Name : String read FName;
         property Flags : TdwsParserRuleFlags read FFlags;

         property Items[index : Integer] : TdwsRuleItem read GetItem;
         function ItemCount : Integer; inline;
         function Add(anItem : TdwsRuleItem) : TdwsParserRule; inline;

         function AddMatchName : TdwsParserRule;
         function AddMatchTokenType(aTokenType : TTokenType; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
         function AddMatchTokenTypes(aTokenTypes : TTokenTypes; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;
         function AddMatchStatementEnd : TdwsParserRule;
         function AddSubRule(aRule : TdwsParserRule; const flags : TdwsRuleItemFlags = []) : TdwsParserRule;

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; virtual; abstract;
   end;

   TdwsParserRuleNode = class (TdwsParserRule)
      private
         FDOMNodeClass : TdwsCodeDOMNodeClass;

      public
         constructor Create(const aName : String; aDOMNodeClass : TdwsCodeDOMNodeClass; aFlags : TdwsParserRuleFlags = []);

         property DOMNodeClass : TdwsCodeDOMNodeClass read FDOMNodeClass;

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
   end;

   TdwsParserRuleAlternative = class (TdwsParserRule)
      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
   end;

   TdwsParserRuleArray = array of TdwsParserRule;

   TdwsParserRules = class (TObjectList<TdwsParserRule>)
      protected
         procedure ResetStates;

      public
         function RuleByName(const aName : String) : TdwsParserRule;

         function NewRuleNode(const aName : String; aDOMNodeClass : TdwsCodeDOMNodeClass;
                              aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleNode;
         function NewRuleAlternative(const aName : String; aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleAlternative;
   end;

   TdwsParser = class
      private
         FTokenizer : TTokenizer;
         FRules : TdwsParserRules;
         FRootRules : TdwsParserRuleArray;

      protected
         procedure PrepareRootRules;

      public
         constructor Create(aTokenizer : TTokenizer; aRules : TdwsParserRules);
         destructor Destroy; override;

         property Tokenizer : TTokenizer read FTokenizer;
         property Rules : TdwsParserRules read FRules;

         function Parse(const source : TSourceFile) : TdwsCodeDOM;
   end;

   TdwsRuleItem_MatchTokenType = class (TdwsRuleItem)
      private
         FTokenTypes : TTokenTypes;

      public
         constructor Create(aTokenTypes : TTokenTypes);

         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;

         property TokenTypes : TTokenTypes read FTokenTypes;
   end;

   TdwsRuleItem_MatchStatementEnd = class (TdwsRuleItem)
      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
   end;

   TdwsRuleItem_MatchName = class (TdwsRuleItem)
      public
         function Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode; override;
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
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

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
   Result := Self;
end;

// AddMatchName
//
function TdwsParserRule.AddMatchName : TdwsParserRule;
begin
   Result := Add(TdwsRuleItem_MatchName.Create);
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
      context.Debug('Skipping %s', [ name ]);
      Exit(nil);
   end;

   context.Debug('Parsing %s', [ name ]);

   initialToken := context.Token;

   Result := nil;
   i := 0;
   while i < ItemCount do begin
      ruleItem := Items[i];
      Inc(i);
      subNode := ruleItem.Parse(context);
      if subNode = nil then begin
         if rifOptional in ruleItem.Flags then continue;
         if rifEndIfNotPresent in ruleItem.FFlags then break;
         context.Debug('   FAILED %s[%d]', [ name, i ]);
         FLastFailedAttemptAt := context.TokenBeginPos;
         if Result <> nil then begin
            FreeAndNil(Result);
            context.Token := initialToken;
         end;
         Exit;
      end;
      context.Debug('   Passed %s[%d]', [ name, i-1 ]);
      if rifRestart in ruleItem.Flags then
         i := 0;
      if Result = nil then
         Result := DOMNodeClass.Create;
      if rifSkipSnippet in ruleItem.Flags then
         subNode.Free
      else Result.AddChild(subNode);
   end;

   if Result <> nil then begin
      context.Debug('   Passed %s', [ name ]);
      if (prfReplaceBySingleChild in Flags) and (Result.ChildCount = 1) then begin
         var child := Result.Child[0];
         Result.Extract(child);
         Result.Free;
         Result := child;
      end;
   end else begin
      context.Debug('   FAILED %s', [ name ]);
      context.Token := initialToken;
   end;
end;

// ------------------
// ------------------ TdwsParserRuleAlternative ------------------
// ------------------

// Parse
//
function TdwsParserRuleAlternative.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
var
   ruleItem : TdwsRuleItem;
   i : Integer;
begin
   Assert(ItemCount > 0, 'Rule ' + Name + ' undefined');
   if FLastFailedAttemptAt.SamePosAs(context.TokenBeginPos) then begin
      context.Debug('Skipping %s', [ name ]);
      Exit(nil);
   end;
   context.Debug('Parsing %s', [ name ]);

   Result := nil;

   for i := 0 to ItemCount-1 do begin
      ruleItem := Items[i];
      Result := ruleItem.Parse(context);
      if Result <> nil then begin
         context.Debug('   Passed %s[%d]', [ name, i ]);
         Exit;
      end;
   end;
   context.Debug('   FAILED %s', [ name ]);
   FLastFailedAttemptAt := context.TokenBeginPos;
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

// Parse
//
function TdwsRuleItem_MatchTokenType.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   if context.TokenType in TokenTypes then
      Result := TdwsCodeDOMSnippet.ParseFromContext(context)
   else Result := nil;
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
   Add(Result);
end;

// NewRuleAlternative
//
function TdwsParserRules.NewRuleAlternative(const aName : String; aFlags : TdwsParserRuleFlags = []) : TdwsParserRuleAlternative;
begin
   Result := TdwsParserRuleAlternative.Create(aName, aFlags);
   Add(Result);
end;

// ------------------
// ------------------ TdwsParserRules ------------------
// ------------------

// Create
//
constructor TdwsParser.Create(aTokenizer : TTokenizer; aRules : TdwsParserRules);
begin
   inherited Create;
   FTokenizer := aTokenizer;
   FRules := aRules;
   PrepareRootRules;
end;

// Destroy
//
destructor TdwsParser.Destroy;
begin
   inherited;
   FTokenizer.Free;
   FRules.Free;
end;

// Parse
//
function TdwsParser.Parse(const source : TSourceFile) : TdwsCodeDOM;
begin
   Result := TdwsCodeDOM.Create;

   Rules.ResetStates;

   var context := TdwsCodeDOMContext.Create;
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
            node := FRootRules[i].Parse(context);
            if node <> nil then Break;
         end;
         if node = nil then begin
            Assert(context.Token <> nil);
            Result.Root.AddChildSnippet(context);
         end else Result.Root.AddChild(node);
      end;

      Result.Prepare;
   finally
      context.Free;
   end;

end;

// PrepareRootRules
//
procedure TdwsParser.PrepareRootRules;
var
   i, k : Integer;
begin
   SetLength(FRootRules, Rules.Count);
   k := 0;
   for i := 0 to Rules.Count-1 do begin
      var rule := Rules[i];
      if prfRoot in rule.Flags then begin
         FRootRules[k] := rule;
         Inc(k);
      end;
   end;
   SetLength(FRootRules, k);
end;

// ------------------
// ------------------ TdwsRuleItem_MatchName ------------------
// ------------------

// Parse
//
function TdwsRuleItem_MatchName.Parse(context : TdwsCodeDOMContext) : TdwsCodeDOMNode;
begin
   if context.TokenType = ttNAME then begin
      Result := TdwsCodeDOMSnippet.ParseFromContext(context);
   end else Result := nil;
end;

end.
