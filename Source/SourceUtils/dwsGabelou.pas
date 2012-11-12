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
unit dwsGabelou;

{$I ../dws.inc}

interface

uses SysUtils, dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsGabelouStrings;

type

   TGabelouMessage = class(TScriptMessage)
      public
         constructor Create(msgs : TdwsMessageList;
                            const msgFmt : String; const args : array of const;
                            const scriptPos : TScriptPos);
         constructor CreateOnSymbolPosList(
                            msgs : TdwsMessageList; symPosList : TSymbolPositionList;
                            const description : String);

         function AsInfo : String; override;
   end;

   IdwsGabelouRule = interface
      function GetName : String;
      function GetDescription : String;

      property Name : String read GetName;
      property Description : String read GetDescription;

      procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList);
   end;

   TdwsGabelouRule = class abstract (TInterfacedSelfObject, IdwsGabelouRule)
      private
         FName : String;
         FDescription : String;

         function GetName : String;
         function GetDescription : String;

      public
         constructor Create; virtual;

         procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList); virtual; abstract;

         property Name : String read FName write FName;
         property Description : String read FDescription write FDescription;
   end;

   TdwsGabelouRuleClass = class of TdwsGabelouRule;

   TdwsGabelou = class abstract (TRefCountedObject)
      private
         class var vRegisteredRuleClasses : array of TdwsGabelouRuleClass;

         FRules : array of IdwsGabelouRule;

         procedure AddRegisteredRules;

      public
         constructor Create;

         procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList); overload;
         procedure Evaluate(const aProg : IdwsProgram); overload;

         procedure AddRule(const rule : IdwsGabelouRule);

         class procedure RegisterRuleClass(aClass : TdwsGabelouRuleClass); static;
         class procedure RegisterRuleClasses(const aClasses : array of TdwsGabelouRuleClass); static;
   end;

   TdwsSymbolDictionaryGabelouRule = class abstract (TdwsGabelouRule)
      public
         procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList); override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); virtual; abstract;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsGabelouRule ------------------
// ------------------

// Create
//
constructor TdwsGabelouRule.Create;
begin
   inherited;
end;

// GetName
//
function TdwsGabelouRule.GetName : String;
begin
   Result:=FName;
end;

// GetDescription
//
function TdwsGabelouRule.GetDescription : String;
begin
   Result:=FDescription;
end;

// ------------------
// ------------------ TdwsGabelou ------------------
// ------------------

// Create
//
constructor TdwsGabelou.Create;
begin
   inherited;
   AddRegisteredRules;
end;

// AddRegisteredRules
//
procedure TdwsGabelou.AddRegisteredRules;
var
   i : Integer;
begin
   for i:=0 to High(vRegisteredRuleClasses) do
      AddRule(vRegisteredRuleClasses[i].Create);
end;

// Evaluate
//
procedure TdwsGabelou.Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList);
var
   i : Integer;
begin
   for i:=0 to High(FRules) do
      FRules[i].Evaluate(aProg, msgs);
end;

// Evaluate
//
procedure TdwsGabelou.Evaluate(const aProg : IdwsProgram);
begin
   Evaluate(aProg, aProg.Msgs);
end;

// AddRule
//
procedure TdwsGabelou.AddRule(const rule : IdwsGabelouRule);
var
   n : Integer;
begin
   n:=Length(FRules);
   SetLength(FRules, n+1);
   FRules[n]:=rule;
end;

// RegisterRuleClass
//
class procedure TdwsGabelou.RegisterRuleClass(aClass : TdwsGabelouRuleClass);
var
   n : Integer;
begin
   n:=Length(vRegisteredRuleClasses);
   SetLength(vRegisteredRuleClasses, n+1);
   vRegisteredRuleClasses[n]:=aClass;
end;

// RegisterRuleClasses
//
class procedure TdwsGabelou.RegisterRuleClasses(const aClasses : array of TdwsGabelouRuleClass);
var
   i : Integer;
begin
   for i:=0 to High(aClasses) do
      RegisterRuleClass(aClasses[i]);
end;

// ------------------
// ------------------ TdwsSymbolDictionaryGabelouRule ------------------
// ------------------

// Evaluate
//
procedure TdwsSymbolDictionaryGabelouRule.Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList);
var
   i : Integer;
   symDict : TdwsSymbolDictionary;
   symPosList : TSymbolPositionList;
   sym : TSymbol;
begin
   symDict:=aProg.SymbolDictionary;
   for i:=0 to symDict.Count-1 do begin
      symPosList:=symDict.Items[i];
      sym:=symPosList.Symbol;
      if (sym.Name='') or (Pos(' ', sym.Name)>0) then continue; // skip magic symbols
      EvaluateSymbol(symPosList, msgs);
   end;
end;

// ------------------
// ------------------ TGabelouMessage ------------------
// ------------------

// Create
//
constructor TGabelouMessage.Create(msgs : TdwsMessageList;
                            const msgFmt : String; const args : array of const;
                            const scriptPos : TScriptPos);
begin
   inherited Create(msgs, Format(msgFmt, args), scriptPos);
end;

// CreateOnSymbolPosList
//
constructor TGabelouMessage.CreateOnSymbolPosList(
                            msgs : TdwsMessageList; symPosList : TSymbolPositionList;
                            const description : String);
begin
   inherited Create(msgs, Format('"%s", %s', [symPosList.Symbol.Name, description]),
                    symPosList.Items[0].ScriptPos);
end;

// AsInfo
//
function TGabelouMessage.AsInfo: String;
begin
   Result:=Format(GAB_HintMessage, [inherited AsInfo]);
end;

end.
