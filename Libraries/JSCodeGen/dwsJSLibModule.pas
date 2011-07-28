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
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{$I dws.inc}
unit dwsJSLibModule;

interface

uses Windows, Classes, SysUtils, dwsLanguageExtension, dwsComp, dwsCompiler,
   dwsExprs, dwsTokenizer, dwsSymbols, dwsErrors, dwsCoreExprs, dwsStack,
   dwsStrings, dwsXPlatform;

type

   // TdwsJSLibModule
   //
   TdwsJSLibModule = class (TdwsCustomLangageExtension)
      private

      protected
         function CreateExtension : TdwsLanguageExtension; override;

      public
   end;

   // TdwsJSLanguageExtension
   //
   TdwsJSLanguageExtension = class (TdwsLanguageExtension)
      public
         function ReadInstr(compiler : TdwsCompiler) : TNoResultExpr; override;
   end;

   // TdwsJSBlockExpr
   //
   TdwsJSBlockExpr = class (TNoResultExpr)
      private
         FCode : String;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const code : String);

         procedure EvalNoResult(exec : TdwsExecution); override;

         property Code : String read FCode write FCode;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsJSLibModule ------------------
// ------------------

// CreateExtension
//
function TdwsJSLibModule.CreateExtension : TdwsLanguageExtension;
begin
   Result:=TdwsJSLanguageExtension.Create;
end;

// ------------------
// ------------------ TdwsJSLanguageExtension ------------------
// ------------------

// ReadInstr
//
function TdwsJSLanguageExtension.ReadInstr(compiler : TdwsCompiler) : TNoResultExpr;
var
   tok : TTokenizer;
   startPos : PChar;
   hotPos : TScriptPos;
   jsCode : String;
begin
   Result:=nil;
   tok:=compiler.Tokenizer;

   if not (tok.TestName and SameText(tok.GetToken.FString, 'asm')) then Exit;

   hotPos:=tok.HotPos;
   tok.KillToken;
   startPos:=tok.PosPtr;
   tok.TestName;

   // collect everything until 'end'
   while tok.HasTokens do begin

      if tok.Test(ttEND) then begin
         SetString(jsCode, startPos, (NativeUInt(tok.PosPtr)-NativeUInt(startPos)) div SizeOf(Char)-3);
         tok.KillToken;
         Break;
      end;

      tok.KillToken;
   end;

   if not tok.HasTokens then
      raise EScriptError.CreatePosFmt(tok.HotPos, 'Incomplete asm block%s', [tok.HotPos.AsInfo]);

   Result:=TdwsJSBlockExpr.Create(compiler.CurrentProg, hotPos, jsCode);
end;

// ------------------
// ------------------ TdwsJSBlockExpr ------------------
// ------------------

// Create
//
constructor TdwsJSBlockExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; const code : String);
begin
   inherited Create(Prog, Pos);
   FCode:=code;
end;

// EvalNoResult
//
procedure TdwsJSBlockExpr.EvalNoResult(exec : TdwsExecution);
begin
   Assert(False, ClassName+' cannot be executed');
end;

end.
