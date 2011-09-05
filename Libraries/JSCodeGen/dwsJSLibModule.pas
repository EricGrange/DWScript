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
unit dwsJSLibModule;

{$I dws.inc}

interface

uses Windows, Classes, SysUtils, dwsLanguageExtension, dwsComp, dwsCompiler,
   dwsExprs, dwsTokenizer, dwsSymbols, dwsErrors, dwsCoreExprs, dwsStack,
   dwsStrings, dwsXPlatform, StrUtils, dwsUtils;

type

   // TdwsJSLibModule
   //
   TdwsJSLibModule = class (TdwsCustomLangageExtension)
      private
         FSymbolMarker : TTokenType;

      protected
         function CreateExtension : TdwsLanguageExtension; override;
         function StoreSymbolMarker : Boolean;

      public
         constructor Create(AOwner: TComponent); override;

      published
         property SymbolMarker : TTokenType read FSymbolMarker write FSymbolMarker stored StoreSymbolMarker;
   end;

   // TdwsJSLanguageExtension
   //
   TdwsJSLanguageExtension = class (TdwsLanguageExtension)
      private
         FSymbolMarker : TTokenType;

      public
         function ReadInstr(compiler : TdwsCompiler) : TNoResultExpr; override;

         property SymbolMarker : TTokenType read FSymbolMarker write FSymbolMarker;
   end;

   // TdwsJSBlockExpr
   //
   TdwsJSBlockExpr = class (TNoResultExpr)
      private
         FCode : String;
         FSymbols : TTightList;
         FSymbolOffsets : array of Integer;

      protected
         function GetSymbol(idx : Integer) : TSymbol;
         function GetSymbolOffset(idx : Integer) : Integer;

      public
         destructor Destroy; override;

         procedure RegisterSymbol(symbol : TSymbol; offset : Integer);

         property Symbols[idx : Integer] : TSymbol read GetSymbol;
         property SymbolOffsets[idx : Integer] : Integer read GetSymbolOffset;
         function SymbolsCount : Integer;

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

const
   cDefaultSymbolMarker = ttAT;

// ------------------
// ------------------ TdwsJSLibModule ------------------
// ------------------

// Create
//
constructor TdwsJSLibModule.Create(AOwner: TComponent);
begin
   FSymbolMarker:=cDefaultSymbolMarker;
   inherited;
end;

// CreateExtension
//
function TdwsJSLibModule.CreateExtension : TdwsLanguageExtension;
var
   ext : TdwsJSLanguageExtension;
begin
   ext:=TdwsJSLanguageExtension.Create;
   ext.SymbolMarker:=SymbolMarker;
   Result:=ext;
end;

// StoreSymbolMarker
//
function TdwsJSLibModule.StoreSymbolMarker : Boolean;
begin
   Result:=(FSymbolMarker<>cDefaultSymbolMarker)
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
   jsCode, name : String;
   sym : TSymbol;
   table : TSymbolTable;
   blockExpr : TdwsJSBlockExpr;

   function FlushCode(drop : Integer) : String;
   begin
      SetString(Result, startPos, (NativeUInt(tok.PosPtr)-NativeUInt(startPos)) div SizeOf(Char)-NativeUInt(drop));
      startPos:=tok.PosPtr;
   end;

begin
   Result:=nil;
   tok:=compiler.Tokenizer;

   if not (tok.TestName and SameText(tok.GetToken.FString, 'asm')) then Exit;

   hotPos:=tok.HotPos;
   tok.KillToken;
   startPos:=tok.PosPtr;
   tok.TestName;

   blockExpr:=TdwsJSBlockExpr.Create(compiler.CurrentProg, hotPos);
   try

      jsCode:='';

      // collect everything until 'end'
      while tok.HasTokens do begin

         if tok.Test(SymbolMarker) then begin
            tok.KillToken;
            jsCode:=jsCode+FlushCode(1);

            table:=compiler.CurrentProg.Table;

            repeat

               if not tok.TestDeleteNamePos(name, hotPos) then
                  compiler.Msgs.AddCompilerStop(hotPos, CPE_NameExpected);

               tok.KillToken;
               FlushCode(0);

               sym:=table.FindSymbol(name, cvMagic);
               if sym=nil then
                  compiler.Msgs.AddCompilerStopFmt(hotPos, CPE_UnknownName, [name]);
               if sym is TStructuredTypeSymbol then
                  table:=TStructuredTypeSymbol(sym).Members;

            until not tok.TestDelete(ttDOT);

            blockExpr.RegisterSymbol(sym, Length(jsCode)+1);

         end;

         if tok.Test(ttEND) then begin
            jsCode:=jsCode+FlushCode(3);
            tok.KillToken;
            Break;
         end;

         tok.KillToken;
      end;

      blockExpr.Code:=jsCode;

   except
      blockExpr.Free;
      raise;
   end;

   Result:=blockExpr;

   if not tok.HasTokens then
      compiler.Msgs.AddCompilerErrorFmt(tok.HotPos, 'Incomplete asm block%s', [tok.HotPos.AsInfo]);
end;

// ------------------
// ------------------ TdwsJSBlockExpr ------------------
// ------------------

// Destroy
//
destructor TdwsJSBlockExpr.Destroy;
begin
   inherited;
   FSymbols.Free;
end;

// RegisterSymbol
//
procedure TdwsJSBlockExpr.RegisterSymbol(symbol : TSymbol; offset : Integer);
var
   n : Integer;
begin
   FSymbols.Add(symbol);
   n:=Length(FSymbolOffsets);
   SetLength(FSymbolOffsets, n+1);
   FSymbolOffsets[n]:=offset;
end;

// SymbolsCount
//
function TdwsJSBlockExpr.SymbolsCount : Integer;
begin
   Result:=Length(FSymbolOffsets);
end;

// EvalNoResult
//
procedure TdwsJSBlockExpr.EvalNoResult(exec : TdwsExecution);
begin
   Assert(False, ClassName+' cannot be executed');
end;

// GetSymbol
//
function TdwsJSBlockExpr.GetSymbol(idx : Integer) : TSymbol;
begin
   Result:=TSymbol(FSymbols.List[idx]);
end;

// GetSymbolOffset
//
function TdwsJSBlockExpr.GetSymbolOffset(idx : Integer) : Integer;
begin
   Result:=FSymbolOffsets[idx];
end;

end.
