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
{$I dws.inc}
unit dwsMathComplexFunctions;

interface

uses dwsFunctions, dwsSymbols, dwsExprs, dwsStrings, dwsOperators, dwsStack,
   dwsTokenizer, SysUtils;

type
   TComplexBinOpExpr = class(TInternalFunction)
   end;

   TComplexMakeExpr = class(TComplexBinOpExpr)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

   TComplexToStrExpr = class(TComplexBinOpExpr)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

   TComplexAddOpExpr = class(TComplexBinOpExpr)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

const
   SYS_COMPLEX = 'TComplex';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TComplexProgram = class helper for TdwsProgram
      function TypComplex : TRecordSymbol;
   end;

// TypComplex
//
function TComplexProgram.TypComplex : TRecordSymbol;
begin
   Result:=SystemTable.FindTypeSymbol(SYS_COMPLEX, cvMagic) as TRecordSymbol;
end;

// RegisterComplexType
//
procedure RegisterComplexType(systemTable, unitSyms, unitTable : TSymbolTable; operators : TOperators);
var
   typComplex : TRecordSymbol;
   typFloat : TBaseFloatSymbol;
begin
   typFloat:=SystemTable.FindSymbol(SYS_FLOAT, cvMagic) as TBaseFloatSymbol;

   typComplex:=TRecordSymbol.Create(SYS_COMPLEX, nil);
   typComplex.AddField(TFieldSymbol.Create('Re', typFloat, cvPublic));
   typComplex.AddField(TFieldSymbol.Create('Im', typFloat, cvPublic));

   systemTable.AddSymbol(typComplex);
end;

// RegisterComplexOperators
//
procedure RegisterComplexOperators(systemTable, unitSyms, unitTable : TSymbolTable; operators : TOperators);
var
   typComplex : TRecordSymbol;
begin
   typComplex:=systemTable.FindTypeSymbol(SYS_COMPLEX, cvMagic) as TRecordSymbol;

   operators.RegisterOperator(ttPLUS, unitTable.FindSymbol('ComplexAdd', cvMagic) as TFuncSymbol, typComplex, typComplex);
end;

// ------------------
// ------------------ TComplexMakeExpr ------------------
// ------------------

// Execute
//
procedure TComplexMakeExpr.Execute(info : TProgramInfo);
var
   result : IInfo;
begin
   result:=info.ResultVars;
   result.Member['Re'].Value:=info.ParamAsFloat[0];
   result.Member['Im'].Value:=info.ParamAsFloat[1];
end;

// ------------------
// ------------------ TComplexToStrExpr ------------------
// ------------------

// Execute
//
procedure TComplexToStrExpr.Execute(info : TProgramInfo);
var
   c : IInfo;
   r, i : Double;
begin
   c:=info.Vars['c'];
   r:=c.Member['Re'].ValueAsFloat;
   i:=c.Member['Im'].ValueAsFloat;
   if i>0 then
      info.ResultAsString:=Format('%f + %fi', [r, i])
   else if i<0 then
      info.ResultAsString:=Format('%f - %fi', [r, Abs(i)])
   else info.ResultAsString:=Format('%f', [r]);
end;

// ------------------
// ------------------ TComplexAddOpExpr ------------------
// ------------------

// Execute
//
procedure TComplexAddOpExpr.Execute(info : TProgramInfo);
var
   result, left, right : IInfo;
begin
   result:=info.ResultVars;
   left:=info.Vars['left'];
   right:=info.Vars['right'];
   result.Member['Re'].Value:=left.Member['Re'].ValueAsFloat+right.Member['Re'].ValueAsFloat;
   result.Member['Im'].Value:=left.Member['Im'].ValueAsFloat+right.Member['Im'].ValueAsFloat;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddPreInitProc(RegisterComplexType);
   dwsInternalUnit.AddPostInitProc(RegisterComplexOperators);

   RegisterInternalFunction(TComplexMakeExpr, 'Complex', ['real', SYS_FLOAT, 'imaginary', SYS_FLOAT], SYS_COMPLEX);
   RegisterInternalFunction(TComplexToStrExpr, 'ComplexToStr', ['c', SYS_COMPLEX], SYS_STRING);

   RegisterInternalFunction(TComplexAddOpExpr, 'ComplexAdd', ['left', SYS_COMPLEX, 'right', SYS_COMPLEX], SYS_COMPLEX);

end.
