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
unit dwsMetrics;

{$I ../dws.inc}

interface

uses Classes, SysUtils, dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsCoreExprs,
   dwsTokenizer, dwsSymbolDictionary, dwsContextMap;

type

   // provide various metrics on compiled programs

   IdwsMetric = interface (IGetSelf)
      function GetName : UnicodeString;
      property Name : UnicodeString read GetName;

      function GetCount : Integer;
      property Count : Integer read GetCount;

      function GetValue : Double;
      property Value : Double read GetValue;
      function GetValueMin : Double;
      property ValueMin : Double read GetValueMin;
      function GetValueMax : Double;
      property ValueMax : Double read GetValueMax;
   end;

   TdwsMetric = class abstract (TInterfacedSelfObject, IdwsMetric)
      private
         FName : UnicodeString;
         FValue : Double;
         FValueMin : Double;
         FValueMax : Double;
         FCount : Integer;

      protected
         function GetValue : Double;
         function GetValueMin : Double;
         function GetValueMax : Double;
         function GetName : UnicodeString;
         function GetCount : Integer;

         property Name : UnicodeString read FName write FName;
         property Count : Integer read FCount write FCount;
         property Value : Double read FValue write FValue;
         property ValueMin : Double read FValueMin write FValueMin;
         property ValueMax : Double read FValueMax write FValueMax;

      public
         constructor Create(const aProg : IdwsProgram); virtual; abstract;
   end;

   TdwsSourceFuncSymbolMetric = class abstract (TdwsMetric)
      public
         constructor Create(const aProg : IdwsProgram); override;
         procedure Evaluate(const aProg : IdwsProgram; funcSymbol : TFuncSymbol); virtual; abstract;
   end;

   TdwsParametersCountMetric = class (TdwsSourceFuncSymbolMetric)
      public
         constructor Create(const aProg : IdwsProgram); override;
         procedure Evaluate(const aProg : IdwsProgram; funcSymbol : TFuncSymbol); override;
   end;

   TdwsProcedureLengthMetric = class (TdwsSourceFuncSymbolMetric)
      protected
         procedure ContextCallback(context : TdwsSourceContext);
      public
         constructor Create(const aProg : IdwsProgram); override;
         procedure Evaluate(const aProg : IdwsProgram; funcSymbol : TFuncSymbol); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

resourcestring
   METRIC_ParametersCount = 'Parameters Count';
   METRIC_ProcedureLength = 'Procedure Length';

// ------------------
// ------------------ TdwsMetric ------------------
// ------------------

// GetValue
//
function TdwsMetric.GetValue : Double;
begin
   Result:=FValue;
end;

// GetValueMin
//
function TdwsMetric.GetValueMin : Double;
begin
   Result:=FValueMin;
end;

// GetValueMax
//
function TdwsMetric.GetValueMax : Double;
begin
   Result:=FValueMax;
end;

// GetName
//
function TdwsMetric.GetName : UnicodeString;
begin
   Result:=FName;
end;

// GetCount
//
function TdwsMetric.GetCount : Integer;
begin
   Result:=FCount;
end;

// ------------------
// ------------------ TdwsSourceFuncSymbolMetric ------------------
// ------------------

// Create
//
constructor TdwsSourceFuncSymbolMetric.Create(const aProg : IdwsProgram);
var
   i : Integer;
   symDict : TdwsSymbolDictionary;
   sym : TSymbol;
   funcSym : TFuncSymbol;
begin
   symDict:=aProg.SymbolDictionary;
   if (symDict=nil) or (symDict.Count=0) then Exit;

   for i:=0 to symDict.Count-1 do begin
      sym:=symDict.Items[i].Symbol;
      funcSym:=sym.AsFuncSymbol;
      if funcSym=nil then continue;
      if funcSym.IsType then continue;
      if (funcSym is TSourceFuncSymbol) or (funcSym is TSourceMethodSymbol) then begin
         Evaluate(aProg, funcSym);
         Count:=Count+1;
      end;
   end;
end;

// ------------------
// ------------------ TdwsParametersCountMetric ------------------
// ------------------

// Create
//
constructor TdwsParametersCountMetric.Create(const aProg : IdwsProgram);
begin
   Name:=METRIC_ParametersCount;

   inherited Create(aProg);

   if Count>0 then
      Value:=Value/Count;
end;

// Evaluate
//
procedure TdwsParametersCountMetric.Evaluate(const aProg : IdwsProgram; funcSymbol : TFuncSymbol);
var
   n : Double;
begin
   n:=funcSymbol.Params.Count;
   if n>ValueMax then ValueMax:=n;
   Value:=Value+n;
end;

// ------------------
// ------------------ TdwsProcedureLengthMetric ------------------
// ------------------

// Create
//
constructor TdwsProcedureLengthMetric.Create(const aProg : IdwsProgram);
begin
   Name:=METRIC_ProcedureLength;

   inherited Create(aProg);

   if Count>0 then
      Value:=Value/Count;
end;

// Evaluate
//
procedure TdwsProcedureLengthMetric.Evaluate(const aProg : IdwsProgram; funcSymbol : TFuncSymbol);
var
   map : TdwsSourceContextMap;
begin
   map:=aProg.SourceContextMap;
   if map=nil then Exit;
   map.EnumerateContextsOfSymbol(funcSymbol, ContextCallback);
end;

// ContextCallback
//
procedure TdwsProcedureLengthMetric.ContextCallback(context : TdwsSourceContext);
var
   n : Integer;
begin
   if context.Token=ttBEGIN then begin
      n:=context.StartPos.Line;
      if context.EndPos.SourceFile<>nil then
         n:=1+context.EndPos.Line-n
      else n:=context.StartPos.SourceFile.LineCount-n;
      if n<1 then n:=1;
      if n>ValueMax then
         ValueMax:=n;
      if Value=0 then
         ValueMin:=n
      else if n<ValueMin then
         ValueMin:=n;
      Value:=Value+n;
   end;
end;

end.
