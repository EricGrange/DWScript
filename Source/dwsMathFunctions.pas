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
{$I dws.inc}
unit dwsMathFunctions;

interface

uses Classes, Math, dwsFunctions, dwsExprs, dwsSymbols;

type

   TOddFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(args : TExprBaseList) : Boolean; override;
   end;

   TSinFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TSinhFunc = class(TInternalMagicFloatFunction)
     procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TCosFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TCoshFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TTanFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TTanhFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TArcSinFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TArcSinhFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TArcCosFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TArcCoshFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TArcTanFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TArcTan2Func = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TArcTanhFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TCotanFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   THypotFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TAbsFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TFactorialFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TExpFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TLnFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TLog2Func = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TLog10Func = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TLogNFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TPowerFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TIntPowerFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TSqrtFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TIntFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TFloorFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TCeilFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TFracFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TTruncFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TRoundFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TDegToRadFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TRadToDegFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TMaxFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TMinFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TMaxIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TMinIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TClampIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TPiFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TRandomFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TRandomIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TRandomizeFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(args : TExprBaseList); override;
   end;

   TRandGFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
   end;

   TRandSeedFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
   end;

   TSetRandSeedFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(args : TExprBaseList); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsCoreExprs;

const // type constants
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';

{ TOddFunc }

function TOddFunc.DoEvalAsBoolean(args : TExprBaseList) : Boolean;
begin
   Result:=Odd(args.AsInteger[0]);
end;

{ TSinFunc }

procedure TSinFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Sin(args.AsFloat[0]);
end;

{ TSinhFunc }

procedure TSinhFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Sinh(args.AsFloat[0]);
end;

{ TCosFunc }

procedure TCosFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Cos(args.AsFloat[0]);
end;

{ TCoshFunc }

procedure TCoshFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Cosh(args.AsFloat[0]);
end;

{ TTanFunc }

procedure TTanFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Tan(args.AsFloat[0]);
end;

{ TTanhFunc }

procedure TTanhFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Tanh(args.AsFloat[0]);
end;

{ TArcSinFunc }

procedure TArcSinFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=ArcSin(args.AsFloat[0]);
end;

{ TArcSinhFunc }

procedure TArcSinhFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=ArcSinh(args.AsFloat[0]);
end;

{ TArcCosFunc }

procedure TArcCosFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=ArcCos(args.AsFloat[0]);
end;

{ TArcCoshFunc }

procedure TArcCoshFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=ArcCosh(args.AsFloat[0]);
end;

{ TArcTanFunc }

procedure TArcTanFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=ArcTan(args.AsFloat[0]);
end;

{ TArcTan2Func }

// DoEvalAsFloat
//
procedure TArcTan2Func.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=ArcTan2(args.AsFloat[0], args.AsFloat[1]);
end;

{ TArcTanhFunc }

procedure TArcTanhFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=ArcTanh(args.AsFloat[0]);
end;

{ TCotanFunc }

procedure TCotanFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Cotan(args.AsFloat[0]);
end;

{ THypotFunc }

procedure THypotFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Hypot(args.AsFloat[0], args.AsFloat[1]);
end;

{ TAbsFunc }

procedure TAbsFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Abs(args.AsFloat[0]);
end;

{ TFactorialFunc }

procedure TFactorialFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
var
   i : Int64;
begin
   i:=args.AsInteger[0];
   Result:=1;
   while i>1 do begin
      Result:=Result*i;
      Dec(i);
   end;
end;

{ TExpFunc }

procedure TExpFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Exp(args.AsFloat[0]);
end;

{ TLnFunc }

procedure TLnFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Ln(args.AsFloat[0]);
end;

{ TLog2Func }

procedure TLog2Func.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Log2(args.AsFloat[0]);
end;

{ TLog10Func }

procedure TLog10Func.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Log10(args.AsFloat[0]);
end;

{ TLogNFunc }

procedure TLogNFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=LogN(args.AsFloat[0], args.AsFloat[1]);
end;

{ TSqrtFunc }

procedure TSqrtFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Sqrt(args.AsFloat[0]);
end;

{ TIntFunc }

procedure TIntFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Int(args.AsFloat[0]);
end;

{ TFloorFunc }

function TFloorFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=Floor(args.AsFloat[0]);
end;

{ TCeilFunc }

function TCeilFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=Ceil(args.AsFloat[0]);
end;

{ TFracFunc }

procedure TFracFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Frac(args.AsFloat[0]);
end;

{ TTruncFunc }

function TTruncFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=Trunc(args.AsFloat[0]);
end;

{ TRoundFunc }

function TRoundFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=Round(args.AsFloat[0]);
end;

{ TPowerFunc }

procedure TPowerFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Power(args.AsFloat[0], args.AsFloat[1]);
end;

{ TIntPowerFunc }

procedure TIntPowerFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=IntPower(args.AsFloat[0], args.AsInteger[1]);
end;

{ TDegToRadFunc }

procedure TDegToRadFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=DegToRad(args.AsFloat[0]);
end;

{ TRadToDegFunc }

procedure TRadToDegFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=RadToDeg(args.AsFloat[0]);
end;

{ TMaxFunc }

procedure TMaxFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Max(args.AsFloat[0], args.AsFloat[1]);
end;

{ TMinFunc }

procedure TMinFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Min(args.AsFloat[0], args.AsFloat[1]);
end;

{ TMaxIntFunc }

function TMaxIntFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=Max(args.AsInteger[0], args.AsInteger[1]);
end;

{ TMinIntFunc }

function TMinIntFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=Min(args.AsInteger[0], args.AsInteger[1]);
end;

{ TClampIntFunc }

function TClampIntFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
var
   r : Int64;
begin
   Result:=args.AsInteger[0];
   r:=args.AsInteger[1];
   if Result<r then
      Result:=r
   else begin
      r:=args.AsInteger[2];
      if Result>r then
         Result:=r;
   end;
end;

{ TPiFunc }

procedure TPiFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=PI;
end;

{ TRandomFunc }

procedure TRandomFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=Random;
end;

{ TRandomIntFunc }

function TRandomIntFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=Random(args.AsInteger[0]);
end;

{ TRandomizeFunc }

// DoEvalProc
//
procedure TRandomizeFunc.DoEvalProc(args : TExprBaseList);
begin
   Randomize;
end;

{ TRandGFunc }

procedure TRandGFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=RandG(args.AsFloat[0], args.AsFloat[1]);
end;

{ TRandSeedFunc }

function TRandSeedFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=RandSeed;
end;

{ TSetRandSeedFunc }

procedure TSetRandSeedFunc.DoEvalProc(args : TExprBaseList);
begin
   RandSeed:=args.AsInteger[0];
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterInternalBoolFunction(TOddFunc, 'Odd', ['i', cInteger], True);

   RegisterInternalFloatFunction(TSinFunc, 'Sin', ['a', cFloat], True);
   RegisterInternalFloatFunction(TSinhFunc, 'Sinh', ['a', cFloat], True);
   RegisterInternalFloatFunction(TCosFunc, 'Cos', ['a', cFloat], True);
   RegisterInternalFloatFunction(TCoshFunc, 'Cosh', ['a', cFloat], True);
   RegisterInternalFloatFunction(TTanFunc, 'Tan', ['a', cFloat], True);
   RegisterInternalFloatFunction(TTanhFunc, 'Tanh', ['a', cFloat], True);
   RegisterInternalFloatFunction(TArcSinFunc, 'ArcSin', ['v', cFloat], True);
   RegisterInternalFloatFunction(TArcSinhFunc, 'ArcSinh', ['v', cFloat], True);
   RegisterInternalFloatFunction(TArcCosFunc, 'ArcCos', ['v', cFloat], True);
   RegisterInternalFloatFunction(TArcCoshFunc, 'ArcCosh', ['v', cFloat], True);
   RegisterInternalFloatFunction(TArcTanFunc, 'ArcTan', ['v', cFloat], True);
   RegisterInternalFloatFunction(TArcTan2Func, 'ArcTan2', ['y', cFloat, 'x', cFloat], True);
   RegisterInternalFloatFunction(TArcTanhFunc, 'ArcTanh', ['v', cFloat], True);
   RegisterInternalFloatFunction(TCotanFunc, 'Cotan', ['a', cFloat], True);
   RegisterInternalFloatFunction(THypotFunc, 'Hypot', ['x', cFloat, 'y', cFloat], True);
   RegisterInternalFloatFunction(TAbsFunc, 'Abs', ['v', cFloat], True);
   RegisterInternalFloatFunction(TFactorialFunc, 'Factorial', ['v', cInteger], True);
   RegisterInternalFloatFunction(TExpFunc, 'Exp', ['v', cFloat], True);
   RegisterInternalFloatFunction(TLnFunc, 'Ln', ['v', cFloat], True);
   RegisterInternalFloatFunction(TLog2Func, 'Log2', ['v', cFloat], True);
   RegisterInternalFloatFunction(TLog10Func, 'Log10', ['v', cFloat], True);
   RegisterInternalFloatFunction(TLogNFunc, 'LogN', ['n', cFloat, 'x', cFloat], True);
   RegisterInternalFloatFunction(TPowerFunc, 'Power', ['base', cFloat, 'exponent', cFloat], True);
   RegisterInternalFloatFunction(TIntPowerFunc, 'IntPower', ['base', cFloat, 'exponent', cInteger], True);
   RegisterInternalFloatFunction(TSqrtFunc, 'Sqrt', ['v', cFloat], True);
   RegisterInternalFloatFunction(TIntFunc, 'Int', ['v', cFloat], True);
   RegisterInternalFloatFunction(TFracFunc, 'Frac', ['v', cFloat], True);
   RegisterInternalIntFunction(TFloorFunc, 'Floor', ['v', cFloat], True);
   RegisterInternalIntFunction(TCeilFunc, 'Ceil', ['v', cFloat], True);

   RegisterInternalFunction(TTruncFunc, 'Trunc', ['v', cFloat], cInteger, True);
   RegisterInternalFunction(TRoundFunc, 'Round', ['v', cFloat], cInteger, True);

   RegisterInternalFloatFunction(TDegToRadFunc, 'DegToRad', ['a', cFloat], True);
   RegisterInternalFloatFunction(TRadToDegFunc, 'RadToDeg', ['a', cFloat], True);

   RegisterInternalFloatFunction(TMaxFunc, 'Max', ['v1', cFloat, 'v2', cFloat], True);
   RegisterInternalFloatFunction(TMinFunc, 'Min', ['v1', cFloat, 'v2', cFloat], True);

   RegisterInternalIntFunction(TMaxIntFunc, 'MaxInt', ['v1', cInteger, 'v2', cInteger], True);
   RegisterInternalIntFunction(TMinIntFunc, 'MinInt', ['v1', cInteger, 'v2', cInteger], True);
   RegisterInternalIntFunction(TClampIntFunc, 'ClampInt', ['v', cInteger, 'min', cInteger, 'max', cInteger], True);

   RegisterInternalFloatFunction(TPiFunc, 'Pi', [], True);
   RegisterInternalFloatFunction(TRandomFunc, 'Random', []);
   RegisterInternalIntFunction(TRandomIntFunc, 'RandomInt', ['range', cInteger]);
   RegisterInternalFunction(TRandomizeFunc, 'Randomize', [], '');
   RegisterInternalFloatFunction(TRandGFunc, 'RandG', ['mean', cFloat, 'stdDev', cFloat]);
   RegisterInternalIntFunction(TRandSeedFunc, 'RandSeed', []);
   RegisterInternalProcedure(TSetRandSeedFunc, 'SetRandSeed', ['seed', cInteger]);

end.
