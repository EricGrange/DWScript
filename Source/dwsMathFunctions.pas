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
unit dwsMathFunctions;

{$I dws.inc}

interface

uses
   Classes, Math,
   dwsUtils, dwsStrings,
   dwsFunctions, dwsExprs, dwsSymbols, dwsMagicExprs, dwsXPlatform, dwsExprList;

type

   TOddFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TSinFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TSinhFunc = class(TInternalMagicFloatFunction)
     procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TCosFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TCoshFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TTanFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TTanhFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TArcSinFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TArcSinhFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TArcCosFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TArcCoshFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TArcTanFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TArcTan2Func = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TArcTanhFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TCotanFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   THypotFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TFactorialFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TExpFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TLnFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TLog2Func = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TLog10Func = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TLogNFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TPowerFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TIntPowerFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TSqrIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TSqrFloatFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TSqrtFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TIntFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TFloorFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TCeilFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TFracFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TTruncFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TRoundFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TUnsigned32Func = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TDegToRadFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TRadToDegFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TSignFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TSignIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TDivModFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TMaxFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TMinFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TClampFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TMaxIntValueFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;
   TMaxIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TMinIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TClampIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TPiFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TInfinityFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TNaNFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TIsNaNFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TIsInfiniteFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TIsFiniteFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TGcdFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TLcmFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TIsPrimeFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TLeastFactorFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TRandomFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TRandomIntFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TRandomizeFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TRandGFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TRandSeedFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TSetRandSeedFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

function Gcd(a, b : Int64) : Int64;
function Lcm(const a, b : Int64) : Int64;
function LeastFactor(const n : Int64) : Int64;
function IsPrime(const n : Int64) : Boolean;
function IsFinite(const v : Double) : Boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// Gcd
//
function Gcd(a, b : Int64) : Int64;
var
   r : Int64;
begin
   while b<>0 do begin
      r:=a mod b;
      a:=b;
      b:=r;
   end;
   Result:=a;
end;

// Lcm
//
function Lcm(const a, b : Int64) : Int64;
var
   g : Int64;
begin
   g:=Gcd(a, b);
   if g<>0 then
      Result:=(a div g)*b
   else Result:=0;
end;

// LeastFactor
//
function LeastFactor(const n : Int64) : Int64;
var
   i, lim : Int64;
begin
   if n<=1 then begin
      if n=1 then
         Result:=1
      else Result:=0
   end else if (n and 1)=0 then
      Result:=2
   else if (n mod 3)=0 then
      Result:=3
   else begin
      lim:=Round(Sqrt(n));
      i:=5;
      while i<=lim do begin
         if (n mod i)=0 then Exit(i);
         Inc(i, 2);
         if (n mod i)=0 then Exit(i);
         Inc(i, 4);
      end;
      Result:=n;
   end;
end;

// IsPrime
//
function IsPrime(const n : Int64) : Boolean;
begin
   if n<=3 then
      Result:=(n>=2)
   else Result:=((n and 1)<>0) and (LeastFactor(n)=n);
end;

// IsFinite
//
function IsFinite(const v : Double) : Boolean;
begin
   Result:=not (IsNan(v) or IsInfinite(v));
end;

{ TOddFunc }

function TOddFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=Odd(args.AsInteger[0]);
end;

{ TSinFunc }

procedure TSinFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Sin(args.AsFloat[0]);
end;

{ TSinhFunc }

procedure TSinhFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Sinh(args.AsFloat[0]);
end;

{ TCosFunc }

procedure TCosFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Cos(args.AsFloat[0]);
end;

{ TCoshFunc }

procedure TCoshFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Cosh(args.AsFloat[0]);
end;

{ TTanFunc }

procedure TTanFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Tan(args.AsFloat[0]);
end;

{ TTanhFunc }

procedure TTanhFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Tanh(args.AsFloat[0]);
end;

{ TArcSinFunc }

procedure TArcSinFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=ArcSin(args.AsFloat[0]);
end;

{ TArcSinhFunc }

procedure TArcSinhFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=ArcSinh(args.AsFloat[0]);
end;

{ TArcCosFunc }

procedure TArcCosFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=ArcCos(args.AsFloat[0]);
end;

{ TArcCoshFunc }

procedure TArcCoshFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=ArcCosh(args.AsFloat[0]);
end;

{ TArcTanFunc }

procedure TArcTanFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=ArcTan(args.AsFloat[0]);
end;

{ TArcTan2Func }

procedure TArcTan2Func.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=ArcTan2(args.AsFloat[0], args.AsFloat[1]);
end;

{ TArcTanhFunc }

procedure TArcTanhFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=ArcTanh(args.AsFloat[0]);
end;

{ TCotanFunc }

procedure TCotanFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Cotan(args.AsFloat[0]);
end;

{ THypotFunc }

procedure THypotFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Hypot(args.AsFloat[0], args.AsFloat[1]);
end;

{ TFactorialFunc }

procedure TFactorialFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
const
   cFactTable : array [2..12] of Integer =
      (2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600 );
var
   i : Integer;
begin
   i:=args.AsInteger[0];
   if i<=1 then
      Result:=1
   else if i<=High(cFactTable) then
      Result:=cFactTable[i]
   else begin
      Result:=cFactTable[High(cFactTable)];
      while i>High(cFactTable) do begin
         Result:=Result*i;
         Dec(i);
      end;
   end;
end;

{ TExpFunc }

procedure TExpFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Exp(args.AsFloat[0]);
end;

{ TLnFunc }

procedure TLnFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Ln(args.AsFloat[0]);
end;

{ TLog2Func }

procedure TLog2Func.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Log2(args.AsFloat[0]);
end;

{ TLog10Func }

procedure TLog10Func.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Log10(args.AsFloat[0]);
end;

{ TLogNFunc }

procedure TLogNFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=LogN(args.AsFloat[0], args.AsFloat[1]);
end;

{ TSqrIntFunc }

function TSqrIntFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Sqr(args.AsInteger[0]);
end;

{ TSqrFloatFunc }

procedure TSqrFloatFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Sqr(args.AsFloat[0]);
end;

{ TSqrtFunc }

procedure TSqrtFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Sqrt(args.AsFloat[0]);
end;

{ TIntFunc }

procedure TIntFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Int(args.AsFloat[0]);
end;

{ TFloorFunc }

function TFloorFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Floor(args.AsFloat[0]);
end;

{ TCeilFunc }

function TCeilFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Ceil(args.AsFloat[0]);
end;

{ TFracFunc }

procedure TFracFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Frac(args.AsFloat[0]);
end;

{ TTruncFunc }

function TTruncFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Trunc(args.AsFloat[0]);
end;

{ TRoundFunc }

function TRoundFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Round(args.AsFloat[0]);
end;

{ TUnsignedFunc }

function TUnsigned32Func.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Cardinal(args.AsInteger[0]);
end;

{ TPowerFunc }

procedure TPowerFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Power(args.AsFloat[0], args.AsFloat[1]);
end;

{ TIntPowerFunc }

procedure TIntPowerFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=IntPower(args.AsFloat[0], args.AsInteger[1]);
end;

{ TDegToRadFunc }

procedure TDegToRadFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=DegToRad(args.AsFloat[0]);
end;

{ TRadToDegFunc }

procedure TRadToDegFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=RadToDeg(args.AsFloat[0]);
end;

{ TSignFunc }

function TSignFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Sign(args.AsFloat[0]);
end;

{ TSignIntFunc }

function TSignIntFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Sign(args.AsInteger[0]);
end;

{ TDivModFunc }

procedure TDivModFunc.DoEvalProc(const args : TExprBaseListExec);
var
   dividend, divisor, result : Int64;
begin
   dividend:=args.AsInteger[0];
   divisor:=args.AsInteger[1];
   result:=dividend div divisor;
   args.AsInteger[2]:=result;
   args.AsInteger[3]:=dividend-result*divisor;
end;

{ TMaxFunc }

procedure TMaxFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Max(args.AsFloat[0], args.AsFloat[1]);
end;

{ TMinFunc }

procedure TMinFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Math.Min(args.AsFloat[0], args.AsFloat[1]);
end;

{ TClampFunc }

procedure TClampFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   r : Double;
begin
   Result:=args.AsFloat[0];
   r:=args.AsFloat[1];
   if Result<r then
      Result:=r
   else begin
      r:=args.AsFloat[2];
      if Result>r then
         Result:=r;
   end;
end;

{ TMaxIntBalueFunc }

function TMaxIntValueFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=MaxInt;
end;

{ TMaxIntFunc }

function TMaxIntFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Max(args.AsInteger[0], args.AsInteger[1]);
end;

{ TMinIntFunc }

function TMinIntFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Min(args.AsInteger[0], args.AsInteger[1]);
end;

{ TClampIntFunc }

function TClampIntFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
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

procedure TPiFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=PI;
end;

{ TInfinityFunc }

procedure TInfinityFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=Infinity;
end;

{ TNaNFunc }

procedure TNaNFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=NaN;
end;

{ IsNaNFunc }

function TIsNaNFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=IsNan(args.AsFloat[0]);
end;

{ IsInfinite }

function TIsInfiniteFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=IsInfinite(args.AsFloat[0]);
end;

{ IsFinite }

function TIsFiniteFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=IsFinite(args.AsFloat[0]);
end;

{ TGcdFunc }

function TGcdFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Gcd(args.AsInteger[0], args.AsInteger[1]);
end;

{ TLcmFunc }

function TLcmFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Lcm(args.AsInteger[0], args.AsInteger[1]);
end;

{ TIsPrimeFunc }

function TIsPrimeFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result:=IsPrime(args.AsInteger[0]);
end;

{ TLeastFactorFunc }

function TLeastFactorFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=LeastFactor(args.AsInteger[0]);
end;

{ TRandomFunc }

procedure TRandomFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result:=args.Exec.Random;
end;

{ TRandomIntFunc }

function TRandomIntFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Trunc(args.Exec.Random*args.AsInteger[0]);
end;

{ TRandomizeFunc }

{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ELSE}
  {$UNDEF RANGEON}
{$ENDIF}
var
   vSeedBase : UInt64;
procedure TRandomizeFunc.DoEvalProc(const args : TExprBaseListExec);
var
   x : UInt64;
begin
   x:=GetSystemMilliseconds xor vSeedBase;
   x:=(x shl 40) xor x;
   vSeedBase:=x;
   args.Exec.RandSeed:=x;
end;
{$IFDEF RANGEON}
  {$R+}
{$ENDIF}

{ TRandGFunc }

procedure TRandGFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   x, y, n : Double;
begin
   // Marsaglia-Bray
   repeat
      x:=2*args.Exec.Random-1;
      y:=2*args.Exec.Random-1;
      n:=sqr(x)+sqr(y);
   until n<1;
   Result:=Sqrt(-2*Ln(n)/n)*x*args.AsFloat[1]+args.AsFloat[0];
end;

{ TRandSeedFunc }

function TRandSeedFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result:=Int64(args.Exec.RandSeed) xor cDefaultRandSeed;
end;

{ TSetRandSeedFunc }

procedure TSetRandSeedFunc.DoEvalProc(const args : TExprBaseListExec);
begin
   args.Exec.RandSeed:=args.AsInteger[0] xor cDefaultRandSeed;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterInternalBoolFunction(TOddFunc, 'Odd', ['i', SYS_INTEGER], [iffStateLess, iffOverloaded], 'IsOdd');

   RegisterInternalFloatFunction(TSinFunc, 'Sin', ['a', SYS_FLOAT], [iffStateLess], 'Sin');
   RegisterInternalFloatFunction(TSinhFunc, 'Sinh', ['a', SYS_FLOAT], [iffStateLess], 'Sinh');
   RegisterInternalFloatFunction(TCosFunc, 'Cos', ['a', SYS_FLOAT], [iffStateLess], 'Cos');
   RegisterInternalFloatFunction(TCoshFunc, 'Cosh', ['a', SYS_FLOAT], [iffStateLess], 'Cosh');
   RegisterInternalFloatFunction(TTanFunc, 'Tan', ['a', SYS_FLOAT], [iffStateLess], 'Tan');
   RegisterInternalFloatFunction(TTanhFunc, 'Tanh', ['a', SYS_FLOAT], [iffStateLess], 'Tanh');
   RegisterInternalFloatFunction(TArcSinFunc, 'ArcSin', ['v', SYS_FLOAT], [iffStateLess], 'ArcSin');
   RegisterInternalFloatFunction(TArcSinhFunc, 'ArcSinh', ['v', SYS_FLOAT], [iffStateLess], 'ArcSinh');
   RegisterInternalFloatFunction(TArcCosFunc, 'ArcCos', ['v', SYS_FLOAT], [iffStateLess], 'ArcCos');
   RegisterInternalFloatFunction(TArcCoshFunc, 'ArcCosh', ['v', SYS_FLOAT], [iffStateLess], 'ArcCosh');
   RegisterInternalFloatFunction(TArcTanFunc, 'ArcTan', ['v', SYS_FLOAT], [iffStateLess], 'ArcTan');
   RegisterInternalFloatFunction(TArcTan2Func, 'ArcTan2', ['y', SYS_FLOAT, 'x', SYS_FLOAT], [iffStateLess]);
   RegisterInternalFloatFunction(TArcTanhFunc, 'ArcTanh', ['v', SYS_FLOAT], [iffStateLess], 'ArcTanh');
   RegisterInternalFloatFunction(TCotanFunc, 'Cotan', ['a', SYS_FLOAT], [iffStateLess], 'Cotan');
   RegisterInternalFloatFunction(THypotFunc, 'Hypot', ['x', SYS_FLOAT, 'y', SYS_FLOAT], [iffStateLess]);
   RegisterInternalFloatFunction(TFactorialFunc, 'Factorial', ['v', SYS_INTEGER], [iffStateLess], 'Factorial');
   RegisterInternalFloatFunction(TExpFunc, 'Exp', ['v', SYS_FLOAT], [iffStateLess], 'Exp');
   RegisterInternalFloatFunction(TLnFunc, 'Ln', ['v', SYS_FLOAT], [iffStateLess], 'Ln');
   RegisterInternalFloatFunction(TLog2Func, 'Log2', ['v', SYS_FLOAT], [iffStateLess], 'Log2');
   RegisterInternalFloatFunction(TLog10Func, 'Log10', ['v', SYS_FLOAT], [iffStateLess], 'Log10');
   RegisterInternalFloatFunction(TLogNFunc, 'LogN', ['n', SYS_FLOAT, 'x', SYS_FLOAT], [iffStateLess], 'LogN');
   RegisterInternalFloatFunction(TPowerFunc, 'Power', ['base', SYS_FLOAT, 'exponent', SYS_FLOAT], [iffStateLess], 'Power');
   RegisterInternalFloatFunction(TIntPowerFunc, 'IntPower', ['base', SYS_FLOAT, 'exponent', SYS_INTEGER], [iffStateLess, iffOverloaded], 'Power');
   RegisterInternalIntFunction(TSqrIntFunc, 'Sqr', ['v', SYS_INTEGER], [iffStateLess, iffOverloaded], 'Sqr');
   RegisterInternalFloatFunction(TSqrFloatFunc, 'Sqr', ['v', SYS_FLOAT], [iffStateLess, iffOverloaded], 'Sqr');
   RegisterInternalFloatFunction(TSqrtFunc, 'Sqrt', ['v', SYS_FLOAT], [iffStateLess], 'Sqrt');
   RegisterInternalFloatFunction(TIntFunc, 'Int', ['v', SYS_FLOAT], [iffStateLess], 'Int');
   RegisterInternalFloatFunction(TFracFunc, 'Frac', ['v', SYS_FLOAT], [iffStateLess], 'Fraction');
   RegisterInternalIntFunction(TFloorFunc, 'Floor', ['v', SYS_FLOAT], [iffStateLess], 'Floor');
   RegisterInternalIntFunction(TCeilFunc, 'Ceil', ['v', SYS_FLOAT], [iffStateLess], 'Ceiling');

   RegisterInternalIntFunction(TTruncFunc, 'Trunc', ['v', SYS_FLOAT], [iffStateLess], 'Truncate');
   RegisterInternalIntFunction(TRoundFunc, 'Round', ['v', SYS_FLOAT], [iffStateLess], 'Round');

   RegisterInternalIntFunction(TUnsigned32Func, 'Unsigned32', ['v', SYS_INTEGER], [iffStateLess], 'Unsigned32');

   RegisterInternalFloatFunction(TDegToRadFunc, 'DegToRad', ['a', SYS_FLOAT], [iffStateLess], 'DegToRad');
   RegisterInternalFloatFunction(TRadToDegFunc, 'RadToDeg', ['a', SYS_FLOAT], [iffStateLess], 'RadToDeg');

   RegisterInternalIntFunction(TSignFunc, 'Sign', ['v', SYS_FLOAT], [iffStateLess, iffOverloaded], 'Sign');
   RegisterInternalIntFunction(TSignIntFunc, 'Sign', ['v', SYS_INTEGER], [iffStateLess, iffOverloaded], 'Sign');

   RegisterInternalProcedure(TDivModFunc, 'DivMod',
                             ['dividend', SYS_INTEGER, 'divisor', SYS_INTEGER,
                              '@result', SYS_INTEGER, '@remainder', SYS_INTEGER], '', [iffOverloaded]);

   RegisterInternalFloatFunction(TMaxFunc, 'Max', ['v1', SYS_FLOAT, 'v2', SYS_FLOAT], [iffStateLess, iffOverloaded], 'Max');
   RegisterInternalIntFunction(TMaxIntFunc, 'Max', ['v1', SYS_INTEGER, 'v2', SYS_INTEGER], [iffStateLess, iffOverloaded], 'Max');
   RegisterInternalFloatFunction(TMinFunc, 'Min', ['v1', SYS_FLOAT, 'v2', SYS_FLOAT], [iffStateLess, iffOverloaded], 'Min');
   RegisterInternalIntFunction(TMinIntFunc, 'Min', ['v1', SYS_INTEGER, 'v2', SYS_INTEGER], [iffStateLess, iffOverloaded], 'Min');
   RegisterInternalFloatFunction(TClampFunc, 'Clamp', ['v', SYS_FLOAT, 'min', SYS_FLOAT, 'max', SYS_FLOAT], [iffStateLess], 'Clamp');

   RegisterInternalIntFunction(TMaxIntValueFunc, 'MaxInt', [], [iffStateLess, iffOverloaded]);
   RegisterInternalIntFunction(TMaxIntFunc, 'MaxInt', ['v1', SYS_INTEGER, 'v2', SYS_INTEGER], [iffStateLess, iffOverloaded]);
   RegisterInternalIntFunction(TMinIntFunc, 'MinInt', ['v1', SYS_INTEGER, 'v2', SYS_INTEGER], [iffStateLess]);
   RegisterInternalIntFunction(TClampIntFunc, 'ClampInt', ['v', SYS_INTEGER, 'min', SYS_INTEGER, 'max', SYS_INTEGER], [iffStateLess], 'Clamp');

   RegisterInternalFloatFunction(TPiFunc, 'Pi', [], [iffStateLess]);
   RegisterInternalFloatFunction(TInfinityFunc, 'Infinity', [], [iffStateLess]);
   RegisterInternalFloatFunction(TNaNFunc, 'NaN', [], [iffStateLess]);

   RegisterInternalBoolFunction(TIsNaNFunc, 'IsNaN', ['v', SYS_FLOAT], [iffStateLess], 'IsNaN');
   RegisterInternalBoolFunction(TIsInfiniteFunc, 'IsInfinite', ['v', SYS_FLOAT], [iffStateLess], 'IsInfinite');
   RegisterInternalBoolFunction(TIsFiniteFunc, 'IsFinite', ['v', SYS_FLOAT], [iffStateLess], 'IsFinite');

   RegisterInternalIntFunction(TGcdFunc, 'Gcd', ['a', SYS_INTEGER, 'b', SYS_INTEGER], [iffStateLess, iffOverloaded]);
   RegisterInternalIntFunction(TLcmFunc, 'Lcm', ['a', SYS_INTEGER, 'b', SYS_INTEGER], [iffStateLess, iffOverloaded]);
   RegisterInternalBoolFunction(TIsPrimeFunc, 'IsPrime', ['n', SYS_INTEGER], [iffStateLess, iffOverloaded], 'IsPrime');
   RegisterInternalIntFunction(TLeastFactorFunc, 'LeastFactor', ['n', SYS_INTEGER], [iffStateLess], 'LeastFactor');

   RegisterInternalFloatFunction(TRandomFunc, 'Random', []);
   RegisterInternalIntFunction(TRandomIntFunc, 'RandomInt', ['range', SYS_INTEGER]);
   RegisterInternalFunction(TRandomizeFunc, 'Randomize', [], '');
   RegisterInternalFloatFunction(TRandGFunc, 'RandG', ['mean', SYS_FLOAT, 'stdDev', SYS_FLOAT]);
   RegisterInternalIntFunction(TRandSeedFunc, 'RandSeed', [], [iffDeprecated]);
   RegisterInternalProcedure(TSetRandSeedFunc, 'SetRandSeed', ['seed', SYS_INTEGER]);

end.
