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
unit dwsBigIntegerFunctions.GMP;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatform, dwsUtils, dwsStrings,
   dwsFunctions, dwsSymbols, dwsExprs, dwsCoreExprs, dwsExprList, dwsUnitSymbols,
   dwsConstExprs, dwsMagicExprs, dwsDataContext, dwsErrors, dwsRelExprs,
   dwsOperators, dwsTokenizer, dwsCryptoXPlatform,
   dwsMPIR;

const
   SYS_BIGINTEGER = 'BigInteger';

type

   TBaseBigIntegerSymbol = class (TBaseSymbol)
      public
         constructor Create;

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;
   end;

   IdwsBigInteger = interface
      ['{93A7FA32-DE99-44AB-A5B4-861FD50E9AAB}']
      function GetValue : pmpz_t;
      procedure SetValue(const v : pmpz_t);
      property Value : pmpz_t read GetValue write SetValue;

      function BitLength : Integer;

      function ToStringBase(base : Integer) : String;
      function ToHexString : String;

      function ToInt64 : Int64;
   end;

   TBigIntegerWrapper = class (TInterfacedObject, IdwsBigInteger, IGetSelf)
      protected
         function GetValue : pmpz_t; inline;
         procedure SetValue(const v : pmpz_t); inline;
         function GetSelf : TObject;

      public
         Value : mpz_t;

         constructor CreateZero;
         constructor CreateInt64(const i : Int64);
         constructor CreateString(const s : String; base : Integer);
         constructor Wrap(const v : mpz_t);
         destructor Destroy; override;

         function BitLength : Integer;

         function ToStringBase(base : Integer) : String;
         function ToHexString : String;
         function ToString : String; override;

         function ToInt64 : Int64;
   end;

   TBigIntegerOpExpr = class(TBinaryOpExpr)
      constructor Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr); override;
   end;

   TBigIntegerAddOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerSubOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerMultOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerDivOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerModOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TBigIntegerAndOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerOrOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerXorOpExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TBigIntegerShiftLeftExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TBigIntegerShiftRightExpr = class(TBigIntegerOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TBigIntegerOpAssignExpr = class(TOpAssignExpr)
     procedure TypeCheckAssign(prog : TdwsProgram; exec : TdwsExecution); override;
   end;

   TBigIntegerPlusAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBigIntegerMinusAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBigIntegerMultAssignExpr = class(TBigIntegerOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TBigIntegerRelOpExpr = class(TBoolRelOpExpr)
      protected
        function InternalCompare(exec : TdwsExecution) : Integer;
   end;
   TBigIntegerRelOpExprClass = class of TBigIntegerRelOpExpr;

   TBigIntegerEqualOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerNotEqualOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerGreaterOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerGreaterEqualOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerLessOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TBigIntegerLessEqualOpExpr = class(TBigIntegerRelOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TBigIntegerUnaryOpExpr = class (TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;
   end;

   TConvIntegerToBigIntegerExpr = class(TBigIntegerUnaryOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TConvStringToBigIntegerExpr = class(TBigIntegerUnaryOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;
   TConvBigIntegerToIntegerExpr = class(TUnaryOpIntExpr)
      function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TConvBigIntegerToFloatExpr = class(TUnaryOpFloatExpr)
      function  EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   TBigIntegerToStringFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;
   TStringToBigIntegerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;
   TBigIntegerToHexFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;
   THexToBigIntegerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerToBlobFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;
   TBlobToBigIntegerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerToFloatFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;
   TBigIntegerToIntegerFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerOddFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;
   TBigIntegerEvenFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TBigIntegerSignFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerBitLengthFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TBigIntegerAbsExpr = class(TBigIntegerUnaryOpExpr)
      public
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
   end;

   TBigIntegerGcdFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerLcmFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerIsPrimeFunc = class(TInternalMagicBoolFunction)
      function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TBigIntegerPowerFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerSqrFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerModPowFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerDivModFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TBigIntegerFactorialFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TBigIntegerRandomFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RegisterBigIntegerType                                      
//
procedure RegisterBigIntegerType(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                 unitTable : TSymbolTable);
var
   typBigInteger : TBaseBigIntegerSymbol;
begin
   if systemTable.FindLocal(SYS_BIGINTEGER)<>nil then exit;

   typBigInteger:=TBaseBigIntegerSymbol.Create;

   systemTable.AddSymbol(typBigInteger);
end;

// RegisterBigIntegerOperators
//
procedure RegisterBigIntegerOperators(systemTable : TSystemSymbolTable;
                                  unitTable : TSymbolTable; operators : TOperators);
var
   typBigInteger : TBaseBigIntegerSymbol;

   procedure RegisterOperators(token : TTokenType; exprClass : TBinaryOpExprClass);
   begin
      operators.RegisterOperator(token, exprClass, typBigInteger, typBigInteger);
      operators.RegisterOperator(token, exprClass, systemTable.TypInteger, typBigInteger);
      operators.RegisterOperator(token, exprClass, typBigInteger, systemTable.TypInteger);
   end;

begin
   typBigInteger:=systemTable.FindTypeSymbol(SYS_BIGINTEGER, cvMagic) as TBaseBigIntegerSymbol;

   if operators.FindCaster(typBigInteger, systemTable.TypInteger) <> nil then Exit;

   RegisterOperators(ttPLUS,     TBigIntegerAddOpExpr);
   RegisterOperators(ttMINUS,    TBigIntegerSubOpExpr);
   RegisterOperators(ttTIMES,    TBigIntegerMultOpExpr);
   RegisterOperators(ttDIV,      TBigIntegerDivOpExpr);
   RegisterOperators(ttMOD,      TBigIntegerModOpExpr);
   RegisterOperators(ttAND,      TBigIntegerAndOpExpr);
   RegisterOperators(ttOR,       TBigIntegerOrOpExpr);
   RegisterOperators(ttXOR,      TBigIntegerXorOpExpr);

   operators.RegisterOperator(ttSHL, TBigIntegerShiftLeftExpr,   typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttSAR, TBigIntegerShiftRightExpr,  typBigInteger, systemTable.TypInteger);

   operators.RegisterOperator(ttPLUS_ASSIGN,  TBigIntegerPlusAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttPLUS_ASSIGN,  TBigIntegerPlusAssignExpr, typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttMINUS_ASSIGN, TBigIntegerMinusAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttMINUS_ASSIGN, TBigIntegerMinusAssignExpr, typBigInteger, systemTable.TypInteger);
   operators.RegisterOperator(ttTIMES_ASSIGN, TBigIntegerMultAssignExpr, typBigInteger, typBigInteger);
   operators.RegisterOperator(ttTIMES_ASSIGN, TBigIntegerMultAssignExpr, typBigInteger, systemTable.TypInteger);

   RegisterOperators(ttEQ,       TBigIntegerEqualOpExpr);
   RegisterOperators(ttNOTEQ,    TBigIntegerNotEqualOpExpr);
   RegisterOperators(ttGTR,      TBigIntegerGreaterOpExpr);
   RegisterOperators(ttGTREQ,    TBigIntegerGreaterEqualOpExpr);
   RegisterOperators(ttLESS,     TBigIntegerLessOpExpr);
   RegisterOperators(ttLESSEQ,   TBigIntegerLessEqualOpExpr);

   operators.RegisterCaster(typBigInteger, systemTable.TypInteger, TConvIntegerToBigIntegerExpr);
   operators.RegisterCaster(typBigInteger, systemTable.TypString,  TConvStringToBigIntegerExpr);
   operators.RegisterCaster(systemTable.TypInteger, typBigInteger, TConvBigIntegerToIntegerExpr);
   operators.RegisterCaster(systemTable.TypFloat, typBigInteger,   TConvBigIntegerToFloatExpr);
end;

// HandleBigIntegerAbs
//
function HandleBigIntegerAbs(prog : TdwsProgram; argExpr : TTypedExpr) : TTypedExpr;
begin
   if argExpr.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol) then
      Result:=TBigIntegerAbsExpr.Create(prog, argExpr)
   else Result:=nil;
end;

type
   TTypedExprBigIntegerHelper = class helper for TTypedExpr
      function EvalAsBigInteger(exec : TdwsExecution) : IdwsBigInteger;
   end;

function TTypedExprBigIntegerHelper.EvalAsBigInteger(exec : TdwsExecution) : IdwsBigInteger;
var
   v : Variant;
begin
   if Typ.UnAliasedType.ClassType = TBaseBigIntegerSymbol then begin
      EvalAsVariant(exec, v);
      Assert(TVarData(v).VType=varUnknown);
      if TVarData(v).VUnknown<>nil then
         Result := IdwsBigInteger(TVarData(v).VUnknown)
      else Result := TBigIntegerWrapper.CreateZero;
   end else Result := TBigIntegerWrapper.CreateInt64( EvalAsInteger(exec) );
end;

// ArgBigInteger
//
function ArgBigInteger(const args : TExprBaseListExec; index : Integer) : IdwsBigInteger;
begin
   Result := (args.ExprBase[index] as TTypedExpr).EvalAsBigInteger(args.Exec);
end;

// BigIntegerWrap
//
function BigIntegerWrap(const bi : mpz_t) : IInterface;
begin
   Result := TBigIntegerWrapper.Wrap(bi) as IdwsBigInteger;
end;

// ------------------
// ------------------ TBaseBigIntegerSymbol ------------------
// ------------------

// Create
//
constructor TBaseBigIntegerSymbol.Create;
begin
   inherited Create(SYS_BIGINTEGER);
end;

// IsCompatible
//
function TBaseBigIntegerSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym<>nil) and (typSym.UnAliasedType.ClassType=TBaseBigIntegerSymbol);
end;

// InitData
//
procedure TBaseBigIntegerSymbol.InitData(const data : TData; offset : Integer);
begin
   data[offset] := IUnknown(nil);
end;

// ------------------
// ------------------ TBigIntegerWrapper ------------------
// ------------------

// CreateZero
//
constructor TBigIntegerWrapper.CreateZero;
begin
   inherited Create;
   if not Bind_MPIR_DLL then
      raise Exception.Create('mpir.dll is required for BigInteger');

   mpz_init(Value);
end;

// CreateInt64
//
constructor TBigIntegerWrapper.CreateInt64(const i : Int64);
begin
   CreateZero;
   mpz_set_int64(Value, i);
end;

// CreateString
//
constructor TBigIntegerWrapper.CreateString(const s : String; base : Integer);
var
   buf : RawByteString;
begin
   CreateZero;
   if s <> '' then begin
      ScriptStringToRawByteString(s, buf);
      mpz_set_str(Value, Pointer(buf), base);
   end;
end;

// Wrap
//
constructor TBigIntegerWrapper.Wrap(const v : mpz_t);
begin
   CreateZero;
   mpz_set(Value, v);
end;

// Destroy
//
destructor TBigIntegerWrapper.Destroy;
begin
   if Value.mp_alloc <> 0 then
      mpz_clear(Value);
   inherited;
end;

// GetValue
//
function TBigIntegerWrapper.GetValue : pmpz_t;
begin
   Result := @Value;
end;

// SetValue
//
procedure TBigIntegerWrapper.SetValue(const v : pmpz_t);
begin
   mpz_set(Value, v^);
end;

// GetSelf
//
function TBigIntegerWrapper.GetSelf : TObject;
begin
   Result := Self;
end;

// BitLength
//
function TBigIntegerWrapper.BitLength : Integer;
begin
   if Value.mp_size = 0 then
      Result := 0
   else Result := mpz_sizeinbase(Value, 2);
end;

// ToStringBase
//
function TBigIntegerWrapper.ToStringBase(base : Integer) : String;
var
   size : Integer;
   buf : RawByteString;
begin
   Assert(base in [2..62]);

   if Value.mp_size = 0 then Exit('0');

   size := mpz_sizeinbase(Value, base);
   Assert(size > 0);
   if Value.mp_size < 0 then
      Inc(size);
   SetLength(buf, size);
   mpz_get_str(Pointer(buf), base, Value);
   if (size > 1) and (buf[size] = #0) then
      SetLength(buf, size-1); // clear occasional trailing #0
   Result := RawByteStringToScriptString(buf);
end;

// ToHexString
//
function TBigIntegerWrapper.ToHexString : String;
begin
   Result := ToStringBase(16);
end;

// ToString
//
function TBigIntegerWrapper.ToString : String;
begin
   Result := ToStringBase(10);
end;

// ToInt64
//
function TBigIntegerWrapper.ToInt64 : Int64;
var
   n : Integer;
begin
   Result := 0;

   n := Abs(Value.mp_size);
   if n > 2 then n := 3;
   System.Move(Value.mp_d^, Result, n*4);

   if Value.mp_size < 0 then begin
      Result := -Result;
   end;
end;

// ------------------
// ------------------ TBigIntegerOpExpr ------------------
// ------------------

// Create
//
constructor TBigIntegerOpExpr.Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr);
begin
   inherited;
   Bind_MPIR_DLL;
   if aLeft.Typ.UnAliasedTypeIs(TBaseIntegerSymbol) then
      Typ := aRight.Typ
   else Typ := aLeft.Typ;
end;

// ------------------
// ------------------ TBigIntegerAddOpExpr ------------------
// ------------------

procedure TBigIntegerAddOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_add(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerSubOpExpr ------------------
// ------------------

procedure TBigIntegerSubOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_sub(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerMultOpExpr ------------------
// ------------------

procedure TBigIntegerMultOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_mul(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerDivOpExpr ------------------
// ------------------

procedure TBigIntegerDivOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_divexact(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerModOpExpr ------------------
// ------------------

procedure TBigIntegerModOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_mod(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerAndOpExpr ------------------
// ------------------

procedure TBigIntegerAndOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_and(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerOrOpExpr ------------------
// ------------------

procedure TBigIntegerOrOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_ior(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerXorOpExpr ------------------
// ------------------

procedure TBigIntegerXorOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_xor(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerRelOpExpr ------------------
// ------------------

// InternalCompare
//
function TBigIntegerRelOpExpr.InternalCompare(exec : TdwsExecution) : Integer;
begin
   Result := mpz_cmp(Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
end;

// ------------------
// ------------------ TBigIntegerEqualOpExpr ------------------
// ------------------

function TBigIntegerEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) = 0;
end;

// ------------------
// ------------------ TBigIntegerNotEqualOpExpr ------------------
// ------------------

function TBigIntegerNotEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) <> 0;
end;

// ------------------
// ------------------ TBigIntegerGreaterOpExpr ------------------
// ------------------

function TBigIntegerGreaterOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) > 0;
end;

// ------------------
// ------------------ TBigIntegerGreaterEqualOpExpr ------------------
// ------------------

function TBigIntegerGreaterEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) >= 0;
end;

// ------------------
// ------------------ TBigIntegerLessOpExpr ------------------
// ------------------

function TBigIntegerLessOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) < 0;
end;

// ------------------
// ------------------ TBigIntegerLessEqualOpExpr ------------------
// ------------------

function TBigIntegerLessEqualOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := InternalCompare(exec) <= 0;
end;

// ------------------
// ------------------ TBigIntegerUnaryOpExpr ------------------
// ------------------

constructor TBigIntegerUnaryOpExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
begin
   inherited Create(prog, expr);
   Typ := prog.Root.SystemTable.SymbolTable.FindTypeSymbol(SYS_BIGINTEGER, cvMagic);
end;

// ------------------
// ------------------ TConvIntegerToBigIntegerExpr ------------------
// ------------------

procedure TConvIntegerToBigIntegerExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := TBigIntegerWrapper.CreateInt64( Expr.EvalAsInteger(exec) ) as IdwsBigInteger;
end;

// ------------------
// ------------------ TConvStringToBigIntegerExpr ------------------
// ------------------

procedure TConvStringToBigIntegerExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   s : String;
begin
   Expr.EvalAsString(exec, s);
   result := TBigIntegerWrapper.CreateString( s, 10 ) as IdwsBigInteger;
end;

// ------------------
// ------------------ TConvBigIntegerToIntegerExpr ------------------
// ------------------

function TConvBigIntegerToIntegerExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := Expr.EvalAsBigInteger(exec).ToInt64;
end;

// ------------------
// ------------------ TConvBigIntegerToFloatExpr ------------------
// ------------------

function TConvBigIntegerToFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result := mpz_get_d(Expr.EvalAsBigInteger(exec).Value^);
end;

// ------------------
// ------------------ TBigIntegerToStringFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TBigIntegerToStringFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result := ArgBigInteger(args, 0).ToStringBase(args.AsInteger[1]);
end;

// ------------------
// ------------------ TStringToBigIntegerFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TStringToBigIntegerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := TBigIntegerWrapper.CreateString( args.AsString[0], args.AsInteger[1] ) as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerToHexFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TBigIntegerToHexFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Result := ArgBigInteger(args, 0).ToStringBase(16);
end;

// ------------------
// ------------------ THexToBigIntegerFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure THexToBigIntegerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   result := TBigIntegerWrapper.CreateString( args.AsString[0], 16 ) as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerToFloatFunc ------------------
// ------------------

procedure TBigIntegerToFloatFunc.DoEvalAsFloat(const args : TExprBaseListExec; var result : Double);
begin
   result := mpz_get_d(ArgBigInteger(args, 0).Value^);
end;

// ------------------
// ------------------ TBigIntegerToIntegerFunc ------------------
// ------------------

function TBigIntegerToIntegerFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).ToInt64;
end;

// ------------------
// ------------------ TBigIntegerOddFunc ------------------
// ------------------

function TBigIntegerOddFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := mpz_odd_p(ArgBigInteger(args, 0).Value^);
end;

// ------------------
// ------------------ TBigIntegerEvenFunc ------------------
// ------------------

function TBigIntegerEvenFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := mpz_even_p(ArgBigInteger(args, 0).Value^);
end;

// ------------------
// ------------------ TBigIntegerSignFunc ------------------
// ------------------

function TBigIntegerSignFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := mpz_sgn(ArgBigInteger(args, 0).Value^);
end;

// ------------------
// ------------------ TBigIntegerAbsExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TBigIntegerAbsExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   bi.SetValue(Expr.EvalAsBigInteger(exec).Value);
   bi.Value.mp_size := Abs(bi.Value.mp_size);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerGcdFunc ------------------
// ------------------

procedure TBigIntegerGcdFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_gcd(bi.Value, ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerLcmFunc ------------------
// ------------------

procedure TBigIntegerLcmFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_lcm(bi.Value, ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerIsPrimeFunc ------------------
// ------------------

// DoEvalAsBoolean
//
function TBigIntegerIsPrimeFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
var
   state : gmp_randstate_t;
begin
   gmp_randinit_mt(state);
   Result := mpz_probable_prime_p(ArgBigInteger(args, 0).Value^, state, args.AsInteger[1], 0) > 0;
   gmp_randclear(state);
end;

// ------------------
// ------------------ TBigIntegerPowerFunc ------------------
// ------------------

procedure TBigIntegerPowerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_pow_ui(bi.Value, ArgBigInteger(args, 0).Value^, args.AsInteger[1]);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerSqrFunc ------------------
// ------------------

procedure TBigIntegerSqrFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_pow_ui(bi.Value, ArgBigInteger(args, 0).Value^, 2);
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerDivModFunc ------------------
// ------------------

procedure TBigIntegerDivModFunc.DoEvalProc(const args : TExprBaseListExec);
var
   biQ, biR : TBigIntegerWrapper;
begin
   biQ := TBigIntegerWrapper.CreateZero;
   biR := TBigIntegerWrapper.CreateZero;

   mpz_tdiv_qr(biQ.Value, biR.Value, ArgBigInteger(args, 0).Value^, ArgBigInteger(args, 1).Value^);

   args.ExprBase[2].AssignValue(args.Exec, biQ as IdwsBigInteger);
   args.ExprBase[3].AssignValue(args.Exec, biR as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerToBlobFunc ------------------
// ------------------

procedure TBigIntegerToBlobFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bufString : RawByteString;
   pDest : PInteger;
   n : Integer;
   gmp : pmpz_t;
begin
   gmp := ArgBigInteger(args, 0).Value;
   n := Abs(gmp.mp_size);
   if n = 0 then
      bufString := ''
   else begin
      SetLength(bufString, n*4+4);
      pDest := Pointer(bufString);
      pDest^ := n;
      if n > 0 then begin
         Inc(pDest);
         System.Move(gmp.mp_d^, pDest^, n*4);
      end;
   end;
   Result := bufString;
end;

// ------------------
// ------------------ TBlobToBigIntegerFunc ------------------
// ------------------

procedure TBlobToBigIntegerFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bufString : RawByteString;
   pSrc : PInteger;
   n : Integer;
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;

   bufString := args.AsDataString[0];
   if bufString <> '' then begin

      Assert( (Length(bufString) and 3) = 0 );
      pSrc := Pointer(bufString);
      if pSrc^ <> 0 then begin
         n := Abs(pSrc^);
         mpz_realloc(bi.Value, n);
         bi.Value.mp_size := pSrc^;
         Inc(pSrc);
         System.Move( bi.Value.mp_d^, pSrc^, n*4);
      end;

   end;

   Result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerShiftLeftExpr ------------------
// ------------------

procedure TBigIntegerShiftLeftExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_mul_2exp(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsInteger(exec));
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerShiftRightExpr ------------------
// ------------------

procedure TBigIntegerShiftRightExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_tdiv_q_2exp(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsInteger(exec));
   result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerOpAssignExpr ------------------
// ------------------

procedure TBigIntegerOpAssignExpr.TypeCheckAssign(prog : TdwsProgram; exec : TdwsExecution);
begin
   // nothing here
end;

// ------------------
// ------------------ TBigIntegerPlusAssignExpr ------------------
// ------------------

procedure TBigIntegerPlusAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_add(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   FLeft.AssignValue(exec, bi as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerMinusAssignExpr ------------------
// ------------------

procedure TBigIntegerMinusAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_sub(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   FLeft.AssignValue(exec, bi as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerMultAssignExpr ------------------
// ------------------

procedure TBigIntegerMultAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_mul(bi.Value, Left.EvalAsBigInteger(exec).Value^, Right.EvalAsBigInteger(exec).Value^);
   FLeft.AssignValue(exec, bi as IdwsBigInteger);
end;

// ------------------
// ------------------ TBigIntegerRandomFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TBigIntegerRandomFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);

   function RandomBigIntegerOfBitLength(nb : Integer) : IdwsBigInteger;
   var
      mask : Integer;
      bytes : TBytes;
      rnd : RawByteString;
      bi : TBigIntegerWrapper;
   begin
      // adapted from BigInteger.Create(NumBits: Integer; const Random: IRandom)
      // uses cryptographic random
      rnd := CryptographicRandom( (nb + 7) div 8 + 1 );
      Setlength(bytes, Length(rnd));
      System.Move(rnd[1], bytes[0], Length(rnd));

      // One byte too many was allocated, to get a top byte of 0, i.e. always positive.
      bytes[High(bytes)] := 0;

      // Set bits above required bit length to 0.
      mask := $7F shr (7 - (nb and 7));
      bytes[High(bytes)-1] := bytes[High(bytes)-1] and mask;

      bi := TBigIntegerWrapper.CreateZero;
      mpz_realloc(bi.Value, (nb div (8*4))+1);
      FillChar(bi.Value.mp_d^, bi.Value.mp_alloc*4, 0);
      bi.Value.mp_size := bi.Value.mp_alloc;
      System.Move(bytes[0], bi.Value.mp_d^, Length(bytes));

      Result := bi as IdwsBigInteger;
   end;

var
   bi, limit : IdwsBigInteger;
   bits : Integer;
begin
   limit := ArgBigInteger(args, 0);
   if mpz_cmp_ui(limit.Value^, 1) <= 0 then begin
      result := TBigIntegerWrapper.CreateZero as IdwsBigInteger;
   end else begin
      bits := limit.BitLength;
      repeat
         bi := RandomBigIntegerOfBitLength(bits);
      until mpz_cmp(bi.Value^, limit.Value^) < 0;
   end;
   result := bi;
end;

// ------------------
// ------------------ TBigIntegerBitLengthFunc ------------------
// ------------------

function TBigIntegerBitLengthFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := ArgBigInteger(args, 0).BitLength;
end;

// ------------------
// ------------------ TBigIntegerModPowFunc ------------------
// ------------------

procedure TBigIntegerModPowFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
begin
   bi := TBigIntegerWrapper.CreateZero;
   mpz_powm(bi.Value,
            ArgBigInteger(args, 0).Value^,
            ArgBigInteger(args, 1).Value^,
            ArgBigInteger(args, 2).Value^);
   Result := bi as IdwsBigInteger;
end;

// ------------------
// ------------------ TBigIntegerFactorialFunc ------------------
// ------------------

procedure TBigIntegerFactorialFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   bi : TBigIntegerWrapper;
   i : Int64;
begin
   bi := TBigIntegerWrapper.CreateZero;
   i:=args.AsInteger[0];
   if i <= 1 then
      mpz_set_uint64(bi.Value, 1)
   else mpz_fac_ui(bi.Value, i);
   Result := bi as IdwsBigInteger;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterBigIntegerType);
   dwsInternalUnit.AddOperatorsRegistrationProc(RegisterBigIntegerOperators);
   dwsInternalUnit.AddAbsHandler(HandleBigIntegerAbs);

   RegisterInternalStringFunction(TBigIntegerToStringFunc,  'BigIntegerToHex', ['v', SYS_BIGINTEGER, 'base=10', SYS_INTEGER], [iffStateLess], 'ToString');
   RegisterInternalFunction(TStringToBigIntegerFunc,        'StringToBigInteger', ['s', SYS_STRING, 'base=10', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess], 'ToBigInteger');
   RegisterInternalStringFunction(TBigIntegerToHexFunc,     'BigIntegerToHex', ['v', SYS_BIGINTEGER], [iffStateLess], 'ToHex');
   RegisterInternalFunction(THexToBigIntegerFunc,           'HexToBigInteger', ['h', SYS_STRING], SYS_BIGINTEGER, [iffStateLess], 'HexToBigInteger');

   RegisterInternalFunction(TBigIntegerToBlobFunc,          'BigIntegerToBlobParameter', ['v', SYS_BIGINTEGER], SYS_VARIANT, [iffStateLess], 'ToBlobParameter');
   RegisterInternalFunction(TBlobToBigIntegerFunc,          'BlobFieldToBigInteger', ['b', SYS_STRING], SYS_BIGINTEGER, [iffStateLess]);

   RegisterInternalFloatFunction(TBigIntegerToFloatFunc,    '',   ['v', SYS_BIGINTEGER], [iffStateLess], 'ToFloat');
   RegisterInternalIntFunction(TBigIntegerToIntegerFunc,    '',   ['v', SYS_BIGINTEGER], [iffStateLess], 'ToInteger');

   RegisterInternalBoolFunction(TBigIntegerOddFunc,   'Odd',      ['i', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'IsOdd');
   RegisterInternalBoolFunction(TBigIntegerEvenFunc,  'Even',     ['i', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'IsEven');
   RegisterInternalIntFunction(TBigIntegerSignFunc,   'Sign',     ['v', SYS_BIGINTEGER], [iffStateLess, iffOverloaded], 'Sign');
   RegisterInternalIntFunction(TBigIntegerBitLengthFunc, '',      ['v', SYS_BIGINTEGER], [iffStateLess], 'BitLength');
   RegisterInternalFunction(TBigIntegerGcdFunc,       'Gcd',      ['a', SYS_BIGINTEGER, 'b', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded]);
   RegisterInternalFunction(TBigIntegerLcmFunc,       'Gcd',      ['a', SYS_BIGINTEGER, 'b', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded]);
   RegisterInternalBoolFunction(TBigIntegerIsPrimeFunc, 'IsPrime',['n', SYS_BIGINTEGER, 'prob=25', SYS_INTEGER], [iffStateLess, iffOverloaded], 'IsPrime');
   RegisterInternalFunction(TBigIntegerPowerFunc,     'IntPower', ['base', SYS_BIGINTEGER, 'exponent', SYS_INTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'Power');
   RegisterInternalFunction(TBigIntegerSqrFunc,       'Sqr',      ['v', SYS_BIGINTEGER], SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'Sqr');
   RegisterInternalProcedure(TBigIntegerDivModFunc,   'DivMod',
                             ['dividend', SYS_BIGINTEGER, 'divisor', SYS_BIGINTEGER,
                              '@result', SYS_BIGINTEGER, '@remainder', SYS_BIGINTEGER], '', [iffOverloaded]);
   RegisterInternalFunction(TBigIntegerModPowFunc,    'ModPow',   ['base', SYS_BIGINTEGER, 'exponent', SYS_BIGINTEGER, 'modulus', SYS_BIGINTEGER],
                                                                  SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'ModPow');
   RegisterInternalFunction(TBigIntegerModPowFunc,    'ModPow',   ['base', SYS_BIGINTEGER, 'exponent', SYS_INTEGER, 'modulus', SYS_BIGINTEGER],
                                                                  SYS_BIGINTEGER, [iffStateLess, iffOverloaded], 'ModPow');
   RegisterInternalFunction(TBigIntegerFactorialFunc, 'BigFactorial', ['n', SYS_INTEGER], SYS_VARIANT, [iffStateLess]);

   RegisterInternalFunction(TBigIntegerRandomFunc,    'RandomBigInteger', ['limitPlusOne', SYS_BIGINTEGER], SYS_BIGINTEGER);

end.

