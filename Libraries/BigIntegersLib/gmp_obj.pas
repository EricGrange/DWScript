unit gmp_obj;

{
  GMP(MPIR) Library for Delphi.

  This wrapper only supports Delphi versions with operator overloading.
  For earlier versions please use header file gmp_lib.pas directly.

  GMP Library: http://gmplib.org/
  MPIR Project: http://www.mpir.org/

  Author: wqyfavor
  Date: 2011.2
  Blog: hi.baidu.com/wqyfavor
  Email: wqyfavor@qq.com
  QQ: 466798985
  Tweet: t.qq.com/wqyfavor
}

interface
uses
   SysUtils,
   Classes,
   gmp_lib;

const
   INITIAL_FLOAT_PRECISION = 40; // You can modify this value. But I still recommend
                                 // that the precision of floats be explicitly defined
                                 // once by calling SetDefaultFloatPrecision.
   SMALL_FLOAT_MAX_ZERO_PREFIX = 25; // Define how many zeros should be output for
                                     // small float values such as 0.0000000000000001.
                                     // If there are too many zeros, the float will
                                     // be output as exponent format.

   GMP_MIN_PRECISION = 10;

type
   TGMPObjectType = (gmpotInteger, gmpotRational, gmpotFloat);
   TGMPExceptionCode = (gmpeFatalError, gmpeFloatPrecisionTooLow, gmpeDivideByZero,
      gmpeConversionError, gmpeBaseError, gmpeParameterError);

   // sizeof(gmpObject) = sizeof(gmpInteger) = sizeof(gmpRational) = sizeof(gmpFloat)
   pgmpObject = ^gmpObject;
   gmpObject = packed record
      NativeType: TGMPObjectType;
      RelativeNode: Pointer;
      Fill: Integer; // No use for gmpInteger and gmpRantional at present.
      NativeObject: Pointer;
   end;

   pgmpInteger = ^gmpInteger;
   pgmpRational = ^gmpRational;
   pgmpFloat = ^gmpFloat;

   { TIPS ABOUT gmpInteger
     [1] String is regarded as gmpFloat by default, you should cast a string
          to gmpInteger explicitly using gmpInteger(String).
     [2] Note that this function returns a pointer to gmpRational type. That is
         because I cannot put declaration of gmpRational infront of gmpInteger.
         In this way users of this library do not need to care about the
         encapsulation, and all implementation is hidden from upper-class.
         You can find that gmpRational implements operator overloading with
         pgmpRational pointer.

         This is also the reason that each overloaded utility function such
         as gmpSin accepts pgmpRational pointer. }

   gmpInteger = packed record
   private
      // The actual type for distinguishing the variable
      NativeType: TGMPObjectType;

      // For temporary gmpXXX objects.
      RelativeNode: Pointer;

      Fill: Integer;

      procedure CreateGeneric;

      function FGetIsTemporary: Boolean; inline;
      function FGetIsOdd: Boolean; inline;
      function FGetIsEven: Boolean; inline;
      function FGetIsPerfectPower: Boolean; inline;
      function FGetIsPerfectSquare: Boolean; inline;
      function FGetFitsCardinal: Boolean; inline;
      function FGetFitsInteger: Boolean; inline;

      procedure ReleaseTemporary;
      property IsTemporary: Boolean read FGetIsTemporary;

   public
      // Pointer to the native prototype gmp object
      NativeObject: pmpz_t;

      // Constructors for delphi native types. These functions will create gmpInteger with an initial value.
      constructor Create(const Value: Integer); overload;
      constructor Create(const Value: Cardinal); overload;
      constructor Create(const Value: Int64); overload;
      constructor Create(const Value: UInt64); overload;

      // [TIP] Create gmpInteger from a string.
      { From GMP manual:
          White space is allowed in the string, and is simply ignored.
          The base may vary from 2 to 62, or if base is 0, then the leading characters are used:
              0x and 0X for hexadecimal, 0b and 0B for binary, 0 for octal, or decimal otherwise.
          For bases up to 36, case is ignored; upper-case and lower-case letters have the same value.
          For bases 37 to 62, upper-case letter represent the usual 10..35 while lower-case letter represent 36..61. }
      constructor Create(const Value: AnsiString); overload; // base 10 by default
      constructor Create(const Value: AnsiString; Base: Integer); overload;

      constructor Create(const Value: gmpInteger); overload;

      // [TIP] Call this destructor after a gmpInteger type is no longer used.
      // Also you can use gmpFree procedure in gmp_util unit to free a series of gmpXXXX objects.
      procedure Free;

      // [TIP] Assign functions. Must be used after constructor is called.
      procedure Assign(const Value: Integer); overload; inline;
      procedure Assign(const Value: Cardinal); overload; inline;
      procedure Assign(const Value: Int64); overload; inline;
      procedure Assign(const Value: UInt64); overload; inline;
      procedure Assign(const Value: AnsiString); overload; // base 10 by default
      procedure Assign(const Value: AnsiString; Base: Integer); overload;
      procedure Assign(const Value: gmpInteger); overload;

      // Set functions
      procedure SetZero; inline; // Equals to "Int.Assign(0)"
      procedure SetNegative; inline; // More efficient than "Int.Assign(-Int)"
      procedure SetAbsolute; inline; // Set A to Abs(A)
      procedure SetSquare; inline; // Set A to A^2
      procedure SetCubic; inline; // Set A to A^3
      procedure SetIntPower(Exp: Cardinal); inline; // Set A to A^Exp

      // Set A to truncated integer part of Sqrt(A). Return true if the
      // computation was exact. If not, use GetRem to find the remainder.
      function SetSqrt: Boolean; inline;
      function SetNthRoot(N: Cardinal): Boolean; inline; // Set A to truncated integer part of A^(1/N)

      procedure SetMul2Exp(const Base: gmpInteger; N: mp_bitcnt_t); // Set A to Base*2^N
      procedure SetDiv2Exp(const Base: gmpInteger; N: mp_bitcnt_t); // Set A to Base/2^N

      // Remove all occurrences of the factor F from A. The return value is how
      // many such occurrences were removed.
      function RemoveFactor(F: Cardinal): Cardinal; overload;
      function RemoveFactor(const F: gmpInteger): Cardinal; overload;

      // ################### bit manipulation methods ###################
      function GetPopulationCount: mp_bitcnt_t; // refer to description of mpz_popcount
      function GetHammingDistance(const Op2: gmpInteger): mp_bitcnt_t; // refer to description of mpz_hamdist
      function Scan0(StartingBit: mp_bitcnt_t): mp_bitcnt_t; // refer to description of mpz_scan0
      function Scan1(StartingBit: mp_bitcnt_t): mp_bitcnt_t; // refer to description of mpz_scan1
      procedure SetBit(BitIndex: mp_bitcnt_t); inline; // set bit to 1
      procedure ClearBit(BitIndex: mp_bitcnt_t); inline; // set bit to 0
      procedure ComplementBit(BitIndex: mp_bitcnt_t); inline; // complement bit
      function TestBit(BitIndex: mp_bitcnt_t): Integer; // get bit

      // ################### Output methods ###################
      function ToCardinal: Cardinal; inline;
      function ToInteger: Integer; inline;
      function ToDouble: Double; inline;
      procedure To2Exp(var D: Double; var Exp: Integer); inline; // Return gmpInteger in format D*2^Exp and 0.5 <= |D| < 1
      function ToString(Base: Integer = 10): AnsiString;

      // ################### Increment methods ###################
      procedure Inc; overload; inline;
      procedure Inc(const Value: Integer); overload; inline;
      procedure Inc(const Value: Cardinal); overload; inline;
      procedure Inc(const Value: Int64); overload;
      procedure Inc(const Value: UInt64); overload;
      procedure Inc(const Value: AnsiString); overload; // base 10 by default
      procedure Inc(const Value: AnsiString; Base: Integer); overload;
      procedure Inc(const Value: gmpInteger); overload;

      // ################### Decrement methods ###################
      procedure Dec; overload; inline;
      procedure Dec(const Value: Integer); overload; inline;
      procedure Dec(const Value: Cardinal); overload; inline;
      procedure Dec(const Value: Int64); overload;
      procedure Dec(const Value: UInt64); overload;
      procedure Dec(const Value: AnsiString); overload; // base 10 by default
      procedure Dec(const Value: AnsiString; Base: Integer); overload;
      procedure Dec(const Value: gmpInteger); overload;

      // ################### Multiply and Divide methods ###################
      procedure MultiplyBy(const Value: gmpInteger); overload;
      procedure MultiplyBy(const Value: Integer); overload;
      procedure MultiplyBy(const Value: Cardinal); overload;
      procedure MultiplyBy(const Value: Int64); overload;
      procedure MultiplyBy(const Value: UInt64); overload;

      // Integer divide, remainder is discarded.
      procedure DivideBy(const Value: gmpInteger); overload;
      procedure DivideBy(const Value: Integer); overload;
      procedure DivideBy(const Value: Cardinal); overload;
      procedure DivideBy(const Value: Int64); overload;
      procedure DivideBy(const Value: UInt64); overload;

      procedure Mul2Exp(const N: mp_bitcnt_t); inline; // Set A to A*2^N
      procedure Div2Exp(const N: mp_bitcnt_t); inline; // Set A to A/2^N

      // Operators

      // [TIP] Not supported because of memory management, USE Assign procedures instead.
      // Note that such code will cause memory leak or serious error:
      // var a, b: gmpInteger;
      //    a.Create(0); b.Create(100);   // memory allocated for both objects.
      //    a := b;                       // this will cause the memory of a unreferenced.
      //    a.Free;                       // free memory of b actually.
      //    b.Free;                       // this will cause the memory of b freed twice.
      // class operator Implicit(const Value: gmpInteger): gmpInteger;

      // [TIP] Explicit casting is supported and temporary objects will used.
      // Use like this:
      // var a: gmpInteger;
      //    a.Create(0);
      //    a.Assign(1 + gmpInteger('10000000000000000000000000') / 3);
      class operator Explicit(const Value: Integer): gmpInteger; overload;
      class operator Explicit(const Value: Cardinal): gmpInteger; overload;
      class operator Explicit(const Value: Int64): gmpInteger; overload;
      class operator Explicit(const Value: UInt64): gmpInteger; overload;
      class operator Explicit(const Value: AnsiString): gmpInteger; overload; // base 10 by default

      // Executed in expression "a.Assign(-b)" (both a and b are gmpInteger variables.)
      class operator Negative(const Value: gmpInteger): gmpInteger;
      class operator Positive(const Value: gmpInteger): gmpInteger;

      // [TIP] Not supported because of memory management, USE Assign procedures or
      // internal Inc and Dec methodes instead.
      // Example: "a.Assign(a + 2)" or "a.Inc;". Note that "a.Inc(2)" is much more efficient.
      // class operator Inc(const Value: gmpInteger): gmpInteger;
      // class operator Dec(const Value: gmpInteger): gmpInteger;

      // [TIP] Trunc and Round functions return a copy of the input integer.
      class operator Trunc(const Value: gmpInteger): gmpInteger;
      class operator Round(const Value: gmpInteger): gmpInteger;

      // ################### Add operator functions ###################
      class operator Add(const Left, Right: gmpInteger): gmpInteger; overload;

      class operator Add(const Left: gmpInteger; const Right: Integer): gmpInteger; overload;
      class operator Add(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;
      class operator Add(const Left: gmpInteger; const Right: Int64): gmpInteger; overload;
      class operator Add(const Left: gmpInteger; const Right: UInt64): gmpInteger; overload;
      class operator Add(const Left: gmpInteger; const Right: Single): pgmpRational; overload;
      class operator Add(const Left: gmpInteger; const Right: Double): pgmpRational; overload;
      // class operator Add(const Left: gmpInteger; const Right: AnsiString): gmpInteger; overload; // Please refer to TIP1

      class operator Add(const Left: Integer; const Right: gmpInteger): gmpInteger; overload;
      class operator Add(const Left: Cardinal; const Right: gmpInteger): gmpInteger; overload;
      class operator Add(const Left: Int64; const Right: gmpInteger): gmpInteger; overload;
      class operator Add(const Left: UInt64; const Right: gmpInteger): gmpInteger; overload;
      class operator Add(const Left: Single; const Right: gmpInteger): pgmpRational; overload;
      class operator Add(const Left: Double; const Right: gmpInteger): pgmpRational; overload;
      // class operator Add(const Left: AnsiString; const Right: gmpInteger): gmpInteger; overload; // Please refer to TIP1

      // ################### Subtract operator functions ###################
      class operator Subtract(const Left, Right: gmpInteger): gmpInteger; overload;

      class operator Subtract(const Left: gmpInteger; const Right: Integer): gmpInteger; overload;
      class operator Subtract(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;
      class operator Subtract(const Left: gmpInteger; const Right: Int64): gmpInteger; overload;
      class operator Subtract(const Left: gmpInteger; const Right: UInt64): gmpInteger; overload;
      // class operator Subtract(const Left: gmpInteger; const Right: AnsiString): gmpInteger; overload; // Please refer to TIP1

      class operator Subtract(const Left: Integer; const Right: gmpInteger): gmpInteger; overload;
      class operator Subtract(const Left: Cardinal; const Right: gmpInteger): gmpInteger; overload;
      class operator Subtract(const Left: Int64; const Right: gmpInteger): gmpInteger; overload;
      class operator Subtract(const Left: UInt64; const Right: gmpInteger): gmpInteger; overload;
      // class operator Subtract(const Left: AnsiString; const Right: gmpInteger): gmpInteger; overload; // Please refer to TIP1

      // ################### Multiply operator functions ###################
      class operator Multiply(const Left, Right: gmpInteger): gmpInteger; overload;

      class operator Multiply(const Left: Integer; const Right: gmpInteger): gmpInteger; overload;
      class operator Multiply(const Left: Cardinal; const Right: gmpInteger): gmpInteger; overload;
      class operator Multiply(const Left: Int64; const Right: gmpInteger): gmpInteger; overload;
      class operator Multiply(const Left: UInt64; const Right: gmpInteger): gmpInteger; overload;
      // class operator Multiply(const Left: AnsiString; const Right: gmpInteger): gmpInteger; overload; // Please refer to TIP1
      class operator Multiply(const Left: Single; const Right: gmpInteger): pgmpRational; overload; // Please refer to TIP2
      class operator Multiply(const Left: Double; const Right: gmpInteger): pgmpRational; overload; // Please refer to TIP2

      class operator Multiply(const Left: gmpInteger; const Right: Integer): gmpInteger; overload;
      class operator Multiply(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;
      class operator Multiply(const Left: gmpInteger; const Right: Int64): gmpInteger; overload;
      class operator Multiply(const Left: gmpInteger; const Right: UInt64): gmpInteger; overload;
      // class operator Multiply(const Left: gmpInteger; const Right: AnsiString): gmpInteger; overload; // Please refer to TIP1
      class operator Multiply(const Left: gmpInteger; const Right: Single): pgmpRational; overload; // Please refer to TIP2
      class operator Multiply(const Left: gmpInteger; const Right: Double): pgmpRational; overload; // Please refer to TIP2

      // ################### Division operator functions ###################
      // For pascal style, integer dividing integer generates a float type.
      // To keep precision, gmpRational is returned.
      // Please refer to TIP2 about pgmpRational return type.
      class operator Divide(const Left, Right: gmpInteger): pgmpRational; overload;

      class operator Divide(const Left: Integer; const Right: gmpInteger): pgmpRational; overload;
      class operator Divide(const Left: Cardinal; const Right: gmpInteger): pgmpRational; overload;
      class operator Divide(const Left: Int64; const Right: gmpInteger): pgmpRational; overload;
      class operator Divide(const Left: UInt64; const Right: gmpInteger): pgmpRational; overload;
      // class operator Divide(const Left: AnsiString; const Right: gmpInteger): pgmpRational; overload; // Please refer to TIP1
      class operator Divide(const Left: Single; const Right: gmpInteger): pgmpRational; overload;
      class operator Divide(const Left: Double; const Right: gmpInteger): pgmpRational; overload;

      class operator Divide(const Left: gmpInteger; const Right: Integer): pgmpRational; overload;
      class operator Divide(const Left: gmpInteger; const Right: Cardinal): pgmpRational; overload;
      class operator Divide(const Left: gmpInteger; const Right: Int64): pgmpRational; overload;
      class operator Divide(const Left: gmpInteger; const Right: UInt64): pgmpRational; overload;
      // class operator Divide(const Left: gmpInteger; const Right: AnsiString): pgmpRational; overload; // Please refer to TIP1
      class operator Divide(const Left: gmpInteger; const Right: Single): pgmpRational; overload;
      class operator Divide(const Left: gmpInteger; const Right: Double): pgmpRational; overload;

      // ################### Integer-divide operator functions ###################
      class operator IntDivide(const Left, Right: gmpInteger): gmpInteger; overload;

      class operator IntDivide(const Left: Integer; const Right: gmpInteger): gmpInteger; overload;
      class operator IntDivide(const Left: Cardinal; const Right: gmpInteger): gmpInteger; overload;
      class operator IntDivide(const Left: Int64; const Right: gmpInteger): gmpInteger; overload;
      class operator IntDivide(const Left: UInt64; const Right: gmpInteger): gmpInteger; overload;
      // class operator IntDivide(const Left: AnsiString; const Right: gmpInteger): gmpInteger; overload; // Please refer to TIP1

      class operator IntDivide(const Left: gmpInteger; const Right: Integer): gmpInteger; overload;
      class operator IntDivide(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;
      class operator IntDivide(const Left: gmpInteger; const Right: Int64): gmpInteger; overload;
      class operator IntDivide(const Left: gmpInteger; const Right: UInt64): gmpInteger; overload;
      // class operator IntDivide(const Left: gmpInteger; const Right: AnsiString): gmpInteger; overload; // Please refer to TIP1

      // ################### Modulus operator functions ###################
      class operator Modulus(const Left, Right: gmpInteger): gmpInteger; overload;

      class operator Modulus(const Left: Integer; const Right: gmpInteger): gmpInteger; overload;
      class operator Modulus(const Left: Cardinal; const Right: gmpInteger): gmpInteger; overload;
      class operator Modulus(const Left: Int64; const Right: gmpInteger): gmpInteger; overload;
      class operator Modulus(const Left: UInt64; const Right: gmpInteger): gmpInteger; overload;
      // class operator Modulus(const Left: AnsiString; const Right: gmpInteger): gmpInteger; overload; // Please refer to TIP1

      class operator Modulus(const Left: gmpInteger; const Right: Integer): gmpInteger; overload;
      class operator Modulus(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;
      class operator Modulus(const Left: gmpInteger; const Right: Int64): gmpInteger; overload;
      class operator Modulus(const Left: gmpInteger; const Right: UInt64): gmpInteger; overload;
      // class operator Modulus(const Left: gmpInteger; const Right: AnsiString): gmpInteger; overload; // Please refer to TIP1

      // ################### Equal operator functions ###################
      class operator Equal(const Left, Right: gmpInteger): Boolean; overload;

      class operator Equal(const Left: gmpInteger; const Right: Integer): Boolean; overload;
      class operator Equal(const Left: gmpInteger; const Right: Cardinal): Boolean; overload;
      class operator Equal(const Left: gmpInteger; const Right: Int64): Boolean; overload;
      class operator Equal(const Left: gmpInteger; const Right: UInt64): Boolean; overload;
      class operator Equal(const Left: gmpInteger; const Right: Single): Boolean; overload;
      class operator Equal(const Left: gmpInteger; const Right: Double): Boolean; overload;
      // class operator Equal(const Left: gmpInteger; const Right: AnsiString): Boolean; overload; // Please refer to TIP1

      class operator Equal(const Left: Integer; const Right: gmpInteger): Boolean; overload;
      class operator Equal(const Left: Cardinal; const Right: gmpInteger): Boolean; overload;
      class operator Equal(const Left: Int64; const Right: gmpInteger): Boolean; overload;
      class operator Equal(const Left: UInt64; const Right: gmpInteger): Boolean; overload;
      class operator Equal(const Left: Single; const Right: gmpInteger): Boolean; overload;
      class operator Equal(const Left: Double; const Right: gmpInteger): Boolean; overload;
      // class operator Equal(const Left: AnsiString; const Right: gmpInteger): Boolean; overload; // Please refer to TIP1

      // ################### Not-Equal operator functions ###################
      class operator NotEqual(const Left, Right: gmpInteger): Boolean; overload;

      class operator NotEqual(const Left: gmpInteger; const Right: Integer): Boolean; overload;
      class operator NotEqual(const Left: gmpInteger; const Right: Cardinal): Boolean; overload;
      class operator NotEqual(const Left: gmpInteger; const Right: Int64): Boolean; overload;
      class operator NotEqual(const Left: gmpInteger; const Right: UInt64): Boolean; overload;
      class operator NotEqual(const Left: gmpInteger; const Right: Single): Boolean; overload;
      class operator NotEqual(const Left: gmpInteger; const Right: Double): Boolean; overload;
      // class operator NotEqual(const Left: gmpInteger; const Right: AnsiString): Boolean; overload; // Please refer to TIP1

      class operator NotEqual(const Left: Integer; const Right: gmpInteger): Boolean; overload;
      class operator NotEqual(const Left: Cardinal; const Right: gmpInteger): Boolean; overload;
      class operator NotEqual(const Left: Int64; const Right: gmpInteger): Boolean; overload;
      class operator NotEqual(const Left: UInt64; const Right: gmpInteger): Boolean; overload;
      class operator NotEqual(const Left: Single; const Right: gmpInteger): Boolean; overload;
      class operator NotEqual(const Left: Double; const Right: gmpInteger): Boolean; overload;
      // class operator NotEqual(const Left: AnsiString; const Right: gmpInteger): Boolean; overload; // Please refer to TIP1

      // ################### Greater-Than operator functions ###################
      class operator GreaterThan(const Left, Right: gmpInteger): Boolean; overload;

      class operator GreaterThan(const Left: gmpInteger; const Right: Integer): Boolean; overload;
      class operator GreaterThan(const Left: gmpInteger; const Right: Cardinal): Boolean; overload;
      class operator GreaterThan(const Left: gmpInteger; const Right: Int64): Boolean; overload;
      class operator GreaterThan(const Left: gmpInteger; const Right: UInt64): Boolean; overload;
      class operator GreaterThan(const Left: gmpInteger; const Right: Single): Boolean; overload;
      class operator GreaterThan(const Left: gmpInteger; const Right: Double): Boolean; overload;
      // class operator GreaterThan(const Left: gmpInteger; const Right: AnsiString): Boolean; overload; // Please refer to TIP1

      class operator GreaterThan(const Left: Integer; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThan(const Left: Cardinal; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThan(const Left: Int64; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThan(const Left: UInt64; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThan(const Left: Single; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThan(const Left: Double; const Right: gmpInteger): Boolean; overload;
      // class operator GreaterThan(const Left: AnsiString; const Right: gmpInteger): Boolean; overload; // Please refer to TIP1

      // ################### GreaterThanOrEqual operator functions ###################
      class operator GreaterThanOrEqual(const Left, Right: gmpInteger): Boolean; overload;

      class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: Integer): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: Cardinal): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: Int64): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: UInt64): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: Single): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: Double): Boolean; overload;
      // class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: AnsiString): Boolean; overload; // Please refer to TIP1

      class operator GreaterThanOrEqual(const Left: Integer; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Cardinal; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Int64; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: UInt64; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Single; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Double; const Right: gmpInteger): Boolean; overload;
      // class operator GreaterThanOrEqual(const Left: AnsiString; const Right: gmpInteger): Boolean; overload; // Please refer to TIP1

      // ################### Less-Than operator functions ###################
      class operator LessThan(const Left, Right: gmpInteger): Boolean; overload;

      class operator LessThan(const Left: gmpInteger; const Right: Integer): Boolean; overload;
      class operator LessThan(const Left: gmpInteger; const Right: Cardinal): Boolean; overload;
      class operator LessThan(const Left: gmpInteger; const Right: Int64): Boolean; overload;
      class operator LessThan(const Left: gmpInteger; const Right: UInt64): Boolean; overload;
      class operator LessThan(const Left: gmpInteger; const Right: Single): Boolean; overload;
      class operator LessThan(const Left: gmpInteger; const Right: Double): Boolean; overload;
      // class operator LessThan(const Left: gmpInteger; const Right: AnsiString): Boolean; overload; // Please refer to TIP1

      class operator LessThan(const Left: Integer; const Right: gmpInteger): Boolean; overload;
      class operator LessThan(const Left: Cardinal; const Right: gmpInteger): Boolean; overload;
      class operator LessThan(const Left: Int64; const Right: gmpInteger): Boolean; overload;
      class operator LessThan(const Left: UInt64; const Right: gmpInteger): Boolean; overload;
      class operator LessThan(const Left: Single; const Right: gmpInteger): Boolean; overload;
      class operator LessThan(const Left: Double; const Right: gmpInteger): Boolean; overload;
      // class operator LessThan(const Left: AnsiString; const Right: gmpInteger): Boolean; overload; // Please refer to TIP1

      // ################### LessThanOrEqual operator functions ###################
      class operator LessThanOrEqual(const Left, Right: gmpInteger): Boolean; overload;

      class operator LessThanOrEqual(const Left: gmpInteger; const Right: Integer): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpInteger; const Right: Cardinal): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpInteger; const Right: Int64): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpInteger; const Right: UInt64): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpInteger; const Right: Single): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpInteger; const Right: Double): Boolean; overload;
      // class operator LessThanOrEqual(const Left: gmpInteger; const Right: AnsiString): Boolean; overload; // Please refer to TIP1

      class operator LessThanOrEqual(const Left: Integer; const Right: gmpInteger): Boolean; overload;
      class operator LessThanOrEqual(const Left: Cardinal; const Right: gmpInteger): Boolean; overload;
      class operator LessThanOrEqual(const Left: Int64; const Right: gmpInteger): Boolean; overload;
      class operator LessThanOrEqual(const Left: UInt64; const Right: gmpInteger): Boolean; overload;
      class operator LessThanOrEqual(const Left: Single; const Right: gmpInteger): Boolean; overload;
      class operator LessThanOrEqual(const Left: Double; const Right: gmpInteger): Boolean; overload;
      // class operator LessThanOrEqual(const Left: AnsiString; const Right: gmpInteger): Boolean; overload; // Please refer to TIP1

      // Logical operation not supported for integer type
      // class operator LogicalNot(const Value: gmpInteger): gmpInteger;
      // class operator LogicalAnd(const Left: gmpInteger; const Right: gmpInteger): gmpInteger;
      // class operator LogicalOr(const Left: gmpInteger; const Right: gmpInteger): gmpInteger;
      // class operator LogicalXor(const Left: gmpInteger; const Right: gmpInteger): gmpInteger;

      // ################### BitwiseNot operator functions ###################
      // This is bitwise-not actually
      class operator LogicalNot(const Value: gmpInteger): gmpInteger; // Delphi regulation

      // ################### BitwiseAnd operator functions ###################
      class operator BitwiseAnd(const Left, Right: gmpInteger): gmpInteger; overload;

      class operator BitwiseAnd(const Left: Integer; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseAnd(const Left: Cardinal; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseAnd(const Left: Int64; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseAnd(const Left: UInt64; const Right: gmpInteger): gmpInteger; overload;
      // class operator BitwiseAnd(const Left: AnsiString; const Right: gmpInteger): gmpInteger; overload; // Please refer to TIP1

      class operator BitwiseAnd(const Left: gmpInteger; const Right: Integer): gmpInteger; overload;
      class operator BitwiseAnd(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;
      class operator BitwiseAnd(const Left: gmpInteger; const Right: Int64): gmpInteger; overload;
      class operator BitwiseAnd(const Left: gmpInteger; const Right: UInt64): gmpInteger; overload;
      // class operator BitwiseAnd(const Left: gmpInteger; const Right: AnsiString): gmpInteger; overload; // Please refer to TIP1

      // ################### BitwiseOr operator functions ###################
      class operator BitwiseOr(const Left, Right: gmpInteger): gmpInteger; overload;

      class operator BitwiseOr(const Left: Integer; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseOr(const Left: Cardinal; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseOr(const Left: Int64; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseOr(const Left: UInt64; const Right: gmpInteger): gmpInteger; overload;
      // class operator BitwiseOr(const Left: AnsiString; const Right: gmpInteger): gmpInteger; overload; // Please refer to TIP1

      class operator BitwiseOr(const Left: gmpInteger; const Right: Integer): gmpInteger; overload;
      class operator BitwiseOr(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;
      class operator BitwiseOr(const Left: gmpInteger; const Right: Int64): gmpInteger; overload;
      class operator BitwiseOr(const Left: gmpInteger; const Right: UInt64): gmpInteger; overload;
      // class operator BitwiseOr(const Left: gmpInteger; const Right: AnsiString): gmpInteger; overload; // Please refer to TIP1

      // ################### BitwiseXor operator functions ###################
      class operator BitwiseXor(const Left, Right: gmpInteger): gmpInteger; overload;

      class operator BitwiseXor(const Left: Integer; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseXor(const Left: Cardinal; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseXor(const Left: Int64; const Right: gmpInteger): gmpInteger; overload;
      class operator BitwiseXor(const Left: UInt64; const Right: gmpInteger): gmpInteger; overload;
      // class operator BitwiseXor(const Left: AnsiString; const Right: gmpInteger): gmpInteger; overload; // Please refer to TIP1

      class operator BitwiseXor(const Left: gmpInteger; const Right: Integer): gmpInteger; overload;
      class operator BitwiseXor(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;
      class operator BitwiseXor(const Left: gmpInteger; const Right: Int64): gmpInteger; overload;
      class operator BitwiseXor(const Left: gmpInteger; const Right: UInt64): gmpInteger; overload;
      // class operator BitwiseXor(const Left: gmpInteger; const Right: AnsiString): gmpInteger; overload; // Please refer to TIP1

      // ################### LeftShift operator functions ###################
      class operator LeftShift(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;

      // ################### RightShift operator functions ###################
      class operator RightShift(const Left: gmpInteger; const Right: Cardinal): gmpInteger; overload;

      property IsOdd: Boolean read FGetIsOdd;
      property IsEven: Boolean read FGetIsEven;
      property IsPerfectPower: Boolean read FGetIsPerfectPower;
      property IsPerfectSquare: Boolean read FGetIsPerfectSquare;
      property FitsCardinal: Boolean read FGetFitsCardinal;
      property FitsInteger: Boolean read FGetFitsInteger;
   end;

   { TIPS ABOUT gmpRational
     [1] Support pointer because some operations of gmpInteger return pgmpRational type.
     [2] Because some operations of gmpInteger generate pgmpRational type
         variable, so sometimes you will find that unary operations of gmpRational
         won't work.
         For example:
           var a: gmpInteger;
               b: gmpRational;

               a.Create(10);
               b.Create(-(a * 0.5)); // This won't compile because "a * 0.5" returns
                                     // a pgmpRational variable and "-" operator
                                     // doesn't accept pgmpRational as parameter.
               b.Create(-a * 0.5);   // This compiles because Create procedure accepts
                                     // pgmpRational type.

         So use this library with awareness of some hidden-principles.
   }

   gmpRational = packed record
   private
      // The actual type for distinguishing the variable
      NativeType: TGMPObjectType;

      // For temporary gmpXXX objects.
      RelativeNode: Pointer;

      Fill: Integer;

      procedure CreateGeneric;

      function FGetIsTemporary: Boolean; inline;
      procedure ReleaseTemporary;
      property IsTemporary: Boolean read FGetIsTemporary;

   public
      // Pointer to the native prototype gmp object
      NativeObject: pmpq_t;

      // Constructors for delphi native types. These functions will create gmpRational with an initial value.
      constructor Create(const Value: Integer); overload;
      constructor Create(const Value: Cardinal); overload;
      constructor Create(const Value: Int64); overload;
      constructor Create(const Value: UInt64); overload;
      constructor Create(const Value: Single); overload;
      constructor Create(const Value: Double); overload;

      // [TIP] Create gmpRational from a string.
      { From GMP manual:
           Set rop from a null-terminated string str in the given base.
           The string can be an integer like "41" or a fraction like "41/152".
           The fraction must be in canonical form, or if not then mpq_canonicalize
           must be called.
           The numerator and optional denominator are parsed the same as in
           mpz_set_str of integer type. White space is allowed in the string,
           and is simply ignored. The base can vary from 2 to 62, or if base is 0
           then the leading characters are used:
             0x or 0X for hex, 0b or 0B for binary, 0 for octal, or decimal otherwise.
             Note that this is done separately for the numerator and denominator,
             so for instance 0xEF/100 is 239/100, whereas 0xEF/0x100 is 239/256.
             The return value is 0 if the entire string is a valid number, or  1 if not. }
      constructor Create(const Value: AnsiString); overload; // base 10 by default
      constructor Create(const Value: AnsiString; Base: Integer); overload;

      constructor Create(const Value: gmpInteger); overload;
      constructor Create(const Value: gmpRational); overload;
      constructor Create(const Value: pgmpRational); overload; // Please refer to TIP1
      constructor Create(const Value: pgmpFloat); overload;

      // Create and set value of rationl number to Numerator/Denominator.
      // Denominator must not be zero.
      constructor Create(const Numerator, Denominator: Integer); overload;
      constructor Create(const Numerator, Denominator: Int64); overload;
      constructor Create(const Numerator, Denominator: gmpInteger); overload;

      // [TIP] Call this destructor after a gmpRational type is no longer used.
      // Also you can use gmpFree procedure in gmp_util unit to free a series of gmpXXXX objects.
      procedure Free;

      // [TIP] Assign functions. Must be used after constructor is called.
      procedure Assign(const Value: Integer); overload; inline;
      procedure Assign(const Value: Cardinal); overload; inline;
      procedure Assign(const Value: Int64); overload;
      procedure Assign(const Value: UInt64); overload;
      procedure Assign(const Value: Single); overload; inline;
      procedure Assign(const Value: Double); overload; inline;
      procedure Assign(const Value: AnsiString); overload; // base 10 by default
      procedure Assign(const Value: AnsiString; Base: Integer); overload;
      procedure Assign(const Value: gmpInteger); overload; inline;
      procedure Assign(const Value: gmpRational); overload; inline;
      procedure Assign(const Value: pgmpRational); overload; inline; // Please refer to TIP1
      procedure Assign(const Value: pgmpFloat); overload; inline;
      procedure Assign(const Numerator, Denominator: gmpInteger); overload;

      procedure Canonicalize; // For most cases, you don't need to call this function.
                              // Only after this gmpRational is assigned by gmp_scanf.

      // Set functions
      procedure SetZero; inline; // Equals to "Rational.Assign(0)"
      procedure SetNegative; inline; // More efficient than "Rational.Assign(-Rational)"
      procedure SetAbsolute; inline; // Set A to Abs(A)
      procedure SetReciprocal; // Set A to 1/A. Swap denominator and numerator.
      procedure SetSquare;
      procedure SetCubic;
      procedure SetIntPower(Exp: Cardinal);

      procedure SetMul2Exp(const Base: gmpRational; N: mp_bitcnt_t); // Set A to Base*2^N
      procedure SetDiv2Exp(const Base: gmpRational; N: mp_bitcnt_t); // Set A to Base/2^N

      // ################### Manipulate num/den separately ###################
      procedure SetNumerator(const Value: gmpInteger);
      procedure SetDenominator(const Value: gmpInteger);
      procedure GetNumerator(var Output: gmpInteger); inline;
      procedure GetDenominator(var Output: gmpInteger); inline;

      // ################### Output methods ###################
      function ToDouble: Double; inline;
      function ToString(Base: Integer = 10): AnsiString; // As float by default
      function ToStringFraction(Base: Integer = 10): AnsiString; // As fraction

      // ################### Increment methods ###################
      procedure Inc; overload;
      procedure Inc(const Value: Integer); overload;
      procedure Inc(const Value: Cardinal); overload;
      procedure Inc(const Value: Int64); overload;
      procedure Inc(const Value: UInt64); overload;
      procedure Inc(const Value: Single); overload;
      procedure Inc(const Value: Double); overload;
      procedure Inc(const Value: AnsiString); overload; // base 10 by default
      procedure Inc(const Value: AnsiString; Base: Integer); overload;
      procedure Inc(const Value: gmpInteger); overload;
      procedure Inc(const Value: gmpRational); overload;
      procedure Inc(const Value: pgmpRational); overload; // Please refer to TIP1
      procedure Inc(const Value: pgmpFloat); overload;

      // ################### Decrement methods ###################
      procedure Dec; overload;
      procedure Dec(const Value: Integer); overload;
      procedure Dec(const Value: Cardinal); overload;
      procedure Dec(const Value: Int64); overload;
      procedure Dec(const Value: UInt64); overload;
      procedure Dec(const Value: Single); overload;
      procedure Dec(const Value: Double); overload;
      procedure Dec(const Value: AnsiString); overload; // base 10 by default
      procedure Dec(const Value: AnsiString; Base: Integer); overload;
      procedure Dec(const Value: gmpInteger); overload;
      procedure Dec(const Value: gmpRational); overload;
      procedure Dec(const Value: pgmpRational); overload; // Please refer to TIP1
      procedure Dec(const Value: pgmpFloat); overload;

      // ################### Multiply and Divide methods ###################
      procedure MultiplyBy(const Value: Integer); overload;
      procedure MultiplyBy(const Value: Cardinal); overload;
      procedure MultiplyBy(const Value: Int64); overload;
      procedure MultiplyBy(const Value: UInt64); overload;
      procedure MultiplyBy(const Value: Single); overload;
      procedure MultiplyBy(const Value: Double); overload;
      procedure MultiplyBy(const Value: gmpInteger); overload;
      procedure MultiplyBy(const Value: gmpRational); overload;
      procedure MultiplyBy(const Value: pgmpRational); overload;
      procedure MultiplyBy(const Value: pgmpFloat); overload;

      procedure DivideBy(const Value: Integer); overload;
      procedure DivideBy(const Value: Cardinal); overload;
      procedure DivideBy(const Value: Int64); overload;
      procedure DivideBy(const Value: UInt64); overload;
      procedure DivideBy(const Value: Single); overload;
      procedure DivideBy(const Value: Double); overload;
      procedure DivideBy(const Value: gmpInteger); overload;
      procedure DivideBy(const Value: gmpRational); overload;
      procedure DivideBy(const Value: pgmpRational); overload;
      procedure DivideBy(const Value: pgmpFloat); overload;

      procedure Mul2Exp(const N: mp_bitcnt_t); inline; // Set A to A*2^N
      procedure Div2Exp(const N: mp_bitcnt_t); inline; // Set A to A/2^N

      // Operators

      // [TIP] Refer to tips about explicit-casting methods of gmpInteger.
      class operator Explicit(const Value: Integer): gmpRational; overload;
      class operator Explicit(const Value: Cardinal): gmpRational; overload;
      class operator Explicit(const Value: Int64): gmpRational; overload;
      class operator Explicit(const Value: UInt64): gmpRational; overload;
      class operator Explicit(const Value: Single): gmpRational; overload;
      class operator Explicit(const Value: Double): gmpRational; overload;
      class operator Explicit(const Value: AnsiString): gmpRational; overload; // base 10 by default
      class operator Explicit(const Value: gmpInteger): gmpRational; overload;
      class operator Explicit(const Value: pgmpRational): gmpRational; overload; // Please refer to TIP1
      class operator Explicit(const Value: pgmpFloat): gmpRational; overload;

      // Executed in expression "a.Assign(-b)" (both a and b are gmpRational variables.)
      class operator Negative(const Value: gmpRational): gmpRational;
      class operator Positive(const Value: gmpRational): gmpRational;

      // Not supported for memory management. Refer to tips of gmpInteger.
      // class operator Inc(const Value: gmpRational): gmpRational;
      // class operator Dec(const Value: gmpRational): gmpRational;

      // Not supported for gmpRational type
      // class operator Trunc(const Value: gmpRational): gmpRational;
      // class operator Round(const Value: gmpRational): gmpRational;

      // ################### Add operator functions ###################
      class operator Add(const Left, Right: gmpRational): gmpRational; overload;

      class operator Add(const Left: gmpRational; const Right: Integer): gmpRational; overload;
      class operator Add(const Left: gmpRational; const Right: Cardinal): gmpRational; overload;
      class operator Add(const Left: gmpRational; const Right: Int64): gmpRational; overload;
      class operator Add(const Left: gmpRational; const Right: UInt64): gmpRational; overload;
      class operator Add(const Left: gmpRational; const Right: Single): gmpRational; overload;
      class operator Add(const Left: gmpRational; const Right: Double): gmpRational; overload;
      // class operator Add(const Left: gmpRational; const Right: AnsiString): gmpRational; overload; // Please refer to TIP1 of gmpInteger
      class operator Add(const Left: gmpRational; const Right: gmpInteger): gmpRational; overload;
      class operator Add(const Left: gmpRational; const Right: pgmpRational): gmpRational; overload; // Please refer to TIP1

      class operator Add(const Left: Integer; const Right: gmpRational): gmpRational; overload;
      class operator Add(const Left: Cardinal; const Right: gmpRational): gmpRational; overload;
      class operator Add(const Left: Int64; const Right: gmpRational): gmpRational; overload;
      class operator Add(const Left: UInt64; const Right: gmpRational): gmpRational; overload;
      class operator Add(const Left: Single; const Right: gmpRational): gmpRational; overload;
      class operator Add(const Left: Double; const Right: gmpRational): gmpRational; overload;
      // class operator Add(const Left: AnsiString; const Right: gmpRational): gmpRational; overload; // Please refer to TIP1 of gmpInteger
      class operator Add(const Left: gmpInteger; const Right: gmpRational): gmpRational; overload;
      class operator Add(const Left: pgmpRational; const Right: gmpRational): gmpRational; overload; // Please refer to TIP1

      // ################### Subtract operator functions ###################
      class operator Subtract(const Left, Right: gmpRational): gmpRational; overload;

      class operator Subtract(const Left: gmpRational; const Right: Integer): gmpRational; overload;
      class operator Subtract(const Left: gmpRational; const Right: Cardinal): gmpRational; overload;
      class operator Subtract(const Left: gmpRational; const Right: Int64): gmpRational; overload;
      class operator Subtract(const Left: gmpRational; const Right: UInt64): gmpRational; overload;
      class operator Subtract(const Left: gmpRational; const Right: Single): gmpRational; overload;
      class operator Subtract(const Left: gmpRational; const Right: Double): gmpRational; overload;
      // class operator Subtract(const Left: gmpRational; const Right: AnsiString): gmpRational; overload; // Please refer to TIP1 of gmpInteger
      class operator Subtract(const Left: gmpRational; const Right: gmpInteger): gmpRational; overload;
      class operator Subtract(const Left: gmpRational; const Right: pgmpRational): gmpRational; overload; // Please refer to TIP1

      class operator Subtract(const Left: Integer; const Right: gmpRational): gmpRational; overload;
      class operator Subtract(const Left: Cardinal; const Right: gmpRational): gmpRational; overload;
      class operator Subtract(const Left: Int64; const Right: gmpRational): gmpRational; overload;
      class operator Subtract(const Left: UInt64; const Right: gmpRational): gmpRational; overload;
      class operator Subtract(const Left: Single; const Right: gmpRational): gmpRational; overload;
      class operator Subtract(const Left: Double; const Right: gmpRational): gmpRational; overload;
      // class operator Subtract(const Left: AnsiString; const Right: gmpRational): gmpRational; overload; // Please refer to TIP1 of gmpInteger
      class operator Subtract(const Left: gmpInteger; const Right: gmpRational): gmpRational; overload;
      class operator Subtract(const Left: pgmpRational; const Right: gmpRational): gmpRational; overload; // Please refer to TIP1

      // ################### Multiply operator functions ###################
      class operator Multiply(const Left, Right: gmpRational): gmpRational; overload;

      class operator Multiply(const Left: Integer; const Right: gmpRational): gmpRational; overload;
      class operator Multiply(const Left: Cardinal; const Right: gmpRational): gmpRational; overload;
      class operator Multiply(const Left: Int64; const Right: gmpRational): gmpRational; overload;
      class operator Multiply(const Left: UInt64; const Right: gmpRational): gmpRational; overload;
      class operator Multiply(const Left: Single; const Right: gmpRational): gmpRational; overload;
      class operator Multiply(const Left: Double; const Right: gmpRational): gmpRational; overload;
      class operator Multiply(const Left: gmpInteger; const Right: gmpRational): gmpRational; overload;
      class operator Multiply(const Left: pgmpRational; const Right: gmpRational): gmpRational; overload;

      class operator Multiply(const Left: gmpRational; const Right: Integer): gmpRational; overload;
      class operator Multiply(const Left: gmpRational; const Right: Cardinal): gmpRational; overload;
      class operator Multiply(const Left: gmpRational; const Right: Int64): gmpRational; overload;
      class operator Multiply(const Left: gmpRational; const Right: UInt64): gmpRational; overload;
      class operator Multiply(const Left: gmpRational; const Right: Single): gmpRational; overload;
      class operator Multiply(const Left: gmpRational; const Right: Double): gmpRational; overload;
      class operator Multiply(const Left: gmpRational; const Right: gmpInteger): gmpRational; overload;
      class operator Multiply(const Left: gmpRational; const Right: pgmpRational): gmpRational; overload;

      // ################### Division operator functions ###################
      class operator Divide(const Left, Right: gmpRational): gmpRational; overload;

      class operator Divide(const Left: Integer; const Right: gmpRational): gmpRational; overload;
      class operator Divide(const Left: Cardinal; const Right: gmpRational): gmpRational; overload;
      class operator Divide(const Left: Int64; const Right: gmpRational): gmpRational; overload;
      class operator Divide(const Left: UInt64; const Right: gmpRational): gmpRational; overload;
      // class operator Divide(const Left: AnsiString; const Right: gmpRational): gmpRational; overload; // Please refer to TIP1 of gmpInteger
      class operator Divide(const Left: Single; const Right: gmpRational): gmpRational; overload;
      class operator Divide(const Left: Double; const Right: gmpRational): gmpRational; overload;
      class operator Divide(const Left: gmpInteger; const Right: gmpRational): gmpRational; overload;
      class operator Divide(const Left: pgmpRational; const Right: gmpRational): gmpRational; overload;

      class operator Divide(const Left: gmpRational; const Right: Integer): gmpRational; overload;
      class operator Divide(const Left: gmpRational; const Right: Cardinal): gmpRational; overload;
      class operator Divide(const Left: gmpRational; const Right: Int64): gmpRational; overload;
      class operator Divide(const Left: gmpRational; const Right: UInt64): gmpRational; overload;
      // class operator Divide(const Left: gmpRational; const Right: AnsiString): gmpRational; overload; // Please refer to TIP1 of gmpInteger
      class operator Divide(const Left: gmpRational; const Right: Single): gmpRational; overload;
      class operator Divide(const Left: gmpRational; const Right: Double): gmpRational; overload;
      class operator Divide(const Left: gmpRational; const Right: gmpInteger): gmpRational; overload;
      class operator Divide(const Left: gmpRational; const Right: pgmpRational): gmpRational; overload;

      // IntDivide operation not supported for rational type
      // Modulus operation not supported for rational type

      // ################### Equal operator functions ###################
      class operator Equal(const Left, Right: gmpRational): Boolean; overload;

      class operator Equal(const Left: gmpRational; const Right: Integer): Boolean; overload;
      class operator Equal(const Left: gmpRational; const Right: Cardinal): Boolean; overload;
      class operator Equal(const Left: gmpRational; const Right: Int64): Boolean; overload;
      class operator Equal(const Left: gmpRational; const Right: UInt64): Boolean; overload;
      class operator Equal(const Left: gmpRational; const Right: Single): Boolean; overload;
      class operator Equal(const Left: gmpRational; const Right: Double): Boolean; overload;
      // class operator Equal(const Left: gmpRational; const Right: AnsiString): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator Equal(const Left: gmpRational; const Right: gmpInteger): Boolean; overload;
      class operator Equal(const Left: gmpRational; const Right: pgmpRational): Boolean; overload; // Please refer to TIP1

      class operator Equal(const Left: Integer; const Right: gmpRational): Boolean; overload;
      class operator Equal(const Left: Cardinal; const Right: gmpRational): Boolean; overload;
      class operator Equal(const Left: Int64; const Right: gmpRational): Boolean; overload;
      class operator Equal(const Left: UInt64; const Right: gmpRational): Boolean; overload;
      class operator Equal(const Left: Single; const Right: gmpRational): Boolean; overload;
      class operator Equal(const Left: Double; const Right: gmpRational): Boolean; overload;
      // class operator Equal(const Left: AnsiString; const Right: gmpRational): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator Equal(const Left: gmpInteger; const Right: gmpRational): Boolean; overload;
      class operator Equal(const Left: pgmpRational; const Right: gmpRational): Boolean; overload; // Please refer to TIP1

      // ################### Not-Equal operator functions ###################
      class operator NotEqual(const Left, Right: gmpRational): Boolean; overload;

      class operator NotEqual(const Left: gmpRational; const Right: Integer): Boolean; overload;
      class operator NotEqual(const Left: gmpRational; const Right: Cardinal): Boolean; overload;
      class operator NotEqual(const Left: gmpRational; const Right: Int64): Boolean; overload;
      class operator NotEqual(const Left: gmpRational; const Right: UInt64): Boolean; overload;
      class operator NotEqual(const Left: gmpRational; const Right: Single): Boolean; overload;
      class operator NotEqual(const Left: gmpRational; const Right: Double): Boolean; overload;
      // class operator NotEqual(const Left: gmpRational; const Right: AnsiString): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator NotEqual(const Left: gmpRational; const Right: gmpInteger): Boolean; overload;
      class operator NotEqual(const Left: gmpRational; const Right: pgmpRational): Boolean; overload; // Please refer to TIP1

      class operator NotEqual(const Left: Integer; const Right: gmpRational): Boolean; overload;
      class operator NotEqual(const Left: Cardinal; const Right: gmpRational): Boolean; overload;
      class operator NotEqual(const Left: Int64; const Right: gmpRational): Boolean; overload;
      class operator NotEqual(const Left: UInt64; const Right: gmpRational): Boolean; overload;
      class operator NotEqual(const Left: Single; const Right: gmpRational): Boolean; overload;
      class operator NotEqual(const Left: Double; const Right: gmpRational): Boolean; overload;
      // class operator NotEqual(const Left: AnsiString; const Right: gmpRational): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator NotEqual(const Left: gmpInteger; const Right: gmpRational): Boolean; overload;
      class operator NotEqual(const Left: pgmpRational; const Right: gmpRational): Boolean; overload; // Please refer to TIP1

      // ################### Greater-Than operator functions ###################
      class operator GreaterThan(const Left, Right: gmpRational): Boolean; overload;

      class operator GreaterThan(const Left: gmpRational; const Right: Integer): Boolean; overload;
      class operator GreaterThan(const Left: gmpRational; const Right: Cardinal): Boolean; overload;
      class operator GreaterThan(const Left: gmpRational; const Right: Int64): Boolean; overload;
      class operator GreaterThan(const Left: gmpRational; const Right: UInt64): Boolean; overload;
      class operator GreaterThan(const Left: gmpRational; const Right: Single): Boolean; overload;
      class operator GreaterThan(const Left: gmpRational; const Right: Double): Boolean; overload;
      // class operator GreaterThan(const Left: gmpRational; const Right: AnsiString): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator GreaterThan(const Left: gmpRational; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThan(const Left: gmpRational; const Right: pgmpRational): Boolean; overload; // Please refer to TIP1

      class operator GreaterThan(const Left: Integer; const Right: gmpRational): Boolean; overload;
      class operator GreaterThan(const Left: Cardinal; const Right: gmpRational): Boolean; overload;
      class operator GreaterThan(const Left: Int64; const Right: gmpRational): Boolean; overload;
      class operator GreaterThan(const Left: UInt64; const Right: gmpRational): Boolean; overload;
      class operator GreaterThan(const Left: Single; const Right: gmpRational): Boolean; overload;
      class operator GreaterThan(const Left: Double; const Right: gmpRational): Boolean; overload;
      // class operator GreaterThan(const Left: AnsiString; const Right: gmpRational): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator GreaterThan(const Left: gmpInteger; const Right: gmpRational): Boolean; overload;
      class operator GreaterThan(const Left: pgmpRational; const Right: gmpRational): Boolean; overload; // Please refer to TIP1

      // ################### GreaterThanOrEqual operator functions ###################
      class operator GreaterThanOrEqual(const Left, Right: gmpRational): Boolean; overload;

      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: Integer): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: Cardinal): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: Int64): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: UInt64): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: Single): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: Double): Boolean; overload;
      // class operator GreaterThanOrEqual(const Left: gmpRational; const Right: AnsiString): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: pgmpRational): Boolean; overload; // Please refer to TIP1

      class operator GreaterThanOrEqual(const Left: Integer; const Right: gmpRational): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Cardinal; const Right: gmpRational): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Int64; const Right: gmpRational): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: UInt64; const Right: gmpRational): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Single; const Right: gmpRational): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Double; const Right: gmpRational): Boolean; overload;
      // class operator GreaterThanOrEqual(const Left: AnsiString; const Right: gmpRational): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: gmpRational): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: pgmpRational; const Right: gmpRational): Boolean; overload; // Please refer to TIP1

      // ################### Less-Than operator functions ###################
      class operator LessThan(const Left, Right: gmpRational): Boolean; overload;

      class operator LessThan(const Left: gmpRational; const Right: Integer): Boolean; overload;
      class operator LessThan(const Left: gmpRational; const Right: Cardinal): Boolean; overload;
      class operator LessThan(const Left: gmpRational; const Right: Int64): Boolean; overload;
      class operator LessThan(const Left: gmpRational; const Right: UInt64): Boolean; overload;
      class operator LessThan(const Left: gmpRational; const Right: Single): Boolean; overload;
      class operator LessThan(const Left: gmpRational; const Right: Double): Boolean; overload;
      // class operator LessThan(const Left: gmpRational; const Right: AnsiString): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator LessThan(const Left: gmpRational; const Right: gmpInteger): Boolean; overload;
      class operator LessThan(const Left: gmpRational; const Right: pgmpRational): Boolean; overload; // Please refer to TIP1

      class operator LessThan(const Left: Integer; const Right: gmpRational): Boolean; overload;
      class operator LessThan(const Left: Cardinal; const Right: gmpRational): Boolean; overload;
      class operator LessThan(const Left: Int64; const Right: gmpRational): Boolean; overload;
      class operator LessThan(const Left: UInt64; const Right: gmpRational): Boolean; overload;
      class operator LessThan(const Left: Single; const Right: gmpRational): Boolean; overload;
      class operator LessThan(const Left: Double; const Right: gmpRational): Boolean; overload;
      // class operator LessThan(const Left: AnsiString; const Right: gmpRational): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator LessThan(const Left: gmpInteger; const Right: gmpRational): Boolean; overload;
      class operator LessThan(const Left: pgmpRational; const Right: gmpRational): Boolean; overload; // Please refer to TIP1

      // ################### LessThanOrEqual operator functions ###################
      class operator LessThanOrEqual(const Left, Right: gmpRational): Boolean; overload;

      class operator LessThanOrEqual(const Left: gmpRational; const Right: Integer): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpRational; const Right: Cardinal): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpRational; const Right: Int64): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpRational; const Right: UInt64): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpRational; const Right: Single): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpRational; const Right: Double): Boolean; overload;
      // class operator LessThanOrEqual(const Left: gmpRational; const Right: AnsiString): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator LessThanOrEqual(const Left: gmpRational; const Right: gmpInteger): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpRational; const Right: pgmpRational): Boolean; overload; // Please refer to TIP1

      class operator LessThanOrEqual(const Left: Integer; const Right: gmpRational): Boolean; overload;
      class operator LessThanOrEqual(const Left: Cardinal; const Right: gmpRational): Boolean; overload;
      class operator LessThanOrEqual(const Left: Int64; const Right: gmpRational): Boolean; overload;
      class operator LessThanOrEqual(const Left: UInt64; const Right: gmpRational): Boolean; overload;
      class operator LessThanOrEqual(const Left: Single; const Right: gmpRational): Boolean; overload;
      class operator LessThanOrEqual(const Left: Double; const Right: gmpRational): Boolean; overload;
      // class operator LessThanOrEqual(const Left: AnsiString; const Right: gmpRational): Boolean; overload; // Please refer to TIP1 of gmpInteger
      class operator LessThanOrEqual(const Left: gmpInteger; const Right: gmpRational): Boolean; overload;
      class operator LessThanOrEqual(const Left: pgmpRational; const Right: gmpRational): Boolean; overload; // Please refer to TIP1

      // Logical operation not supported for rational type
      // Bitwise operation not supported for rational type
      // Shift operation not supported for rational type
   end;

   { TIPS ABOUT gmpFloat
     [1] String is regarded as gmpFloat by default, so you can you should cast a string
          to gmpInteger explicitly using gmpInteger(AString).
     [2] Support pointer because some operations of gmpInteger return pgmpRational type.

   }

   gmpFloat = packed record
   private
      // The actual type for distinguishing the variable
      NativeType: TGMPObjectType;

      // For temporary gmpXXX objects.
      RelativeNode: Pointer;

      PrecisionBitCount: mp_bitcnt_t;

      procedure CreateGeneric(Precision: Cardinal = 0);

      function FGetIsTemporary: Boolean; inline;
      function FGetIsInteger: Boolean; inline;
      function FGetPrecision: mp_bitcnt_t;
      procedure FSetPrecision(Value: mp_bitcnt_t);

      procedure ReleaseTemporary;
      property IsTemporary: Boolean read FGetIsTemporary;

   public
      // Pointer to the native prototype gmp object
      NativeObject: pmpf_t;

      // Constructors for delphi native types. These functions will create
      // gmpFloat with an initial value.
      // You can specifically set the precision of this float by @Precision
      // parameter. @Precision is optional and zero means to use the default precision.
      constructor Create(const Value: Integer; Precision: mp_bitcnt_t = 0); overload;
      constructor Create(const Value: Cardinal; Precision: mp_bitcnt_t = 0); overload;
      constructor Create(const Value: Int64; Precision: mp_bitcnt_t = 0); overload;
      constructor Create(const Value: UInt64; Precision: mp_bitcnt_t = 0); overload;
      constructor Create(const Value: Single; Precision: mp_bitcnt_t = 0); overload;
      constructor Create(const Value: Double; Precision: mp_bitcnt_t = 0); overload;

      // [TIP] Create gmpRational from a string.
      // Please refer to GMP manual for details about float format.
      constructor Create(const Value: AnsiString; Precision: mp_bitcnt_t = 0); overload; // base 10 by default
      constructor Create(const Value: AnsiString; Base: Integer; Precision: mp_bitcnt_t); overload;

      constructor Create(const Value: gmpInteger; Precision: mp_bitcnt_t = 0); overload;
      constructor Create(const Value: gmpRational; Precision: mp_bitcnt_t = 0); overload;
      constructor Create(const Value: gmpFloat; Precision: mp_bitcnt_t = 0); overload;
      constructor Create(const Value: pgmpRational; Precision: mp_bitcnt_t = 0); overload; // Refer to TIP2

      // [TIP] Call this destructor after a gmpFloat type is no longer used.
      // Also you can use gmpFree procedure in gmp_util unit to free a series of gmpXXXX objects.
      procedure Free;

      // [TIP] Assign functions. Must be used after constructor is called.
      procedure Assign(const Value: Integer); overload; inline;
      procedure Assign(const Value: Cardinal); overload; inline;
      procedure Assign(const Value: Int64); overload;
      procedure Assign(const Value: UInt64); overload;
      procedure Assign(const Value: Single); overload; inline;
      procedure Assign(const Value: Double); overload; inline;
      procedure Assign(const Value: AnsiString); overload; // base 10 by default
      procedure Assign(const Value: AnsiString; Base: Integer); overload;
      procedure Assign(const Value: gmpInteger); overload;
      procedure Assign(const Value: gmpRational); overload;
      procedure Assign(const Value: gmpFloat); overload;
      procedure Assign(const Value: pgmpRational); overload;  // Refer to TIP2

      // Set functions
      procedure SetZero; inline; // Equals to "Float.Assign(0)"
      procedure SetNegative; inline; // More efficient than "Float.Assign(-Rational)"
      procedure SetAbsolute; inline; // Set A to Abs(A)
      procedure SetReciprocal; // Set A to 1/A.

      // These functions will not change current precision, so reset precision
      // beforehand if a specific precision is wanted.
      procedure SetSquare; inline; // Set A to A^2
      procedure SetCubic; inline; // Set A to A^3
      procedure SetIntPower(Exp: Cardinal); inline; // Set A to A^Exp
      procedure SetPower(Exp: gmpFloat);
      procedure SetSqrt; inline; // Set A to A^(1/2)
      procedure SetNthRoot(N: Cardinal); // Set A to A^(1/N)
      procedure SetExp; inline; // Set A to e^A
      procedure SetLn; inline; // Set A to Ln(A)

      procedure SetMul2Exp(const Base: gmpFloat; N: mp_bitcnt_t); // Set A to Base*2^N
      procedure SetDiv2Exp(const Base: gmpFloat; N: mp_bitcnt_t); // Set A to Base/2^N

      procedure SetRandom;

      // ################### Output methods ###################
      function ToInteger: Integer; inline;
      function ToCardinal: Cardinal; inline;
      function ToDouble: Double; inline;
      procedure To2Exp(var D: Double; var Exp: Integer); inline; // Return gmpFloat in format D*2^Exp and 0.5 <= |D| < 1

      // Output float in natural format "3.1416". All significant digits will be output.
      function ToString(Base: Integer = 10): AnsiString;
      // Output float in format "0.31416 E 1" for 3.1416.
      function ToStringExp(Base: Integer = 10; DigitCount: Integer = 0): AnsiString;  // Digit = 0 means to get all significant digits.
      // Output float with raw string "31416" and returns Exp 1.
      function ToStringRaw(var Exp: Integer; Base: Integer = 10; DigitCount: Integer = 0): AnsiString;

      // ################### Increment methods ###################
      procedure Inc; overload; inline;
      procedure Inc(const Value: Integer); overload;
      procedure Inc(const Value: Cardinal); overload; inline;
      procedure Inc(const Value: Int64); overload;
      procedure Inc(const Value: UInt64); overload;
      procedure Inc(const Value: Single); overload;
      procedure Inc(const Value: Double); overload;
      procedure Inc(const Value: AnsiString); overload; // base 10 by default
      procedure Inc(const Value: AnsiString; Base: Integer); overload;
      procedure Inc(const Value: gmpInteger); overload;
      procedure Inc(const Value: gmpRational); overload;
      procedure Inc(const Value: gmpFloat); overload;
      procedure Inc(const Value: pgmpRational); overload; // Refer to TIP2

      // ################### Decrement methods ###################
      procedure Dec; overload; inline;
      procedure Dec(const Value: Integer); overload;
      procedure Dec(const Value: Cardinal); overload; inline;
      procedure Dec(const Value: Int64); overload;
      procedure Dec(const Value: UInt64); overload;
      procedure Dec(const Value: Single); overload;
      procedure Dec(const Value: Double); overload;
      procedure Dec(const Value: AnsiString); overload; // base 10 by default
      procedure Dec(const Value: AnsiString; Base: Integer); overload;
      procedure Dec(const Value: gmpInteger); overload;
      procedure Dec(const Value: gmpRational); overload;
      procedure Dec(const Value: gmpFloat); overload;
      procedure Dec(const Value: pgmpRational); overload; // Refer to TIP2

      // ################### Multiply and Divide methods ###################
      procedure MultiplyBy(const Value: Integer); overload;
      procedure MultiplyBy(const Value: Cardinal); overload;
      procedure MultiplyBy(const Value: Int64); overload;
      procedure MultiplyBy(const Value: UInt64); overload;
      procedure MultiplyBy(const Value: Single); overload;
      procedure MultiplyBy(const Value: Double); overload;
      procedure MultiplyBy(const Value: gmpInteger); overload;
      procedure MultiplyBy(const Value: gmpRational); overload;
      procedure MultiplyBy(const Value: pgmpRational); overload;
      procedure MultiplyBy(const Value: gmpFloat); overload;

      procedure DivideBy(const Value: Integer); overload;
      procedure DivideBy(const Value: Cardinal); overload;
      procedure DivideBy(const Value: Int64); overload;
      procedure DivideBy(const Value: UInt64); overload;
      procedure DivideBy(const Value: Single); overload;
      procedure DivideBy(const Value: Double); overload;
      procedure DivideBy(const Value: gmpInteger); overload;
      procedure DivideBy(const Value: gmpRational); overload;
      procedure DivideBy(const Value: pgmpRational); overload;
      procedure DivideBy(const Value: gmpFloat); overload;

      // Float bit-shift functions
      procedure Mul2Exp(const N: mp_bitcnt_t); inline; // Set A to A*2^N
      procedure Div2Exp(const N: mp_bitcnt_t); inline; // Set A to A/2^N

      // Operators

      // [TIP] Refer to tips about explicit-casting methods of gmpInteger.
      class operator Explicit(const Value: Integer): gmpFloat; overload;
      class operator Explicit(const Value: Cardinal): gmpFloat; overload;
      class operator Explicit(const Value: Int64): gmpFloat; overload;
      class operator Explicit(const Value: UInt64): gmpFloat; overload;
      class operator Explicit(const Value: Single): gmpFloat; overload;
      class operator Explicit(const Value: Double): gmpFloat; overload;
      class operator Explicit(const Value: AnsiString): gmpFloat; overload; // base 10 by default
      class operator Explicit(const Value: gmpInteger): gmpFloat; overload;
      class operator Explicit(const Value: gmpRational): gmpFloat; overload;
      class operator Explicit(const Value: pgmpRational): gmpFloat; overload; // Refer to TIP2

      // Executed in expression "a.Assign(-b)" (both a and b are gmpFloat variables.)
      class operator Negative(const Value: gmpFloat): gmpFloat;
      class operator Positive(const Value: gmpFloat): gmpFloat;

      // Not supported for memory management. Refer to tips of gmpInteger.
      // class operator Inc(const Value: gmpFloat): gmpFloat;
      // class operator Dec(const Value: gmpFloat): gmpFloat;

      // ################### Trunc operator function ###################
      class operator Trunc(const Value: gmpFloat): gmpFloat; // Note that trunc returns a gmpFloat, not a gmpInteger.
                                                             // Please refer to TIP1 of gmpFloat.

      // Not supported for gmpFloat type
      // class operator Round(const Value: gmpFloat): gmpFloat;

      // ################### Add operator functions ###################
      class operator Add(const Left, Right: gmpFloat): gmpFloat; overload;

      class operator Add(const Left: gmpFloat; const Right: Integer): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: Cardinal): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: Int64): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: UInt64): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: Single): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: Double): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: AnsiString): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: gmpInteger): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: gmpRational): gmpFloat; overload;
      class operator Add(const Left: gmpFloat; const Right: pgmpRational): gmpFloat; overload; // Refer to TIP2

      class operator Add(const Left: Integer; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: Cardinal; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: Int64; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: UInt64; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: Single; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: Double; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: AnsiString; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: gmpInteger; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: gmpRational; const Right: gmpFloat): gmpFloat; overload;
      class operator Add(const Left: pgmpRational; const Right: gmpFloat): gmpFloat; overload; // Refer to TIP2

      // ################### Subtract operator functions ###################
      class operator Subtract(const Left, Right: gmpFloat): gmpFloat; overload;

      class operator Subtract(const Left: gmpFloat; const Right: Integer): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: Cardinal): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: Int64): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: UInt64): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: Single): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: Double): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: AnsiString): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: gmpInteger): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: gmpRational): gmpFloat; overload;
      class operator Subtract(const Left: gmpFloat; const Right: pgmpRational): gmpFloat; overload; // Refer to TIP2

      class operator Subtract(const Left: Integer; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: Cardinal; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: Int64; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: UInt64; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: Single; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: Double; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: AnsiString; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: gmpInteger; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: gmpRational; const Right: gmpFloat): gmpFloat; overload;
      class operator Subtract(const Left: pgmpRational; const Right: gmpFloat): gmpFloat; overload; // Refer to TIP2

      // ################### Multiply operator functions ###################
      class operator Multiply(const Left, Right: gmpFloat): gmpFloat; overload;

      class operator Multiply(const Left: Integer; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: Cardinal; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: Int64; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: UInt64; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: Single; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: Double; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: AnsiString; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: gmpInteger; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: gmpRational; const Right: gmpFloat): gmpFloat; overload;
      class operator Multiply(const Left: pgmpRational; const Right: gmpFloat): gmpFloat; overload; // Refer to TIP2

      class operator Multiply(const Left: gmpFloat; const Right: Integer): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: Cardinal): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: Int64): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: UInt64): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: Single): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: Double): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: AnsiString): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: gmpInteger): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: gmpRational): gmpFloat; overload;
      class operator Multiply(const Left: gmpFloat; const Right: pgmpRational): gmpFloat; overload; // Refer to TIP2

      // ################### Division operator functions ###################
      class operator Divide(const Left, Right: gmpFloat): gmpFloat; overload;

      class operator Divide(const Left: Integer; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: Cardinal; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: Int64; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: UInt64; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: AnsiString; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: Single; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: Double; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: gmpInteger; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: gmpRational; const Right: gmpFloat): gmpFloat; overload;
      class operator Divide(const Left: pgmpRational; const Right: gmpFloat): gmpFloat; overload; // Refer to TIP2

      class operator Divide(const Left: gmpFloat; const Right: Integer): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: Cardinal): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: Int64): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: UInt64): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: AnsiString): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: Single): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: Double): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: gmpInteger): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: gmpRational): gmpFloat; overload;
      class operator Divide(const Left: gmpFloat; const Right: pgmpRational): gmpFloat; overload; // Refer to TIP2

      // IntDivide operation not supported for float type
      // Modulus operation not supported for float type

      // ################### Equal operator functions ###################
      class operator Equal(const Left, Right: gmpFloat): Boolean; overload;

      class operator Equal(const Left: gmpFloat; const Right: Integer): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: Cardinal): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: Int64): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: UInt64): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: Single): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: Double): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: AnsiString): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: gmpInteger): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: gmpRational): Boolean; overload;
      class operator Equal(const Left: gmpFloat; const Right: pgmpRational): Boolean; overload; // Refer to TIP2

      class operator Equal(const Left: Integer; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: Cardinal; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: Int64; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: UInt64; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: Single; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: Double; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: AnsiString; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: gmpInteger; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: gmpRational; const Right: gmpFloat): Boolean; overload;
      class operator Equal(const Left: pgmpRational; const Right: gmpFloat): Boolean; overload; // Refer to TIP2

      // ################### Not-Equal operator functions ###################
      class operator NotEqual(const Left, Right: gmpFloat): Boolean; overload;

      class operator NotEqual(const Left: gmpFloat; const Right: Integer): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: Cardinal): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: Int64): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: UInt64): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: Single): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: Double): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: AnsiString): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: gmpInteger): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: gmpRational): Boolean; overload;
      class operator NotEqual(const Left: gmpFloat; const Right: pgmpRational): Boolean; overload; // Refer to TIP2

      class operator NotEqual(const Left: Integer; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: Cardinal; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: Int64; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: UInt64; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: Single; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: Double; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: AnsiString; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: gmpInteger; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: gmpRational; const Right: gmpFloat): Boolean; overload;
      class operator NotEqual(const Left: pgmpRational; const Right: gmpFloat): Boolean; overload; // Refer to TIP2

      // ################### Greater-Than operator functions ###################
      class operator GreaterThan(const Left, Right: gmpFloat): Boolean; overload;

      class operator GreaterThan(const Left: gmpFloat; const Right: Integer): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: Cardinal): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: Int64): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: UInt64): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: Single): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: Double): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: AnsiString): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: gmpRational): Boolean; overload;
      class operator GreaterThan(const Left: gmpFloat; const Right: pgmpRational): Boolean; overload; // Refer to TIP2

      class operator GreaterThan(const Left: Integer; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: Cardinal; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: Int64; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: UInt64; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: Single; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: Double; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: AnsiString; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: gmpInteger; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: gmpRational; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThan(const Left: pgmpRational; const Right: gmpFloat): Boolean; overload; // Refer to TIP2

      // ################### GreaterThanOrEqual operator functions ###################
      class operator GreaterThanOrEqual(const Left, Right: gmpFloat): Boolean; overload;

      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: Integer): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: Cardinal): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: Int64): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: UInt64): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: Single): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: Double): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: AnsiString): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: gmpInteger): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: gmpRational): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpFloat; const Right: pgmpRational): Boolean; overload; // Refer to TIP2

      class operator GreaterThanOrEqual(const Left: Integer; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Cardinal; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Int64; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: UInt64; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Single; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: Double; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: AnsiString; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpInteger; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: gmpRational; const Right: gmpFloat): Boolean; overload;
      class operator GreaterThanOrEqual(const Left: pgmpRational; const Right: gmpFloat): Boolean; overload; // Refer to TIP2

      // ################### Less-Than operator functions ###################
      class operator LessThan(const Left, Right: gmpFloat): Boolean; overload;

      class operator LessThan(const Left: gmpFloat; const Right: Integer): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: Cardinal): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: Int64): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: UInt64): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: Single): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: Double): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: AnsiString): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: gmpInteger): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: gmpRational): Boolean; overload;
      class operator LessThan(const Left: gmpFloat; const Right: pgmpRational): Boolean; overload; // Refer to TIP2

      class operator LessThan(const Left: Integer; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: Cardinal; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: Int64; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: UInt64; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: Single; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: Double; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: AnsiString; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: gmpInteger; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: gmpRational; const Right: gmpFloat): Boolean; overload;
      class operator LessThan(const Left: pgmpRational; const Right: gmpFloat): Boolean; overload; // Refer to TIP2

      // ################### LessThanOrEqual operator functions ###################
      class operator LessThanOrEqual(const Left, Right: gmpFloat): Boolean; overload;

      class operator LessThanOrEqual(const Left: gmpFloat; const Right: Integer): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: Cardinal): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: Int64): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: UInt64): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: Single): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: Double): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: AnsiString): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: gmpInteger): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: gmpRational): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpFloat; const Right: pgmpRational): Boolean; overload; // Refer to TIP2

      class operator LessThanOrEqual(const Left: Integer; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: Cardinal; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: Int64; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: UInt64; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: Single; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: Double; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: AnsiString; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpInteger; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: gmpRational; const Right: gmpFloat): Boolean; overload;
      class operator LessThanOrEqual(const Left: pgmpRational; const Right: gmpFloat): Boolean; overload; // Refer to TIP2

      // Logical operation not supported for float type
      // Bitwise operation not supported for float type
      // Shift operation not supported for float type

      property IsInteger: Boolean read FGetIsInteger;
      property Precision: mp_bitcnt_t read FGetPrecision write FSetPrecision;
   end;

   ///////////////////////////////////////////////////////////////////////

   gmpException = class(Exception)
   private
      FErrorCode: TGMPExceptionCode;
   public
      constructor Create(ErrorCode: TGMPExceptionCode); overload;
      // Raise and release temporary objects.
      constructor Create(ErrorCode: TGMPExceptionCode; const Obj: gmpInteger); overload;
      constructor Create(ErrorCode: TGMPExceptionCode; const Obj: gmpRational); overload;
      constructor Create(ErrorCode: TGMPExceptionCode; const Obj: gmpFloat); overload;

      property ErrorCode: TGMPExceptionCode read FErrorCode;
   end;

   //////////////////////////////////////////////////////////////////////

   // Set the default precision to be at least @Value bits. All subsequently
   // created gmpFloats will use this precision, but previously initialized
   // variables are unaffected. Note that this call will check the precision
   // of all temporary floats and constants to keep intermediate calculation
   // precise. This process is time consuming and shouldn't be used in tight loop.
   // !! Whenever a float of higer precision is created, all temporary floats and
   // !! constants are reallocated. So try to avoid creating a single float using
   // !! a unique precision. If you have to, call ResetDefaultFloatPrecision
   // !! afterwards.
   procedure SetDefaultFloatPrecision(Value: mp_bitcnt_t);
   procedure ResetDefaultFloatPrecision(Value: mp_bitcnt_t);
   function GetDefaultFloatPrecision: mp_bitcnt_t;

   // Allocate memories for variables and initialize them to default value(zero).
   // typeDef defines the types of the variables:
   // 'I' or 'i' for gmpInteger, 'R' or 'r' for gmpRational, 'F' or 'f' for gmpFloat
   procedure gmpCreate(const Args: array of Pointer; const typeDef: AnsiString); overload;

   // Create a series of gmpInteger variables. Args must be array of the pointers to gmpInteger.
   procedure gmpCreateI(const Args: array of Pointer);

   // Create a series of gmpRational variables. Args must be array of the pointers to gmpRational.
   procedure gmpCreateR(const Args: array of Pointer);

   // Create a series of gmpFloat variables. Args must be array of the pointers to gmpFloat.
   procedure gmpCreateF(const Args: array of Pointer);

   // Free memories of variables. Args must be array of the pointers to the variables.
   procedure gmpFree(const Args: array of Pointer); overload;

   // Free mnemories of a gmpXXX variable.
   procedure gmpFree(p: Pointer); overload;

   // Get object type referenced by a pointer.
   function gmpObjectType(p: Pointer): TGMPObjectType; inline;

   // Quick swap the value of A and B
   procedure gmpSwap(const A, B: gmpInteger); overload;
   procedure gmpSwap(const A, B: gmpRational); overload;
   procedure gmpSwap(const A: pgmpRational; const B: gmpRational); overload; inline;
   procedure gmpSwap(const A: gmpRational; const B: pgmpRational); overload; inline;
   procedure gmpSwap(const A, B: gmpFloat); overload;

   // Min/Max of 2 values
   function gmpMin(const A, B: gmpInteger): gmpInteger; overload;
   function gmpMin(const A, B: gmpRational): gmpRational; overload;
   function gmpMin(const A: pgmpRational; const B: gmpRational): gmpRational; overload; inline;
   function gmpMin(const A: gmpRational; const B: pgmpRational): gmpRational; overload; inline;
   function gmpMin(const A, B: gmpFloat): gmpFloat; overload;

   function gmpMax(const A, B: gmpInteger): gmpInteger; overload;
   function gmpMax(const A, B: gmpRational): gmpRational; overload;
   function gmpMax(const A: pgmpRational; const B: gmpRational): gmpRational; overload; inline;
   function gmpMax(const A: gmpRational; const B: pgmpRational): gmpRational; overload; inline;
   function gmpMax(const A, B: gmpFloat): gmpFloat; overload;

   // Arithmetic functions
   function gmpAbs(const Value: gmpInteger): gmpInteger; overload;
   function gmpAbs(const Value: gmpRational): gmpRational; overload;
   function gmpAbs(const Value: pgmpRational): gmpRational; overload; inline;
   function gmpAbs(const Value: gmpFloat): gmpFloat; overload;

   // Calculate e^Value
   function gmpExp(const Value: gmpInteger): gmpFloat; overload;
   function gmpExp(const Value: gmpRational): gmpFloat; overload;
   function gmpExp(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpExp(const Value: gmpFloat): gmpFloat; overload;

   // Calculate logarithm base of e
   function gmpLn(const Value: gmpInteger): gmpFloat; overload;
   function gmpLn(const Value: gmpRational): gmpFloat; overload;
   function gmpLn(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpLn(const Value: gmpFloat): gmpFloat; overload;

   // Calculate logarithm base of 2
   function gmpLog2(const Value: gmpInteger): gmpFloat; overload;
   function gmpLog2(const Value: gmpRational): gmpFloat; overload;
   function gmpLog2(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpLog2(const Value: gmpFloat): gmpFloat; overload;

   // Calculate logarithm base of 10
   function gmpLog10(const Value: gmpInteger): gmpFloat; overload;
   function gmpLog10(const Value: gmpRational): gmpFloat; overload;
   function gmpLog10(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpLog10(const Value: gmpFloat): gmpFloat; overload;

   // Calculate logarithm base of N
   function gmpLog(const Value: gmpInteger; const Base: Double): gmpFloat; overload;
   function gmpLog(const Value: gmpRational; const Base: Double): gmpFloat; overload;
   function gmpLog(const Value: pgmpRational; const Base: Double): gmpFloat; overload; inline;
   function gmpLog(const Value: gmpFloat; const Base: Double): gmpFloat; overload;
   function gmpLog(const Value: gmpInteger; const Base: gmpFloat): gmpFloat; overload;
   function gmpLog(const Value: gmpRational; const Base: gmpFloat): gmpFloat; overload;
   function gmpLog(const Value: pgmpRational; const Base: gmpFloat): gmpFloat; overload; inline;
   function gmpLog(const Value: gmpFloat; const Base: gmpFloat): gmpFloat; overload;

   // Calculate square
   function gmpSqr(const Value: gmpInteger): gmpInteger; overload;
   function gmpSqr(const Value: gmpRational): gmpRational; overload;
   function gmpSqr(const Value: pgmpRational): gmpRational; overload; inline;
   function gmpSqr(const Value: gmpFloat): gmpFloat; overload;

   // Calculate square root
   function gmpSqrt(const Value: gmpInteger): gmpFloat; overload;
   function gmpSqrt(const Value: gmpRational): gmpFloat; overload;
   function gmpSqrt(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpSqrt(const Value: gmpFloat): gmpFloat; overload;

   // Get truncated integer part of Sqrt(A)
   function gmpIntSqrt(const Value: gmpInteger): gmpInteger;
   // Get "A - gmpIntSqrt(A)^2"
   function gmpIntSqrtRem(const Value: gmpInteger): gmpInteger;

   // Calculate cubic(^3)
   function gmpCubic(const Value: gmpInteger): gmpInteger; overload;
   function gmpCubic(const Value: gmpRational): gmpRational; overload;
   function gmpCubic(const Value: pgmpRational): gmpRational; overload; inline;
   function gmpCubic(const Value: gmpFloat): gmpFloat; overload;

   // Calculate Value^Exp
   function gmpIntPower(const Value: gmpInteger; Exp: Cardinal): gmpInteger; overload;
   function gmpIntPower(const Value: gmpRational; Exp: Cardinal): gmpRational; overload;
   function gmpIntPower(const Value: pgmpRational; Exp: Cardinal): gmpRational; overload; inline;
   function gmpIntPower(const Value: gmpFloat; Exp: Cardinal): gmpFloat; overload;

   // Calculate Value^Exp
   function gmpPower(const Value: gmpInteger; Exp: Double): gmpFloat; overload;
   function gmpPower(const Value: gmpRational; Exp: Double): gmpFloat; overload;
   function gmpPower(const Value: pgmpRational; Exp: Double): gmpFloat; overload; inline;
   function gmpPower(const Value: gmpFloat; Exp: Double): gmpFloat; overload;
   function gmpPower(const Value: gmpInteger; Exp: gmpFloat): gmpFloat; overload;
   function gmpPower(const Value: gmpRational; Exp: gmpFloat): gmpFloat; overload;
   function gmpPower(const Value: pgmpRational; Exp: gmpFloat): gmpFloat; overload; inline;
   function gmpPower(const Value: gmpFloat; Exp: gmpFloat): gmpFloat; overload;

   // Calculate Nth Root
   function gmpRoot(const Value: gmpInteger; N: Cardinal): gmpFloat; overload;
   function gmpRoot(const Value: gmpRational; N: Cardinal): gmpFloat; overload;
   function gmpRoot(const Value: pgmpRational; N: Cardinal): gmpFloat; overload; inline;
   function gmpRoot(const Value: gmpFloat; N: Cardinal): gmpFloat; overload;

   // Get truncated integer part of Nth root of A.
   function gmpIntRoot(const Value: gmpInteger; N: Cardinal): gmpInteger;
   // Get "A - gmpIntRoot(A)^N"
   function gmpIntRootRem(const Value: gmpInteger; N: Cardinal): gmpInteger;

   // Trigonometric functions
   procedure gmpSinCos(const Value: gmpInteger; var sinValue, cosValue: gmpFloat); overload;
   procedure gmpSinCos(const Value: gmpRational; var sinValue, cosValue: gmpFloat); overload;
   procedure gmpSinCos(const Value: pgmpRational; var sinValue, cosValue: gmpFloat); overload; inline;
   procedure gmpSinCos(const Value: gmpFloat; var sinValue, cosValue: gmpFloat); overload;

   function gmpSin(const Value: gmpInteger): gmpFloat; overload;
   function gmpSin(const Value: gmpRational): gmpFloat; overload;
   function gmpSin(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpSin(const Value: gmpFloat): gmpFloat; overload;

   function gmpCos(const Value: gmpInteger): gmpFloat; overload; inline; // sin(Pi/2-X)
   function gmpCos(const Value: gmpRational): gmpFloat; overload; inline;
   function gmpCos(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpCos(const Value: gmpFloat): gmpFloat; overload; inline;

   function gmpTan(const Value: gmpInteger): gmpFloat; overload; inline; // sin(X)/cos(X)
   function gmpTan(const Value: gmpRational): gmpFloat; overload; inline;
   function gmpTan(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpTan(const Value: gmpFloat): gmpFloat; overload; inline;

   function gmpCot(const Value: gmpInteger): gmpFloat; overload; inline; // cos(X)/sin(X)
   function gmpCot(const Value: gmpRational): gmpFloat; overload; inline;
   function gmpCot(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpCot(const Value: gmpFloat): gmpFloat; overload; inline;

   // Secant
   function gmpSec(const Value: gmpInteger): gmpFloat; overload; inline; // 1 / cos(X)
   function gmpSec(const Value: gmpRational): gmpFloat; overload; inline;
   function gmpSec(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpSec(const Value: gmpFloat): gmpFloat; overload; inline;

   // Cosecant
   function gmpCsc(const Value: gmpInteger): gmpFloat; overload; inline; // 1 / sin(X)
   function gmpCsc(const Value: gmpRational): gmpFloat; overload; inline;
   function gmpCsc(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpCsc(const Value: gmpFloat): gmpFloat; overload; inline;

   // IN: |Value| <= 1  OUT: [-PI/2..PI/2] radians
   function gmpArcSin(const Value: gmpInteger): gmpFloat; overload; // ArcTan(X/Sqrt((1+X)*(1-X)))
   function gmpArcSin(const Value: gmpRational): gmpFloat; overload;
   function gmpArcSin(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcSin(const Value: gmpFloat): gmpFloat; overload;

   // IN: |Value| <= 1  OUT: [0..PI] radians
   function gmpArcCos(const Value: gmpInteger): gmpFloat; overload;  // ArcTan(Sqrt((1+X)*(1-X))/X)
   function gmpArcCos(const Value: gmpRational): gmpFloat; overload;
   function gmpArcCos(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcCos(const Value: gmpFloat): gmpFloat; overload;

   function gmpArcTan(const Value: gmpInteger): gmpFloat; overload;
   function gmpArcTan(const Value: gmpRational): gmpFloat; overload;
   function gmpArcTan(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcTan(const Value: gmpFloat): gmpFloat; overload;

   // Calculates ArcTan(Y/X)
   function gmpArcTan2(const Y, X: gmpFloat): gmpFloat; overload;

   function gmpArcCot(const Value: gmpInteger): gmpFloat; overload; // ArcTan(1/X)
   function gmpArcCot(const Value: gmpRational): gmpFloat; overload;
   function gmpArcCot(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcCot(const Value: gmpFloat): gmpFloat; overload;

   function gmpArcSec(const Value: gmpInteger): gmpFloat; overload; // ArcCos(1/X)
   function gmpArcSec(const Value: gmpRational): gmpFloat; overload;
   function gmpArcSec(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcSec(const Value: gmpFloat): gmpFloat; overload;

   function gmpArcCsc(const Value: gmpInteger): gmpFloat; overload; // ArcSin(1/X)
   function gmpArcCsc(const Value: gmpRational): gmpFloat; overload;
   function gmpArcCsc(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcCsc(const Value: gmpFloat): gmpFloat; overload;

   // Hyperbolic functions
   function gmpSinH(const Value: gmpInteger): gmpFloat; overload; // (Exp(X) - Exp(-X)) / 2
   function gmpSinH(const Value: gmpRational): gmpFloat; overload;
   function gmpSinH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpSinH(const Value: gmpFloat): gmpFloat; overload;

   function gmpCosH(const Value: gmpInteger): gmpFloat; overload; // (Exp(X) + Exp(-X)) / 2
   function gmpCosH(const Value: gmpRational): gmpFloat; overload;
   function gmpCosH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpCosH(const Value: gmpFloat): gmpFloat; overload;

   function gmpTanH(const Value: gmpInteger): gmpFloat; overload; // (Exp(X) - Exp(-X)) / (Exp(X) + Exp(-X))
   function gmpTanH(const Value: gmpRational): gmpFloat; overload;
   function gmpTanH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpTanH(const Value: gmpFloat): gmpFloat; overload;

   function gmpCotH(const Value: gmpInteger): gmpFloat; overload; // (Exp(X) + Exp(-X)) / (Exp(X) - Exp(-X))
   function gmpCotH(const Value: gmpRational): gmpFloat; overload;
   function gmpCotH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpCotH(const Value: gmpFloat): gmpFloat; overload; inline;

   function gmpSecH(const Value: gmpInteger): gmpFloat; overload; inline; // 1 / cosH(X)
   function gmpSecH(const Value: gmpRational): gmpFloat; overload; inline;
   function gmpSecH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpSecH(const Value: gmpFloat): gmpFloat; overload; inline;

   function gmpCscH(const Value: gmpInteger): gmpFloat; overload; inline; // 1 / sinH(X)
   function gmpCscH(const Value: gmpRational): gmpFloat; overload; inline;
   function gmpCscH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpCscH(const Value: gmpFloat): gmpFloat; overload; inline;

   function gmpArcSinH(const Value: gmpInteger): gmpFloat; overload; // Ln(X + Sqrt((X * X) + 1))
   function gmpArcSinH(const Value: gmpRational): gmpFloat; overload;
   function gmpArcSinH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcSinH(const Value: gmpFloat): gmpFloat; overload;

   function gmpArcCosH(const Value: gmpInteger): gmpFloat; overload; // Ln(X + Sqrt((X - 1) / (X + 1)) * (X + 1))
   function gmpArcCosH(const Value: gmpRational): gmpFloat; overload;
   function gmpArcCosH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcCosH(const Value: gmpFloat): gmpFloat; overload;

   function gmpArcTanH(const Value: gmpInteger): gmpFloat; overload; // 0.5 * Ln((1 + X) / (1 - X))
   function gmpArcTanH(const Value: gmpRational): gmpFloat; overload;
   function gmpArcTanH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcTanH(const Value: gmpFloat): gmpFloat; overload;

   function gmpArcCotH(const Value: gmpInteger): gmpFloat; overload; // 0.5 * Ln((X + 1) / (X - 1))
   function gmpArcCotH(const Value: gmpRational): gmpFloat; overload;
   function gmpArcCotH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcCotH(const Value: gmpFloat): gmpFloat; overload;

   function gmpArcSecH(const Value: gmpInteger): gmpFloat; overload; // Ln((Sqrt(1 - X * X) + 1) / X)
   function gmpArcSecH(const Value: gmpRational): gmpFloat; overload;
   function gmpArcSecH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcSecH(const Value: gmpFloat): gmpFloat; overload;

   function gmpArcCscH(const Value: gmpInteger): gmpFloat; overload; // Ln(Sqrt(1 + (1 / (X * X)) + (1 / X)))
   function gmpArcCscH(const Value: gmpRational): gmpFloat; overload;
   function gmpArcCscH(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpArcCscH(const Value: gmpFloat): gmpFloat; overload;

   // Angle unit conversion routines
   function gmpRadToDeg(const Radians: gmpInteger): gmpFloat; overload;       { Degrees := Radians * 180 / PI }
   function gmpRadToDeg(const Radians: gmpRational): gmpFloat; overload;
   function gmpRadToDeg(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpRadToDeg(const Radians: gmpFloat): gmpFloat; overload;

   function gmpDegToRad(const Degrees: gmpInteger): gmpFloat; overload;       { Radians := Degrees * PI / 180}
   function gmpDegToRad(const Degrees: gmpRational): gmpFloat; overload;
   function gmpDegToRad(const Value: pgmpRational): gmpFloat; overload; inline;
   function gmpDegToRad(const Degrees: gmpFloat): gmpFloat; overload;

   // Others
   function gmpMean(const A, B: gmpInteger): gmpRational; overload; // (A+B)/2
   function gmpMean(const A, B: gmpRational): gmpRational; overload;
   function gmpMean(const A: pgmpRational; const B: gmpRational): gmpRational; overload; inline;
   function gmpMean(const A: gmpRational; const B: pgmpRational): gmpRational; overload; inline;
   function gmpMean(const A, B: gmpFloat): gmpFloat; overload;

   function gmpGeoMean(const A, B: gmpInteger): gmpFloat; overload; // Sqrt(A*B)
   function gmpGeoMean(const A, B: gmpRational): gmpFloat; overload;
   function gmpGeoMean(const A: pgmpRational; const B: gmpRational): gmpFloat; overload; inline;
   function gmpGeoMean(const A: gmpRational; const B: pgmpRational): gmpFloat; overload; inline;
   function gmpGeoMean(const A, B: gmpFloat): gmpFloat; overload;

   function gmpHarmonicMean(const A, B: gmpInteger): gmpFloat; overload; // 2/(1/A+1/B)
   function gmpHarmonicMean(const A, B: gmpRational): gmpFloat; overload;
   function gmpHarmonicMean(const A: pgmpRational; const B: gmpRational): gmpFloat; overload; inline;
   function gmpHarmonicMean(const A: gmpRational; const B: pgmpRational): gmpFloat; overload; inline;
   function gmpHarmonicMean(const A, B: gmpFloat): gmpFloat; overload;

   function gmpHypot(const A, B: gmpInteger): gmpFloat; overload; // Sqrt(A^2 + B^2)
   function gmpHypot(const A, B: gmpRational): gmpFloat; overload;
   function gmpHypot(const A: pgmpRational; const B: gmpRational): gmpFloat; overload; inline;
   function gmpHypot(const A: gmpRational; const B: pgmpRational): gmpFloat; overload; inline;
   function gmpHypot(const A, B: gmpFloat): gmpFloat; overload;

   // For integer
   function gmpGCD(const A, B: gmpInteger): gmpInteger; overload; // Greatest common divisor
   function gmpGCD(const A: gmpInteger; B: Cardinal): gmpInteger; overload;
   // Returns GCD of A, B and set s and t to coefficients satisfying As + Bt = GCD(A, B)
   function gmpGCDEx(const A, B: gmpInteger; var s, t: gmpInteger): gmpInteger;

   function gmpLCM(const A, B: gmpInteger): gmpInteger; overload; // Least common multiple
   function gmpLCM(const A: gmpInteger; B: Cardinal): gmpInteger; overload;

   // Compute the modular inverse of op1 modulo op2 and put the result in Output.
   // If the inverse exists, the return value is True and Output will satisfy
   // 0 ≤ Output < B. If an inverse doesn’t exist the return value is False and
   // Output is unde?ned.
   // Modular inverse is defined as: it is the number x such that ax ≡ 1 (mod n)
   // The inverse of 3 modulo 11 is 4 because 4 · 3 ≡ 1 (mod 11)
   function gmpModularInverse(const Op1, Op2: gmpInteger; var Output: gmpInteger): Boolean;

   // Calculate the Jacobi symbol (A/B). This is de?ned only for B odd.
   function gmpJacobi(const A, B: gmpInteger): Integer;

   // Calculate the Legendre symbol (A/B). This is de?ned only for B an odd
   // positive prime, and for such B it’s identical to the Jacobi symbol.
   // User must make sure that B is correct value.
   function gmpLegendre(const A, B: gmpInteger): Integer;

   // Please refer to the GMP manual.
   function gmpKronecker(const A, B: gmpInteger): Integer; overload;
   function gmpKronecker(const A: gmpInteger; B: Integer): Integer; overload;
   function gmpKronecker(const A: gmpInteger; B: Cardinal): Integer; overload;
   function gmpKronecker(A: Integer; const B: gmpInteger): Integer; overload;
   function gmpKronecker(A: Cardinal; const B: gmpInteger): Integer; overload;

   // Remove all occurrences of the factor F from Src and put result in Output.
   // The return value is how many such occurrences were removed.
   function gmpRemoveFactor(const Src: gmpInteger; Factor: Cardinal; var Output: gmpInteger): Cardinal; overload;
   function gmpRemoveFactor(const Src, Factor: gmpInteger; var Output: gmpInteger): Cardinal; overload;

   // Calculates factorial of Op. Op!
   function gmpFactorial(Op: Cardinal): gmpInteger;

   // Calculates binomial coefficient
   function gmpBinomial(const N: gmpInteger; K: Cardinal): gmpInteger; overload;
   function gmpBinomial(N, K: Cardinal): gmpInteger; overload;

   // Calculates N’th Fibonacci number. And NSub is F(N-1).
   function gmpFibonacci(N: Cardinal): gmpInteger; overload;
   function gmpFibonacci(N: Cardinal; var NSub: gmpInteger): gmpInteger; overload;

   // Calculates N’th Lucas number. And NSub is L(N-1).
   function gmpLucas(N: Cardinal): gmpInteger; overload;
   function gmpLucas(N: Cardinal; var NSub: gmpInteger): gmpInteger; overload;

   // Random number functions. Refer to GMP manual for details.
   // Random state is initialized using gmpInitializeRandomDefault when application starts.
   procedure gmpInitializeRandomDefault;
   procedure gmpInitializeRandomMT; inline; // Mersenne Twister algorithm. This algorithm is default, so gmpInitializeRandomDefault = gmpInitializeRandomMT
   procedure gmpInitializeRandomLC_2Exp(const A: gmpInteger; C: Cardinal; m2Exp: mp_bitcnt_t);
   procedure gmpInitializeRandomLC_2Exp_Size(Size: mp_bitcnt_t);

   procedure gmpRandomSeed(const Seed: gmpInteger); overload;
   procedure gmpRandomSeed(Seed: Cardinal); overload;

   // Create a uniformly distributed random integer in the range 0 to 2^n ? 1, inclusive.
   // For the first one, N must be less than or equal to the number of bits in a Cardinal.
   function gmpRandomIntegerBit(N: mp_bitcnt_t): Cardinal; overload;
   function gmpRandomIntegerBit2(N: mp_bitcnt_t): gmpInteger; overload;
   function gmpRandomIntegerBit_Debug(N: mp_bitcnt_t): gmpInteger; // Refer to mpz_rrandomb in manual.

   // Create a uniformly distributed random integer in the range 0 to Max ? 1, inclusive.
   function gmpRandomInteger(Max: Cardinal): Cardinal; overload;
   function gmpRandomInteger(const Max: gmpInteger): gmpInteger; overload;

   // Generate a uniformly distributed random ?oat, such that 0 ≤ Result < 1.
   function gmpRandomFloat: gmpFloat;
   function gmpRandomFloatDebug(MaxSize: mp_size_t; Exp: mp_exp_t): gmpFloat; // Refer to manual.

   // Generate random number in range, inclusive.
   function gmpRandomRange(const Min, Max: gmpInteger): gmpInteger; overload;
   function gmpRandomRange(const Min, Max: gmpFloat): gmpFloat; overload;

   // Prime test functions. Refer to GMP documentation for more detail.
   // Return True if N is probably prime and return False if N is definitely composite.
   // [*] The first function with Prob parameter determines whether N is a probable
   // prime with the chance of error being at most 1 in 2^prob.
   // [*] The second determines whether n is likely a prime, i.e. you can consider
   // it a prime for practical purposes. This function ensures that N will not
   // have small factors.
   // [*] DivTested can be used to inform the function that trial division up
   // to div has already been performed on N and so N has NO divisors <= div.
   // Use 0 to inform the function that no trial division has been done.
   function gmpProbablePrime(const N: gmpInteger; Prob: Integer; DivTested: Integer = 0): Boolean; overload;
   function gmpProbablePrime(const N: gmpInteger; DivTested: Integer = 0): Boolean; overload;
   function gmpNextLikelyPrime(const N: gmpInteger): gmpInteger; // Returns the next likely prime greater than N.

   // For float
   function gmpCeil(const Value: gmpFloat): gmpFloat;
   function gmpFloor(const Value: gmpFloat): gmpFloat;
   function gmpRelDiff(const A, B: gmpFloat): gmpFloat; // |A - B| / A

// Constants
var
   gmpPi: gmpFloat; // Pi
   gmpPiDiv2: gmpFloat; // Pi/2
   gmpPiMul2: gmpFloat; // Pi*2
   gmp1DivPi: gmpFloat; // 1/Pi
   gmpE: gmpFloat; // e
   gmpSqrt2: gmpFloat; // Sqrt(2)

implementation

// Management of temporary variables for calculation

type
   PPTempChainNode = ^PTempChainNode;
   PTempChainNode = ^TTempChainNode;
   TTempChainNode = packed record
      Occupied: Boolean;
      Body: gmpObject;
      Prev: PTempChainNode;
      Next: PTempChainNode;
   end;

var
   TemporaryInitialized: Boolean = False;

   TemporaryHeads: array[TGMPObjectType] of PTempChainNode;
   TemporaryTails: array[TGMPObjectType] of PTempChainNode;

   // for Log2 and Log10 calculation
   gmp1DivLn2: gmpFloat; // 1 / Ln(2)
   gmp1DivLn10: gmpFloat; // 1 / Ln(10)

   LocalInteger: gmpInteger;
   LocalRational: gmpRational;
   LocalFloat: gmpFloat;
   LocalFloat2: gmpFloat;
   LocalFloat3: gmpFloat;
   LocalZeroRational: gmpRational;

   TemporaryIncrement: Integer; // If the buffer is not big enough, create how many objects as supplement.

   RandomInitialized: Boolean = False;
   RandomState: gmp_randstate_t;

function InitializeNode(Node: PPTempChainNode; ObjType: TGMPObjectType; Prev: PTempChainNode): PTempChainNode;
begin
   New(Node^);
   Node^^.Occupied := False;
   Node^^.Prev := Prev;
   Node^^.Next := nil;

   case ObjType of
      gmpotInteger: gmpInteger(Node^^.Body).Create(0);
      gmpotRational: gmpRational(Node^^.Body).Create(0);
      gmpotFloat: gmpFloat(Node^^.Body).Create(0);
   end;

   Node^^.Body.RelativeNode := Node^;
   Result := Node^;
end;

procedure InternalCreateChain(PHead, PTail: PPTempChainNode; Count: Integer; ObjType: TGMPObjectType);
var
   t: Integer;
   Node, preNode: PTempChainNode;
begin
   if Count <= 1 then
      Count := 2;

   preNode := InitializeNode(PHead, ObjType, nil);
   for t := 0 to Count - 1 do
   begin
      preNode^.Next := InitializeNode(@Node, ObjType, preNode);
      preNode := Node;
   end;
   PTail^ := preNode;
end;

// Initialize temporary integers, rationals and floats buffer. Each has the count of size.
procedure InitializeTemporaryObjects(Size, Increment: Integer);
var
   i: TGMPObjectType;
begin
   if Size <= 1 then
      Size := 2;

   for i := Low(TGMPObjectType) to High(TGMPObjectType) do
      InternalCreateChain(@TemporaryHeads[i], @TemporaryTails[i], Size, i);

   TemporaryIncrement := Increment;

   // Initialize temporary objects used as local variables
   LocalInteger.Create(0);
   LocalRational.Create(0);
   LocalFloat.Create(0);
   LocalFloat2.Create(0);
   LocalFloat3.Create(0);
   LocalZeroRational.Create(0);
end;

procedure FinalizeTemporaryObjects;
var
   i: TGMPObjectType;
   Node, tmpNode: PTempChainNode;
begin
   if not TemporaryInitialized then
      Exit;

   for i := Low(TGMPObjectType) to High(TGMPObjectType) do
   begin
      Node := TemporaryHeads[i];
      while Assigned(Node) do
      begin
         tmpNode := Node^.Next;
         gmpFree(@Node^.Body);
         Dispose(Node);
         Node := tmpNode;
      end;
   end;

   LocalInteger.Free;
   LocalRational.Free;
   LocalFloat.Free;
   LocalFloat2.Free;
   LocalFloat3.Free;
   LocalZeroRational.Free;
end;

// Snatch an object from the head node and put it to tail.
// And make sure that the head of the chain is always unoccupied objects.
function GetTemporaryObject(ObjType: TGMPObjectType): Pointer;
var
   Node: PTempChainNode;
   Head, Tail: PTempChainNode; // for supplement chain
begin
   Node := TemporaryHeads[ObjType];

   // Put Node to tail and make the next one as head
   Node^.Prev := TemporaryTails[ObjType];
   TemporaryHeads[ObjType] := Node^.Next;
   TemporaryHeads[ObjType]^.Prev := nil;
   TemporaryTails[ObjType]^.Next := Node;
   TemporaryTails[ObjType] := Node;
   Node^.Next := nil;
   Node^.Occupied := True;

   // Check if the current head is occupied. If so, temporary objects are not enough, and create more.
   if TemporaryHeads[ObjType]^.Occupied then
   begin
      InternalCreateChain(@Head, @Tail, TemporaryIncrement, ObjType);
      // Combine TemporaryHeads[ObjType] to TemporaryTails[ObjType] with Head to Tail
      Tail^.Next := TemporaryHeads[ObjType];
      TemporaryHeads[ObjType]^.Prev := Tail;
      TemporaryHeads[ObjType] := Head;
   end;

   Result := @Node^.Body;
end;

// Lift the node to top of the chain and make it unoccpied.
procedure ReleaseTemporaryObject(ObjType: TGMPObjectType; ObjPointer: pgmpObject);
var
   Node: PTempChainNode;
begin
   Node := PTempChainNode(ObjPointer^.RelativeNode);

   if not Node.Occupied then
      Exit;

   // Node can never be the head of the chain because the head of the chain must be an unoccupied object.
   Node^.Prev^.Next := Node^.Next; // Node^.Prev must be assigned
   if Assigned(Node^.Next) then
      Node^.Next^.Prev := Node^.Prev
   else
      TemporaryTails[ObjType] := Node^.Prev; // Node is the tail of the chain

   Node^.Next := TemporaryHeads[ObjType];
   TemporaryHeads[ObjType]^.Prev := Node;
   TemporaryHeads[ObjType] := Node;
   Node^.Occupied := False;
end;

// gmpInteger
function GetTemporaryInteger: pgmpInteger; inline;
begin
   Result := pgmpInteger(GetTemporaryObject(gmpotInteger));
end;

procedure ReleaseTemporaryInteger(const Obj: gmpInteger); inline;
begin
   ReleaseTemporaryObject(gmpotInteger, @Obj);
end;

// gmpRational
function GetTemporaryRational: pgmpRational; inline;
begin
   Result := pgmpRational(GetTemporaryObject(gmpotRational));
end;

procedure ReleaseTemporaryRational(const Obj: gmpRational); inline;
begin
   ReleaseTemporaryObject(gmpotRational, @Obj);
end;

// gmpFloat
function GetTemporaryFloat: pgmpFloat; inline;
begin
   Result := pgmpFloat(GetTemporaryObject(gmpotFloat));
end;

procedure ReleaseTemporaryFloat(const Obj: gmpFloat); inline;
begin
   ReleaseTemporaryObject(gmpotFloat, @Obj);
end;

//////////////////////////////////////////////////////////////////////

var
   DefaultFloatPrecision: mp_bitcnt_t;
   CurrentTemporaryFloatPrecision: mp_bitcnt_t = 0;

procedure gmpSetConstantPrecision(Value: mp_bitcnt_t); forward;

procedure CheckTemporaryFloatPrecision(Value: mp_bitcnt_t; IncreasePrecision: Boolean = True);
var
   Flag: Boolean;
   Node: PTempChainNode;
begin
   if IncreasePrecision then
      Flag := CurrentTemporaryFloatPrecision < Value
   else
      Flag := CurrentTemporaryFloatPrecision <> Value;

   if Flag then
   begin
      Node := TemporaryHeads[gmpotFloat];
      while Assigned(Node) do
      begin
         mpf_set_prec(pmpf_t(Node^.Body.NativeObject)^, Value);
         Node := Node^.Next;
      end;

      if not TemporaryInitialized then
      begin
         InitializeTemporaryObjects(10, 5);
         TemporaryInitialized := True;
      end;

      // Reset precision of LocalFloat.
      mpf_set_prec(LocalFloat.NativeObject^, Value);
      mpf_set_prec(LocalFloat2.NativeObject^, Value);
      mpf_set_prec(LocalFloat3.NativeObject^, Value);

      // Reset precision of constants.
      gmpSetConstantPrecision(Value);

      CurrentTemporaryFloatPrecision := Value;
   end;
end;

procedure SetDefaultFloatPrecision(Value: mp_bitcnt_t);
begin
   if Value < GMP_MIN_PRECISION then
      raise gmpException.Create(gmpeFloatPrecisionTooLow);

   DefaultFloatPrecision := Value;
   mpf_set_default_prec(Value);

   CheckTemporaryFloatPrecision(Value);
end;

procedure ResetDefaultFloatPrecision(Value: mp_bitcnt_t);
begin
   if Value < GMP_MIN_PRECISION then
      raise gmpException.Create(gmpeFloatPrecisionTooLow);

   DefaultFloatPrecision := Value;
   mpf_set_default_prec(Value);

   CheckTemporaryFloatPrecision(Value, False);
end;

function GetDefaultFloatPrecision: mp_bitcnt_t;
begin
   Result := mpf_get_default_prec;
end;

{ gmpInteger }

procedure gmpInteger.CreateGeneric;
begin
   try
      NativeType := gmpotInteger;
      RelativeNode := nil;
      New(NativeObject); // allocate memory for prototype variable
      mpz_init(NativeObject^); // initialize prototype and allocate memory in gmp DLL.
   except
      raise gmpException.Create(gmpeFatalError);
   end;
end;

function gmpInteger.FGetIsTemporary: Boolean;
begin
   Result := Assigned(RelativeNode);
end;

function gmpInteger.FGetIsOdd: Boolean;
begin
   Result := mpz_odd_p(NativeObject^) <> 0;
   ReleaseTemporary;
end;

function gmpInteger.FGetIsEven: Boolean;
begin
   Result := mpz_even_p(NativeObject^) <> 0;
   ReleaseTemporary;
end;

function gmpInteger.FGetIsPerfectPower: Boolean;
begin
   Result := mpz_perfect_power_p(NativeObject^) <> 0;
   ReleaseTemporary;
end;

function gmpInteger.FGetIsPerfectSquare: Boolean;
begin
   Result := mpz_perfect_square_p(NativeObject^) <> 0;
   ReleaseTemporary;
end;

function gmpInteger.FGetFitsCardinal: Boolean;
begin
   Result := mpz_fits_ulong_p(NativeObject^) <> 0;
   ReleaseTemporary;
end;

function gmpInteger.FGetFitsInteger: Boolean;
begin
   Result := mpz_fits_slong_p(NativeObject^) <> 0;
   ReleaseTemporary;
end;

procedure gmpInteger.ReleaseTemporary;
begin
   if Assigned(RelativeNode) then
      ReleaseTemporaryInteger(Self);
end;

constructor gmpInteger.Create(const Value: Integer);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpInteger.Create(const Value: Cardinal);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpInteger.Create(const Value: Int64);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpInteger.Create(const Value: UInt64);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpInteger.Create(const Value: AnsiString);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpInteger.Create(const Value: AnsiString; Base: Integer);
begin
   CreateGeneric;
   Assign(Value, Base);
end;

constructor gmpInteger.Create(const Value: gmpInteger);
begin
   CreateGeneric;
   Assign(Value);
end;

procedure gmpInteger.Free;
begin
   try
      mpz_clear(NativeObject^); // free memory in gmp DLL
      Dispose(NativeObject); // free memory of prototype variable
   except
      raise gmpException.Create(gmpeFatalError);
   end;
end;

// Assign functions for delphi native types. Must be used after constructor is called.

procedure gmpInteger.Assign(const Value: Integer);
begin
   mpz_set_si(NativeObject^, Value);
end;

procedure gmpInteger.Assign(const Value: Cardinal);
begin
   mpz_set_ui(NativeObject^, Value);
end;

procedure gmpInteger.Assign(const Value: Int64);
begin
   mpz_set_int64(NativeObject^, Value);
end;

procedure gmpInteger.Assign(const Value: UInt64);
begin
   mpz_set_uint64(NativeObject^, Value);
end;

procedure gmpInteger.Assign(const Value: AnsiString);
begin
   if mpz_set_str(NativeObject^, PAnsiChar(Value), 10) <> 0 then
      raise gmpException.Create(gmpeConversionError);
end;

procedure gmpInteger.Assign(const Value: AnsiString; Base: Integer);
begin
   if (Base = 0) or ((Base >= 2) and (Base <= 62)) then
   begin
      if mpz_set_str(NativeObject^, PAnsiChar(Value), Base) <> 0 then
         raise gmpException.Create(gmpeConversionError);
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

procedure gmpInteger.Assign(const Value: gmpInteger);
begin
   mpz_set(NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpInteger.SetZero;
begin
   mpz_set_si(NativeObject^, 0);
end;

procedure gmpInteger.SetNegative;
begin
   NativeObject^.mp_size := -NativeObject^.mp_size;
   //mpz_neg(NativeObject^, NativeObject^); // TODO
end;

procedure gmpInteger.SetAbsolute;
begin
   NativeObject^.mp_size := System.Abs(NativeObject^.mp_size);
   //mpz_abs(NativeObject^, NativeObject^);} // TODO
end;

procedure gmpInteger.SetSquare;
begin
   mpz_pow_ui(NativeObject^, NativeObject^, 2);
end;

procedure gmpInteger.SetCubic;
begin
   mpz_pow_ui(NativeObject^, NativeObject^, 3);
end;

procedure gmpInteger.SetIntPower(Exp: Cardinal);
begin
   mpz_pow_ui(NativeObject^, NativeObject^, Exp);
end;

function gmpInteger.SetSqrt: Boolean;
begin
   Result := mpz_root(NativeObject^, NativeObject^, 2) <> 0;
end;

function gmpInteger.SetNthRoot(N: Cardinal): Boolean;
begin
   Result := mpz_root(NativeObject^, NativeObject^, N) <> 0;
end;

procedure gmpInteger.SetMul2Exp(const Base: gmpInteger; N: mp_bitcnt_t);
begin
   mpz_mul_2exp(NativeObject^, Base.NativeObject^, N);
   Base.ReleaseTemporary;
end;

procedure gmpInteger.SetDiv2Exp(const Base: gmpInteger; N: mp_bitcnt_t);
begin
   mpz_tdiv_q_2exp(NativeObject^, Base.NativeObject^, N);
   Base.ReleaseTemporary;
end;

function gmpInteger.RemoveFactor(F: Cardinal): Cardinal;
begin
   LocalInteger.Assign(F);
   Result := mpz_remove(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

function gmpInteger.RemoveFactor(const F: gmpInteger): Cardinal;
begin
   Result := mpz_remove(NativeObject^, NativeObject^, F.NativeObject^);
   F.ReleaseTemporary;
end;

// Bit manipulation methods

function gmpInteger.GetPopulationCount: mp_bitcnt_t;
begin
   Result := mpz_popcount(NativeObject^);
   ReleaseTemporary;
end;

function gmpInteger.GetHammingDistance(const Op2: gmpInteger): mp_bitcnt_t;
begin
   Result := mpz_hamdist(NativeObject^, Op2.NativeObject^);
   ReleaseTemporary;
   Op2.ReleaseTemporary;
end;

function gmpInteger.Scan0(StartingBit: mp_bitcnt_t): mp_bitcnt_t;
begin
   Result := mpz_scan0(NativeObject^, StartingBit);
   ReleaseTemporary;
end;

function gmpInteger.Scan1(StartingBit: mp_bitcnt_t): mp_bitcnt_t;
begin
   Result := mpz_scan1(NativeObject^, StartingBit);
   ReleaseTemporary;
end;

procedure gmpInteger.SetBit(BitIndex: mp_bitcnt_t);
begin
   mpz_setbit(NativeObject^, BitIndex);
end;

procedure gmpInteger.ClearBit(BitIndex: mp_bitcnt_t);
begin
   mpz_clrbit(NativeObject^, BitIndex);
end;

procedure gmpInteger.ComplementBit(BitIndex: mp_bitcnt_t);
begin
   mpz_combit(NativeObject^, BitIndex);
end;

function gmpInteger.TestBit(BitIndex: mp_bitcnt_t): Integer;
begin
   Result := mpz_tstbit(NativeObject^, BitIndex);
   ReleaseTemporary;
end;

// Output methods

function gmpInteger.ToCardinal: Cardinal;
begin
   ReleaseTemporary;

   if mpz_fits_uint_p(NativeObject^) = 0 then
      raise gmpException.Create(gmpeConversionError);

   Result := mpz_get_ui(NativeObject^);
end;

function gmpInteger.ToInteger: Integer;
begin
   ReleaseTemporary;

   if mpz_fits_sint_p(NativeObject^) = 0 then
      raise gmpException.Create(gmpeConversionError);
   Result := mpz_get_si(NativeObject^);
end;

function gmpInteger.ToDouble: Double;
begin
   Result := mpz_get_d(NativeObject^);
   ReleaseTemporary;
end;

procedure gmpInteger.To2Exp(var D: Double; var Exp: Integer);
begin
   D := mpz_get_d_2exp(Exp, NativeObject^);
   ReleaseTemporary;
end;

function gmpInteger.ToString(Base: Integer = 10): AnsiString;
var
   size: Integer;
begin
   ReleaseTemporary;

   if ((Base <= 62) and (Base >= 2)) or ((Base <= -2) and (Base >= -36)) then
   begin
      size := mpz_sizeinbase(NativeObject^, Base) + 1;
      try
         SetLength(Result, size);
      except
         raise gmpException.Create(gmpeFatalError);
      end;
      mpz_get_str(PAnsiChar(Result), Base, NativeObject^);
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

// Explicit casting

class operator gmpInteger.Explicit(const Value: Integer): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Value);
end;

class operator gmpInteger.Explicit(const Value: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Value);
end;

class operator gmpInteger.Explicit(const Value: Int64): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Value);
end;

class operator gmpInteger.Explicit(const Value: UInt64): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Value);
end;

class operator gmpInteger.Explicit(const Value: AnsiString): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Value);
end;

class operator gmpInteger.Negative(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_neg(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

class operator gmpInteger.Positive(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Value);
end;

// Trunc and Round methods

class operator gmpInteger.Trunc(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Value);
end;

class operator gmpInteger.Round(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Value);
end;

// Increment methods

procedure gmpInteger.Inc;
begin
   mpz_add_ui(NativeObject^, NativeObject^, 1);
end;

procedure gmpInteger.Inc(const Value: Integer);
begin
   if Value >= 0 then
      mpz_add_ui(NativeObject^, NativeObject^, Cardinal(Value))
   else
      mpz_sub_ui(NativeObject^, NativeObject^, Cardinal(System.Abs(Value)));
end;

procedure gmpInteger.Inc(const Value: Cardinal);
begin
   mpz_add_ui(NativeObject^, NativeObject^, Value);
end;

procedure gmpInteger.Inc(const Value: Int64);
begin
   mpz_set_int64(LocalInteger.NativeObject^, Value);
   mpz_add(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.Inc(const Value: UInt64);
begin
   mpz_set_uint64(LocalInteger.NativeObject^, Value);
   mpz_add(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.Inc(const Value: AnsiString);
begin
   if mpz_init_set_str(LocalInteger.NativeObject^, PAnsiChar(Value), 10) <> 0 then
      raise gmpException.Create(gmpeConversionError);

   mpz_add(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.Inc(const Value: AnsiString; Base: Integer);
begin
   if (Base = 0) or ((Base >= 2) and (Base <= 62)) then
   begin
      if mpz_init_set_str(LocalInteger.NativeObject^, PAnsiChar(Value), Base) <> 0 then
         raise gmpException.Create(gmpeConversionError);

      mpz_add(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

procedure gmpInteger.Inc(const Value: gmpInteger);
begin
   mpz_add(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Decrement methods

procedure gmpInteger.Dec;
begin
   mpz_sub_ui(NativeObject^, NativeObject^, 1);
end;

procedure gmpInteger.Dec(const Value: Integer);
begin
   if Value >= 0 then
      mpz_sub_ui(NativeObject^, NativeObject^, Cardinal(Value))
   else
      mpz_add_ui(NativeObject^, NativeObject^, Cardinal(System.Abs(Value)));
end;

procedure gmpInteger.Dec(const Value: Cardinal);
begin
   mpz_sub_ui(NativeObject^, NativeObject^, Value);
end;

procedure gmpInteger.Dec(const Value: Int64);
begin
   mpz_set_int64(LocalInteger.NativeObject^, Value);
   mpz_sub(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.Dec(const Value: UInt64);
begin
   mpz_set_uint64(LocalInteger.NativeObject^, Value);
   mpz_sub(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.Dec(const Value: AnsiString);
begin
   if mpz_init_set_str(LocalInteger.NativeObject^, PAnsiChar(Value), 10) <> 0 then
      raise gmpException.Create(gmpeConversionError);

   mpz_sub(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.Dec(const Value: AnsiString; Base: Integer);
begin
   if (Base = 0) or ((Base >= 2) and (Base <= 62)) then
   begin
      if mpz_init_set_str(LocalInteger.NativeObject^, PAnsiChar(Value), Base) <> 0 then
         raise gmpException.Create(gmpeConversionError);

      mpz_sub(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

procedure gmpInteger.Dec(const Value: gmpInteger);
begin
   mpz_sub(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Multiply and Divide methods

procedure gmpInteger.MultiplyBy(const Value: gmpInteger);
begin
   mpz_mul(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpInteger.MultiplyBy(const Value: Integer);
begin
   mpz_mul_si(NativeObject^, NativeObject^, Value);
end;

procedure gmpInteger.MultiplyBy(const Value: Cardinal);
begin
   mpz_mul_ui(NativeObject^, NativeObject^, Value);
end;

procedure gmpInteger.MultiplyBy(const Value: Int64);
begin
   LocalInteger.Assign(Value);
   mpz_mul(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.MultiplyBy(const Value: UInt64);
begin
   LocalInteger.Assign(Value);
   mpz_mul(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.DivideBy(const Value: gmpInteger);
begin
   Value.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Value.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   mpz_tdiv_q(NativeObject^, NativeObject^, Value.NativeObject^);
end;

procedure gmpInteger.DivideBy(const Value: Integer);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   mpz_tdiv_q_ui(NativeObject^, NativeObject^, Cardinal(System.Abs(Value)));

   if Value < 0 then
      SetNegative;
end;

procedure gmpInteger.DivideBy(const Value: Cardinal);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   mpz_tdiv_q_ui(NativeObject^, NativeObject^, Value);
end;

procedure gmpInteger.DivideBy(const Value: Int64);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalInteger.Assign(Value);
   mpz_tdiv_q(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.DivideBy(const Value: UInt64);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalInteger.Assign(Value);
   mpz_tdiv_q(NativeObject^, NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpInteger.Mul2Exp(const N: mp_bitcnt_t);
begin
   mpz_mul_2exp(NativeObject^, NativeObject^, N);
end;

procedure gmpInteger.Div2Exp(const N: mp_bitcnt_t);
begin
   mpz_tdiv_q_2exp(NativeObject^, NativeObject^, N);
end;

// Add operator functions
// Infact, all add operator functions can be written as:
//   Result := GetTemporaryInteger()^;
//   Result.Assign(Left);
//   Result.Inc(Right);
// But for some, optimization is done.

class operator gmpInteger.Add(const Left, Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_add(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Add(const Left: gmpInteger; const Right: Integer): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   if Right >= 0 then
      mpz_add_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(Right))
   else
      mpz_sub_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(System.Abs(Right)));

   Left.ReleaseTemporary;
end;

class operator gmpInteger.Add(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_add_ui(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Add(const Left: gmpInteger; const Right: Int64): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpInteger.Add(const Left: gmpInteger; const Right: UInt64): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpInteger.Add(const Left: gmpInteger; const Right: Single): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_add(Result^.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpInteger.Add(const Left: gmpInteger; const Right: Double): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_add(Result^.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpInteger.Add(const Left: Integer; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   if Left >= 0 then
      mpz_add_ui(Result.NativeObject^, Right.NativeObject^, Cardinal(Left))
   else
      mpz_sub_ui(Result.NativeObject^, Right.NativeObject^, Cardinal(System.Abs(Left)));

   Right.ReleaseTemporary;
end;

class operator gmpInteger.Add(const Left: Cardinal; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_add_ui(Result.NativeObject^, Right.NativeObject^, Left);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Add(const Left: Int64; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpInteger.Add(const Left: UInt64; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpInteger.Add(const Left: Single; const Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_add(Result^.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpInteger.Add(const Left: Double; const Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_add(Result^.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

// Subtract operator functions
// Infact, all subtract operator functions can be written as:
//   Result := GetTemporaryInteger()^;
//   Result.Assign(Left);
//   Result.Dec(Right);
// But for some, optimization is done.

class operator gmpInteger.Subtract(const Left, Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_sub(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Subtract(const Left: gmpInteger; const Right: Integer): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   if Right >= 0 then
      mpz_sub_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(Right))
   else
      mpz_add_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(System.Abs(Right)));

   Left.ReleaseTemporary;
end;

class operator gmpInteger.Subtract(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_sub_ui(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Subtract(const Left: gmpInteger; const Right: Int64): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpInteger.Subtract(const Left: gmpInteger; const Right: UInt64): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpInteger.Subtract(const Left: Integer; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   if Left >= 0 then
      mpz_ui_sub(Result.NativeObject^, Cardinal(Left), Right.NativeObject^)
   else
   begin
      mpz_add_ui(Result.NativeObject^, Right.NativeObject^, Cardinal(System.Abs(Left)));
      Result.NativeObject^.mp_size := -Result.NativeObject^.mp_size;
      //mpz_neg(Result.NativeObject^, Result.NativeObject^); // TODO
   end;

   Right.ReleaseTemporary;
end;

class operator gmpInteger.Subtract(const Left: Cardinal; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_ui_sub(Result.NativeObject^, Left, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Subtract(const Left: Int64; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpInteger.Subtract(const Left: UInt64; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

// Multiply operator functions

class operator gmpInteger.Multiply(const Left, Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_mul(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: Integer; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_mul_si(Result.NativeObject^, Right.NativeObject^, Left);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: Cardinal; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_mul_ui(Result.NativeObject^, Right.NativeObject^, Left);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: Int64; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_mul(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: UInt64; const Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_mul(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: Single; const Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_mul(Result^.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpInteger.Multiply(const Left: Double; const Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_mul(Result^.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpInteger.Multiply(const Left: gmpInteger; const Right: Integer): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_mul_si(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_mul_ui(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: gmpInteger; const Right: Int64): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Right);
   mpz_mul(Result.NativeObject^, Result.NativeObject^, Left.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: gmpInteger; const Right: UInt64): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   Result.Assign(Right);
   mpz_mul(Result.NativeObject^, Result.NativeObject^, Left.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Multiply(const Left: gmpInteger; const Right: Single): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_mul(Result^.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpInteger.Multiply(const Left: gmpInteger; const Right: Double): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_mul(Result^.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

// Division operator functions

class operator gmpInteger.Divide(const Left, Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   Result^.Assign(Left, Right);
end;

class operator gmpInteger.Divide(const Left: Integer; const Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   LocalInteger.Assign(Left);
   Result^.Assign(LocalInteger, Right);
end;

class operator gmpInteger.Divide(const Left: Cardinal; const Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   LocalInteger.Assign(Left);
   Result^.Assign(LocalInteger, Right);
end;

class operator gmpInteger.Divide(const Left: Int64; const Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   LocalInteger.Assign(Left);
   Result^.Assign(LocalInteger, Right);
end;

class operator gmpInteger.Divide(const Left: UInt64; const Right: gmpInteger): pgmpRational;
begin
   Result := GetTemporaryRational;
   LocalInteger.Assign(Left);
   Result^.Assign(LocalInteger, Right);
end;

class operator gmpInteger.Divide(const Left: Single; const Right: gmpInteger): pgmpRational;
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Right);
   {$ENDIF}

   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpInteger.Divide(const Left: Double; const Right: gmpInteger): pgmpRational;
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Right);
   {$ENDIF}

   Result := GetTemporaryRational;
   Result^.Assign(Left);
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Result^.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpInteger.Divide(const Left: gmpInteger; const Right: Integer): pgmpRational;
begin
   Result := GetTemporaryRational;
   LocalInteger.Assign(Right);
   Result^.Assign(Left, LocalInteger);
end;

class operator gmpInteger.Divide(const Left: gmpInteger; const Right: Cardinal): pgmpRational;
begin
   Result := GetTemporaryRational;
   LocalInteger.Assign(Right);
   Result^.Assign(Left, LocalInteger);
end;

class operator gmpInteger.Divide(const Left: gmpInteger; const Right: Int64): pgmpRational;
begin
   Result := GetTemporaryRational;
   LocalInteger.Assign(Right);
   Result^.Assign(Left, LocalInteger);
end;

class operator gmpInteger.Divide(const Left: gmpInteger; const Right: UInt64): pgmpRational;
begin
   Result := GetTemporaryRational;
   LocalInteger.Assign(Right);
   Result^.Assign(Left, LocalInteger);
end;

class operator gmpInteger.Divide(const Left: gmpInteger; const Right: Single): pgmpRational;
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0.0 then
      raise gmpException.Create(gmpeDivideByZero, Left);
   {$ENDIF}

   Result := GetTemporaryRational;
   Result^.Assign(Right);
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Result^.NativeObject^);
end;

class operator gmpInteger.Divide(const Left: gmpInteger; const Right: Double): pgmpRational;
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0.0 then
      raise gmpException.Create(gmpeDivideByZero, Left);
   {$ENDIF}

   Result := GetTemporaryRational;
   Result^.Assign(Right);
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Result^.NativeObject^);
end;

// Integer-divide operator functions

class operator gmpInteger.IntDivide(const Left, Right: gmpInteger): gmpInteger;
begin
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   mpz_tdiv_q(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.IntDivide(const Left: Integer; const Right: gmpInteger): gmpInteger;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_tdiv_q(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.IntDivide(const Left: Cardinal; const Right: gmpInteger): gmpInteger;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_tdiv_q(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.IntDivide(const Left: Int64; const Right: gmpInteger): gmpInteger;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_tdiv_q(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.IntDivide(const Left: UInt64; const Right: gmpInteger): gmpInteger;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_tdiv_q(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.IntDivide(const Left: gmpInteger; const Right: Integer): gmpInteger;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   mpz_tdiv_q_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(System.Abs(Right)));

   if Right < 0 then
      Result.SetNegative;
end;

class operator gmpInteger.IntDivide(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   mpz_tdiv_q_ui(Result.NativeObject^, Left.NativeObject^, Right);
end;

class operator gmpInteger.IntDivide(const Left: gmpInteger; const Right: Int64): gmpInteger;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Right);
   mpz_tdiv_q(Result.NativeObject^, Left.NativeObject^, Result.NativeObject^);
end;

class operator gmpInteger.IntDivide(const Left: gmpInteger; const Right: UInt64): gmpInteger;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Right);
   mpz_tdiv_q(Result.NativeObject^, Left.NativeObject^, Result.NativeObject^);
end;

// ################### Modulus operator functions ###################

class operator gmpInteger.Modulus(const Left, Right: gmpInteger): gmpInteger;
begin
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   mpz_tdiv_r(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.Modulus(const Left: Integer; const Right: gmpInteger): gmpInteger;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_tdiv_r(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.Modulus(const Left: Cardinal; const Right: gmpInteger): gmpInteger;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_tdiv_r(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.Modulus(const Left: Int64; const Right: gmpInteger): gmpInteger;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_tdiv_r(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.Modulus(const Left: UInt64; const Right: gmpInteger): gmpInteger;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_si(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Left);
   mpz_tdiv_r(Result.NativeObject^, Result.NativeObject^, Right.NativeObject^);
end;

class operator gmpInteger.Modulus(const Left: gmpInteger; const Right: Integer): gmpInteger;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   mpz_tdiv_r_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(System.Abs(Right)));
end;

class operator gmpInteger.Modulus(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   mpz_tdiv_r_ui(Result.NativeObject^, Left.NativeObject^, Right);
end;

class operator gmpInteger.Modulus(const Left: gmpInteger; const Right: Int64): gmpInteger;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Right);
   mpz_tdiv_r(Result.NativeObject^, Left.NativeObject^, Result.NativeObject^);
end;

class operator gmpInteger.Modulus(const Left: gmpInteger; const Right: UInt64): gmpInteger;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryInteger()^;
   Result.Assign(Right);
   mpz_tdiv_r(Result.NativeObject^, Left.NativeObject^, Result.NativeObject^);
end;

// Equal operator functions

class operator gmpInteger.Equal(const Left, Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp(Left.NativeObject^, Right.NativeObject^) = 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: gmpInteger; const Right: Integer): Boolean;
begin
   Result := mpz_cmp_si(Left.NativeObject^, Right) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: gmpInteger; const Right: Cardinal): Boolean;
begin
   Result := mpz_cmp_ui(Left.NativeObject^, Right) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: gmpInteger; const Right: Int64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: gmpInteger; const Right: UInt64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: gmpInteger; const Right: Single): Boolean;
var
   d: Double;
begin
   d := Right;
   Result := mpz_cmp_d(Left.NativeObject^, d) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: gmpInteger; const Right: Double): Boolean;
begin
   Result := mpz_cmp_d(Left.NativeObject^, Right) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: Integer; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_si(Right.NativeObject^, Left) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: Cardinal; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_ui(Right.NativeObject^, Left) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: Int64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: UInt64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: Single; const Right: gmpInteger): Boolean;
var
   d: Double;
begin
   d := Left;
   Result := mpz_cmp_d(Right.NativeObject^, d) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.Equal(const Left: Double; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_d(Right.NativeObject^, Left) = 0;
   Right.ReleaseTemporary;
end;

// Not-Equal operator functions

class operator gmpInteger.NotEqual(const Left, Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp(Left.NativeObject^, Right.NativeObject^) <> 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: gmpInteger; const Right: Integer): Boolean;
begin
   Result := mpz_cmp_si(Left.NativeObject^, Right) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: gmpInteger; const Right: Cardinal): Boolean;
begin
   Result := mpz_cmp_ui(Left.NativeObject^, Right) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: gmpInteger; const Right: Int64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: gmpInteger; const Right: UInt64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: gmpInteger; const Right: Single): Boolean;
var
   d: Double;
begin
   d := Right;
   Result := mpz_cmp_d(Left.NativeObject^, d) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: gmpInteger; const Right: Double): Boolean;
begin
   Result := mpz_cmp_d(Left.NativeObject^, Right) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: Integer; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_si(Right.NativeObject^, Left) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: Cardinal; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_ui(Right.NativeObject^, Left) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: Int64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: UInt64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: Single; const Right: gmpInteger): Boolean;
var
   d: Double;
begin
   d := Left;
   Result := mpz_cmp_d(Right.NativeObject^, d) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.NotEqual(const Left: Double; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_d(Right.NativeObject^, Left) <> 0;
   Right.ReleaseTemporary;
end;

// Greater-Than operator functions

class operator gmpInteger.GreaterThan(const Left, Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp(Left.NativeObject^, Right.NativeObject^) > 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: gmpInteger; const Right: Integer): Boolean;
begin
   Result := mpz_cmp_si(Left.NativeObject^, Right) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: gmpInteger; const Right: Cardinal): Boolean;
begin
   Result := mpz_cmp_ui(Left.NativeObject^, Right) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: gmpInteger; const Right: Int64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: gmpInteger; const Right: UInt64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: gmpInteger; const Right: Single): Boolean;
var
   d: Double;
begin
   d := Right;
   Result := mpz_cmp_d(Left.NativeObject^, d) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: gmpInteger; const Right: Double): Boolean;
begin
   Result := mpz_cmp_d(Left.NativeObject^, Right) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: Integer; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_si(Right.NativeObject^, Left) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: Cardinal; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_ui(Right.NativeObject^, Left) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: Int64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: UInt64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: Single; const Right: gmpInteger): Boolean;
var
   d: Double;
begin
   d := Left;
   Result := mpz_cmp_d(Right.NativeObject^, d) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThan(const Left: Double; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_d(Right.NativeObject^, Left) < 0;
   Right.ReleaseTemporary;
end;

// GreaterThanOrEqual operator functions

class operator gmpInteger.GreaterThanOrEqual(const Left, Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp(Left.NativeObject^, Right.NativeObject^) >= 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: gmpInteger; const Right: Integer): Boolean;
begin
   Result := mpz_cmp_si(Left.NativeObject^, Right) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: gmpInteger; const Right: Cardinal): Boolean;
begin
   Result := mpz_cmp_ui(Left.NativeObject^, Right) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: gmpInteger; const Right: Int64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: gmpInteger; const Right: UInt64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: gmpInteger; const Right: Single): Boolean;
var
   d: Double;
begin
   d := Right;
   Result := mpz_cmp_d(Left.NativeObject^, d) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: gmpInteger; const Right: Double): Boolean;
begin
   Result := mpz_cmp_d(Left.NativeObject^, Right) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: Integer; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_si(Right.NativeObject^, Left) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: Cardinal; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_ui(Right.NativeObject^, Left) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: Int64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: UInt64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: Single; const Right: gmpInteger): Boolean;
var
   d: Double;
begin
   d := Left;
   Result := mpz_cmp_d(Right.NativeObject^, d) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.GreaterThanOrEqual(const Left: Double; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_d(Right.NativeObject^, Left) <= 0;
   Right.ReleaseTemporary;
end;

// Less-Than operator functions

class operator gmpInteger.LessThan(const Left, Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp(Left.NativeObject^, Right.NativeObject^) < 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: gmpInteger; const Right: Integer): Boolean;
begin
   Result := mpz_cmp_si(Left.NativeObject^, Right) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: gmpInteger; const Right: Cardinal): Boolean;
begin
   Result := mpz_cmp_ui(Left.NativeObject^, Right) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: gmpInteger; const Right: Int64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: gmpInteger; const Right: UInt64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: gmpInteger; const Right: Single): Boolean;
var
   d: Double;
begin
   d := Right;
   Result := mpz_cmp_d(Left.NativeObject^, d) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: gmpInteger; const Right: Double): Boolean;
begin
   Result := mpz_cmp_d(Left.NativeObject^, Right) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: Integer; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_si(Right.NativeObject^, Left) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: Cardinal; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_ui(Right.NativeObject^, Left) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: Int64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: UInt64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: Single; const Right: gmpInteger): Boolean;
var
   d: Double;
begin
   d := Left;
   Result := mpz_cmp_d(Right.NativeObject^, d) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThan(const Left: Double; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_d(Right.NativeObject^, Left) > 0;
   Right.ReleaseTemporary;
end;

// LessThanOrEqual operator functions

class operator gmpInteger.LessThanOrEqual(const Left, Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp(Left.NativeObject^, Right.NativeObject^) <= 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: gmpInteger; const Right: Integer): Boolean;
begin
   Result := mpz_cmp_si(Left.NativeObject^, Right) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: gmpInteger; const Right: Cardinal): Boolean;
begin
   Result := mpz_cmp_ui(Left.NativeObject^, Right) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: gmpInteger; const Right: Int64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: gmpInteger; const Right: UInt64): Boolean;
begin
   LocalInteger.Assign(Right);
   Result := mpz_cmp(Left.NativeObject^, LocalInteger.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: gmpInteger; const Right: Single): Boolean;
var
   d: Double;
begin
   d := Right;
   Result := mpz_cmp_d(Left.NativeObject^, d) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: gmpInteger; const Right: Double): Boolean;
begin
   Result := mpz_cmp_d(Left.NativeObject^, Right) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: Integer; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_si(Right.NativeObject^, Left) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: Cardinal; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_ui(Right.NativeObject^, Left) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: Int64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: UInt64; const Right: gmpInteger): Boolean;
begin
   LocalInteger.Assign(Left);
   Result := mpz_cmp(Right.NativeObject^, LocalInteger.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: Single; const Right: gmpInteger): Boolean;
var
   d: Double;
begin
   d := Left;
   Result := mpz_cmp_d(Right.NativeObject^, d) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.LessThanOrEqual(const Left: Double; const Right: gmpInteger): Boolean;
begin
   Result := mpz_cmp_d(Right.NativeObject^, Left) >= 0;
   Right.ReleaseTemporary;
end;

// BitwiseNot operator functions

class operator gmpInteger.LogicalNot(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_com(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// BitwiseAnd operator functions

class operator gmpInteger.BitwiseAnd(const Left, Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseAnd(const Left: Integer; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseAnd(const Left: Cardinal; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseAnd(const Left: Int64; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseAnd(const Left: UInt64; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseAnd(const Left: gmpInteger; const Right: Integer): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseAnd(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseAnd(const Left: gmpInteger; const Right: Int64): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseAnd(const Left: gmpInteger; const Right: UInt64): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_and(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

// BitwiseOr operator functions

class operator gmpInteger.BitwiseOr(const Left, Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseOr(const Left: Integer; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseOr(const Left: Cardinal; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseOr(const Left: Int64; const Right: gmpInteger): gmpInteger;  begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseOr(const Left: UInt64; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseOr(const Left: gmpInteger; const Right: Integer): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseOr(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseOr(const Left: gmpInteger; const Right: Int64): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseOr(const Left: gmpInteger; const Right: UInt64): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_ior(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

// BitwiseXor operator functions

class operator gmpInteger.BitwiseXor(const Left, Right: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseXor(const Left: Integer; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseXor(const Left: Cardinal; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseXor(const Left: Int64; const Right: gmpInteger): gmpInteger;  begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseXor(const Left: UInt64; const Right: gmpInteger): gmpInteger;
begin
   LocalInteger.Assign(Left);
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, LocalInteger.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseXor(const Left: gmpInteger; const Right: Integer): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseXor(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseXor(const Left: gmpInteger; const Right: Int64): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpInteger.BitwiseXor(const Left: gmpInteger; const Right: UInt64): gmpInteger;
begin
   LocalInteger.Assign(Right);
   Result := GetTemporaryInteger()^;
   mpz_xor(Result.NativeObject^, Left.NativeObject^, LocalInteger.NativeObject^);
   Left.ReleaseTemporary;
end;

// ################### LeftShift operator functions ###################

class operator gmpInteger.LeftShift(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_mul_2exp(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

// ################### RightShift operator functions ###################

class operator gmpInteger.RightShift(const Left: gmpInteger; const Right: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_tdiv_q_2exp(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

{ gmpRational }

procedure gmpRational.CreateGeneric;
begin
   try
      NativeType := gmpotRational;
      RelativeNode := nil;
      New(NativeObject); // allocate memory for prototype variable
      mpq_init(NativeObject^); // initialize prototype and allocate memory in gmp DLL.
   except
      raise gmpException.Create(gmpeFatalError);
   end;
end;

function gmpRational.FGetIsTemporary: Boolean;
begin
   Result := Assigned(RelativeNode);
end;

procedure gmpRational.ReleaseTemporary;
begin
   if Assigned(RelativeNode) then
      ReleaseTemporaryRational(Self);
end;

constructor gmpRational.Create(const Value: Integer);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: Cardinal);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: Int64);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: UInt64);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: Single);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: Double);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: AnsiString);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: AnsiString; Base: Integer);
begin
   CreateGeneric;
   Assign(Value, Base);
end;

constructor gmpRational.Create(const Value: gmpInteger);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: gmpRational);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: pgmpRational);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Value: pgmpFloat);
begin
   CreateGeneric;
   Assign(Value);
end;

constructor gmpRational.Create(const Numerator, Denominator: Integer);
begin
   if Denominator = 0 then
      raise gmpException.Create(gmpeDivideByZero);

   CreateGeneric;
   mpq_set_si(NativeObject^, Numerator, Cardinal(System.Abs(Denominator)));
   if Denominator < 0 then
      mpq_neg(NativeObject^, NativeObject^);

   mpq_canonicalize(NativeObject^);
end;

constructor gmpRational.Create(const Numerator, Denominator: Int64);
begin
   if Denominator = 0 then
      raise gmpException.Create(gmpeDivideByZero);

   CreateGeneric;

   LocalInteger.Assign(Denominator);
   mpq_set_den(NativeObject^, LocalInteger.NativeObject^);
   LocalInteger.Assign(Numerator);
   mpq_set_num(NativeObject^, LocalInteger.NativeObject^);

   mpq_canonicalize(NativeObject^);
end;

procedure gmpRational.Free;
begin
   try
      mpq_clear(NativeObject^); // free memory in gmp DLL
      Dispose(NativeObject); // free memory of prototype variable
   except
      raise gmpException.Create(gmpeFatalError);
   end;
end;

constructor gmpRational.Create(const Numerator, Denominator: gmpInteger);
begin
   Numerator.ReleaseTemporary;
   Denominator.ReleaseTemporary;

   if Denominator = 0 then
      raise gmpException.Create(gmpeDivideByZero);

   CreateGeneric;

   mpq_set_den(NativeObject^, Denominator.NativeObject^);
   mpq_set_num(NativeObject^, Numerator.NativeObject^);
   mpq_canonicalize(NativeObject^);
end;

procedure gmpRational.Assign(const Value: Integer);
begin
   mpq_set_si(NativeObject^, Value, 1);
end;

procedure gmpRational.Assign(const Value: Cardinal);
begin
   mpq_set_ui(NativeObject^, Value, 1);
end;

procedure gmpRational.Assign(const Value: Int64);
begin
   LocalInteger.Assign(Value);
   mpq_set_z(NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpRational.Assign(const Value: UInt64);
begin
   LocalInteger.Assign(Value);
   mpq_set_z(NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpRational.Assign(const Value: Single);
var
   tmp: Double;
begin
   tmp := Value;
   mpq_set_d(NativeObject^, tmp);
end;

procedure gmpRational.Assign(const Value: Double);
begin
   mpq_set_d(NativeObject^, Value);
end;

procedure gmpRational.Assign(const Value: AnsiString);
begin
   if mpq_set_str(NativeObject^, PAnsiChar(Value), 10) <> 0 then
      raise gmpException.Create(gmpeConversionError);
end;

procedure gmpRational.Assign(const Value: AnsiString; Base: Integer);
begin
   if (Base = 0) or ((Base >= 2) and (Base <= 62)) then
   begin
      if mpq_set_str(NativeObject^, PAnsiChar(Value), Base) <> 0 then
         raise gmpException.Create(gmpeConversionError);
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

procedure gmpRational.Assign(const Value: gmpInteger);
begin
   mpq_set_z(NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpRational.Assign(const Value: gmpRational);
begin
   mpq_set(NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpRational.Assign(const Value: pgmpRational);
begin
   mpq_set(NativeObject^, Value^.NativeObject^);
   Value^.ReleaseTemporary;
end;

procedure gmpRational.Assign(const Value: pgmpFloat);
begin
   mpq_set_f(NativeObject^, Value^.NativeObject^);
   Value^.ReleaseTemporary;
end;

procedure gmpRational.Assign(const Numerator, Denominator: gmpInteger);
begin
   Numerator.ReleaseTemporary;
   Denominator.ReleaseTemporary;

   if Denominator = 0 then
      raise gmpException.Create(gmpeDivideByZero);

   mpq_set_den(NativeObject^, Denominator.NativeObject^);
   mpq_set_num(NativeObject^, Numerator.NativeObject^);
   mpq_canonicalize(NativeObject^);
end;

procedure gmpRational.Canonicalize;
begin
   mpq_canonicalize(NativeObject^);
   ReleaseTemporary;
end;

////////////////////

procedure gmpRational.SetZero;
begin
   mpq_set_si(NativeObject^, 0, 1);
end;

procedure gmpRational.SetNegative;
begin
   mpq_neg(NativeObject^, NativeObject^);
end;

procedure gmpRational.SetAbsolute;
begin
   mpq_abs(NativeObject^, NativeObject^);
end;

procedure gmpRational.SetReciprocal;
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   mpq_inv(NativeObject^, NativeObject^);
end;

procedure gmpRational.SetSquare;
begin
   GetNumerator(LocalInteger);
   LocalInteger.SetSquare;
   mpq_set_num(NativeObject^, LocalInteger.NativeObject^);

   GetDenominator(LocalInteger);
   LocalInteger.SetSquare;
   mpq_set_den(NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpRational.SetCubic;
begin
   GetNumerator(LocalInteger);
   LocalInteger.SetCubic;
   mpq_set_num(NativeObject^, LocalInteger.NativeObject^);

   GetDenominator(LocalInteger);
   LocalInteger.SetCubic;
   mpq_set_den(NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpRational.SetIntPower(Exp: Cardinal);
begin
   GetNumerator(LocalInteger);
   LocalInteger.SetIntPower(Exp);
   mpq_set_num(NativeObject^, LocalInteger.NativeObject^);

   GetDenominator(LocalInteger);
   LocalInteger.SetIntPower(Exp);
   mpq_set_den(NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpRational.SetMul2Exp(const Base: gmpRational; N: mp_bitcnt_t);
begin
   mpq_mul_2exp(NativeObject^, Base.NativeObject^, N);
   Base.ReleaseTemporary;
end;

procedure gmpRational.SetDiv2Exp(const Base: gmpRational; N: mp_bitcnt_t);
begin
   mpq_div_2exp(NativeObject^, Base.NativeObject^, N);
   Base.ReleaseTemporary;
end;

// Manipulate num/den separately

procedure gmpRational.SetNumerator(const Value: gmpInteger);
begin
   mpq_set_num(NativeObject^, Value.NativeObject^);
   mpq_canonicalize(NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpRational.SetDenominator(const Value: gmpInteger);
begin
   Value.ReleaseTemporary;

   if mpz_cmp_ui(Value.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);

   mpq_set_den(NativeObject^, Value.NativeObject^);
   mpq_canonicalize(NativeObject^);
end;

procedure gmpRational.GetNumerator(var Output: gmpInteger);
begin
   mpq_get_num(Output.NativeObject^, NativeObject^);
end;

procedure gmpRational.GetDenominator(var Output: gmpInteger);
begin
   mpq_get_den(Output.NativeObject^, NativeObject^);
end;

// Output methods

function gmpRational.ToDouble: Double;
begin
   Result := mpq_get_d(NativeObject^);
   ReleaseTemporary;
end;

function gmpRational.ToString(Base: Integer = 10): AnsiString;
begin
   LocalFloat.Assign(Self);
   Result := LocalFloat.ToString(Base);
end;

function gmpRational.ToStringFraction(Base: Integer = 10): AnsiString;
var
   size: Integer;
   num_z: pmpz_t;
   den_z: pmpz_t;
begin
   ReleaseTemporary;

   if (Base >= 2) and (Base <= 36) then
   begin
      num_z := mpq_numref(NativeObject^);
      den_z := mpq_denref(NativeObject^);
      size := mpz_sizeinbase(num_z^, Base) + mpz_sizeinbase(den_z^, Base) + 2;
      try
         SetLength(Result, size);
      except
         raise gmpException.Create(gmpeFatalError);
      end;
      mpq_get_str(PAnsiChar(Result), Base, NativeObject^);
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

// Increment methods

procedure gmpRational.Inc;
begin
   LocalRational.Assign(1);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: Integer);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: Cardinal);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: Int64);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: UInt64);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: Single);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: Double);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: AnsiString);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: AnsiString; Base: Integer);
begin
   LocalRational.Assign(Value, Base);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: gmpInteger);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Inc(const Value: gmpRational);
begin
   mpq_add(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpRational.Inc(const Value: pgmpRational);
begin
   mpq_add(NativeObject^, NativeObject^, Value^.NativeObject^);
   Value^.ReleaseTemporary;
end;

procedure gmpRational.Inc(const Value: pgmpFloat);
begin
   LocalRational.Assign(Value);
   mpq_add(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec;
begin
   LocalRational.Assign(1);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: Integer);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: Cardinal);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: Int64);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: UInt64);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: Single);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: Double);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: AnsiString);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: AnsiString; Base: Integer);
begin
   LocalRational.Assign(Value, Base);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: gmpInteger);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Dec(const Value: gmpRational);
begin
   mpq_sub(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpRational.Dec(const Value: pgmpRational);
begin
   mpq_sub(NativeObject^, NativeObject^, Value^.NativeObject^);
   Value^.ReleaseTemporary;
end;

procedure gmpRational.Dec(const Value: pgmpFloat);
begin
   LocalRational.Assign(Value);
   mpq_sub(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

// Multiply and Divide methods

procedure gmpRational.MultiplyBy(const Value: Integer);
begin
   LocalRational.Assign(Value);
   mpq_mul(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.MultiplyBy(const Value: Cardinal);
begin
   LocalRational.Assign(Value);
   mpq_mul(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.MultiplyBy(const Value: Int64);
begin
   LocalRational.Assign(Value);
   mpq_mul(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.MultiplyBy(const Value: UInt64);
begin
   LocalRational.Assign(Value);
   mpq_mul(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.MultiplyBy(const Value: Single);
begin
   LocalRational.Assign(Value);
   mpq_mul(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.MultiplyBy(const Value: Double);
begin
   LocalRational.Assign(Value);
   mpq_mul(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.MultiplyBy(const Value: gmpInteger);
begin
   LocalRational.Assign(Value);
   mpq_mul(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.MultiplyBy(const Value: gmpRational);
begin
   mpq_mul(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpRational.MultiplyBy(const Value: pgmpRational);
begin
   mpq_mul(NativeObject^, NativeObject^, Value^.NativeObject^);
   Value^.ReleaseTemporary;
end;

procedure gmpRational.MultiplyBy(const Value: pgmpFloat);
begin
   LocalRational.Assign(Value);
   mpq_mul(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: Integer);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalRational.Assign(Value);
   mpq_div(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: Cardinal);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalRational.Assign(Value);
   mpq_div(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: Int64);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalRational.Assign(Value);
   mpq_div(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: UInt64);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalRational.Assign(Value);
   mpq_div(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: Single);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0.0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalRational.Assign(Value);
   mpq_div(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: Double);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0.0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalRational.Assign(Value);
   mpq_div(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: gmpInteger);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_ui(Value.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Value);
   {$ENDIF}

   LocalRational.Assign(Value);
   mpq_div(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: gmpRational);
begin
   Value.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Value.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   mpq_div(NativeObject^, NativeObject^, Value.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: pgmpRational);
begin
   Value^.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Value^.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   mpq_div(NativeObject^, NativeObject^, Value^.NativeObject^);
end;

procedure gmpRational.DivideBy(const Value: pgmpFloat);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Value^.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Value^);
   {$ENDIF}

   LocalRational.Assign(Value);
   mpq_div(NativeObject^, NativeObject^, LocalRational.NativeObject^);
end;

procedure gmpRational.Mul2Exp(const N: mp_bitcnt_t);
begin
   mpq_mul_2exp(NativeObject^, NativeObject^, N);
end;

procedure gmpRational.Div2Exp(const N: mp_bitcnt_t);
begin
   mpq_div_2exp(NativeObject^, NativeObject^, N);
end;

// Explicit casting

class operator gmpRational.Explicit(const Value: Integer): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: Cardinal): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: Int64): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: UInt64): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: Single): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: Double): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: AnsiString): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: gmpInteger): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: pgmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

class operator gmpRational.Explicit(const Value: pgmpFloat): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

// Negative and Positive operators

class operator gmpRational.Negative(const Value: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   mpq_neg(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

class operator gmpRational.Positive(const Value: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
end;

// Add operator functions

class operator gmpRational.Add(const Left, Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   mpq_add(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Add(const Left: gmpRational; const Right: Integer): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpRational.Add(const Left: gmpRational; const Right: Cardinal): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpRational.Add(const Left: gmpRational; const Right: Int64): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpRational.Add(const Left: gmpRational; const Right: UInt64): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpRational.Add(const Left: gmpRational; const Right: Single): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpRational.Add(const Left: gmpRational; const Right: Double): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpRational.Add(const Left: gmpRational; const Right: gmpInteger): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpRational.Add(const Left: gmpRational; const Right: pgmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Right);
   Result.Inc(Left);
end;

class operator gmpRational.Add(const Left: Integer; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpRational.Add(const Left: Cardinal; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpRational.Add(const Left: Int64; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpRational.Add(const Left: UInt64; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpRational.Add(const Left: Single; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpRational.Add(const Left: Double; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpRational.Add(const Left: gmpInteger; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

class operator gmpRational.Add(const Left: pgmpRational; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Inc(Right);
end;

// Subtract operator functions

class operator gmpRational.Subtract(const Left, Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   mpq_sub(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Subtract(const Left: gmpRational; const Right: Integer): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: gmpRational; const Right: Cardinal): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: gmpRational; const Right: Int64): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: gmpRational; const Right: UInt64): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: gmpRational; const Right: Single): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: gmpRational; const Right: Double): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: gmpRational; const Right: gmpInteger): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: gmpRational; const Right: pgmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: Integer; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: Cardinal; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: Int64; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: UInt64; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: Single; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: Double; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: gmpInteger; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

class operator gmpRational.Subtract(const Left: pgmpRational; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Left);
   Result.Dec(Right);
end;

// Multiply operator functions

class operator gmpRational.Multiply(const Left, Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: Integer; const Right: gmpRational): gmpRational;
begin
   LocalRational.Assign(Left);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: Cardinal; const Right: gmpRational): gmpRational;
begin
   LocalRational.Assign(Left);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: Int64; const Right: gmpRational): gmpRational;
begin
   LocalRational.Assign(Left);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: UInt64; const Right: gmpRational): gmpRational;
begin
   LocalRational.Assign(Left);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: Single; const Right: gmpRational): gmpRational;
begin
   LocalRational.Assign(Left);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: Double; const Right: gmpRational): gmpRational;
begin
   LocalRational.Assign(Left);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpInteger; const Right: gmpRational): gmpRational;
begin
   LocalRational.Assign(Left);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: pgmpRational; const Right: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left^.NativeObject^, Right.NativeObject^);
   Left^.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpRational; const Right: Integer): gmpRational;
begin
   LocalRational.Assign(Right);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpRational; const Right: Cardinal): gmpRational;
begin
   LocalRational.Assign(Right);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpRational; const Right: Int64): gmpRational;
begin
   LocalRational.Assign(Right);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpRational; const Right: UInt64): gmpRational;
begin
   LocalRational.Assign(Right);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpRational; const Right: Single): gmpRational;
begin
   LocalRational.Assign(Right);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpRational; const Right: Double): gmpRational;
begin
   LocalRational.Assign(Right);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpRational; const Right: gmpInteger): gmpRational;
begin
   LocalRational.Assign(Right);
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpRational.Multiply(const Left: gmpRational; const Right: pgmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   mpq_mul(Result.NativeObject^, Left.NativeObject^, Right^.NativeObject^);
   Left.ReleaseTemporary;
   Right^.ReleaseTemporary;
end;

// Division operator functions

class operator gmpRational.Divide(const Left, Right: gmpRational): gmpRational;
begin
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   mpq_div(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
end;

class operator gmpRational.Divide(const Left: Integer; const Right: gmpRational): gmpRational;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^)
end;

class operator gmpRational.Divide(const Left: Cardinal; const Right: gmpRational): gmpRational;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
end;

class operator gmpRational.Divide(const Left: Int64; const Right: gmpRational): gmpRational;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
end;

class operator gmpRational.Divide(const Left: UInt64; const Right: gmpRational): gmpRational;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
end;

class operator gmpRational.Divide(const Left: Single; const Right: gmpRational): gmpRational;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
end;

class operator gmpRational.Divide(const Left: Double; const Right: gmpRational): gmpRational;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpInteger; const Right: gmpRational): gmpRational;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero, Left);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Left);
   mpq_div(Result.NativeObject^, LocalRational.NativeObject^, Right.NativeObject^);
end;

class operator gmpRational.Divide(const Left: pgmpRational; const Right: gmpRational): gmpRational;
begin
   Left^.ReleaseTemporary;
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   mpq_div(Result.NativeObject^, Left^.NativeObject^, Right.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpRational; const Right: Integer): gmpRational;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpRational; const Right: Cardinal): gmpRational;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpRational; const Right: Int64): gmpRational;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpRational; const Right: UInt64): gmpRational;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpRational; const Right: Single): gmpRational;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0.0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpRational; const Right: Double): gmpRational;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0.0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpRational; const Right: gmpInteger): gmpRational;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Right);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   LocalRational.Assign(Right);
   mpq_div(Result.NativeObject^, Left.NativeObject^, LocalRational.NativeObject^);
end;

class operator gmpRational.Divide(const Left: gmpRational; const Right: pgmpRational): gmpRational;
begin
   Left.ReleaseTemporary;
   Right^.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right^.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryRational()^;
   mpq_div(Result.NativeObject^, Left.NativeObject^, Right^.NativeObject^);
end;

// Equal operator functions

class operator gmpRational.Equal(const Left, Right: gmpRational): Boolean;
begin
   Result := mpq_equal(Left.NativeObject^, Right.NativeObject^) <> 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpRational; const Right: Integer): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpRational; const Right: Cardinal): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpRational; const Right: Int64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpRational; const Right: UInt64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpRational; const Right: Single): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpRational; const Right: Double): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpRational; const Right: gmpInteger): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpRational; const Right: pgmpRational): Boolean;
begin
   Result := mpq_equal(Left.NativeObject^, Right^.NativeObject^) <> 0;
   Left.ReleaseTemporary;
   Right^.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: Integer; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: Cardinal; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: Int64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: UInt64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: Single; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: Double; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: gmpInteger; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.Equal(const Left: pgmpRational; const Right: gmpRational): Boolean;
begin
   Result := mpq_equal(Left^.NativeObject^, Right.NativeObject^) <> 0;
   Left^.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

// Not equal operator functions

class operator gmpRational.NotEqual(const Left, Right: gmpRational): Boolean;
begin
   Result := mpq_equal(Left.NativeObject^, Right.NativeObject^) = 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpRational; const Right: Integer): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpRational; const Right: Cardinal): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpRational; const Right: Int64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpRational; const Right: UInt64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpRational; const Right: Single): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpRational; const Right: Double): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpRational; const Right: gmpInteger): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_equal(Left.NativeObject^, LocalRational.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpRational; const Right: pgmpRational): Boolean;
begin
   Result := mpq_equal(Left.NativeObject^, Right^.NativeObject^) = 0;
   Left.ReleaseTemporary;
   Right^.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: Integer; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: Cardinal; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: Int64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: UInt64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: Single; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: Double; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: gmpInteger; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_equal(LocalRational.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.NotEqual(const Left: pgmpRational; const Right: gmpRational): Boolean;
begin
   Result := mpq_equal(Left^.NativeObject^, Right.NativeObject^) = 0;
   Left^.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

// GreaterThan operator functions

class operator gmpRational.GreaterThan(const Left, Right: gmpRational): Boolean;
begin
   Result := mpq_cmp(Left.NativeObject^, Right.NativeObject^) > 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpRational; const Right: Integer): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpRational; const Right: Cardinal): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpRational; const Right: Int64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpRational; const Right: UInt64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpRational; const Right: Single): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpRational; const Right: Double): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpRational; const Right: gmpInteger): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpRational; const Right: pgmpRational): Boolean;
begin
   Result := mpq_cmp(Left.NativeObject^, Right^.NativeObject^) > 0;
   Left.ReleaseTemporary;
   Right^.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: Integer; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: Cardinal; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: Int64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: UInt64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: Single; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: Double; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: gmpInteger; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThan(const Left: pgmpRational; const Right: gmpRational): Boolean;
begin
   Result := mpq_cmp(Left^.NativeObject^, Right.NativeObject^) > 0;
   Left^.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

// GreaterThanOrEqual operator functions

class operator gmpRational.GreaterThanOrEqual(const Left, Right: gmpRational): Boolean;
begin
   Result := mpq_cmp(Left.NativeObject^, Right.NativeObject^) >= 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpRational; const Right: Integer): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpRational; const Right: Cardinal): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpRational; const Right: Int64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpRational; const Right: UInt64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpRational; const Right: Single): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpRational; const Right: Double): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpRational; const Right: gmpInteger): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpRational; const Right: pgmpRational): Boolean;
begin
   Result := mpq_cmp(Left.NativeObject^, Right^.NativeObject^) >= 0;
   Left.ReleaseTemporary;
   Right^.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: Integer; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: Cardinal; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: Int64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: UInt64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: Single; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: Double; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: gmpInteger; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.GreaterThanOrEqual(const Left: pgmpRational; const Right: gmpRational): Boolean;
begin
   Result := mpq_cmp(Left^.NativeObject^, Right.NativeObject^) >= 0;
   Left^.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

// LessThan operator functions

class operator gmpRational.LessThan(const Left, Right: gmpRational): Boolean;
begin
   Result := mpq_cmp(Left.NativeObject^, Right.NativeObject^) < 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpRational; const Right: Integer): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpRational; const Right: Cardinal): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpRational; const Right: Int64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpRational; const Right: UInt64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpRational; const Right: Single): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpRational; const Right: Double): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpRational; const Right: gmpInteger): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpRational; const Right: pgmpRational): Boolean;
begin
   Result := mpq_cmp(Left.NativeObject^, Right^.NativeObject^) < 0;
   Left.ReleaseTemporary;
   Right^.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: Integer; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: Cardinal; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: Int64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: UInt64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: Single; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: Double; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: gmpInteger; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThan(const Left: pgmpRational; const Right: gmpRational): Boolean;
begin
   Result := mpq_cmp(Left^.NativeObject^, Right.NativeObject^) < 0;
   Left^.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

// LessThanOrEqual operator functions

class operator gmpRational.LessThanOrEqual(const Left, Right: gmpRational): Boolean;
begin
   Result := mpq_cmp(Left.NativeObject^, Right.NativeObject^) <= 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpRational; const Right: Integer): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpRational; const Right: Cardinal): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpRational; const Right: Int64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpRational; const Right: UInt64): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpRational; const Right: Single): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpRational; const Right: Double): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpRational; const Right: gmpInteger): Boolean;
begin
   LocalRational.Assign(Right);
   Result := mpq_cmp(Left.NativeObject^, LocalRational.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpRational; const Right: pgmpRational): Boolean;
begin
   Result := mpq_cmp(Left.NativeObject^, Right^.NativeObject^) <= 0;
   Left.ReleaseTemporary;
   Right^.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: Integer; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: Cardinal; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: Int64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: UInt64; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: Single; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: Double; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: gmpInteger; const Right: gmpRational): Boolean;
begin
   LocalRational.Assign(Left);
   Result := mpq_cmp(LocalRational.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpRational.LessThanOrEqual(const Left: pgmpRational; const Right: gmpRational): Boolean;
begin
   Result := mpq_cmp(Left^.NativeObject^, Right.NativeObject^) <= 0;
   Left^.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

{ gmpFloat }

procedure gmpFloat.CreateGeneric(Precision: Cardinal = 0);
begin
   if (Precision > 0) and (Precision < GMP_MIN_PRECISION) then
      raise gmpException.Create(gmpeFloatPrecisionTooLow);

   try
      NativeType := gmpotFloat;
      RelativeNode := nil;
      New(NativeObject); // allocate memory for prototype variable

      // initialize prototype and allocate memory in gmp DLL.
      if Precision = 0 then
      begin
         mpf_init(NativeObject^);
         PrecisionBitCount := CurrentTemporaryFloatPrecision;
      end
      else
      begin
         mpf_init2(NativeObject^, Precision);
         PrecisionBitCount := Precision;
      end;

      // Make sure that temporary floats have higher precision than external variables.
      CheckTemporaryFloatPrecision(Precision);
   except
      raise gmpException.Create(gmpeFatalError);
   end;
end;

function gmpFloat.FGetIsTemporary: Boolean;
begin
   Result := Assigned(RelativeNode);
end;

function gmpFloat.FGetIsInteger: Boolean;
begin
   Result := mpf_integer_p(NativeObject^) <> 0;
   ReleaseTemporary;
end;

function gmpFloat.FGetPrecision: mp_bitcnt_t;
begin
   Result := PrecisionBitCount;
   ReleaseTemporary;
end;

procedure gmpFloat.FSetPrecision(Value: mp_bitcnt_t);
begin
   if (Value > 0) and (Value < GMP_MIN_PRECISION) then
      raise gmpException.Create(gmpeFloatPrecisionTooLow);

   mpf_set_prec(NativeObject^, Value);
   PrecisionBitCount := Value;
   ReleaseTemporary;
   CheckTemporaryFloatPrecision(Value);
end;

procedure gmpFloat.ReleaseTemporary;
begin
   if Assigned(RelativeNode) then
      ReleaseTemporaryFloat(Self);
end;

constructor gmpFloat.Create(const Value: Integer; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: Cardinal; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: Int64; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: UInt64; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: Single; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: Double; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: AnsiString; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: AnsiString; Base: Integer; Precision: mp_bitcnt_t);
begin
   CreateGeneric(Precision);
   Assign(Value, Base);
end;

constructor gmpFloat.Create(const Value: gmpInteger; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: gmpRational; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: gmpFloat; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

constructor gmpFloat.Create(const Value: pgmpRational; Precision: mp_bitcnt_t = 0);
begin
   CreateGeneric(Precision);
   Assign(Value);
end;

procedure gmpFloat.Free;
begin
   try
      mpf_clear(NativeObject^); // free memory in gmp DLL
      Dispose(NativeObject); // free memory of prototype variable
   except
      raise gmpException.Create(gmpeFatalError);
   end;
end;

procedure gmpFloat.Assign(const Value: Integer);
begin
   mpf_set_si(NativeObject^, Value);
end;

procedure gmpFloat.Assign(const Value: Cardinal);
begin
   mpf_set_ui(NativeObject^, Value);
end;

procedure gmpFloat.Assign(const Value: Int64);
begin
   LocalInteger.Assign(Value);
   mpf_set_z(NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpFloat.Assign(const Value: UInt64);
begin
   LocalInteger.Assign(Value);
   mpf_set_z(NativeObject^, LocalInteger.NativeObject^);
end;

procedure gmpFloat.Assign(const Value: Single);
var
   tmp: Double;
begin
   tmp := Value;
   mpf_set_d(NativeObject^, tmp);
end;

procedure gmpFloat.Assign(const Value: Double);
begin
   mpf_set_d(NativeObject^, Value);
end;

procedure gmpFloat.Assign(const Value: AnsiString);
begin
   if mpf_set_str(NativeObject^, PAnsiChar(Value), 10) <> 0 then
      raise gmpException.Create(gmpeConversionError);
end;

procedure gmpFloat.Assign(const Value: AnsiString; Base: Integer);
begin
   if (Abs(Base) >= 2) and (Abs(Base) <= 62) then
   begin
      if mpf_set_str(NativeObject^, PAnsiChar(Value), Base) <> 0 then
         raise gmpException.Create(gmpeConversionError);
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

procedure gmpFloat.Assign(const Value: gmpInteger);
begin
   mpf_set_z(NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpFloat.Assign(const Value: gmpRational);
begin
   mpf_set_q(NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpFloat.Assign(const Value: gmpFloat);
begin
   mpf_set(NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpFloat.Assign(const Value: pgmpRational);
begin
   mpf_set_q(NativeObject^, Value^.NativeObject^);
   Value^.ReleaseTemporary;
end;

// Set functions

procedure gmpFloat.SetZero;
begin
   mpf_set_si(NativeObject^, 0);
end;

procedure gmpFloat.SetNegative;
begin
   mpf_neg(NativeObject^, NativeObject^);
end;

procedure gmpFloat.SetAbsolute;
begin
   mpf_abs(NativeObject^, NativeObject^);
end;

procedure gmpFloat.SetReciprocal;
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_si(NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}
   mpf_ui_div(NativeObject^, 1, NativeObject^);
end;

procedure gmpFloat.SetSquare;
begin
   mpf_pow_ui(NativeObject^, NativeObject^, 2);
end;

procedure gmpFloat.SetCubic;
begin
   mpf_pow_ui(NativeObject^, NativeObject^, 3);
end;

procedure gmpFloat.SetIntPower(Exp: Cardinal);
begin
   mpf_pow_ui(NativeObject^, NativeObject^, Exp);
end;

procedure gmpFloat.SetPower(Exp: gmpFloat);
begin
   mpf_pow(NativeObject^, NativeObject^, Exp.NativeObject^);
   Exp.ReleaseTemporary;
end;

procedure gmpFloat.SetSqrt;
begin
   mpf_sqrt(NativeObject^, NativeObject^);
end;

procedure gmpFloat.SetNthRoot(N: Cardinal);
begin
   LocalFloat.Assign(N);
   LocalFloat.SetReciprocal; // 1/N
   mpf_pow(NativeObject^, NativeObject^, LocalFloat.NativeObject^); // Self^(1/N)
end;

procedure gmpFloat.SetExp;
begin
   mpf_exp(NativeObject^, NativeObject^);
end;

procedure gmpFloat.SetLn;
begin
   mpf_ln(NativeObject^, NativeObject^);
end;

procedure gmpFloat.SetMul2Exp(const Base: gmpFloat; N: mp_bitcnt_t);
begin
   mpf_mul_2exp(NativeObject^, Base.NativeObject^, N);
   Base.ReleaseTemporary;
end;

procedure gmpFloat.SetDiv2Exp(const Base: gmpFloat; N: mp_bitcnt_t);
begin
   mpf_div_2exp(NativeObject^, Base.NativeObject^, N);
   Base.ReleaseTemporary;
end;

procedure gmpFloat.SetRandom;
begin
   mpf_urandomb(NativeObject^, RandomState, PrecisionBitCount);
end;

// Output methods

function gmpFloat.ToInteger: Integer;
begin
   ReleaseTemporary;

   if mpf_fits_sint_p(NativeObject^) = 0 then
      raise gmpException.Create(gmpeConversionError);

   Result := mpf_get_si(NativeObject^);
end;

function gmpFloat.ToCardinal: Cardinal;
begin
   ReleaseTemporary;

   if mpf_fits_uint_p(NativeObject^) = 0 then
      raise gmpException.Create(gmpeConversionError);

   Result := mpf_get_ui(NativeObject^);
end;

function gmpFloat.ToDouble: Double;
begin
   Result := mpf_get_d(NativeObject^);
   ReleaseTemporary;
end;

procedure gmpFloat.To2Exp(var D: Double; var Exp: Integer);
begin
   D := mpf_get_d_2exp(Exp, NativeObject^);
   ReleaseTemporary;
end;

function gmpFloat.ToString(Base: Integer = 10): AnsiString;
var
   size: mp_size_t;
   Exponent: mp_exp_t;
   i: Integer;
   tmpResult: AnsiString;
begin
   ReleaseTemporary;

   if ((Base <= 62) and (Base >= 2)) or ((Base <= -2) and (Base >= -36)) then
   begin
      size := mpf_get_prec(NativeObject^); // this is actually bit_count, but bit_count is always bigger than digit_count

      try
         SetLength(tmpResult, size);
      except
         // potential memory allocation failure
         raise gmpException.Create(gmpeFatalError);
      end;

      mpf_get_str(PAnsiChar(tmpResult), Exponent, Base, size, NativeObject^);
      size := StrLen(PAnsiChar(tmpResult));

      // Resize string
      if Exponent > 0 then
      begin
         if tmpResult[1] = AnsiChar('-') then
            i := Exponent + 1
         else
            i := Exponent;

         // Move '.' to the right place
         try
            if size - i = 0 then
               SetLength(Result, size) // no '.'
            else
               SetLength(Result, size + 1); // 1 for '.'
         except
            raise gmpException.Create(gmpeFatalError);
         end;

         Move(tmpResult[1], Result[1], i * SizeOf(AnsiChar));
         if size - i <> 0 then
         begin
            Result[i + 1] := AnsiChar('.');
            Move(tmpResult[i + 1], Result[i + 2], size - i);
         end;
      end
      else if Exponent >= -SMALL_FLOAT_MAX_ZERO_PREFIX then
      begin
         if (Exponent = 0) and (size = 0) then
         begin
            Result := '0';
            Exit;
         end;

         // Add "0.000 ..." as prefix and Exponent must be negative.
         try
            SetLength(Result, size + 2 - Exponent); // 2 for '0.', (-Exponent) for the following zeros.
         except
            raise gmpException.Create(gmpeFatalError);
         end;

         if tmpResult[1] = AnsiChar('-') then
         begin
            Result[1] := AnsiChar('-');
            Result[2] := AnsiChar('0');
            Result[3] := AnsiChar('.');

            for i := 4 to 3 - Exponent do
               Result[i] := '0';

            Move(tmpResult[2], Result[4 - Exponent], (size - 1) * SizeOf(AnsiChar));
         end
         else
         begin
            Result[1] := AnsiChar('0');
            Result[2] := AnsiChar('.');

            for i := 3 to 2 - Exponent do
               Result[i] := '0';

            Move(tmpResult[1], Result[3 - Exponent], size * SizeOf(AnsiChar));
         end;
      end
      else
      begin
         // Output as exponent format
         Result := ToStringExp(Base);
      end;
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

function gmpFloat.ToStringExp(Base: Integer = 10; DigitCount: Integer = 0): AnsiString;
var
   size: mp_size_t;
   Exponent: mp_exp_t;
   i: Integer;
   expString: AnsiString;
   IntPtr: Integer;
begin
   ReleaseTemporary;

   if ((Base <= 62) and (Base >= 2)) or ((Base <= -2) and (Base >= -36)) then
   begin
      if DigitCount = 0 then
         size := mpf_get_prec(NativeObject^) // this is actually bit_count, but bit_count is always bigger than digit_count
      else
         size := DigitCount;
{
      try
         SetLength(Result, size);
      except
      end;
      mpf_get_str(PAnsiChar(Result), Exponent, Base, size, NativeObject^);
      size := StrLen(PAnsiChar(Result));
      SetLength(Result, size);
      Result := AnsiString('0.') + Result + AnsiString(' E ') + AnsiString(IntToStr(Exponent));
}
      // Optimized as such. You can modify the output format as you wish.

      try
         SetLength(Result, size + 2); // 2 for '0.'
      except
         raise gmpException.Create(gmpeFatalError);
      end;

      IntPtr := Integer(PAnsiChar(Result));
      mpf_get_str(PAnsiChar(IntPtr + SizeOf(AnsiChar) * 2), Exponent, Base, size, NativeObject^);

      expString := AnsiString(IntToStr(Exponent));

      // Resize string
      size := StrLen(PAnsiChar(IntPtr + SizeOf(AnsiChar) * 2));
      SetLength(Result, size + 2 + 3 + Length(expString)); // 2 for '0.' and 3 for ' E '

      if (Exponent = 0) and (size = 0) then
      begin
         Result := '0 E 0';
         Exit;
      end;

      // Fill '0.'
      if Result[3] = AnsiChar('-') then // Negative
      begin
         Result[1] := AnsiChar('-');
         Result[2] := AnsiChar('0');
         Result[3] := AnsiChar('.');
      end
      else
      begin
         Result[1] := AnsiChar('0');
         Result[2] := AnsiChar('.');
      end;

      // Append ' E '
      Result[size + 3] := AnsiChar(' ');
      Result[size + 4] := AnsiChar('E');
      Result[size + 5] := AnsiChar(' ');

      // Append exponent string
      for i := 1 to Length(expString) do
         Result[size + 5 + i] := expString[i];
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

function gmpFloat.ToStringRaw(var Exp: Integer; Base: Integer = 10; DigitCount: Integer = 0): AnsiString;
var
   size: mp_size_t;
begin
   ReleaseTemporary;

   if ((Base <= 62) and (Base >= 2)) or ((Base <= -2) and (Base >= -36)) then
   begin
      if DigitCount = 0 then
         size := mpf_get_prec(NativeObject^) // this is actually bit_count, but bit_count is always bigger than digit_count
      else
         size := DigitCount;

      try
         SetLength(Result, size);
      except
         raise gmpException.Create(gmpeFatalError);
      end;
      mpf_get_str(PAnsiChar(Result), Exp, Base, size, NativeObject^);
      size := StrLen(PAnsiChar(Result));
      SetLength(Result, size);
   end
   else
      raise gmpException.Create(gmpeBaseError);
end;

// Increment methods

procedure gmpFloat.Inc;
begin
   mpf_add_ui(NativeObject^, NativeObject^, 1);
end;

procedure gmpFloat.Inc(const Value: Integer);
begin
   if Value > 0 then
      mpf_add_ui(NativeObject^, NativeObject^, Cardinal(Value))
   else if Value < 0 then
      mpf_sub_ui(NativeObject^, NativeObject^, Cardinal(Abs(Value)));
end;

procedure gmpFloat.Inc(const Value: Cardinal);
begin
   mpf_add_ui(NativeObject^, NativeObject^, Value);
end;

procedure gmpFloat.Inc(const Value: Int64);
begin
   LocalFloat.Assign(Value);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Inc(const Value: UInt64);
begin
   LocalFloat.Assign(Value);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Inc(const Value: Single);
begin
   LocalFloat.Assign(Value);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Inc(const Value: Double);
begin
   LocalFloat.Assign(Value);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Inc(const Value: AnsiString);
begin
   LocalFloat.Assign(Value);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Inc(const Value: AnsiString; Base: Integer);
begin
   LocalFloat.Assign(Value, Base);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Inc(const Value: gmpInteger);
begin
   LocalFloat.Assign(Value);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Inc(const Value: gmpRational);
begin
   LocalFloat.Assign(Value);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Inc(const Value: gmpFloat);
begin
   mpf_add(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpFloat.Inc(const Value: pgmpRational);
begin
   LocalFloat.Assign(Value);
   mpf_add(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

// Decrement methods

procedure gmpFloat.Dec;
begin
   mpf_sub_ui(NativeObject^, NativeObject^, 1);
end;

procedure gmpFloat.Dec(const Value: Integer);
begin
   if Value > 0 then
      mpf_sub_ui(NativeObject^, NativeObject^, Cardinal(Value))
   else if Value < 0 then
      mpf_add_ui(NativeObject^, NativeObject^, Cardinal(Abs(Value)));
end;

procedure gmpFloat.Dec(const Value: Cardinal);
begin
   mpf_sub_ui(NativeObject^, NativeObject^, Value);
end;

procedure gmpFloat.Dec(const Value: Int64);
begin
   LocalFloat.Assign(Value);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Dec(const Value: UInt64);
begin
   LocalFloat.Assign(Value);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Dec(const Value: Single);
begin
   LocalFloat.Assign(Value);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Dec(const Value: Double);
begin
   LocalFloat.Assign(Value);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Dec(const Value: AnsiString);
begin
   LocalFloat.Assign(Value);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Dec(const Value: AnsiString; Base: Integer);
begin
   LocalFloat.Assign(Value, Base);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Dec(const Value: gmpInteger);
begin
   LocalFloat.Assign(Value);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Dec(const Value: gmpRational);
begin
   LocalFloat.Assign(Value);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.Dec(const Value: gmpFloat);
begin
   mpf_sub(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpFloat.Dec(const Value: pgmpRational);
begin
   LocalFloat.Assign(Value);
   mpf_sub(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

// Multiply and Divide methods

procedure gmpFloat.MultiplyBy(const Value: Integer);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: Cardinal);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: Int64);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: UInt64);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: Single);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: Double);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: gmpInteger);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: gmpRational);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: pgmpRational);
begin
   LocalFloat.Assign(Value);
   mpf_mul(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.MultiplyBy(const Value: gmpFloat);
begin
   mpf_mul(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpFloat.DivideBy(const Value: Integer);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: Cardinal);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: Int64);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: UInt64);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: Single);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0.0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: Double);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if Value = 0.0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: gmpInteger);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_ui(Value.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Value);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: gmpRational);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Value.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero, Value);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: pgmpRational);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Value^.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero, Value^);
   {$ENDIF}

   LocalFloat.Assign(Value);
   mpf_div(NativeObject^, NativeObject^, LocalFloat.NativeObject^);
end;

procedure gmpFloat.DivideBy(const Value: gmpFloat);
begin
   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Value.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Value);
   {$ENDIF}

   mpf_div(NativeObject^, NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

procedure gmpFloat.Mul2Exp(const N: mp_bitcnt_t);
begin
   mpf_mul_2exp(NativeObject^, NativeObject^, N);
end;

procedure gmpFloat.Div2Exp(const N: mp_bitcnt_t);
begin
   mpf_div_2exp(NativeObject^, NativeObject^, N);
end;

// Explicit casting methods.

class operator gmpFloat.Explicit(const Value: Integer): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: Cardinal): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: Int64): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: UInt64): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: Single): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: Double): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: AnsiString): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: gmpInteger): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: gmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

class operator gmpFloat.Explicit(const Value: pgmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

// Sign operator functions

class operator gmpFloat.Negative(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_neg(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

class operator gmpFloat.Positive(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
end;

// Trunc operator function

class operator gmpFloat.Trunc(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_trunc(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Add operator functions

class operator gmpFloat.Add(const Left, Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_add(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: Integer): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   if Right >= 0 then
      mpf_add_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(Right))
   else
      mpf_sub_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(Abs(Right)));

   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: Cardinal): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_add_ui(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: Int64): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_add(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: UInt64): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_add(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: Single): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_add(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: Double): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_add(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: AnsiString): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_add(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: gmpInteger): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_add(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: gmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_add(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpFloat; const Right: pgmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_add(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: Integer; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   if Left >= 0 then
      mpf_add_ui(Result.NativeObject^, Right.NativeObject^, Cardinal(Left))
   else
      mpf_sub_ui(Result.NativeObject^, Right.NativeObject^, Cardinal(Abs(Left)));

   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: Cardinal; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_add_ui(Result.NativeObject^, Right.NativeObject^, Left);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: Int64; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_add(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: UInt64; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_add(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: Single; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_add(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: Double; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_add(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: AnsiString; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_add(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpInteger; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_add(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: gmpRational; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_add(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Add(const Left: pgmpRational; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_add(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

// Subtract operator functions

class operator gmpFloat.Subtract(const Left, Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_sub(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: Integer): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   if Right >= 0 then
      mpf_sub_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(Right))
   else
      mpf_add_ui(Result.NativeObject^, Left.NativeObject^, Cardinal(Abs(Right)));

   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: Cardinal): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_sub_ui(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: Int64): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_sub(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: UInt64): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_sub(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: Single): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_sub(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: Double): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_sub(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: AnsiString): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_sub(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: gmpInteger): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_sub(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: gmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_sub(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpFloat; const Right: pgmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_sub(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: Integer; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: Cardinal; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_ui_sub(Result.NativeObject^, Left, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: Int64; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: UInt64; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: Single; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: Double; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: AnsiString; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpInteger; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: gmpRational; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Subtract(const Left: pgmpRational; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_sub(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

// Multiply operator functions

class operator gmpFloat.Multiply(const Left, Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_mul(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: Integer; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: Cardinal; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_mul_ui(Result.NativeObject^, Right.NativeObject^, Left);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: Int64; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: UInt64; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: Single; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: Double; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: AnsiString; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpInteger; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpRational; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: pgmpRational; const Right: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_mul(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: Integer): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: Cardinal): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_mul_ui(Result.NativeObject^, Left.NativeObject^, Right);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: Int64): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: UInt64): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: Single): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: Double): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: AnsiString): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: gmpInteger): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: gmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Multiply(const Left: gmpFloat; const Right: pgmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_mul(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

// Division operator functions

class operator gmpFloat.Divide(const Left, Right: gmpFloat): gmpFloat;
begin
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   mpf_div(Result.NativeObject^, Left.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: Integer; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: Cardinal; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   mpf_ui_div(Result.NativeObject^, Left, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: Int64; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: UInt64; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: Single; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: Double; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: AnsiString; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpInteger; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Left);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpRational; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Left);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: pgmpRational; const Right: gmpFloat): gmpFloat;
begin
   Right.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Left^);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Left);
   mpf_div(Result.NativeObject^, LocalFloat.NativeObject^, Right.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: Integer): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: Cardinal): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   mpf_div_ui(Result.NativeObject^, Left.NativeObject^, Right);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: Int64): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: UInt64): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: Single): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0.0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: Double): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if Right = 0.0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: AnsiString): gmpFloat;
begin
   Left.ReleaseTemporary;
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpf_cmp_ui(LocalFloat.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero);
   {$ENDIF}

   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: gmpInteger): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpz_cmp_ui(Right.NativeObject^, 0) = 0 then
      raise gmpException.Create(gmpeDivideByZero, Right);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: gmpRational): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero, Right);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Divide(const Left: gmpFloat; const Right: pgmpRational): gmpFloat;
begin
   Left.ReleaseTemporary;

   {$IFDEF RAISE_DIV_BY_ZERO}
   if mpq_equal(Right^.NativeObject^, LocalZeroRational.NativeObject^) <> 0 then
      raise gmpException.Create(gmpeDivideByZero, Right^);
   {$ENDIF}

   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Right);
   mpf_div(Result.NativeObject^, Left.NativeObject^, LocalFloat.NativeObject^);
end;

// Equal operator functions

class operator gmpFloat.Equal(const Left, Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp(Left.NativeObject^, Right.NativeObject^) = 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: Integer): Boolean;
begin
   Result := mpf_cmp_si(Left.NativeObject^, Right) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: Cardinal): Boolean;
begin
   Result := mpf_cmp_ui(Left.NativeObject^, Right) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: Int64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: UInt64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: Single): Boolean;
var
   tmp: Double;
begin
   tmp := Right;
   Result := mpf_cmp_d(Left.NativeObject^, tmp) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: Double): Boolean;
begin
   Result := mpf_cmp_d(Left.NativeObject^, Right) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: AnsiString): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: gmpInteger): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: gmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpFloat; const Right: pgmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) = 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: Integer; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_si(Right.NativeObject^, Left) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: Cardinal; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_ui(Right.NativeObject^, Left) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: Int64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: UInt64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: Single; const Right: gmpFloat): Boolean;
var
   tmp: Double;
begin
   tmp := Left;
   Result := mpf_cmp_d(Right.NativeObject^, tmp) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: Double; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_d(Right.NativeObject^, Left) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: AnsiString; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpInteger; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: gmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.Equal(const Left: pgmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) = 0;
   Right.ReleaseTemporary;
end;

// Not-Equal operator functions

class operator gmpFloat.NotEqual(const Left, Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp(Left.NativeObject^, Right.NativeObject^) <> 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: Integer): Boolean;
begin
   Result := mpf_cmp_si(Left.NativeObject^, Right) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: Cardinal): Boolean;
begin
   Result := mpf_cmp_ui(Left.NativeObject^, Right) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: Int64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: UInt64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: Single): Boolean;
var
   tmp: Double;
begin
   tmp := Right;
   Result := mpf_cmp_d(Left.NativeObject^, tmp) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: Double): Boolean;
begin
   Result := mpf_cmp_d(Left.NativeObject^, Right) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: AnsiString): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: gmpInteger): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: gmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpFloat; const Right: pgmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <> 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: Integer; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_si(Right.NativeObject^, Left) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: Cardinal; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_ui(Right.NativeObject^, Left) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: Int64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: UInt64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: Single; const Right: gmpFloat): Boolean;
var
   tmp: Double;
begin
   tmp := Left;
   Result := mpf_cmp_d(Right.NativeObject^, tmp) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: Double; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_d(Right.NativeObject^, Left) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: AnsiString; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpInteger; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: gmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.NotEqual(const Left: pgmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <> 0;
   Right.ReleaseTemporary;
end;

// Greater-Than operator functions

class operator gmpFloat.GreaterThan(const Left, Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp(Left.NativeObject^, Right.NativeObject^) > 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: Integer): Boolean;
begin
   Result := mpf_cmp_si(Left.NativeObject^, Right) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: Cardinal): Boolean;
begin
   Result := mpf_cmp_ui(Left.NativeObject^, Right) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: Int64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: UInt64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: Single): Boolean;
var
   tmp: Double;
begin
   tmp := Right;
   Result := mpf_cmp_d(Left.NativeObject^, tmp) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: Double): Boolean;
begin
   Result := mpf_cmp_d(Left.NativeObject^, Right) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: AnsiString): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: gmpInteger): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: gmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpFloat; const Right: pgmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) > 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: Integer; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_si(Right.NativeObject^, Left) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: Cardinal; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_ui(Right.NativeObject^, Left) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: Int64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: UInt64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: Single; const Right: gmpFloat): Boolean;
var
   tmp: Double;
begin
   tmp := Left;
   Result := mpf_cmp_d(Right.NativeObject^, tmp) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: Double; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_d(Right.NativeObject^, Left) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: AnsiString; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpInteger; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: gmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThan(const Left: pgmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) > 0;
   Right.ReleaseTemporary;
end;

// GreaterThanOrEqual operator functions

class operator gmpFloat.GreaterThanOrEqual(const Left, Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp(Left.NativeObject^, Right.NativeObject^) >= 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: Integer): Boolean;
begin
   Result := mpf_cmp_si(Left.NativeObject^, Right) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: Cardinal): Boolean;
begin
   Result := mpf_cmp_ui(Left.NativeObject^, Right) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: Int64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: UInt64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: Single): Boolean;
var
   tmp: Double;
begin
   tmp := Right;
   Result := mpf_cmp_d(Left.NativeObject^, tmp) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: Double): Boolean;
begin
   Result := mpf_cmp_d(Left.NativeObject^, Right) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: AnsiString): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: gmpInteger): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: gmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpFloat; const Right: pgmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) >= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: Integer; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_si(Right.NativeObject^, Left) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: Cardinal; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_ui(Right.NativeObject^, Left) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: Int64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: UInt64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: Single; const Right: gmpFloat): Boolean;
var
   tmp: Double;
begin
   tmp := Left;
   Result := mpf_cmp_d(Right.NativeObject^, tmp) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: Double; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_d(Right.NativeObject^, Left) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: AnsiString; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpInteger; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: gmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.GreaterThanOrEqual(const Left: pgmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) >= 0;
   Right.ReleaseTemporary;
end;

// Less-Than operator functions

class operator gmpFloat.LessThan(const Left, Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp(Left.NativeObject^, Right.NativeObject^) < 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: Integer): Boolean;
begin
   Result := mpf_cmp_si(Left.NativeObject^, Right) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: Cardinal): Boolean;
begin
   Result := mpf_cmp_ui(Left.NativeObject^, Right) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: Int64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: UInt64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: Single): Boolean;
var
   tmp: Double;
begin
   tmp := Right;
   Result := mpf_cmp_d(Left.NativeObject^, tmp) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: Double): Boolean;
begin
   Result := mpf_cmp_d(Left.NativeObject^, Right) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: AnsiString): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: gmpInteger): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: gmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpFloat; const Right: pgmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) < 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: Integer; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_si(Right.NativeObject^, Left) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: Cardinal; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_ui(Right.NativeObject^, Left) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: Int64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: UInt64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: Single; const Right: gmpFloat): Boolean;
var
   tmp: Double;
begin
   tmp := Left;
   Result := mpf_cmp_d(Right.NativeObject^, tmp) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: Double; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_d(Right.NativeObject^, Left) > 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: AnsiString; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpInteger; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: gmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThan(const Left: pgmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) < 0;
   Right.ReleaseTemporary;
end;

// LessThanOrEqual operator functions

class operator gmpFloat.LessThanOrEqual(const Left, Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp(Left.NativeObject^, Right.NativeObject^) <= 0;
   Left.ReleaseTemporary;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: Integer): Boolean;
begin
   Result := mpf_cmp_si(Left.NativeObject^, Right) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: Cardinal): Boolean;
begin
   Result := mpf_cmp_ui(Left.NativeObject^, Right) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: Int64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: UInt64): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: Single): Boolean;
var
   tmp: Double;
begin
   tmp := Right;
   Result := mpf_cmp_d(Left.NativeObject^, tmp) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: Double): Boolean;
begin
   Result := mpf_cmp_d(Left.NativeObject^, Right) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: AnsiString): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: gmpInteger): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: gmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpFloat; const Right: pgmpRational): Boolean;
begin
   LocalFloat.Assign(Right);
   Result := mpf_cmp(Left.NativeObject^, LocalFloat.NativeObject^) <= 0;
   Left.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: Integer; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_si(Right.NativeObject^, Left) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: Cardinal; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_ui(Right.NativeObject^, Left) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: Int64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: UInt64; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: Single; const Right: gmpFloat): Boolean;
var
   tmp: Double;
begin
   tmp := Left;
   Result := mpf_cmp_d(Right.NativeObject^, tmp) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: Double; const Right: gmpFloat): Boolean;
begin
   Result := mpf_cmp_d(Right.NativeObject^, Left) >= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: AnsiString; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpInteger; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: gmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

class operator gmpFloat.LessThanOrEqual(const Left: pgmpRational; const Right: gmpFloat): Boolean;
begin
   LocalFloat.Assign(Left);
   Result := mpf_cmp(LocalFloat.NativeObject^, Right.NativeObject^) <= 0;
   Right.ReleaseTemporary;
end;

{ gmpException }

constructor gmpException.Create(ErrorCode: TGMPExceptionCode);
var
   ErrorString: string;
begin
   FErrorCode := ErrorCode;
   case FErrorCode of
     gmpeFatalError: ErrorString := 'Fatal Error';
     gmpeFloatPrecisionTooLow: ErrorString := 'Float precision too low';
     gmpeDivideByZero: ErrorString := 'Divided by zero';
     gmpeConversionError: ErrorString := 'Conversion Error';
     gmpeBaseError: ErrorString := 'Base Error';
     gmpeParameterError: ErrorString := 'Parameter Error';
   end;
   Create(ErrorString);
end;

constructor gmpException.Create(ErrorCode: TGMPExceptionCode; const Obj: gmpInteger);
begin
   Create(ErrorCode);
   Obj.ReleaseTemporary;
end;

constructor gmpException.Create(ErrorCode: TGMPExceptionCode; const Obj: gmpRational);
begin
   Create(ErrorCode);
   Obj.ReleaseTemporary;
end;

constructor gmpException.Create(ErrorCode: TGMPExceptionCode; const Obj: gmpFloat);
begin
   Create(ErrorCode);
   Obj.ReleaseTemporary;
end;

/////////////////// Utility Functions ///////////////////////////

procedure gmpCreate(const Args: array of Pointer; const typeDef: AnsiString);
var
   i: Integer;
   p: Pointer;
begin
   if Length(typeDef) < High(Args) - Low(Args) + 1 then
      raise gmpException.Create(gmpeParameterError);

   for i := Low(Args) to High(Args) do
   begin
      p := Args[i];
      if not Assigned(p) then
         raise gmpException.Create(gmpeParameterError);

      case typeDef[i] of
         'I', 'i': pgmpInteger(p).Create(0);
         'R', 'r': pgmpRational(p).Create(0);
         'F', 'f': pgmpFloat(p).Create(0);
      else
         raise gmpException.Create(gmpeParameterError);
      end;
   end;
end;

procedure gmpCreateI(const Args: array of Pointer);
var
   i: Integer;
   p: Pointer;
begin
   for i := Low(Args) to High(Args) do
   begin
      p := Args[i];
      if not Assigned(p) then
         raise gmpException.Create(gmpeParameterError);

      pgmpInteger(p).Create(0);
   end;
end;

procedure gmpCreateR(const Args: array of Pointer);
var
   i: Integer;
   p: Pointer;
begin
   for i := Low(Args) to High(Args) do
   begin
      p := Args[i];
      if not Assigned(p) then
         raise gmpException.Create(gmpeParameterError);

      pgmpRational(p).Create(0);
   end;
end;

procedure gmpCreateF(const Args: array of Pointer);
var
   i: Integer;
   p: Pointer;
begin
   for i := Low(Args) to High(Args) do
   begin
      p := Args[i];
      if not Assigned(p) then
         raise gmpException.Create(gmpeParameterError);

      pgmpFloat(p).Create(0);
   end;
end;

procedure gmpFree(const Args: array of Pointer);
var
   i: Integer;
begin
   for i := Low(Args) to High(Args) do
      gmpFree(Args[i]);
end;

procedure gmpFree(p: Pointer);
begin
   if not Assigned(p) then
      raise gmpException.Create(gmpeParameterError);

   case gmpObjectType(p) of
      gmpotInteger: pgmpInteger(p)^.Free;
      gmpotRational: pgmpRational(p)^.Free;
      gmpotFloat: pgmpFloat(p)^.Free;
   else
      raise gmpException.Create(gmpeParameterError);
   end;
end;

function gmpObjectType(p: Pointer): TGMPObjectType;
begin
   Result := TGMPObjectType(p^);
end;

// Constants

var
   CurrentConstantFloatPrecision: mp_bitcnt_t = 0;
   ConstantsCreated: Boolean = False;

procedure FinalizeConstants;
begin
   if ConstantsCreated then
   begin
      gmpPi.Free;
      gmpPiDiv2.Free;
      gmpPiMul2.Free;
      gmp1DivPi.Free;
      gmpE.Free;
      gmpSqrt2.Free;
      gmp1DivLn2.Free;
      gmp1DivLn10.Free;
   end;
end;

procedure gmpSetConstantPrecision(Value: mp_bitcnt_t);
begin
   if not ConstantsCreated then
   begin
      gmpPi.Create(0);
      gmpPiDiv2.Create(0);
      gmpPiMul2.Create(0);
      gmp1DivPi.Create(0);
      gmpE.Create(0);
      gmpSqrt2.Create(0);
      gmp1DivLn2.Create(0);
      gmp1DivLn10.Create(0);
      ConstantsCreated := True;
   end;

   if CurrentConstantFloatPrecision <> Value then
   begin
      // Recalculate constants
      mpf_set_prec(gmpPi.NativeObject^, Value);
      mpf_pi(gmpPi.NativeObject^);

      mpf_set_prec(gmpPiDiv2.NativeObject^, Value);
      gmpPiDiv2.Assign(gmpPi);
      gmpPiDiv2.DivideBy(2);

      mpf_set_prec(gmpPiMul2.NativeObject^, Value);
      gmpPiMul2.Assign(gmpPi);
      gmpPiMul2.MultiplyBy(2);

      mpf_set_prec(gmp1DivPi.NativeObject^, Value);
      gmp1DivPi.Assign(gmpPi);
      gmp1DivPi.SetReciprocal;

      mpf_set_prec(gmpE.NativeObject^, Value);
      LocalFloat.Assign(1);
      mpf_exp(gmpE.NativeObject^, LocalFloat.NativeObject^);

      mpf_set_prec(gmpSqrt2.NativeObject^, Value);
      gmpSqrt2.Assign(2);
      gmpSqrt2.SetSqrt;

      mpf_set_prec(gmp1DivLn2.NativeObject^, Value);
      LocalFloat.Assign(2);
      mpf_ln(gmp1DivLn2.NativeObject^, LocalFloat.NativeObject^);
      gmp1DivLn2.SetReciprocal;

      mpf_set_prec(gmp1DivLn10.NativeObject^, Value);
      LocalFloat.Assign(10);
      mpf_ln(gmp1DivLn10.NativeObject^, LocalFloat.NativeObject^);
      gmp1DivLn10.SetReciprocal;
   end;
end;

// Quick swap the value of A and B

procedure gmpSwap(const A, B: gmpInteger);
begin
   mpz_swap(A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

procedure gmpSwap(const A, B: gmpRational);
begin
   mpq_swap(A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

procedure gmpSwap(const A: pgmpRational; const B: gmpRational);
begin
   gmpSwap(A^, B);
end;

procedure gmpSwap(const A: gmpRational; const B: pgmpRational);
begin
   gmpSwap(A, B^);
end;

procedure gmpSwap(const A, B: gmpFloat);
begin
   mpf_swap(A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

// Min/Max of 2 values
function gmpMin(const A, B: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   if A < B then
   begin
      Result.Assign(A);
      B.ReleaseTemporary;
   end
   else
   begin
      Result.Assign(B);
      A.ReleaseTemporary;
   end;
end;

function gmpMin(const A, B: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   if A < B then
   begin
      Result.Assign(A);
      B.ReleaseTemporary;
   end
   else
   begin
      Result.Assign(B);
      A.ReleaseTemporary;
   end;
end;

function gmpMin(const A: pgmpRational; const B: gmpRational): gmpRational;
begin
   Result := gmpMin(A^, B);
end;

function gmpMin(const A: gmpRational; const B: pgmpRational): gmpRational;
begin
   Result := gmpMin(A, B^);
end;

function gmpMin(const A, B: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   if A < B then
   begin
      Result.Assign(A);
      B.ReleaseTemporary;
   end
   else
   begin
      Result.Assign(B);
      A.ReleaseTemporary;
   end;
end;

function gmpMax(const A, B: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   if A > B then
   begin
      Result.Assign(A);
      B.ReleaseTemporary;
   end
   else
   begin
      Result.Assign(B);
      A.ReleaseTemporary;
   end;
end;

function gmpMax(const A, B: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   if A > B then
   begin
      Result.Assign(A);
      B.ReleaseTemporary;
   end
   else
   begin
      Result.Assign(B);
      A.ReleaseTemporary;
   end;
end;

function gmpMax(const A: pgmpRational; const B: gmpRational): gmpRational;
begin
   Result := gmpMax(A^, B);
end;

function gmpMax(const A: gmpRational; const B: pgmpRational): gmpRational;
begin
   Result := gmpMax(A, B^);
end;

function gmpMax(const A, B: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   if A > B then
   begin
      Result.Assign(A);
      B.ReleaseTemporary;
   end
   else
   begin
      Result.Assign(B);
      A.ReleaseTemporary;
   end;
end;

// Arithmetic functions

procedure DirectAssign(const Dest, Src: gmpFloat); inline; // Dest <= Src
begin
   mpf_set(Dest.NativeObject^, Src.NativeObject^);
end;

function gmpAbs(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_abs(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

function gmpAbs(const Value: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   mpq_abs(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

function gmpAbs(const Value: pgmpRational): gmpRational;
begin
   Result := gmpAbs(Value^);
end;

function gmpAbs(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_abs(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Calculate e^Value
function gmpExp(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpExp(LocalFloat3);
end;

function gmpExp(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpExp(LocalFloat3);
end;

function gmpExp(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpExp(Value^);
end;

function gmpExp(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_exp(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Calculate logarithm base of e
function gmpLn(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLn(LocalFloat3);
end;

function gmpLn(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLn(LocalFloat3);
end;

function gmpLn(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpLn(Value^)
end;

function gmpLn(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_ln(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Calculate logarithm base of 2
function gmpLog2(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLog2(LocalFloat3);
end;

function gmpLog2(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLog2(LocalFloat3);
end;

function gmpLog2(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpLog2(Value^);
end;

function gmpLog2(const Value: gmpFloat): gmpFloat;
begin
   // Log2(X) = Ln(X) / Ln(2) = Ln(X) * (1 / Ln(2)) // Extract "1 / Ln(2)" as a constant.
   Result := GetTemporaryFloat()^;
   mpf_ln(Result.NativeObject^, Value.NativeObject^);
   Result.MultiplyBy(gmp1DivLn2);
   Value.ReleaseTemporary;
end;

// Calculate logarithm base of 10
function gmpLog10(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLog10(LocalFloat3);
end;

function gmpLog10(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLog10(LocalFloat3);
end;

function gmpLog10(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpLog10(Value^);
end;

function gmpLog10(const Value: gmpFloat): gmpFloat;
begin
   // Log10(X) = Ln(X) / Ln(10) = Ln(X) * (1 / Ln(10)) // Extract "1 / Ln(10)" as a constant.
   Result := GetTemporaryFloat()^;
   mpf_ln(Result.NativeObject^, Value.NativeObject^);
   Result.MultiplyBy(gmp1DivLn10);
   Value.ReleaseTemporary;
end;

// Calculate logarithm base of N
function gmpLog(const Value: gmpInteger; const Base: Double): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLog(LocalFloat3, Base);
end;

function gmpLog(const Value: gmpRational; const Base: Double): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLog(LocalFloat3, Base);
end;

function gmpLog(const Value: pgmpRational; const Base: Double): gmpFloat;
begin
   Result := gmpLog(Value^, Base);
end;

function gmpLog(const Value: gmpFloat; const Base: Double): gmpFloat;
begin
   // LogN(X) := Ln(X) / Ln(N)
   Result := GetTemporaryFloat()^;
   mpf_ln(Result.NativeObject^, Value.NativeObject^); // Ln(X)
   LocalFloat.Assign(Base);
   LocalFloat.SetLn; // Ln(N)
   Result.DivideBy(LocalFloat);
   Value.ReleaseTemporary;
end;

function gmpLog(const Value: gmpInteger; const Base: gmpFloat): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLog(LocalFloat3, Base);
end;

function gmpLog(const Value: gmpRational; const Base: gmpFloat): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpLog(LocalFloat3, Base);
end;

function gmpLog(const Value: pgmpRational; const Base: gmpFloat): gmpFloat;
begin
   Result := gmpLog(Value^, Base);
end;

function gmpLog(const Value: gmpFloat; const Base: gmpFloat): gmpFloat;
begin
   // LogN(X) := Ln(X) / Ln(N)
   Result := GetTemporaryFloat()^;
   mpf_ln(Result.NativeObject^, Value.NativeObject^); // Ln(X)
   LocalFloat.Assign(Base);
   LocalFloat.SetLn; // Ln(N)
   Result.DivideBy(LocalFloat);
   Value.ReleaseTemporary;
end;

// Calculate square
function gmpSqr(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_pow_ui(Result.NativeObject^, Value.NativeObject^, 2);
   Value.ReleaseTemporary;
end;

function gmpSqr(const Value: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
   Result.SetSquare;
end;

function gmpSqr(const Value: pgmpRational): gmpRational;
begin
   Result := gmpSqr(Value^);
end;

function gmpSqr(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_pow_ui(Result.NativeObject^, Value.NativeObject^, 2);
   Value.ReleaseTemporary;
end;

// Calculate square root
function gmpSqrt(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpSqrt(LocalFloat3);
end;

function gmpSqrt(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpSqrt(LocalFloat3);
end;

function gmpSqrt(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpSqrt(Value^);
end;

function gmpSqrt(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_sqrt(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Get truncated integer part of Sqrt(A)
function gmpIntSqrt(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_root(Result.NativeObject^, Value.NativeObject^, 2);
   Value.ReleaseTemporary;
end;

// Get "A - gmpIntSqrt(A)^2"
function gmpIntSqrtRem(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_rootrem(LocalInteger.NativeObject^, Result.NativeObject^, Value.NativeObject^, 2);
   Value.ReleaseTemporary;
end;

// Calculate cubic(^3)
function gmpCubic(const Value: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_pow_ui(Result.NativeObject^, Value.NativeObject^, 3);
   Value.ReleaseTemporary;
end;

function gmpCubic(const Value: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
   Result.SetCubic;
end;

function gmpCubic(const Value: pgmpRational): gmpRational;
begin
   Result := gmpCubic(Value^);
end;

function gmpCubic(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_pow_ui(Result.NativeObject^, Value.NativeObject^, 3);
   Value.ReleaseTemporary;
end;

// Calculate Value^Exp
function gmpIntPower(const Value: gmpInteger; Exp: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_pow_ui(Result.NativeObject^, Value.NativeObject^, Exp);
   Value.ReleaseTemporary;
end;

function gmpIntPower(const Value: gmpRational; Exp: Cardinal): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(Value);
   Result.SetIntPower(Exp);
end;

function gmpIntPower(const Value: pgmpRational; Exp: Cardinal): gmpRational;
begin
   Result := gmpIntPower(Value^, Exp);
end;

function gmpIntPower(const Value: gmpFloat; Exp: Cardinal): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_pow_ui(Result.NativeObject^, Value.NativeObject^, Exp);
   Value.ReleaseTemporary;
end;

// Calculate Value^Exp
function gmpPower(const Value: gmpInteger; Exp: Double): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpPower(LocalFloat3, Exp);
end;

function gmpPower(const Value: gmpRational; Exp: Double): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpPower(LocalFloat3, Exp);
end;

function gmpPower(const Value: pgmpRational; Exp: Double): gmpFloat;
begin
   Result := gmpPower(Value^, Exp);
end;

function gmpPower(const Value: gmpFloat; Exp: Double): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Exp);
   mpf_pow(Result.NativeObject^, Value.NativeObject^, LocalFloat.NativeObject^);
   Value.ReleaseTemporary;
end;

function gmpPower(const Value: gmpInteger; Exp: gmpFloat): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpPower(LocalFloat3, Exp);
end;

function gmpPower(const Value: gmpRational; Exp: gmpFloat): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpPower(LocalFloat3, Exp);
end;

function gmpPower(const Value: pgmpRational; Exp: gmpFloat): gmpFloat;
begin
   Result := gmpPower(Value^, Exp);
end;

function gmpPower(const Value: gmpFloat; Exp: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_pow(Result.NativeObject^, Value.NativeObject^, Exp.NativeObject^);
   Value.ReleaseTemporary;
   Exp.ReleaseTemporary;
end;

// Calculate Nth Root
function gmpRoot(const Value: gmpInteger; N: Cardinal): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpRoot(LocalFloat3, N);
end;

function gmpRoot(const Value: gmpRational; N: Cardinal): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpRoot(LocalFloat3, N);
end;

function gmpRoot(const Value: pgmpRational; N: Cardinal): gmpFloat;
begin
   Result := gmpRoot(Value^, N);
end;

function gmpRoot(const Value: gmpFloat; N: Cardinal): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Value);
   Result.SetNthRoot(N);
end;

// Get truncated integer part of Nth root of A.
function gmpIntRoot(const Value: gmpInteger; N: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_root(Result.NativeObject^, Value.NativeObject^, N);
   Value.ReleaseTemporary;
end;

// Get "A - gmpIntRoot(A)^N"
function gmpIntRootRem(const Value: gmpInteger; N: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_rootrem(LocalInteger.NativeObject^, Result.NativeObject^, Value.NativeObject^, N);
   Value.ReleaseTemporary;
end;

// Trigonometric functions
procedure gmpSinCos(const Value: gmpInteger; var sinValue, cosValue: gmpFloat);
begin
   sinValue.Assign(gmpSin(Value));
   cosValue.Assign(gmpCos(Value));
end;

procedure gmpSinCos(const Value: gmpRational; var sinValue, cosValue: gmpFloat);
begin
   sinValue.Assign(gmpSin(Value));
   cosValue.Assign(gmpCos(Value));
end;

procedure gmpSinCos(const Value: pgmpRational; var sinValue, cosValue: gmpFloat);
begin
   gmpSinCos(Value^, sinValue, cosValue);
end;

procedure gmpSinCos(const Value: gmpFloat; var sinValue, cosValue: gmpFloat);
begin
   sinValue.Assign(gmpSin(Value));
   cosValue.Assign(gmpCos(Value));
end;

function gmpSin(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat.Assign(Value);
   Result := GetTemporaryFloat()^;
   mpf_sin(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpSin(const Value: gmpRational): gmpFloat;
begin
   LocalFloat.Assign(Value);
   Result := GetTemporaryFloat()^;
   mpf_sin(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpSin(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpSin(Value^);
end;

function gmpSin(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_sin(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Cos(x) = Sin(Pi/2 - x)
function gmpCos(const Value: gmpInteger): gmpFloat;
begin
   Result := gmpSin(gmpPiDiv2 - Value);
end;

function gmpCos(const Value: gmpRational): gmpFloat;
begin
   Result := gmpSin(gmpPiDiv2 - Value);
end;

function gmpCos(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpCos(Value^);
end;

function gmpCos(const Value: gmpFloat): gmpFloat;
begin
   Result := gmpSin(gmpPiDiv2 - Value);
end;

function gmpTan(const Value: gmpInteger): gmpFloat;
begin
   Result := gmpSin(Value) / gmpCos(Value);
end;

function gmpTan(const Value: gmpRational): gmpFloat;
begin
   Result := gmpSin(Value) / gmpCos(Value);
end;

function gmpTan(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpTan(Value^);
end;

function gmpTan(const Value: gmpFloat): gmpFloat;
begin
   Result := gmpSin(Value) / gmpCos(Value);
end;

function gmpCot(const Value: gmpInteger): gmpFloat;
begin
   Result := gmpCos(Value) / gmpSin(Value);
end;

function gmpCot(const Value: gmpRational): gmpFloat;
begin
   Result := gmpCos(Value) / gmpSin(Value);
end;

function gmpCot(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpCot(Value^);
end;

function gmpCot(const Value: gmpFloat): gmpFloat;
begin
   Result := gmpCos(Value) / gmpSin(Value);
end;

// Secant
function gmpSec(const Value: gmpInteger): gmpFloat;
begin
   Result := 1 / gmpCos(Value);
end;

function gmpSec(const Value: gmpRational): gmpFloat;
begin
   Result := 1 / gmpCos(Value);
end;

function gmpSec(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpSec(Value^);
end;

function gmpSec(const Value: gmpFloat): gmpFloat;
begin
   Result := 1 / gmpCos(Value);
end;

// Cosecant
function gmpCsc(const Value: gmpInteger): gmpFloat;
begin
   Result := 1 / gmpSin(Value);
end;

function gmpCsc(const Value: gmpRational): gmpFloat;
begin
   Result := 1 / gmpSin(Value);
end;

function gmpCsc(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpCsc(Value^);
end;

function gmpCsc(const Value: gmpFloat): gmpFloat;
begin
   Result := 1 / gmpSin(Value);
end;

// IN: |Value| <= 1  OUT: [-PI/2..PI/2] radians
function gmpArcSin(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcSin(LocalFloat3);
   //Result := gmpArcSin(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcSin(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcSin(LocalFloat3);
   //Result := gmpArcSin(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcSin(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcSin(Value^);
end;

function gmpArcSin(const Value: gmpFloat): gmpFloat;
begin
   // ArcTan(X/Sqrt((1+X)*(1-X)))
   LocalFloat.Assign(Value); // X
   LocalFloat.SetSquare; // X^2
   LocalFloat.Dec; // X^2-1
   LocalFloat.SetNegative; // 1-X^2
   LocalFloat.SetSqrt; // Sqrt(1-X^2)
   DirectAssign(LocalFloat2, Value); // X
   LocalFloat2.DivideBy(LocalFloat); // X/Sqrt(1-X^2)
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, LocalFloat2.NativeObject^);
end;

// IN: |Value| <= 1  OUT: [0..PI] radians
function gmpArcCos(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcCos(LocalFloat3);
   //Result := gmpArcCos(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcCos(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcCos(LocalFloat3);
   //Result := gmpArcCos(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcCos(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcCos(Value^);
end;

function gmpArcCos(const Value: gmpFloat): gmpFloat;
begin
   // ArcTan(Sqrt((1+X)*(1-X))/X)
   LocalFloat2.Assign(Value); // X
   LocalFloat2.SetSquare; // X^2
   LocalFloat2.Dec; // X^2-1
   LocalFloat2.SetNegative; // 1-X^2
   LocalFloat2.SetSqrt; // Sqrt(1-X^2)
   LocalFloat2.DivideBy(Value); // Sqrt(1-X^2)/X
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, LocalFloat2.NativeObject^);
end;

function gmpArcTan(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat.Assign(Value);
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpArcTan(const Value: gmpRational): gmpFloat;
begin
   LocalFloat.Assign(Value);
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpArcTan(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcTan(Value^);
end;

function gmpArcTan(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

// Calculates ArcTan(Y/X)
function gmpArcTan2(const Y, X: gmpFloat): gmpFloat;
begin
   LocalFloat.Assign(Y);
   LocalFloat.DivideBy(X);
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpArcCot(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat.Assign(Value);
   LocalFloat.SetReciprocal; // 1/X
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpArcCot(const Value: gmpRational): gmpFloat;
begin
   LocalFloat.Assign(Value);
   LocalFloat.SetReciprocal; // 1/X
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpArcCot(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcCot(Value^);
end;

function gmpArcCot(const Value: gmpFloat): gmpFloat;
begin
   LocalFloat.Assign(Value);
   LocalFloat.SetReciprocal; // 1/X
   Result := GetTemporaryFloat()^;
   mpf_arctan(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpArcSec(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat.Assign(Value);
   LocalFloat.SetReciprocal; // 1/X
   Result := gmpArcCos(LocalFloat);
end;

function gmpArcSec(const Value: gmpRational): gmpFloat;
begin
   LocalFloat.Assign(Value);
   LocalFloat.SetReciprocal; // 1/X
   Result := gmpArcCos(LocalFloat);
end;

function gmpArcSec(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcSec(Value^);
end;

function gmpArcSec(const Value: gmpFloat): gmpFloat;
begin
   LocalFloat.Assign(Value);
   LocalFloat.SetReciprocal; // 1/X
   Result := gmpArcCos(LocalFloat);
end;

function gmpArcCsc(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   LocalFloat3.SetReciprocal; // 1/X
   Result := gmpArcSin(LocalFloat3);
end;

function gmpArcCsc(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   LocalFloat3.SetReciprocal; // 1/X
   Result := gmpArcSin(LocalFloat3);
end;

function gmpArcCsc(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcCsc(Value^);
end;

function gmpArcCsc(const Value: gmpFloat): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   LocalFloat3.SetReciprocal; // 1/X
   Result := gmpArcSin(LocalFloat3);
end;

// Hyperbolic functions
function gmpSinH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpSinH(LocalFloat3);
   //Result := gmpSinH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpSinH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpSinH(LocalFloat3);
   //Result := gmpSinH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpSinH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpSinH(Value^);
end;

function gmpSinH(const Value: gmpFloat): gmpFloat;
begin
   // (Exp(X) - Exp(-X)) / 2
   LocalFloat.Assign(Value);
   LocalFloat.SetExp; // Exp(X)
   LocalFloat2.Assign(LocalFloat);
   LocalFloat2.SetReciprocal; // Exp(-X)
   LocalFloat.Dec(LocalFloat2); // Exp(X)-Exp(-X)
   LocalFloat.DivideBy(2);
   Result := GetTemporaryFloat()^;
   gmpSwap(LocalFloat, Result);
end;

function gmpCosH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpCosH(LocalFloat3);
   //Result := gmpCosH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpCosH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpCosH(LocalFloat3);
   //Result := gmpCosH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpCosH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpCosH(Value^);
end;

function gmpCosH(const Value: gmpFloat): gmpFloat;
begin
   // (Exp(X) + Exp(-X)) / 2
   LocalFloat.Assign(Value);
   LocalFloat.SetExp; // Exp(X)
   LocalFloat2.Assign(LocalFloat);
   LocalFloat2.SetReciprocal; // Exp(-X)
   LocalFloat.Inc(LocalFloat2); // Exp(X)+Exp(-X)
   LocalFloat.DivideBy(2);
   Result := GetTemporaryFloat()^;
   gmpSwap(LocalFloat, Result);
end;

function gmpTanH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpTanH(LocalFloat3);
   //Result := gmpTanH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpTanH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpTanH(LocalFloat3);
   //Result := gmpTanH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpTanH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpTanH(Value^);
end;

function gmpTanH(const Value: gmpFloat): gmpFloat;
begin
   // (Exp(X) - Exp(-X)) / (Exp(X) + Exp(-X))
   Result := GetTemporaryFloat()^;
   LocalFloat.Assign(Value);
   LocalFloat.SetExp; // Exp(X)
   LocalFloat2.Assign(LocalFloat);
   LocalFloat2.SetReciprocal; // Exp(-X)
   Result.Assign(LocalFloat); // Exp(X)
   Result.Dec(LocalFloat2); // Exp(X) - Exp(-X)
   LocalFloat2.Inc(LocalFloat); // Exp(X) + Exp(-X)
   Result.DivideBy(LocalFloat2); // (Exp(X) - Exp(-X))/(Exp(X) + Exp(-X))
end;

function gmpCotH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpCotH(LocalFloat3);
   //Result := gmpCotH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpCotH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpCotH(LocalFloat3);
   //Result := gmpCotH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpCotH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpCotH(Value^);
end;

function gmpCotH(const Value: gmpFloat): gmpFloat;
begin
   Result := 1 / gmpTanH(Value);
end;

function gmpSecH(const Value: gmpInteger): gmpFloat;
begin
   Result := 1 / gmpCosH(Value);
end;

function gmpSecH(const Value: gmpRational): gmpFloat;
begin
   Result := 1 / gmpCosH(Value);
end;

function gmpSecH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpSecH(Value^);
end;

function gmpSecH(const Value: gmpFloat): gmpFloat;
begin
   Result := 1 / gmpCosH(Value);
end;

function gmpCscH(const Value: gmpInteger): gmpFloat;
begin
   Result := 1 / gmpSinH(Value);
end;

function gmpCscH(const Value: gmpRational): gmpFloat;
begin
   Result := 1 / gmpSinH(Value);
end;

function gmpCscH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpCscH(Value^);
end;

function gmpCscH(const Value: gmpFloat): gmpFloat;
begin
   Result := 1 / gmpSinH(Value);
end;

function gmpArcSinH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcSinH(LocalFloat3);
   //Result := gmpArcSinH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcSinH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcSinH(LocalFloat3);
   //Result := gmpArcSinH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcSinH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcSinH(Value^);
end;

function gmpArcSinH(const Value: gmpFloat): gmpFloat;
begin
   // Ln(X + Sqrt((X * X) + 1))
   LocalFloat.Assign(Value);
   LocalFloat.SetSquare; // X^2
   LocalFloat.Inc; // X^2+1
   LocalFloat.SetSqrt; // Sqrt((X * X) + 1)
   LocalFloat.Inc(Value); // X + Sqrt((X * X) + 1)
   Result := GetTemporaryFloat()^;
   mpf_ln(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpArcCosH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcCosH(LocalFloat3);
   //Result := gmpArcCosH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcCosH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcCosH(LocalFloat3);
   //Result := gmpArcCosH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcCosH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcCosH(Value^);
end;

function gmpArcCosH(const Value: gmpFloat): gmpFloat;
begin
   // Ln(X + Sqrt((X - 1) / (X + 1)) * (X + 1))
   LocalFloat.Assign(Value);
   LocalFloat.SetSquare; // X^2
   LocalFloat.Dec; // X^2-1
   LocalFloat.SetSqrt; // Sqrt(X^2-1)
   DirectAssign(LocalFloat2, Value);
   LocalFloat2.Inc; // X+1
   LocalFloat.MultiplyBy(LocalFloat2);
   LocalFloat.Inc(Value); // X + Sqrt(X^2-1)*(X+1)
   Result := GetTemporaryFloat()^;
   mpf_ln(Result.NativeObject^, LocalFloat.NativeObject^);
end;

function gmpArcTanH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcTanH(LocalFloat3);
   //Result := gmpArcTanH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcTanH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcTanH(LocalFloat3);
   //Result := gmpArcTanH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcTanH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcTanH(Value^);
end;

function gmpArcTanH(const Value: gmpFloat): gmpFloat;
begin
   // 0.5 * Ln((1 + X) / (1 - X))
   LocalFloat.Assign(Value);
   LocalFloat.SetSquare; // X^2
   LocalFloat.Dec; // X^2-1
   LocalFloat.SetNegative; // 1-X^2
   LocalFloat.SetLn; // Ln(1-X^2)
   LocalFloat.DivideBy(2); // 0.5 * Ln(1-X^2)
   Result := GetTemporaryFloat()^;
   gmpSwap(LocalFloat, Result);
end;

function gmpArcCotH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcCotH(LocalFloat3);
   //Result := gmpArcCotH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcCotH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcCotH(LocalFloat3);
   //Result := gmpArcCotH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcCotH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcCotH(Value^);
end;

function gmpArcCotH(const Value: gmpFloat): gmpFloat;
begin
   // 0.5 * Ln((X + 1) / (X - 1))
   LocalFloat.Assign(Value);
   LocalFloat.SetSquare; // X^2
   LocalFloat.Dec; // X^2-1
   LocalFloat.SetLn; // Ln(X^2-1)
   LocalFloat.DivideBy(2); // 0.5 * Ln(X^2-1)
   Result := GetTemporaryFloat()^;
   gmpSwap(LocalFloat, Result);
end;

function gmpArcSecH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcSecH(LocalFloat3);
   //Result := gmpArcSecH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcSecH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcSecH(LocalFloat3);
   //Result := gmpArcSecH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcSecH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcSecH(Value^);
end;

function gmpArcSecH(const Value: gmpFloat): gmpFloat;
begin
   // Ln((Sqrt(1 - X * X) + 1) / X)
   LocalFloat.Assign(Value);
   LocalFloat.SetSquare; // X^2
   LocalFloat.Dec; // X^2-1
   LocalFloat.SetNegative; // 1-X^2
   LocalFloat.SetSqrt; // Sqrt(1-X^2)
   LocalFloat.Inc; // Sqrt(1-X^2) + 1
   LocalFloat.DivideBy(Value); // (Sqrt(1-X^2) + 1)/X
   LocalFloat.SetLn; // Ln((Sqrt(1 - X * X) + 1) / X)
   Result := GetTemporaryFloat()^;
   gmpSwap(LocalFloat, Result);
end;

function gmpArcCscH(const Value: gmpInteger): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcCscH(LocalFloat3);
   //Result := gmpArcCscH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcCscH(const Value: gmpRational): gmpFloat;
begin
   LocalFloat3.Assign(Value);
   Result := gmpArcCscH(LocalFloat3);
   //Result := gmpArcCscH(gmpFloat(Value)); // Low effciency for explicit cast will use temporary objects.
end;

function gmpArcCscH(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpArcCscH(Value^);
end;

function gmpArcCscH(const Value: gmpFloat): gmpFloat;
begin
   // Ln(Sqrt(1 + (1 / (X * X)) + (1 / X)))
   LocalFloat.Assign(Value);
   LocalFloat.SetReciprocal; // 1/X

   DirectAssign(LocalFloat2, LocalFloat);
   LocalFloat2.SetSquare; // 1/X^2
   LocalFloat2.Inc; // 1 + 1/X^2
   LocalFloat2.SetSqrt; // Sqrt(1 + (1 / (X * X))

   LocalFloat.Inc(LocalFloat2); // Sqrt(1 + (1 / (X * X)) + (1 / X)
   LocalFloat.SetLn;
   Result := GetTemporaryFloat()^;
   gmpSwap(LocalFloat, Result);
end;

// Angle unit conversion routines
function gmpRadToDeg(const Radians: gmpInteger): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Radians);
   Result.MultiplyBy(180);
   Result.DivideBy(gmpPi);
end;

function gmpRadToDeg(const Radians: gmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Radians);
   Result.MultiplyBy(180);
   Result.DivideBy(gmpPi);
end;

function gmpRadToDeg(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpRadToDeg(Value^);
end;

function gmpRadToDeg(const Radians: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Radians);
   Result.MultiplyBy(180);
   Result.DivideBy(gmpPi);
end;

function gmpDegToRad(const Degrees: gmpInteger): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Degrees);
   Result.MultiplyBy(gmpPi);
   Result.DivideBy(180);
end;

function gmpDegToRad(const Degrees: gmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Degrees);
   Result.MultiplyBy(gmpPi);
   Result.DivideBy(180);
end;

function gmpDegToRad(const Value: pgmpRational): gmpFloat;
begin
   Result := gmpDegToRad(Value^);
end;

function gmpDegToRad(const Degrees: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(Degrees);
   Result.MultiplyBy(gmpPi);
   Result.DivideBy(180);
end;

// Others
function gmpMean(const A, B: gmpInteger): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(A);
   Result.Inc(B);
   Result.DivideBy(2);
end;

function gmpMean(const A, B: gmpRational): gmpRational;
begin
   Result := GetTemporaryRational()^;
   Result.Assign(A);
   Result.Inc(B);
   Result.DivideBy(2);
end;

function gmpMean(const A: pgmpRational; const B: gmpRational): gmpRational;
begin
   Result := gmpMean(A^, B);
end;

function gmpMean(const A: gmpRational; const B: pgmpRational): gmpRational;
begin
   Result := gmpMean(A, B^);
end;

function gmpMean(const A, B: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(A);
   Result.Inc(B);
   Result.DivideBy(2);
end;

function gmpGeoMean(const A, B: gmpInteger): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(A);
   Result.MultiplyBy(B);
   Result.SetSqrt;
end;

function gmpGeoMean(const A, B: gmpRational): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(A);
   Result.MultiplyBy(B);
   Result.SetSqrt;
end;

function gmpGeoMean(const A: pgmpRational; const B: gmpRational): gmpFloat;
begin
   Result := gmpGeoMean(A^, B);
end;

function gmpGeoMean(const A: gmpRational; const B: pgmpRational): gmpFloat;
begin
   Result := gmpGeoMean(A, B^);
end;

function gmpGeoMean(const A, B: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   Result.Assign(A);
   Result.MultiplyBy(B);
   Result.SetSqrt;
end;

// 2/(1/A+1/B)
function gmpHarmonicMean(const A, B: gmpInteger): gmpFloat;
begin
   LocalFloat.Assign(A);
   LocalFloat.SetReciprocal; // 1/A
   LocalFloat2.Assign(B);
   LocalFloat2.SetReciprocal; // 1/B
   LocalFloat.Inc(LocalFloat2); // 1/A + 1/B

   Result := GetTemporaryFloat()^;
   Result.Assign(2);
   Result.DivideBy(LocalFloat);
end;

function gmpHarmonicMean(const A, B: gmpRational): gmpFloat;
begin
   LocalFloat.Assign(A);
   LocalFloat.SetReciprocal; // 1/A
   LocalFloat2.Assign(B);
   LocalFloat2.SetReciprocal; // 1/B
   LocalFloat.Inc(LocalFloat2); // 1/A + 1/B

   Result := GetTemporaryFloat()^;
   Result.Assign(2);
   Result.DivideBy(LocalFloat);
end;

function gmpHarmonicMean(const A: pgmpRational; const B: gmpRational): gmpFloat;
begin
   Result := gmpHarmonicMean(A^, B);
end;

function gmpHarmonicMean(const A: gmpRational; const B: pgmpRational): gmpFloat;
begin
   Result := gmpHarmonicMean(A, B^);
end;

function gmpHarmonicMean(const A, B: gmpFloat): gmpFloat;
begin
   LocalFloat.Assign(A);
   LocalFloat.SetReciprocal; // 1/A
   LocalFloat2.Assign(B);
   LocalFloat2.SetReciprocal; // 1/B
   LocalFloat.Inc(LocalFloat2); // 1/A + 1/B

   Result := GetTemporaryFloat()^;
   Result.Assign(2);
   Result.DivideBy(LocalFloat);
end;

// Sqrt(A^2 + B^2)
function gmpHypot(const A, B: gmpInteger): gmpFloat;
begin
   LocalFloat.Assign(A);
   LocalFloat.SetSquare; // A^2
   Result := GetTemporaryFloat()^;
   Result.Assign(B);
   Result.SetSquare; // B^2
   Result.Inc(LocalFloat); // A^2+B^2
   Result.SetSqrt;
end;

function gmpHypot(const A, B: gmpRational): gmpFloat;
begin
   LocalFloat.Assign(A);
   LocalFloat.SetSquare; // A^2
   Result := GetTemporaryFloat()^;
   Result.Assign(B);
   Result.SetSquare; // B^2
   Result.Inc(LocalFloat); // A^2+B^2
   Result.SetSqrt;
end;

function gmpHypot(const A: pgmpRational; const B: gmpRational): gmpFloat;
begin
   Result := gmpHypot(A^, B);
end;

function gmpHypot(const A: gmpRational; const B: pgmpRational): gmpFloat;
begin
   Result := gmpHypot(A, B^);
end;

function gmpHypot(const A, B: gmpFloat): gmpFloat;
begin
   LocalFloat.Assign(A);
   LocalFloat.SetSquare; // A^2
   Result := GetTemporaryFloat()^;
   Result.Assign(B);
   Result.SetSquare; // B^2
   Result.Inc(LocalFloat); // A^2+B^2
   Result.SetSqrt;
end;

// For Integer
function gmpGCD(const A, B: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_gcd(Result.NativeObject^, A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

function gmpGCD(const A: gmpInteger; B: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_gcd_ui(Result.NativeObject^, A.NativeObject^, B);
   A.ReleaseTemporary;
end;

function gmpGCDEx(const A, B: gmpInteger; var s, t: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_gcdext(Result.NativeObject^, s.NativeObject^, t.NativeObject^, A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

function gmpLCM(const A, B: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_lcm(Result.NativeObject^, A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

function gmpLCM(const A: gmpInteger; B: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_lcm_ui(Result.NativeObject^, A.NativeObject^, B);
   A.ReleaseTemporary;
end;

function gmpModularInverse(const Op1, Op2: gmpInteger; var Output: gmpInteger): Boolean;
begin
   Result := mpz_invert(Output.NativeObject^, Op1.NativeObject^, Op2.NativeObject^) <> 0;
   Op1.ReleaseTemporary;
   Op2.ReleaseTemporary;
end;

function gmpJacobi(const A, B: gmpInteger): Integer;
begin
   if B.IsEven then
      raise gmpException.Create(gmpeParameterError);

   Result := mpz_jacobi(A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

function gmpLegendre(const A, B: gmpInteger): Integer;
begin
   Result := mpz_legendre(A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

function gmpKronecker(const A, B: gmpInteger): Integer;
begin
   Result := mpz_kronecker(A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

function gmpKronecker(const A: gmpInteger; B: Integer): Integer;
begin
   Result := mpz_kronecker_si(A.NativeObject^, B);
   A.ReleaseTemporary;
end;

function gmpKronecker(const A: gmpInteger; B: Cardinal): Integer;
begin
   Result := mpz_kronecker_ui(A.NativeObject^, B);
   A.ReleaseTemporary;
end;

function gmpKronecker(A: Integer; const B: gmpInteger): Integer;
begin
   Result := mpz_si_kronecker(A, B.NativeObject^);
   B.ReleaseTemporary;
end;

function gmpKronecker(A: Cardinal; const B: gmpInteger): Integer;
begin
   Result := mpz_ui_kronecker(A, B.NativeObject^);
   B.ReleaseTemporary;
end;

function gmpRemoveFactor(const Src: gmpInteger; Factor: Cardinal; var Output: gmpInteger): Cardinal;
begin
   LocalInteger.Assign(Factor);
   Result := mpz_remove(Output.NativeObject^, Src.NativeObject^, LocalInteger.NativeObject^);
   Src.ReleaseTemporary;
end;

function gmpRemoveFactor(const Src, Factor: gmpInteger; var Output: gmpInteger): Cardinal;
begin
   Result := mpz_remove(Output.NativeObject^, Src.NativeObject^, Factor.NativeObject^);
   Src.ReleaseTemporary;
   Factor.ReleaseTemporary;
end;

function gmpFactorial(Op: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_fac_ui(Result.NativeObject^, Op);
end;

// Calculates binomial coefficient
function gmpBinomial(const N: gmpInteger; K: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_bin_ui(Result.NativeObject^, N.NativeObject^, K);
   N.ReleaseTemporary;
end;

function gmpBinomial(N, K: Cardinal): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_bin_uiui(Result.NativeObject^, N, K);
end;

// Calculates N’th Fibonacci number. And NSub is F(N-1).
function gmpFibonacci(N: Cardinal): gmpInteger;
begin
   if N = 0 then
      raise gmpException.Create(gmpeParameterError);

   Result := GetTemporaryInteger()^;
   mpz_fib_ui(Result.NativeObject^, N);
end;

function gmpFibonacci(N: Cardinal; var NSub: gmpInteger): gmpInteger;
begin
   if N <= 1 then
      raise gmpException.Create(gmpeParameterError);

   Result := GetTemporaryInteger()^;
   mpz_fib2_ui(Result.NativeObject^, NSub.NativeObject^, N);
end;

// Calculates N’th Lucas number. And NSub is L(N-1).
function gmpLucas(N: Cardinal): gmpInteger;
begin
   if N = 0 then
      raise gmpException.Create(gmpeParameterError);

   Result := GetTemporaryInteger()^;
   mpz_lucnum_ui(Result.NativeObject^, N);
end;

function gmpLucas(N: Cardinal; var NSub: gmpInteger): gmpInteger;
begin
   if N <= 1 then
      raise gmpException.Create(gmpeParameterError);

   Result := GetTemporaryInteger()^;
   mpz_lucnum2_ui(Result.NativeObject^, NSub.NativeObject^, N);
end;

// Random number functions. Refer to GMP manual for details.

procedure FinalizeRandomState;
begin
   gmp_randclear(RandomState); // Clear random state
   RandomInitialized := False;
end;

procedure gmpInitializeRandomDefault;
begin
   if RandomInitialized then
      FinalizeRandomState;

   gmp_randinit_default(RandomState);
   RandomInitialized := True;
end;

procedure gmpInitializeRandomMT;
begin
   gmpInitializeRandomDefault;
end;

procedure gmpInitializeRandomLC_2Exp(const A: gmpInteger; C: Cardinal; m2Exp: mp_bitcnt_t);
begin
   if RandomInitialized then
      FinalizeRandomState;

   gmp_randinit_lc_2exp(RandomState, A.NativeObject^, C, m2Exp);
   RandomInitialized := True;
   A.ReleaseTemporary;
end;

procedure gmpInitializeRandomLC_2Exp_Size(Size: mp_bitcnt_t);
begin
   if RandomInitialized then
      FinalizeRandomState;

   gmp_randinit_lc_2exp_size(RandomState, Size);
   RandomInitialized := True;
end;

procedure gmpRandomSeed(const Seed: gmpInteger);
begin
   gmp_randseed(RandomState, Seed.NativeObject^);
   Seed.ReleaseTemporary;
end;

procedure gmpRandomSeed(Seed: Cardinal);
begin
   gmp_randseed_ui(RandomState, Seed);
end;

function gmpRandomIntegerBit(N: mp_bitcnt_t): Cardinal;
begin
   if N > 32 then
      raise gmpException.Create(gmpeParameterError);

   Result := gmp_urandomb_ui(RandomState, N);
end;

function gmpRandomIntegerBit2(N: mp_bitcnt_t): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_urandomb(Result.NativeObject^, RandomState, N);
end;

function gmpRandomIntegerBit_Debug(N: mp_bitcnt_t): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_rrandomb(Result.NativeObject^, RandomState, N);
end;

function gmpRandomInteger(Max: Cardinal): Cardinal;
begin
   Result := gmp_urandomm_ui(RandomState, Max);
end;

function gmpRandomInteger(const Max: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_urandomm(Result.NativeObject^, RandomState, Max.NativeObject^);
   Max.ReleaseTemporary;
end;

function gmpRandomFloat: gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_urandomb(Result.NativeObject^, RandomState, CurrentTemporaryFloatPrecision);
end;

function gmpRandomFloatDebug(MaxSize: mp_size_t; Exp: mp_exp_t): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_rrandomb(Result.NativeObject^, RandomState, MaxSize, Exp);
end;

function gmpRandomRange(const Min, Max: gmpInteger): gmpInteger;
begin
   if Min > Max then
      Result := gmpRandomInteger(Min - Max) + Max
   else
      Result := gmpRandomInteger(Max - Min) + Min;
end;

function gmpRandomRange(const Min, Max: gmpFloat): gmpFloat;
begin
   if Min > Max then
      Result := gmpRandomFloat * (Min - Max) + Max
   else
      Result := gmpRandomFloat * (Max - Min) + Min;
end;

// Prime test
function gmpProbablePrime(const N: gmpInteger; Prob: Integer; DivTested: Integer = 0): Boolean;
begin
   if Prob <= 0 then
      raise gmpException.Create(gmpeParameterError);

   Result := mpz_probable_prime_p(N.NativeObject^, RandomState, Prob, DivTested) = 1;
end;

function gmpProbablePrime(const N: gmpInteger; DivTested: Integer = 0): Boolean;
begin
   Result := mpz_likely_prime_p(N.NativeObject^, RandomState, DivTested) = 1;
end;

function gmpNextLikelyPrime(const N: gmpInteger): gmpInteger;
begin
   Result := GetTemporaryInteger()^;
   mpz_next_likely_prime(Result.NativeObject^, N.NativeObject^, RandomState);
   N.ReleaseTemporary;
end;

// For float
function gmpCeil(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_ceil(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

function gmpFloor(const Value: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_floor(Result.NativeObject^, Value.NativeObject^);
   Value.ReleaseTemporary;
end;

function gmpRelDiff(const A, B: gmpFloat): gmpFloat;
begin
   Result := GetTemporaryFloat()^;
   mpf_reldiff(Result.NativeObject^, A.NativeObject^, B.NativeObject^);
   A.ReleaseTemporary;
   B.ReleaseTemporary;
end;

initialization
   SetDefaultFloatPrecision(INITIAL_FLOAT_PRECISION); // And initialize temporary objects.
   gmpInitializeRandomDefault;

finalization
   FinalizeTemporaryObjects;
   FinalizeConstants;
   FinalizeRandomState;

end.


