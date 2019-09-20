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
unit dwsSynSQLiteFunctions;

{$I dws.inc}

interface

uses
   SynSQLite3;

// Sqrt()
procedure SQLiteFunc_Sqrt(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;

// Median() aggregate
procedure SQLiteFunc_MedianStep(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;
procedure SQLiteFunc_MedianFinal(context: TSQLite3FunctionContext); cdecl;

// Bool_And() / Bool_Or() aggregates
procedure SQLiteFunc_BoolAndStep(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;
procedure SQLiteFunc_BoolOrStep(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;
procedure SQLiteFunc_BoolFinal(context: TSQLite3FunctionContext); cdecl;

// Bit_And() / Bit_Or() aggregates
procedure SQLiteFunc_BitAndStep(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;
procedure SQLiteFunc_BitOrStep(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;
procedure SQLiteFunc_BitFinal(context: TSQLite3FunctionContext); cdecl;

// Bitwise Population count (for integers & blobs)
procedure SQLiteFunc_BitPopCount(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;

// Hamming Distance
procedure SQLiteFunc_HammingDistance(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$ifdef DELPHI_XE2_PLUS}
   {$if Defined(WIN32_ASM) or Defined(WIN64_ASM)}
      {$define TEST_POPCNT}
   {$ifend}
{$endif}

uses dwsUtils
   {$ifdef TEST_POPCNT}, System.Math{$endif};

type
   TNullableBoolean = (nbNull = 0, nbTrue, nbFalse);

   TNullableInt64 = record
      Value : Int64;
      NotNull : Boolean;
   end;

// Maths ---------------------------------------------------

// SQLiteFunc_Sqrt
//
procedure SQLiteFunc_Sqrt(context : TSQLite3FunctionContext;
                          argc : Integer; var argv : TSQLite3ValueArray);
var
   a : Double;
begin
   Assert(argc = 1);
   a := sqlite3.value_double(argv[0]);
   if a <= 0 then
      sqlite3.result_null(context)
   else sqlite3.result_double(context, Sqrt(a));
end;

// Median ---------------------------------------------------

// SQLiteFunc_MedianStep
//
procedure SQLiteFunc_MedianStep(context : TSQLite3FunctionContext;
                                argc : Integer; var argv : TSQLite3ValueArray);
var
   list : TSimpleDoubleList;
   p : ^TSimpleDoubleList;
begin
   Assert(argc = 1);
   if sqlite3.value_type(argv[0]) <> SQLITE_NULL then begin
      p := sqlite3.aggregate_context(context, SizeOf(p^));
      list := p^;
      if list = nil then begin
         list := TSimpleDoubleList.Create;
         p^ := list;
      end;
      list.Add(sqlite3.value_double(argv[0]));
   end;
end;

// SQLiteFunc_MedianFinal
//
procedure SQLiteFunc_MedianFinal(context: TSQLite3FunctionContext);
var
   list : TSimpleDoubleList;
   p : ^TSimpleDoubleList;
begin
   p := sqlite3.aggregate_context(context, SizeOf(p^));
   list := p^;
   if list = nil then
      sqlite3.result_null(context)
   else try
      sqlite3.result_double(context, list.QuickMedian);
   finally
      list.Free;
   end;
end;

//
// Bool aggregates ---------------------------------------------------
//

// SQLiteFunc_BoolAndStep
//
procedure SQLiteFunc_BoolAndStep(context : TSQLite3FunctionContext;
                                 argc : Integer; var argv : TSQLite3ValueArray);
var
   p : ^TNullableBoolean;
begin
   Assert(argc = 1);
   if sqlite3.value_type(argv[0]) <> SQLITE_NULL then begin
      p := sqlite3.aggregate_context(context, SizeOf(p^));
      case p^ of
         nbNull :
            if sqlite3.value_int64(argv[0]) <> 0 then
               p^ := nbTrue
            else p^ := nbFalse;
         nbTrue :
            if sqlite3.value_int64(argv[0]) = 0 then
               p^ := nbFalse;
      end;
   end;
end;

// SQLiteFunc_BoolOrStep
//
procedure SQLiteFunc_BoolOrStep(context : TSQLite3FunctionContext;
                                argc : Integer; var argv : TSQLite3ValueArray);
var
   p : ^TNullableBoolean;
begin
   Assert(argc = 1);
   if sqlite3.value_type(argv[0]) <> SQLITE_NULL then begin
      p := sqlite3.aggregate_context(context, SizeOf(p^));
      case p^ of
         nbNull :
            if sqlite3.value_int64(argv[0]) <> 0 then
               p^ := nbTrue
            else p^ := nbFalse;
         nbFalse :
            if sqlite3.value_int64(argv[0]) <> 0 then
               p^ := nbTrue;
      end;
   end;
end;

// SQLiteFunc_BoolFinal
//
procedure SQLiteFunc_BoolFinal(context: TSQLite3FunctionContext);
var
   p : ^TNullableBoolean;
begin
   p := sqlite3.aggregate_context(context, SizeOf(p^));
   case p^ of
      nbTrue : sqlite3.result_int64(context, 1);
      nbFalse : sqlite3.result_int64(context, 0);
   else
      sqlite3.result_null(context);
   end;
end;

//
// Bitwise aggregates ---------------------------------------------------
//

// SQLiteFunc_BitAndStep
//
procedure SQLiteFunc_BitAndStep(context : TSQLite3FunctionContext;
                                argc : Integer; var argv : TSQLite3ValueArray);
var
   p : ^TNullableInt64;
begin
   Assert(argc = 1);
   if sqlite3.value_type(argv[0]) <> SQLITE_NULL then begin
      p := sqlite3.aggregate_context(context, SizeOf(p^));
      if p^.NotNull then
         p^.Value := p^.Value and sqlite3.value_int64(argv[0])
      else begin
         p^.Value := sqlite3.value_int64(argv[0]);
         p^.NotNull := True;
      end;
   end;
end;

// SQLiteFunc_BitOrStep
//
procedure SQLiteFunc_BitOrStep(context : TSQLite3FunctionContext;
                               argc : Integer; var argv : TSQLite3ValueArray);
var
   p : ^TNullableInt64;
begin
   Assert(argc = 1);
   if sqlite3.value_type(argv[0]) <> SQLITE_NULL then begin
      p := sqlite3.aggregate_context(context, SizeOf(p^));
      if p^.NotNull then
         p^.Value := p^.Value or sqlite3.value_int64(argv[0])
      else begin
         p^.Value := sqlite3.value_int64(argv[0]);
         p^.NotNull := True;
      end;
   end;
end;

// SQLiteFunc_BitFinal
//
procedure SQLiteFunc_BitFinal(context: TSQLite3FunctionContext);
var
   p : ^TNullableInt64;
begin
   p := sqlite3.aggregate_context(context, SizeOf(p^));
   if p^.NotNull then
      sqlite3.result_int64(context, p^.Value)
   else sqlite3.result_null(context);
end;

//
// Bitwise population count ---------------------------------------------------
//

// SQLiteFunc_BitPopCount
//
procedure SQLiteFunc_BitPopCount(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;
var
   v : Int64;
begin
   Assert(argc = 1);
   case sqlite3.value_type(argv[0]) of
      SQLITE_INTEGER : begin
         v := sqlite3.value_int64(argv[0]);
         sqlite3.result_int64(context, PopCount64(@v, 1));
      end;
      SQLITE_BLOB : begin
         sqlite3.result_int64(context, PopCount(sqlite3.value_blob(argv[0]), sqlite3.value_bytes(argv[0])));
      end
   else
      sqlite3.result_null(context);
   end;
end;

//
// Hamming distance ---------------------------------------------------
//

// HammingDistance
//
function HammingDistance(p1, p2 : PByte; n : Integer) : Integer;
var
   v : Integer;
   x : Int64;
begin
   Result := 0;
   while n >= 8 do begin
      x := PInt64(p1)^ xor PInt64(p2)^;
      Inc(Result, PopCount64(@x, 1));
      Inc(p1, 8);
      Inc(p2, 8);
      Dec(n, 8);
   end;
   if n >= 4 then begin
      Inc(Result, PopCount32(PInteger(p1)^ xor PInteger(p2)^));
      Inc(p1, 4);
      Inc(p2, 4);
      Dec(n, 4);
   end;
   case n of
      1 : v := p1^ xor p2^;
      2 : v := PWord(p1)^ xor PWord(p2)^;
      3 : v :=     ((PWord(p1)^ shl 16) or p1[2])
               xor ((PWord(p2)^ shl 16) or p2[2]);
   else
      Exit
   end;
   Inc(Result, PopCount32(v));
end;

// SQLiteFunc_HammingDistance
//
procedure SQLiteFunc_HammingDistance(context : TSQLite3FunctionContext; argc : Integer; var argv : TSQLite3ValueArray); cdecl;

   procedure HandleBlob;
   var
      buf1 : array of Byte;
      size : Integer;
   begin
      size := sqlite3.value_bytes(argv[0]);
      if sqlite3.value_bytes(argv[1]) <> size then begin
         sqlite3.result_null(context);
         Exit;
      end;
      if size = 0 then begin
         sqlite3.result_int64(context, 0);
         Exit;
      end;
      SetLength(buf1, size);
      System.Move(sqlite3.value_blob(argv[0])^, buf1[0], size);
      sqlite3.result_int64(context, HammingDistance(Pointer(buf1), sqlite3.value_blob(argv[1]), size));
   end;

   procedure HandleText;
   var
      buf1, buf2 : String;
      size, i, n : Integer;
      p1, p2 : PChar;
   begin
      size := sqlite3.value_bytes(argv[0]);
      if sqlite3.value_bytes(argv[1]) <> size then begin
         sqlite3.result_null(context);
         Exit;
      end;
      if size = 0 then begin
         sqlite3.result_int64(context, 0);
         Exit;
      end;
      buf1 := UTF8ToUnicodeString(sqlite3.value_text(argv[0]));
      buf2 := UTF8ToUnicodeString(sqlite3.value_text(argv[1]));
      size := Length(buf1);
      if Length(buf2) <> size then begin
         sqlite3.result_null(context);
         Exit;
      end;
      p1 := Pointer(buf1);
      p2 := Pointer(buf2);
      n := 0;
      for i := 0 to size-1 do begin
         if p1[i] <> p2[i] then
            Inc(n);
      end;
      sqlite3.result_int64(context, n);
   end;

var
   typ : Integer;
   v1, v2 : Int64;
begin
   Assert(argc = 2);
   typ := sqlite3.value_type(argv[0]);
   if sqlite3.value_type(argv[1]) <> typ then
      sqlite3.result_null(context)
   else case typ of
      SQLITE_INTEGER : begin
         v1 := sqlite3.value_int64(argv[0]);
         v2 := sqlite3.value_int64(argv[1]);
         sqlite3.result_int64(context, HammingDistance(@v1, @v2, SizeOf(Int64)));
      end;
      SQLITE_TEXT : HandleText;
      SQLITE_BLOB : HandleBlob;
   else
      sqlite3.result_null(context);
   end;
end;

end.
