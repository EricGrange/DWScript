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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsUtils;

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

end.
