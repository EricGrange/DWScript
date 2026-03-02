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
{    The Initial Developer of the Original Code is Eric Grange.        }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsMathArrayFunctions;

{$I dws.inc}

interface

uses
   System.Classes,
   dwsUtils, dwsStrings,
   dwsFunctions, dwsExprs, dwsSymbols, dwsMagicExprs, dwsXPlatform, dwsExprList;

type

   TArrayDotProductFunc = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TFloatArrayProcessFunc = class(TInternalMagicDynArrayFunction)
      procedure DoEvalAsDynArray(const args : TExprBaseListExec; var result : IScriptDynArray); override;
      procedure DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec); virtual; abstract;
   end;

   TTwoFloatArrayProcessFunc = class(TInternalMagicDynArrayFunction)
      procedure DoEvalAsDynArray(const args : TExprBaseListExec; var result : IScriptDynArray); override;
      procedure DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer); virtual; abstract;
   end;

   TFloatArrayOffsetFunc = class(TFloatArrayProcessFunc)
      procedure DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec); override;
   end;
   TFloatArrayOffsetArrayFunc = class(TTwoFloatArrayProcessFunc)
      procedure DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer); override;
   end;
   TFloatArrayMultiplyFunc = class(TFloatArrayProcessFunc)
      procedure DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec); override;
   end;
   TFloatArrayMultiplyArrayFunc = class(TTwoFloatArrayProcessFunc)
      procedure DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer); override;
   end;
   TFloatArrayMinFunc = class(TFloatArrayProcessFunc)
      procedure DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec); override;
   end;
   TFloatArrayMinArrayFunc = class(TTwoFloatArrayProcessFunc)
      procedure DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer); override;
   end;
   TFloatArrayMaxFunc = class(TFloatArrayProcessFunc)
      procedure DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec); override;
   end;
   TFloatArrayMaxArrayFunc = class(TTwoFloatArrayProcessFunc)
      procedure DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer); override;
   end;
   TFloatArrayMultiplyAddFunc = class(TFloatArrayProcessFunc)
      procedure DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec); override;
   end;
   TFloatArrayReciprocalFunc = class(TFloatArrayProcessFunc)
      procedure DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec); override;
   end;

implementation

uses dwsDynamicArrays;

{$ifdef WIN64_ASM}
function SSE2_DotProduct(p1, p2 : PDouble; count : NativeInt) : Double;
// p1 = rcx, p2 = rdx, count = r8
asm
   xorpd xmm0, xmm0 // accumulator 1
   xorpd xmm1, xmm1 // accumulator 2
   
   mov rax, r8
   shr rax, 3      // rax = count / 8
   and r8, 7       // r8 = count % 8
   
   test rax, rax
   jz @@tail

@@loop8:
   movupd xmm2, [rcx]
   movupd xmm4, [rdx]
   mulpd xmm2, xmm4
   addpd xmm0, xmm2
   
   movupd xmm3, [rcx+16]
   movupd xmm4, [rdx+16]
   mulpd xmm3, xmm4
   addpd xmm1, xmm3

   movupd xmm2, [rcx+32]
   movupd xmm4, [rdx+32]
   mulpd xmm2, xmm4
   addpd xmm0, xmm2
   
   movupd xmm3, [rcx+48]
   movupd xmm4, [rdx+48]
   mulpd xmm3, xmm4
   addpd xmm1, xmm3

   add rcx, 64
   add rdx, 64
   dec rax
   jnz @@loop8

@@tail:
   addpd xmm0, xmm1

   test r8, 4
   jz @@tail2

   movupd xmm2, [rcx]
   movupd xmm4, [rdx]
   mulpd xmm2, xmm4
   addpd xmm0, xmm2
   movupd xmm3, [rcx+16]
   movupd xmm4, [rdx+16]
   mulpd xmm3, xmm4
   addpd xmm0, xmm3
   add rcx, 32
   add rdx, 32

@@tail2:
   test r8, 2
   jz @@tail1

   movupd xmm2, [rcx]
   movupd xmm4, [rdx]
   mulpd xmm2, xmm4
   addpd xmm0, xmm2
   add rcx, 16
   add rdx, 16

@@tail1:
   // Horizontal add: sum high and low doubles of xmm0
   movhlps xmm1, xmm0
   addsd xmm0, xmm1

   test r8, 1
   jz @@exit
   movsd xmm1, [rcx]
   mulsd xmm1, [rdx]
   addsd xmm0, xmm1

@@exit:
end;

procedure SSE2_AddArrays(p1, p2 : PDouble; count : NativeInt);
// p1 = rcx, p2 = rdx, count = r8
asm
   mov rax, r8
   shr rax, 3
   and r8, 7
   test rax, rax
   jz @@tail

@@loop8:
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   addpd xmm0, xmm1
   movupd [rcx], xmm0

   movupd xmm2, [rcx+16]
   movupd xmm3, [rdx+16]
   addpd xmm2, xmm3
   movupd [rcx+16], xmm2

   movupd xmm0, [rcx+32]
   movupd xmm1, [rdx+32]
   addpd xmm0, xmm1
   movupd [rcx+32], xmm0

   movupd xmm2, [rcx+48]
   movupd xmm3, [rdx+48]
   addpd xmm2, xmm3
   movupd [rcx+48], xmm2

   add rcx, 64
   add rdx, 64
   dec rax
   jnz @@loop8

@@tail:
   test r8, 4
   jz @@tail2
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   addpd xmm0, xmm1
   movupd [rcx], xmm0
   movupd xmm2, [rcx+16]
   movupd xmm3, [rdx+16]
   addpd xmm2, xmm3
   movupd [rcx+16], xmm2
   add rcx, 32
   add rdx, 32

@@tail2:
   test r8, 2
   jz @@tail1
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   addpd xmm0, xmm1
   movupd [rcx], xmm0
   add rcx, 16
   add rdx, 16

@@tail1:
   test r8, 1
   jz @@exit
   movsd xmm0, [rcx]
   addsd xmm0, [rdx]
   movsd [rcx], xmm0
@@exit:
end;

procedure SSE2_MultiplyArrays(p1, p2 : PDouble; count : NativeInt);
// p1 = rcx, p2 = rdx, count = r8
asm
   mov rax, r8
   shr rax, 3
   and r8, 7
   test rax, rax
   jz @@tail

@@loop8:
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   mulpd xmm0, xmm1
   movupd [rcx], xmm0

   movupd xmm2, [rcx+16]
   movupd xmm3, [rdx+16]
   mulpd xmm2, xmm3
   movupd [rcx+16], xmm2

   movupd xmm0, [rcx+32]
   movupd xmm1, [rdx+32]
   mulpd xmm0, xmm1
   movupd [rcx+32], xmm0

   movupd xmm2, [rcx+48]
   movupd xmm3, [rdx+48]
   mulpd xmm2, xmm3
   movupd [rcx+48], xmm2

   add rcx, 64
   add rdx, 64
   dec rax
   jnz @@loop8

@@tail:
   test r8, 4
   jz @@tail2
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   mulpd xmm0, xmm1
   movupd [rcx], xmm0
   movupd xmm2, [rcx+16]
   movupd xmm3, [rdx+16]
   mulpd xmm2, xmm3
   movupd [rcx+16], xmm2
   add rcx, 32
   add rdx, 32

@@tail2:
   test r8, 2
   jz @@tail1
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   mulpd xmm0, xmm1
   movupd [rcx], xmm0
   add rcx, 16
   add rdx, 16

@@tail1:
   test r8, 1
   jz @@exit
   movsd xmm0, [rcx]
   mulsd xmm0, [rdx]
   movsd [rcx], xmm0
@@exit:
end;

procedure SSE2_MinArrays(p1, p2 : PDouble; count : NativeInt);
// p1 = rcx, p2 = rdx, count = r8
asm
   mov rax, r8
   shr rax, 3
   and r8, 7
   test rax, rax
   jz @@tail

@@loop8:
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   minpd xmm0, xmm1
   movupd [rcx], xmm0

   movupd xmm2, [rcx+16]
   movupd xmm3, [rdx+16]
   minpd xmm2, xmm3
   movupd [rcx+16], xmm2

   movupd xmm0, [rcx+32]
   movupd xmm1, [rdx+32]
   minpd xmm0, xmm1
   movupd [rcx+32], xmm0

   movupd xmm2, [rcx+48]
   movupd xmm3, [rdx+48]
   minpd xmm2, xmm3
   movupd [rcx+48], xmm2

   add rcx, 64
   add rdx, 64
   dec rax
   jnz @@loop8

@@tail:
   test r8, 4
   jz @@tail2
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   minpd xmm0, xmm1
   movupd [rcx], xmm0
   movupd xmm2, [rcx+16]
   movupd xmm3, [rdx+16]
   minpd xmm2, xmm3
   movupd [rcx+16], xmm2
   add rcx, 32
   add rdx, 32

@@tail2:
   test r8, 2
   jz @@tail1
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   minpd xmm0, xmm1
   movupd [rcx], xmm0
   add rcx, 16
   add rdx, 16

@@tail1:
   test r8, 1
   jz @@exit
   movsd xmm0, [rcx]
   minsd xmm0, [rdx]
   movsd [rcx], xmm0
@@exit:
end;

procedure SSE2_MaxArrays(p1, p2 : PDouble; count : NativeInt);
// p1 = rcx, p2 = rdx, count = r8
asm
   mov rax, r8
   shr rax, 3
   and r8, 7
   test rax, rax
   jz @@tail

@@loop8:
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   maxpd xmm0, xmm1
   movupd [rcx], xmm0

   movupd xmm2, [rcx+16]
   movupd xmm3, [rdx+16]
   maxpd xmm2, xmm3
   movupd [rcx+16], xmm2

   movupd xmm0, [rcx+32]
   movupd xmm1, [rdx+32]
   maxpd xmm0, xmm1
   movupd [rcx+32], xmm0

   movupd xmm2, [rcx+48]
   movupd xmm3, [rdx+48]
   maxpd xmm2, xmm3
   movupd [rcx+48], xmm2

   add rcx, 64
   add rdx, 64
   dec rax
   jnz @@loop8

@@tail:
   test r8, 4
   jz @@tail2
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   maxpd xmm0, xmm1
   movupd [rcx], xmm0
   movupd xmm2, [rcx+16]
   movupd xmm3, [rdx+16]
   maxpd xmm2, xmm3
   movupd [rcx+16], xmm2
   add rcx, 32
   add rdx, 32

@@tail2:
   test r8, 2
   jz @@tail1
   movupd xmm0, [rcx]
   movupd xmm1, [rdx]
   maxpd xmm0, xmm1
   movupd [rcx], xmm0
   add rcx, 16
   add rdx, 16

@@tail1:
   test r8, 1
   jz @@exit
   movsd xmm0, [rcx]
   maxsd xmm0, [rdx]
   movsd [rcx], xmm0
@@exit:
end;

procedure SSE2_MultiplyAdd(p : PDouble; nb : NativeInt; scale, offset : Double);
// p = rcx, nb = rdx, scale = xmm0, offset = xmm1
asm
   unpcklpd xmm0, xmm0
   unpcklpd xmm1, xmm1

   test rdx, 1
   jz @@loop2
   dec rdx
   movsd xmm2, [rcx]
   mulsd xmm2, xmm0
   addsd xmm2, xmm1
   movsd [rcx], xmm2
   add rcx, 8

@@loop2:
   movupd xmm2, [rcx]
   mulpd xmm2, xmm0
   addpd xmm2, xmm1
   movupd [rcx], xmm2
   add rcx, 16
   sub rdx, 2
   jnz @@loop2
end;
{$endif}

{ TArrayDotProductFunc }

procedure TArrayDotProductFunc.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   arr1, arr2 : IScriptDynArray;
   off1, off2, count : Integer;
   n1, stride1, n2, stride2 : NativeInt;
   p1, p2 : PDouble;
begin
   args.EvalAsDynArray(0, arr1);
   args.EvalAsDynArray(1, arr2);

   p1 := (arr1 as IPDoubleArray).AsPDouble(n1, stride1);
   p2 := (arr2 as IPDoubleArray).AsPDouble(n2, stride2);

   if args.Count = 2 then begin
      off1 := 0;
      off2 := 0;
      if n1 <= n2 then
        count := n1
      else count := n2;
   end else begin
      off1 := args.AsInteger[2];
      off2 := args.AsInteger[3];
      count := args.AsInteger[4];

      if count < 0 then
         raise EScriptError.CreateFmt(RTE_PositiveCountExpected, [count]);

      if (off1 < 0) or (off1 + count > n1) or (off2 < 0) or (off2 + count > n2) then
         raise EScriptError.Create('ArrayDotProduct: Index out of bounds');
   end;

   Result := 0;
   if count <= 0 then Exit;

   p1 := Pointer(IntPtr(p1) + off1 * stride1);
   p2 := Pointer(IntPtr(p2) + off2 * stride2);

   {$ifdef WIN64_ASM}
   if (stride1 = SizeOf(Double)) and (stride2 = SizeOf(Double)) then
      Result := SSE2_DotProduct(p1, p2, count);
   {$else}
   for var i := 0 to count - 1 do begin
      Result := Result + p1^ * p2^;
      p1 := Pointer(IntPtr(p1) + stride1);
      p2 := Pointer(IntPtr(p2) + stride2);
   end;
   {$endif}
end;

// ------------------
// ------------------ TFloatArrayProcessFunc ------------------
// ------------------

// DoEvalAsDynArray
//
procedure TFloatArrayProcessFunc.DoEvalAsDynArray(const args : TExprBaseListExec; var result : IScriptDynArray);
var
   n, stride : NativeInt;
   p : PDouble;
begin
   args.EvalAsDynArray(0, result);
   p := (result as IPDoubleArray).AsPDouble(n, stride);
   if p <> nil then
      DoProcess(p, n, stride, args);
end;

// ------------------
// ------------------ TTwoFloatArrayProcessFunc ------------------
// ------------------

// DoEvalAsDynArray
//
procedure TTwoFloatArrayProcessFunc.DoEvalAsDynArray(const args : TExprBaseListExec; var result : IScriptDynArray);
var
   arr1, arr2 : IScriptDynArray;
   off1, off2, count : Integer;
   n1, stride1, n2, stride2 : NativeInt;
   p1, p2 : PDouble;
begin
   args.EvalAsDynArray(0, arr1);
   result := arr1;

   args.EvalAsDynArray(1, arr2);
   p1 := (arr1 as IPDoubleArray).AsPDouble(n1, stride1);
   p2 := (arr2 as IPDoubleArray).AsPDouble(n2, stride2);

   if args.Count = 2 then begin
      off1 := 0;
      off2 := 0;
      if n1 <= n2 then
        count := n1
      else count := n2;
   end else begin
      off1 := args.AsInteger[2];
      off2 := args.AsInteger[3];
      count := args.AsInteger[4];

      if count < 0 then
         raise EScriptError.CreateFmt(RTE_PositiveCountExpected, [count]);

      if (off1 < 0) or (off1 + count > n1) or (off2 < 0) or (off2 + count > n2) then
         raise EScriptError.Create('Index out of bounds');
   end;

   if count <= 0 then Exit;

   p1 := Pointer(IntPtr(p1) + off1 * stride1);
   p2 := Pointer(IntPtr(p2) + off2 * stride2);

   DoProcessTwo(p1, p2, count, stride1, stride2);
end;

// ------------------
// ------------------ TFloatArrayOffsetFunc ------------------
// ------------------

// DoProcess
//
procedure TFloatArrayOffsetFunc.DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec);
var
   operand : Double;
   i : NativeInt;
begin
   operand := args.AsFloat[1];
   for i := 0 to n-1 do begin
      p^ := p^ + operand;
      p := Pointer(IntPtr(p) + stride);
   end;
end;

// ------------------
// ------------------ TFloatArrayOffsetArrayFunc ------------------
// ------------------

// DoProcessTwo
//
procedure TFloatArrayOffsetArrayFunc.DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer);
begin
   {$ifdef WIN64_ASM}
   if (stride1 = SizeOf(Double)) and (stride2 = SizeOf(Double)) then begin
      SSE2_AddArrays(p1, p2, count);
      Exit;
   end;
   {$endif}

   for var i := 0 to count - 1 do begin
      p1^ := p1^ + p2^;
      p1 := Pointer(IntPtr(p1) + stride1);
      p2 := Pointer(IntPtr(p2) + stride2);
   end;
end;

// ------------------
// ------------------ TFloatArrayMultiplyFunc ------------------
// ------------------

// DoProcess
//
procedure TFloatArrayMultiplyFunc.DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec);
var
   operand : Double;
   i : NativeInt;
begin
   operand := args.AsFloat[1];
   for i := 0 to n-1 do begin
      p^ := p^ * operand;
      p := Pointer(IntPtr(p) + stride);
   end;
end;

// ------------------
// ------------------ TFloatArrayMultiplyArrayFunc ------------------
// ------------------

// DoProcessTwo
//
procedure TFloatArrayMultiplyArrayFunc.DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer);
begin
   {$ifdef WIN64_ASM}
   if (stride1 = SizeOf(Double)) and (stride2 = SizeOf(Double)) then begin
      SSE2_MultiplyArrays(p1, p2, count);
      Exit;
   end;
   {$endif}

   for var i := 0 to count - 1 do begin
      p1^ := p1^ * p2^;
      p1 := Pointer(IntPtr(p1) + stride1);
      p2 := Pointer(IntPtr(p2) + stride2);
   end;
end;

// ------------------
// ------------------ TFloatArrayMinFunc ------------------
// ------------------

// DoProcess
//
procedure TFloatArrayMinFunc.DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec);
var
   operand : Double;
   i : NativeInt;
begin
   operand := args.AsFloat[1];
   for i := 0 to n-1 do begin
      if operand < p^ then p^ := operand;
      p := Pointer(IntPtr(p) + stride);
   end;
end;

// ------------------
// ------------------ TFloatArrayMinArrayFunc ------------------
// ------------------

// DoProcessTwo
//
procedure TFloatArrayMinArrayFunc.DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer);
begin
   {$ifdef WIN64_ASM}
   if (stride1 = SizeOf(Double)) and (stride2 = SizeOf(Double)) then begin
      SSE2_MinArrays(p1, p2, count);
      Exit;
   end;
   {$endif}

   for var i := 0 to count - 1 do begin
      if p2^ < p1^ then p1^ := p2^;
      p1 := Pointer(IntPtr(p1) + stride1);
      p2 := Pointer(IntPtr(p2) + stride2);
   end;
end;

// ------------------
// ------------------ TFloatArrayMaxFunc ------------------
// ------------------

// DoProcess
//
procedure TFloatArrayMaxFunc.DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec);
var
   operand : Double;
   i : NativeInt;
begin
   operand := args.AsFloat[1];
   for i := 0 to n-1 do begin
      if operand > p^ then p^ := operand;
      p := Pointer(IntPtr(p) + stride);
   end;
end;

// ------------------
// ------------------ TFloatArrayMaxArrayFunc ------------------
// ------------------

// DoProcessTwo
//
procedure TFloatArrayMaxArrayFunc.DoProcessTwo(p1, p2 : PDouble; count : NativeInt; stride1, stride2 : Integer);
begin
   {$ifdef WIN64_ASM}
   if (stride1 = SizeOf(Double)) and (stride2 = SizeOf(Double)) then begin
      SSE2_MaxArrays(p1, p2, count);
      Exit;
   end;
   {$endif}

   for var i := 0 to count - 1 do begin
      if p2^ > p1^ then p1^ := p2^;
      p1 := Pointer(IntPtr(p1) + stride1);
      p2 := Pointer(IntPtr(p2) + stride2);
   end;
end;

// ------------------
// ------------------ TFloatArrayMultiplyAddFunc ------------------
// ------------------

// DoProcess
//
procedure TFloatArrayMultiplyAddFunc.DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec);
var
   scale, offset : Double;
   i : NativeInt;
begin
   scale := args.AsFloat[1];
   offset := args.AsFloat[2];
   {$ifdef WIN64_ASM}
   if (n > 4) and (stride = SizeOf(Double)) then begin
      SSE2_MultiplyAdd(p, n, scale, offset);
      Exit;
   end;
   {$endif}
   for i := 0 to n-1 do begin
      p^ := p^ * scale + offset;
      p := Pointer(IntPtr(p) + stride);
   end;
end;

// ------------------
// ------------------ TFloatArrayReciprocalFunc ------------------
// ------------------

// DoProcess
//
procedure TFloatArrayReciprocalFunc.DoProcess(p : PDouble; n : NativeInt; stride : Integer; const args : TExprBaseListExec);
var
   i : NativeInt;
begin
   for i := 0 to n-1 do begin
      p^ := 1 / p^;
      p := Pointer(IntPtr(p) + stride);
   end;
end;

initialization

   RegisterInternalFloatFunction(TArrayDotProductFunc, 'ArrayDotProduct', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT], [iffStateLess, iffOverloaded], 'DotProduct');
   RegisterInternalFloatFunction(TArrayDotProductFunc, 'ArrayDotProduct', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT, 'offset1', SYS_INTEGER, 'offset2', SYS_INTEGER, 'count', SYS_INTEGER], [iffStateLess, iffOverloaded]);

   RegisterInternalFunction(TFloatArrayOffsetArrayFunc, '', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT, 'offset1', SYS_INTEGER, 'offset2', SYS_INTEGER, 'count', SYS_INTEGER], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Offset');
   RegisterInternalFunction(TFloatArrayOffsetArrayFunc, '', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Offset');
   RegisterInternalFunction(TFloatArrayOffsetFunc, '', ['a', SYS_ARRAY_OF_FLOAT, 'operand', SYS_FLOAT], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Offset');

   RegisterInternalFunction(TFloatArrayMultiplyArrayFunc, '', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT, 'offset1', SYS_INTEGER, 'offset2', SYS_INTEGER, 'count', SYS_INTEGER], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Multiply');
   RegisterInternalFunction(TFloatArrayMultiplyArrayFunc, '', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Multiply');
   RegisterInternalFunction(TFloatArrayMultiplyFunc, '', ['a', SYS_ARRAY_OF_FLOAT, 'operand', SYS_FLOAT], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Multiply');

   RegisterInternalFunction(TFloatArrayMinArrayFunc, '', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT, 'offset1', SYS_INTEGER, 'offset2', SYS_INTEGER, 'count', SYS_INTEGER], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Min');
   RegisterInternalFunction(TFloatArrayMinArrayFunc, '', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Min');
   RegisterInternalFunction(TFloatArrayMinFunc, '', ['a', SYS_ARRAY_OF_FLOAT, 'operand', SYS_FLOAT], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Min');

   RegisterInternalFunction(TFloatArrayMaxArrayFunc, '', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT, 'offset1', SYS_INTEGER, 'offset2', SYS_INTEGER, 'count', SYS_INTEGER], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Max');
   RegisterInternalFunction(TFloatArrayMaxArrayFunc, '', ['arr1', SYS_ARRAY_OF_FLOAT, 'arr2', SYS_ARRAY_OF_FLOAT], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Max');
   RegisterInternalFunction(TFloatArrayMaxFunc, '', ['a', SYS_ARRAY_OF_FLOAT, 'operand', SYS_FLOAT], SYS_ARRAY_OF_FLOAT, [iffOverloaded], 'Max');

   RegisterInternalFunction(TFloatArrayMultiplyAddFunc, '', ['a', SYS_ARRAY_OF_FLOAT, 'scale', SYS_FLOAT, 'offset', SYS_FLOAT], SYS_ARRAY_OF_FLOAT, [], 'MultiplyAdd');
   RegisterInternalFunction(TFloatArrayReciprocalFunc, '', ['a', SYS_ARRAY_OF_FLOAT], SYS_ARRAY_OF_FLOAT, [], 'Reciprocal');

end.
