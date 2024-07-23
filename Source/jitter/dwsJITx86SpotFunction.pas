{**************************************************************************}
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{     file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Copyright Eric Grange / Creative IT                                   }
{                                                                          }
{**************************************************************************}
unit dwsJITx86SpotFunction;

{$I ../dws.inc}

{$ifdef DEBUG}
   {$define DECLARE_TO_SAMPLING_PROFILER}
{$endif}

interface

uses
   Windows, SysUtils, Classes,
   dwsJITx86Intrinsics;

type
   IdwsJITSpotFunction = interface
      ['{CA1C0ECD-00B1-429D-8BD5-8ACA6736741B}']
      function Ptr : Pointer;
      function Size : Integer;
      function Name : String;
   end;

   TdwsJITSpotFunction = class (TInterfacedObject, IdwsJITSpotFunction)
      private
         FBuffer : Tx86_Platform_WriteOnlyStream;
         FPtr : Pointer;
         FSize : Integer;
         FAlteredXMM : TxmmRegisters;
         FName : String;

         function Name : String;

      public
         constructor Create(const aName : String = '');
         destructor Destroy; override;

         procedure Build;
         function Ptr : Pointer;
         function Size : Integer;

         property Buffer : Tx86_Platform_WriteOnlyStream read FBuffer;
         function Position : Integer;

         property AlteredXMM : TxmmRegisters read FAlteredXMM write FAlteredXMM;
         procedure Alter(reg : TxmmRegister);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsJITSpotFunction ------------------
// ------------------

// Create
//
constructor TdwsJITSpotFunction.Create(const aName : String = '');
begin
   inherited Create;
   if aName = '' then
      FName := ClassName
   else FName := aName;
   FBuffer := Tx86_Platform_WriteOnlyStream.Create;
end;

// Destroy
//
destructor TdwsJITSpotFunction.Destroy;
begin
   inherited;
   if Assigned(FPtr) then begin
      {$ifdef DECLARE_TO_SAMPLING_PROFILER}
      var undeclare := StringToUTF8('SAMPLING UNDECLARE ' + IntToHex(IntPtr(FPtr), 8));
      OutputDebugStringA(PAnsiChar(undeclare));
      {$endif}

      VirtualFree(FPtr, 0 , MEM_RELEASE);
   end;
end;

// Build
//
procedure TdwsJITSpotFunction.Build;
begin
   Assert(FPtr = nil, 'Already built');

   var opcodes := FBuffer.ToRawBytes;

   if Tx86_Platform_WriteOnlyStream = Tx86_64_WriteOnlyStream then begin
      var nbAlteredXmm := 0;
      for var i := xmm6 to xmm15 do
         if i in FAlteredXMM then
            Inc(nbAlteredXmm);
      if nbAlteredXmm > 0 then begin
         var stackPush := Tx86_64_WriteOnlyStream.Create;
         var stackPop := Tx86_64_WriteOnlyStream.Create;
         try
            stackPush._sub_reg_imm(gprRSP, nbAlteredXmm*16);
            var offset := 0;
            for var i := xmm6 to xmm15 do begin
               stackPush._vmovups_ptr_reg_reg(gprRSP, offset, i);
               stackPop._vmovups_ptr_reg(i, gprRSP, offset);
               Inc(offset, 16);
            end;
            stackPop._add_reg_imm(gprRSP, nbAlteredXmm*128);
            opcodes := stackPush.ToRawBytes + opcodes + stackPop.ToRawBytes;
         finally
            stackPush.Free;
            stackPop.Free;
         end;
      end else
   end;

   opcodes := opcodes + AnsiChar($C3) + AnsiChar($90) + AnsiChar($90) + AnsiChar($90); // ret nop nop nop

   FSize := Length(opcodes);
   FPtr := VirtualAlloc(nil, FSize, MEM_COMMIT, PAGE_READWRITE);
   System.Move(Pointer(opcodes)^, FPtr^, FSize);
   var oldProtect : Integer;
   VirtualProtect(FPtr, FSize, PAGE_EXECUTE_READ, @oldProtect);

   FreeAndNil(FBuffer);

   {$ifdef DECLARE_TO_SAMPLING_PROFILER}
   var declare := StringToUTF8('SAMPLING DECLARE ' + IntToHex(IntPtr(FPtr), 8) + ' ' + IntToHex(FSize, 8) + ' ' + FName);
   OutputDebugStringA(PAnsiChar(declare));
   {$endif}
end;

// Ptr
//
function TdwsJITSpotFunction.Ptr : Pointer;
begin
   Result := FPtr;
end;

// Size
//
function TdwsJITSpotFunction.Size : Integer;
begin
   Result := FSize;
end;

// Position
//
function TdwsJITSpotFunction.Position : Integer;
begin
   Result := FBuffer.Position;
end;

// Alter
//
procedure TdwsJITSpotFunction.Alter(reg : TxmmRegister);
begin
   Include(FAlteredXMM, reg);
end;

// Name
//
function TdwsJITSpotFunction.Name : String;
begin
   Result := FName;
end;

end.
