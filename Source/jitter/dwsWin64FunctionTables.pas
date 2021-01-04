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
unit dwsWin64FunctionTables;

{$I ../dws.inc}

interface

uses Types;

type
   DWORD64 = UInt64;
   UBYTE = Byte;
   USHORT = WORD;

   _IMAGE_RUNTIME_FUNCTION_ENTRY = record
      BeginAddress : DWORD;
      EndAddress : DWORD;
      case Integer of
         0 : (UnwindInfoAddress : DWORD);
         1 : (UnwindData : DWORD);
   end;
   RUNTIME_FUNCTION = _IMAGE_RUNTIME_FUNCTION_ENTRY;
   PRUNTIME_FUNCTION = ^RUNTIME_FUNCTION;
   _PIMAGE_RUNTIME_FUNCTION_ENTRY = ^_IMAGE_RUNTIME_FUNCTION_ENTRY;

   _UNWIND_OP_CODES = (
      UWOP_PUSH_NONVOL = 0, // info == register number
      UWOP_ALLOC_LARGE,     // no info, alloc size in next 2 slots
      UWOP_ALLOC_SMALL,     // info == size of allocation / 8 - 1
      UWOP_SET_FPREG,       // no info, FP = RSP + UNWIND_INFO.FPRegOffset*16
      UWOP_SAVE_NONVOL,     // info == register number, offset in next slot
      UWOP_SAVE_NONVOL_FAR, // info == register number, offset in next 2 slots
      UWOP_SAVE_XMM128 = 8, // info == XMM reg number, offset in next slot
      UWOP_SAVE_XMM128_FAR, // info == XMM reg number, offset in next 2 slots
      UWOP_PUSH_MACHFRAME   // info == 0: no error-code, 1: error-code
   );
   UNWIND_CODE_OPS = _UNWIND_OP_CODES;

   _UNWIND_CODE = record
      CodeOffset : UBYTE;
      UnwindOp_OpInfo : UBYTE;   // 4 bits 4 bits
      FrameOffset : USHORT;

      function GetUnwindOp : UNWIND_CODE_OPS;
      procedure SetUnwindOp(v : UNWIND_CODE_OPS);
      property UnwindOp : UNWIND_CODE_OPS read GetUnwindOp write SetUnwindOp;
      function GetOpInfo : UBYTE;
      procedure SetOpInfo(v : UBYTE);
      property OpInfo : UBYTE read GetOpInfo write SetOpInfo;
   end;
   UNWIND_CODE = _UNWIND_CODE;
   PUNWIND_CODE = ^UNWIND_CODE;

const
   UNW_FLAG_EHANDLER  = $01;
   UNW_FLAG_UHANDLER  = $02;
   UNW_FLAG_CHAININFO = $04;

type
   _UNWIND_INFO = record
      Version_Flags : UBYTE;    // 3 bits 5 bits

      SizeOfProlog : UBYTE;
      CountOfCodes : UBYTE;

      FrameRegister_FrameOffset : UBYTE;    // 4 bits 4 bits

      UnwindCode : array [0..0] of UNWIND_CODE;

      function GetVersion : UBYTE;
      procedure SetVersion(v : UBYTE);
      property Version : UBYTE read GetVersion write SetVersion;
      function GetFlags : UBYTE;
      procedure SetFlags(v : UBYTE);
      property Flags : UBYTE read GetFlags write SetFlags;

      function GetFrameRegister : UBYTE;
      procedure SetFrameRegister(v : UBYTE);
      property FrameRegister : UBYTE read GetFrameRegister write SetFrameRegister;
      function GetFrameOffset : UBYTE;
      procedure SetFrameOffset(v : UBYTE);
      property FrameOffset : UBYTE read GetFrameOffset write SetFrameOffset;

   end;
   UNWIND_INFO = _UNWIND_INFO;
   PUNWIND_INFO = ^UNWIND_INFO;

(*
#define GetUnwindCodeEntry(info, index) \
    ((info)->UnwindCode[index])

#define GetLanguageSpecificDataPtr(info) \
    ((PVOID)&GetUnwindCodeEntry((info),((info)->CountOfCodes + 1) & ~1))

#define GetExceptionHandler(base, info) \
    ((PEXCEPTION_HANDLER)((base) + *(PULONG)GetLanguageSpecificDataPtr(info)))

#define GetChainedFunctionEntry(base, info) \
    ((PRUNTIME_FUNCTION)((base) + *(PULONG)GetLanguageSpecificDataPtr(info)))

#define GetExceptionDataPtr(info) \
    ((PVOID)((PULONG)GetLanguageSpecificData(info) + 1)
*)

function RtlAddFunctionTable(FunctionTable : PRUNTIME_FUNCTION; EntryCount : DWORD; BaseAddress: DWORD64) : BOOLEAN; external 'kernel32';
function RtlDeleteFunctionTable(FunctionTable : PRUNTIME_FUNCTION) : BOOLEAN; external 'kernel32';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ UNWIND_CODE ------------------
// ------------------

// GetUnwindOp
//
function UNWIND_CODE.GetUnwindOp : UNWIND_CODE_OPS;
begin
   Result := UNWIND_CODE_OPS(UnwindOp_OpInfo and $F);
end;

// SetUnwindOp
//
procedure UNWIND_CODE.SetUnwindOp(v : UNWIND_CODE_OPS);
begin
   UnwindOp_OpInfo := (UnwindOp_OpInfo and $F0) or (Byte(Ord(v)) and $0F);
end;

// GetOpInfo
//
function UNWIND_CODE.GetOpInfo : UBYTE;
begin
   Result := (UnwindOp_OpInfo shr 4) and $F;
end;

// SetOpInfo
//
procedure UNWIND_CODE.SetOpInfo(v : UBYTE);
begin
   UnwindOp_OpInfo := (UnwindOp_OpInfo and $F) or ((v shl 4) and $F0);
end;

// ------------------
// ------------------ UNWIND_CODE ------------------
// ------------------

// GetVersion
//
function UNWIND_INFO.GetVersion : UBYTE;
begin
   Result := (Version_Flags and $7);
end;

// SetVersion
//
procedure UNWIND_INFO.SetVersion(v : UBYTE);
begin
   Version_Flags := (Version_Flags and $F8) or (v and $7);
end;

// GetFlags
//
function UNWIND_INFO.GetFlags : UBYTE;
begin
   Result := (Version_Flags and $F8) shr 3;
end;

// SetFlags
//
procedure UNWIND_INFO.SetFlags(v : UBYTE);
begin
   Version_Flags := (Version_Flags and $7) or ((v shl 3) and $F8);
end;

// GetFrameRegister
//
function UNWIND_INFO.GetFrameRegister : UBYTE;
begin
   Result := (FrameRegister_FrameOffset and $F);
end;

// SetFrameRegister
//
procedure UNWIND_INFO.SetFrameRegister(v : UBYTE);
begin
   FrameRegister_FrameOffset := (FrameRegister_FrameOffset and $F0) or (v and $F);
end;

// GetFrameOffset
//
function UNWIND_INFO.GetFrameOffset : UBYTE;
begin
   Result := (FrameRegister_FrameOffset and $F0) shr 4;
end;

// SetFrameOffset
//
procedure UNWIND_INFO.SetFrameOffset(v : UBYTE);
begin
   FrameRegister_FrameOffset := (FrameRegister_FrameOffset and $F) or ((v shl 4) and $F0);
end;

end.
