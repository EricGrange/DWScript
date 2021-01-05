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

uses Types, SysUtils;

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
//      FrameOffset : USHORT;

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
   UNWIND_REG = (
      UNWIND_RAX = 0,   UNWIND_RCX = 1,   UNWIND_RDX = 2,   UNWIND_RBX = 3,
      UNWIND_RSP = 4,   UNWIND_RBP = 5,   UNWIND_RSI = 6,   UNWIND_RDI = 7,
      UNWIND_R8  = 8,   UNWIND_R9  = 9,   UNWIND_R10 = 10,  UNWIND_R11 = 11,
      UNWIND_R12 = 12,  UNWIND_R13 = 13,  UNWIND_R14 = 14,  UNWIND_R15 = 15
   );

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

      function GetFrameRegister : UNWIND_REG;
      procedure SetFrameRegister(v : UNWIND_REG);
      property FrameRegister : UNWIND_REG read GetFrameRegister write SetFrameRegister;
      function GetFrameOffset : UBYTE;
      procedure SetFrameOffset(v : UBYTE);
      property FrameOffset : UBYTE read GetFrameOffset write SetFrameOffset;

   end;
   UNWIND_INFO = _UNWIND_INFO;
   PUNWIND_INFO = ^UNWIND_INFO;

   // builds an UNWIND codes array
   // record ops in the prolog order
   TUnwindCodeBuilder = record
      private
         FCodes : array of UNWIND_CODE;

         procedure Insert(nb : Integer);

      public
         procedure PushNonVolatile(codeOffset : Byte; reg : UNWIND_REG);
         procedure AllocBytes(codeOffset : Byte; nbBytes : Integer);
         procedure AllocSlots(codeOffset : Byte; nb8BytesSlots : Integer);

         function SizeInOps : Integer; inline;
         function SizeInBytes : Integer;
         procedure CopyTo(dest : Pointer);
   end;

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

   TRUNTIME_FUNCTION_Helper = record helper for RUNTIME_FUNCTION
      function ToString(baseAddress : Pointer) : String;
   end;

const
   cUNWIND_REG_NAMES : array [UNWIND_REG] of String = (
      'RAX', 'RCX', 'RDX', 'RBX', 'RSP', 'RBP', 'RSI', 'RDI',
      'R8',  'R9',  'R10', 'R11', 'R12', 'R13', 'R14', 'R15'
   );

function RtlAddFunctionTable(FunctionTable : PRUNTIME_FUNCTION; EntryCount : DWORD; BaseAddress: Pointer) : BOOLEAN; external 'kernel32';
function RtlDeleteFunctionTable(FunctionTable : PRUNTIME_FUNCTION) : BOOLEAN; external 'kernel32';

function RtlLookupFunctionEntry(controlPc : Pointer; var imageBase : Pointer; historyTable : Pointer) : PRUNTIME_FUNCTION; external 'kernel32';

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

// ToString
//
function TRUNTIME_FUNCTION_Helper.ToString(baseAddress : Pointer) : String;
var
   pUnwind : PUNWIND_INFO;
   pCode : PUNWIND_CODE;
   n : Integer;
begin
   if (baseAddress = nil) or (@Self = nil) then
      Exit('No information available (nil pointer)');
   pUnwind := PUNWIND_INFO(IntPtr(baseAddress) + UnwindInfoAddress);
   Result := 'Base: ' + IntToHex(UInt64(baseAddress), 8) + #13#10
           + 'BeginAddress: 0x' + IntToHex(BeginAddress, 4)
                              + ' ('  + IntToHex(UInt64(baseAddress) + BeginAddress, 8) + ')' + #13#10
           + 'EndAddress: 0x' + IntToHex(EndAddress, 4)
                            + ' ('  + IntToHex(UInt64(baseAddress) + EndAddress, 8) + ')' + #13#10
           + 'Unwind version: ' + IntToStr(pUnwind.Version_Flags) + #13#10
           + 'Unwind flags: 0x' + IntToHex(pUnwind.Flags) + #13#10
           + 'Size of prologue: 0x' + IntToHex(pUnwind.SizeOfProlog) + #13#10
           + 'Count of codes: ' + IntToStr(pUnwind.CountOfCodes) + #13#10
           + 'Frame register: ';
   if pUnwind.FrameRegister <> UNWIND_RAX then begin
      Result := Result + cUNWIND_REG_NAMES[pUnwind.FrameRegister]
                       + ' (offset 0x' + IntToHex(pUnwind.FrameOffset*16, 2) + ')'#13#10;
   end else Result := Result + 'none'#13#10;
   n := pUnwind.CountOfCodes;
   if n > 0 then begin
      pCode := @pUnwind.UnwindCode[0];
      Result := Result + 'Unwind codes:'#13#10;
      while n > 0 do begin
         Result := Result + '  ' + IntToHex(pCode.CodeOffset, 2) + ': ';
         case pCode.UnwindOp of
            UWOP_PUSH_NONVOL : begin
               Result := Result + 'PUSH_NONVOL, register='
                                + cUNWIND_REG_NAMES[UNWIND_REG(pCode.OpInfo)] + #13#10;
            end;
            UWOP_ALLOC_SMALL : begin
               Result := Result + 'ALLOC_SMALL, size=0x' + IntToHex(pCode.OpInfo*8+8, 2) + #13#10;
            end;
            UWOP_ALLOC_LARGE : begin
               Result := Result + 'ALLOC_LARGE, size=0x';
               case pCode.OpInfo of
                  0 : begin
                     Inc(pCode); Dec(n);
                     Result := Result + IntToHex(PWORD(pCode)^*8, 4) + #13#10;
                  end;
                  1 : begin
                     Inc(pCode);
                     Result := Result + IntToHex(PCardinal(pCode)^, 4) + #13#10;
                     Inc(pCode);
                     Dec(n, 2);
                  end;
               else
                  Result := Result + '???'#13#10;
               end;
            end;
            UWOP_SET_FPREG : begin
               Result := Result + 'SET_FPREG, '
                                + cUNWIND_REG_NAMES[pUnwind.FrameRegister] + '=RSP+'
                                + IntToHex(pUnwind.FrameOffset*16, 2) + #13#10;
            end;
         else
            Result := Result + '??? 0x' + IntToHex(pCode.UnwindOp_OpInfo, 2) + #13#10
         end;
         Inc(pCode);
         Dec(n);
      end;
   end;
end;

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
function UNWIND_INFO.GetFrameRegister : UNWIND_REG;
begin
   Result := UNWIND_REG(FrameRegister_FrameOffset and $F);
end;

// SetFrameRegister
//
procedure UNWIND_INFO.SetFrameRegister(v : UNWIND_REG);
begin
   FrameRegister_FrameOffset := (FrameRegister_FrameOffset and $F0) or Byte(Ord(v) and $F);
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

// ------------------
// ------------------ TUnwindCodeBuilder ------------------
// ------------------

// Insert
//
procedure TUnwindCodeBuilder.Insert(nb : Integer);
var
   n : Integer;
begin
   n := Length(FCodes);
   SetLength(FCodes, n+nb);
   while n >= nb do begin
      FCodes[n] := FCodes[n-nb];
      Dec(n);
   end;
   FillChar(FCodes[0], nb*SizeOf(UNWIND_CODE), 0);
end;

// PushNonVolatile
//
procedure TUnwindCodeBuilder.PushNonVolatile(codeOffset : Byte; reg : UNWIND_REG);
begin
   Insert(1);
   FCodes[0].CodeOffset := codeOffset;
   FCodes[0].UnwindOp_OpInfo := Ord(UWOP_PUSH_NONVOL) + (Ord(reg) shl 4);
end;

// AllocBytes
//
procedure TUnwindCodeBuilder.AllocBytes(codeOffset : Byte; nbBytes : Integer);
begin
   Assert((nbBytes and 7) = 0, 'AllocBytes should be a multiple of 8');
   AllocSlots(codeOffset, nbBytes shr 3);
end;

// AllocSlots
//
procedure TUnwindCodeBuilder.AllocSlots(codeOffset : Byte; nb8BytesSlots : Integer);
begin
   Assert(nb8BytesSlots > 0);
   case nb8BytesSlots of
      1..16 : begin
         Insert(1);
         FCodes[0].CodeOffset := codeOffset;
         FCodes[0].UnwindOp_OpInfo := Ord(UWOP_ALLOC_SMALL) + ((nb8BytesSlots-1) shl 4);
      end;
      17 .. (512*1024 div 8)-1 : begin
         Insert(2);
         FCodes[0].CodeOffset := codeOffset;
         FCodes[0].UnwindOp_OpInfo := Ord(UWOP_ALLOC_LARGE);
         PWord(@FCodes[1])^ := nb8BytesSlots;
      end;
   else
      Insert(3);
      FCodes[0].CodeOffset := codeOffset;
      FCodes[0].UnwindOp_OpInfo := Ord(UWOP_ALLOC_LARGE) + 1;
      PCardinal(@FCodes[1])^ := nb8BytesSlots;
   end;
end;

// SizeInOps
//
function TUnwindCodeBuilder.SizeInOps : Integer;
begin
   Result := Length(FCodes);
end;

// SizeInBytes
//
function TUnwindCodeBuilder.SizeInBytes : Integer;
begin
   Result := SizeInOps*SizeOf(UNWIND_CODE);
end;

// CopyTo
//
procedure TUnwindCodeBuilder.CopyTo(dest : Pointer);
var
   n : Integer;
begin
   n := SizeInBytes;
   if n > 0 then
      System.Move(FCodes[0], dest^, n);
end;

end.
