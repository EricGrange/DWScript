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
unit dwsJITx86Intrinsics;

{$I ../dws.inc}

interface

uses
   Types,
   dwsUtils;

type
   TxmmRegister = (
      xmmNone = -1,
      xmm0 = 0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7
   );

   TxmmOp = (
      xmm_cvtsi2sd   = $2A,
      xmm_sqrtsd     = $51,
      xmm_addsd      = $58,
      xmm_multsd     = $59,
      xmm_subsd      = $5C,
      xmm_minsd      = $5D,
      xmm_divsd      = $5E,
      xmm_maxsd      = $5F
   );

   TgpRegister = (
      gprEAX = 0,
      gprECX = 1,
      gprEDX = 2,
      gprEBX = 3,
      gprESP = 4,
      gprEBP = 5,
      gprESI = 6,
      gprEDI = 7
   );

   TboolFlags = (
      flagsNone = 0,
      flagsA = $77,     // if above (CF=0 and ZF=0)
      flagsAE = $73,    // if above or equal (CF=0)
      flagsB = $72,     // if below (CF=1)
      flagsBE = $76,    // if below or equal (CF=1 or ZF=1)
      flagsC = $72,     // if carry (CF=1)
      flagsE = $74,     // if equal (ZF=1)
      flagsG = $7F,     // if greater (ZF=0 and SF=OF)
      flagsGE = $7D,    // if greater or equal (SF=OF)
      flagsL = $7C,     // if less (SF<>OF)
      flagsLE = $7E,    // if less or equal (ZF=1 or SF<>OF)
      flagsNA = $76,    // if not above (CF=1 or ZF=1)
      flagsNAE = $72,   // if not above or equal (CF=1)
      flagsNB = $73,    // if not below (CF=0)
      flagsNBE = $77,   // if not below or equal (CF=0 and ZF=0)
      flagsNC = $73,    // if not carry (CF=0)
      flagsNE = $75,    // if not equal (ZF=0)
      flagsNG = $7E,    // if not greater (ZF=1 or SF<>OF)
      flagsNGE = $7C,   // if not greater or equal (SF<>OF)
      flagsNL = $7D,    // if not less (SF=OF)
      flagsNLE = $7F,   // if not less or equal (ZF=0 and SF=OF)
      flagsNO = $71,    // if not overflow (OF=0)
      flagsNP = $7B,    // if not parity (PF=0)
      flagsNS = $79,    // if not sign (SF=0)
      flagsNZ = $75,    // if not zero (ZF=0)
      flagsO = $70,     // if overflow (OF=1)
      flagsP = $7A,     // if parity (PF=1)
      flagsPE = $7A,    // if parity even (PF=1)
      flagsPO = $7B,    // if parity odd (PF=0)
      flagsS = $78,     // if sign (SF=1)
      flagsZ = $74      // if zero (ZF = 1)
   );

   Tx86WriteOnlyStream = class(TWriteOnlyBlockStream)
      private
         procedure _modRMSIB_reg_reg(const opCode : array of Byte; dest, src : TxmmRegister);
         procedure _modRMSIB_reg_bpmem(const opCode : array of Byte; reg : TxmmRegister; stackAddr : Integer);
         procedure _modRMSIB_reg_absmem(const opCode : array of Byte; reg : TxmmRegister; ptr : Pointer);

      public
         procedure WritePointer(const p : Pointer);

         procedure _xmm_reg_reg(op : TxmmOp; dest, src : TxmmRegister);
         procedure _xmm_reg_bpmem(op : TxmmOp; reg : TxmmRegister; stackAddr : Integer);
         procedure _xmm_reg_absmem(op : TxmmOp; reg : TxmmRegister; ptr : Pointer);

         procedure _xorps_reg_reg(dest, src : TxmmRegister);

         procedure _comisd_reg_reg(dest, src : TxmmRegister);
         procedure _comisd_reg_bpmem(reg : TxmmRegister; stackAddr : Integer);
         procedure _comisd_reg_absmem(reg : TxmmRegister;  ptr : Pointer);

         procedure _movsd_reg_bpmem(reg : TxmmRegister; stackAddr : Integer);
         procedure _movsd_bpmem_reg(stackAddr : Integer; reg : TxmmRegister);
         procedure _movsd_reg_absmem(reg : TxmmRegister; ptr : Pointer);
         procedure _movsd_reg_esp(reg : TxmmRegister);
         procedure _movsd_esp_reg(reg : TxmmRegister);

         procedure _movq_bpmem_reg(stackAddr : Integer; reg : TxmmRegister);
         procedure _movq_reg_absmem(reg : TxmmRegister; ptr : Pointer);

         procedure _mov_eaxedx_bpmem(stackAddr : Integer);
         procedure _mov_bpmem_eaxedx(stackAddr : Integer);
         procedure _mov_bpmem_imm(stackAddr : Integer; const imm : Int64);
         procedure _mov_eaxedx_imm(const imm : Int64);
         procedure _inc_eaxedx_imm(const imm : Int64);

         procedure _cmp_bpmem_dword(stackAddr, offset : Integer; value : DWORD);

         procedure _set_al_flags(flags : TboolFlags);

         procedure _int32_inc(stackAddr, offset : Integer);
         procedure _int64_inc(stackAddr : Integer);

         procedure _fild_bpmem(stackAddr : Integer);
         procedure _fld_esp;
         procedure _fstp_esp;

         procedure _push_reg(reg : TgpRegister);
         procedure _pop_reg(reg : TgpRegister);

         procedure _mov_reg_reg(dest, src : TgpRegister);
         procedure _mov_reg_dword(reg : TgpRegister; imm : DWORD);

         procedure _nop(nb : Integer);
         procedure _ret;
   end;

const
   cStackMixinBaseDataOffset = 8;

function NegateBoolFlags(flags : TboolFlags) : TboolFlags;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// StackAddrToOffset
//
function StackAddrToOffset(addr : Integer) : Integer;
begin
   Result:=addr*SizeOf(Variant)+8;
end;

// NegateBoolFlags
//
function NegateBoolFlags(flags : TboolFlags) : TboolFlags;
begin
   case flags of
      flagsA : Result:=flagsNA;
      flagsAE : Result:=flagsNAE;
      flagsB : Result:=flagsNB;
      flagsBE : Result:=flagsNBE;
      flagsE : Result:=flagsNE;
      flagsG : Result:=flagsNG;
      flagsGE : Result:=flagsNGE;
      flagsL : Result:=flagsNL;
      flagsLE : Result:=flagsNLE;
      flagsNE : Result:=flagsE;
      flagsNO : Result:=flagsO;
      flagsNP : Result:=flagsP;
      flagsNS : Result:=flagsS;
      flagsO : Result:=flagsNO;
      flagsP : Result:=flagsNP;
      flagsS : Result:=flagsNS;
   else
      Result:=flags;
   end;
end;

// ------------------
// ------------------ Tx86WriteOnlyStream ------------------
// ------------------

// _modRMSIB_reg_reg
//
procedure Tx86WriteOnlyStream._modRMSIB_reg_reg(const opCode : array of Byte; dest, src : TxmmRegister);
begin
   Assert(dest in [xmm0..High(TxmmRegister)]);
   Assert(src in [xmm0..High(TxmmRegister)]);

   WriteBytes(opCode);

   WriteByte($C0+Ord(src)+Ord(dest)*8);
end;

// _modRMSIB_reg_bpmem
//
procedure Tx86WriteOnlyStream._modRMSIB_reg_bpmem(const opCode : array of Byte; reg : TxmmRegister; stackAddr : Integer);
var
   offset : Integer;
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   WriteBytes(opCode);

   offset:=StackAddrToOffset(stackAddr);

   if (offset>=-128) and (offset<=127) then begin
      WriteByte($45+Ord(reg)*8);
      WriteByte(offset);
   end else begin
      WriteByte($85+Ord(reg)*8);
      WriteInt32(offset);
   end;
end;

// _modRMSIB_reg_absmem
//
procedure Tx86WriteOnlyStream._modRMSIB_reg_absmem(const opCode : array of Byte; reg : TxmmRegister; ptr : Pointer);
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   WriteBytes(opCode);

   WriteByte($05+Ord(reg)*8);
   WritePointer(ptr);
end;

// WritePointer
//
procedure Tx86WriteOnlyStream.WritePointer(const p : Pointer);
begin
   Write(p, 4);
end;

// _xmm_reg_reg
//
procedure Tx86WriteOnlyStream._xmm_reg_reg(op : TxmmOp; dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$F2, $0F, Ord(op)], dest, src);
end;

// _xmm_reg_bpmem
//
procedure Tx86WriteOnlyStream._xmm_reg_bpmem(op : TxmmOp; reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_bpmem([$F2, $0F, Ord(op)], reg, stackAddr);
end;

// _xmm_reg_absmem
//
procedure Tx86WriteOnlyStream._xmm_reg_absmem(op : TxmmOp; reg : TxmmRegister;  ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$F2, $0F, Ord(op)], reg, ptr);
end;

// _xorps_reg_reg
//
procedure Tx86WriteOnlyStream._xorps_reg_reg(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$0F, $57], dest, src);
end;

// _comisd_reg_reg
//
procedure Tx86WriteOnlyStream._comisd_reg_reg(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$66, $0F, $2F], dest, src);
end;

// _comisd_reg_bpmem
//
procedure Tx86WriteOnlyStream._comisd_reg_bpmem(reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_bpmem([$66, $0F, $2F], reg, stackAddr);
end;

// _comisd_reg_absmem
//
procedure Tx86WriteOnlyStream._comisd_reg_absmem(reg : TxmmRegister;  ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$66, $0F, $2F], reg, ptr);
end;

// _movsd_reg_bpmem
//
procedure Tx86WriteOnlyStream._movsd_reg_bpmem(reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_bpmem([$F2, $0F, $10], reg, stackAddr);
end;

// _movsd_bpmem_reg
//
procedure Tx86WriteOnlyStream._movsd_bpmem_reg(stackAddr : Integer; reg : TxmmRegister);
begin
   _modRMSIB_reg_bpmem([$F2, $0F, $11], reg, stackAddr);
end;

// _movsd_reg_absmem
//
procedure Tx86WriteOnlyStream._movsd_reg_absmem(reg : TxmmRegister; ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$F2, $0F, $10], reg, ptr);
end;

// _movsd_reg_esp
//
procedure Tx86WriteOnlyStream._movsd_reg_esp(reg : TxmmRegister);
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   WriteBytes([$F2, $0F, $10, $04+8*Ord(reg), $24]);
end;

// _movsd_esp_reg
//
procedure Tx86WriteOnlyStream._movsd_esp_reg(reg : TxmmRegister);
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   WriteBytes([$F2, $0F, $11, $04+8*Ord(reg), $24]);
end;

// _movq_bpmem_reg
//
procedure Tx86WriteOnlyStream._movq_bpmem_reg(stackAddr : Integer; reg : TxmmRegister);
begin
   _modRMSIB_reg_bpmem([$66, $0F, $D6], reg, stackAddr);
end;

// _movq_reg_absmem
//
procedure Tx86WriteOnlyStream._movq_reg_absmem(reg : TxmmRegister; ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$F3, $0F, $7E], reg, ptr);
end;

// _mov_eaxedx_bpmem
//
procedure Tx86WriteOnlyStream._mov_eaxedx_bpmem(stackAddr : Integer);
var
   offset : Integer;
begin
   offset:=StackAddrToOffset(stackAddr);

   if Abs(offset)<=120 then begin

      // mov eax, [ebp + offset ]
      WriteBytes([$8B, $45, offset]);
      // mov edx, [ebp + offset + 4 ]
      WriteBytes([$8B, $55, offset+4]);

   end else begin

      // mov eax, [ebp + offset ]
      WriteBytes([$8B, $85]);
      WriteInt32(offset);
      // mov edx, [ebp + offset + 4 ]
      WriteBytes([$8B, $95]);
      WriteInt32(offset+4);

   end;
end;

// _mov_bpmem_eaxedx
//
procedure Tx86WriteOnlyStream._mov_bpmem_eaxedx(stackAddr : Integer);
var
   offset : Integer;
begin
   offset:=StackAddrToOffset(stackAddr);

   if Abs(offset)<=120 then begin

      // mov [ebp + offset ], eax
      WriteBytes([$89, $45, offset]);
      // mov [ebp + offset + 4 ], edx
      WriteBytes([$89, $55, offset+4]);

   end else begin

      // mov [ebp + offset ], eax
      WriteBytes([$89, $85]);
      WriteInt32(offset);
      // mov [bp + offset + 4 ], edx
      WriteBytes([$89, $95]);
      WriteInt32(offset+4);

   end;
end;

// _mov_bpmem_imm
//
procedure Tx86WriteOnlyStream._mov_bpmem_imm(stackAddr : Integer; const imm : Int64);
var
   offset : Integer;
begin
   offset:=StackAddrToOffset(stackAddr);

   if imm=0 then begin

      // xor eax, eax
      WriteBytes([$31, $C0]);

      if Abs(offset)<=120 then begin
         // mov [ebp + offset ], eax
         WriteBytes([$89, $45, offset]);
         WriteBytes([$89, $45, offset+4]);
      end else begin
         // mov [ebp + offset ], eax
         WriteBytes([$89, $85]);
         WriteInt32(offset);
         WriteBytes([$89, $85]);
         WriteInt32(offset+4);
      end;

   end else begin

      _mov_eaxedx_imm(imm);
      _mov_bpmem_eaxedx(stackAddr);

   end;
end;

// _mov_eaxedx_imm
//
procedure Tx86WriteOnlyStream._mov_eaxedx_imm(const imm : Int64);
begin
   _mov_reg_dword(gprEAX, DWORD(imm));
   _mov_reg_dword(gprEDX, DWORD(imm shr 32));
end;

// _inc_eaxedx_imm
//
procedure Tx86WriteOnlyStream._inc_eaxedx_imm(const imm : Int64);
var
   lo, hi : DWORD;
begin
   lo:=DWORD(imm);
   hi:=DWORD(imm shr 32);

   // add eax, lo
   if lo<256 then
      WriteBytes([$83, $C0, lo])
   else begin
      WriteBytes([$05]);
      WriteDWord(lo);
   end;

   // adc edx, hi
   if hi<256 then
      WriteBytes([$83, $D2, hi])
   else begin
      WriteBytes([$81, $D2]);
      WriteDWord(hi);
   end;
end;

// _cmp_bpmem_dword
//
procedure Tx86WriteOnlyStream._cmp_bpmem_dword(stackAddr, offset : Integer; value : DWORD);
begin
   offset:=StackAddrToOffset(stackAddr)+offset;

   if (offset>=-128) and (offset<=127) then begin

      if value<=$FF then begin
         WriteBytes([$83, $7D, offset, value]);
      end else begin
         WriteBytes([$81, $7D, offset]);
         WriteInt32(value);
      end;

   end else begin

      if value<=$FF then begin
         WriteBytes([$83, $BD]);
         WriteInt32(offset);
         WriteBytes([value]);
      end else begin
         WriteBytes([$81, $BD]);
         WriteInt32(offset);
         WriteInt32(value);
      end;

   end;
end;

// _set_al_flags
//
procedure Tx86WriteOnlyStream._set_al_flags(flags : TboolFlags);
begin
   WriteBytes([$0F, Ord(flags)+$20, $C0]);
end;

// _int32_inc
//
procedure Tx86WriteOnlyStream._int32_inc(stackAddr, offset : Integer);
begin
   offset:=StackAddrToOffset(stackAddr)+offset;

   if (offset>=-128) and (offset<=127) then begin

      WriteBytes([$83, $45, offset, 1]);   // add dword ptr [esp+offset], 1

   end else begin

      WriteBytes([$83, $85]);
      WriteInt32(offset);
      WriteBytes([1]);

   end;
end;

// _int64_inc
//
procedure Tx86WriteOnlyStream._int64_inc(stackAddr : Integer);
var
   offset : Integer;
begin
   _int32_inc(stackAddr, 0);

   offset:=StackAddrToOffset(stackAddr)+4;

   if (offset>=-128) and (offset<=127) then begin

      // adc dword ptr [esp+offset], 0
      WriteBytes([$83, $55, offset, 0]);

   end else begin

      WriteBytes([$83, $95]);
      WriteInt32(offset);
      WriteBytes([0]);

   end;
end;

// _fild_bpmem
//
procedure Tx86WriteOnlyStream._fild_bpmem(stackAddr : Integer);
var
   offset : Integer;
begin
   offset:=StackAddrToOffset(stackAddr);

   if (offset>=-128) and (offset<=127) then

      WriteBytes([$DF, $6D, offset])

   else begin

      WriteBytes([$DF, $AD]);
      WriteInt32(offset);

   end;
end;

// _fld_esp
//
procedure Tx86WriteOnlyStream._fld_esp;
begin
   // fld qword ptr [esp]
   WriteBytes([$DD, $04, $24]);
end;

// _fstp_esp
//
procedure Tx86WriteOnlyStream._fstp_esp;
begin
   // fstp qword ptr [esp]
   WriteBytes([$DD, $1C, $24]);
end;

// _push_reg
//
procedure Tx86WriteOnlyStream._push_reg(reg : TgpRegister);
begin
   Assert(reg in [gprEAX..gprEDI]);

   WriteBytes([$50+Ord(reg)]);
end;

// _pop_reg
//
procedure Tx86WriteOnlyStream._pop_reg(reg : TgpRegister);
begin
   Assert(reg in [gprEAX..gprEDI]);

   WriteBytes([$58+Ord(reg)]);
end;

// _mov_reg_reg
//
procedure Tx86WriteOnlyStream._mov_reg_reg(dest, src : TgpRegister);
begin
   WriteBytes([$89, $C0+Ord(dest)+8*Ord(src)]);
end;

// _mov_reg_dword
//
procedure Tx86WriteOnlyStream._mov_reg_dword(reg : TgpRegister; imm : DWORD);
begin
   if imm=0 then begin
      // xor reg, reg
      WriteBytes([$31, $C0+Ord(reg)*9]);
   end else begin
      WriteBytes([$B8+Ord(reg)]);
      WriteDWord(imm);
   end;
end;

// _nop
//
procedure Tx86WriteOnlyStream._nop(nb : Integer);
begin
   while nb>=3 do begin
      WriteBytes([$66, $66, $90]);
      Dec(nb, 3);
   end;
   case nb of
      1 : WriteBytes([$90]);
      2 : WriteBytes([$66, $90]);
   end;
end;

// _ret
//
procedure Tx86WriteOnlyStream._ret;
begin
   WriteBytes([$C3]);
end;

end.
