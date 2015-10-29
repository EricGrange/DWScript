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
   Types, SysUtils,
   dwsDataContext,
   dwsUtils;

type
   TxmmRegister = (
      xmmNone = -1,
      xmm0 = 0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7
   );

   TxmmRegisters = set of xmm0..xmm7;

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

   TgpOp = packed record
      Short1, SIB : Byte;
      Long1 : Byte;
      LongEAX : Byte;
      RegReg : Byte;
      RegRegHigh : Byte;
   end;

   TgpShift = (gpShr = $E8, gpShl = $E0, gpSar = $F8, gpSal = $F0);

   Tx86WriteOnlyStream = class(TWriteOnlyBlockStream)
      private
         procedure _modRMSIB_reg_reg(const opCode : array of Byte; dest, src : TxmmRegister);
         procedure _modRMSIB_reg_execmem(const opCode : array of Byte; reg : TxmmRegister; stackAddr : Integer); overload;
         procedure _modRMSIB_reg_execmem(const opCode : array of Byte; reg : TgpRegister; stackAddr, offset : Integer); overload;
         procedure _modRMSIB_reg_absmem(const opCode : array of Byte; reg : TxmmRegister; ptr : Pointer);
         procedure _modRMSIB_op_execmem_int32(code1, code2 : Byte; stackAddr, offset, value : Integer);
         procedure _modRMSIB_regnum_ptr_reg(const opCode : array of Byte; destNum : Integer; src : TgpRegister; offset : Integer);
         procedure _modRMSIB_ptr_reg(rm : Integer; reg : TgpRegister; offset : Integer);
         procedure _modRMSIB_ptr_reg_reg(rm : Integer; base, index : TgpRegister; scale, offset : Integer);

      public
         procedure WritePointer(const p : Pointer);

         procedure _xmm_reg_reg(op : TxmmOp; dest, src : TxmmRegister);
         procedure _xmm_reg_execmem(op : TxmmOp; reg : TxmmRegister; stackAddr : Integer);
         procedure _xmm_reg_absmem(op : TxmmOp; reg : TxmmRegister; ptr : Pointer);

         procedure _xorps_reg_reg(dest, src : TxmmRegister);

         procedure _comisd_reg_reg(dest, src : TxmmRegister);
         procedure _comisd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
         procedure _comisd_reg_absmem(reg : TxmmRegister;  ptr : Pointer);

         procedure _movsd_reg_reg(dest, src : TxmmRegister);
         procedure _movsd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
         procedure _movsd_execmem_reg(stackAddr : Integer; reg : TxmmRegister);
         procedure _movsd_reg_absmem(reg : TxmmRegister; ptr : Pointer);
         procedure _movsd_reg_esp(reg : TxmmRegister; offset : Integer = 0);
         procedure _movsd_esp_reg(reg : TxmmRegister); overload;
         procedure _movsd_esp_reg(offset : Integer; reg : TxmmRegister); overload;
         procedure _movsd_qword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TxmmRegister);
         procedure _movsd_reg_qword_ptr_reg(dest : TxmmRegister; src : TgpRegister; offset : Integer);
         procedure _movsd_reg_qword_ptr_indexed(dest : TxmmRegister; base, index : TgpRegister; scale, offset : Integer);
         procedure _movsd_qword_ptr_indexed_reg(base, index : TgpRegister; scale, offset : Integer; src : TxmmRegister);

         procedure _movq_execmem_reg(stackAddr : Integer; reg : TxmmRegister);
         procedure _movq_reg_absmem(reg : TxmmRegister; ptr : Pointer);

         procedure _mov_reg_execmem(reg : TgpRegister; stackAddr : Integer; offset : Integer = 0);
         procedure _mov_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
         procedure _mov_eaxedx_execmem(stackAddr : Integer);
         procedure _mov_execmem_eaxedx(stackAddr : Integer);
         procedure _mov_execmem_imm(stackAddr : Integer; const imm : Int64);
         procedure _mov_eaxedx_imm(const imm : Int64);

         procedure _mov_reg_reg(dest, src : TgpRegister);
         procedure _mov_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer = 0);
         procedure _mov_reg_dword_ptr_indexed(dest, base, index : TgpRegister; scale, offset : Integer);
         procedure _mov_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TgpRegister);
         procedure _mov_qword_ptr_reg_eaxedx(dest : TgpRegister; offset : Integer);
         procedure _mov_eaxedx_qword_ptr_reg(src : TgpRegister; offset : Integer);
         procedure _mov_reg_dword(reg : TgpRegister; imm : DWORD);

         procedure _add_eaxedx_imm(const imm : Int64);
         procedure _add_eaxedx_execmem(stackAddr : Integer);
         procedure _sub_eaxedx_imm(const imm : Int64);
         procedure _sub_eaxedx_execmem(stackAddr : Integer);

         procedure _cmp_execmem_int32(stackAddr, offset, value : Integer);
         procedure _cmp_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
         procedure _cmp_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
         procedure _cmp_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; reg : TgpRegister);
         procedure _cmp_reg_dword_ptr_reg(reg : TgpRegister; dest : TgpRegister; offset : Integer);

         procedure _test_reg_reg(dest, src : TgpRegister);
         procedure _test_reg_imm(reg : TgpRegister; imm : DWORD);
         procedure _test_dword_ptr_reg_dword(dest : TgpRegister; offset : Integer; imm : DWORD);
         procedure _test_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; imm : Byte);
         procedure _test_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TgpRegister);
         procedure _test_execmem_imm(stackAddr, offset : Integer; imm : DWORD);
         procedure _test_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);

         procedure _set_al_flags(flags : TboolFlags);

         procedure _op_reg_int32(const op : TgpOP; reg : TgpRegister; value : Integer);
         procedure _op_reg_reg(const op : TgpOP; dest, src : TgpRegister);
         procedure _op_reg_dword_ptr_reg(const op : TgpOP; dest, src : TgpRegister; offset : Integer);

         procedure _cmp_reg_int32(reg : TgpRegister; value : Integer);
         procedure _add_reg_reg(dest, src : TgpRegister);
         procedure _adc_reg_reg(dest, src : TgpRegister);
         procedure _add_reg_int32(reg : TgpRegister; value : Integer);
         procedure _adc_reg_int32(reg : TgpRegister; value : Integer);
         procedure _sub_reg_int32(reg : TgpRegister; value : Integer);
         procedure _sbb_reg_int32(reg : TgpRegister; value : Integer);
         procedure _add_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
         procedure _adc_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
         procedure _sub_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
         procedure _sbb_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
         procedure _add_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
         procedure _adc_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
         procedure _sub_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
         procedure _sbb_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
         procedure _and_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
         procedure _or_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
         procedure _xor_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);

         procedure _inc_dword_ptr_reg(reg : TgpRegister; offset : Integer);

         procedure _mul_reg(reg : TgpRegister);
         procedure _mul_dword_ptr_reg(reg : TgpRegister; offset : Integer);
         procedure _imul_reg_reg(dest, src : TgpRegister);
         procedure _imul_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);

         procedure _neg_reg(reg : TgpRegister);
         procedure _not_reg(reg : TgpRegister);
         procedure _neg_eaxedx;

         procedure _shift_reg_imm(shift : TgpShift; reg : TgpRegister; value : Integer);
         procedure _shift_reg_cl(shift : TgpShift; reg : TgpRegister);

         procedure _shr_eaxedx_imm(value : Integer);
         procedure _shl_eaxedx_imm(value : Integer);
         procedure _sar_eaxedx_imm(value : Integer);
         procedure _shr_eaxedx_cl;
         procedure _shl_eaxedx_cl;
         procedure _sar_eaxedx_cl;

         procedure _add_execmem_int32(stackAddr, offset, value : Integer);
         procedure _adc_execmem_int32(stackAddr, offset, value : Integer);
         procedure _sub_execmem_int32(stackAddr, offset, value : Integer);
         procedure _sbb_execmem_int32(stackAddr, offset, value : Integer);
         procedure _add_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
         procedure _adc_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
         procedure _sub_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
         procedure _sbb_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);

         procedure _and_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; value : Byte);
         procedure _or_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; value : Byte);

         procedure _execmem32_inc(stackAddr : Integer; const imm : Int32);
         procedure _execmem64_inc(stackAddr : Integer; const imm : Int64);
         procedure _execmem64_dec(stackAddr : Integer; const imm : Int64);

         procedure _fild_execmem(stackAddr : Integer);
         procedure _fild_esp;
         procedure _fistp_esp;
         procedure _fld_esp;
         procedure _fstp_esp;
         procedure _ffree(n : Integer);

         procedure _push_reg(reg : TgpRegister);
         procedure _pop_reg(reg : TgpRegister);

         procedure _xor_reg_reg(dest, src : TgpRegister);

         procedure _call_reg(reg : TgpRegister; offset : Integer);
         procedure _call_absmem(ptr : Pointer);

         procedure _test_al_al;
         procedure _nop(nb : Integer);
         procedure _ret;

         //at result, insert literal address of return value from _end_finally_block, *minus 1*
         function  _begin_tryf_frame: integer;

          //at result, insert literal address of code after _end_finally_block
         function  _begin_finally_block: integer;

          //must pass return value of _begin_finally_block
          //at result, insert relative address to @HandleFinally
         function _end_finally_block(beginResult: integer): integer;
   end;

const
   gpOp_add : TgpOp = (Short1: $83; SIB: $C0; Long1: $81; LongEAX: $05; RegReg: $01);
   gpOp_adc : TgpOp = (Short1: $83; SIB: $D0; Long1: $81; LongEAX: $15; RegReg: $11);
   gpOp_sub : TgpOp = (Short1: $83; SIB: $E8; Long1: $81; LongEAX: $2D; RegReg: $29);
   gpOp_sbb : TgpOp = (Short1: $83; SIB: $D8; Long1: $81; LongEAX: $1D; RegReg: $19);
   gpOp_xor : TgpOp = (Short1: $83; SIB: $F0; Long1: $81; LongEAX: $35; RegReg: $31);
   gpOp_and : TgpOp = (Short1: $83; SIB: $E0; Long1: $81; LongEAX: $25; RegReg: $21);
   gpOp_or  : TgpOp = (Short1: $83; SIB: $C8; Long1: $81; LongEAX: $0D; RegReg: $09);
   gpOp_cmp : TgpOp = (Short1: $83; SIB: $F8; Long1: $81; LongEAX: $3D; RegReg: $39);

   cStackMixinBaseDataOffset = 8;
   cVariant_DataOffset = 8;
   cgpRegisterName : array [TgpRegister] of String = (
      'eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi'
      );
   cgpRegister8bitName : array [TgpRegister] of String = (
      'al', 'cl', 'dl', 'bl', '??', '??', '??', '??'
      );
   cExecMemGPR = gprEBX;

function NegateBoolFlags(flags : TboolFlags) : TboolFlags;
function StackAddrToOffset(addr : Integer) : Integer;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R-}

// StackAddrToOffset
//
function StackAddrToOffset(addr : Integer) : Integer;
begin
   Result:=addr*SizeOf(Variant)+cVariant_DataOffset;
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

// _modRMSIB_reg_execmem (xmm)
//
procedure Tx86WriteOnlyStream._modRMSIB_reg_execmem(const opCode : array of Byte; reg : TxmmRegister; stackAddr : Integer);
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg(opCode, Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr));
end;

// _modRMSIB_reg_execmem (gpr)
//
procedure Tx86WriteOnlyStream._modRMSIB_reg_execmem(const opCode : array of Byte; reg : TgpRegister; stackAddr, offset : Integer);
begin
   Assert(reg in [gprEAX..High(gprEDI)]);

   _modRMSIB_regnum_ptr_reg(opCode, Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
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

// _modRMSIB_op_execmem_int32
//
procedure Tx86WriteOnlyStream._modRMSIB_op_execmem_int32(code1, code2 : Byte; stackAddr, offset, value : Integer);
begin
   offset:=StackAddrToOffset(stackAddr)+offset;

   Inc(code2, Ord(cExecMemGPR));

   if (offset>=-128) and (offset<=127) then begin

      if (value>=-128) and (value<=127) then
         WriteBytes([code1, code2, Byte(offset), Byte(value)])
      else begin
         WriteBytes([code1-2, code2, offset]);
         WriteInt32(value);
      end;

   end else begin

      if (value>=-128) and (value<=127) then begin
         WriteBytes([code1, code2+$40]);
         WriteInt32(offset);
         WriteByte(Byte(value));
      end else begin
         WriteBytes([code1-2, code2+$40]);
         WriteInt32(offset);
         WriteInt32(value);
      end;

   end;
end;

// _modRMSIB_regnum_ptr_reg
//
procedure Tx86WriteOnlyStream._modRMSIB_regnum_ptr_reg(const opCode : array of Byte; destNum : Integer; src : TgpRegister; offset : Integer);
begin
   WriteBytes(opCode);
   _modRMSIB_ptr_reg(destNum*8, src, offset)
end;

// _modRMSIB_ptr_reg
//
procedure Tx86WriteOnlyStream._modRMSIB_ptr_reg(rm : Integer; reg : TgpRegister; offset : Integer);
begin
   Inc(rm, Ord(reg));
   if (offset<>0) or (reg=gprEBP) then begin
      if (offset>=-128) and (offset<=127) then
         Inc(rm, $40)
      else Inc(rm, $80);
   end;
   WriteByte(rm);
   if reg=gprESP then
      WriteByte($24);
   if (rm and $40)<>0 then
      WriteByte(Byte(offset))
   else if (rm and $80)<>0 then
      WriteInt32(offset);
end;

// _modRMSIB_ptr_reg_reg
//
procedure Tx86WriteOnlyStream._modRMSIB_ptr_reg_reg(rm : Integer; base, index : TgpRegister; scale, offset : Integer);
var
   sib : Integer;
begin
   Assert(scale in [1, 2, 4, 8]);

   if (index=gprESP) and (base<>gprESP) then begin
      Assert(scale=1);
      _modRMSIB_ptr_reg_reg(rm, index, base, 1, offset);
   end;

   if (offset=0) and (base<>gprEBP) then
      Inc(rm, $04)
   else if (offset>=-128) and (offset<127) then
      Inc(rm, $44)
   else Inc(rm, $84);
   WriteByte(rm);

   sib:=Ord(index)*8+Ord(base);
   case scale of
      2 : Inc(sib, $40);
      4 : Inc(sib, $80);
      8 : Inc(sib, $C0);
   end;
   WriteByte(sib);

   if (rm and $40)<>0 then
      WriteByte(Byte(offset))
   else if (rm and $80)<>0 then
      WriteInt32(offset);
end;

// WritePointer
//
procedure Tx86WriteOnlyStream.WritePointer(const p : Pointer);
var
   ip : NativeInt;
begin
   // use local variable to work around XE3/XE4 bug (QC #115212)
   ip:=NativeInt(p);
   Write(ip, SizeOf(NativeInt));
end;

// _xmm_reg_reg
//
procedure Tx86WriteOnlyStream._xmm_reg_reg(op : TxmmOp; dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$F2, $0F, Ord(op)], dest, src);
end;

// _xmm_reg_execmem
//
procedure Tx86WriteOnlyStream._xmm_reg_execmem(op : TxmmOp; reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_execmem([$F2, $0F, Ord(op)], reg, stackAddr);
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

// _comisd_reg_execmem
//
procedure Tx86WriteOnlyStream._comisd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_execmem([$66, $0F, $2F], reg, stackAddr);
end;

// _comisd_reg_absmem
//
procedure Tx86WriteOnlyStream._comisd_reg_absmem(reg : TxmmRegister;  ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$66, $0F, $2F], reg, ptr);
end;

// _movsd_reg_reg
//
procedure Tx86WriteOnlyStream._movsd_reg_reg(dest, src : TxmmRegister);
begin
   // really does a movaps to copy registers (shorter encoding)
   _modRMSIB_reg_reg([$0F, $28], dest, src);
end;

// _movsd_reg_execmem
//
procedure Tx86WriteOnlyStream._movsd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_execmem([$F2, $0F, $10], reg, stackAddr);
end;

// _movsd_execmem_reg
//
procedure Tx86WriteOnlyStream._movsd_execmem_reg(stackAddr : Integer; reg : TxmmRegister);
begin
   _modRMSIB_reg_execmem([$F2, $0F, $11], reg, stackAddr);
end;

// _movsd_reg_absmem
//
procedure Tx86WriteOnlyStream._movsd_reg_absmem(reg : TxmmRegister; ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$F2, $0F, $10], reg, ptr);
end;

// _movsd_reg_esp
//
procedure Tx86WriteOnlyStream._movsd_reg_esp(reg : TxmmRegister; offset : Integer = 0);
begin
   _movsd_reg_qword_ptr_reg(reg, gprESP, offset);
end;

// _movsd_esp_reg
//
procedure Tx86WriteOnlyStream._movsd_esp_reg(reg : TxmmRegister);
begin
   _movsd_qword_ptr_reg_reg(gprESP, 0, reg);
end;

// _movsd_esp_reg
//
procedure Tx86WriteOnlyStream._movsd_esp_reg(offset : Integer; reg : TxmmRegister);
begin
   _movsd_qword_ptr_reg_reg(gprESP, offset, reg);
end;

// _movsd_qword_ptr_reg_reg
//
procedure Tx86WriteOnlyStream._movsd_qword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TxmmRegister);
begin
   Assert(src in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg([$F2, $0F, $11], Ord(src), dest, offset);
end;

// _movsd_reg_qword_ptr_reg
//
procedure Tx86WriteOnlyStream._movsd_reg_qword_ptr_reg(dest : TxmmRegister; src : TgpRegister; offset : Integer);
begin
   Assert(dest in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg([$F2, $0F, $10], Ord(dest), src, offset);
end;

// _movsd_reg_qword_ptr_indexed
//
procedure Tx86WriteOnlyStream._movsd_reg_qword_ptr_indexed(dest : TxmmRegister; base, index : TgpRegister; scale, offset : Integer);
begin
   Assert(dest in [xmm0..High(TxmmRegister)]);

   WriteBytes([$F2, $0F, $10]);

   _modRMSIB_ptr_reg_reg(Ord(dest)*8, base, index, scale, offset);
end;

// _movsd_qword_ptr_indexed_reg
//
procedure Tx86WriteOnlyStream._movsd_qword_ptr_indexed_reg(base, index : TgpRegister; scale, offset : Integer; src : TxmmRegister);
begin
   Assert(src in [xmm0..High(TxmmRegister)]);

   WriteBytes([$F2, $0F, $11]);

   _modRMSIB_ptr_reg_reg(Ord(src)*8, base, index, scale, offset);
end;

// _movq_execmem_reg
//
procedure Tx86WriteOnlyStream._movq_execmem_reg(stackAddr : Integer; reg : TxmmRegister);
begin
   _modRMSIB_reg_execmem([$66, $0F, $D6], reg, stackAddr);
end;

// _movq_reg_absmem
//
procedure Tx86WriteOnlyStream._movq_reg_absmem(reg : TxmmRegister; ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$F3, $0F, $7E], reg, ptr);
end;

// _mov_reg_execmem
//
procedure Tx86WriteOnlyStream._mov_reg_execmem(reg : TgpRegister; stackAddr : Integer; offset : Integer = 0);
begin
   _modRMSIB_regnum_ptr_reg([$8B], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _mov_execmem_reg
//
procedure Tx86WriteOnlyStream._mov_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$89], reg, stackAddr, offset);
end;

// _mov_eaxedx_execmem
//
procedure Tx86WriteOnlyStream._mov_eaxedx_execmem(stackAddr : Integer);
begin
   _mov_reg_execmem(gprEAX, stackAddr, 0);
   _mov_reg_execmem(gprEDX, stackAddr, 4);
end;

// _mov_execmem_eaxedx
//
procedure Tx86WriteOnlyStream._mov_execmem_eaxedx(stackAddr : Integer);
begin
   _mov_execmem_reg(stackAddr, 0, gprEAX);
   _mov_execmem_reg(stackAddr, 4, gprEDX);
end;

// _mov_execmem_imm
//
procedure Tx86WriteOnlyStream._mov_execmem_imm(stackAddr : Integer; const imm : Int64);
var
   v : DWORD;
begin
   v:=DWORD(imm);
   if v=(imm shr 32) then begin

      _mov_reg_dword(gprEAX, v);

      _mov_execmem_reg(stackAddr, 0, gprEAX);
      _mov_execmem_reg(stackAddr, 4, gprEAX);

   end else begin

      _mov_eaxedx_imm(imm);
      _mov_execmem_eaxedx(stackAddr);

   end;
end;

// _mov_eaxedx_imm
//
procedure Tx86WriteOnlyStream._mov_eaxedx_imm(const imm : Int64);
begin
   _mov_reg_dword(gprEAX, DWORD(imm));
   _mov_reg_dword(gprEDX, DWORD(imm shr 32));
end;

// _mov_reg_reg
//
procedure Tx86WriteOnlyStream._mov_reg_reg(dest, src : TgpRegister);
begin
   if dest<>src then
      WriteBytes([$89, $C0+Ord(dest)+8*Ord(src)]);
end;

// _mov_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._mov_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer = 0);
begin
   _modRMSIB_regnum_ptr_reg([$8B], Ord(dest), src, offset);
end;

// _mov_reg_dword_ptr_indexed
//
procedure Tx86WriteOnlyStream._mov_reg_dword_ptr_indexed(dest, base, index : TgpRegister; scale, offset : Integer);
begin
   WriteBytes([$8B]);

   _modRMSIB_ptr_reg_reg(Ord(dest)*8, base, index, scale, offset);
end;

// _mov_dword_ptr_reg_reg
//
procedure Tx86WriteOnlyStream._mov_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TgpRegister);
begin
   _modRMSIB_regnum_ptr_reg([$89], Ord(src), dest, offset);
end;

// _mov_qword_ptr_reg_eaxedx
//
procedure Tx86WriteOnlyStream._mov_qword_ptr_reg_eaxedx(dest : TgpRegister; offset : Integer);
begin
   _mov_dword_ptr_reg_reg(dest, offset, gprEAX);
   _mov_dword_ptr_reg_reg(dest, offset+4, gprEDX);
end;

// _mov_eaxedx_qword_ptr_reg
//
procedure Tx86WriteOnlyStream._mov_eaxedx_qword_ptr_reg(src : TgpRegister; offset : Integer);
begin
   if src<>gprEAX then begin
      _mov_reg_dword_ptr_reg(gprEAX, src, offset);
      _mov_reg_dword_ptr_reg(gprEDX, src, offset+4);
   end else begin
      _mov_reg_dword_ptr_reg(gprEDX, src, offset+4);
      _mov_reg_dword_ptr_reg(gprEAX, src, offset);
   end;
end;

// _mov_reg_dword
//
procedure Tx86WriteOnlyStream._mov_reg_dword(reg : TgpRegister; imm : DWORD);
begin
   if imm=0 then
      _xor_reg_reg(reg, reg)
   else begin
      WriteBytes([$B8+Ord(reg)]);
      WriteDWord(imm);
   end;
end;

// _add_eaxedx_imm
//
procedure Tx86WriteOnlyStream._add_eaxedx_imm(const imm : Int64);
begin
   if imm=0 then Exit;

   _add_reg_int32(gprEAX, Integer(imm));
   _adc_reg_int32(gprEDX, Integer(imm shr 32));
end;

// _add_eaxedx_execmem
//
procedure Tx86WriteOnlyStream._add_eaxedx_execmem(stackAddr : Integer);
begin
   _add_reg_execmem(gprEAX, stackAddr, 0);
   _adc_reg_execmem(gprEDX, stackAddr, 4);
end;

// _sub_eaxedx_imm
//
procedure Tx86WriteOnlyStream._sub_eaxedx_imm(const imm : Int64);
begin
   if imm=0 then Exit;

   _sub_reg_int32(gprEAX, Integer(imm));
   _sbb_reg_int32(gprEDX, Integer(imm shr 32));
end;

// _sub_eaxedx_execmem
//
procedure Tx86WriteOnlyStream._sub_eaxedx_execmem(stackAddr : Integer);
begin
   _sub_reg_execmem(gprEAX, stackAddr, 0);
   _sbb_reg_execmem(gprEDX, stackAddr, 4);
end;

// _cmp_execmem_int32
//
procedure Tx86WriteOnlyStream._cmp_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $78, stackAddr, offset, value);
end;

// _cmp_execmem_reg
//
procedure Tx86WriteOnlyStream._cmp_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _cmp_dword_ptr_reg_reg(cExecMemGPR, StackAddrToOffset(stackAddr)+offset, reg);
end;

// _cmp_reg_execmem
//
procedure Tx86WriteOnlyStream._cmp_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _cmp_reg_dword_ptr_reg(reg, cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _cmp_execmem_reg
//
procedure Tx86WriteOnlyStream._cmp_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_regnum_ptr_reg([$39], Ord(reg), dest, offset);
end;

// _cmp_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._cmp_reg_dword_ptr_reg(reg : TgpRegister; dest : TgpRegister; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$3B], Ord(reg), dest, offset);
end;

// _test_reg_reg
//
procedure Tx86WriteOnlyStream._test_reg_reg(dest, src : TgpRegister);
begin
   WriteByte($85);
   WriteByte($C0+Ord(dest)+Ord(src)*8);
end;

// _test_reg_imm
//
procedure Tx86WriteOnlyStream._test_reg_imm(reg : TgpRegister; imm : DWORD);
begin
   if (imm<=$FF) and (reg in [gprEAX..gprEBX]) then begin
      if reg=gprEAX then
         WriteByte($A8)
      else begin
         WriteByte($F6);
         WriteByte($C0+Ord(reg));
      end;
      WriteByte(imm);
   end else begin
      if reg=gprEAX then
         WriteByte($A9)
      else begin
         WriteByte($F7);
         WriteByte($C0+Ord(reg));
      end;
      WriteInt32(imm);
   end;
end;

// _test_dword_ptr_reg_dword
//
procedure Tx86WriteOnlyStream._test_dword_ptr_reg_dword(dest : TgpRegister; offset : Integer; imm : DWORD);
var
   i : Integer;
begin
   for i:=0 to 3 do begin
      if (imm and ($FF shl (i*8)))=imm then begin
         _test_dword_ptr_reg_byte(dest, offset+i, imm shr (i*8));
         Exit;
      end;
   end;

   WriteByte($F7);
   _modRMSIB_ptr_reg(0, dest, offset);
   WriteDWord(imm);
end;

// _test_dword_ptr_reg_byte
//
procedure Tx86WriteOnlyStream._test_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; imm : Byte);
begin
   WriteByte($F6);
   _modRMSIB_ptr_reg(0, dest, offset);
   WriteByte(imm);
end;

// _test_dword_ptr_reg_reg
//
procedure Tx86WriteOnlyStream._test_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TgpRegister);
begin
   _modRMSIB_regnum_ptr_reg([$85], Ord(src), dest, offset);
end;

// _test_execmem_imm
//
procedure Tx86WriteOnlyStream._test_execmem_imm(stackAddr, offset : Integer; imm : DWORD);
begin
   _test_dword_ptr_reg_dword(cExecMemGPR, StackAddrToOffset(stackAddr)+offset, imm);
end;

// _test_execmem_reg
//
procedure Tx86WriteOnlyStream._test_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _test_dword_ptr_reg_reg(cExecMemGPR, StackAddrToOffset(stackAddr)+offset, reg);
end;

// _set_al_flags
//
procedure Tx86WriteOnlyStream._set_al_flags(flags : TboolFlags);
begin
   WriteBytes([$0F, Ord(flags)+$20, $C0]);
end;

// _op_reg_int32
//
procedure Tx86WriteOnlyStream._op_reg_int32(const op : TgpOP; reg : TgpRegister; value : Integer);
begin
   if (value>=-128) and (value<=127) then
      WriteBytes([op.Short1, op.SIB+Ord(reg), Byte(value)])
   else begin
      if reg=gprEAX then
         WriteBytes([op.LongEAX+Ord(reg)])
      else WriteBytes([op.Long1, op.SIB+Ord(reg)]);
      WriteInt32(value);
   end;
end;

// _op_reg_reg
//
procedure Tx86WriteOnlyStream._op_reg_reg(const op : TgpOP; dest, src : TgpRegister);
begin
   WriteBytes([op.RegReg, $C0+Ord(dest)+8*Ord(src)])
end;

// _op_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._op_reg_dword_ptr_reg(const op : TgpOP; dest, src : TgpRegister; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([op.RegReg+2], Ord(dest), src, offset);
end;

// _cmp_reg_int32
//
procedure Tx86WriteOnlyStream._cmp_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_cmp, reg, value);
end;

// _add_reg_reg
//
procedure Tx86WriteOnlyStream._add_reg_reg(dest, src : TgpRegister);
begin
   _op_reg_reg(gpOp_add, dest, src);
end;

// _adc_reg_reg
//
procedure Tx86WriteOnlyStream._adc_reg_reg(dest, src : TgpRegister);
begin
   _op_reg_reg(gpOp_adc, dest, src);
end;

// _add_reg_int32
//
procedure Tx86WriteOnlyStream._add_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_add, reg, value);
end;

// _adc_reg_int32
//
procedure Tx86WriteOnlyStream._adc_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_adc, reg, value);
end;

// _sub_reg_int32
//
procedure Tx86WriteOnlyStream._sub_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_sub, reg, value);
end;

// _sbb_reg_int32
//
procedure Tx86WriteOnlyStream._sbb_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_sbb, reg, value);
end;

// _add_reg_execmem
//
procedure Tx86WriteOnlyStream._add_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$03], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _adc_reg_execmem
//
procedure Tx86WriteOnlyStream._adc_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$13], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _sub_reg_execmem
//
procedure Tx86WriteOnlyStream._sub_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$2B], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _sbb_reg_execmem
//
procedure Tx86WriteOnlyStream._sbb_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$1B], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _add_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._add_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_add, dest, src, offset);
end;

// _adc_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._adc_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_adc, dest, src, offset);
end;

// _sub_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._sub_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_sub, dest, src, offset);
end;

// _sbb_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._sbb_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_sbb, dest, src, offset);
end;

// _and_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._and_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_and, dest, src, offset);
end;

function Tx86WriteOnlyStream._begin_tryf_frame: integer;
begin
   _xor_reg_reg(gprEAX, gprEAX);
   _push_reg(gprEBP);
   WriteByte($68); //push dword literal
   result := self.Size;
   WriteDWord(0);
   WriteBytes([$64, $FF, $30]); //push dword ptr fs:[eax]
   WriteBytes([$64, $89, $20]); //mov fs:[eax],esp
end;

function Tx86WriteOnlyStream._begin_finally_block: integer;
begin
   _xor_reg_reg(gprEAX, gprEAX);
   _pop_reg(gprEDX);
   _pop_reg(gprECX);
   _pop_reg(gprECX);
   WriteBytes([$64, $89, $10]); //mov fs:[eax],edx
   WriteByte($68); //push dword literal
   result := self.Size;
   WriteDWord(0);
end;

function Tx86WriteOnlyStream._end_finally_block(beginResult: integer): integer;
begin
   _ret;
   WriteByte($E9); //push dword literal
   result := self.Size;
   WriteDWord(0);

   WriteByte($EB); //relative jump, 1 byte
   WriteByte(shortint((beginResult + sizeof(pointer)) - (self.size + 1)));
end;

// _or_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._or_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_or, dest, src, offset);
end;

// _xor_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._xor_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_xor, dest, src, offset);
end;

// _inc_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._inc_dword_ptr_reg(reg : TgpRegister; offset : Integer);
begin
   WriteByte($FF);
   _modRMSIB_ptr_reg(0, reg, offset);
end;

// _mul_reg
//
procedure Tx86WriteOnlyStream._mul_reg(reg : TgpRegister);
begin
   WriteBytes([$F7, $E0+Ord(reg)]);
end;

// _mul_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._mul_dword_ptr_reg(reg : TgpRegister; offset : Integer);
begin
   WriteByte($F7);
   _modRMSIB_ptr_reg($20, reg, offset);
end;

// _imul_reg_reg
//
procedure Tx86WriteOnlyStream._imul_reg_reg(dest, src : TgpRegister);
begin
   WriteBytes([$0F, $AF, $C0+Ord(dest)*8+Ord(src)]);
end;

// _imul_reg_dword_ptr_reg
//
procedure Tx86WriteOnlyStream._imul_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$0F, $AF], Ord(dest), src, offset);
//   asm
//      imul eax, [eax+1];
//   end;
end;

// _neg_reg
//
procedure Tx86WriteOnlyStream._neg_reg(reg : TgpRegister);
begin
   WriteBytes([$F7, $D8+Ord(reg)]);
end;

// _not_reg
//
procedure Tx86WriteOnlyStream._not_reg(reg : TgpRegister);
begin
   WriteBytes([$F7, $D0+Ord(reg)]);
end;

// _neg_eaxedx
//
procedure Tx86WriteOnlyStream._neg_eaxedx;
begin
   _neg_reg(gprEDX);
   _neg_reg(gprEAX);
   _sbb_reg_int32(gprEDX, 0);
end;

// _shift_reg_imm
//
procedure Tx86WriteOnlyStream._shift_reg_imm(shift : TgpShift; reg : TgpRegister; value : Integer);
begin
   if value<>0 then begin
      if value=1 then
         WriteBytes([$D1, Ord(shift)+Ord(reg)])
      else WriteBytes([$C1, Ord(shift)+Ord(reg), value]);
   end;
end;

// _shift_reg_cl
//
procedure Tx86WriteOnlyStream._shift_reg_cl(shift : TgpShift; reg : TgpRegister);
begin
   WriteBytes([$D3, Ord(shift)+Ord(reg)])
end;

// _shr_eaxedx_imm
//
procedure Tx86WriteOnlyStream._shr_eaxedx_imm(value : Integer);
begin
   if value<32 then begin
      // shrd eax, edx, value
      WriteBytes([$0F, $AC, $D0, value]);
      _shift_reg_imm(gpShr, gprEDX, value);
   end else if value<64 then begin
      _mov_reg_reg(gprEAX, gprEDX);
      _shift_reg_imm(gpShr, gprEAX, value-32);
      _xor_reg_reg(gprEDX, gprEDX);
   end else _mov_eaxedx_imm(0);
end;

// _shl_eaxedx_imm
//
procedure Tx86WriteOnlyStream._shl_eaxedx_imm(value : Integer);
begin
   if value=1 then begin
      _add_reg_reg(gprEAX, gprEAX);
      _adc_reg_reg(gprEDX, gprEDX);
   end else if value<32 then begin
      // shld edx, eax, value
      WriteBytes([$0F, $A4, $C2, value]);
      _shift_reg_imm(gpShl, gprEAX, value);
   end else if value<64 then begin
      _mov_reg_reg(gprEDX, gprEAX);
      _shift_reg_imm(gpShl, gprEDX, value-32);
      _xor_reg_reg(gprEAX, gprEAX);
   end else _mov_eaxedx_imm(0);
end;

// _sar_eaxedx_imm
//
procedure Tx86WriteOnlyStream._sar_eaxedx_imm(value : Integer);
begin
   if value<32 then begin
      // shrd eax, edx, value
      WriteBytes([$0F, $AC, $D0, value]);
      _shift_reg_imm(gpSar, gprEDX, value);
   end else if value<64 then begin
      _mov_reg_reg(gprEAX, gprEDX);
      _shift_reg_imm(gpSar, gprEAX, value-32);
      _mov_reg_dword(gprEDX, DWORD(-1));
   end else begin;
      _shift_reg_imm(gpSar, gprEDX, 31);
      _mov_reg_reg(gprEAX, gprEDX);
   end;
end;

// _shr_eaxedx_cl
//
procedure Tx86WriteOnlyStream._shr_eaxedx_cl;
begin
   // shrd eax, edx, cl
   WriteBytes([$0F, $AD, $D0]);
   _shift_reg_cl(gpShr, gprEDX);
end;

// _shl_eaxedx_cl
//
procedure Tx86WriteOnlyStream._shl_eaxedx_cl;
begin
   // shld edx, eax, cl
   WriteBytes([$0F, $A5, $C2]);
   _shift_reg_cl(gpShl, gprEAX);
end;

// _sar_eaxedx_cl
//
procedure Tx86WriteOnlyStream._sar_eaxedx_cl;
begin
   // shrd eax, edx, cl
   WriteBytes([$0F, $AD, $D0]);
   _shift_reg_cl(gpSar, gprEDX);
end;

// _add_execmem_int32
//
procedure Tx86WriteOnlyStream._add_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $40, stackAddr, offset, value);
end;

// _adc_execmem_int32
//
procedure Tx86WriteOnlyStream._adc_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $50, stackAddr, offset, value);
end;

// _sub_execmem_int32
//
procedure Tx86WriteOnlyStream._sub_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $68, stackAddr, offset, value);
end;

// _sbb_execmem_int32
//
procedure Tx86WriteOnlyStream._sbb_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $58, stackAddr, offset, value);
end;

// _add_execmem_reg
//
procedure Tx86WriteOnlyStream._add_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$01], reg, stackAddr, offset);
end;

// _adc_execmem_reg
//
procedure Tx86WriteOnlyStream._adc_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$11], reg, stackAddr, offset);
end;

// _sub_execmem_reg
//
procedure Tx86WriteOnlyStream._sub_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$29], reg, stackAddr, offset);
end;

// _sbb_execmem_reg
//
procedure Tx86WriteOnlyStream._sbb_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$19], reg, stackAddr, offset);
end;

// _and_dword_ptr_reg_byte
//
procedure Tx86WriteOnlyStream._and_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; value : Byte);
begin
   WriteByte($80);
   _modRMSIB_ptr_reg($20, dest, offset);
   WriteByte(value);
end;

// _or_dword_ptr_reg_byte
//
procedure Tx86WriteOnlyStream._or_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; value : Byte);
begin
   WriteByte($80);
   _modRMSIB_ptr_reg($08, dest, offset);
   WriteByte(value);
end;

// _execmem32_inc
//
procedure Tx86WriteOnlyStream._execmem32_inc(stackAddr : Integer; const imm : Int32);
begin
   if imm=0 then Exit;
   if imm=1 then
      _inc_dword_ptr_reg(cExecMemGPR, StackAddrToOffset(stackAddr))
   else _add_execmem_int32(stackAddr, 0, imm);
end;

// _execmem64_inc
//
procedure Tx86WriteOnlyStream._execmem64_inc(stackAddr : Integer; const imm : Int64);
begin
   _add_execmem_int32(stackAddr, 0, Integer(imm));
   _adc_execmem_int32(stackAddr, 4, Integer(imm shr 32));
end;

// _execmem64_dec
//
procedure Tx86WriteOnlyStream._execmem64_dec(stackAddr : Integer; const imm : Int64);
begin
   _sub_execmem_int32(stackAddr, 0, Integer(imm));
   _sbb_execmem_int32(stackAddr, 4, Integer(imm shr 32));
end;

// _fild_execmem
//
procedure Tx86WriteOnlyStream._fild_execmem(stackAddr : Integer);
var
   offset : Integer;
begin
   offset:=StackAddrToOffset(stackAddr);

   if (offset>=-128) and (offset<=127) then

      WriteBytes([$DF, $68+Ord(cExecMemGPR), offset])

   else begin

      WriteBytes([$DF, $A8+Ord(cExecMemGPR)]);
      WriteInt32(offset);

   end;
end;

// _fild_esp
//
procedure Tx86WriteOnlyStream._fild_esp;
begin
   WriteBytes([$DF, $2C, $24]);
end;

// _fistp_esp
//
procedure Tx86WriteOnlyStream._fistp_esp;
begin
   WriteBytes([$DF, $3C, $24]);
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

// _ffree
//
procedure Tx86WriteOnlyStream._ffree(n : Integer);
begin
   // ffree st(n)
   Assert(n in [0..7]);
   WriteBytes([$DD, $C0+n]);
end;

// _push_reg
//
procedure Tx86WriteOnlyStream._push_reg(reg : TgpRegister);
begin
   WriteBytes([$50+Ord(reg)]);
end;

// _pop_reg
//
procedure Tx86WriteOnlyStream._pop_reg(reg : TgpRegister);
begin
   WriteBytes([$58+Ord(reg)]);
end;

// _xor_reg_reg
//
procedure Tx86WriteOnlyStream._xor_reg_reg(dest, src : TgpRegister);
begin
   _op_reg_reg(gpOp_xor, dest, src);
end;

// _call_reg
//
procedure Tx86WriteOnlyStream._call_reg(reg : TgpRegister; offset : Integer);
begin
   WriteByte($FF);
   _modRMSIB_ptr_reg($10, reg, offset)
end;

// _call_absmem
//
procedure Tx86WriteOnlyStream._call_absmem(ptr : Pointer);
begin
   WriteBytes([$FF, $15]);
   WritePointer(ptr);
end;

// _test_al_al
//
procedure Tx86WriteOnlyStream._test_al_al;
begin
   WriteBytes([$84, $C0]);
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
// D2009: if you after a build get:
// [DCC Fatal Error] dwsJITx86Intrinsics.pas(1361): F2051 Unit dwsJITx86Intrinsics was compiled with a different version of dwsUtils.TWriteOnlyBlockStream.Read
// Just do a re-compile, and it should go away... - HV

