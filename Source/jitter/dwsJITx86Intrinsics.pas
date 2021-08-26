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
   dwsUtils;

type
   QWORD = UInt64;
   QWORD_PTR = ^QWORD;

   TxmmRegister = (
      xmmNone = -1,
      xmm0 = 0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 {$ifdef WIN64},
      xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15
      {$endif}
   );
   TxmmRegisters = set of xmm0..High(TxmmRegister);// xmm15;

   TymmRegister = (
      ymm0 = 0, ymm1, ymm2, ymm3, ymm4, ymm5, ymm6, ymm7{$ifdef WIN64},
      ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm15
      {$endif}
   );

   TxmmOp = (
      xmm_cvtsi2sd   = $2A,
      xmm_sqrtsd     = $51,
      xmm_addsd      = $58,
      xmm_multsd     = $59,
      xmm_subsd      = $5C,
      xmm_minsd      = $5D,
      xmm_divsd      = $5E,
      xmm_maxsd      = $5F,

      xmm_movsd      = $10
   );

   TxmmOp_pd = (
      xmm_sqrtpd     = $51,
      xmm_andpd      = $54,
      xmm_andnpd     = $55,
      xmm_orpd       = $56,
      xmm_xorpd      = $57,
      xmm_addpd      = $58,
      xmm_mulpd      = $59,
      xmm_subpd      = $5C,
      xmm_minpd      = $5D,
      xmm_divpd      = $5E,
      xmm_maxpd      = $5F,

      xmm_movpd      = $10
   );

   TgpRegister = (
      gprEAX = 0, gprECX = 1, gprEDX = 2, gprEBX = 3,
      gprESP = 4, gprEBP = 5, gprESI = 6, gprEDI = 7
   );

   TgpRegister64 = (
      gprRAX =  0, gprRCX =  1, gprRDX =  2, gprRBX =  3,
      gprRSP =  4, gprRBP =  5, gprRSI =  6, gprRDI =  7,
      gprR8  =  8, gprR9  =  9, gprR10 = 10, gprR11 = 11,
      gprR12 = 12, gprR13 = 13, gprR14 = 14, gprR15 = 15
   );
   TgpRegister64s = set of TgpRegister64;
   TgpRegister64d = (
      gprR8d  =  8, gprR9d  =  9, gprR10d = 10, gprR11d = 11,
      gprR12d = 12, gprR13d = 13, gprR14d = 14, gprR15d = 15
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

   Tx86BaseWriteOnlyStream = class(TWriteOnlyBlockStream)
      protected
         procedure _modRMSIB_regnum_regnum(const opCode : array of Byte; dest, src : Integer);
         procedure _modRMSIB_reg_reg(const opCode : array of Byte; dest, src : TxmmRegister); overload;
         procedure _modRMSIB_reg_reg(const opCode : array of Byte; dest, src : TymmRegister); overload;

         procedure _modRMSIB_ptr_reg8(rm, reg8, offset : Integer);
         procedure _modRMSIB_reg8_reg8(const opCode : Byte; dest, src : Integer);

      public
         procedure _mov_reg_reg(dest, src : TgpRegister); overload;
         procedure _mov_reg_dword(reg : TgpRegister; imm : DWORD); overload;

         procedure _op_reg_int32(const op : TgpOP; reg : TgpRegister; value : Integer); overload;
         procedure _op_reg_reg(const op : TgpOP; dest, src : TgpRegister);

         procedure _cmp_reg_int32(reg : TgpRegister; value : Integer); overload;

         procedure _add_reg_reg(dest, src : TgpRegister); overload;
         procedure _adc_reg_reg(dest, src : TgpRegister);
         procedure _add_reg_int32(reg : TgpRegister; value : Integer); overload;
         procedure _adc_reg_int32(reg : TgpRegister; value : Integer);

         procedure _sub_reg_int32(reg : TgpRegister; value : Integer);
         procedure _sbb_reg_int32(reg : TgpRegister; value : Integer);

         procedure _xor_reg_reg(dest, src : TgpRegister);

         procedure _dec(reg : TgpRegister); overload;

         procedure _set_al_flags(flags : TboolFlags);

         procedure _jump(flags: TboolFlags; offset : Int32);
         class function SizeOf_jump(flags: TboolFlags; offset : Int32) : Integer;

         procedure _nop(nb : Integer);
         procedure _ret;

         procedure _xorps_reg_reg(dest, src : TxmmRegister);
         procedure _addps_reg_reg(dest, src : TxmmRegister); overload;
         procedure _mulps_reg_reg(dest, src : TxmmRegister);

         procedure _addss(dest, src : TxmmRegister);

         procedure _haddps(dest, src : TxmmRegister);

         procedure _movshdup(dest, src : TxmmRegister);
         procedure _movhlps(dest, src : TxmmRegister);

         procedure _xmm_reg_reg(op : TxmmOp; dest, src : TxmmRegister);
   end;

   {$ifdef WIN32}
   Tx86_32_WriteOnlyStream = class(Tx86BaseWriteOnlyStream)
      private
         procedure _modRMSIB_reg_execmem(const opCode : array of Byte; reg : TxmmRegister; stackAddr : Integer); overload;
         procedure _modRMSIB_reg_execmem(const opCode : array of Byte; reg : TgpRegister; stackAddr, offset : Integer); overload;
         procedure _modRMSIB_reg_absmem(const opCode : array of Byte; reg : TxmmRegister; ptr : Pointer);
         procedure _modRMSIB_op_execmem_int32(code1, code2 : Byte; stackAddr, offset, value : Integer);
         procedure _modRMSIB_regnum_ptr_reg(const opCode : array of Byte; destNum : Integer; src : TgpRegister; offset : Integer);
         procedure _modRMSIB_ptr_reg_reg(rm : Integer; base, index : TgpRegister; scale, offset : Integer);
         procedure _modRMSIB_ptr_reg(rm : Integer; reg : TgpRegister; offset : Integer);

      public
         procedure WritePointer(const p : Pointer);

         procedure _push_reg(reg : TgpRegister);
         procedure _pop_reg(reg : TgpRegister);

         procedure _xmm_reg_execmem(op : TxmmOp; reg : TxmmRegister; stackAddr : Integer);
         procedure _xmm_reg_absmem(op : TxmmOp; reg : TxmmRegister; ptr : Pointer);

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

         procedure _mov_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer = 0);
         procedure _mov_reg_dword_ptr_indexed(dest, base, index : TgpRegister; scale, offset : Integer);
         procedure _mov_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TgpRegister);
         procedure _mov_qword_ptr_reg_eaxedx(dest : TgpRegister; offset : Integer);
         procedure _mov_eaxedx_qword_ptr_reg(src : TgpRegister; offset : Integer);

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

         procedure _op_reg_dword_ptr_reg(const op : TgpOP; dest, src : TgpRegister; offset : Integer);

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

         procedure _call_reg(reg : TgpRegister; offset : Integer);
         procedure _call_absmem(ptr : Pointer);

         procedure _test_al_al;

         //at result, insert literal address of return value from _end_finally_block, *minus 1*
         function  _begin_tryf_frame: integer;

          //at result, insert literal address of code after _end_finally_block
         function  _begin_finally_block: integer;

          //must pass return value of _begin_finally_block
          //at result, insert relative address to @HandleFinally
         function _end_finally_block(beginResult: integer): integer;
   end;
   {$endif}

   {$ifdef WIN64}
   Tx86_64_WriteOnlyStream = class(Tx86BaseWriteOnlyStream)
      private
         FFlagCalls : Boolean;
         FFrameRegisterOffset : Integer;
         FSupportsAVX : Boolean;

      protected
         procedure _modRMSIB_regnum_ptr_reg(const prefix, opCode : array of Byte; destNum : Integer; src : TgpRegister64; offset : Integer);
         procedure _modRMSIB_ptr_reg8_reg8(rm : Integer; base, index : Integer; scale, offset : Integer);

         procedure _vex_ps_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TxmmRegister); overload;
         procedure _vex_pd_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TymmRegister); overload;
         procedure _vex_ss_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TxmmRegister); overload;
         procedure _vex_sd_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TxmmRegister); overload;
         procedure _vex_dq_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TymmRegister; with4th : Boolean = False); overload;

         procedure _vex_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TymmRegister); overload;
         procedure _vex_modRMSIB_reg_reg_ptr_reg(const opCode : Byte; dest, src1 : TymmRegister; src2 : TgpRegister64; offset : Integer); overload;
         procedure _vex_modRMSIB_reg_reg_ptr_reg(const opCode : Byte; dest, src1 : TxmmRegister; src2 : TgpRegister64; offset : Integer); overload;

         procedure _vex_ps_modRMSIB_reg_ptr_reg(const opCode : Byte; dest : TymmRegister; src : TgpRegister64; offset : Integer);
         procedure _vex_pd_modRMSIB_reg_ptr_reg(const opCode : Byte; dest : TymmRegister; src : TgpRegister64; offset : Integer);
         procedure _vex_dq_modRMSIB_reg_ptr_reg(const opCode : Byte; dest : TymmRegister; src : TgpRegister64; offset : Integer);
         procedure _vex_dqu_modRMSIB_reg_ptr_reg(const opCode : Byte; dest : TymmRegister; src : TgpRegister64; offset : Integer);

         procedure _vex_pd_modRMSIB_reg_ptr_indexed(const opCode : Byte; dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
         procedure _vex_dq_modRMSIB_reg_ptr_indexed(const opCode : Byte; dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
         procedure _vex_dqu_modRMSIB_reg_ptr_indexed(const opCode : Byte; dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);


      public
         constructor Create;

         property SupportsAVX : Boolean read FSupportsAVX write FSupportsAVX;

         property FlagCalls : Boolean read FFlagCalls;
         property FrameRegisterOffset : Integer read FFrameRegisterOffset write FFrameRegisterOffset;
         procedure ClearFlags;

         procedure _mov_reg32_reg32(dest, src : TgpRegister64);
         procedure _mov_reg_reg(dest, src : TgpRegister64); overload;
         procedure _mov_reg_reg(dest : TxmmRegister; src : TgpRegister64); overload;
         procedure _mov_reg_dword(reg : TgpRegister64d; imm : DWORD); overload;
         procedure _mov_reg_dword(reg : TgpRegister64; imm : DWORD); overload;
         procedure _mov_reg_qword(reg : TgpRegister64; imm : QWORD); overload;
         procedure _mov_reg_imm(reg : TgpRegister64; imm : Int64); overload;
         procedure _mov_al_byte(imm : Byte);

         procedure _movsd_qword_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);
         procedure _movsd_reg_qword_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
         procedure _movsd_reg_qword_ptr_indexed(dest : TxmmRegister; base, index : TgpRegister64; scale, offset : Integer);
         procedure _movsd_qword_ptr_indexed_reg(base, index : TgpRegister64; scale, offset : Integer; src : TxmmRegister);
         procedure _movsd_reg_absmem(reg : TxmmRegister; ptr : Pointer);

         procedure _movupd_reg_dqword_ptr_indexed(dest : TxmmRegister; base, index : TgpRegister64; scale, offset : Integer);
         procedure _movupd_dqword_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);

         procedure _mov_reg_qword_ptr_indexed(dest, base, index : TgpRegister64; scale, offset : Integer);
         procedure _mov_qword_ptr_indexed_reg(base, index : TgpRegister64; scale, offset : Integer; src : TgpRegister64);
         procedure _mov_reg_qword_ptr_reg(dest, src : TgpRegister64; offset : Integer = 0);
         procedure _mov_qword_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TgpRegister64);
         procedure _mov_reg_qword_ptr_imm(dest : TgpRegister64; offset : Integer; imm : Int64);

         procedure _mov_reg_byte_ptr_reg(dest, src : TgpRegister64; offset : Integer = 0);
         procedure _mov_byte_ptr_reg_imm(dest : TgpRegister64; offset : Integer; imm : Byte);

         procedure _mov_execmem_imm(stackAddr : Integer; const imm : Int64);
         procedure _mov_execmem_reg(stackAddr : Integer; const reg : TgpRegister64);
         procedure _mov_reg_execmem(reg : TgpRegister64; stackAddr : Integer; offset : Integer = 0);

         procedure _cmov(flags : TboolFlags; dest, src : TgpRegister64);

         procedure _movsd_reg_reg(dest, src : TxmmRegister);
         procedure _movsd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
         procedure _movsd_execmem_reg(stackAddr : Integer; reg : TxmmRegister);

         procedure _movsd_rsp_reg(reg : TxmmRegister); overload;
         procedure _movsd_rsp_reg(offset : Integer; reg : TxmmRegister); overload;
         procedure _movsd_reg_rsp(reg : TxmmRegister; offset : Integer = 0);

         procedure _lea_reg_ptr_indexed_reg(dest, base, index : TgpRegister64; scale, offset : Integer);
         procedure _lea_reg_reg(dest, base: TgpRegister64; offset : Integer);

         procedure _op_reg_imm(const op : TgpOP; reg : TgpRegister64; value : Int64); overload;
         procedure _op_reg_reg(const op : TgpOP; dest, src : TgpRegister64); overload;
         procedure _op_reg_execmem(const op : TgpOP; reg : TgpRegister64; stackAddr : Integer);
         procedure _op_reg_qword_ptr_reg(const op : TgpOP; dest, operand : TgpRegister64; offset : Integer);

         procedure _imul_reg_qword_ptr_reg(dest, operand : TgpRegister64; offset : Integer);
         procedure _imul_reg_reg_imm(dest, operand : TgpRegister64; value : Int64);
         procedure _imul_reg_reg(dest, operand : TgpRegister64);

         procedure _neg_reg(reg : TgpRegister64);
         procedure _not_reg(reg : TgpRegister64);

         procedure _shift_reg_imm(shift : TgpShift; reg : TgpRegister64; value : Byte);

         procedure _cmp_reg_imm(reg : TgpRegister64; value : Int64); overload;
         procedure _cmp_reg_reg(left, right : TgpRegister64);
         procedure _cmp_qword_ptr_reg_imm(reg : TgpRegister64; offset : Integer; value : Int64);
         procedure _cmp_reg_execmem(reg : TgpRegister64; stackAddr : Integer);
         procedure _cmp_reg_qword_ptr_reg(left, right : TgpRegister64; offset : Integer);
         procedure _cmp_execmem_imm(stackAddr : Integer; value : Int64);
         procedure _cmp_execmem_reg(stackAddr : Integer; reg : TgpRegister64);

         procedure _test_reg_reg(dest, src : TgpRegister64);
         procedure _test_reg_imm(reg : TgpRegister64; value : Int64);

         procedure _inc(reg : TgpRegister64); overload;
         procedure _dec(reg : TgpRegister64); overload;
         procedure _add_reg_imm(reg : TgpRegister64; value : Int64);
         procedure _add_reg_reg(dest, src : TgpRegister64); overload;
         procedure _sub_reg_imm(reg : TgpRegister64; value : Int64); overload;
         procedure _sub_reg_reg(dest, src : TgpRegister64); overload;

         procedure _xor_reg_reg(dest, src : TgpRegister64);

         procedure _inc(reg : TgpRegister64d); overload;
         procedure _inc_qword_ptr_reg(reg : TgpRegister64; offset : Integer);
         procedure _dec_qword_ptr_reg(reg : TgpRegister64; offset : Integer);
         procedure _add_qword_ptr_reg(dest : TgpRegister64; offset : Integer; operand : TgpRegister64);
         procedure _sub_qword_ptr_reg(dest : TgpRegister64; offset : Integer; operand : TgpRegister64);

         procedure _add_execmem_imm(stackAddr : Integer; stepValue : Int64);
         procedure _sub_execmem_imm(stackAddr : Integer; stepValue : Int64);
         procedure _add_execmem_reg(stackAddr : Integer; operand : TgpRegister64);
         procedure _sub_execmem_reg(stackAddr : Integer; operand : TgpRegister64);

         procedure _push_reg(reg : TgpRegister64);
         procedure _pop_reg(reg : TgpRegister64);
         procedure _push_imm(imm : DWORD);

         procedure _call_absmem(ptr : Pointer);
         procedure _call_reg(reg : TgpRegister64; offset : Integer);

         procedure _test_al_al;

         procedure _cvtsi2sd(dest : TxmmRegister; src : TgpRegister64);
         procedure _cvtsd2si(dest : TgpRegister64; src : TxmmRegister);

         procedure _prefetch_ptr_reg(src : TgpRegister64; offset : Integer);
         procedure _prefetcht0_ptr_reg(src : TgpRegister64; offset : Integer);

         procedure _movss_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);
         procedure _movss_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);

         procedure _movaps_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
         procedure _movups_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);

         procedure _xmm_reg_dword_ptr_reg(op : TxmmOp; reg : TxmmRegister; src : TgpRegister64; offset : Integer);
         procedure _xmm_reg_qword_ptr_reg(op : TxmmOp; reg : TxmmRegister; src : TgpRegister64; offset : Integer);
         procedure _xmm_reg_execmem(op : TxmmOp; reg : TxmmRegister; stackAddr : Integer);
         procedure _xmm_reg_absmem(op : TxmmOp; reg : TxmmRegister; ptr : Pointer);

         procedure _xmm_pd_reg_reg(op : TxmmOp_pd; dest, src : TxmmRegister);

         procedure _comisd_reg_reg(dest, src : TxmmRegister);
         procedure _comisd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
         procedure _comisd_reg_absmem(reg : TxmmRegister;  ptr : Pointer);

         procedure _mulps_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
         procedure _mulss_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);

         procedure _v_op_pd(op : TxmmOp_pd; dest, src1, src2 : TymmRegister); overload;

         procedure _vcmppd(dest, src1, src2 : TymmRegister; predicate : Byte);
         procedure _vpcmpeqq(dest, src1, src2 : TymmRegister);

         procedure _vpblendvb(dest, src1, src2, src3 : TymmRegister);

         procedure _vbroadcastsd(dest: TymmRegister; src : TxmmRegister);
         procedure _vbroadcastsd_ptr_reg(dest: TymmRegister; src : TgpRegister64; offset : Integer);
         procedure _vpbroadcastq_ptr_reg(dest: TymmRegister; src : TgpRegister64; offset : Integer);
         procedure _vpbroadcastq_ptr_indexed(dest: TymmRegister; base, index : TgpRegister64; scale, offset : Integer);

         procedure _vxorps(dest, src1, src2 : TymmRegister); overload;
         procedure _vxorps(reg : TymmRegister); overload; inline;

         procedure _vaddss(dest, src1, src2 : TxmmRegister); overload;

         procedure _vaddps(dest, src1, src2 : TxmmRegister); overload;
         procedure _vaddps(dest, src1, src2 : TymmRegister); overload;
         procedure _vaddpd(dest, src1, src2 : TymmRegister); overload;

         procedure _vmulps(dest, src1, src2 : TxmmRegister); overload;
         procedure _vmulps(dest, src1, src2 : TymmRegister); overload;

         procedure _vmulsd(dest, src1, src2 : TxmmRegister); overload;

         procedure _vhaddps(dest, src1, src2 : TxmmRegister); overload;

         procedure _vmovshdup(dest, src : TxmmRegister);
         procedure _vmovhlps(dest, src1, src2 : TxmmRegister);

         procedure _vmovaps_ptr_reg(dest : TymmRegister; src : TgpRegister64; offset : Integer);
         procedure _vmovups_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer); overload;
         procedure _vmovups_ptr_reg(dest : TymmRegister; src : TgpRegister64; offset : Integer); overload;
         procedure _vmovups_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);

         procedure _vmovupd_ptr_reg(dest : TymmRegister; src : TgpRegister64; offset : Integer); overload;
         procedure _vmovupd_ptr_indexed(dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
         procedure _vmovdqu_ptr_reg(dest : TymmRegister; src : TgpRegister64; offset : Integer);
         procedure _vmovdqu_ptr_indexed(dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);

         procedure _vmovupd_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TymmRegister);

         procedure _vmovss_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);
         procedure _vmovss_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);

         procedure _vmulps_ptr_reg(dest, src1 : TymmRegister; src2 : TgpRegister64; offset : Integer); overload;
         procedure _vmulps_ptr_reg(dest, src1 : TxmmRegister; src2 : TgpRegister64; offset : Integer); overload;

         procedure _vextract128_low(dest : TxmmRegister; src : TymmRegister);
         procedure _vextract128_high(dest : TxmmRegister; src : TymmRegister); overload;

         procedure _vfmadd231ss_ptr_reg(dest, src1 : TxmmRegister; src2 : TgpRegister64; offset : Int32); overload;
         procedure _vfmadd231ps_ptr_reg(dest, src1 : TxmmRegister; src2 : TgpRegister64; offset : Int32); overload;
         procedure _vfmadd231ps_ptr_reg(dest, src1 : TymmRegister; src2 : TgpRegister64; offset : Int32); overload;
         procedure _vfmadd231pd_ptr_reg(dest, src1 : TymmRegister; src2 : TgpRegister64; offset : Int32); overload;

         procedure _vfmadd231ps(dest, src1, src2 : TxmmRegister); overload;
         procedure _vfmadd231ps(dest, src1, src2 : TymmRegister); overload;

         procedure _vfmadd_pd(op : Integer; dest, src1, src2 : TymmRegister);

         procedure _vzeroupper;
         procedure _vzeroall;
   end;
   {$endif}

    Tx86_Platform_WriteOnlyStream = {$ifdef WIN32}Tx86_32_WriteOnlyStream{$endif}{$ifdef WIN64}Tx86_64_WriteOnlyStream{$endif};

const
   gpOp_add : TgpOp = (Short1: $83; SIB: $C0; Long1: $81; LongEAX: $05; RegReg: $01);
   gpOp_adc : TgpOp = (Short1: $83; SIB: $D0; Long1: $81; LongEAX: $15; RegReg: $11);
   gpOp_sub : TgpOp = (Short1: $83; SIB: $E8; Long1: $81; LongEAX: $2D; RegReg: $29);
   gpOp_sbb : TgpOp = (Short1: $83; SIB: $D8; Long1: $81; LongEAX: $1D; RegReg: $19);
   gpOp_xor : TgpOp = (Short1: $83; SIB: $F0; Long1: $81; LongEAX: $35; RegReg: $31);
   gpOp_and : TgpOp = (Short1: $83; SIB: $E0; Long1: $81; LongEAX: $25; RegReg: $21);
   gpOp_or  : TgpOp = (Short1: $83; SIB: $C8; Long1: $81; LongEAX: $0D; RegReg: $09);
   gpOp_cmp : TgpOp = (Short1: $83; SIB: $F8; Long1: $81; LongEAX: $3D; RegReg: $39);

   cgpRegisterName : array [TgpRegister] of UnicodeString = (
      'eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi'
      );
   cgpRegister8bitName : array [TgpRegister] of UnicodeString = (
      'al', 'cl', 'dl', 'bl', '??', '??', '??', '??'
      );
   cgpRegister64Name : array [TgpRegister64] of UnicodeString = (
      'rax', 'rcx', 'rdx', 'rbx', 'rsp', 'rbp', 'rsi', 'rdi',
      'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'r15'
      );
   cgpRegister64dName : array [TgpRegister64] of UnicodeString = (
      'eax', 'ecx', 'edx', 'ebx', 'esp', 'ebp', 'esi', 'edi',
      'r8d', 'r9d', 'r10d', 'r11d', 'r12d', 'r13d', 'r14d', 'r15d'
      );
   cExecMemGPR = {$ifdef WIN32} gprEBX {$endif} {$ifdef WIN64} gprRBX {$endif};

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
   Result := addr*SizeOf(Variant) + Integer(@PVarData(nil).VInt64);
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
// ------------------ Tx86BaseWriteOnlyStream ------------------
// ------------------

// _modRMSIB_regnum_regnum
//
procedure Tx86BaseWriteOnlyStream._modRMSIB_regnum_regnum(const opCode : array of Byte; dest, src : Integer);
begin
   if dest >= 8 then
      if src >= 8 then
         WriteByte($45)
      else WriteByte($44)
   else if src >= 8 then
      WriteByte($41);

   WriteBytes(opCode);

   WriteByte($C0 + (Ord(src) and 7) + (Ord(dest) and 7)*8);
end;

// _modRMSIB_reg_reg
//
procedure Tx86BaseWriteOnlyStream._modRMSIB_reg_reg(const opCode : array of Byte; dest, src : TxmmRegister);
begin
   _modRMSIB_regnum_regnum(opCode, Ord(dest), Ord(src));
end;

// _modRMSIB_reg_reg
//
procedure Tx86BaseWriteOnlyStream._modRMSIB_reg_reg(const opCode : array of Byte; dest, src : TymmRegister);
begin
   _modRMSIB_regnum_regnum(opCode, Ord(dest), Ord(src));
end;

// _modRMSIB_ptr_reg8
//
procedure Tx86BaseWriteOnlyStream._modRMSIB_ptr_reg8(rm, reg8, offset : Integer);
begin
   Inc(rm, reg8);
   if (offset <> 0) or (reg8 = Ord(gprEBP)) then begin
      if Int8(offset) = offset then
         Inc(rm, $40)
      else Inc(rm, $80);
   end;
   WriteByte(rm);
   if reg8 = Ord(gprESP) then
      WriteByte($24);
   if (rm and $40) <> 0 then
      WriteByte(Byte(offset))
   else if (rm and $80) <> 0 then
      WriteInt32(offset);
end;

// _modRMSIB_reg8_reg8
//
procedure Tx86BaseWriteOnlyStream._modRMSIB_reg8_reg8(const opCode : Byte; dest, src : Integer);
begin
   WriteBytes([
      opCode,
      $C0 + Ord(src) + Ord(dest)*8
   ]);
end;

// _mov_reg_reg
//
procedure Tx86BaseWriteOnlyStream._mov_reg_reg(dest, src : TgpRegister);
begin
   if dest<>src then
      WriteBytes([$89, $C0+Ord(dest)+8*Ord(src)]);
end;

// _mov_reg_dword
//
procedure Tx86BaseWriteOnlyStream._mov_reg_dword(reg : TgpRegister; imm : DWORD);
begin
   if imm=0 then
      _xor_reg_reg(reg, reg)
   else begin
      WriteBytes([$B8+Ord(reg)]);
      WriteDWord(imm);
   end;
end;

// _op_reg_int32
//
procedure Tx86BaseWriteOnlyStream._op_reg_int32(const op : TgpOP; reg : TgpRegister; value : Integer);
begin
   if Int8(value) = value then
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
procedure Tx86BaseWriteOnlyStream._op_reg_reg(const op : TgpOP; dest, src : TgpRegister);
begin
   WriteBytes([op.RegReg, $C0+Ord(dest)+8*Ord(src)])
end;

// _cmp_reg_int32
//
procedure Tx86BaseWriteOnlyStream._cmp_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_cmp, reg, value);
end;

// _add_reg_reg
//
procedure Tx86BaseWriteOnlyStream._add_reg_reg(dest, src : TgpRegister);
begin
   _op_reg_reg(gpOp_add, dest, src);
end;

// _adc_reg_reg
//
procedure Tx86BaseWriteOnlyStream._adc_reg_reg(dest, src : TgpRegister);
begin
   _op_reg_reg(gpOp_adc, dest, src);
end;

// _add_reg_int32
//
procedure Tx86BaseWriteOnlyStream._add_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_add, reg, value);
end;

// _adc_reg_int32
//
procedure Tx86BaseWriteOnlyStream._adc_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_adc, reg, value);
end;

// _sub_reg_int32
//
procedure Tx86BaseWriteOnlyStream._sub_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_sub, reg, value);
end;

// _sbb_reg_int32
//
procedure Tx86BaseWriteOnlyStream._sbb_reg_int32(reg : TgpRegister; value : Integer);
begin
   _op_reg_int32(gpOp_sbb, reg, value);
end;

// _xor_reg_reg
//
procedure Tx86BaseWriteOnlyStream._xor_reg_reg(dest, src : TgpRegister);
begin
   _op_reg_reg(gpOp_xor, dest, src);
end;

// _dec
//
procedure Tx86BaseWriteOnlyStream._dec(reg : TgpRegister);
begin
   WriteBytes([ $ff, $c8 + Ord(reg) ]);
end;

// _set_al_flags
//
procedure Tx86BaseWriteOnlyStream._set_al_flags(flags : TboolFlags);
begin
   WriteBytes([$0F, Ord(flags)+$20, $C0]);
end;

// _jump
//
procedure Tx86BaseWriteOnlyStream._jump(flags: TboolFlags; offset : Int32);
begin
   if offset = 0 then
      // ignore
   else if Int8(offset-2) = offset-2 then begin

      if flags = flagsNone then
         WriteByte($EB)
      else WriteByte(Ord(flags));
      WriteByte(Byte(offset - 2));

   end else begin

      if flags = flagsNone then begin
         WriteByte($E9);
         WriteInt32(offset - 5);
      end else begin
         WriteByte($0F);
         WriteByte(Ord(flags) + $10);
         WriteInt32(offset - 6);
      end;

   end;
end;

// SizeOf_jump
//
class function Tx86BaseWriteOnlyStream.SizeOf_jump(flags: TboolFlags; offset : Int32) : Integer;
begin
   if offset = 0 then
      Result := 0
   else if Int8(offset-2) = offset-2 then
      Result := 2
   else if flags = flagsNone then
      Result := 5
   else Result := 6;
end;

// _nop
//
procedure Tx86BaseWriteOnlyStream._nop(nb : Integer);
begin
   while nb>=3 do begin
      WriteBytes([$0F, $1F, $00]);
      Dec(nb, 3);
   end;
   case nb of
      1 : WriteBytes([$90]);
      2 : WriteBytes([$66, $90]);
   end;
end;

// _ret
//
procedure Tx86BaseWriteOnlyStream._ret;
begin
   WriteBytes([$C3]);
end;

// _xorps_reg_reg
//
procedure Tx86BaseWriteOnlyStream._xorps_reg_reg(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$0F, $57], dest, src);
end;

// _addps_reg_reg
//
procedure Tx86BaseWriteOnlyStream._addps_reg_reg(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$0F, $58], dest, src);
end;

// _mulps_reg_reg
//
procedure Tx86BaseWriteOnlyStream._mulps_reg_reg(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$0F, $59], dest, src);
end;

// _addss
//
procedure Tx86BaseWriteOnlyStream._addss(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$F3, $0F, $58], dest, src);
end;

// _haddps
//
procedure Tx86BaseWriteOnlyStream._haddps(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$F2, $0F, $7C], dest, src);
end;

// _movshdup
//
procedure Tx86BaseWriteOnlyStream._movshdup(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$F3, $0F, $16], dest, src);
end;

// _movhlps
//
procedure Tx86BaseWriteOnlyStream._movhlps(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$0F, $12], dest, src);
end;

// _xmm_reg_reg
//
procedure Tx86BaseWriteOnlyStream._xmm_reg_reg(op : TxmmOp; dest, src : TxmmRegister);
begin
   WriteByte($F2);
   _modRMSIB_reg_reg([$0F, Ord(op)], dest, src);
end;

// ------------------
// ------------------ Tx86_32_WriteOnlyStream ------------------
// ------------------

{$ifdef WIN32}

// _modRMSIB_reg_execmem (xmm)
//
procedure Tx86_32_WriteOnlyStream._modRMSIB_reg_execmem(const opCode : array of Byte; reg : TxmmRegister; stackAddr : Integer);
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg(opCode, Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr));
end;

// _modRMSIB_reg_execmem (gpr)
//
procedure Tx86_32_WriteOnlyStream._modRMSIB_reg_execmem(const opCode : array of Byte; reg : TgpRegister; stackAddr, offset : Integer);
begin
   Assert(reg in [gprEAX..High(gprEDI)]);

   _modRMSIB_regnum_ptr_reg(opCode, Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _modRMSIB_reg_absmem
//
procedure Tx86_32_WriteOnlyStream._modRMSIB_reg_absmem(const opCode : array of Byte; reg : TxmmRegister; ptr : Pointer);
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   WriteBytes(opCode);

   WriteByte($05+Ord(reg)*8);
   WritePointer(ptr);
end;

// _modRMSIB_op_execmem_int32
//
procedure Tx86_32_WriteOnlyStream._modRMSIB_op_execmem_int32(code1, code2 : Byte; stackAddr, offset, value : Integer);
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
procedure Tx86_32_WriteOnlyStream._modRMSIB_regnum_ptr_reg(const opCode : array of Byte; destNum : Integer; src : TgpRegister; offset : Integer);
begin
   WriteBytes(opCode);
   _modRMSIB_ptr_reg(destNum*8, src, offset)
end;

// _modRMSIB_ptr_reg_reg
//
procedure Tx86_32_WriteOnlyStream._modRMSIB_ptr_reg_reg(rm : Integer; base, index : TgpRegister; scale, offset : Integer);
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

// _modRMSIB_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._modRMSIB_ptr_reg(rm : Integer; reg : TgpRegister; offset : Integer);
begin
   _modRMSIB_ptr_reg8(rm, Ord(reg), offset);
end;

// WritePointer
//
procedure Tx86_32_WriteOnlyStream.WritePointer(const p : Pointer);
var
   ip : NativeInt;
begin
   // use local variable to work around XE3/XE4 bug (QC #115212)
   ip:=NativeInt(p);
   Write(ip, SizeOf(NativeInt));
end;

// _push_reg
//
procedure Tx86_32_WriteOnlyStream._push_reg(reg : TgpRegister);
begin
   WriteByte($50 + Ord(reg));
end;

// _pop_reg
//
procedure Tx86_32_WriteOnlyStream._pop_reg(reg : TgpRegister);
begin
   WriteByte($58 + Ord(reg));
end;

// _xmm_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._xmm_reg_execmem(op : TxmmOp; reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_execmem([$F2, $0F, Ord(op)], reg, stackAddr);
end;

// _xmm_reg_absmem
//
procedure Tx86_32_WriteOnlyStream._xmm_reg_absmem(op : TxmmOp; reg : TxmmRegister;  ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$F2, $0F, Ord(op)], reg, ptr);
end;

// _comisd_reg_reg
//
procedure Tx86_32_WriteOnlyStream._comisd_reg_reg(dest, src : TxmmRegister);
begin
   _modRMSIB_reg_reg([$66, $0F, $2F], dest, src);
end;

// _comisd_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._comisd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_execmem([$66, $0F, $2F], reg, stackAddr);
end;

// _comisd_reg_absmem
//
procedure Tx86_32_WriteOnlyStream._comisd_reg_absmem(reg : TxmmRegister;  ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$66, $0F, $2F], reg, ptr);
end;

// _movsd_reg_reg
//
procedure Tx86_32_WriteOnlyStream._movsd_reg_reg(dest, src : TxmmRegister);
begin
   // really does a movaps to copy registers (shorter encoding)
   _modRMSIB_reg_reg([$0F, $28], dest, src);
end;

// _movsd_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._movsd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_reg_execmem([$F2, $0F, $10], reg, stackAddr);
end;

// _movsd_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._movsd_execmem_reg(stackAddr : Integer; reg : TxmmRegister);
begin
   _modRMSIB_reg_execmem([$F2, $0F, $11], reg, stackAddr);
end;

// _movsd_reg_absmem
//
procedure Tx86_32_WriteOnlyStream._movsd_reg_absmem(reg : TxmmRegister; ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$F2, $0F, $10], reg, ptr);
end;

// _movsd_reg_esp
//
procedure Tx86_32_WriteOnlyStream._movsd_reg_esp(reg : TxmmRegister; offset : Integer = 0);
begin
   _movsd_reg_qword_ptr_reg(reg, gprESP, offset);
end;

// _movsd_esp_reg
//
procedure Tx86_32_WriteOnlyStream._movsd_esp_reg(reg : TxmmRegister);
begin
   _movsd_qword_ptr_reg_reg(gprESP, 0, reg);
end;

// _movsd_esp_reg
//
procedure Tx86_32_WriteOnlyStream._movsd_esp_reg(offset : Integer; reg : TxmmRegister);
begin
   _movsd_qword_ptr_reg_reg(gprESP, offset, reg);
end;

// _movsd_qword_ptr_reg_reg
//
procedure Tx86_32_WriteOnlyStream._movsd_qword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TxmmRegister);
begin
   Assert(src in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg([$F2, $0F, $11], Ord(src), dest, offset);
end;

// _movsd_reg_qword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._movsd_reg_qword_ptr_reg(dest : TxmmRegister; src : TgpRegister; offset : Integer);
begin
   Assert(dest in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg([$F2, $0F, $10], Ord(dest), src, offset);
end;

// _movsd_reg_qword_ptr_indexed
//
procedure Tx86_32_WriteOnlyStream._movsd_reg_qword_ptr_indexed(dest : TxmmRegister; base, index : TgpRegister; scale, offset : Integer);
begin
   Assert(dest in [xmm0..High(TxmmRegister)]);

   WriteBytes([$F2, $0F, $10]);

   _modRMSIB_ptr_reg_reg(Ord(dest)*8, base, index, scale, offset);
end;

// _movsd_qword_ptr_indexed_reg
//
procedure Tx86_32_WriteOnlyStream._movsd_qword_ptr_indexed_reg(base, index : TgpRegister; scale, offset : Integer; src : TxmmRegister);
begin
   Assert(src in [xmm0..High(TxmmRegister)]);

   WriteBytes([$F2, $0F, $11]);

   _modRMSIB_ptr_reg_reg(Ord(src)*8, base, index, scale, offset);
end;

// _movq_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._movq_execmem_reg(stackAddr : Integer; reg : TxmmRegister);
begin
   _modRMSIB_reg_execmem([$66, $0F, $D6], reg, stackAddr);
end;

// _movq_reg_absmem
//
procedure Tx86_32_WriteOnlyStream._movq_reg_absmem(reg : TxmmRegister; ptr : Pointer);
begin
   _modRMSIB_reg_absmem([$F3, $0F, $7E], reg, ptr);
end;

// _mov_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._mov_reg_execmem(reg : TgpRegister; stackAddr : Integer; offset : Integer = 0);
begin
   _modRMSIB_regnum_ptr_reg([$8B], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _mov_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._mov_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$89], reg, stackAddr, offset);
end;

// _mov_eaxedx_execmem
//
procedure Tx86_32_WriteOnlyStream._mov_eaxedx_execmem(stackAddr : Integer);
begin
   _mov_reg_execmem(gprEAX, stackAddr, 0);
   _mov_reg_execmem(gprEDX, stackAddr, 4);
end;

// _mov_execmem_eaxedx
//
procedure Tx86_32_WriteOnlyStream._mov_execmem_eaxedx(stackAddr : Integer);
begin
   _mov_execmem_reg(stackAddr, 0, gprEAX);
   _mov_execmem_reg(stackAddr, 4, gprEDX);
end;

// _mov_execmem_imm
//
procedure Tx86_32_WriteOnlyStream._mov_execmem_imm(stackAddr : Integer; const imm : Int64);
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
procedure Tx86_32_WriteOnlyStream._mov_eaxedx_imm(const imm : Int64);
begin
   _mov_reg_dword(gprEAX, DWORD(imm));
   _mov_reg_dword(gprEDX, DWORD(imm shr 32));
end;

// _mov_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._mov_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer = 0);
begin
   _modRMSIB_regnum_ptr_reg([$8B], Ord(dest), src, offset);
end;

// _mov_reg_dword_ptr_indexed
//
procedure Tx86_32_WriteOnlyStream._mov_reg_dword_ptr_indexed(dest, base, index : TgpRegister; scale, offset : Integer);
begin
   WriteBytes([$8B]);

   _modRMSIB_ptr_reg_reg(Ord(dest)*8, base, index, scale, offset);
end;

// _mov_dword_ptr_reg_reg
//
procedure Tx86_32_WriteOnlyStream._mov_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TgpRegister);
begin
   _modRMSIB_regnum_ptr_reg([$89], Ord(src), dest, offset);
end;

// _mov_qword_ptr_reg_eaxedx
//
procedure Tx86_32_WriteOnlyStream._mov_qword_ptr_reg_eaxedx(dest : TgpRegister; offset : Integer);
begin
   _mov_dword_ptr_reg_reg(dest, offset, gprEAX);
   _mov_dword_ptr_reg_reg(dest, offset+4, gprEDX);
end;

// _mov_eaxedx_qword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._mov_eaxedx_qword_ptr_reg(src : TgpRegister; offset : Integer);
begin
   if src<>gprEAX then begin
      _mov_reg_dword_ptr_reg(gprEAX, src, offset);
      _mov_reg_dword_ptr_reg(gprEDX, src, offset+4);
   end else begin
      _mov_reg_dword_ptr_reg(gprEDX, src, offset+4);
      _mov_reg_dword_ptr_reg(gprEAX, src, offset);
   end;
end;

// _add_eaxedx_imm
//
procedure Tx86_32_WriteOnlyStream._add_eaxedx_imm(const imm : Int64);
begin
   if imm=0 then Exit;

   _add_reg_int32(gprEAX, Integer(imm));
   _adc_reg_int32(gprEDX, Integer(imm shr 32));
end;

// _add_eaxedx_execmem
//
procedure Tx86_32_WriteOnlyStream._add_eaxedx_execmem(stackAddr : Integer);
begin
   _add_reg_execmem(gprEAX, stackAddr, 0);
   _adc_reg_execmem(gprEDX, stackAddr, 4);
end;

// _sub_eaxedx_imm
//
procedure Tx86_32_WriteOnlyStream._sub_eaxedx_imm(const imm : Int64);
begin
   if imm=0 then Exit;

   _sub_reg_int32(gprEAX, Integer(imm));
   _sbb_reg_int32(gprEDX, Integer(imm shr 32));
end;

// _sub_eaxedx_execmem
//
procedure Tx86_32_WriteOnlyStream._sub_eaxedx_execmem(stackAddr : Integer);
begin
   _sub_reg_execmem(gprEAX, stackAddr, 0);
   _sbb_reg_execmem(gprEDX, stackAddr, 4);
end;

// _cmp_execmem_int32
//
procedure Tx86_32_WriteOnlyStream._cmp_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $78, stackAddr, offset, value);
end;

// _cmp_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._cmp_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _cmp_dword_ptr_reg_reg(cExecMemGPR, StackAddrToOffset(stackAddr)+offset, reg);
end;

// _cmp_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._cmp_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _cmp_reg_dword_ptr_reg(reg, cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _cmp_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._cmp_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_regnum_ptr_reg([$39], Ord(reg), dest, offset);
end;

// _cmp_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._cmp_reg_dword_ptr_reg(reg : TgpRegister; dest : TgpRegister; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$3B], Ord(reg), dest, offset);
end;

// _test_reg_reg
//
procedure Tx86_32_WriteOnlyStream._test_reg_reg(dest, src : TgpRegister);
begin
   WriteBytes([ $85, $C0+Ord(dest)+Ord(src)*8 ]);
end;

// _test_reg_imm
//
procedure Tx86_32_WriteOnlyStream._test_reg_imm(reg : TgpRegister; imm : DWORD);
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
procedure Tx86_32_WriteOnlyStream._test_dword_ptr_reg_dword(dest : TgpRegister; offset : Integer; imm : DWORD);
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
procedure Tx86_32_WriteOnlyStream._test_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; imm : Byte);
begin
   WriteByte($F6);
   _modRMSIB_ptr_reg(0, dest, offset);
   WriteByte(imm);
end;

// _test_dword_ptr_reg_reg
//
procedure Tx86_32_WriteOnlyStream._test_dword_ptr_reg_reg(dest : TgpRegister; offset : Integer; src : TgpRegister);
begin
   _modRMSIB_regnum_ptr_reg([$85], Ord(src), dest, offset);
end;

// _test_execmem_imm
//
procedure Tx86_32_WriteOnlyStream._test_execmem_imm(stackAddr, offset : Integer; imm : DWORD);
begin
   _test_dword_ptr_reg_dword(cExecMemGPR, StackAddrToOffset(stackAddr)+offset, imm);
end;

// _test_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._test_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _test_dword_ptr_reg_reg(cExecMemGPR, StackAddrToOffset(stackAddr)+offset, reg);
end;

// _op_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._op_reg_dword_ptr_reg(const op : TgpOP; dest, src : TgpRegister; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([op.RegReg+2], Ord(dest), src, offset);
end;

// _add_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._add_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$03], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _adc_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._adc_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$13], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _sub_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._sub_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$2B], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _sbb_reg_execmem
//
procedure Tx86_32_WriteOnlyStream._sbb_reg_execmem(reg : TgpRegister; stackAddr, offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$1B], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr)+offset);
end;

// _add_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._add_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_add, dest, src, offset);
end;

// _adc_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._adc_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_adc, dest, src, offset);
end;

// _sub_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._sub_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_sub, dest, src, offset);
end;

// _sbb_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._sbb_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_sbb, dest, src, offset);
end;

// _and_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._and_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_and, dest, src, offset);
end;

function Tx86_32_WriteOnlyStream._begin_tryf_frame: integer;
begin
   _xor_reg_reg(gprEAX, gprEAX);
   _push_reg(gprEBP);
   WriteByte($68); //push dword literal
   result := self.Size;
   WriteDWord(0);
   WriteBytes([
      $64, $FF, $30, // push dword ptr fs:[eax]
      $64, $89, $20  // mov  fs:[eax], esp
   ]);
end;

function Tx86_32_WriteOnlyStream._begin_finally_block: integer;
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

function Tx86_32_WriteOnlyStream._end_finally_block(beginResult: integer): integer;
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
procedure Tx86_32_WriteOnlyStream._or_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_or, dest, src, offset);
end;

// _xor_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._xor_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _op_reg_dword_ptr_reg(gpOp_xor, dest, src, offset);
end;

// _inc_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._inc_dword_ptr_reg(reg : TgpRegister; offset : Integer);
begin
   WriteByte($FF);
   _modRMSIB_ptr_reg(0, reg, offset);
end;

// _mul_reg
//
procedure Tx86_32_WriteOnlyStream._mul_reg(reg : TgpRegister);
begin
   WriteBytes([$F7, $E0+Ord(reg)]);
end;

// _mul_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._mul_dword_ptr_reg(reg : TgpRegister; offset : Integer);
begin
   WriteByte($F7);
   _modRMSIB_ptr_reg($20, reg, offset);
end;

// _imul_reg_reg
//
procedure Tx86_32_WriteOnlyStream._imul_reg_reg(dest, src : TgpRegister);
begin
   WriteBytes([$0F, $AF, $C0+Ord(dest)*8+Ord(src)]);
end;

// _imul_reg_dword_ptr_reg
//
procedure Tx86_32_WriteOnlyStream._imul_reg_dword_ptr_reg(dest, src : TgpRegister; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$0F, $AF], Ord(dest), src, offset);
//   asm
//      imul eax, [eax+1];
//   end;
end;

// _neg_reg
//
procedure Tx86_32_WriteOnlyStream._neg_reg(reg : TgpRegister);
begin
   WriteBytes([$F7, $D8+Ord(reg)]);
end;

// _not_reg
//
procedure Tx86_32_WriteOnlyStream._not_reg(reg : TgpRegister);
begin
   WriteBytes([$F7, $D0+Ord(reg)]);
end;

// _neg_eaxedx
//
procedure Tx86_32_WriteOnlyStream._neg_eaxedx;
begin
   _neg_reg(gprEDX);
   _neg_reg(gprEAX);
   _sbb_reg_int32(gprEDX, 0);
end;

// _shift_reg_imm
//
procedure Tx86_32_WriteOnlyStream._shift_reg_imm(shift : TgpShift; reg : TgpRegister; value : Integer);
begin
   if value<>0 then begin
      if value=1 then
         WriteBytes([$D1, Ord(shift)+Ord(reg)])
      else WriteBytes([$C1, Ord(shift)+Ord(reg), value]);
   end;
end;

// _shift_reg_cl
//
procedure Tx86_32_WriteOnlyStream._shift_reg_cl(shift : TgpShift; reg : TgpRegister);
begin
   WriteBytes([$D3, Ord(shift)+Ord(reg)])
end;

// _shr_eaxedx_imm
//
procedure Tx86_32_WriteOnlyStream._shr_eaxedx_imm(value : Integer);
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
procedure Tx86_32_WriteOnlyStream._shl_eaxedx_imm(value : Integer);
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
procedure Tx86_32_WriteOnlyStream._sar_eaxedx_imm(value : Integer);
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
procedure Tx86_32_WriteOnlyStream._shr_eaxedx_cl;
begin
   // shrd eax, edx, cl
   WriteBytes([$0F, $AD, $D0]);
   _shift_reg_cl(gpShr, gprEDX);
end;

// _shl_eaxedx_cl
//
procedure Tx86_32_WriteOnlyStream._shl_eaxedx_cl;
begin
   // shld edx, eax, cl
   WriteBytes([$0F, $A5, $C2]);
   _shift_reg_cl(gpShl, gprEAX);
end;

// _sar_eaxedx_cl
//
procedure Tx86_32_WriteOnlyStream._sar_eaxedx_cl;
begin
   // shrd eax, edx, cl
   WriteBytes([$0F, $AD, $D0]);
   _shift_reg_cl(gpSar, gprEDX);
end;

// _add_execmem_int32
//
procedure Tx86_32_WriteOnlyStream._add_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $40, stackAddr, offset, value);
end;

// _adc_execmem_int32
//
procedure Tx86_32_WriteOnlyStream._adc_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $50, stackAddr, offset, value);
end;

// _sub_execmem_int32
//
procedure Tx86_32_WriteOnlyStream._sub_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $68, stackAddr, offset, value);
end;

// _sbb_execmem_int32
//
procedure Tx86_32_WriteOnlyStream._sbb_execmem_int32(stackAddr, offset, value : Integer);
begin
   _modRMSIB_op_execmem_int32($83, $58, stackAddr, offset, value);
end;

// _add_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._add_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$01], reg, stackAddr, offset);
end;

// _adc_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._adc_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$11], reg, stackAddr, offset);
end;

// _sub_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._sub_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$29], reg, stackAddr, offset);
end;

// _sbb_execmem_reg
//
procedure Tx86_32_WriteOnlyStream._sbb_execmem_reg(stackAddr, offset : Integer; reg : TgpRegister);
begin
   _modRMSIB_reg_execmem([$19], reg, stackAddr, offset);
end;

// _and_dword_ptr_reg_byte
//
procedure Tx86_32_WriteOnlyStream._and_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; value : Byte);
begin
   WriteByte($80);
   _modRMSIB_ptr_reg($20, dest, offset);
   WriteByte(value);
end;

// _or_dword_ptr_reg_byte
//
procedure Tx86_32_WriteOnlyStream._or_dword_ptr_reg_byte(dest : TgpRegister; offset : Integer; value : Byte);
begin
   WriteByte($80);
   _modRMSIB_ptr_reg($08, dest, offset);
   WriteByte(value);
end;

// _execmem32_inc
//
procedure Tx86_32_WriteOnlyStream._execmem32_inc(stackAddr : Integer; const imm : Int32);
begin
   if imm=0 then Exit;
   if imm=1 then
      _inc_dword_ptr_reg(cExecMemGPR, StackAddrToOffset(stackAddr))
   else _add_execmem_int32(stackAddr, 0, imm);
end;

// _execmem64_inc
//
procedure Tx86_32_WriteOnlyStream._execmem64_inc(stackAddr : Integer; const imm : Int64);
begin
   _add_execmem_int32(stackAddr, 0, Integer(imm));
   _adc_execmem_int32(stackAddr, 4, Integer(imm shr 32));
end;

// _execmem64_dec
//
procedure Tx86_32_WriteOnlyStream._execmem64_dec(stackAddr : Integer; const imm : Int64);
begin
   _sub_execmem_int32(stackAddr, 0, Integer(imm));
   _sbb_execmem_int32(stackAddr, 4, Integer(imm shr 32));
end;

// _fild_execmem
//
procedure Tx86_32_WriteOnlyStream._fild_execmem(stackAddr : Integer);
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
procedure Tx86_32_WriteOnlyStream._fild_esp;
begin
   WriteBytes([$DF, $2C, $24]);
end;

// _fistp_esp
//
procedure Tx86_32_WriteOnlyStream._fistp_esp;
begin
   WriteBytes([$DF, $3C, $24]);
end;

// _fld_esp
//
procedure Tx86_32_WriteOnlyStream._fld_esp;
begin
   // fld qword ptr [esp]
   WriteBytes([$DD, $04, $24]);
end;

// _fstp_esp
//
procedure Tx86_32_WriteOnlyStream._fstp_esp;
begin
   // fstp qword ptr [esp]
   WriteBytes([$DD, $1C, $24]);
end;

// _ffree
//
procedure Tx86_32_WriteOnlyStream._ffree(n : Integer);
begin
   // ffree st(n)
   Assert(n in [0..7]);
   WriteBytes([$DD, $C0+n]);
end;

// _call_reg
//
procedure Tx86_32_WriteOnlyStream._call_reg(reg : TgpRegister; offset : Integer);
begin
   WriteByte($FF);
   _modRMSIB_ptr_reg($10, reg, offset)
end;

// _call_absmem
//
procedure Tx86_32_WriteOnlyStream._call_absmem(ptr : Pointer);
begin
   WriteBytes([$FF, $15]);
   WritePointer(ptr);
end;

// _test_al_al
//
procedure Tx86_32_WriteOnlyStream._test_al_al;
begin
   WriteBytes([$84, $C0]);
end;

{$endif WIN32}

// ------------------
// ------------------ Tx86_64_WriteOnlyStream ------------------
// ------------------

{$ifdef WIN64}

// Create
//
constructor Tx86_64_WriteOnlyStream.Create;
begin
   inherited;
   //FSupportsAVX := True; // experimental
end;

// _modRMSIB_regnum_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._modRMSIB_regnum_ptr_reg(const prefix, opCode : array of Byte; destNum : Integer; src : TgpRegister64; offset : Integer);
begin
   WriteBytes(prefix);
   if destNum >= 8 then
      if src >= gprR8 then
         WriteByte($45)
      else WriteByte($44)
   else if src >= gprR8 then
      WriteByte($41);
   WriteBytes(opCode);
   _modRMSIB_ptr_reg8((destNum and 7) shl 3, Ord(src) and 7, offset);
end;

// _modRMSIB_ptr_reg8_reg8
//
procedure Tx86_64_WriteOnlyStream._modRMSIB_ptr_reg8_reg8(rm : Integer; base, index : Integer; scale, offset : Integer);
var
   sib : Integer;
begin
   Assert(scale in [1, 2, 4, 8]);

   if (index=Ord(gprESP)) and (base<>Ord(gprESP)) then begin
      Assert(scale=1);
      _modRMSIB_ptr_reg8_reg8(rm, index, base, 1, offset);
   end;

   if (offset=0) and (base<>Ord(gprEBP)) then
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

// _vex_modRMSIB_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vex_ps_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TxmmRegister);
begin
   if src2 > xmm7 then
      WriteBytes([ $c4, $c1 ])
   else WriteByte($c5);
   WriteByte($f8 - Ord(Ord(dest) >= 8)*$80 - Ord(src1)*8);
   _modRMSIB_reg8_reg8(opCode, Ord(dest) and 7, Ord(src2) and 7);
end;

// _vex_pd_modRMSIB_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vex_pd_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TymmRegister);
begin
   if src2 > ymm7 then begin
      WriteBytes([
         $c4,
         $c1 - Ord(Ord(dest) >= 8)*$80,
         $7d - Ord(Ord(src1) >= 8)*$40 - (Ord(src1) and 7)*8,
         opCode,
         $c0 + (Ord(src2) and 7) + (Ord(dest) and 7)*8
      ]);
   end else begin
      WriteBytes([
         $c5,
         $fd - Ord(Ord(dest) >= 8)*$80 - Ord(Ord(src1) >= 8)*$40 - (Ord(src1) and 7) * 8,
         opCode,
         $c0 + (Ord(src2) and 7) + (Ord(dest) and 7)*8
      ]);
   end;
end;

// _vex_ss_modRMSIB_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vex_ss_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TxmmRegister);
begin
   Assert(src1 <= xmm7);
   if src2 > xmm7 then
      WriteBytes([ $c4, $c1 ])
   else WriteByte($c5);
   WriteBytes([
      $fa - Ord(dest > xmm7)*$80 - Ord(src1)*8,
      opCode,
      $C0 + (Ord(src2) and 7) + (Ord(dest) and 7)*8
   ]);
end;

// _vex_sd_modRMSIB_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vex_sd_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TxmmRegister);
begin
   if src2 > xmm7 then
      WriteBytes([ $c4, $c1 ])
   else WriteByte($c5);
   WriteByte($fb - Ord(Ord(dest) >= 8)*$80 - Ord(src1)*8);
   _modRMSIB_reg8_reg8(opCode, Ord(dest) and 7, Ord(src2) and 7);
end;

// _vex_dq_modRMSIB_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vex_dq_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TymmRegister; with4th : Boolean = False);
begin
   WriteBytes([
      $c4,
      $e2 - Ord(Ord(dest) >= 8)*$80 - Ord(Ord(src2) >= 8)*$20 + Ord(with4th),
      $7d - Ord(Ord(src1) >= 8)*$40 - (Ord(src1) and 7)*8
   ]);
   _modRMSIB_reg8_reg8(opCode, Ord(dest) and 7, Ord(src2) and 7);
end;

// _vex_modRMSIB_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vex_modRMSIB_reg_reg(const opCode : Byte; dest, src1, src2 : TymmRegister);
begin
   Assert(dest <= ymm7);
   Assert(src1 <= ymm7);
   Assert(src2 <= ymm7);
   WriteBytes([ $c5, $fc - Ord(src1)*8]);
   _modRMSIB_reg_reg(opCode, dest, src2);
end;

// _vex_modRMSIB_reg_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vex_modRMSIB_reg_reg_ptr_reg(const opCode : Byte; dest, src1 : TymmRegister; src2 : TgpRegister64; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg(
      [$c5, $fc - Ord(dest > ymm7)*$80 - Ord(src1 > ymm7)*$40 - (Ord(src1) and 7)*8],
      opCode, Ord(dest) and 7, src2, offset);
end;

// _vex_modRMSIB_reg_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vex_modRMSIB_reg_reg_ptr_reg(const opCode : Byte; dest, src1 : TxmmRegister; src2 : TgpRegister64; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg(
      [$c5, $f8 - Ord(dest > xmm7)*$80 - Ord(src1 > xmm7)*$40 - (Ord(src1) and 7)*8],
      opCode, Ord(dest) and 7, src2, offset);
end;

// _vex_ps_modRMSIB_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vex_ps_modRMSIB_reg_ptr_reg(const opCode : Byte; dest : TymmRegister; src : TgpRegister64; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg(
      [$c5, $7c + 8*Ord(dest >= ymm8)],
      opCode, Ord(dest) and 7, src, offset);
end;

// _vex_pd_modRMSIB_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vex_pd_modRMSIB_reg_ptr_reg(const opCode : Byte; dest : TymmRegister; src : TgpRegister64; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg(
      [$c5, $fd - $80*Ord(dest >= ymm8)],
      opCode, Ord(dest) and 7, src, offset);
end;

// _vex_dq_modRMSIB_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vex_dq_modRMSIB_reg_ptr_reg(const opCode : Byte; dest : TymmRegister; src : TgpRegister64; offset : Integer);
begin
   WriteBytes([
      $c4,
      $e2 - Ord(Ord(dest) >= 8)*$80 - Ord(src >= gprR8)*$20,
      $7d,
      opCode
   ]);
   _modRMSIB_ptr_reg8((Ord(dest) and 7)*8, Ord(src) and 7, offset);
end;

// _vex_dqu_modRMSIB_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vex_dqu_modRMSIB_reg_ptr_reg(const opCode : Byte; dest : TymmRegister; src : TgpRegister64; offset : Integer);
begin
   WriteBytes([
      $c5,
      $fe - $80*Ord(dest >= ymm8),
      opCode
   ]);
   _modRMSIB_ptr_reg8((Ord(dest) and 7)*8, Ord(src) and 7, offset);
end;

// _vex_pd_modRMSIB_reg_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._vex_pd_modRMSIB_reg_ptr_indexed(const opCode : Byte; dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
begin
   Assert(base < gprR8);
   Assert(index < gprR8);
   WriteBytes([$c5, $fd - $80*Ord(dest >= ymm8), opCode]);
   _modRMSIB_ptr_reg8_reg8((Ord(dest) and 7)*8, Ord(base), Ord(index), scale, offset);
end;

// _vex_dq_modRMSIB_reg_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._vex_dq_modRMSIB_reg_ptr_indexed(const opCode : Byte; dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
begin
   Assert(base < gprR8);
   Assert(index < gprR8);
   WriteBytes([
      $c4,
      $e2 - Ord(Ord(dest) >= 8)*$80 - Ord(base >= gprR8)*$20,
      $7d,
      opCode
   ]);
   _modRMSIB_ptr_reg8_reg8((Ord(dest) and 7)*8, Ord(base), Ord(index), scale, offset);
end;

// _vex_dqu_modRMSIB_reg_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._vex_dqu_modRMSIB_reg_ptr_indexed(const opCode : Byte; dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
begin
   Assert(base < gprR8);
   Assert(index < gprR8);
   WriteBytes([
      $c5,
      $fe - $80*Ord(dest >= ymm8),
      opCode
   ]);
   _modRMSIB_ptr_reg8_reg8((Ord(dest) and 7)*8, Ord(base), Ord(index), scale, offset);
end;

// ClearFlags
//
procedure Tx86_64_WriteOnlyStream.ClearFlags;
begin
   FFlagCalls := False;
end;

// _mov_reg32_reg32
//
procedure Tx86_64_WriteOnlyStream._mov_reg32_reg32(dest, src : TgpRegister64);
begin
   if (dest >= gprR8) or (src >= gprR8) then
      WriteByte($40 + Ord(dest >= gprR8) + 4*Ord(src >= gprR8));
   WriteBytes([ $89, $C0 + (Ord(dest) and 7) + 8*(Ord(src) and 7) ]);
end;

// _mov_reg_reg
//
procedure Tx86_64_WriteOnlyStream._mov_reg_reg(dest, src : TgpRegister64);
begin
   if dest = src then Exit;
   WriteBytes([
      $48 + Ord(dest >= gprR8) + 4 * Ord(src >= gprR8),
      $89,
      $c0 + (Ord(dest) and 7) + 8 * (Ord(src) and 7)
   ]);
end;

// _mov_reg_reg
//
procedure Tx86_64_WriteOnlyStream._mov_reg_reg(dest : TxmmRegister; src : TgpRegister64);
begin
   WriteBytes([
      $66,
      $48 + Ord(dest >= xmm8) + 4 * Ord(src >= gprR8),
      $0F, $6E,
      $c0 + (Ord(dest) and 7) + 8 * (Ord(src) and 7)
   ]);
end;

// _mov_reg_dword
//
procedure Tx86_64_WriteOnlyStream._mov_reg_dword(reg : TgpRegister64d; imm : DWORD);
begin
   WriteByte($41);
   _mov_reg_dword(TgpRegister(Ord(reg) and 7), imm);
end;

// _mov_reg_qword
//
procedure Tx86_64_WriteOnlyStream._mov_reg_qword(reg : TgpRegister64; imm : QWORD);
begin
   if imm=0 then
      _xor_reg_reg(reg, reg)
   else if Int32(imm) = Int64(imm) then begin
      if (Int32(imm) > 0) and (reg < gprR8) then begin
         // 32 bit GPR assignment are zero extended on the higher bits
         WriteByte($B8 + Ord(reg));
      end else begin
         WriteBytes([
            $48 + Ord(reg >= gprR8),
            $c7,
            $c0 + (Ord(reg) and 7)
         ]);
      end;
      WriteInt32(imm);
   end else begin
      WriteBytes([$48 + Ord(reg >= gprR8), $b8 + (Ord(reg) and 7)]);
      WriteQWord(imm);
   end;
end;

// _mov_reg_imm
//
procedure Tx86_64_WriteOnlyStream._mov_reg_imm(reg : TgpRegister64; imm : Int64);
begin
   _mov_reg_qword(reg, QWORD(imm))
end;

// _mov_al_byte
//
procedure Tx86_64_WriteOnlyStream._mov_al_byte(imm : Byte);
begin
   WriteBytes([ $b0, imm ]);
end;

// _movsd_qword_ptr_reg_reg
//
procedure Tx86_64_WriteOnlyStream._movsd_qword_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);
begin
   Assert(src in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg([$F2], [$0F, $11], Ord(src), dest, offset);
end;

// _movsd_reg_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._movsd_reg_qword_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   Assert(dest in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg([$F2], [$0F, $10], Ord(dest), src, offset);
end;

// _movsd_reg_qword_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._movsd_reg_qword_ptr_indexed(dest : TxmmRegister; base, index : TgpRegister64; scale, offset : Integer);
begin
   Assert(dest in [xmm0..High(TxmmRegister)]);

   WriteByte($F2);

   if (dest >= xmm8) or (base >= gprR8) or (index >= gprR8) then
      WriteByte($40 + 4*Ord(dest >= xmm8) + 2*Ord(index >= gprR8) + Ord(base >= gprR8));

   WriteBytes([ $0F, $10 ]);

   _modRMSIB_ptr_reg8_reg8((Ord(dest) and 7)*8, Ord(base) and 7, Ord(index) and 7, scale, offset);
end;

// _movsd_qword_ptr_indexed_reg
//
procedure Tx86_64_WriteOnlyStream._movsd_qword_ptr_indexed_reg(base, index : TgpRegister64; scale, offset : Integer; src : TxmmRegister);
begin
   Assert(src in [xmm0..High(TxmmRegister)]);

   WriteByte($F2);

   if (src >= xmm8) or (base >= gprR8) or (index >= gprR8) then
      WriteByte($40 + 4*Ord(src >= xmm8) + 2*Ord(index >= gprR8) + Ord(base >= gprR8));

   WriteBytes([ $0F, $11 ]);

   _modRMSIB_ptr_reg8_reg8((Ord(src) and 7)*8, Ord(base) and 7, Ord(index) and 7, scale, offset);
end;

// _movsd_reg_absmem
//
procedure Tx86_64_WriteOnlyStream._movsd_reg_absmem(reg : TxmmRegister; ptr : Pointer);
begin
   _mov_reg_qword(gprRAX, QWORD(ptr));
   _movsd_reg_qword_ptr_reg(reg, gprRAX, 0);
end;

// _movupd_reg_dqword_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._movupd_reg_dqword_ptr_indexed(dest : TxmmRegister; base, index : TgpRegister64; scale, offset : Integer);
begin
   Assert(dest in [xmm0..High(TxmmRegister)]);

   WriteByte($66);

   if (dest >= xmm8) or (base >= gprR8) or (index >= gprR8) then
      WriteByte($40 + 4*Ord(dest >= xmm8) + 2*Ord(index >= gprR8) + Ord(base >= gprR8));

   WriteBytes([ $0F, $10 ]);

   _modRMSIB_ptr_reg8_reg8((Ord(dest) and 7)*8, Ord(base) and 7, Ord(index) and 7, scale, offset);
end;

// _movupd_dqword_ptr_reg_reg
//
procedure Tx86_64_WriteOnlyStream._movupd_dqword_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);
begin
   Assert(src in [xmm0..High(TxmmRegister)]);

   _modRMSIB_regnum_ptr_reg([$66], [$0F, $11], Ord(src), dest, offset);
end;

// _mov_reg_qword_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._mov_reg_qword_ptr_indexed(dest, base, index : TgpRegister64; scale, offset : Integer);
begin
   WriteByte($48 + 4*Ord(dest >= gprR8) + 2*Ord(index >= gprR8) + Ord(base >= gprR8));
   WriteByte($8B);

   _modRMSIB_ptr_reg8_reg8((Ord(dest) and 7)*8, Ord(base) and 7, Ord(index) and 7, scale, offset);
end;

// _mov_qword_ptr_indexed_reg
//
procedure Tx86_64_WriteOnlyStream._mov_qword_ptr_indexed_reg(base, index : TgpRegister64; scale, offset : Integer; src : TgpRegister64);
begin
   WriteByte($48 + 4*Ord(src >= gprR8) + 2*Ord(index >= gprR8) + Ord(base >= gprR8));
   WriteByte($89);

   _modRMSIB_ptr_reg8_reg8((Ord(src) and 7)*8, Ord(base) and 7, Ord(index) and 7, scale, offset);
end;

// _mov_reg_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._mov_reg_qword_ptr_reg(dest, src : TgpRegister64; offset : Integer = 0);
begin
   WriteByte($48 + Ord(src >= gprR8) + 4*Ord(dest >= gprR8));
   WriteByte($8B);
   _modRMSIB_ptr_reg8(8*(Ord(dest) and 7), Ord(src) and 7, offset);
end;

// _mov_qword_ptr_reg_reg
//
procedure Tx86_64_WriteOnlyStream._mov_qword_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TgpRegister64);
begin
   WriteByte($48 + Ord(dest >= gprR8) + 4*Ord(src >= gprR8));
   WriteByte($89);
   _modRMSIB_ptr_reg8(8*(Ord(src) and 7), Ord(dest) and 7, offset);
end;

// _mov_reg_qword_ptr_imm
//
procedure Tx86_64_WriteOnlyStream._mov_reg_qword_ptr_imm(dest : TgpRegister64; offset : Integer; imm : Int64);
begin
   if Int64(Int32(imm)) = imm then begin
      WriteByte($48 + Ord(dest >= gprR8));
      WriteByte($C7);
      _modRMSIB_ptr_reg8(0, Ord(dest) and 7, offset);
      WriteInt32(imm);
   end else begin
      _mov_reg_imm(gprRAX, imm);
      _mov_qword_ptr_reg_reg(dest, offset, gprRAX);
   end;
end;

// _mov_reg_byte_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._mov_reg_byte_ptr_reg(dest, src : TgpRegister64; offset : Integer = 0);
begin
   // actuallay a movzx
   WriteByte($48 + Ord(src >= gprR8) + 4*Ord(dest >= gprR8));
   WriteByte($0F);
   WriteByte($B6);
   _modRMSIB_ptr_reg8(8*(Ord(dest) and 7), Ord(src) and 7, offset);
end;

// _mov_byte_ptr_reg_imm
//
procedure Tx86_64_WriteOnlyStream._mov_byte_ptr_reg_imm(dest : TgpRegister64; offset : Integer; imm : Byte);
begin
   if dest >= gprR8 then
      WriteByte($41);
   WriteByte($C6);
   _modRMSIB_ptr_reg8($00, Ord(dest) and 7, offset);
   WriteByte(imm);
end;

// _mov_execmem_imm
//
procedure Tx86_64_WriteOnlyStream._mov_execmem_imm(stackAddr : Integer; const imm : Int64);
begin
   _mov_reg_qword_ptr_imm(cExecMemGPR, StackAddrToOffset(stackAddr), imm);
end;

// _mov_execmem_reg
//
procedure Tx86_64_WriteOnlyStream._mov_execmem_reg(stackAddr : Integer; const reg : TgpRegister64);
begin
   _mov_qword_ptr_reg_reg(cExecMemGPR, StackAddrToOffset(stackAddr), reg);
end;

// _mov_reg_execmem
//
procedure Tx86_64_WriteOnlyStream._mov_reg_execmem(reg : TgpRegister64; stackAddr : Integer; offset : Integer = 0);
begin
   offset := offset + StackAddrToOffset(stackAddr);
   _mov_reg_qword_ptr_reg(reg, cExecMemGPR, offset);
end;

// _cmov
//
procedure Tx86_64_WriteOnlyStream._cmov(flags : TboolFlags; dest, src : TgpRegister64);
begin
   WriteBytes([
      $48 + 4*Ord(dest >= gprR8) + Ord(src >= gprR8),
      $0F, Ord(flags) - $30,
      $C0 + 8*(Ord(dest) and 7) + (Ord(src) and 7)
   ]);
end;

// _movsd_reg_reg
//
procedure Tx86_64_WriteOnlyStream._movsd_reg_reg(dest, src : TxmmRegister);
begin
   if dest = src then Exit;

   // really does a movaps to copy registers (shorter encoding)
   _modRMSIB_reg_reg([$0F, $28], dest, src);
end;

// _movsd_reg_execmem
//
procedure Tx86_64_WriteOnlyStream._movsd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
begin
   _movsd_reg_qword_ptr_reg(reg, cExecMemGPR, StackAddrToOffset(stackAddr));
end;

// _movsd_execmem_reg
//
procedure Tx86_64_WriteOnlyStream._movsd_execmem_reg(stackAddr : Integer; reg : TxmmRegister);
begin
   _movsd_qword_ptr_reg_reg(cExecMemGPR, StackAddrToOffset(stackAddr), reg);
end;

// _movsd_rsp_reg
//
procedure Tx86_64_WriteOnlyStream._movsd_rsp_reg(reg : TxmmRegister);
begin
   _movsd_qword_ptr_reg_reg(gprRSP, 0, reg);
end;

// _movsd_reg_rsp
//
procedure Tx86_64_WriteOnlyStream._movsd_reg_rsp(reg : TxmmRegister; offset : Integer = 0);
begin
   _movsd_reg_qword_ptr_reg(reg, gprRSP, offset);
end;

// _lea_reg_ptr_indexed_reg
//
procedure Tx86_64_WriteOnlyStream._lea_reg_ptr_indexed_reg(dest, base, index : TgpRegister64; scale, offset : Integer);
begin
   WriteByte($48 + 4*Ord(dest >= gprR8) + 2*Ord(index >= gprR8) + Ord(base >= gprR8));
   WriteByte($8D);

   _modRMSIB_ptr_reg8_reg8((Ord(dest) and 7)*8, Ord(base) and 7, Ord(index) and 7, scale, offset);
end;

// _lea_reg_reg
//
procedure Tx86_64_WriteOnlyStream._lea_reg_reg(dest, base: TgpRegister64; offset : Integer);
begin
   WriteBytes([
      $48 + 4*Ord(dest >= gprR8) + Ord(base >= gprR8),
      $8D
   ]);

   _modRMSIB_ptr_reg8((Ord(dest) and 7)*8, Ord(base) and 7, offset);
end;

// _movsd_rsp_reg
//
procedure Tx86_64_WriteOnlyStream._movsd_rsp_reg(offset : Integer; reg : TxmmRegister);
begin
   _movsd_qword_ptr_reg_reg(gprRSP, offset, reg);
end;

// _mov_reg_dword
//
procedure Tx86_64_WriteOnlyStream._mov_reg_dword(reg : TgpRegister64; imm : DWORD);
begin
   if imm=0 then
      _xor_reg_reg(reg, reg)
   else begin
      if reg < gprR8 then
         WriteByte($48)
      else WriteByte($49);
      WriteBytes([$c7, $c0 + (Ord(reg) and 7)]);
      WriteDWord(imm);
   end;
end;

// _op_reg_imm
//
procedure Tx86_64_WriteOnlyStream._op_reg_imm(const op : TgpOP; reg : TgpRegister64; value : Int64);
begin
   if Int32(value) = value then begin
      WriteByte($48 + Ord(reg >= gprR8));
      if Int8(value) = value then begin
         WriteByte($83);
         WriteByte(op.SIB + (Ord(reg) and 7));
         WriteByte(Byte(value));
      end else begin
         WriteByte($81);
         WriteByte(op.SIB + (Ord(reg) and 7));
         WriteInt32(value);
      end;
   end else begin
      if reg <> gprRAX then begin
         _mov_reg_imm(gprRAX, value);
         _op_reg_reg(op, reg, gprRAX);
      end else begin
         FFlagCalls := True; // TODO proper reservation
         _mov_qword_ptr_reg_reg(gprRSP, -8, gprRAX);
         _mov_reg_imm(gprRAX, value);
         _mov_qword_ptr_reg_reg(gprRSP, -16, gprRAX);
         _mov_reg_qword_ptr_reg(gprRAX, gprRSP, -8);
         _op_reg_qword_ptr_reg(op, reg, gprRSP, -16);
      end;
   end;
end;

// _op_reg_reg
//
procedure Tx86_64_WriteOnlyStream._op_reg_reg(const op : TgpOP; dest, src : TgpRegister64);
begin
   WriteByte($48 + Ord(dest >= gprR8) + Ord(src >= gprR8)*4);
   _op_reg_reg(op, TgpRegister(Ord(dest) and 7), TgpRegister(Ord(src) and 7));
end;

// _op_reg_execmem
//
procedure Tx86_64_WriteOnlyStream._op_reg_execmem(const op : TgpOP; reg : TgpRegister64; stackAddr : Integer);
begin
   _op_reg_qword_ptr_reg(op, reg, cExecMemGPR, StackAddrToOffset(stackAddr));
end;

// _op_reg_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._op_reg_qword_ptr_reg(const op : TgpOP; dest, operand : TgpRegister64; offset : Integer);
begin
   WriteBytes([
      $48 + Ord(operand >= gprR8) + Ord(dest >= gprR8)*4,
      op.RegReg or 2
   ]);
   _modRMSIB_ptr_reg8(8*(Ord(dest) and 7), Ord(operand) and 7, offset);
end;

// _imul_reg_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._imul_reg_qword_ptr_reg(dest, operand : TgpRegister64; offset : Integer);
begin
   WriteBytes([
      $48 + Ord(operand >= gprR8) + Ord(dest >= gprR8)*4,
      $0F, $AF
   ]);
   _modRMSIB_ptr_reg8(8*(Ord(dest) and 7), Ord(operand) and 7, offset);
end;

// _imul_reg_reg_imm
//
procedure Tx86_64_WriteOnlyStream._imul_reg_reg_imm(dest, operand : TgpRegister64; value : Int64);
var
   p : Integer;
begin
   case value of
      -1 : begin
         _mov_reg_reg(dest, operand);
         _neg_reg(dest);
      end;
      0 : _mov_reg_qword(dest, 0);
      1 : Exit;
      2 : _lea_reg_ptr_indexed_reg(dest, operand, operand, 1, 0);
      3 : _lea_reg_ptr_indexed_reg(dest, operand, operand, 2, 0);
      4 : begin
         _mov_reg_reg(dest, operand);
         _add_reg_reg(dest, dest);
      end;
      5 : _lea_reg_ptr_indexed_reg(dest, operand, operand, 4, 0);
      6 : begin
         _lea_reg_ptr_indexed_reg(dest, operand, operand, 2, 0);
         _add_reg_reg(dest, dest);
      end;
      8, 16 : begin
         _mov_reg_reg(dest, operand);
         _shift_reg_imm(gpSal, dest, WhichPowerOfTwo(value));
      end;
      9 : _lea_reg_ptr_indexed_reg(dest, operand, operand, 8, 0);
      10 : begin
         _lea_reg_ptr_indexed_reg(dest, operand, operand, 4, 0);
         _add_reg_reg(dest, dest);
      end;
      24 : begin   // SizeOf(Variant)
         _lea_reg_ptr_indexed_reg(dest, operand, operand, 2, 0);
         _shift_reg_imm(gpSal, dest, 3);
      end;
   else
      p := WhichPowerOfTwo(value);
      if p > 0 then begin
         _mov_reg_reg(dest, operand);
         _shift_reg_imm(gpSal, dest, WhichPowerOfTwo(value));
      end else begin
         WriteByte($48 + Ord(operand >= gprR8) + 4*Ord(dest >= gprR8));
         if Int8(value) = value then begin
            WriteBytes([ $6B, $C0 + (Ord(operand) and 7) + 8*(Ord(dest) and 7) ]);
            WriteByte(Byte(value));
         end else if Int32(value) = value then begin
            WriteBytes([ $69, $C0 + (Ord(operand) and 7) + 8*(Ord(dest) and 7) ]);
            WriteInt32(value);
         end else Assert(False);
      end;
   end;
end;

// _imul_reg_reg
//
procedure Tx86_64_WriteOnlyStream._imul_reg_reg(dest, operand : TgpRegister64);
begin
   WriteBytes([
      $48 + Ord(operand >= gprR8) + 4 * Ord(dest >= gprR8),
      $0F, $AF,
      $c0 + (Ord(operand) and 7) + 8 * (Ord(dest) and 7)
   ]);
end;

// _neg_reg
//
procedure Tx86_64_WriteOnlyStream._neg_reg(reg : TgpRegister64);
begin
   WriteBytes([ $48 + Ord(reg >= gprR8), $F7, $D8 + (Ord(reg) and 7) ]);
end;

// _shift_reg_imm
//
procedure Tx86_64_WriteOnlyStream._shift_reg_imm(shift : TgpShift; reg : TgpRegister64; value : Byte);
begin
   if value<>0 then begin
      WriteByte($48 + Ord(reg >= gprR8));
      if value=1 then
         WriteBytes([$D1, Ord(shift) + (Ord(reg) and 7)])
      else WriteBytes([$C1, Ord(shift) + (Ord(reg) and 7), value]);
   end;
end;

// _not_reg
//
procedure Tx86_64_WriteOnlyStream._not_reg(reg : TgpRegister64);
begin
   WriteBytes([ $48 + Ord(reg >= gprR8), $F7, $D0 + (Ord(reg) and 7) ]);
end;

// _cmp_reg_imm
//
procedure Tx86_64_WriteOnlyStream._cmp_reg_imm(reg : TgpRegister64; value : Int64);
begin
   _op_reg_imm(gpOp_cmp, reg, value);
end;

// _cmp_reg_reg
//
procedure Tx86_64_WriteOnlyStream._cmp_reg_reg(left, right : TgpRegister64);
begin
   _op_reg_reg(gpOp_cmp, left, right);
end;

// _cmp_qword_ptr_reg_imm
//
procedure Tx86_64_WriteOnlyStream._cmp_qword_ptr_reg_imm(reg : TgpRegister64; offset : Integer; value : Int64);
begin
   if Int8(value) = value then begin
      WriteByte($48 + Ord(reg >= gprR8));
      WriteByte($83);
      _modRMSIB_ptr_reg8($38, (Ord(reg) and 7), offset);
      WriteByte(Byte(value));
   end else if Int32(value) = value then begin
      WriteByte($48 + Ord(reg >= gprR8));
      WriteByte($81);
      _modRMSIB_ptr_reg8($38, (Ord(reg) and 7), offset);
      WriteInt32(value);
   end else begin
      Assert(False); // TODO
   end;
end;

// _cmp_reg_execmem
//
procedure Tx86_64_WriteOnlyStream._cmp_reg_execmem(reg : TgpRegister64; stackAddr : Integer);
begin
   _op_reg_execmem(gpOp_cmp, reg, stackAddr);
end;

// _cmp_reg_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._cmp_reg_qword_ptr_reg(left, right : TgpRegister64; offset : Integer);
begin
   _op_reg_qword_ptr_reg(gpOp_cmp, left, right, offset);
end;

// _cmp_execmem_imm
//
procedure Tx86_64_WriteOnlyStream._cmp_execmem_imm(stackAddr : Integer; value : Int64);
begin
   _cmp_qword_ptr_reg_imm(cExecMemGPR, StackAddrToOffset(stackAddr), value);
end;

// _cmp_execmem_reg
//
procedure Tx86_64_WriteOnlyStream._cmp_execmem_reg(stackAddr : Integer; reg : TgpRegister64);
begin
   _modRMSIB_regnum_ptr_reg([], [$39], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr));
end;

// _test_reg_reg
//
procedure Tx86_64_WriteOnlyStream._test_reg_reg(dest, src : TgpRegister64);
begin
   WriteByte($48 + Ord(dest >= gprR8) + 4*Ord(src >= gprR8));
   WriteByte($85);
   WriteByte($C0 + (Ord(dest) and 7) + (Ord(src) and 7)*8);
end;

// _test_reg_imm
//
procedure Tx86_64_WriteOnlyStream._test_reg_imm(reg : TgpRegister64; value : Int64);
begin
   if Int32(value) = value then begin
      case reg of
         gprRAX :
            WriteBytes([ $48, $A9 ]);
         gprRCX..gprRDI :
            WriteBytes([ $48, $F7, $C0 + (Ord(reg) and 7) ]);
      else
         WriteBytes([ $49, $F7, $C0 + (Ord(reg) and 7) ]);
      end;
      WriteInt32(value);
   end else Assert(False);
end;

// _inc
//
procedure Tx86_64_WriteOnlyStream._inc(reg : TgpRegister64);
begin
   WriteBytes([$48 + Ord(reg >= gprR8), $FF, $C0 + (Ord(reg) and 7)]);
end;

// _dec
//
procedure Tx86_64_WriteOnlyStream._dec(reg : TgpRegister64);
begin
   WriteBytes([$48 + Ord(reg >= gprR8), $FF, $C8 + (Ord(reg) and 7)]);
end;

// _add_reg_imm
//
procedure Tx86_64_WriteOnlyStream._add_reg_imm(reg : TgpRegister64; value : Int64);
begin
   case value of
      0 : ;
      1 : _inc(reg);
      -1 : _dec(reg);
   else
      _op_reg_imm(gpOp_add, reg, value);
   end;
end;

// _add_reg_reg
//
procedure Tx86_64_WriteOnlyStream._add_reg_reg(dest, src : TgpRegister64);
begin
   _op_reg_reg(gpOp_add, dest, src);
end;

// _sub_reg_imm
//
procedure Tx86_64_WriteOnlyStream._sub_reg_imm(reg : TgpRegister64; value : Int64);
begin
   case value of
      0 : ;
      1 : _dec(reg);
      -1 : _inc(reg);
   else
      _op_reg_imm(gpOp_sub, reg, value);
   end;
end;

// _sub_reg_reg
//
procedure Tx86_64_WriteOnlyStream._sub_reg_reg(dest, src : TgpRegister64);
begin
   _op_reg_reg(gpOp_sub, dest, src);
end;

// _xor_reg_reg
//
procedure Tx86_64_WriteOnlyStream._xor_reg_reg(dest, src : TgpRegister64);
begin
   _op_reg_reg(gpOp_xor, dest, src);
end;

// _inc
//
procedure Tx86_64_WriteOnlyStream._inc(reg : TgpRegister64d);
begin
   WriteByte($41);
   _dec(TgpRegister(Ord(reg) and 7));
end;

// _inc_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._inc_qword_ptr_reg(reg : TgpRegister64; offset : Integer);
begin
   if reg < gprR8 then
      WriteByte($48)
   else WriteByte($49);
   WriteByte($ff);
   _modRMSIB_ptr_reg8(0, Ord(reg) and 7, offset);
end;

// _dec_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._dec_qword_ptr_reg(reg : TgpRegister64; offset : Integer);
begin
   if reg < gprR8 then
      WriteByte($48)
   else WriteByte($49);
   WriteByte($ff);
   _modRMSIB_ptr_reg8($8, Ord(reg) and 7, offset);
end;

// _add_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._add_qword_ptr_reg(dest : TgpRegister64; offset : Integer; operand : TgpRegister64);
begin
   WriteBytes([
      $48 + Ord(dest >= gprR8) + 4*Ord(operand >= gprR8),
      $01
   ]);
   _modRMSIB_ptr_reg8($00, Ord(dest) and 7, offset);
end;

// _sub_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._sub_qword_ptr_reg(dest : TgpRegister64; offset : Integer; operand : TgpRegister64);
begin
   WriteBytes([
      $48 + Ord(dest >= gprR8) + 4*Ord(operand >= gprR8),
      $29
   ]);
   _modRMSIB_ptr_reg8($00, Ord(dest) and 7, offset);
end;

// _add_execmem_imm
//
procedure Tx86_64_WriteOnlyStream._add_execmem_imm(stackAddr : Integer; stepValue : Int64);
begin
   if stepValue = 0 then Exit;
   if stepValue = 1 then
      _inc_qword_ptr_reg(cExecMemGPR, StackAddrToOffset(stackAddr))
   else begin
      _mov_reg_imm(gprRAX, stepValue);
      _add_qword_ptr_reg(cExecMemGPR, StackAddrToOffset(stackAddr), gprRAX);
   end;
end;

// _sub_execmem_imm
//
procedure Tx86_64_WriteOnlyStream._sub_execmem_imm(stackAddr : Integer; stepValue : Int64);
begin
   if stepValue = 0 then Exit;
   if stepValue = 1 then
      _dec_qword_ptr_reg(cExecMemGPR, StackAddrToOffset(stackAddr))
   else begin
      _mov_reg_imm(gprRAX, stepValue);
      _sub_qword_ptr_reg(cExecMemGPR, StackAddrToOffset(stackAddr), gprRAX);
   end;
end;

// _add_execmem_reg
//
procedure Tx86_64_WriteOnlyStream._add_execmem_reg(stackAddr : Integer; operand : TgpRegister64);
begin
   _add_qword_ptr_reg(cExecMemGPR, StackAddrToOffset(stackAddr), operand);
end;

// _sub_execmem_reg
//
procedure Tx86_64_WriteOnlyStream._sub_execmem_reg(stackAddr : Integer; operand : TgpRegister64);
begin
   _sub_qword_ptr_reg(cExecMemGPR, StackAddrToOffset(stackAddr), operand);
end;

// _push_reg
//
procedure Tx86_64_WriteOnlyStream._push_reg(reg : TgpRegister64);
begin
   if reg >= gprR8 then
      WriteByte($41);
   WriteByte($50 + (Ord(reg) and 7));
end;

// _pop_reg
//
procedure Tx86_64_WriteOnlyStream._pop_reg(reg : TgpRegister64);
begin
   if reg >= gprR8 then
      WriteByte($41);
   WriteByte($58 + (Ord(reg) and 7));
end;

// _push_imm
//
procedure Tx86_64_WriteOnlyStream._push_imm(imm : DWORD);
begin
   WriteByte($68);
   WriteQWord(imm);
end;

// _call_absmem
//
procedure Tx86_64_WriteOnlyStream._call_absmem(ptr : Pointer);
begin
   FFlagCalls := True;
   _mov_reg_qword(gprRAX, QWORD(ptr));
   WriteBytes([$ff, $d0]);  // call rax
end;

// _call_reg
//
procedure Tx86_64_WriteOnlyStream._call_reg(reg : TgpRegister64; offset : Integer);
begin
   FFlagCalls := True;
   if reg >= gprR8 then
      WriteByte($41);
   WriteByte($FF);
   _modRMSIB_ptr_reg8($10, Ord(reg) and 7, offset);
end;

// _test_al_al
//
procedure Tx86_64_WriteOnlyStream._test_al_al;
begin
   WriteBytes([$84, $C0]);
end;

// _cvtsi2sd
//
procedure Tx86_64_WriteOnlyStream._cvtsi2sd(dest : TxmmRegister; src : TgpRegister64);
begin
   WriteBytes([
      $F2, $48 + Ord(src >= gprR8) + 4*Ord(dest >= xmm8),
      $0F, $2A, $c0 + (Ord(src) and 7) + 8*(Ord(dest) and 7)
   ]);
end;

// _cvtsd2si
//
procedure Tx86_64_WriteOnlyStream._cvtsd2si(dest : TgpRegister64; src : TxmmRegister);
begin
   WriteBytes([
      $F2, $48 + Ord(src >= xmm8) + 4*Ord(dest >= gprR8),
      $0F, $2D, $c0 + (Ord(src) and 7) + 8*(Ord(dest) and 7)
   ]);
end;

// _prefetch_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._prefetch_ptr_reg(src : TgpRegister64; offset : Integer);
begin
   WriteBytes([$0F, $0D]);
   _modRMSIB_ptr_reg8(0, Ord(src), offset);
end;

// _prefetcht0_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._prefetcht0_ptr_reg(src : TgpRegister64; offset : Integer);
begin
   WriteBytes([$0F, $18]);
   _modRMSIB_ptr_reg8(0, Ord(src), offset);
end;

// _movss_ptr_reg_reg
//
procedure Tx86_64_WriteOnlyStream._movss_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);
begin
   Assert(src <= xmm7);
   if dest >= gprR8 then
      WriteBytes([$f3, $41, $0F, $11])
   else WriteBytes([$f3, $0F, $11]);
   _modRMSIB_ptr_reg8(Ord(src)*8, Ord(dest) and 7, offset);
end;

// _movss_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._movss_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   WriteByte($f3);
   if dest > xmm7 then
      WriteByte($44);
   _modRMSIB_regnum_ptr_reg([], [$0F, $10], Ord(dest) and 7, src, offset);
end;

// _movaps_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._movaps_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   Assert(dest <= xmm7);
   _modRMSIB_regnum_ptr_reg([], [$0F, $28], Ord(dest), src, offset);
end;

// _movups_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._movups_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   if dest > xmm7 then
      WriteByte($44);
   _modRMSIB_regnum_ptr_reg([], [$0F, $10], Ord(dest) and 7, src, offset);
end;

// _xmm_reg_dword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._xmm_reg_dword_ptr_reg(op : TxmmOp; reg : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   if (reg < xmm8) and (src < gprR8) then
      _modRMSIB_regnum_ptr_reg([$F2], [$0F, Ord(op)], Ord(reg), src, offset)
   else _modRMSIB_regnum_ptr_reg([$F2], [$0F, Ord(op)],
                                 Ord(reg), src, offset);
end;

// _xmm_reg_qword_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._xmm_reg_qword_ptr_reg(op : TxmmOp; reg : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   Assert(reg in [xmm0..High(TxmmRegister)]);

   WriteBytes([
      $F2,
      $48 + Ord(src >= gprR8) + 4 * Ord(reg >= xmm8),
      $0F, Ord(op)
   ]);
   _modRMSIB_ptr_reg8(8*(Ord(reg) and 7), Ord(src) and 7, offset);
end;

// _xmm_reg_execmem
//
procedure Tx86_64_WriteOnlyStream._xmm_reg_execmem(op : TxmmOp; reg : TxmmRegister; stackAddr : Integer);
begin
   _xmm_reg_dword_ptr_reg(op, reg, cExecMemGPR, StackAddrToOffset(stackAddr));
end;

// _xmm_reg_absmem
//
procedure Tx86_64_WriteOnlyStream._xmm_reg_absmem(op : TxmmOp; reg : TxmmRegister; ptr : Pointer);
begin
   _mov_reg_qword(gprRAX, QWORD(ptr));
   _xmm_reg_dword_ptr_reg(op, reg, gprRAX, 0);
end;

// _xmm_pd_reg_reg
//
procedure Tx86_64_WriteOnlyStream._xmm_pd_reg_reg(op : TxmmOp_pd; dest, src : TxmmRegister);
begin
   WriteByte($66);
   _modRMSIB_reg_reg([$0F, Ord(op)], dest, src);
end;

// _comisd_reg_reg
//
procedure Tx86_64_WriteOnlyStream._comisd_reg_reg(dest, src : TxmmRegister);
begin
   WriteByte($66);
   if (dest >= xmm8) or (src >= xmm8) then
      WriteByte($40 + Ord(src >= xmm8) + 4*Ord(dest >= xmm8));
   WriteBytes([
      $0F, $2F,
      $C0 + 8*(Ord(dest) and 7) + (Ord(src) and 7)
   ]);
end;

// _comisd_reg_execmem
//
procedure Tx86_64_WriteOnlyStream._comisd_reg_execmem(reg : TxmmRegister; stackAddr : Integer);
begin
   _modRMSIB_regnum_ptr_reg([$66], [$0F, $2F], Ord(reg), cExecMemGPR, StackAddrToOffset(stackAddr));
end;

// _comisd_reg_absmem
//
procedure Tx86_64_WriteOnlyStream._comisd_reg_absmem(reg : TxmmRegister;  ptr : Pointer);
begin
   _mov_reg_qword(gprRAX, QWORD(ptr));
   _modRMSIB_regnum_ptr_reg([$66], [$0F, $2F], Ord(reg), gprRAX, 0);
end;

// _mulps_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._mulps_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   if dest > xmm7 then
      WriteByte($44);
   _modRMSIB_regnum_ptr_reg([], [$0F, $59], Ord(dest) and 7, src, offset);
end;

// _mulss_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._mulss_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([], [$F3, $0F, $59], Ord(dest), src, offset);
end;

// _vmovaps_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vmovaps_ptr_reg(dest : TymmRegister; src : TgpRegister64; offset : Integer);
begin
   _vex_ps_modRMSIB_reg_ptr_reg($28, dest, src, offset);
end;

// _vmovss_ptr_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vmovss_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);
begin
   if dest < gprR8 then
      WriteBytes([$c5, $fa, $11])
   else WriteBytes([$c4, $c1, $7a, $11]);
   _modRMSIB_ptr_reg8(Ord(src)*8, Ord(dest) and 7, offset);
end;

// _vmovss_reg_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vmovss_reg_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   if src < gprR8 then
      WriteBytes([$c5, $fa - Ord(dest > xmm7)*$80, $10])
   else WriteBytes([$c4, $c1 - Ord(dest > xmm7)*$80, $7a, $10]);
   _modRMSIB_ptr_reg8((Ord(dest) and 7)*8, Ord(src) and 7, offset);
end;

// _vmovups_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vmovups_ptr_reg(dest : TxmmRegister; src : TgpRegister64; offset : Integer);
begin
   _modRMSIB_regnum_ptr_reg([], [$C5, $f8  - Ord(dest > xmm7)*$80, $10], Ord(dest) and 7, src, offset);
end;

// _vmovups_ptr_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vmovups_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TxmmRegister);
begin
   if dest < gprR8 then
      WriteBytes([$c5, $f8, $11])
   else WriteBytes([$c4, $c1, $78, $11]);
   _modRMSIB_ptr_reg8(Ord(src)*8, Ord(dest) and 7, offset);
end;

// _vmovups_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vmovups_ptr_reg(dest : TymmRegister; src : TgpRegister64; offset : Integer);
begin
   _vex_ps_modRMSIB_reg_ptr_reg($10, dest, src, offset);
end;

// _vmovupd_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vmovupd_ptr_reg(dest : TymmRegister; src : TgpRegister64; offset : Integer);
begin
   _vex_pd_modRMSIB_reg_ptr_reg($10, dest, src, offset);
end;

// _vmovupd_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._vmovupd_ptr_indexed(dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
begin
   _vex_pd_modRMSIB_reg_ptr_indexed($10, dest, base, index, scale, offset);
end;

// _vmovdqu_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vmovdqu_ptr_reg(dest : TymmRegister; src : TgpRegister64; offset : Integer);
begin
   _vex_dqu_modRMSIB_reg_ptr_reg($6f, dest, src, offset);
end;

// _vmovdqu_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._vmovdqu_ptr_indexed(dest : TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
begin
   _vex_dqu_modRMSIB_reg_ptr_indexed($6f, dest, base, index, scale, offset);
end;

// _vmovupd_ptr_reg_reg
//
procedure Tx86_64_WriteOnlyStream._vmovupd_ptr_reg_reg(dest : TgpRegister64; offset : Integer; src : TymmRegister);
begin
   if dest < gprR8 then begin
      WriteBytes([
         $c5,
         $fd - $80*Ord(src >= ymm8),
         $11
      ]);
   end else begin
      WriteBytes([
         $c4,
         $c1 - $80*Ord(src >= ymm8),
         $7d,
         $11
      ]);
   end;
   _modRMSIB_ptr_reg8((Ord(src) and 7)*8, Ord(dest) and 7, offset);
end;

// _v_op_pd
//
procedure Tx86_64_WriteOnlyStream._v_op_pd(op : TxmmOp_pd; dest, src1, src2 : TymmRegister);
begin
   _vex_pd_modRMSIB_reg_reg(Ord(op), dest, src1, src2);
end;

// _vcmppd
//
procedure Tx86_64_WriteOnlyStream._vcmppd(dest, src1, src2 : TymmRegister; predicate : Byte);
begin
   _vex_pd_modRMSIB_reg_reg($C2, dest, src1, src2);
   WriteByte(predicate);
end;

// _vpcmpeqq
//
procedure Tx86_64_WriteOnlyStream._vpcmpeqq(dest, src1, src2 : TymmRegister);
begin
   _vex_dq_modRMSIB_reg_reg($29, dest, src1, src2);
end;

// _vpblendvb
//
procedure Tx86_64_WriteOnlyStream._vpblendvb(dest, src1, src2, src3 : TymmRegister);
begin
   _vex_dq_modRMSIB_reg_reg($4C, dest, src1, src2, True);
   WriteByte(Ord(src3) shl 4);
end;

// _vbroadcastsd
//
procedure Tx86_64_WriteOnlyStream._vbroadcastsd(dest: TymmRegister; src : TxmmRegister);
begin
   _vex_dq_modRMSIB_reg_reg($19, dest, ymm0, TymmRegister(src));
end;

// _vbroadcastsd_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vbroadcastsd_ptr_reg(dest: TymmRegister; src : TgpRegister64; offset : Integer);
begin
   _vex_dq_modRMSIB_reg_ptr_reg($19, dest, src, offset);
end;

// _vpbroadcastq_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vpbroadcastq_ptr_reg(dest: TymmRegister; src : TgpRegister64; offset : Integer);
begin
   _vex_dq_modRMSIB_reg_ptr_reg($59, dest, src, offset);
end;

// _vpbroadcastq_ptr_indexed
//
procedure Tx86_64_WriteOnlyStream._vpbroadcastq_ptr_indexed(dest: TymmRegister; base, index : TgpRegister64; scale, offset : Integer);
begin
   _vex_dq_modRMSIB_reg_ptr_indexed($59, dest, base, index, scale, offset);
end;

// _vxorps
//
procedure Tx86_64_WriteOnlyStream._vxorps(dest, src1, src2 : TymmRegister);
begin
   _vex_modRMSIB_reg_reg($57, dest, src1, src2);
end;

// _vxorps
//
procedure Tx86_64_WriteOnlyStream._vxorps(reg : TymmRegister);
begin
   _vxorps(reg, reg, reg);
end;

// _vaddss
//
procedure Tx86_64_WriteOnlyStream._vaddss(dest, src1, src2 : TxmmRegister);
begin
   _vex_ss_modRMSIB_reg_reg($58, dest, src1, src2);
end;

// _vaddps
//
procedure Tx86_64_WriteOnlyStream._vaddps(dest, src1, src2 : TxmmRegister);
begin
   _vex_ps_modRMSIB_reg_reg($58, dest, src1, src2);
end;

// _vaddpd
//
procedure Tx86_64_WriteOnlyStream._vaddpd(dest, src1, src2 : TymmRegister);
begin
   _v_op_pd(xmm_addpd, dest, src1, src2);
end;

// _vaddps
//
procedure Tx86_64_WriteOnlyStream._vaddps(dest, src1, src2 : TymmRegister);
begin
   _vex_modRMSIB_reg_reg($58, dest, src1, src2);
end;

// _vmulps
//
procedure Tx86_64_WriteOnlyStream._vmulps(dest, src1, src2 : TxmmRegister);
begin
   _vex_ps_modRMSIB_reg_reg($59, dest, src1, src2);
end;

// _vmulsd
//
procedure Tx86_64_WriteOnlyStream._vmulsd(dest, src1, src2 : TxmmRegister);
begin
   _vex_sd_modRMSIB_reg_reg($59, dest, src1, src2);
end;

// _vhaddps
//
procedure Tx86_64_WriteOnlyStream._vhaddps(dest, src1, src2 : TxmmRegister);
begin
   WriteBytes([ $c5, $fb - Ord(src1)*8 ]);
   _modRMSIB_reg_reg([ $7c ], dest, src2);
end;

// _vmovshdup
//
procedure Tx86_64_WriteOnlyStream._vmovshdup(dest, src : TxmmRegister);
begin
   _vex_ss_modRMSIB_reg_reg($16, dest, xmm0, src);
end;

// _vmovhlps
//
procedure Tx86_64_WriteOnlyStream._vmovhlps(dest, src1, src2 : TxmmRegister);
begin
   _vex_ps_modRMSIB_reg_reg($12, dest, src1, src2);
end;

// _vmulps
//
procedure Tx86_64_WriteOnlyStream._vmulps(dest, src1, src2 : TymmRegister);
begin
   _vex_modRMSIB_reg_reg($59, dest, src1, src2);
end;

// _vextract128_low
//
procedure Tx86_64_WriteOnlyStream._vextract128_low(dest : TxmmRegister; src : TymmRegister);
begin
   Assert(src <= ymm7);
   WriteBytes([$c4, $e3, $7d, $19, $c0 + Ord(dest) + Ord(Src)*8, $00]);
end;

// _vextract128_high
//
procedure Tx86_64_WriteOnlyStream._vextract128_high(dest : TxmmRegister; src : TymmRegister);
begin
   Assert(src <= ymm7);
   WriteBytes([$c4, $c3 + Ord(dest <= xmm7)*$20, $7d, $19, $c0 + (Ord(dest) and 7) + (Ord(Src) and 7)*8, $01]);
end;

// _vfmadd231ps
//
procedure Tx86_64_WriteOnlyStream._vfmadd231ps(dest, src1, src2 : TxmmRegister);
begin
   WriteBytes([$c4, $e2, $79 - Ord(src1)*8, $b8, $c0 + 8*Ord(dest) + Ord(src2)]);
end;

// _vfmadd231pd
//
procedure Tx86_64_WriteOnlyStream._vfmadd_pd(op : Integer; dest, src1, src2 : TymmRegister);
begin
   case op of
      132 : op := $98;
      213 : op := $A8;
      231 : op := $B8;
   else
      Assert(False, 'vfma op can only be 132, 213 or 231');
   end;
   WriteBytes([
      $c4,
      $e2 - Ord(Ord(dest) >= 8)*$80 - Ord(Ord(src2) >= 8)*$20,
      $fd - Ord(Ord(src1) >= 8)*$40 - (Ord(src1) and 7)*8,
      op,
      $C0 + (Ord(src2) and 7) + (Ord(dest) and 7)*8
   ]);
end;

// _vfmadd231ps
//
procedure Tx86_64_WriteOnlyStream._vfmadd231ps(dest, src1, src2 : TymmRegister);
begin
   Assert(dest <= ymm7);
   Assert(src1 <= ymm7);
   Assert(src2 <= ymm7);
   WriteBytes([$c4, $e2, $7d - Ord(src1)*8, $b8, $c0 + 8*Ord(dest) + Ord(src2)]);
end;

// _vzeroupper
//
procedure Tx86_64_WriteOnlyStream._vzeroupper;
begin
   WriteBytes([$c5, $f8, $77]);
end;

// _vzeroall
//
procedure Tx86_64_WriteOnlyStream._vzeroall;
begin
   WriteBytes([$c5, $fc, $77]);
end;

// _vmulps_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vmulps_ptr_reg(dest, src1 : TymmRegister; src2 : TgpRegister64; offset : Integer);
begin
   _vex_modRMSIB_reg_reg_ptr_reg($59, dest, src1, src2, offset);
end;

// _vmulps_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vmulps_ptr_reg(dest, src1 : TxmmRegister; src2 : TgpRegister64; offset : Integer);
begin
   _vex_modRMSIB_reg_reg_ptr_reg($59, dest, src1, src2, offset);
end;

// _vfmadd231ss_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vfmadd231ss_ptr_reg(dest, src1 : TxmmRegister; src2 : TgpRegister64; offset : Int32);
begin
   WriteBytes([$c4, $e2, $79 - Ord(src1 > xmm7)*$40 - (Ord(src1) and 7)*8, $b9]);
   _modRMSIB_ptr_reg8(Ord(dest) shl 3, Ord(src2), offset);
end;

// _vfmadd231ps_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vfmadd231ps_ptr_reg(dest, src1 : TxmmRegister; src2 : TgpRegister64; offset : Int32);
begin
   WriteBytes([$c4, $e2, $79 - Ord(src1 > xmm7)*$40 - (Ord(src1) and 7)*8, $b8]);
   _modRMSIB_ptr_reg8(Ord(dest) shl 3, Ord(src2), offset);
end;

// _vfmadd231pd_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vfmadd231pd_ptr_reg(dest, src1 : TymmRegister; src2 : TgpRegister64; offset : Int32);
begin
   WriteBytes([$c4, $e2, $f9 - Ord(src1 > ymm7)*$40 - (Ord(src1) and 7)*8, $b8]);
   _modRMSIB_ptr_reg8(Ord(dest) shl 3, Ord(src2), offset);
end;

// _vfmadd231ps_ptr_reg
//
procedure Tx86_64_WriteOnlyStream._vfmadd231ps_ptr_reg(dest, src1 : TymmRegister; src2 : TgpRegister64; offset : Int32);
begin
   Assert(dest <= ymm7);
   WriteBytes([$c4, $e2, $7d - (Ord(src1) and 7)*8 - $40*Ord(src1 > ymm7), $b8]);
   _modRMSIB_ptr_reg8(Ord(dest) shl 3, Ord(src2), offset);
end;

{$endif}

end.

