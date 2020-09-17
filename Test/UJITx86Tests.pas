unit UJITx86Tests;

interface

{$IF Defined(WIN32)}

uses
   Classes, SysUtils, Variants,
   dwsXPlatformTests, dwsJITx86Intrinsics, dwsUtils,
   BeaEngineDelphi, AnsiStrings;

type

   TJITx86Tests = class (TTestCase)
      private
         FStream : Tx86_32_WriteOnlyStream;

      protected
         function DisasmStream : String;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure xmm_ops;
         procedure comisd;
         procedure xor_ops;
         procedure movsd;
         procedure movq;
         procedure mov_reg_dword_ptr_reg;
         procedure mov_dword_ptr_reg_reg;
         procedure mov_ops;
         procedure mov_reg_reg;
         procedure mov_reg_dword;
         procedure inc_dec_64;
         procedure mov_64;
         procedure add_sub_32;
         procedure add_sub_execmem;
         procedure inc_dword_ptr;
         procedure add_sub_dword_ptr_reg;
         procedure and_or_xor_dword_ptr_reg;
         procedure neg_not_32;
         procedure shr_shl_32;
         procedure shr_shl_64;
         procedure xor_and_or_cmp_32;
         procedure xor_and_or_cmp_reg;
         procedure mul_imul_reg;
         procedure mul_imul_dword_ptr_reg;
         procedure fpu_ops;
         procedure push_pop;
         procedure nops;
         procedure calls;
         procedure cmp_execmem_int32;
         procedure cmp_dword_ptr_reg_reg;
         procedure cmp_reg_int32;
         procedure test_reg_reg;
         procedure test_reg_int32;
         procedure test_dword_ptr_reg_int32;
         procedure test_dword_ptr_reg_byte;
         procedure test_dword_ptr_reg_reg;
         procedure and_or_byte;
         procedure boolflags;
         procedure movsd_indexed;
         procedure mov_indexed;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TJITx86Tests ------------------
// ------------------

// SetUp
//
procedure TJITx86Tests.SetUp;
begin
   FStream:=Tx86_32_WriteOnlyStream.Create;
end;

// TearDown
//
procedure TJITx86Tests.TearDown;
begin
   FStream.Free;
end;

// DisasmStream
//
function TJITx86Tests.DisasmStream : String;
var
   b : TBytes;
   d : TDISASM;
   len : Integer;
   blockEnd : NativeInt;
begin
   Result:='';
   b:=FStream.ToBytes;
   FStream.Clear;

   if Length(b)=0 then Exit;

   FillChar(d, SizeOf(d), 0);
   d.EIP:=Integer(@b[0]);
   blockEnd:=d.EIP+Length(b);

   repeat
      len:=Disasm(d);
      if len>0 then begin
         FStream.WriteString(UTF8ToString(AnsiStrings.StrPas(d.CompleteInstr)));
         FStream.WriteCRLF;
         d.EIP:=d.EIP+len;
         d.SecurityBlock:=blockEnd-d.EIP;
      end else Break;
   until d.SecurityBlock<=0;
   Result:=FStream.ToString;
   FStream.Clear;
end;

// xmm_ops
//
procedure TJITx86Tests.xmm_ops;
begin
   FStream._xmm_reg_reg(xmm_addsd, xmm0, xmm1);
   FStream._xmm_reg_reg(xmm_subsd, xmm1, xmm2);
   FStream._xmm_reg_reg(xmm_multsd, xmm2, xmm3);
   FStream._xmm_reg_reg(xmm_divsd, xmm3, xmm4);
   FStream._xmm_reg_reg(xmm_minsd, xmm4, xmm5);
   FStream._xmm_reg_reg(xmm_maxsd, xmm6, xmm7);
   FStream._xmm_reg_reg(xmm_sqrtsd, xmm7, xmm0);
   FStream._xmm_reg_reg(xmm_cvtsi2sd, xmm6, xmm1);

   CheckEquals( 'addsd xmm0, xmm1'#13#10
               +'subsd xmm1, xmm2'#13#10
               +'mulsd xmm2, xmm3'#13#10
               +'divsd xmm3, xmm4'#13#10
               +'minsd xmm4, xmm5'#13#10
               +'maxsd xmm6, xmm7'#13#10
               +'sqrtsd xmm7, xmm0'#13#10
               +'cvtsi2sd xmm6, ecx'#13#10
               , DisasmStream);

   FStream._xmm_reg_execmem(xmm_addsd, xmm0, $11);
   FStream._xmm_reg_execmem(xmm_multsd, xmm1, $2233);
   FStream._xmm_reg_absmem(xmm_divsd, xmm2, Pointer($1234));

   CheckEquals( 'addsd xmm0, qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00000118h]'#13#10
               +'mulsd xmm1, qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00022338h]'#13#10
               +'divsd xmm2, qword ptr [00001234h]'#13#10
               , DisasmStream);
end;

// comisd
//
procedure TJITx86Tests.comisd;
begin
   FStream._comisd_reg_reg(xmm0, xmm7);
   FStream._comisd_reg_execmem(xmm1, $11);
   FStream._comisd_reg_execmem(xmm1, $2233);
   FStream._comisd_reg_absmem(xmm0, Pointer($1234));

   CheckEquals( 'comisd xmm0, xmm7'#13#10
               +'comisd xmm1, qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00000118h]'#13#10
               +'comisd xmm1, qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00022338h]'#13#10
               +'comisd xmm0, qword ptr [00001234h]'#13#10
               , DisasmStream);
end;

// xor_ops
//
procedure TJITx86Tests.xor_ops;
begin
   FStream._xorps_reg_reg(xmm1, xmm6);
   FStream._xor_reg_reg(gprEAX, gprEDI);
   FStream._xor_reg_reg(gprESI, gprECX);

   CheckEquals( 'xorps xmm1, xmm6'#13#10
               +'xor eax, edi'#13#10
               +'xor esi, ecx'#13#10
               , DisasmStream);
end;

// movsd
//
procedure TJITx86Tests.movsd;
begin
   FStream._movsd_reg_execmem(xmm3, $11);
   FStream._movsd_reg_execmem(xmm5, $1122);
   FStream._movsd_execmem_reg($22, xmm4);
   FStream._movsd_execmem_reg($2244, xmm6);
   FStream._movsd_reg_absmem(xmm0, Pointer($12345));
   FStream._movsd_reg_esp(xmm1);
   FStream._movsd_esp_reg(xmm2);
   FStream._movsd_qword_ptr_reg_reg(gprEDX, $11, xmm3);
   FStream._movsd_qword_ptr_reg_reg(gprECX, $1122, xmm4);
   FStream._movsd_reg_qword_ptr_reg(xmm3, gprEAX, $22);
   FStream._movsd_reg_qword_ptr_reg(xmm4, gprECX, $80);

   CheckEquals( 'movsd xmm3, qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00000118h]'#13#10
               +'movsd xmm5, qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00011228h]'#13#10
               +'movsd qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00000228h], xmm4'#13#10
               +'movsd qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00022448h], xmm6'#13#10
               +'movsd xmm0, qword ptr [00012345h]'#13#10
               +'movsd xmm1, qword ptr [esp]'#13#10
               +'movsd qword ptr [esp], xmm2'#13#10
               +'movsd qword ptr [edx+11h], xmm3'#13#10
               +'movsd qword ptr [ecx+00001122h], xmm4'#13#10
               +'movsd xmm3, qword ptr [eax+22h]'#13#10
               +'movsd xmm4, qword ptr [ecx+00000080h]'#13#10
               , DisasmStream);
end;

// movq
//
procedure TJITx86Tests.movq;
begin
   FStream._movq_execmem_reg($22, xmm4);
   FStream._movq_execmem_reg($2244, xmm6);
   FStream._movq_reg_absmem(xmm3, Pointer($11));

   CheckEquals( 'movq qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00000228h], xmm4'#13#10
               +'movq qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00022448h], xmm6'#13#10
               +'movq xmm3, qword ptr [00000011h]'#13#10
               , DisasmStream);
end;

// mov_reg_dword_ptr_reg
//
procedure TJITx86Tests.mov_reg_dword_ptr_reg;
var
   offset : Integer;
   dest, src : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      for src:=gprEAX to gprEDI do begin
         expect:='';
         for offset:=0 to 2 do begin
            FStream._mov_reg_dword_ptr_reg(dest, src, offset*$40);
            expect:=expect+'mov '+cgpRegisterName[dest]+', dword ptr ['
                          +cgpRegisterName[src];
            case offset of
               1 : expect:=expect+'+40h';
               2 : expect:=expect+'+00000080h';
            else
               if src=gprEBP then
                  expect:=expect+'+00h';
            end;
            expect:=expect+']'#13#10;
         end;
         CheckEquals(expect, DisasmStream);
      end;
   end;
end;

// mov_dword_ptr_reg_reg
//
procedure TJITx86Tests.mov_dword_ptr_reg_reg;
var
   offset : Integer;
   dest, src : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      for src:=gprEAX to gprEDI do begin
         expect:='';
         for offset:=0 to 2 do begin
            FStream._mov_dword_ptr_reg_reg(dest, offset*$40, src);
            expect:=expect+'mov dword ptr ['
                          +cgpRegisterName[dest];
            case offset of
               1 : expect:=expect+'+40h';
               2 : expect:=expect+'+00000080h';
            else
               if dest=gprEBP then
                  expect:=expect+'+00h';
            end;
            expect:=expect+'], '+cgpRegisterName[src]+#13#10;
         end;
         CheckEquals(expect, DisasmStream);
      end;
   end;
end;

// mov_ops
//
procedure TJITx86Tests.mov_ops;
begin
   FStream._mov_reg_execmem(gprEAX, $1);
   FStream._mov_reg_execmem(gprEDX, $100, 1);
   FStream._mov_execmem_reg($2, 2, gprECX);
   FStream._mov_execmem_reg($2233, 0, gprECX);

   CheckEquals( 'mov eax, dword ptr ['+cgpRegisterName[cExecMemGPR]+'+18h]'#13#10
               +'mov edx, dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00001009h]'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+2Ah], ecx'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00022338h], ecx'#13#10
               , DisasmStream);

   FStream._mov_eaxedx_execmem($1);
   FStream._mov_eaxedx_execmem($100);
   FStream._mov_execmem_eaxedx($1);
   FStream._mov_execmem_eaxedx($100);

   CheckEquals( 'mov eax, dword ptr ['+cgpRegisterName[cExecMemGPR]+'+18h]'#13#10
               +'mov edx, dword ptr ['+cgpRegisterName[cExecMemGPR]+'+1Ch]'#13#10
               +'mov eax, dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00001008h]'#13#10
               +'mov edx, dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0000100Ch]'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+18h], eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+1Ch], edx'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00001008h], eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0000100Ch], edx'#13#10
               , DisasmStream);

   FStream._mov_execmem_imm(0, 0);
   FStream._mov_execmem_imm(0, 1);
   FStream._mov_execmem_imm($100, 0);
   FStream._mov_execmem_imm($100, -1);
   FStream._mov_execmem_imm($100, $11);

   CheckEquals( 'xor eax, eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+08h], eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0Ch], eax'#13#10
               +'mov eax, 00000001h'#13#10
               +'xor edx, edx'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+08h], eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0Ch], edx'#13#10
               +'xor eax, eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00001008h], eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0000100Ch], eax'#13#10
               +'mov eax, FFFFFFFFh'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00001008h], eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0000100Ch], eax'#13#10
               +'mov eax, 00000011h'#13#10
               +'xor edx, edx'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00001008h], eax'#13#10
               +'mov dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0000100Ch], edx'#13#10
               , DisasmStream);

   FStream._mov_eaxedx_imm(0);
   FStream._mov_eaxedx_imm(1);
   FStream._mov_eaxedx_imm(-1);
   FStream._mov_eaxedx_imm($1122334455);
   FStream._mov_eaxedx_imm($11223344556677);

   CheckEquals( 'xor eax, eax'#13#10
               +'xor edx, edx'#13#10
               +'mov eax, 00000001h'#13#10
               +'xor edx, edx'#13#10
               +'mov eax, FFFFFFFFh'#13#10
               +'mov edx, FFFFFFFFh'#13#10
               +'mov eax, 22334455h'#13#10
               +'mov edx, 00000011h'#13#10
               +'mov eax, 44556677h'#13#10
               +'mov edx, 00112233h'#13#10
               , DisasmStream);
end;

// mov_reg_reg
//
procedure TJITx86Tests.mov_reg_reg;
var
   dest, src : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      expect:='';
      for src:=gprEAX to gprEDI do begin
         FStream._mov_reg_reg(dest, src);
         if dest<>src then
            expect:=expect+'mov '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10;
      end;
      CheckEquals(expect, DisasmStream);
   end;
end;

// mov_reg_dword
//
procedure TJITx86Tests.mov_reg_dword;
var
   dest : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      FStream._mov_reg_dword(dest, 0);
      FStream._mov_reg_dword(dest, 1);
      FStream._mov_reg_dword(dest, $80);
      expect:= 'xor '+cgpRegisterName[dest]+', '+cgpRegisterName[dest]+#13#10
              +'mov '+cgpRegisterName[dest]+', 00000001h'#13#10
              +'mov '+cgpRegisterName[dest]+', 00000080h'#13#10;
      CheckEquals(expect, DisasmStream);
   end;
end;

// inc_dec_64
//
procedure TJITx86Tests.inc_dec_64;
begin
   FStream._add_eaxedx_imm(0);
   FStream._add_eaxedx_imm(1);
   FStream._add_eaxedx_imm($200030001);
   FStream._add_eaxedx_imm($112200000033);

   FStream._execmem64_inc(0, 1);
   FStream._execmem64_inc(1, $80);
   FStream._execmem64_inc($80, $200030001);
   FStream._execmem64_dec(0, 1);
   FStream._execmem64_dec(1, $80);
   FStream._execmem64_dec($80, $200030001);

   CheckEquals( 'add eax, 01h'#13#10
               +'adc edx, 00000000h'#13#10
               +'add eax, 00030001h'#13#10
               +'adc edx, 02h'#13#10
               +'add eax, 33h'#13#10
               +'adc edx, 00001122h'#13#10
               +'add dword ptr ['+cgpRegisterName[cExecMemGPR]+'+08h], 01h'#13#10
               +'adc dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0Ch], 00000000h'#13#10
               +'add dword ptr ['+cgpRegisterName[cExecMemGPR]+'+18h], 00000080h'#13#10
               +'adc dword ptr ['+cgpRegisterName[cExecMemGPR]+'+1Ch], 00000000h'#13#10
               +'add dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00000808h], 00030001h'#13#10
               +'adc dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0000080Ch], 02h'#13#10
               +'sub dword ptr ['+cgpRegisterName[cExecMemGPR]+'+08h], 01h'#13#10
               +'sbb dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0Ch], 00000000h'#13#10
               +'sub dword ptr ['+cgpRegisterName[cExecMemGPR]+'+18h], 00000080h'#13#10
               +'sbb dword ptr ['+cgpRegisterName[cExecMemGPR]+'+1Ch], 00000000h'#13#10
               +'sub dword ptr ['+cgpRegisterName[cExecMemGPR]+'+00000808h], 00030001h'#13#10
               +'sbb dword ptr ['+cgpRegisterName[cExecMemGPR]+'+0000080Ch], 02h'#13#10
               , DisasmStream);
end;

// mov_64
//
procedure TJITx86Tests.mov_64;
var
   offset : Integer;
   reg : TgpRegister;
   expect, offtextA, offtextD : String;
begin
   for reg:=gprEAX to gprEDI do begin
      expect:='';
      for offset:=0 to 2 do begin
         FStream._mov_eaxedx_qword_ptr_reg(reg, offset*$40);
         FStream._mov_qword_ptr_reg_eaxedx(reg, offset*$40);
         if offset=0 then begin
            if reg=gprEBP then
               offtextA:='+00h'
            else offtextA:='';
            offtextD:='+04h';
         end else if offset=1 then begin
            offtextA:='+40h';
            offtextD:='+44h';
         end else begin
            offtextA:='+00000080h';
            offtextD:='+00000084h';
         end;
         if reg<>gprEAX then begin
            expect:= expect
                    +'mov eax, dword ptr ['+cgpRegisterName[reg]+offtextA+']'#13#10
                    +'mov edx, dword ptr ['+cgpRegisterName[reg]+offtextD+']'#13#10;
         end else begin
            expect:= expect
                    +'mov edx, dword ptr ['+cgpRegisterName[reg]+offtextD+']'#13#10
                    +'mov eax, dword ptr ['+cgpRegisterName[reg]+offtextA+']'#13#10;
         end;
         expect:=expect
                 +'mov dword ptr ['+cgpRegisterName[reg]+offtextA+'], eax'#13#10
                 +'mov dword ptr ['+cgpRegisterName[reg]+offtextD+'], edx'#13#10
                 ;
      end;
      CheckEquals(expect, DisasmStream);
   end;
end;

// add_sub_32
//
procedure TJITx86Tests.add_sub_32;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._add_reg_int32(reg, 1);
      FStream._add_reg_int32(reg, $80);
      FStream._adc_reg_int32(reg, 1);
      FStream._adc_reg_int32(reg, $80);
      FStream._sub_reg_int32(reg, 1);
      FStream._sub_reg_int32(reg, $80);
      FStream._sbb_reg_int32(reg, 1);
      FStream._sbb_reg_int32(reg, $80);
      expect:= 'add '+cgpRegisterName[reg]+', 01h'#13#10
              +'add '+cgpRegisterName[reg]+', 00000080h'#13#10
              +'adc '+cgpRegisterName[reg]+', 01h'#13#10
              +'adc '+cgpRegisterName[reg]+', 00000080h'#13#10
              +'sub '+cgpRegisterName[reg]+', 01h'#13#10
              +'sub '+cgpRegisterName[reg]+', 00000080h'#13#10
              +'sbb '+cgpRegisterName[reg]+', 01h'#13#10
              +'sbb '+cgpRegisterName[reg]+', 00000080h'#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
end;

// add_sub_execmem
//
procedure TJITx86Tests.add_sub_execmem;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._add_reg_execmem(reg, 1, 0);
      FStream._add_reg_execmem(reg, $80, 0);
      FStream._adc_reg_execmem(reg, 1, 0);
      FStream._adc_reg_execmem(reg, $80, 0);
      FStream._sub_reg_execmem(reg, 1, 0);
      FStream._sub_reg_execmem(reg, $80, 0);
      FStream._sbb_reg_execmem(reg, 1, 0);
      FStream._sbb_reg_execmem(reg, $80, 0);
      expect:= 'add '+cgpRegisterName[reg]+', dword ptr [ebx+18h]'#13#10
              +'add '+cgpRegisterName[reg]+', dword ptr [ebx+00000808h]'#13#10
              +'adc '+cgpRegisterName[reg]+', dword ptr [ebx+18h]'#13#10
              +'adc '+cgpRegisterName[reg]+', dword ptr [ebx+00000808h]'#13#10
              +'sub '+cgpRegisterName[reg]+', dword ptr [ebx+18h]'#13#10
              +'sub '+cgpRegisterName[reg]+', dword ptr [ebx+00000808h]'#13#10
              +'sbb '+cgpRegisterName[reg]+', dword ptr [ebx+18h]'#13#10
              +'sbb '+cgpRegisterName[reg]+', dword ptr [ebx+00000808h]'#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
   for reg:=gprEAX to gprEDI do begin
      FStream._add_execmem_reg(1, 0, reg);
      FStream._add_execmem_reg($80, 0, reg);
      FStream._adc_execmem_reg(1, 0, reg);
      FStream._adc_execmem_reg($80, 0, reg);
      FStream._sub_execmem_reg(1, 0, reg);
      FStream._sub_execmem_reg($80, 0, reg);
      FStream._sbb_execmem_reg(1, 0, reg);
      FStream._sbb_execmem_reg($80, 0, reg);
      expect:= 'add dword ptr [ebx+18h], '+cgpRegisterName[reg]+#13#10
              +'add dword ptr [ebx+00000808h], '+cgpRegisterName[reg]+#13#10
              +'adc dword ptr [ebx+18h], '+cgpRegisterName[reg]+#13#10
              +'adc dword ptr [ebx+00000808h], '+cgpRegisterName[reg]+#13#10
              +'sub dword ptr [ebx+18h], '+cgpRegisterName[reg]+#13#10
              +'sub dword ptr [ebx+00000808h], '+cgpRegisterName[reg]+#13#10
              +'sbb dword ptr [ebx+18h], '+cgpRegisterName[reg]+#13#10
              +'sbb dword ptr [ebx+00000808h], '+cgpRegisterName[reg]+#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
end;

// inc_dword_ptr
//
procedure TJITx86Tests.inc_dword_ptr;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._inc_dword_ptr_reg(reg, 0);
      FStream._inc_dword_ptr_reg(reg, $40);
      FStream._inc_dword_ptr_reg(reg, $80);
      if reg=gprEBP then
         expect:='inc dword ptr ['+cgpRegisterName[reg]+'+00h]'#13#10
      else expect:='inc dword ptr ['+cgpRegisterName[reg]+']'#13#10;
      expect:= expect
              +'inc dword ptr ['+cgpRegisterName[reg]+'+40h]'#13#10
              +'inc dword ptr ['+cgpRegisterName[reg]+'+00000080h]'#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
end;

// add_sub_dword_ptr_reg
//
procedure TJITx86Tests.add_sub_dword_ptr_reg;
var
   dest, src : TgpRegister;
   offset : Integer;
   expect, offtextA : String;
begin
   for dest:=gprEAX to gprEDI do begin
      for src:=gprEAX to gprEDI do begin
         expect:='';
         for offset:=0 to 2 do begin
            FStream._add_reg_dword_ptr_reg(dest, src, offset*$40);
            FStream._adc_reg_dword_ptr_reg(dest, src, offset*$40);
            FStream._sub_reg_dword_ptr_reg(dest, src, offset*$40);
            FStream._sbb_reg_dword_ptr_reg(dest, src, offset*$40);
            if offset=0 then begin
               if src=gprEBP then
                  offtextA:='+00h'
               else offtextA:='';
            end else if offset=1 then begin
               offtextA:='+40h';
            end else begin
               offtextA:='+00000080h';
            end;
            expect:= expect
                    +'add '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
                    +'adc '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
                    +'sub '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
                    +'sbb '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
                    ;
         end;
         CheckEquals(expect, DisasmStream);
      end;
   end;
end;

// and_or_xor_dword_ptr_reg
//
procedure TJITx86Tests.and_or_xor_dword_ptr_reg;
var
   dest, src : TgpRegister;
   offset : Integer;
   expect, offtextA : String;
begin
   for dest:=gprEAX to gprEDI do begin
      for src:=gprEAX to gprEDI do begin
         expect:='';
         for offset:=0 to 2 do begin
            FStream._and_reg_dword_ptr_reg(dest, src, offset*$40);
            FStream._or_reg_dword_ptr_reg(dest, src, offset*$40);
            FStream._xor_reg_dword_ptr_reg(dest, src, offset*$40);
            if offset=0 then begin
               if src=gprEBP then
                  offtextA:='+00h'
               else offtextA:='';
            end else if offset=1 then begin
               offtextA:='+40h';
            end else begin
               offtextA:='+00000080h';
            end;
            expect:= expect
                    +'and '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
                    +'or '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
                    +'xor '+cgpRegisterName[dest]+', dword ptr ['+cgpRegisterName[src]+offtextA+']'#13#10
                    ;
         end;
         CheckEquals(expect, DisasmStream);
      end;
   end;
end;

// neg_not_32
//
procedure TJITx86Tests.neg_not_32;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._neg_reg(reg);
      FStream._not_reg(reg);
      expect:=expect+'neg '+cgpRegisterName[reg]+#13#10
                    +'not '+cgpRegisterName[reg]+#13#10
              ;
   end;
   CheckEquals(expect, DisasmStream);
end;

// shr_shl_32
//
procedure TJITx86Tests.shr_shl_32;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._shift_reg_cl(gpShr, reg);
      FStream._shift_reg_imm(gpShr, reg, 1);
      FStream._shift_reg_imm(gpShr, reg, $7F);
      FStream._shift_reg_cl(gpShl, reg);
      FStream._shift_reg_imm(gpShl, reg, 1);
      FStream._shift_reg_imm(gpShl, reg, $7F);
      FStream._shift_reg_cl(gpSar, reg);
      FStream._shift_reg_imm(gpSar, reg, 1);
      FStream._shift_reg_imm(gpSar, reg, $7F);
      FStream._shift_reg_cl(gpSal, reg);
      FStream._shift_reg_imm(gpSal, reg, 1);
      FStream._shift_reg_imm(gpSal, reg, $7F);
      expect:= 'shr '+cgpRegisterName[reg]+', cl'#13#10
              +'shr '+cgpRegisterName[reg]+', 1 '#13#10
              +'shr '+cgpRegisterName[reg]+', 7Fh'#13#10
              +'shl '+cgpRegisterName[reg]+', cl'#13#10
              +'shl '+cgpRegisterName[reg]+', 1 '#13#10
              +'shl '+cgpRegisterName[reg]+', 7Fh'#13#10
              +'sar '+cgpRegisterName[reg]+', cl'#13#10
              +'sar '+cgpRegisterName[reg]+', 1 '#13#10
              +'sar '+cgpRegisterName[reg]+', 7Fh'#13#10
              +'sal '+cgpRegisterName[reg]+', cl'#13#10
              +'sal '+cgpRegisterName[reg]+', 1 '#13#10
              +'sal '+cgpRegisterName[reg]+', 7Fh'#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
end;

// shr_shl_64
//
procedure TJITx86Tests.shr_shl_64;
begin
   FStream._shr_eaxedx_imm(7);
   FStream._shl_eaxedx_imm(7);
   FStream._sar_eaxedx_imm(7);

   CheckEquals( 'shrd eax, edx, 00000007h'#13#10
               +'shr edx, 07h'#13#10
               +'shld edx, eax, 00000007h'#13#10
               +'shl eax, 07h'#13#10
               +'shrd eax, edx, 00000007h'#13#10
               +'sar edx, 07h'#13#10,
               DisasmStream);

   FStream._shr_eaxedx_cl;
   FStream._shl_eaxedx_cl;
   FStream._sar_eaxedx_cl;

   CheckEquals( 'shrd eax, edx, cl'#13#10
               +'shr edx, cl'#13#10
               +'shld edx, eax, cl'#13#10
               +'shl eax, cl'#13#10
               +'shrd eax, edx, cl'#13#10
               +'sar edx, cl'#13#10,
               DisasmStream);
end;

// xor_and_or_32
//
procedure TJITx86Tests.xor_and_or_cmp_32;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._op_reg_int32(gpOp_xor, reg, 1);
      FStream._op_reg_int32(gpOp_xor,reg, $80);
      FStream._op_reg_int32(gpOp_and, reg, 1);
      FStream._op_reg_int32(gpOp_and,reg, $80);
      FStream._op_reg_int32(gpOp_or, reg, 1);
      FStream._op_reg_int32(gpOp_or,reg, $80);
      FStream._op_reg_int32(gpOp_cmp, reg, 1);
      FStream._op_reg_int32(gpOp_cmp,reg, $80);
      expect:= 'xor '+cgpRegisterName[reg]+', 01h'#13#10
              +'xor '+cgpRegisterName[reg]+', 00000080h'#13#10
              +'and '+cgpRegisterName[reg]+', 01h'#13#10
              +'and '+cgpRegisterName[reg]+', 00000080h'#13#10
              +'or '+cgpRegisterName[reg]+', 01h'#13#10
              +'or '+cgpRegisterName[reg]+', 00000080h'#13#10
              +'cmp '+cgpRegisterName[reg]+', 01h'#13#10
              +'cmp '+cgpRegisterName[reg]+', 00000080h'#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
end;

// xor_and_or_cmp_reg
//
procedure TJITx86Tests.xor_and_or_cmp_reg;
var
   dest, src : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      for src:=gprEAX to gprEDI do begin
         FStream._op_reg_reg(gpOp_xor, dest, src);
         FStream._op_reg_reg(gpOp_and, dest, src);
         FStream._op_reg_reg(gpOp_or, dest, src);
         FStream._op_reg_reg(gpOp_cmp, dest, src);
         FStream._op_reg_reg(gpOp_add, dest, src);
         FStream._op_reg_reg(gpOp_adc, dest, src);
         FStream._op_reg_reg(gpOp_sub, dest, src);
         FStream._op_reg_reg(gpOp_sbb, dest, src);
         expect:= 'xor '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
                 +'and '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
                 +'or '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
                 +'cmp '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
                 +'add '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
                 +'adc '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
                 +'sub '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
                 +'sbb '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
              ;
         CheckEquals(expect, DisasmStream);
      end;
   end;
end;

// mul_imul_reg
//
procedure TJITx86Tests.mul_imul_reg;
var
   dest, src : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      FStream._mul_reg(dest);
      expect:='mul '+cgpRegisterName[dest]+#13#10;
      for src:=gprEAX to gprEDI do begin
         FStream._imul_reg_reg(dest, src);
         expect:= expect+'imul '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10
              ;
      end;
      CheckEquals(expect, DisasmStream);
   end;
end;

// mul_imul_dword_ptr_reg
//
procedure TJITx86Tests.mul_imul_dword_ptr_reg;
var
   reg, src : TgpRegister;
   offset : Integer;
   expect, offsetText : String;
begin
   for offset:=0 to 2 do begin
      for src:=gprEAX to gprEDI do begin
         case offset of
            1 : offsetText:='+40h';
            2 : offsetText:='+00000080h';
         else
            if src=gprEBP then
               offsetText:='+00h'
            else offsetText:='';
         end;
         expect:='mul dword ptr ['+cgpRegisterName[src]+offsetText+']'#13#10;
         FStream._mul_dword_ptr_reg(src, offset*$40);
         for reg:=gprEAX to gprEDI do begin
            FStream._imul_reg_dword_ptr_reg(reg, src, offset*$40);
            expect:= expect+'imul '+cgpRegisterName[reg]+', dword ptr ['+cgpRegisterName[src]+offsetText+']'#13#10
                 ;
         end;
         CheckEquals(expect, DisasmStream);
      end;
   end;
end;

// fpu_ops
//
procedure TJITx86Tests.fpu_ops;
begin
   FStream._fild_execmem(0);
   FStream._fild_execmem($11);
   FStream._fild_esp;
   FStream._fistp_esp;
   FStream._fld_esp;
   FStream._fstp_esp;

   CheckEquals( 'fild qword ptr ['+cgpRegisterName[cExecMemGPR]+'+08h]'#13#10
               +'fild qword ptr ['+cgpRegisterName[cExecMemGPR]+'+00000118h]'#13#10
               +'fild qword ptr [esp]'#13#10
               +'fistp qword ptr [esp]'#13#10
               +'fld qword ptr [esp]'#13#10
               +'fstp qword ptr [esp]'#13#10
               , DisasmStream);
end;

// push_pop
//
procedure TJITx86Tests.push_pop;
var
   dest : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      FStream._push_reg(dest);
      FStream._pop_reg(dest);
      expect:= 'push '+cgpRegisterName[dest]+#13#10
              +'pop '+cgpRegisterName[dest]+#13#10;
      CheckEquals(expect, DisasmStream);
   end;
end;

// nops
//
procedure TJITx86Tests.nops;
var
   i : Integer;
   buf : String;
begin
   for i:=0 to 15 do begin
      FStream._nop(i);
      FStream._ret;
   end;

   buf:=DisasmStream;
   buf:=StringReplace(buf, 'nop dword ptr [eax]'#13#10, 'n', [rfReplaceAll]);
   buf:=StringReplace(buf, 'nop '#13#10, 'n', [rfReplaceAll]);
   buf:=StringReplace(buf, 'ret '#13#10, 'r', [rfReplaceAll]);

   CheckEquals('rnrnrnrnnrnnrnnrnnnrnnnrnnnrnnnnrnnnnrnnnnrnnnnnrnnnnnrnnnnnr', buf);
end;

// calls
//
procedure TJITx86Tests.calls;
var
   offset : Integer;
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      for offset:=0 to 2 do
         FStream._call_reg(reg, offset*$40);
      expect:='call dword ptr ['+cgpRegisterName[reg];
      if reg=gprEBP then
         expect:=expect+'+00h';
      expect:=expect+']'#13#10;
      CheckEquals( expect
                  +'call dword ptr ['+cgpRegisterName[reg]+'+40h]'#13#10
                  +'call dword ptr ['+cgpRegisterName[reg]+'+00000080h]'#13#10
                  , DisasmStream);
   end;
end;

// cmp_execmem_int32
//
procedure TJITx86Tests.cmp_execmem_int32;
begin
   FStream._cmp_execmem_int32(0, 0, 0);
   FStream._cmp_execmem_int32(0, 1, 1);
   FStream._cmp_execmem_int32(1, 0, $10);
   FStream._cmp_execmem_int32(2, 2, $112233);

   CheckEquals( 'cmp dword ptr ['+cgpRegisterName[cExecMemGPR]+'+08h], 00000000h'#13#10
               +'cmp dword ptr ['+cgpRegisterName[cExecMemGPR]+'+09h], 01h'#13#10
               +'cmp dword ptr ['+cgpRegisterName[cExecMemGPR]+'+18h], 10h'#13#10
               +'cmp dword ptr ['+cgpRegisterName[cExecMemGPR]+'+2Ah], 00112233h'#13#10
               , DisasmStream);
end;

// cmp_dword_ptr_reg_reg
//
procedure TJITx86Tests.cmp_dword_ptr_reg_reg;
var
   offset : Integer;
   dest, src : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      for src:=gprEAX to gprEDI do begin
         expect:='';
         for offset:=0 to 2 do begin
            FStream._cmp_dword_ptr_reg_reg(dest, offset*$40, src);
            expect:=expect+'cmp dword ptr ['
                          +cgpRegisterName[dest];
            case offset of
               1 : expect:=expect+'+40h';
               2 : expect:=expect+'+00000080h';
            else
               if dest=gprEBP then
                  expect:=expect+'+00h';
            end;
            expect:=expect+'], '+cgpRegisterName[src]+#13#10;
         end;
         CheckEquals(expect, DisasmStream);
      end;
   end;
end;

// cmp_reg_int32
//
procedure TJITx86Tests.cmp_reg_int32;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._cmp_reg_int32(reg, $40);
      FStream._cmp_reg_int32(reg, $80);
      expect:= 'cmp '+cgpRegisterName[reg]+', 40h'#13#10
              +'cmp '+cgpRegisterName[reg]+', 00000080h'#13#10;
      CheckEquals(expect, DisasmStream);
   end;
end;

// test_reg_reg
//
procedure TJITx86Tests.test_reg_reg;
var
   dest, src : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      expect:='';
      for src:=gprEAX to gprEDI do begin
         FStream._test_reg_reg(dest, src);
         expect:=expect+'test '+cgpRegisterName[dest]+', '+cgpRegisterName[src]+#13#10;
      end;
      CheckEquals(expect, DisasmStream);
   end;
end;

// test_reg_int32
//
procedure TJITx86Tests.test_reg_int32;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._test_reg_imm(reg, $40);
      FStream._test_reg_imm(reg, $180);
      case reg of
         gprEAX : expect:='test '+cgpRegister8bitName[reg]+', 40h'#13#10;
         gprECX..gprEBX : expect:= 'test '+cgpRegister8bitName[reg]+', 00000040h'#13#10;
      else
         expect:= 'test '+cgpRegisterName[reg]+', 00000040h'#13#10;
      end;
      expect:=expect+'test '+cgpRegisterName[reg]+', 00000180h'#13#10;
      CheckEquals(expect, DisasmStream);
   end;
end;

// test_dword_ptr_reg_int32
//
procedure TJITx86Tests.test_dword_ptr_reg_int32;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._test_dword_ptr_reg_dword(reg, 0, $101);
      FStream._test_dword_ptr_reg_dword(reg, $40, $102);
      FStream._test_dword_ptr_reg_dword(reg, $80, $103);
      if reg=gprEBP then
         expect:='test dword ptr ['+cgpRegisterName[reg]+'+00h], 00000101h'#13#10
      else expect:='test dword ptr ['+cgpRegisterName[reg]+'], 00000101h'#13#10;
      expect:= expect
              +'test dword ptr ['+cgpRegisterName[reg]+'+40h], 00000102h'#13#10
              +'test dword ptr ['+cgpRegisterName[reg]+'+00000080h], 00000103h'#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
end;

// test_dword_ptr_reg_byte
//
procedure TJITx86Tests.test_dword_ptr_reg_byte;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._test_dword_ptr_reg_byte(reg, 0, 0);
      FStream._test_dword_ptr_reg_byte(reg, $40, 1);
      FStream._test_dword_ptr_reg_byte(reg, $80, 2);
      if reg=gprEBP then
         expect:='test byte ptr ['+cgpRegisterName[reg]+'+00h], 00000000h'#13#10
      else expect:='test byte ptr ['+cgpRegisterName[reg]+'], 00000000h'#13#10;
      expect:= expect
              +'test byte ptr ['+cgpRegisterName[reg]+'+40h], 00000001h'#13#10
              +'test byte ptr ['+cgpRegisterName[reg]+'+00000080h], 00000002h'#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
end;

// test_dword_ptr_reg_reg
//
procedure TJITx86Tests.test_dword_ptr_reg_reg;
var
   offset : Integer;
   dest, src : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      for src:=gprEAX to gprEDI do begin
         expect:='';
         for offset:=0 to 2 do begin
            FStream._test_dword_ptr_reg_reg(dest, offset*$40, src);
            expect:=expect+'test dword ptr ['
                          +cgpRegisterName[dest];
            case offset of
               1 : expect:=expect+'+40h';
               2 : expect:=expect+'+00000080h';
            else
               if dest=gprEBP then
                  expect:=expect+'+00h';
            end;
            expect:=expect+'], '+cgpRegisterName[src]+#13#10;
         end;
         CheckEquals(expect, DisasmStream);
      end;
   end;
end;

// and_or_byte
//
procedure TJITx86Tests.and_or_byte;
var
   reg : TgpRegister;
   expect : String;
begin
   for reg:=gprEAX to gprEDI do begin
      FStream._and_dword_ptr_reg_byte(reg, 0, 0);
      FStream._or_dword_ptr_reg_byte(reg, 0, 0);
      FStream._and_dword_ptr_reg_byte(reg, $40, 1);
      FStream._or_dword_ptr_reg_byte(reg, $40, 1);
      FStream._and_dword_ptr_reg_byte(reg, $80, 2);
      FStream._or_dword_ptr_reg_byte(reg, $80, 2);
      if reg=gprEBP then
         expect:= 'and byte ptr ['+cgpRegisterName[reg]+'+00h], 00000000h'#13#10
                 +'or byte ptr ['+cgpRegisterName[reg]+'+00h], 00000000h'#13#10
      else expect:='and byte ptr ['+cgpRegisterName[reg]+'], 00000000h'#13#10
                  +'or byte ptr ['+cgpRegisterName[reg]+'], 00000000h'#13#10;
      expect:= expect
              +'and byte ptr ['+cgpRegisterName[reg]+'+40h], 00000001h'#13#10
              +'or byte ptr ['+cgpRegisterName[reg]+'+40h], 00000001h'#13#10
              +'and byte ptr ['+cgpRegisterName[reg]+'+00000080h], 00000002h'#13#10
              +'or byte ptr ['+cgpRegisterName[reg]+'+00000080h], 00000002h'#13#10
              ;
      CheckEquals(expect, DisasmStream);
   end;
end;

// boolflags
//
procedure TJITx86Tests.boolflags;
begin
   FStream._set_al_flags(NegateBoolFlags(flagsA));
   FStream._set_al_flags(NegateBoolFlags(flagsAE));
   FStream._set_al_flags(NegateBoolFlags(flagsB));
   FStream._set_al_flags(NegateBoolFlags(flagsBE));
   FStream._set_al_flags(NegateBoolFlags(flagsE));
   FStream._set_al_flags(NegateBoolFlags(flagsG));
   FStream._set_al_flags(NegateBoolFlags(flagsGE));
   FStream._set_al_flags(NegateBoolFlags(flagsL));
   FStream._set_al_flags(NegateBoolFlags(flagsLE));
   FStream._set_al_flags(NegateBoolFlags(flagsNE));
   FStream._set_al_flags(NegateBoolFlags(flagsNO));
   FStream._set_al_flags(NegateBoolFlags(flagsNP));
   FStream._set_al_flags(NegateBoolFlags(flagsNS));
   FStream._set_al_flags(NegateBoolFlags(flagsO));
   FStream._set_al_flags(NegateBoolFlags(flagsP));
   FStream._set_al_flags(NegateBoolFlags(flagsS));

   CheckEquals( 'setbe al'#13#10
               +'setb al'#13#10
               +'setnb al'#13#10
               +'setnbe al'#13#10
               +'setne al'#13#10
               +'setle al'#13#10
               +'setl al'#13#10
               +'setnl al'#13#10
               +'setnle al'#13#10
               +'sete al'#13#10
               +'seto al'#13#10
               +'setp al'#13#10
               +'sets al'#13#10
               +'setno al'#13#10
               +'setnp al'#13#10
               +'setns al'#13#10
               , DisasmStream);
end;

// movsd_indexed
//
procedure TJITx86Tests.movsd_indexed;
var
   offset, scale : Integer;
   base, index : TgpRegister;
   dest : TxmmRegister;
   expect : String;
begin
   for dest:=xmm0 to xmm7 do begin
      for base:=gprEAX to gprEDI do begin
         for index:=gprEAX to gprEDI do begin
            if index=gprESP then continue;
            expect:='';
            for offset:=0 to 2 do begin
               for scale:=0 to 3 do begin
                  FStream._movsd_reg_qword_ptr_indexed(dest, base, index, 1 shl scale, offset*$40);
                  expect:=expect+'movsd xmm'+IntToStr(Ord(dest))
                                +', qword ptr ['+cgpRegisterName[base]
                                +'+'+cgpRegisterName[index];
                  case scale of
                     1 : expect:=expect+'*2';
                     2 : expect:=expect+'*4';
                     3 : expect:=expect+'*8';
                  end;
                  case offset of
                     1 : expect:=expect+'+40h';
                     2 : expect:=expect+'+00000080h';
                  else
                     if base=gprEBP then
                        expect:=expect+'+00h';
                  end;
                  expect:=expect+']'#13#10;
               end;
            end;
            CheckEquals(expect, DisasmStream);
            expect:='';
            for offset:=0 to 2 do begin
               for scale:=0 to 3 do begin
                  FStream._movsd_qword_ptr_indexed_reg(base, index, 1 shl scale, offset*$40, dest);
                  expect:=expect+'movsd qword ptr ['+cgpRegisterName[base]
                                +'+'+cgpRegisterName[index];
                  case scale of
                     1 : expect:=expect+'*2';
                     2 : expect:=expect+'*4';
                     3 : expect:=expect+'*8';
                  end;
                  case offset of
                     1 : expect:=expect+'+40h';
                     2 : expect:=expect+'+00000080h';
                  else
                     if base=gprEBP then
                        expect:=expect+'+00h';
                  end;
                  expect:=expect+'], xmm'+IntToStr(Ord(dest))+#13#10;
               end;
            end;
            CheckEquals(expect, DisasmStream);
         end;
      end;
   end;
end;

// mov_indexed
//
procedure TJITx86Tests.mov_indexed;
var
   offset, scale : Integer;
   dest, base, index : TgpRegister;
   expect : String;
begin
   for dest:=gprEAX to gprEDI do begin
      for base:=gprEAX to gprEDI do begin
         for index:=gprEAX to gprEDI do begin
            if index=gprESP then continue;
            expect:='';
            for offset:=0 to 2 do begin
               for scale:=0 to 3 do begin
                  FStream._mov_reg_dword_ptr_indexed(dest, base, index, 1 shl scale, offset*$40);
                  expect:=expect+'mov '+cgpRegisterName[dest]
                                +', dword ptr ['+cgpRegisterName[base]
                                +'+'+cgpRegisterName[index];
                  case scale of
                     1 : expect:=expect+'*2';
                     2 : expect:=expect+'*4';
                     3 : expect:=expect+'*8';
                  end;
                  case offset of
                     1 : expect:=expect+'+40h';
                     2 : expect:=expect+'+00000080h';
                  else
                     if base=gprEBP then
                        expect:=expect+'+00h';
                  end;
                  expect:=expect+']'#13#10;
               end;
            end;
            CheckEquals(expect, DisasmStream);
         end;
      end;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('jitx86Tests', TJITx86Tests);

{$else}
implementation
{$ifend}

end.
