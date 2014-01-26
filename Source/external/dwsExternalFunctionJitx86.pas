unit dwsExternalFunctionJitx86;

interface
uses
   Generics.Collections,
   dwsTokenizer, dwsExternalFunctionJit, dwsExprs, dwsJITx86Intrinsics;

function JitFactory(conv: TTokenType; prog: TdwsProgram): IExternalFunctionJit;

implementation
uses
   SysUtils,
   dwsUtils, dwsSymbols, dwsExprList, dwsVMTOffsets;

type
   Tx86RegisterJit = class(TinterfacedObject, IExternalFunctionJit)
   private
      FProgram: TdwsProgram;
      FInitStream: Tx86WriteOnlyStream;
      FStream: Tx86WriteOnlyStream;
      FReturnValue: TTypeSymbol;
      FParams: integer;
      FRegParams: integer;
      FRegParamDepth: array[0..2] of byte;
      FCalls: TList<TFunctionCall>;

      function GetDepth(depth: byte): shortint;

      procedure InitParams;
      procedure AddCall(call: pointer; offset: integer);
      procedure WriteCall(loc: pointer);
      procedure PushParam(param: TParamSymbol; pType: TTypeSymbol);
      procedure RegPassParam(param: TParamSymbol; pType: TTypeSymbol);
      procedure WriteLoadVarParam(depth: shortint);
   private //IExternalFunctionJit implementation
      procedure BeginProcedure(paramCount: integer);
      procedure BeginFunction(retval: TTypeSymbol; paramCount: integer);
      procedure PassParam(param: TParamSymbol);
      procedure Call;
      procedure PostCall;
      function GetBytes: TBytes;
      function GetCalls: TArray<TFunctionCall>;
      function CallGetParam(pType: TTypeSymbol; index: integer): shortint;
   public
      constructor Create(prog: TdwsProgram);
      destructor Destroy; override;
   end;


function JitFactory(conv: TTokenType; prog: TdwsProgram): IExternalFunctionJit;
begin
   if conv = ttREGISTER then
      result := Tx86RegisterJit.Create(prog)
   else raise Exception.Create('Only REGISTER calling convention is supported so far');
end;

{ Tx86RegisterJit }

constructor Tx86RegisterJit.Create(prog: TdwsProgram);
begin
   FProgram := prog;
   FInitStream := Tx86WriteOnlyStream.Create;
   FStream := Tx86WriteOnlyStream.Create;
   FCalls := TList<TFunctionCall>.Create;
end;

destructor Tx86RegisterJit.Destroy;
begin
   FCalls.Free;
   FStream.Free;
   FInitStream.Free;
   inherited;
end;

procedure Tx86RegisterJit.BeginProcedure(paramCount: integer);
const
   SETUP_STACK:   array[0..5] of byte = ($51, $53, $56, $57, $8B, $DA); //push ECX; push EBX;
                                                                        //push ESI; push EDI;
                                                                        //mov ebx,edx
begin
   FParams := 0;
   FRegParams := 0;
   FInitStream._push_reg(gprEBP);
   FInitStream._mov_reg_reg(gprEBP, gprESP); //begin stack frame
   if ParamCount > 0 then
   begin
      FInitStream._add_reg_int32(gprESP, GetDepth(paramCount)); //reserve stack space
      FInitStream.WriteBytes(SETUP_STACK);
   end;
end;

procedure Tx86RegisterJit.InitParams;
const
   FETCH_ARRAY: array[0..11] of byte = (
     $8B, $03,           //mov eax,[ebx]
     $83, $78, $04, $01, //cmp dword ptr [eax+$04],$01
     $74, $02,           //jz +2
     $8B, $00,           //mov eax,[eax]
     $8B, $F0            //mov esi,eax
   );
begin
//   FInitStream.WriteBytes(FETCH_ARRAY);
   FInitStream._mov_reg_dword_ptr_reg(gprESI, gprEBX);
end;

procedure Tx86RegisterJit.AddCall(call: pointer; offset: integer);
var
   newCall: TFunctionCall;
begin
   newCall.call := NativeUInt(call);
   newCall.offset := offset;
   FCalls.Add(newCall);
end;

procedure Tx86RegisterJit.BeginFunction(retval: TTypeSymbol; paramCount: integer);
begin
   BeginProcedure(paramCount);
   FReturnValue := retval;
end;

procedure Tx86RegisterJit.PushParam(param: TParamSymbol; pType: TTypeSymbol);
begin
   raise Exception.Create('Stack params are not suported yet');
end;

procedure Tx86RegisterJit.WriteCall(loc: pointer);
begin
   FStream.WriteByte($E8); //CALL
   AddCall(loc, FInitStream.Size + FStream.Size);
   FStream.WriteDWord(0); //blank pointer
end;

function Tx86RegisterJit.GetDepth(depth: byte): shortint;
const OVERHEAD_SLOTS = 3;
begin
   result := ((depth + OVERHEAD_SLOTS) * sizeof(pointer));
   result := ($FF - result) + 1;
end;

procedure Tx86RegisterJit.WriteLoadVarParam(depth: shortint);
const
   LEA_PARAM:   array[0..1] of byte = ($8D, $4D);      //lea ecx,[ebp - ??]
begin
   FInitStream._xor_reg_reg(gprECX, gprECX);                  //zero ECX
   FInitStream._mov_dword_ptr_reg_reg(gprEBP, depth, gprECX); //zero stack location
   FStream.WriteBytes(LEA_PARAM);                             //load stack to ECX
   FStream.WriteByte(depth);
end;

function Tx86RegisterJit.CallGetParam(pType: TTypeSymbol; index: integer): shortint;
const
   STORE_VALUE:  array[0..1] of byte = ($89, $45);      //mov [ebp - ??],eax
var
   vmtSlot: byte;
   resultAsVar: boolean;
begin
   resultAsVar := false;
   if pType = FProgram.TypInteger then
      vmtSlot := vmt_TExprBase_EvalAsInteger
   else if pType = FProgram.TypString then
   begin
      vmtSlot := vmt_TExprBase_EvalAsString;
      resultAsVar := true;
   end
   else raise Exception.CreateFmt('Unsupported parameter type: %s', [pType.Name]);

   result := GetDepth(index);
   FStream._mov_reg_dword_ptr_reg(gprEAX, gprESI, index * sizeof(pointer)); //load param to EAX
   FStream._mov_reg_dword_ptr_reg(gprEDX, gprEBX, 8); //load exec to EDX
   if resultAsVar then
      WriteLoadVarParam(result);
   FStream._mov_reg_dword_ptr_reg(gprEDI, gprEAX);    //load VMT to EDI
   FStream._call_reg(gprEDI, vmtSlot);                //virtual method call

   if not resultAsVar then
      FStream._mov_dword_ptr_reg_reg(gprEBP, result, gprEAX);
end;

procedure Tx86RegisterJit.RegPassParam(param: TParamSymbol; pType: TTypeSymbol);
begin
   FRegParamDepth[FRegParams] := CallGetParam(pType, FParams);
   inc(FRegParams);
end;

procedure Tx86RegisterJit.PassParam(param: TParamSymbol);
var
   pType: TTypeSymbol;
   passOnStack: boolean;
begin
   pType := param.Typ;
   if pType is TRecordSymbol then
      raise Exception.Create('Record types are not supported');
   if pType is TFuncSymbol then
      raise Exception.Create('Function pointer types are not supported');
   if param.ClassType = TVarParamSymbol then
      raise Exception.Create('Var parameters are not supported');
   passOnStack := (FRegParams = 3) or (pType = FProgram.TypFloat) or (pType = FProgram.TypVariant);
   if FParams = 0 then
      InitParams;
   if passOnStack then
      PushParam(param, pType)
   else RegPassParam(param, pType);
   inc(FParams);
end;

procedure Tx86RegisterJit.Call;
const
   REG_OPS: array[1..3] of byte = ($45, $55, $4D);
var
   i: integer;
begin
   for i := FRegParams downto 1 do
   begin
      FStream.WriteByte($8B);
      FStream.WriteByte(REG_OPS[i]);
      FStream.WriteByte(FRegParamDepth[i - 1]);
   end;

   writeCall(nil);
end;

procedure Tx86RegisterJit.PostCall;
const
   RESTORE_STACK: array[0..5] of byte = ($5F, $5E, $5B, $59, $8B, $E5); //pop edi, pop esi, pop ebx,
                                                                        //pop ecx, mov esp,ebp
begin
   if FParams > 0 then
      FStream.WriteBytes(RESTORE_STACK);
   FStream.WriteByte($5D); //pop ebp
   FStream.WriteByte($C3); //ret
end;

function Tx86RegisterJit.GetBytes: TBytes;
begin
   FInitStream.WriteBytes(FStream.ToBytes);
   result := FInitStream.ToBytes;
end;

function Tx86RegisterJit.GetCalls: TArray<TFunctionCall>;
begin
   result := FCalls.ToArray;
end;

end.
