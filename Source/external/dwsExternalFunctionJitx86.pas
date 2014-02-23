unit dwsExternalFunctionJitx86;

interface

uses
   dwsTokenizer,
   dwsExternalFunctionJit, dwsExprs, dwsJITx86Intrinsics;

function JitFactory(conv: TTokenType; prog: TdwsProgram): IExternalFunctionJit;

implementation

uses
   {$ifdef WIN32}
   Windows,
   {$endif}
   SysUtils,
   dwsUtils,
   dwsSymbols, dwsExprList, dwsVMTOffsets;

type
   TCleanup = record
      typ: TTypeSymbol;
      depth: shortint;
   end;

   Tx86RegisterJit = class(TinterfacedObject, IExternalFunctionJit)
   private type
      TStackLoc = record
         depth, size: integer;
         constructor Create(depth, size: integer);
      end;
      TResultStyle = (rsNormal, rsVar, rsFloat, rsObj);
   private
      FProgram: TdwsProgram;
      FInitStream: Tx86WriteOnlyStream;
      FStream: Tx86WriteOnlyStream;
      FReturnValue: TTypeSymbol;
      FParams: integer;
      FStackDepth: integer;
      FRegParams: integer;
      FRegParamDepth: array[0..2] of byte;
      FStackParams: array of TStackLoc;
      FCalls: TFunctionCallArray;
      FTryFrame: TTryFrame;
      FCleanups: array of TCleanup;

      function GetDepth(depth: byte): shortint;

      procedure InitParams;
      procedure AddCall(call: pointer; offset: integer);
      procedure WriteCall(loc: pointer);
      function typeSize(value: TTypeSymbol): integer;
      procedure WriteExtractObject(depth: shortint);
      procedure PushParam(param: TParamSymbol; pType: TTypeSymbol);
      procedure RegPassParam(param: TParamSymbol; pType: TTypeSymbol);
      function GetVmtSlot(pType: TTypeSymbol; out resultStyle: TResultStyle): byte;
      function CallGetParam(pType: TTypeSymbol; size: integer): shortint;
      procedure WriteLoadVarParam(depth: shortint);
      procedure AddCleanup(depth: shortint; pType: TTypeSymbol);
      procedure WriteCleanup;

   private //IExternalFunctionJit implementation
      procedure BeginProcedure(params: TParamsSymbolTable);
      procedure BeginFunction(retval: TTypeSymbol; params: TParamsSymbolTable);
      procedure PassParam(param: TParamSymbol);
      procedure Call;
      procedure PostCall;
      function GetBytes: TBytes;
      function GetCalls: TFunctionCallArray;
      function HasTryFrame: boolean;
      function GetTryFrame: TTryFrame;

   public
      constructor Create(prog: TdwsProgram);
      destructor Destroy; override;
   end;


function JitFactory(conv: dwsTokenizer.TTokenType; prog: TdwsProgram): IExternalFunctionJit;
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
end;

destructor Tx86RegisterJit.Destroy;
begin
   FStream.Free;
   FInitStream.Free;
   inherited;
end;

function Tx86RegisterJit.typeSize(value: TTypeSymbol): integer;
begin
   if value = FProgram.TypFloat then
      result := 2
   else if value is TClassSymbol then
      result := 2
   else result := 1;
end;

procedure Tx86RegisterJit.BeginProcedure(params: TParamsSymbolTable);
const
   SETUP_STACK:   array[0..5] of byte = ($51, $53, $56, $57, $8B, $DA); //push ECX; push EBX;
                                                                        //push ESI; push EDI;
                                                                        //mov ebx,edx
var
   i, paramDepth: integer;
begin
   FParams := 0;
   FRegParams := 0;
   FInitStream._push_reg(gprEBP);
   FInitStream._mov_reg_reg(gprEBP, gprESP); //begin stack frame
   if Params.Count > 0 then
   begin
      paramDepth := 0;
      for i := 0 to params.Count - 1 do
         inc(paramDepth, typeSize(params[i].typ));
      FInitStream._add_reg_int32(gprESP, GetDepth(paramDepth)); //reserve stack space
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
   n : Integer;
begin
   newCall.call := NativeUInt(call);
   newCall.offset := offset;
   n:=Length(FCalls);
   SetLength(FCalls, n+1);
   FCalls[n]:=newCall;
end;

procedure Tx86RegisterJit.BeginFunction(retval: TTypeSymbol; params: TParamsSymbolTable);
begin
   BeginProcedure(params);
   FReturnValue := retval;
end;

procedure Tx86RegisterJit.PushParam(param: TParamSymbol; pType: TTypeSymbol);
var
   i, size: integer;
begin
   i := length(FStackParams);
   setLength(FStackParams, i + 1);
   size := typeSize(pType);
   FStackParams[i] := TStackLoc.Create(CallGetParam(pType, size), size);
end;

procedure Tx86RegisterJit.WriteCall(loc: pointer);
begin
   FStream.WriteByte($E8); //CALL
   AddCall(loc, FInitStream.Size + FStream.Size);
   FStream.WriteDWord(0); //blank pointer
end;

function Tx86RegisterJit.GetDepth(depth: byte): shortint;
const OVERHEAD_SLOTS = 1;
begin
   result := ((depth + OVERHEAD_SLOTS) * sizeof(pointer));
   result := shortint(($FF - result) + 1);
end;

function Tx86RegisterJit.GetTryFrame: TTryFrame;
begin
   result := FTryFrame;
end;

function Tx86RegisterJit.HasTryFrame: boolean;
begin
   result := FTryFrame[0] <> 0;
end;

procedure Tx86RegisterJit.WriteLoadVarParam(depth: shortint);
const
   LEA_PARAM: array[0..1] of byte = ($8D, $4D);      //lea ecx,[ebp - ??]
begin
   if FTryFrame[0] = 0 then
      FTryFrame[0] := FInitStream._begin_tryf_frame;
   FInitStream._xor_reg_reg(gprECX, gprECX);                  //zero ECX
   FInitStream._mov_dword_ptr_reg_reg(gprEBP, depth, gprECX); //zero stack location
   FStream.WriteBytes(LEA_PARAM);                             //load stack to ECX
   FStream.WriteByte(byte(depth));
end;

procedure Tx86RegisterJit.AddCleanup(depth: shortint; pType: TTypeSymbol);
var
   n : Integer;
begin
   n:=Length(FCleanups);
   SetLength(FCleanups, n+1);
   FCleanups[n].typ:=pType;
   FCleanups[n].depth:=depth;
end;

function Tx86RegisterJit.GetVmtSlot(pType: TTypeSymbol; out resultStyle: TResultStyle): byte;
begin
   resultStyle := rsNormal;
   if pType = FProgram.TypInteger then
      result := vmt_TExprBase_EvalAsInteger
   else if pType = FProgram.TypBoolean then
      result := vmt_TExprBase_EvalAsBoolean
   else if pType = FProgram.TypString then
   begin
      result := vmt_TExprBase_EvalAsString;
      resultStyle := rsVar;
   end
   else if pType = FProgram.TypFloat then
   begin
      result := vmt_TExprBase_EvalAsFloat;
      resultStyle := rsFloat;
   end
   else if pType is TClassSymbol then
   begin
      result := vmt_TExprBase_EvalAsScriptObj;
      resultStyle := rsObj;
   end
   else raise Exception.CreateFmt('Unsupported parameter type: %s', [pType.Name]);
end;

procedure Tx86RegisterJit.WriteExtractObject(depth: shortint);
begin
   FStream._mov_reg_dword_ptr_reg(gprEAX, gprEBP, depth - sizeof(pointer));
   FStream._mov_reg_dword_ptr_reg(gprEDX, gprEAX);
   FStream._call_reg(gprEDX, vmt_IScriptObj_ExternalObject);
   FStream._mov_dword_ptr_reg_reg(gprEBP, depth, gprEAX);
end;

function Tx86RegisterJit.CallGetParam(pType: TTypeSymbol; size: integer): shortint;
var
   vmtSlot: byte;
   resultStyle: TResultStyle;
begin
   vmtSlot := GetVmtSlot(ptype, resultStyle);

   inc(FStackDepth, size);
   result := GetDepth(FStackDepth);
   FStream._mov_reg_dword_ptr_reg(gprEAX, gprESI, FParams * sizeof(pointer)); //load param to EAX
   FStream._mov_reg_dword_ptr_reg(gprEDX, gprEBX, 8); //load exec to EDX
   if resultStyle in [rsVar, rsObj] then
   begin
      WriteLoadVarParam(result);
      AddCleanup(result, pType);
   end;
   FStream._mov_reg_dword_ptr_reg(gprEDI, gprEAX);    //load VMT to EDI
   FStream._call_reg(gprEDI, vmtSlot);                //virtual method call

   if resultStyle = rsNormal then
      FStream._mov_dword_ptr_reg_reg(gprEBP, result, gprEAX)
   else if resultStyle = rsFloat then
   begin
      FStream.WriteBytes([$DD, $5D]); //fstp qword ptr [ebp - ??]
      FStream.WriteByte(byte(result));
   end
   else if resultStyle = rsObj then
   begin
      inc(result, sizeof(pointer));
      WriteExtractObject(result);
   end;
end;

procedure Tx86RegisterJit.RegPassParam(param: TParamSymbol; pType: TTypeSymbol);
begin
   FRegParamDepth[FRegParams] := byte(CallGetParam(pType, typeSize(pType)));
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
   i, depth: integer;
   loc: TStackLoc;
begin
   for loc in FStackParams do
   begin
      depth := loc.depth;
      for i := 1 to loc.size do
      begin
         FStream.WriteBytes([$FF, $75]); //push dword ptr
         FStream.WriteByte(byte(depth + ((loc.size - i) * 4)));
      end;
   end;
   for i := FRegParams downto 1 do
   begin
      FStream.WriteByte($8B);
      FStream.WriteByte(REG_OPS[i]);
      FStream.WriteByte(FRegParamDepth[i - 1]);
   end;

   writeCall(nil);
end;

procedure Tx86RegisterJit.WriteCleanup;
const ALIGN_BOUNDS = sizeof(pointer);
var
   item: TCleanup;
   initSize: integer;
   alignment: integer;
begin
   initSize := FInitStream.Size;
   FTryFrame[1] := FStream._begin_finally_block + initSize;
   for item in FCleanups do
   begin
      FStream.WriteBytes([$8D, $45]); //lea eax,[ebp - ??]
      FStream.WriteByte(byte(item.depth));
      if item.typ = FProgram.TypString then
         WriteCall(func_ustr_clear)
      else if item.typ is TClassSymbol then
         WriteCall(func_intf_clear)
      else raise Exception.CreateFmt('Unknown type for cleanup: %s', [item.typ.Name]);
   end;

   //+1 for 1-byte RET that _end_finally_block inserts
   alignment := (initSize + FStream.Size + 1) mod ALIGN_BOUNDS;
   if alignment > 0 then
      FStream._nop(alignment);
   FTryFrame[2] := FStream._end_finally_block(FTryFrame[1] - initSize) + initSize;
   AddCall(func_handle_finally, FTryFrame[2]);
   FTryFrame[3] := FStream.Size + initSize;
end;

procedure Tx86RegisterJit.PostCall;
const
   RESTORE_STACK: array[0..5] of byte = ($5F, $5E, $5B, $59, $8B, $E5); //pop edi, pop esi, pop ebx,
                                                                        //pop ecx, mov esp,ebp
begin
   if FTryFrame[0] <> 0 then
      WriteCleanup;
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

function Tx86RegisterJit.GetCalls: TFunctionCallArray;
begin
   Result:=Copy(FCalls);
end;

{ Tx86RegisterJit.TStackLoc }

constructor Tx86RegisterJit.TStackLoc.Create(depth, size: integer);
begin
   self.depth := depth;
   self.size := size;
end;

{$ifdef WIN32}

function SetProcessDEPPolicy(dwFlags: DWORD): BOOL; stdcall; external kernel32 name 'SetProcessDEPPolicy';
initialization
   SetProcessDEPPolicy(0);
{$endif}
end.
