unit dwsExternalFunctionJitx86;

{$define UNSAFE_DEP_OFF}

interface

uses
   dwsTokenizer,
   dwsExternalFunctionJit, dwsExprs, dwsJITx86Intrinsics, dwsExprList, dwsCompilerContext;

function JitFactory(conv: TTokenType; prog: TdwsProgram;
   OnLookupType: TTypeLookupEvent): IExternalFunctionJit;

implementation

uses
   {$ifdef UNSAFE_DEP_OFF}
   Windows,
   {$endif}
   SysUtils,
   dwsUtils,
   dwsSymbols, dwsVMTOffsets;

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
      TResultStyle = (rsNormal, rsVar, rsFloat, rsObj, rsArray);
   private
      FProgram: TdwsProgram;
      FCompilerContext : TdwsCompilerContext;
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
      FOnLookupType: TTypeLookupEvent;
      FEcxClear: boolean;

      function GetDepth(depth: byte): shortint;

      procedure InitParams;
      procedure AddCall(call: pointer; offset: integer);
      procedure WriteCall(loc: pointer);
      function typeSize(value: TTypeSymbol): integer;
      procedure WriteExtractObject(depth: shortint);
      procedure WriteExtractArray(pType: TTypeSymbol; depth: shortint);
      procedure PushParam(param: TParamSymbol; pType: TTypeSymbol);
      procedure RegPassParam(param: TParamSymbol; pType: TTypeSymbol);
      function GetVmtSlot(pType: TTypeSymbol; out resultStyle: TResultStyle): byte;
      function CallGetParam(pType: TTypeSymbol; size: integer): shortint;
      procedure InitClearECX;
      procedure WriteLoadVarParam(depth: shortint);
      procedure WriteStoreIntResult;
      procedure WriteStoreObjResult;
      procedure WriteStoreResultBase;
      procedure WriteStoreResult;
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
      constructor Create(prog: TdwsProgram; OnLookupType: TTypeLookupEvent);
      destructor Destroy; override;
   end;


function JitFactory(conv: dwsTokenizer.TTokenType; prog: TdwsProgram;
  OnLookupType: TTypeLookupEvent): IExternalFunctionJit;
begin
   if conv = ttREGISTER then
      result := Tx86RegisterJit.Create(prog, OnLookupType)
   else raise Exception.Create('Only REGISTER calling convention is supported so far');
end;

function RegisterExternalObject(const self : TExprBaseListExec; AObject: TObject; AutoFree,
  ExactClassMatch: Boolean): variant;
var
   info: TProgramInfo;
begin
   info := (self.Exec as TdwsProgramExecution).ProgramInfo;
   result := info.RegisterExternalObject(AObject, AutoFree, ExactClassMatch);
end;


{ Tx86RegisterJit }

constructor Tx86RegisterJit.Create(prog: TdwsProgram; OnLookupType: TTypeLookupEvent);
begin
   FProgram := prog;
   FCompilerContext := prog.Root.CompilerContext;
   FInitStream := Tx86WriteOnlyStream.Create;
   FStream := Tx86WriteOnlyStream.Create;
   FOnLookupType := OnLookupType;
end;

destructor Tx86RegisterJit.Destroy;
begin
   FStream.Free;
   FInitStream.Free;
   inherited;
end;

function Tx86RegisterJit.typeSize(value: TTypeSymbol): integer;
begin
   if value = FCompilerContext.TypFloat then
      result := 2
   else if (value is TClassSymbol) or (value is TDynamicArraySymbol) then
      result := 2
   else result := 1;
end;

procedure Tx86RegisterJit.BeginProcedure(params: TParamsSymbolTable);
var
   i, paramDepth: integer;
begin
   FParams := 0;
   FRegParams := 0;
   FEcxClear := false;
   FInitStream._push_reg(gprEBP);
   FInitStream._mov_reg_reg(gprEBP, gprESP); //begin stack frame
   if (Params.Count > 0) or assigned(FReturnValue) then
   begin
      paramDepth := 0;
      for i := 0 to params.Count - 1 do
         inc(paramDepth, typeSize(params[i].typ));
      if assigned(FReturnValue) and (FReturnValue is TClassSymbol) then
         inc(paramDepth);
      if paramDepth > 0 then
      begin
         FInitStream._add_reg_int32(gprESP, GetDepth(paramDepth)); //reserve stack space
         FInitStream._push_reg(gprECX); FInitStream._push_reg(gprEBX);
         FInitStream._push_reg(gprESI); FInitStream._push_reg(gprEDI);
         FInitStream._mov_reg_reg(gprEBX, gprEAX);
         if assigned(FReturnValue) then
            FInitStream._mov_dword_ptr_reg_reg(gprEBP, -2 * sizeof(pointer), gprEDX);
      end;
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
   FReturnValue := retval;
   BeginProcedure(params);
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
var
   slots: integer;
begin
   slots := OVERHEAD_SLOTS;
   if assigned(FReturnValue) then
      inc(slots);
   
   result := ((depth + slots) * sizeof(pointer));
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

procedure Tx86RegisterJit.InitClearECX;
begin
   if not FEcxClear then
   begin
      FInitStream._xor_reg_reg(gprECX, gprECX);
      FEcxClear := true;
   end;
end;

procedure Tx86RegisterJit.WriteLoadVarParam(depth: shortint);
const
   LEA_PARAM: array[0..1] of byte = ($8D, $4D);      //lea ecx,[ebp - ??]
begin
   if FTryFrame[0] = 0 then
      FTryFrame[0] := FInitStream._begin_tryf_frame;
   InitClearECX;
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
   if (pType = FCompilerContext.TypInteger) or (ptype is TEnumerationSymbol) then
      result := vmt_TExprBase_EvalAsInteger
   else if pType = FCompilerContext.TypBoolean then
      result := vmt_TExprBase_EvalAsBoolean
   else if pType = FCompilerContext.TypString then
   begin
      result := vmt_TExprBase_EvalAsString;
      resultStyle := rsVar;
   end
   else if pType = FCompilerContext.TypFloat then
   begin
      result := vmt_TExprBase_EvalAsFloat;
      resultStyle := rsFloat;
   end
   else if pType is TClassSymbol then
   begin
      result := vmt_TExprBase_EvalAsScriptObj;
      resultStyle := rsObj;
   end
   else if pType is TDynamicArraySymbol then
   begin
      result := vmt_TExprBase_EvalAsDynArray;
      resultStyle := rsArray;
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

procedure Tx86RegisterJit.WriteExtractArray(pType: TTypeSymbol; depth: shortint);
begin
   InitClearECX;
   FInitStream._mov_dword_ptr_reg_reg(gprEBP, depth, gprECX); //zero stack location
   FStream.WriteBytes([$8D, $55]); //lea edx, [ebp - $??]
   FStream.WriteByte(byte(depth));
   FStream._mov_reg_dword_ptr_reg(gprEAX, gprEBP, depth - sizeof(pointer));
   WriteCall(@FOnLookupType(pType.QualifiedName).event);
   AddCleanup(depth, pType);
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
   if resultStyle in [rsVar, rsObj, rsArray] then
   begin
      WriteLoadVarParam(result);
      if resultStyle = rsArray then
         AddCleanup(result, FCompilerContext.TypInterface)
      else AddCleanup(result, pType);
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
   end
   else if resultStyle = rsArray then
   begin
      inc(result, sizeof(pointer));
      WriteExtractArray(pType, result);
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
   passOnStack := (FRegParams = 3) or (pType = FCompilerContext.TypFloat) or (pType = FCompilerContext.TypVariant);
   if FParams = 0 then
      InitParams;
   if passOnStack then
      PushParam(param, pType)
   else RegPassParam(param, pType);
   inc(FParams);
end;

procedure Tx86RegisterJit.WriteStoreResultBase;
begin
   FStream._mov_reg_reg(gprEDX, gprEAX);
   FStream._mov_reg_dword_ptr_reg(gprEAX, gprEBP, -2 * sizeof(pointer));
end;

procedure Tx86RegisterJit.WriteStoreIntResult;
begin
   WriteStoreResultBase;
   FStream.WriteBytes([$B1, $FC]); //mov CL, $FC (-4 for 3rd param)
   WriteCall(func_var_from_int);
end;

procedure Tx86RegisterJit.WriteStoreObjResult;
begin
   WriteStoreResultBase;
   FStream.WriteBytes([$6A, $01]); // push $01
   FStream._push_reg(gprEAX);
   FStream._mov_reg_reg(gprEAX, gprEBX);
   FStream.WriteBytes([$B1, $01]); //mov CL, 1
   WriteCall(@RegisterExternalObject);
end;

procedure Tx86RegisterJit.WriteStoreResult;
begin
   if (FReturnValue = FCompilerContext.TypInteger) or (FReturnValue is TEnumerationSymbol) then
      WriteStoreIntResult
{
   else if FReturnValue = FCompilerContext.TypBoolean then
   else if FReturnValue = FCompilerContext.TypString then
   else if FReturnValue = FCompilerContext.TypFloat then
}   
   else if FReturnValue is TClassSymbol then
      WriteStoreObjResult
   else raise Exception.CreateFmt('Unsupported result type: %s', [FReturnValue.Name]);
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
   if assigned(FReturnValue) then
      WriteStoreResult;      
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
      if item.typ = FCompilerContext.TypString then
         WriteCall(func_ustr_clear)
      else if item.typ = FCompilerContext.TypVariant then
         WriteCall(func_var_clr)
      else if (item.typ is TClassSymbol) or (item.typ = FCompilerContext.TypInterface) then
         WriteCall(func_intf_clear)
      else if item.typ is TDynamicArraySymbol then
      begin
         FStream._mov_reg_dword(gprEDX, NativeUInt(FOnLookupType(item.typ.QualifiedName).info));
         WriteCall(func_dyn_array_clear)
      end
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
   if (FParams > 0) or (assigned(FReturnValue) and (FReturnValue is TClassSymbol)) then
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

{$ifdef UNSAFE_DEP_OFF}

function SetProcessDEPPolicy(dwFlags: DWORD): BOOL; stdcall; external kernel32 name 'SetProcessDEPPolicy';
initialization
   SetProcessDEPPolicy(0);

{$endif}

end.
