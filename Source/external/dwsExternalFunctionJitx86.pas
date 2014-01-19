unit dwsExternalFunctionJitx86;

interface
uses
   Generics.Collections,
   dwsTokenizer, dwsExternalFunctionJit, dwsExprs;

function JitFactory(conv: TTokenType; prog: TdwsProgram): IExternalFunctionJit;

implementation
uses
   SysUtils,
   dwsUtils, dwsSymbols, dwsExprList, dwsVMTOffsets;

type
   Tx86RegisterJit = class(TinterfacedObject, IExternalFunctionJit)
   private
      FProgram: TdwsProgram;
      FInitStream: TWriteOnlyBlockStream;
      FStream: TWriteOnlyBlockStream;
      FReturnValue: TTypeSymbol;
      FParams: integer;
      FRegParams: integer;
      FRegParamDepth: array[0..2] of byte;
      FCalls: TList<TFunctionCall>;

      function GetDepth(depth: byte): byte;

      procedure InitParams;
      procedure AddCall(call: pointer; offset: integer);
      procedure WriteCall(loc: pointer);
      procedure PushParam(param: TParamSymbol; pType: TTypeSymbol);
      procedure RegPassParam(param: TParamSymbol; pType: TTypeSymbol);
   private //IExternalFunctionJit implementation
      procedure BeginProcedure(paramCount: integer);
      procedure BeginFunction(retval: TTypeSymbol; paramCount: integer);
      procedure PassParam(param: TParamSymbol);
      procedure Call;
      procedure PostCall;
      function GetBytes: TBytes;
      function GetCalls: TArray<TFunctionCall>;
      function CallGetParam(pType: TTypeSymbol; index: integer): byte;
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
   FInitStream := TWriteOnlyBlockStream.Create;
   FStream := TWriteOnlyBlockStream.AllocFromPool;
   FCalls := TList<TFunctionCall>.Create;
end;

destructor Tx86RegisterJit.Destroy;
begin
   FCalls.Free;
   FStream.ReturnToPool;
   FInitStream.Free;
   inherited;
end;

procedure Tx86RegisterJit.BeginProcedure(paramCount: integer);
const
   BEGIN_STACK:   array[0..1] of byte = ($8B, $EC); //mov mov ebp,esp
   RESERVE_SPACE: array[0..1] of byte = ($83, $C4); //add esp, -??
   SETUP_STACK:   array[0..4] of byte = ($51, $53, $56, $8B, $DA); //push ECX; push EBX;
                                                                   //push ESI; mov ebx,edx
begin
   FParams := 0;
   FRegParams := 0;
   FInitStream.WriteByte($55); //push EBP
   FInitStream.WriteBytes(BEGIN_STACK);
   if ParamCount > 0 then
   begin
      FInitStream.WriteBytes(RESERVE_SPACE);
      FInitStream.WriteByte(GetDepth(paramCount));
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
   FInitStream.WriteBytes(FETCH_ARRAY);
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

function Tx86RegisterJit.GetDepth(depth: byte): byte;
const OVERHEAD_SLOTS = 3;
begin
   result := ((depth + OVERHEAD_SLOTS) * sizeof(pointer));
   result := ($FF - result) + 1;
end;

function Tx86RegisterJit.CallGetParam(pType: TTypeSymbol; index: integer): byte;
const
   LOAD_EXEC:    array[0..2] of byte = ($8B, $53, $04); //mov edx,[ebx+$04]
   LOAD_PARAM_0: array[0..1] of byte = ($8B, $06);      //mov eax,[esi]
   LOAD_PARAM:   array[0..1] of byte = ($8B, $46);      //mov eax,[esi + ??]
   LOAD_VMT:     array[0..1] of byte = ($8B, $08);      //mov ecx,[eax]
   VIRTUAL_CALL: array[0..1] of byte = ($FF, $51);      //call dword ptr [ecx + ??]
   STORE_VALUE:  array[0..1] of byte = ($89, $45);      //mov [ebp - ??],eax
var
   vmtSlot: byte;
begin
   if pType = FProgram.TypInteger then
      vmtSlot := vmt_TExprBase_EvalAsInteger
   else raise Exception.CreateFmt('Unsupported parameter type: %s', [pType.Name]);

   FStream.WriteBytes(LOAD_EXEC);
   if index = 0 then
      FStream.WriteBytes(LOAD_PARAM_0)
   else begin
      FStream.WriteBytes(LOAD_PARAM);
      FStream.WriteByte(index * sizeof(pointer));
   end;
   FStream.WriteBytes(LOAD_VMT);
   FStream.WriteBytes(VIRTUAL_CALL);
   FStream.WriteByte(vmtSlot);

   result := GetDepth(index);
   FStream.WriteBytes(STORE_VALUE);
   FStream.WriteByte(result);
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
   RESTORE_STACK: array[0..4] of byte = ($5E, $5B, $59, $8B, $E5); //pop esi, pop ebx,
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
