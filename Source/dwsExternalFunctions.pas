unit dwsExternalFunctions;

interface

uses
   SysUtils,
   dwsXPlatform,
   dwsExprList, dwsExprs, dwsMagicExprs, dwsSymbols, dwsUtils, dwsExternalFunctionJIT;

type

   TExternalProcedure = class(TInternalMagicProcedure, IExternalRoutine)
   private
      type TProcedureStub = procedure(const args : TExprBaseListExec);

   private
      FBuffer: TBytes;
      FStub: TProcedureStub;
      FCalls: TFunctionCallArray;
      FTryFrame: TTryFrame;

      procedure SetExternalPointer(value: pointer);

   public
      constructor Create(aFuncSymbol : TFuncSymbol; prog: TdwsProgram);
      destructor Destroy; override;
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TExternalFunction = class(TInternalMagicVariantFunction, IExternalRoutine)
   private
      type TVariantFunctionStub = function (const args : TExprBaseListExec): Variant;

   private
      FBuffer: TBytes;
      FStub: TVariantFunctionStub;
      FCalls: TFunctionCallArray;
      FTryFrame: TTryFrame;

      procedure SetExternalPointer(value: pointer);

   public
      constructor Create(aFuncSymbol : TFuncSymbol; prog: TdwsProgram);
      destructor Destroy; override;

      function DoEvalAsVariant(const args : TExprBaseListExec) : Variant; override;
   end;

implementation

uses
   Windows,
   dwsCompiler, dwsStrings,
   dwsTokenizer{$IFDEF CPU386}, dwsExternalFunctionJitx86{$ENDIF};

type
   TdwsExternalStubJit = class
   private
      FBuffer: TBytes;
      FInternalJit: IExternalFunctionJit;

      procedure Clear;
   public
      destructor Destroy; override;
      procedure Eval(funcSymbol: TFuncSymbol; prog: TdwsProgram);
   end;

function ExternalRoutineFactory(funcSymbol : TFuncSymbol; mainProg : TdwsMainProgram) : IExternalRoutine;
begin
   if funcSymbol.IsType then
      result := TExternalFunction.Create(funcSymbol, mainProg)
   else result := TExternalProcedure.Create(funcSymbol, mainProg);
end;

procedure RaiseUnHandledExternalCall(exec : TdwsExecution; func : TFuncSymbol);
begin
   raise EdwsExternalFuncHandler.CreateFmt(RTE_UnHandledExternalCall,
                                           [func.Name, '']);
end;

function MakeExecutable(const value: TBytes; const calls: TFunctionCallArray; call: pointer;
   const tryFrame: TTryFrame): pointer;
var
   oldprotect: cardinal;
   lCall, lOffset: nativeInt;
   ptr: pointer;
   fixup: TFunctionCall;
begin
   result := VirtualAlloc(nil, length(value), MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
   system.Move(value[0], result^, length(value));
   for fixup in calls do
   begin
      ptr := @PByte(result)[fixup.offset];
      if fixup.call = 0 then
         lCall := nativeInt(call)
      else lCall := fixup.call;
      lOffset := (lCall - NativeInt(ptr)) - sizeof(pointer);
      PNativeInt(ptr)^ := lOffset;
   end;
   if tryFrame[0] <> 0 then
   begin
      ptr := @PByte(result)[tryFrame[0]];
      if PPointer(ptr)^ <> nil then
         asm int 3 end;
      PPointer(ptr)^ := @PByte(result)[tryFrame[2] - 1];

      ptr := @PByte(result)[tryFrame[1]];
      if PPointer(ptr)^ <> nil then
         asm int 3 end;
      PPointer(ptr)^ := @PByte(result)[tryFrame[3]];
   end;

   if not VirtualProtect(result, length(value), PAGE_EXECUTE_READ, oldProtect) then
      RaiseLastOSError;
end;

procedure MakeNotExecutable(value: pointer);
begin
   if assigned(value) then
      if not VirtualFree(value, 0, MEM_RELEASE) then
         RaiseLastOSError;
end;

{ TExternalProcedure }

constructor TExternalProcedure.Create(aFuncSymbol: TFuncSymbol; prog: TdwsProgram);
var
   jit: TdwsExternalStubJit;
begin
   FuncSymbol:=aFuncSymbol;
   jit := TdwsExternalStubJit.Create;
   try
      jit.Eval(aFuncSymbol, prog);
      FBuffer := jit.FBuffer;
      FCalls := jit.FInternalJit.GetCalls;
      if jit.FInternalJit.HasTryFrame then
         FTryFrame := jit.FInternalJit.GetTryFrame;
   finally
      jit.Free;
   end;
end;

destructor TExternalProcedure.Destroy;
begin
   MakeNotExecutable(@FStub);
   inherited Destroy;
end;

procedure TExternalProcedure.DoEvalProc(const args: TExprBaseListExec);
begin
   if not Assigned(FStub) then
      RaiseUnHandledExternalCall(args.Exec, FuncSymbol);
   FStub(args);
end;

procedure TExternalProcedure.SetExternalPointer(value: pointer);
begin
   if assigned(FStub) then
      raise Exception.Create('External function cannot be assigned twice');
   FStub := MakeExecutable(FBuffer, FCalls, value, FTryFrame);
end;

{ TExternalFunction }

constructor TExternalFunction.Create(aFuncSymbol: TFuncSymbol; prog: TdwsProgram);
var
   jit: TdwsExternalStubJit;
begin
   FuncSymbol:=aFuncSymbol;

   assert(assigned(aFuncSymbol));
   assert(aFuncSymbol.IsType);
   assert(aFuncSymbol.Executable = nil);
   assert(aFuncSymbol.ExternalConvention in [ttREGISTER..ttSTDCALL]);
   jit := TdwsExternalStubJit.Create;
   try
      jit.Eval(aFuncSymbol, prog);
      FBuffer := jit.FBuffer;
      FCalls := jit.FInternalJit.GetCalls;
      if jit.FInternalJit.HasTryFrame then
         FTryFrame := jit.FInternalJit.GetTryFrame;
   finally
      jit.Free;
   end;
end;

destructor TExternalFunction.Destroy;
begin
   MakeNotExecutable(@FStub);
   inherited Destroy;
end;

function TExternalFunction.DoEvalAsVariant(const args: TExprBaseListExec): Variant;
begin
   if not Assigned(FStub) then
      RaiseUnHandledExternalCall(args.Exec, FuncSymbol);
   result := FStub(args);
end;

procedure TExternalFunction.SetExternalPointer(value: pointer);
begin
   if assigned(FStub) then
      raise Exception.Create('External function cannot be assigned twice');
   FStub := MakeExecutable(FBuffer, FCalls, value, FTryFrame);
end;

{ TdwsExternalStubJit }

destructor TdwsExternalStubJit.Destroy;
begin
   Clear;
   inherited;
end;

procedure TdwsExternalStubJit.Clear;
begin
   FBuffer := nil;
   FInternalJit := nil;
end;

procedure TdwsExternalStubJit.Eval(funcSymbol: TFuncSymbol; prog: TdwsProgram);
var
   i: integer;
begin
   Clear;
   FInternalJit := JitFactory(funcSymbol.ExternalConvention, prog);
   if funcSymbol.IsType then
      FInternalJit.BeginFunction(funcSymbol.typ, funcSymbol.Params)
   else FInternalJit.BeginProcedure(funcSymbol.Params);
   for i := 0 to funcSymbol.Params.Count - 1 do
      FInternalJit.PassParam(funcSymbol.Params[i]);
   FInternalJit.Call;
   FInternalJit.PostCall;
   FBuffer := FInternalJit.GetBytes;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsCompiler.RegisterExternalRoutineFactory(ExternalRoutineFactory);

end.
