unit dwsExternalFunctions;

interface
uses
   SysUtils,
   dwsExprList, dwsExprs, dwsMagicExprs, dwsSymbols, dwsUtils, dwsExternalFunctionJit;

type
   IExternalRoutine = interface(ICallable)
   ['{1595278A-94F5-4B46-8173-C3604C93959C}']
      procedure SetExternalPointer(value: pointer);
   end;

   TExternalProcedure = class(TInternalMagicProcedure, IExternalRoutine)
   private
      type TStub = procedure(const args : TExprBaseListExec);
   private
      FBuffer: TBytes;
      FStub: TStub;
      FCalls: TArray<TFunctionCall>;

      procedure SetExternalPointer(value: pointer);
   public
      constructor Create(funcSymbol : TFuncSymbol; prog: TdwsProgram);
      destructor Destroy; override;
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TExternalFunction = class(TInternalMagicVariantFunction, IExternalRoutine)
   private
      type TStub = function (const args : TExprBaseListExec): Variant;
   private
      FBuffer: TBytes;
      FStub: TStub;
      FCalls: TArray<TFunctionCall>;

      procedure SetExternalPointer(value: pointer);
   public
      constructor Create(funcSymbol : TFuncSymbol; prog: TdwsProgram);
      destructor Destroy; override;

      function DoEvalAsVariant(const args : TExprBaseListExec) : Variant; override;
   end;

implementation
uses
   Windows,
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

function MakeExecutable(const value: TBytes; calls: TArray<TFunctionCall>; call: pointer): pointer;
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
      lOffset := (lCall - nativeInt(ptr)) - sizeof(pointer);
      PNativeInt(ptr)^ := lOffset;
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

constructor TExternalProcedure.Create(funcSymbol: TFuncSymbol; prog: TdwsProgram);
var
   jit: TdwsExternalStubJit;
begin
   assert(assigned(funcSymbol));
   assert(not funcSymbol.IsType);
   assert(funcSymbol.Executable = nil);
   assert(funcSymbol.ExternalConvention in [ttREGISTER..ttSTDCALL]);
   jit := TdwsExternalStubJit.Create;
   try
      jit.Eval(funcSymbol, prog);
      FBuffer := jit.FBuffer;
      FCalls := jit.FInternalJit.GetCalls;
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
   if not assigned(FStub) then
      raise Exception.Create('No external function assigned');
   FStub(args);
end;

procedure TExternalProcedure.SetExternalPointer(value: pointer);
begin
   if assigned(FStub) then
      raise Exception.Create('External function cannot be assigned twice');
   FStub := MakeExecutable(FBuffer, FCalls, value);
end;

{ TExternalFunction }

constructor TExternalFunction.Create(funcSymbol: TFuncSymbol; prog: TdwsProgram);
var
   jit: TdwsExternalStubJit;
begin
   assert(assigned(funcSymbol));
   assert(funcSymbol.IsType);
   assert(funcSymbol.Executable = nil);
   assert(funcSymbol.ExternalConvention in [ttREGISTER..ttSTDCALL]);
   jit := TdwsExternalStubJit.Create;
   try
      jit.Eval(funcSymbol, prog);
      FBuffer := jit.FBuffer;
      FCalls := jit.FInternalJit.GetCalls;
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
   if not assigned(FStub) then
      raise Exception.Create('No external function assigned');
   result := FStub(args);
end;

procedure TExternalFunction.SetExternalPointer(value: pointer);
begin
   if assigned(FStub) then
      raise Exception.Create('External function cannot be assigned twice');
   FStub := MakeExecutable(FBuffer, FCalls, value);
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
      FInternalJit.BeginFunction(funcSymbol.typ, funcSymbol.ParamSize - 1)
   else FInternalJit.BeginProcedure(funcSymbol.ParamSize - 1);
   for i := 0 to funcSymbol.ParamSize - 1 do
      FInternalJit.PassParam(funcSymbol.Params[i]);
   FInternalJit.Call;
   FInternalJit.PostCall;
   FBuffer := FInternalJit.GetBytes;

end;

end.
