// Test: JIT Map operations (Sub, ReLU, ReLU6, HardSwish) and dispatch batching.
// Compares JIT backend results against Reference backend for correctness.

function CompareBuffers1D(b1, b2 : TKCLStridedBuffer; count : Integer; tol : Float; label_ : String) : Boolean;
begin
   Result := True;
   for var i := 0 to count - 1 do begin
      var v1 := b1.GetData([i]);
      var v2 := b2.GetData([i]);
      if Abs(v1 - v2) > tol then begin
         PrintLn(label_ + ' FAIL at [' + IntToStr(i) + ']: ref=' + FloatToStr(v1) + ' jit=' + FloatToStr(v2));
         Result := False;
      end;
   end;
end;

var count := 11;
var testValues : array of Float := [-10.0, -5.0, -3.0, -1.5, -0.5, 0.0, 0.5, 1.5, 3.0, 5.0, 10.0];

// --- Test Sub ---
var kSub := TKCLKernel.Create;
var inSub1 := kSub.AddInput('a');
var inSub2 := kSub.AddInput('b');
var sub := kSub.AddSub(inSub1, inSub2);
kSub.MarkOutput(sub);

var bSubA := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bSubB := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bSubRef := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bSubJIT := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);

for var i := 0 to count - 1 do begin
   bSubA.SetData([i], testValues[i]);
   bSubB.SetData([i], testValues[count - 1 - i]);
end;

TKCLReferenceCompiler.Dispatch(kSub, [bSubA, bSubB, bSubRef]);
TKCLKernelCompiler.Dispatch(kSub, [bSubA, bSubB, bSubJIT]);

if CompareBuffers1D(bSubRef, bSubJIT, count, 0.001, 'Sub') then
   PrintLn('Sub: PASS');

// --- Test ReLU standalone ---
var kReLU := TKCLKernel.Create;
var inR := kReLU.AddInput('x');
var relu := kReLU.AddReLU(inR);
kReLU.MarkOutput(relu);

var bRIn := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bRRef := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bRJIT := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);

for var i := 0 to count - 1 do
   bRIn.SetData([i], testValues[i]);

TKCLReferenceCompiler.Dispatch(kReLU, [bRIn, bRRef]);
TKCLKernelCompiler.Dispatch(kReLU, [bRIn, bRJIT]);

if CompareBuffers1D(bRRef, bRJIT, count, 0.001, 'ReLU') then
   PrintLn('ReLU: PASS');

// --- Test ReLU6 standalone ---
var kR6 := TKCLKernel.Create;
var inR6 := kR6.AddInput('x');
var relu6 := kR6.AddReLU6(inR6);
kR6.MarkOutput(relu6);

var bR6In := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bR6Ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bR6JIT := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);

for var i := 0 to count - 1 do
   bR6In.SetData([i], testValues[i]);

TKCLReferenceCompiler.Dispatch(kR6, [bR6In, bR6Ref]);
TKCLKernelCompiler.Dispatch(kR6, [bR6In, bR6JIT]);

if CompareBuffers1D(bR6Ref, bR6JIT, count, 0.001, 'ReLU6') then
   PrintLn('ReLU6: PASS');

// --- Test HardSwish standalone ---
var kHS := TKCLKernel.Create;
var inHS := kHS.AddInput('x');
var hs := kHS.AddHardSwish(inHS);
kHS.MarkOutput(hs);

var bHSIn := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bHSRef := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bHSJIT := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);

for var i := 0 to count - 1 do
   bHSIn.SetData([i], testValues[i]);

TKCLReferenceCompiler.Dispatch(kHS, [bHSIn, bHSRef]);
TKCLKernelCompiler.Dispatch(kHS, [bHSIn, bHSJIT]);

if CompareBuffers1D(bHSRef, bHSJIT, count, 0.001, 'HardSwish') then
   PrintLn('HardSwish: PASS');

// --- Test Dispatch Batching: chain of map ops ---
// Tests that multiple sequential map ops are correctly batched
var kChain := TKCLKernel.Create;
var inC := kChain.AddInput('x');
var addConst := kChain.AddInput('offset');
var step1 := kChain.AddAdd(inC, addConst);     // x + offset
var step2 := kChain.AddReLU(step1);             // relu(x + offset)
var step3 := kChain.AddReLU6(step2);            // relu6(relu(x + offset))
kChain.MarkOutput(step3);

var bChainIn := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bChainOff := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bChainRef := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var bChainJIT := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);

for var i := 0 to count - 1 do begin
   bChainIn.SetData([i], testValues[i]);
   bChainOff.SetData([i], 2.0);
end;

TKCLReferenceCompiler.Dispatch(kChain, [bChainIn, bChainOff, bChainRef]);
TKCLKernelCompiler.Dispatch(kChain, [bChainIn, bChainOff, bChainJIT]);

if CompareBuffers1D(bChainRef, bChainJIT, count, 0.001, 'MapChain') then
   PrintLn('MapChain (Add+ReLU+ReLU6): PASS');

// --- Test repeated dispatch (pre-flight caching) ---
// Dispatch the same kernel again to test that compiled kernels are reused
var bChainJIT2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
TKCLKernelCompiler.Dispatch(kChain, [bChainIn, bChainOff, bChainJIT2]);

if CompareBuffers1D(bChainRef, bChainJIT2, count, 0.001, 'CachedDispatch') then
   PrintLn('CachedDispatch: PASS');
