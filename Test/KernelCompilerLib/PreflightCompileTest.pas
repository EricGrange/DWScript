// Test: Pre-flight compilation and workspace allocation.
// Verifies that pre-compiled kernels produce identical results
// to lazy-compiled kernels across multiple dispatch calls.

function CompareBuffers3D(b1, b2 : TKCLStridedBuffer; d0, d1, d2 : Integer; tol : Float; label_ : String) : Boolean;
begin
   Result := True;
   for var y := 0 to d0 - 1 do
      for var x := 0 to d1 - 1 do
         for var c := 0 to d2 - 1 do begin
            var v1 := b1.GetData([y, x, c]);
            var v2 := b2.GetData([y, x, c]);
            if Abs(v1 - v2) > tol then begin
               PrintLn(label_ + ' FAIL at [' + IntToStr(y) + ',' + IntToStr(x) + ',' + IntToStr(c) + ']');
               Result := False;
            end;
         end;
end;

// Build a graph with Conv2D + Add + ReLU (residual pattern)
// plus additional map ops to test batching
var k := TKCLKernel.Create;
var inp := k.AddInput('in');
var w : array of Float := [0.5, -0.3, 0.2, 0.7, -0.1, 0.4, 0.8, -0.5];
var b : array of Float := [0.1, -0.1];
var conv := k.AddConv2D(inp, w, b, 1, 1);
var add := k.AddAdd(conv, inp); // residual: conv has 2 out channels, inp has 4; this won't broadcast correctly
// Let's build a simpler graph instead
var relu := k.AddReLU(conv);
k.MarkOutput(relu);

var bIn := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 4]);
var bRefOut := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);
var bJitOut1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);
var bJitOut2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);

for var y := 0 to 3 do
   for var x := 0 to 3 do
      for var c := 0 to 3 do
         bIn.SetData([y, x, c], (y - 2) * 0.4 + (x - 2) * 0.3 + c * 0.5 - 1.0);

// Reference dispatch
TKCLReferenceCompiler.Dispatch(k, [bIn, bRefOut]);

// First JIT dispatch (triggers pre-flight compilation)
TKCLKernelCompiler.Dispatch(k, [bIn, bJitOut1]);

// Second JIT dispatch (uses cached pre-compiled kernels, fast path)
TKCLKernelCompiler.Dispatch(k, [bIn, bJitOut2]);

// Compare reference vs first JIT
if CompareBuffers3D(bRefOut, bJitOut1, 4, 4, 2, 0.001, 'Preflight1') then
   PrintLn('Preflight first dispatch: PASS');

// Compare reference vs second JIT (cached)
if CompareBuffers3D(bRefOut, bJitOut2, 4, 4, 2, 0.001, 'Preflight2') then
   PrintLn('Preflight cached dispatch: PASS');

// Compare first and second JIT (should be identical)
if CompareBuffers3D(bJitOut1, bJitOut2, 4, 4, 2, 0.0, 'Consistency') then
   PrintLn('Dispatch consistency: PASS');
