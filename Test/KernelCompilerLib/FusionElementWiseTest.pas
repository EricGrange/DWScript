// Test: Element-wise (Add/Sub/Mul) + Activation fusion patterns
// Verifies that standalone Add/Sub/Mul fused with ReLU/ReLU6/HardSwish
// produces identical results to running them as separate operations.
// This reduces the "dispatch noise" from 150+ element-wise nodes.

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 10000) / 10000;
   Result := FloatToStr(rv);
   if Pos('.', Result) = 0 then Result := Result + '.0000'
   else begin
      while Length(Result) - Pos('.', Result) < 4 do Result := Result + '0';
   end;
end;

// --- Test 1: Add + ReLU ---
PrintLn('=== Add + ReLU ===');
var k1 := TKCLKernel.Create;
var in1a := k1.AddInput('a');
var in1b := k1.AddInput('b');
var add1 := k1.AddAdd(in1a, in1b);
var relu1 := k1.AddReLU(add1);
k1.MarkOutput(relu1);

var bi1a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 6, 1]);
var bi1b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 6, 1]);
var bo1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 6, 1]);

// a = [-3, -1, 0, 1, 3, 5], b = [1, -1, -1, 1, -1, 1]
// add = [-2, -2, -1, 2, 2, 6], relu = [0, 0, 0, 2, 2, 6]
var va : array of Float := [-3, -1, 0, 1, 3, 5];
var vb : array of Float := [1, -1, -1, 1, -1, 1];
for var i := 0 to 5 do begin
   bi1a.SetData([0, i, 0], va[i]);
   bi1b.SetData([0, i, 0], vb[i]);
end;

TKCLKernelCompiler.Dispatch(k1, [bi1a, bi1b, bo1]);

for var i := 0 to 5 do
   PrintLn(RoundedStr(bo1.GetData([0, i, 0])));

// --- Test 2: Add + ReLU6 ---
PrintLn('=== Add + ReLU6 ===');
var k2 := TKCLKernel.Create;
var in2a := k2.AddInput('a');
var in2b := k2.AddInput('b');
var add2 := k2.AddAdd(in2a, in2b);
var relu6_2 := k2.AddReLU6(add2);
k2.MarkOutput(relu6_2);

var bi2a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 6, 1]);
var bi2b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 6, 1]);
var bo2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 6, 1]);

// a = [-2, 0, 2, 4, 6, 8], b = [0, 1, 1, 1, 1, 1]
// add = [-2, 1, 3, 5, 7, 9], relu6 = [0, 1, 3, 5, 6, 6]
var v2a : array of Float := [-2, 0, 2, 4, 6, 8];
var v2b : array of Float := [0, 1, 1, 1, 1, 1];
for var i := 0 to 5 do begin
   bi2a.SetData([0, i, 0], v2a[i]);
   bi2b.SetData([0, i, 0], v2b[i]);
end;

TKCLKernelCompiler.Dispatch(k2, [bi2a, bi2b, bo2]);

for var i := 0 to 5 do
   PrintLn(RoundedStr(bo2.GetData([0, i, 0])));

// --- Test 3: Mul + ReLU ---
PrintLn('=== Mul + ReLU ===');
var k3 := TKCLKernel.Create;
var in3a := k3.AddInput('a');
var in3b := k3.AddInput('b');
var mul3 := k3.AddMul(in3a, in3b);
var relu3 := k3.AddReLU(mul3);
k3.MarkOutput(relu3);

var bi3a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);
var bi3b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);
var bo3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);

// a = [-2, -1, 0, 2, 3], b = [3, -2, 5, 2, -1]
// mul = [-6, 2, 0, 4, -3], relu = [0, 2, 0, 4, 0]
var v3a : array of Float := [-2, -1, 0, 2, 3];
var v3b : array of Float := [3, -2, 5, 2, -1];
for var i := 0 to 4 do begin
   bi3a.SetData([0, i, 0], v3a[i]);
   bi3b.SetData([0, i, 0], v3b[i]);
end;

TKCLKernelCompiler.Dispatch(k3, [bi3a, bi3b, bo3]);

for var i := 0 to 4 do
   PrintLn(RoundedStr(bo3.GetData([0, i, 0])));

// --- Test 4: Add + HardSwish ---
PrintLn('=== Add + HardSwish ===');
var k4 := TKCLKernel.Create;
var in4a := k4.AddInput('a');
var in4b := k4.AddInput('b');
var add4 := k4.AddAdd(in4a, in4b);
var hs4 := k4.AddHardSwish(add4);
k4.MarkOutput(hs4);

var bi4a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);
var bi4b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);
var bo4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);

// a = [-5, -3, 0, 2, 5], b = [0, 0, 0, 0, 0]
// add = [-5, -3, 0, 2, 5]
// HS(-5) = -5 * max(0,min(6,-2))/6 = 0
// HS(-3) = -3 * max(0,min(6,0))/6 = 0
// HS(0) = 0 * max(0,min(6,3))/6 = 0
// HS(2) = 2 * max(0,min(6,5))/6 = 2*5/6 = 1.6667
// HS(5) = 5 * max(0,min(6,8))/6 = 5*6/6 = 5
var v4a : array of Float := [-5, -3, 0, 2, 5];
var v4b : array of Float := [0, 0, 0, 0, 0];
for var i := 0 to 4 do begin
   bi4a.SetData([0, i, 0], v4a[i]);
   bi4b.SetData([0, i, 0], v4b[i]);
end;

TKCLKernelCompiler.Dispatch(k4, [bi4a, bi4b, bo4]);

for var i := 0 to 4 do
   PrintLn(RoundedStr(bo4.GetData([0, i, 0])));

// --- Test 5: Sub + ReLU ---
PrintLn('=== Sub + ReLU ===');
var k5 := TKCLKernel.Create;
var in5a := k5.AddInput('a');
var in5b := k5.AddInput('b');
var sub5 := k5.AddSub(in5a, in5b);
var relu5 := k5.AddReLU(sub5);
k5.MarkOutput(relu5);

var bi5a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bi5b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo5 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);

// a = [1, 3, 2, 5], b = [2, 1, 4, 3]
// sub = [-1, 2, -2, 2], relu = [0, 2, 0, 2]
var v5a : array of Float := [1, 3, 2, 5];
var v5b : array of Float := [2, 1, 4, 3];
for var i := 0 to 3 do begin
   bi5a.SetData([0, i, 0], v5a[i]);
   bi5b.SetData([0, i, 0], v5b[i]);
end;

TKCLKernelCompiler.Dispatch(k5, [bi5a, bi5b, bo5]);

for var i := 0 to 3 do
   PrintLn(RoundedStr(bo5.GetData([0, i, 0])));

// --- Test 6: Unfused control (Add with multiple consumers) ---
PrintLn('=== Unfused: Add used by ReLU + output ===');
var k6 := TKCLKernel.Create;
var in6a := k6.AddInput('a');
var in6b := k6.AddInput('b');
var add6 := k6.AddAdd(in6a, in6b);
var relu6 := k6.AddReLU(add6);
k6.MarkOutput(relu6);
k6.MarkOutput(add6);  // Add has 2 consumers (ReLU + output), no fusion

var bi6a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bi6b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo6a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo6b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);

// a = [-2, 1, -3, 4], b = [1, -2, 1, -1]
// add = [-1, -1, -2, 3]
// relu = [0, 0, 0, 3]
var v6a : array of Float := [-2, 1, -3, 4];
var v6b : array of Float := [1, -2, 1, -1];
for var i := 0 to 3 do begin
   bi6a.SetData([0, i, 0], v6a[i]);
   bi6b.SetData([0, i, 0], v6b[i]);
end;

TKCLKernelCompiler.Dispatch(k6, [bi6a, bi6b, bo6a, bo6b]);

PrintLn('ReLU:');
for var i := 0 to 3 do
   PrintLn(RoundedStr(bo6a.GetData([0, i, 0])));
PrintLn('Add:');
for var i := 0 to 3 do
   PrintLn(RoundedStr(bo6b.GetData([0, i, 0])));

// --- Test 7: Chain fusion: Add+ReLU -> Mul+ReLU6 ---
PrintLn('=== Chain: Add+ReLU -> Mul+ReLU6 ===');
var k7 := TKCLKernel.Create;
var in7a := k7.AddInput('a');
var in7b := k7.AddInput('b');
var in7c := k7.AddInput('c');
var add7 := k7.AddAdd(in7a, in7b);
var relu7 := k7.AddReLU(add7);
var mul7 := k7.AddMul(relu7, in7c);
var relu6_7 := k7.AddReLU6(mul7);
k7.MarkOutput(relu6_7);

var bi7a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bi7b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bi7c := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo7 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);

// a = [-1, 2, 3, 1], b = [-1, 1, 1, 2]
// add = [-2, 3, 4, 3], relu = [0, 3, 4, 3]
// c = [2, 3, 2, 3]
// mul = [0, 9, 8, 9], relu6 = [0, 6, 6, 6]
var v7a : array of Float := [-1, 2, 3, 1];
var v7b : array of Float := [-1, 1, 1, 2];
var v7c : array of Float := [2, 3, 2, 3];
for var i := 0 to 3 do begin
   bi7a.SetData([0, i, 0], v7a[i]);
   bi7b.SetData([0, i, 0], v7b[i]);
   bi7c.SetData([0, i, 0], v7c[i]);
end;

TKCLKernelCompiler.Dispatch(k7, [bi7a, bi7b, bi7c, bo7]);

for var i := 0 to 3 do
   PrintLn(RoundedStr(bo7.GetData([0, i, 0])));
