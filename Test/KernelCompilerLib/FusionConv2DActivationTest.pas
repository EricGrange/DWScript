// Test: Conv2D + Activation fusion patterns
// Verifies that Conv2D fused with ReLU/ReLU6/HardSwish produces
// identical results to running them as separate operations.

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 10000) / 10000;
   Result := FloatToStr(rv);
   if Pos('.', Result) = 0 then Result := Result + '.0000'
   else begin
      while Length(Result) - Pos('.', Result) < 4 do Result := Result + '0';
   end;
end;

// --- Test 1: Conv2D 1x1 + ReLU (pointwise, verifies JIT fusion path) ---
PrintLn('=== Conv2D 1x1 + ReLU ===');
var k1 := TKCLKernel.Create;
var in1 := k1.AddInput('in');
// identity weights with bias that makes some outputs negative
var w1 : array of Float := [1.0, -1.0];
var b1 : array of Float := [-2.0, 2.0];
var conv1 := k1.AddConv2D(in1, w1, b1, 1, 1);
var relu1 := k1.AddReLU(conv1);
k1.MarkOutput(relu1);

var bi1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 3, 1]);
var bo1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 3, 2]);

// Input values: 0, 1, 2, 3, 4, 5
for var y := 0 to 1 do
   for var x := 0 to 2 do
      bi1.SetData([y, x, 0], y * 3 + x);

TKCLKernelCompiler.Dispatch(k1, [bi1, bo1]);

for var y := 0 to 1 do
   for var x := 0 to 2 do
      PrintLn(RoundedStr(bo1.GetData([y,x,0])) + ', ' + RoundedStr(bo1.GetData([y,x,1])));

// --- Test 2: Conv2D 1x1 + ReLU6 ---
PrintLn('=== Conv2D 1x1 + ReLU6 ===');
var k2 := TKCLKernel.Create;
var in2 := k2.AddInput('in');
var w2 : array of Float := [2.0];
var b2_ : array of Float := [-3.0];
var conv2 := k2.AddConv2D(in2, w2, b2_, 1, 1);
var relu6_2 := k2.AddReLU6(conv2);
k2.MarkOutput(relu6_2);

var bi2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 6, 1]);
var bo2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 6, 1]);

// Input: -1, 0, 1, 2, 3, 5 -> conv out: -5, -3, -1, 1, 3, 7 -> relu6: 0, 0, 0, 1, 3, 6
for var i := 0 to 5 do begin
   var vals : array of Float := [-1, 0, 1, 2, 3, 5];
   bi2.SetData([0, i, 0], vals[i]);
end;

TKCLKernelCompiler.Dispatch(k2, [bi2, bo2]);

for var i := 0 to 5 do
   PrintLn(RoundedStr(bo2.GetData([0, i, 0])));

// --- Test 3: Conv2D 3x3 + ReLU (verifies KxK fusion path) ---
PrintLn('=== Conv2D 3x3 + ReLU ===');
var k3 := TKCLKernel.Create;
var in3 := k3.AddInput('in');
var w3 : array of Float;
w3.SetLength(9);
for var i := 0 to 8 do w3[i] := 1.0;
var b3 : array of Float := [-5.0];
var conv3 := k3.AddConv2D(in3, w3, b3, 3, 1);
var relu3 := k3.AddReLU(conv3);
k3.MarkOutput(relu3);

var bi3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);
var bo3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);

for var y := 0 to 3 do
   for var x := 0 to 3 do
      bi3.SetData([y, x, 0], 1.0);

TKCLKernelCompiler.Dispatch(k3, [bi3, bo3]);

for var y := 0 to 3 do begin
   var line := '';
   for var x := 0 to 3 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo3.GetData([y, x, 0]));
   end;
   PrintLn(line);
end;

// --- Test 4: Conv2D + HardSwish ---
PrintLn('=== Conv2D 1x1 + HardSwish ===');
var k4 := TKCLKernel.Create;
var in4 := k4.AddInput('in');
var w4 : array of Float := [1.0];
var b4 : array of Float := [0.0];
var conv4 := k4.AddConv2D(in4, w4, b4, 1, 1);
var hs4 := k4.AddHardSwish(conv4);
k4.MarkOutput(hs4);

var bi4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);
var bo4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);

// Input values chosen to test HardSwish boundaries: -4, -2, 0, 2, 4
var hsvals : array of Float := [-4.0, -2.0, 0.0, 2.0, 4.0];
for var i := 0 to 4 do
   bi4.SetData([0, i, 0], hsvals[i]);

TKCLKernelCompiler.Dispatch(k4, [bi4, bo4]);

for var i := 0 to 4 do
   PrintLn(RoundedStr(bo4.GetData([0, i, 0])));

// --- Test 5: Verify unfused produces same result (control test) ---
// Build same graph but with an extra output preventing fusion
PrintLn('=== Unfused control (Conv2D output also used) ===');
var k5 := TKCLKernel.Create;
var in5 := k5.AddInput('in');
var w5 : array of Float := [1.0, -1.0];
var b5 : array of Float := [-2.0, 2.0];
var conv5 := k5.AddConv2D(in5, w5, b5, 1, 1);
var relu5 := k5.AddReLU(conv5);
k5.MarkOutput(conv5);  // extra output prevents fusion
k5.MarkOutput(relu5);

var bi5 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 3, 1]);
var bo5a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 3, 2]);
var bo5b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 3, 2]);

for var y := 0 to 1 do
   for var x := 0 to 2 do
      bi5.SetData([y, x, 0], y * 3 + x);

TKCLKernelCompiler.Dispatch(k5, [bi5, bo5a, bo5b]);

for var y := 0 to 1 do
   for var x := 0 to 2 do
      PrintLn(RoundedStr(bo5b.GetData([y,x,0])) + ', ' + RoundedStr(bo5b.GetData([y,x,1])));
