// Test: Fusion edge cases and boundary conditions
// Covers cases where fusion should NOT happen and various channel configs.

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 10000) / 10000;
   Result := FloatToStr(rv);
   if Pos('.', Result) = 0 then Result := Result + '.0000'
   else begin
      while Length(Result) - Pos('.', Result) < 4 do Result := Result + '0';
   end;
end;

// --- Test 1: Conv2D output used by two consumers (no fusion) ---
PrintLn('=== No fusion: Conv2D used by ReLU + Add ===');
var k1 := TKCLKernel.Create;
var in1 := k1.AddInput('in');
var w1 : array of Float := [1.0];
var b1 : array of Float := [0.0];
var conv1 := k1.AddConv2D(in1, w1, b1, 1, 1);
var relu1 := k1.AddReLU(conv1);
var add1 := k1.AddAdd(conv1, in1);  // conv has 2 consumers
k1.MarkOutput(relu1);
k1.MarkOutput(add1);

var bi1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo1a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo1b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);

var vals1 : array of Float := [-2, -1, 1, 2];
for var i := 0 to 3 do bi1.SetData([0, i, 0], vals1[i]);

TKCLKernelCompiler.Dispatch(k1, [bi1, bo1a, bo1b]);

PrintLn('ReLU:');
for var i := 0 to 3 do PrintLn(RoundedStr(bo1a.GetData([0, i, 0])));
PrintLn('Add:');
for var i := 0 to 3 do PrintLn(RoundedStr(bo1b.GetData([0, i, 0])));

// --- Test 2: Chain of Conv2D + ReLU + Conv2D + ReLU (double fusion) ---
PrintLn('=== Double fusion: Conv2D+ReLU -> Conv2D+ReLU ===');
var k2 := TKCLKernel.Create;
var in2 := k2.AddInput('in');
var w2a : array of Float := [2.0];
var b2a : array of Float := [-3.0];
var conv2a := k2.AddConv2D(in2, w2a, b2a, 1, 1);
var relu2a := k2.AddReLU(conv2a);
var w2b : array of Float := [1.0];
var b2b : array of Float := [-1.0];
var conv2b := k2.AddConv2D(relu2a, w2b, b2b, 1, 1);
var relu2b := k2.AddReLU(conv2b);
k2.MarkOutput(relu2b);

var bi2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);
var bo2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 5, 1]);

// Input: 0, 1, 2, 3, 4
for var i := 0 to 4 do bi2.SetData([0, i, 0], i);

TKCLKernelCompiler.Dispatch(k2, [bi2, bo2]);

for var i := 0 to 4 do PrintLn(RoundedStr(bo2.GetData([0, i, 0])));

// --- Test 3: Conv2D + Add where Add is an output node (no residual fusion) ---
PrintLn('=== No residual fusion: Add is output ===');
var k3 := TKCLKernel.Create;
var in3 := k3.AddInput('in');
var w3 : array of Float := [1.0];
var b3 : array of Float := [0.0];
var conv3 := k3.AddConv2D(in3, w3, b3, 1, 1);
var add3 := k3.AddAdd(conv3, in3);
k3.MarkOutput(add3);  // Add is output, so no fusion with downstream

var bi3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 3, 1]);
var bo3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 3, 1]);

bi3.SetData([0, 0, 0], 1.0);
bi3.SetData([0, 1, 0], 2.0);
bi3.SetData([0, 2, 0], 3.0);

TKCLKernelCompiler.Dispatch(k3, [bi3, bo3]);

for var i := 0 to 2 do PrintLn(RoundedStr(bo3.GetData([0, i, 0])));

// --- Test 4: Conv2D 1x1 + ReLU with many channels (stress test tiling) ---
PrintLn('=== Conv2D 1x1 + ReLU (12 channels) ===');
var k4 := TKCLKernel.Create;
var in4 := k4.AddInput('in');
var w4 : array of Float;
w4.SetLength(3 * 12); // 3 in -> 12 out
for var i := 0 to w4.Length - 1 do w4[i] := 0.0;
// Set diagonal-like pattern: out[i] = in[i mod 3]
for var oc := 0 to 11 do
   w4[(oc mod 3) * 12 + oc] := 1.0;
var b4 : array of Float;
b4.SetLength(12);
for var i := 0 to 11 do b4[i] := -0.5;
var conv4 := k4.AddConv2D(in4, w4, b4, 1, 1);
var relu4 := k4.AddReLU(conv4);
k4.MarkOutput(relu4);

var bi4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 3]);
var bo4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 12]);

bi4.SetData([0, 0, 0], 0.0); bi4.SetData([0, 0, 1], 1.0); bi4.SetData([0, 0, 2], 2.0);
bi4.SetData([0, 1, 0], -1.0); bi4.SetData([0, 1, 1], 0.0); bi4.SetData([0, 1, 2], 3.0);

TKCLKernelCompiler.Dispatch(k4, [bi4, bo4]);

for var px := 0 to 1 do begin
   var line := '';
   for var c := 0 to 11 do begin
      if c > 0 then line := line + ' ';
      line := line + RoundedStr(bo4.GetData([0, px, c]));
   end;
   PrintLn(line);
end;

// --- Test 5: Conv2D stride 2 + ReLU (strided convolution fusion) ---
PrintLn('=== Conv2D 3x3 stride 2 + ReLU ===');
var k5 := TKCLKernel.Create;
var in5 := k5.AddInput('in');
var w5 : array of Float;
w5.SetLength(9);
for var i := 0 to 8 do w5[i] := 1.0;
var b5 : array of Float := [-4.0];
var conv5 := k5.AddConv2D(in5, w5, b5, 3, 2);
var relu5 := k5.AddReLU(conv5);
k5.MarkOutput(relu5);

var bi5 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);
var bo5 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 1]);

for var y := 0 to 3 do
   for var x := 0 to 3 do
      bi5.SetData([y, x, 0], 1.0);

TKCLKernelCompiler.Dispatch(k5, [bi5, bo5]);

for var y := 0 to 1 do begin
   var line := '';
   for var x := 0 to 1 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo5.GetData([y, x, 0]));
   end;
   PrintLn(line);
end;
