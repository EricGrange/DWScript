// Test: DepthwiseConv2D + Activation fusion patterns
// Covers DepthwiseConv2D fused with ReLU/ReLU6/HardSwish and residual variants.

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 10000) / 10000;
   Result := FloatToStr(rv);
   if Pos('.', Result) = 0 then Result := Result + '.0000'
   else begin
      while Length(Result) - Pos('.', Result) < 4 do Result := Result + '0';
   end;
end;

// --- Test 1: DepthwiseConv2D 3x3 + ReLU ---
PrintLn('=== Depthwise 3x3 + ReLU ===');
var k1 := TKCLKernel.Create;
var in1 := k1.AddInput('in');
var w1 : array of Float;
w1.SetLength(9 * 2);  // 3x3 x 2 channels
for var i := 0 to 17 do w1[i] := 1.0;
var b1 : array of Float := [-5.0, -3.0];
var dw1 := k1.AddDepthwiseConv2D(in1, w1, b1, 3, 1);
var relu1 := k1.AddReLU(dw1);
k1.MarkOutput(relu1);

var bi1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 2]);
var bo1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 2]);

for var y := 0 to 2 do
   for var x := 0 to 2 do begin
      bi1.SetData([y, x, 0], 1.0);
      bi1.SetData([y, x, 1], 1.0);
   end;

TKCLKernelCompiler.Dispatch(k1, [bi1, bo1]);

for var y := 0 to 2 do begin
   var line := '';
   for var x := 0 to 2 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo1.GetData([y, x, 0])) + ',' + RoundedStr(bo1.GetData([y, x, 1]));
   end;
   PrintLn(line);
end;

// --- Test 2: DepthwiseConv2D 3x3 + ReLU6 ---
PrintLn('=== Depthwise 3x3 + ReLU6 ===');
var k2 := TKCLKernel.Create;
var in2 := k2.AddInput('in');
var w2 : array of Float;
w2.SetLength(9);  // 3x3 x 1 channel
for var i := 0 to 8 do w2[i] := 2.0;
var b2 : array of Float := [-5.0];
var dw2 := k2.AddDepthwiseConv2D(in2, w2, b2, 3, 1);
var relu6_2 := k2.AddReLU6(dw2);
k2.MarkOutput(relu6_2);

var bi2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);
var bo2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);

for var y := 0 to 2 do
   for var x := 0 to 2 do
      bi2.SetData([y, x, 0], 1.0);

TKCLKernelCompiler.Dispatch(k2, [bi2, bo2]);

for var y := 0 to 2 do begin
   var line := '';
   for var x := 0 to 2 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo2.GetData([y, x, 0]));
   end;
   PrintLn(line);
end;

// --- Test 3: DepthwiseConv2D 3x3 + HardSwish ---
PrintLn('=== Depthwise 3x3 + HardSwish ===');
var k3 := TKCLKernel.Create;
var in3 := k3.AddInput('in');
var w3 : array of Float;
w3.SetLength(9);
for var i := 0 to 8 do w3[i] := 1.0;
var b3 : array of Float := [-4.0];
var dw3 := k3.AddDepthwiseConv2D(in3, w3, b3, 3, 1);
var hs3 := k3.AddHardSwish(dw3);
k3.MarkOutput(hs3);

var bi3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);
var bo3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);

for var y := 0 to 2 do
   for var x := 0 to 2 do
      bi3.SetData([y, x, 0], 1.0);

TKCLKernelCompiler.Dispatch(k3, [bi3, bo3]);

for var y := 0 to 2 do begin
   var line := '';
   for var x := 0 to 2 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo3.GetData([y, x, 0]));
   end;
   PrintLn(line);
end;

// --- Test 4: DepthwiseConv2D 3x3 + Add + ReLU (residual) ---
PrintLn('=== Depthwise 3x3 + Add + ReLU (residual) ===');
var k4 := TKCLKernel.Create;
var in4 := k4.AddInput('in');
var w4 : array of Float;
w4.SetLength(9);
for var i := 0 to 8 do w4[i] := 1.0;
var b4 : array of Float := [-8.0];
var dw4 := k4.AddDepthwiseConv2D(in4, w4, b4, 3, 1);
var add4 := k4.AddAdd(dw4, in4);
var relu4 := k4.AddReLU(add4);
k4.MarkOutput(relu4);

var bi4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);
var bo4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);

for var y := 0 to 2 do
   for var x := 0 to 2 do
      bi4.SetData([y, x, 0], 1.0);

TKCLKernelCompiler.Dispatch(k4, [bi4, bo4]);

for var y := 0 to 2 do begin
   var line := '';
   for var x := 0 to 2 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo4.GetData([y, x, 0]));
   end;
   PrintLn(line);
end;

// --- Test 5: DepthwiseConv2D + Add + ReLU6 (residual, reversed Add order) ---
PrintLn('=== Depthwise 3x3 + Add(input, dw) + ReLU6 ===');
var k5 := TKCLKernel.Create;
var in5 := k5.AddInput('in');
var w5 : array of Float;
w5.SetLength(9);
for var i := 0 to 8 do w5[i] := 1.0;
var b5 : array of Float := [0.0];
var dw5 := k5.AddDepthwiseConv2D(in5, w5, b5, 3, 1);
var add5 := k5.AddAdd(in5, dw5);  // reversed order
var relu6_5 := k5.AddReLU6(add5);
k5.MarkOutput(relu6_5);

var bi5 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);
var bo5 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);

for var y := 0 to 2 do
   for var x := 0 to 2 do
      bi5.SetData([y, x, 0], 1.0);

TKCLKernelCompiler.Dispatch(k5, [bi5, bo5]);

for var y := 0 to 2 do begin
   var line := '';
   for var x := 0 to 2 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo5.GetData([y, x, 0]));
   end;
   PrintLn(line);
end;

// --- Test 6: Unfused control (DepthwiseConv2D with multiple consumers) ---
PrintLn('=== Unfused: Depthwise used by ReLU + Add ===');
var k6 := TKCLKernel.Create;
var in6 := k6.AddInput('in');
var w6 : array of Float;
w6.SetLength(9);
for var i := 0 to 8 do w6[i] := 1.0;
var b6 : array of Float := [-5.0];
var dw6 := k6.AddDepthwiseConv2D(in6, w6, b6, 3, 1);
var relu6 := k6.AddReLU(dw6);
var add6 := k6.AddAdd(dw6, in6);  // dw has 2 consumers, no fusion
k6.MarkOutput(relu6);
k6.MarkOutput(add6);

var bi6 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);
var bo6a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);
var bo6b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);

for var y := 0 to 2 do
   for var x := 0 to 2 do
      bi6.SetData([y, x, 0], 1.0);

TKCLKernelCompiler.Dispatch(k6, [bi6, bo6a, bo6b]);

PrintLn('ReLU:');
for var y := 0 to 2 do begin
   var line := '';
   for var x := 0 to 2 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo6a.GetData([y, x, 0]));
   end;
   PrintLn(line);
end;
PrintLn('Add:');
for var y := 0 to 2 do begin
   var line := '';
   for var x := 0 to 2 do begin
      if x > 0 then line := line + ' ';
      line := line + RoundedStr(bo6b.GetData([y, x, 0]));
   end;
   PrintLn(line);
end;
