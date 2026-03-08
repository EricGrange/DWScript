// Test: Conv2D + Add + Activation (residual connection) fusion
// Verifies the complete residual block pattern:
//   residual = input
//   x = Conv2D(input)
//   x = x + residual
//   x = ReLU(x)

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 10000) / 10000;
   Result := FloatToStr(rv);
   if Pos('.', Result) = 0 then Result := Result + '.0000'
   else begin
      while Length(Result) - Pos('.', Result) < 4 do Result := Result + '0';
   end;
end;

// --- Test 1: Pointwise Conv2D + Add + ReLU (classic residual) ---
PrintLn('=== Residual: Conv2D 1x1 + Add + ReLU ===');
var k1 := TKCLKernel.Create;
var in1 := k1.AddInput('in');
// Identity-like conv that slightly modifies the input
var w1 : array of Float := [0.5];
var b1 : array of Float := [-1.0];
var conv1 := k1.AddConv2D(in1, w1, b1, 1, 1);
var add1 := k1.AddAdd(conv1, in1);  // residual connection
var relu1 := k1.AddReLU(add1);
k1.MarkOutput(relu1);

var bi1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 4, 1]);
var bo1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 4, 1]);

// Input: -2, -1, 0, 1, 2, 3, 4, 5
for var y := 0 to 1 do
   for var x := 0 to 3 do
      bi1.SetData([y, x, 0], y * 4 + x - 2);

TKCLKernelCompiler.Dispatch(k1, [bi1, bo1]);

for var y := 0 to 1 do
   for var x := 0 to 3 do
      PrintLn(RoundedStr(bo1.GetData([y, x, 0])));

// --- Test 2: Residual with reversed Add operand order ---
PrintLn('=== Residual: Add(input, Conv2D) + ReLU ===');
var k2 := TKCLKernel.Create;
var in2 := k2.AddInput('in');
var w2 : array of Float := [0.5];
var b2_ : array of Float := [-1.0];
var conv2 := k2.AddConv2D(in2, w2, b2_, 1, 1);
var add2 := k2.AddAdd(in2, conv2);  // reversed order
var relu2 := k2.AddReLU(add2);
k2.MarkOutput(relu2);

var bi2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 4, 1]);
var bo2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 4, 1]);

for var y := 0 to 1 do
   for var x := 0 to 3 do
      bi2.SetData([y, x, 0], y * 4 + x - 2);

TKCLKernelCompiler.Dispatch(k2, [bi2, bo2]);

for var y := 0 to 1 do
   for var x := 0 to 3 do
      PrintLn(RoundedStr(bo2.GetData([y, x, 0])));

// --- Test 3: Conv2D 3x3 + Add + ReLU6 (KxK residual) ---
PrintLn('=== Residual: Conv2D 3x3 + Add + ReLU6 ===');
var k3 := TKCLKernel.Create;
var in3 := k3.AddInput('in');
var w3 : array of Float;
w3.SetLength(9);
for var i := 0 to 8 do w3[i] := 0.5;
var b3 : array of Float := [0.0];
var conv3 := k3.AddConv2D(in3, w3, b3, 3, 1);
var add3 := k3.AddAdd(conv3, in3);
var relu6_3 := k3.AddReLU6(add3);
k3.MarkOutput(relu6_3);

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

// --- Test 4: Multi-channel residual ---
PrintLn('=== Residual: Conv2D 1x1 (2ch) + Add + ReLU ===');
var k4 := TKCLKernel.Create;
var in4 := k4.AddInput('in');
// 2-channel identity-ish conv: slight scale-down
var w4 : array of Float := [0.5, 0.0, 0.0, 0.5];
var b4 : array of Float := [0.0, 0.0];
var conv4 := k4.AddConv2D(in4, w4, b4, 1, 1);
var add4 := k4.AddAdd(conv4, in4);
var relu4 := k4.AddReLU(add4);
k4.MarkOutput(relu4);

var bi4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2]);
var bo4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2]);

bi4.SetData([0,0,0], 2.0); bi4.SetData([0,0,1], -3.0);
bi4.SetData([0,1,0], -1.0); bi4.SetData([0,1,1], 4.0);
bi4.SetData([1,0,0], 0.0); bi4.SetData([1,0,1], 1.0);
bi4.SetData([1,1,0], 3.0); bi4.SetData([1,1,1], -2.0);

TKCLKernelCompiler.Dispatch(k4, [bi4, bo4]);

for var y := 0 to 1 do
   for var x := 0 to 1 do
      PrintLn('(' + RoundedStr(bo4.GetData([y,x,0])) + ', ' + RoundedStr(bo4.GetData([y,x,1])) + ')');
