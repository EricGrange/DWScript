var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var w1 : array of Float := [0.5, 0.5]; // [KH, KW, Cin, Cout] = [1, 1, 1, 2]
var b1 : array of Float := [1.0, 1.0]; // Bias for 2 output channels
var conv := k.AddConv2D(in1, w1, b1, 1, 1); // Pointwise 1x1
k.MarkOutput(conv);

// Test case 1: totalPixels = 1 (Tail only)
var b_in1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 1]); // [H, W, C]
b_in1.SetData([10.0]);
var b_out1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 2]);
TKCLKernelCompiler.Dispatch(k, [b_in1, b_out1]);

PrintLn('-- JIT Tail Only (1 pixel) --');
PrintLn('(0,0,0) = ' + FloatToStr(b_out1.GetData([0, 0, 0]))); // 10*0.5 + 1.0 = 6.0
PrintLn('(0,0,1) = ' + FloatToStr(b_out1.GetData([0, 0, 1]))); // 10*0.5 + 1.0 = 6.0

// Test case 2: totalPixels = 9 (1 tile of 8 + 1 tail)
var b_in2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 1]);
b_in2.SetData([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]);
var b_out2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 3, 2]);
TKCLKernelCompiler.Dispatch(k, [b_in2, b_out2]);

PrintLn('-- JIT Tile + Tail (9 pixels) --');
for var i := 0 to 8 do
   Print(FloatToStr(b_out2.GetData([i div 3, i mod 3, 0])) + ' ');
PrintLn('');

// Test case 3: inC > 16 (Loop handling)
var k3 := TKCLKernel.Create;
var in3 := k3.AddInput('in3');
var w3 : array of Float; w3.SetLength(20); for var i := 0 to 19 do w3[i] := 0.1;
var b3 : array of Float := [0.0];
var conv3 := k3.AddConv2D(in3, w3, b3, 1, 1);
k3.MarkOutput(conv3);

var data3 : array of Float; data3.SetLength(20);
for var i := 0 to 19 do data3[i] := 1.0;
var b_in3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 20]);
b_in3.SetData(data3);
var b_out3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 1]);
TKCLKernelCompiler.Dispatch(k3, [b_in3, b_out3]);

PrintLn('-- JIT Large Cin (20 channels) --');
PrintLn('Result = ' + FloatToStr(b_out3.GetData([0, 0, 0]))); // 20 * (1.0 * 0.1) = 2.0
