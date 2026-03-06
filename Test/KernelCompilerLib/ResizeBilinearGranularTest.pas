var k := TKCLKernel.Create;
var in1 := k.AddInput('in1'); // Expected rank 3: [H, W, C]

// 1. Rank mismatch: [1, 2, 2, 1] resized to [4, 4, 1]
var rs_rank := k.AddResizeBilinear(in1, 4, 4);
k.MarkOutput(rs_rank);

// Input: [1, 2, 2, 1]
var b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 2, 1]);
b_in.SetData([10.0, 20.0, 30.0, 40.0]);

// Output: [1, 4, 4, 1]
var b_out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 4, 1]);

TKCLKernelCompiler.Dispatch(k, [b_in, b_out]);

PrintLn('-- Rank Mismatch (Batch=1) --');
for var y := 0 to 3 do begin
   for var x := 0 to 3 do
      Print(FloatToStr(b_out.GetData([0, y, x, 0])) + ' ');
   PrintLn('');
end;

// 2. Large scale factor: 1x1 to 4x4
var k2 := TKCLKernel.Create;
var in2 := k2.AddInput('in2');
var rs_large := k2.AddResizeBilinear(in2, 4, 4);
k2.MarkOutput(rs_large);

var b_in2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 1]);
b_in2.SetData([50.0]);
var b_out2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);

TKCLKernelCompiler.Dispatch(k2, [b_in2, b_out2]);

PrintLn('-- Large Scale (1x1 to 4x4) --');
for var y := 0 to 3 do begin
   for var x := 0 to 3 do
      Print(FloatToStr(b_out2.GetData([y, x, 0])) + ' ');
   PrintLn('');
end;
