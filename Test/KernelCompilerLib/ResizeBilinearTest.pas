var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');

// Target height: 4, width: 4
var rs := k.AddResizeBilinear(in1, 4, 4);
k.MarkOutput(rs);

var b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 1]);
var b_out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);

// 2x2 grid:
// 10  20
// 30  40
b_in.SetData([0, 0, 0], 10);
b_in.SetData([0, 1, 0], 20);
b_in.SetData([1, 0, 0], 30);
b_in.SetData([1, 1, 0], 40);

TKCLKernelCompiler.Dispatch(k, [b_in, b_out]);

for var y := 0 to 3 do begin
   for var x := 0 to 3 do
      Print(FloatToStr(b_out.GetData([y, x, 0])) + ' ');
   PrintLn('');
end;
