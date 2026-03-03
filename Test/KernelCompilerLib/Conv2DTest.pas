var k := TKernel.Create;
var in1 := k.AddInput('in1');
var conv := k.AddConv2D(in1);
k.MarkOutput(conv);

var b1 := TStridedBuffer.Create(TDataType.Float32, [3, 3]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [3, 3]);

var y, x : Integer;
for y := 0 to 2 do
   for x := 0 to 2 do
      b1.SetData([y, x], 1);

TKernelCompiler.Dispatch(k, [b1, b2]);

for y := 0 to 2 do begin
   for x := 0 to 2 do
      Print(FloatToStr(b2.GetData([y, x])) + ' ');
   PrintLn('');
end;
