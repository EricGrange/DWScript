var k := TKernel.Create;
var in1 := k.AddInput('in1');
var conv := k.AddConv2D(in1);
k.MarkOutput(conv);

var b1 := TStridedBuffer.Create(TDataType.Float32, [4, 4]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [4, 4]);

var y, x : Integer;
var val := 1;
for y := 0 to 3 do
   for x := 0 to 3 do begin
      b1.SetData([y, x], val);
      val := val + 1;
   end;

TKernelCompiler.Dispatch(k, [b1, b2]);

for y := 0 to 3 do begin
   for x := 0 to 3 do
      Print(FloatToStr(b2.GetData([y, x])) + ' ');
   PrintLn('');
end;
