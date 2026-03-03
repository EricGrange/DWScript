var k := TKernel.Create;
var in1 := k.AddInput('in1');
var sum := k.AddAdd(in1, in1);
k.MarkOutput(sum);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2, 3, 2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [2, 3, 2]);

var val : Integer := 1;
for var z := 0 to 1 do
   for var y := 0 to 2 do
      for var x := 0 to 1 do begin
         b1.SetData([z, y, x], val);
         val := val + 1;
      end;

TKernelCompiler.Dispatch(k, [b1, b2]);

for var z := 0 to 1 do
   for var y := 0 to 2 do
      for var x := 0 to 1 do
         PrintLn(FloatToStr(b2.GetData([z, y, x])));
