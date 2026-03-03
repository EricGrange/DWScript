var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var sum := k.AddAdd(in1, in2);
k.MarkOutput(sum);

var b1 := TStridedBuffer.Create(TDataType.Float32, [8]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [8]);
var b3 := TStridedBuffer.Create(TDataType.Float32, [8]);

var i : Integer;
for i := 0 to 7 do begin
   b1.SetData([i], i * 1.5 - 3.0);
   b2.SetData([i], (i + 1) * -2.5);
end;

TKernelCompiler.Dispatch(k, [b1, b2, b3]);

for i := 0 to 7 do begin
   PrintLn(FloatToStr(b3.GetData([i])));
end;
