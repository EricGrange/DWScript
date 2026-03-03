var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var sum := k.AddAdd(in1, in2);
k.MarkOutput(sum);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [8]);
var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [8]);
var b3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [8]);

var i : Integer;
for i := 0 to 7 do begin
   b1.SetData([i], i * 1.5 - 3.0);
   b2.SetData([i], (i + 1) * -2.5);
end;

TKCLKernelCompiler.Dispatch(k, [b1, b2, b3]);

for i := 0 to 7 do begin
   PrintLn(FloatToStr(b3.GetData([i])));
end;
