var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var sum := k.AddAdd(in1, in2);
k.MarkOutput(sum);

var b1 := TStridedBuffer.Create(TDataType.Float32, [4]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [4]);
var b3 := TStridedBuffer.Create(TDataType.Float32, [4]);

var i : Integer;
for i := 0 to 3 do begin
   b1.SetData([i], i + 1);
   b2.SetData([i], (i + 1) * 10);
end;

TKernelCompiler.Dispatch(k, [b1, b2, b3]);

for i := 0 to 3 do begin
   PrintLn(b3.GetData([i]));
end;
