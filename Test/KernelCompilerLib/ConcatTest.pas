var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');

// Concat along Axis 2 (Channels)
var cat := k.AddConcat([in1, in2], 2);
k.MarkOutput(cat);

var b1 := TStridedBuffer.Create(TDataType.Float32, [1, 2, 2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [1, 2, 3]);
var b_out := TStridedBuffer.Create(TDataType.Float32, [1, 2, 5]);

// b1 channels
b1.SetData([0, 0, 0], 1);
b1.SetData([0, 0, 1], 2);
b1.SetData([0, 1, 0], 3);
b1.SetData([0, 1, 1], 4);

// b2 channels
b2.SetData([0, 0, 0], 10);
b2.SetData([0, 0, 1], 20);
b2.SetData([0, 0, 2], 30);
b2.SetData([0, 1, 0], 40);
b2.SetData([0, 1, 1], 50);
b2.SetData([0, 1, 2], 60);

TKernelCompiler.Dispatch(k, [b1, b2, b_out]);

for var x := 0 to 1 do begin
   for var c := 0 to 4 do
      Print(FloatToStr(b_out.GetData([0, x, c])) + ' ');
   PrintLn('');
end;
