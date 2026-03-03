var k := TKernel.Create;
var in1 := k.AddInput('in1');

var weights : array of Float;
weights.SetLength(9);
for var i := 0 to 8 do weights[i] := 1.0;

var bias : array of Float;
bias.SetLength(1);
bias[0] := 0.0;

var conv := k.AddConv2D(in1, weights, bias, 3, 1);
k.MarkOutput(conv);

var b1 := TStridedBuffer.Create(TDataType.Float32, [4, 4, 1]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [4, 4, 1]);

var y, x : Integer;
var val := 1;
for y := 0 to 3 do
   for x := 0 to 3 do begin
      b1.SetData([y, x, 0], val);
      val := val + 1;
   end;

TKernelCompiler.Dispatch(k, [b1, b2]);

for y := 0 to 3 do begin
   for x := 0 to 3 do
      Print(FloatToStr(b2.GetData([y, x, 0])) + ' ');
   PrintLn('');
end;
