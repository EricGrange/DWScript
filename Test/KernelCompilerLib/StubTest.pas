var k := TKernel.Create;
var in1 := k.AddInput('in1');

var weights : array of Float;
weights.SetLength(1);
weights[0] := 1.0;

var bias : array of Float;
bias.SetLength(1);
bias[0] := 0.0;

var conv := k.AddConv2D(in1, weights, bias, 1, 1);
var gap := k.AddGlobalAvgPool(conv);
k.MarkOutput(gap);

var b1 := TStridedBuffer.Create(TDataType.Float32, [1, 1, 1]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [1, 1, 1]);

TKernelCompiler.Dispatch(k, [b1, b2]);
PrintLn(b2.GetData([0, 0, 0]));

var b16 := TStridedBuffer.Create(TDataType.Float16, [1]);
b16.SetData([0], 1.0);
PrintLn(b16.GetData([0]));

TKernelCompiler.Dispatch(k, []); // Should not crash
PrintLn('Empty OK');
