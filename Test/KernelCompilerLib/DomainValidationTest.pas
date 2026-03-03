var k := TKernel.Create;
var in1 := k.AddInput('in1');
k.MarkOutput(in1);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2, 2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [3, 3]); // mismatch

PrintLn("Domain mismatch test");
try
   TKernelCompiler.Dispatch(k, [b1, b2]);
except
   on E: Exception do
      PrintLn(E.Message);
end;
