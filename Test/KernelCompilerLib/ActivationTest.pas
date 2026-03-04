var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');

var relu := k.AddReLU(in1);
var relu6 := k.AddReLU6(in1);
var hs := k.AddHardSwish(in1);
var sig := k.AddSigmoid(in1);
var sm := k.AddSoftMax(in1, 0);

k.MarkOutput(relu);
k.MarkOutput(relu6);
k.MarkOutput(hs);
k.MarkOutput(sig);
k.MarkOutput(sm);

var testValues : array of Float := [-500.0, -5.0, -1.5, 0.0, 3.0, 10.0, 500.0];
var count := testValues.Length;

var b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var b_relu := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var b_relu6 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var b_hs := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var b_sig := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);
var b_sm := TKCLStridedBuffer.Create(TKCLDataType.Float32, [count]);

for var i := 0 to count - 1 do
   b_in.SetData([i], testValues[i]);

TKCLKernelCompiler.Dispatch(k, [b_in, b_relu, b_relu6, b_hs, b_sig, b_sm]);

PrintLn('--- Inputs ---');
for var i := 0 to count - 1 do PrintLn(FloatToStr(testValues[i]));

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 1000) / 1000;
   Result := FloatToStr(rv);
   if Pos('.', Result) = 0 then Result := Result + '.000'
   else begin
      while Length(Result) - Pos('.', Result) < 3 do Result := Result + '0';
   end;
end;

PrintLn('--- ReLU ---');
for var i := 0 to count - 1 do PrintLn(RoundedStr(b_relu.GetData([i])));

PrintLn('--- ReLU6 ---');
for var i := 0 to count - 1 do PrintLn(RoundedStr(b_relu6.GetData([i])));

PrintLn('--- HardSwish ---');
for var i := 0 to count - 1 do PrintLn(RoundedStr(b_hs.GetData([i])));

PrintLn('--- Sigmoid ---');
for var i := 0 to count - 1 do PrintLn(RoundedStr(b_sig.GetData([i])));

var smSum := b_sm.GetData([0]) + b_sm.GetData([1]) + b_sm.GetData([2]) + b_sm.GetData([3]) + b_sm.GetData([4]) + b_sm.GetData([5]) + b_sm.GetData([6]);
PrintLn('--- SoftMax (sum: ' + FloatToStr(Round(smSum * 10) / 10) + ') ---');
for var i := 0 to count - 1 do PrintLn(RoundedStr(b_sm.GetData([i])));
