var k := TKCLKernel.Create;

var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');

var mul := k.AddMul(in1, in2);
var add := k.AddAdd(in1, in2);

k.MarkOutput(mul);
k.MarkOutput(add);

var b_in1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2]);
var b_in2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 2]);
var b_mul := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2]);
var b_add := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2]);

// Fill in1 with 1.0
for var h := 0 to 1 do
   for var w := 0 to 1 do
      for var c := 0 to 1 do
         b_in1.SetData([h, w, c], 1.0);
         
// Fill in2 (the SE vector) with [2.0, 3.0]
b_in2.SetData([0, 0, 0], 2.0);
b_in2.SetData([0, 0, 1], 3.0);

TKCLKernelCompiler.Dispatch(k, [b_in1, b_in2, b_mul, b_add]);

PrintLn('--- Broadcasting Mul (2x2x2 * 1x1x2) ---');
for var h := 0 to 1 do
   for var w := 0 to 1 do
      for var c := 0 to 1 do
         PrintLn('[' + IntToStr(h) + ',' + IntToStr(w) + ',' + IntToStr(c) + ']: ' + FloatToStr(b_mul.GetData([h, w, c])));

PrintLn('--- Broadcasting Add (2x2x2 + 1x1x2) ---');
for var h := 0 to 1 do
   for var w := 0 to 1 do
      for var c := 0 to 1 do
         PrintLn('[' + IntToStr(h) + ',' + IntToStr(w) + ',' + IntToStr(c) + ']: ' + FloatToStr(b_add.GetData([h, w, c])));
