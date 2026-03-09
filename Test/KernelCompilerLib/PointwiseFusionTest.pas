// Test: Comprehensive Pointwise Fusion patterns
// Covers binary ops, broadcasting, dequantize, and complex branching with activations.

function RoundedStr(v : Float) : String;
begin
   var rv := Round(v * 10000) / 10000;
   Result := FloatToStr(rv);
   if Pos('.', Result) = 0 then Result := Result + '.0000';
   while Length(Result) - Pos('.', Result) < 4 do Result := Result + '0';
end;

procedure PrintBuffer(const b : TKCLStridedBuffer; count : Integer);
begin
   for var i := 0 to count - 1 do
      PrintLn(RoundedStr(b.GetData([0, i, 0])));
end;

// 1. Basic Binary + Activation
PrintLn('=== Sub + ReLU6 ===');
var k1 := TKCLKernel.Create;
var in1a := k1.AddInput('in1a');
var in1b := k1.AddInput('in1b');
var sub1 := k1.AddSub(in1a, in1b);
var relu6_1 := k1.AddReLU6(sub1);
k1.MarkOutput(relu6_1);

var b1a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var b1b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
b1a.SetData([10.0, 5.0, 2.0, -1.0]);
b1b.SetData([2.0, 1.0, 8.0, 2.0]);

TKCLKernelCompiler.Dispatch(k1, [b1a, b1b, bo1]);
PrintBuffer(bo1, 4);

// 2. Mul + HardSwish
PrintLn('=== Mul + HardSwish ===');
var k2 := TKCLKernel.Create;
var in2a := k2.AddInput('in2a');
var in2b := k2.AddInput('in2b');
var mul2 := k2.AddMul(in2a, in2b);
var hswish2 := k2.AddHardSwish(mul2);
k2.MarkOutput(hswish2);

var b2a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var b2b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
b2a.SetData([2.0, -1.0, 0.5, -4.0]);
b2b.SetData([1.0, 2.0, 4.0, -1.0]);

TKCLKernelCompiler.Dispatch(k2, [b2a, b2b, bo2]);
PrintBuffer(bo2, 4);

// 3. Broadcasting + Activation
PrintLn('=== Add (Broadcast Scalar) + ReLU ===');
var k3 := TKCLKernel.Create;
var in3 := k3.AddInput('in3');
var add3 := k3.AddAdd(in3, k1.AddConstant(-1.0, [1])); // use k1 for convenience or k3
var relu3 := k3.AddReLU(add3);
k3.MarkOutput(relu3);

var b3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
b3.SetData([5.0, 1.0, 0.5, -2.0]);

TKCLKernelCompiler.Dispatch(k3, [b3, bo3]);
PrintBuffer(bo3, 4);

// 4. Dequantize + ReLU
PrintLn('=== Dequantize + ReLU ===');
var k4 := TKCLKernel.Create;
var in4 := k4.AddInput('in4');
var deq4 := k4.AddDequantize(in4, 0.1, 10.0);
var relu4 := k4.AddReLU(deq4);
k4.MarkOutput(relu4);

var b4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
var bo4 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 1]);
b4.SetData([15.0, 10.0, 5.0, 0.0]); // (15-10)*0.1 = 0.5, (10-10)*0.1 = 0, (5-10)*0.1 = -0.5

TKCLKernelCompiler.Dispatch(k4, [b4, bo4]);
PrintBuffer(bo4, 4);

// 5. Branching: Node used by fused and unfused consumers
PrintLn('=== Branching: Fused ReLU + Unfused Mul ===');
var k5 := TKCLKernel.Create;
var in5a := k5.AddInput('in5a');
var in5b := k5.AddInput('in5b');
var add5 := k5.AddAdd(in5a, in5b);
var relu5 := k5.AddReLU(add5);
var mul5 := k5.AddMul(add5, k5.AddConstant(2.0, [1]));
k5.MarkOutput(relu5);
k5.MarkOutput(mul5);

var b5a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 1]);
var b5b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 1]);
var bo5_relu := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 1]);
var bo5_mul := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 1]);
b5a.SetData([1.0, -5.0]);
b5b.SetData([2.0, 1.0]);

TKCLKernelCompiler.Dispatch(k5, [b5a, b5b, bo5_relu, bo5_mul]);
PrintLn('ReLU:');
PrintBuffer(bo5_relu, 2);
PrintLn('Mul:');
PrintBuffer(bo5_mul, 2);

// 6. Chain of Fusions: (A+B)+ReLU -> (Res*C)+ReLU6
PrintLn('=== Chain: (Add+ReLU) -> (Mul+ReLU6) ===');
var k6 := TKCLKernel.Create;
var in6a := k6.AddInput('in6a');
var in6b := k6.AddInput('in6b');
var add6 := k6.AddAdd(in6a, in6b);
var relu6_6a := k6.AddReLU(add6);
var mul6 := k6.AddMul(relu6_6a, k6.AddConstant(10.0, [1]));
var relu6_6b := k6.AddReLU6(mul6);
k6.MarkOutput(relu6_6b);

var b6a := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 1]);
var b6b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 1]);
var bo6 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 1]);
b6a.SetData([0.1, -0.5]);
b6b.SetData([0.2, 0.1]);

TKCLKernelCompiler.Dispatch(k6, [b6a, b6b, bo6]);
PrintBuffer(bo6, 2);
