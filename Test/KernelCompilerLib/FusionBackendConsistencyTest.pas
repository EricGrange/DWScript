// Test: Verify fusion produces consistent results across backends.
// Runs fused pattern graphs through reference and default (optimized) backends,
// comparing results to ensure fusion does not change numerical outcomes.

function CompareBuffers3D(b1, b2 : TKCLStridedBuffer; d0, d1, d2 : Integer; label_ : String) : Boolean;
begin
   Result := True;
   for var y := 0 to d0 - 1 do
      for var x := 0 to d1 - 1 do
         for var c := 0 to d2 - 1 do begin
            var v1 := b1.GetData([y, x, c]);
            var v2 := b2.GetData([y, x, c]);
            if Abs(v1 - v2) > 0.001 then begin
               PrintLn(label_ + ' FAIL at [' + IntToStr(y) + ',' + IntToStr(x) + ',' + IntToStr(c) + ']');
               Result := False;
            end;
         end;
end;

// --- Pattern A: Conv2D 1x1 + ReLU (pointwise fusion) ---
var kA := TKCLKernel.Create;
var inA := kA.AddInput('in');
var wA : array of Float := [0.8, 0.2, -0.3, 0.7, 0.5, -0.5, 0.1, 0.9];
var bA : array of Float := [0.1, -0.1];
var convA := kA.AddConv2D(inA, wA, bA, 1, 1);
var reluA := kA.AddReLU(convA);
kA.MarkOutput(reluA);

var biA := TKCLStridedBuffer.Create(TKCLDataType.Float32, [6, 6, 4]);
var boA_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [6, 6, 2]);
var boA_opt := TKCLStridedBuffer.Create(TKCLDataType.Float32, [6, 6, 2]);

for var y := 0 to 5 do
   for var x := 0 to 5 do
      for var c := 0 to 3 do
         biA.SetData([y, x, c], (y - 3) * 0.5 + (x - 3) * 0.3 + c * 0.7 - 1.0);

TKCLReferenceCompiler.Dispatch(kA, [biA, boA_ref]);
TKCLKernelCompiler.Dispatch(kA, [biA, boA_opt]);

if CompareBuffers3D(boA_ref, boA_opt, 6, 6, 2, 'ConvReLU') then
   PrintLn('Conv2D 1x1 + ReLU: PASS');

// --- Pattern B: Conv2D 1x1 + Add + ReLU (residual fusion) ---
var kB := TKCLKernel.Create;
var inB := kB.AddInput('in');
var wB : array of Float := [0.5, 0.0, 0.0, 0.5];
var bB : array of Float := [-0.2, 0.3];
var convB := kB.AddConv2D(inB, wB, bB, 1, 1);
var addB := kB.AddAdd(convB, inB);
var reluB := kB.AddReLU(addB);
kB.MarkOutput(reluB);

var biB := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 5, 2]);
var boB_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 5, 2]);
var boB_opt := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 5, 2]);

for var y := 0 to 3 do
   for var x := 0 to 4 do
      for var c := 0 to 1 do
         biB.SetData([y, x, c], (y * 5 + x) * 0.2 - 2.0 + c * 1.5);

TKCLReferenceCompiler.Dispatch(kB, [biB, boB_ref]);
TKCLKernelCompiler.Dispatch(kB, [biB, boB_opt]);

if CompareBuffers3D(boB_ref, boB_opt, 4, 5, 2, 'ResidualReLU') then
   PrintLn('Conv2D 1x1 + Add + ReLU: PASS');

// --- Pattern C: Conv2D 3x3 + ReLU6 (KxK fusion) ---
var kC := TKCLKernel.Create;
var inputC := kC.AddInput('in');
var wC : array of Float;
wC.SetLength(9 * 2 * 3);
for var i := 0 to wC.Length - 1 do wC[i] := 0.1 * ((i mod 7) - 3);
var bC : array of Float := [0.5, -0.3, 0.1];
var convC := kC.AddConv2D(inputC, wC, bC, 3, 1);
var relu6C := kC.AddReLU6(convC);
kC.MarkOutput(relu6C);

var biC := TKCLStridedBuffer.Create(TKCLDataType.Float32, [5, 5, 2]);
var boC_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [5, 5, 3]);
var boC_opt := TKCLStridedBuffer.Create(TKCLDataType.Float32, [5, 5, 3]);

for var y := 0 to 4 do
   for var x := 0 to 4 do
      for var c := 0 to 1 do
         biC.SetData([y, x, c], (y - 2) * 0.4 + (x - 2) * 0.6 + c * 0.8);

TKCLReferenceCompiler.Dispatch(kC, [biC, boC_ref]);
TKCLKernelCompiler.Dispatch(kC, [biC, boC_opt]);

if CompareBuffers3D(boC_ref, boC_opt, 5, 5, 3, 'Conv3x3ReLU6') then
   PrintLn('Conv2D 3x3 + ReLU6: PASS');

// --- Pattern D: Conv2D 3x3 + Add + ReLU (KxK residual fusion) ---
var kD := TKCLKernel.Create;
var inD := kD.AddInput('in');
var wD : array of Float;
wD.SetLength(9);
for var i := 0 to 8 do wD[i] := 0.1;
var bD : array of Float := [0.0];
var convD := kD.AddConv2D(inD, wD, bD, 3, 1);
var addD := kD.AddAdd(convD, inD);
var reluD := kD.AddReLU(addD);
kD.MarkOutput(reluD);

var biD := TKCLStridedBuffer.Create(TKCLDataType.Float32, [6, 6, 1]);
var boD_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [6, 6, 1]);
var boD_opt := TKCLStridedBuffer.Create(TKCLDataType.Float32, [6, 6, 1]);

for var y := 0 to 5 do
   for var x := 0 to 5 do
      biD.SetData([y, x, 0], (y - 3) * 0.5 + (x - 3) * 0.4);

TKCLReferenceCompiler.Dispatch(kD, [biD, boD_ref]);
TKCLKernelCompiler.Dispatch(kD, [biD, boD_opt]);

if CompareBuffers3D(boD_ref, boD_opt, 6, 6, 1, 'KxKResidual') then
   PrintLn('Conv2D 3x3 + Add + ReLU: PASS');

// --- Pattern E: DepthwiseConv2D 3x3 + ReLU (depthwise activation fusion) ---
var kE := TKCLKernel.Create;
var inE := kE.AddInput('in');
var wE : array of Float;
wE.SetLength(9 * 3);
for var i := 0 to wE.Length - 1 do wE[i] := 0.2 * ((i mod 5) - 2);
var bE : array of Float := [0.1, -0.2, 0.3];
var dwE := kE.AddDepthwiseConv2D(inE, wE, bE, 3, 1);
var reluE := kE.AddReLU(dwE);
kE.MarkOutput(reluE);

var biE := TKCLStridedBuffer.Create(TKCLDataType.Float32, [5, 5, 3]);
var boE_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [5, 5, 3]);
var boE_opt := TKCLStridedBuffer.Create(TKCLDataType.Float32, [5, 5, 3]);

for var y := 0 to 4 do
   for var x := 0 to 4 do
      for var c := 0 to 2 do
         biE.SetData([y, x, c], (y - 2) * 0.3 + (x - 2) * 0.5 + c * 0.6);

TKCLReferenceCompiler.Dispatch(kE, [biE, boE_ref]);
TKCLKernelCompiler.Dispatch(kE, [biE, boE_opt]);

if CompareBuffers3D(boE_ref, boE_opt, 5, 5, 3, 'DWConvReLU') then
   PrintLn('Depthwise 3x3 + ReLU: PASS');

// --- Pattern F: DepthwiseConv2D 3x3 + Add + ReLU6 (depthwise residual fusion) ---
var kF := TKCLKernel.Create;
var inF := kF.AddInput('in');
var wF : array of Float;
wF.SetLength(9 * 2);
for var i := 0 to wF.Length - 1 do wF[i] := 0.15;
var bF : array of Float := [-0.5, 0.2];
var dwF := kF.AddDepthwiseConv2D(inF, wF, bF, 3, 1);
var addF := kF.AddAdd(dwF, inF);
var relu6F := kF.AddReLU6(addF);
kF.MarkOutput(relu6F);

var biF := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);
var boF_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);
var boF_opt := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);

for var y := 0 to 3 do
   for var x := 0 to 3 do
      for var c := 0 to 1 do
         biF.SetData([y, x, c], (y * 4 + x) * 0.3 - 1.0 + c * 0.8);

TKCLReferenceCompiler.Dispatch(kF, [biF, boF_ref]);
TKCLKernelCompiler.Dispatch(kF, [biF, boF_opt]);

if CompareBuffers3D(boF_ref, boF_opt, 4, 4, 2, 'DWResidual') then
   PrintLn('Depthwise 3x3 + Add + ReLU6: PASS');

// --- Pattern G: Add + ReLU (element-wise fusion) ---
var kG := TKCLKernel.Create;
var inG1 := kG.AddInput('a');
var inG2 := kG.AddInput('b');
var addG := kG.AddAdd(inG1, inG2);
var reluG := kG.AddReLU(addG);
kG.MarkOutput(reluG);

var biG1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 8, 4]);
var biG2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 8, 4]);
var boG_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 8, 4]);
var boG_opt := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3, 8, 4]);

for var y := 0 to 2 do
   for var x := 0 to 7 do
      for var c := 0 to 3 do begin
         biG1.SetData([y, x, c], (y * 8 + x) * 0.2 - 3.0 + c * 0.5);
         biG2.SetData([y, x, c], (y * 8 + x) * 0.1 + 1.0 - c * 0.3);
      end;

TKCLReferenceCompiler.Dispatch(kG, [biG1, biG2, boG_ref]);
TKCLKernelCompiler.Dispatch(kG, [biG1, biG2, boG_opt]);

if CompareBuffers3D(boG_ref, boG_opt, 3, 8, 4, 'AddReLU') then
   PrintLn('Add + ReLU: PASS');

// --- Pattern H: Mul + HardSwish (element-wise fusion) ---
var kH := TKCLKernel.Create;
var inH1 := kH.AddInput('a');
var inH2 := kH.AddInput('b');
var mulH := kH.AddMul(inH1, inH2);
var hsH := kH.AddHardSwish(mulH);
kH.MarkOutput(hsH);

var biH1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 6, 3]);
var biH2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 6, 3]);
var boH_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 6, 3]);
var boH_opt := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 6, 3]);

for var y := 0 to 1 do
   for var x := 0 to 5 do
      for var c := 0 to 2 do begin
         biH1.SetData([y, x, c], (y * 6 + x) * 0.3 - 1.5 + c * 0.4);
         biH2.SetData([y, x, c], (y * 6 + x) * 0.2 + 0.5 - c * 0.2);
      end;

TKCLReferenceCompiler.Dispatch(kH, [biH1, biH2, boH_ref]);
TKCLKernelCompiler.Dispatch(kH, [biH1, biH2, boH_opt]);

if CompareBuffers3D(boH_ref, boH_opt, 2, 6, 3, 'MulHardSwish') then
   PrintLn('Mul + HardSwish: PASS');
