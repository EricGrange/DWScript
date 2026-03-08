uses KernelCompiler;

procedure RunTest;
var
  k: TKCLKernel;
  in_node, pool_node: TKCLNode;
  b_in, b_ref, b_jit: TKCLStridedBuffer;
  arr_in, arr_ref, arr_jit: array of Float;
  i: Integer;
  err: Float := 0;
begin
  PrintLn('Highlighting JIT AveragePool Regression (64x64)...');

  k := TKCLKernel.Create;
  in_node := k.AddInput('in');
  pool_node := k.AddGlobalAvgPool(in_node);
  k.MarkOutput(pool_node);

  b_in  := TKCLStridedBuffer.Create(TKCLDataType.Float32, [64, 64, 16]);
  b_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 16]);
  b_jit := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 16]);

  // Fill with a gradient to simulate real activation data
  arr_in.SetLength(64 * 64 * 16);
  for i := 0 to arr_in.High do arr_in[i] := (i mod 100) / 100.0;
  b_in.SetData(arr_in);

  // Compare Backends
  TKCLReferenceCompiler.Dispatch(k, [b_in, b_ref]);
  TKCLJITCompiler.Dispatch(k, [b_in, b_jit]);

  arr_ref.SetLength(16);
  arr_jit.SetLength(16);
  b_ref.GetData(arr_ref);
  b_jit.GetData(arr_jit);

  for i := 0 to 15 do err := err + Abs(arr_ref[i] - arr_jit[i]);

  if (err / 16) > 0.1 then
    PrintLn('[FAIL] JIT Mathematical Regression Detected')
  else
    PrintLn('[PASS] JIT results match Reference');
end;

try
  RunTest;
except
  on E: Exception do PrintLn('Error: ' + E.Message);
end;