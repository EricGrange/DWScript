uses KernelCompiler;

procedure Test(const Label: String; ActivationType: String);
var
  k: TKCLKernel;
  in_node, pool_node, act_node: TKCLNode;
  b_in, b_ref, b_jit: TKCLStridedBuffer;
  arr_in, arr_ref, arr_jit: array of Float;
  i: Integer;
  err: Float := 0;
begin
  PrintLn('Testing: ' + Label);
  
  k := TKCLKernel.Create;
  in_node := k.AddInput('in');
  
  if ActivationType = 'ReLU' then act_node := k.AddReLU(in_node)
  else if ActivationType = 'HardSwish' then act_node := k.AddHardSwish(in_node)
  else act_node := in_node;
  
  pool_node := k.AddGlobalAvgPool(act_node);
  k.MarkOutput(pool_node);
  
  b_in  := TKCLStridedBuffer.Create(TKCLDataType.Float32, [64, 64, 16]);
  b_ref := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 16]);
  b_jit := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 16]);
  
  arr_in.SetLength(64 * 64 * 16);
  for i := 0 to arr_in.High do arr_in[i] := (i mod 100) / 100.0;
  b_in.SetData(arr_in);
  
  TKCLReferenceCompiler.Dispatch(k, [b_in, b_ref]);
  TKCLJITCompiler.Dispatch(k, [b_in, b_jit]);
  
  arr_ref.SetLength(16);
  arr_jit.SetLength(16);
  b_ref.GetData(arr_ref);
  b_jit.GetData(arr_jit);
  
  err := 0;
  for i := 0 to 15 do err := err + Abs(arr_ref[i] - arr_jit[i]);
  
  if (err / 16) > 0.001 then
    PrintLn('[FAIL] JIT Map Batch Sync Error Detected')
  else
    PrintLn('[PASS] JIT Map Sync Successful');
end;

try
  Test('None -> GlobalAvgPool', 'None');
  Test('ReLU -> GlobalAvgPool', 'ReLU');
  Test('HardSwish -> GlobalAvgPool', 'HardSwish');
except
  on E: Exception do PrintLn('Error: ' + E.Message);
end;