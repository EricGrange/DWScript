uses KernelCompiler;

procedure TestConcatConv;
var
  k: TKCLKernel;
  in1, in2, concat_node, out_node: TKCLNode;
  w, b: array of Float;
  b_in1, b_in2, b_out: TKCLStridedBuffer;
begin
  PrintLn('Testing CONCAT -> CONV_2D...');
  
  k := TKCLKernel.Create;
  in1 := k.AddInput('in1');
  in2 := k.AddInput('in2');
  
  concat_node := k.AddConcat([in1, in2], 2); // concat along channels
  
  w.SetLength(1 * 1 * 32 * 16); b.SetLength(16);
  out_node := k.AddConv2D(concat_node, w, b, 1, 1);
  
  k.MarkOutput(out_node);
  
  b_in1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [16, 16, 16]);
  b_in2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [16, 16, 16]);
  b_out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [16, 16, 16]);
  
  try
    TKCLKernelCompiler.Dispatch(k, [b_in1, b_in2, b_out]);
    PrintLn('  Success!');
  except
    on E: Exception do PrintLn('  FAILED: ' + E.Message);
  end;
end;

try
  TestConcatConv;
except
  on E: Exception do PrintLn('Error: ' + E.Message);
end;
