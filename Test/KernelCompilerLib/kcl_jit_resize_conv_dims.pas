uses KernelCompiler;

procedure TestConv(stride: Integer);
var
  k: TKCLKernel;
  in_node, res_node, out_node: TKCLNode;
  w, b: array of Float;
  b_in, b_out: TKCLStridedBuffer;
begin
  PrintLn('Testing RESIZE_BILINEAR -> CONV_2D (Stride ' + IntToStr(stride) + ')...');
  
  k := TKCLKernel.Create;
  in_node := k.AddInput('in');
  res_node := k.AddResizeBilinear(in_node, 32, 32, false, false);
  
  w.SetLength(1 * 1 * 16 * 16); b.SetLength(16);
  out_node := k.AddConv2D(res_node, w, b, 1, stride);
  
  k.MarkOutput(out_node);
  
  b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [16, 16, 16]);
  b_out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [32 div stride, 32 div stride, 16]);
  
  try
    TKCLKernelCompiler.Dispatch(k, [b_in, b_out]);
    PrintLn('  Success!');
  except
    on E: Exception do PrintLn('  FAILED: ' + E.Message);
  end;
end;

try
  TestConv(1);
  TestConv(2);
except
  on E: Exception do PrintLn('Error: ' + E.Message);
end;
