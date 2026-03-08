uses KernelCompiler;

procedure TestConv2DTransposeConv;
var
  k: TKCLKernel;
  in1, transp_node, out_node: TKCLNode;
  w, b: array of Float;
  w2, b2: array of Float;
  b_in1, b_out: TKCLStridedBuffer;
begin
  PrintLn('Testing CONV_2D_TRANSPOSE -> CONV_2D...');
  
  k := TKCLKernel.Create;
  in1 := k.AddInput('in1');
  
  w.SetLength(1 * 1 * 16 * 16); b.SetLength(16);
  transp_node := k.AddConv2DTranspose(in1, w, b, 1, 2);
  
  w2.SetLength(1 * 1 * 16 * 16); b2.SetLength(16);
  out_node := k.AddConv2D(transp_node, w2, b2, 1, 1);
  
  k.MarkOutput(out_node);
  
  b_in1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [16, 16, 16]);
  b_out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [32, 32, 16]);
  
  try
    TKCLKernelCompiler.Dispatch(k, [b_in1, b_out]);
    PrintLn('  Success!');
  except
    on E: Exception do PrintLn('  FAILED: ' + E.Message);
  end;
end;

try
  TestConv2DTransposeConv;
except
  on E: Exception do PrintLn('Error: ' + E.Message);
end;
