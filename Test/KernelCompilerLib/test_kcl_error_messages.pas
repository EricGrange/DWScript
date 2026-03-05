try
   var k := new TKCLKernel;
   var in1 := k.AddInput('in1');
   var in2 := k.AddInput('in2');
   var add := k.AddAdd(in1, in2);
   k.MarkOutput(add);
   
   var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4]);
   var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2]); // Mismatch
   var bOut := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4]);
   
   PrintLn('Testing broadcasting mismatch...');
   try
      TKCLKernelCompiler.Dispatch(k, [b1, b2, bOut]);
   except
      on e : Exception do PrintLn(e.Message);
   end;

   PrintLn('Testing output spatial domain length mismatch...');
   var b3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4]);
   var bOut2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4]); // Length mismatch
   try
      TKCLKernelCompiler.Dispatch(k, [b1, b3, bOut2]);
   except
      on e : Exception do PrintLn(e.Message);
   end;

   PrintLn('Testing output spatial domain size mismatch...');
   var bOut3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 5]); // Size mismatch at dim 1
   try
      TKCLKernelCompiler.Dispatch(k, [b1, b3, bOut3]);
   except
      on e : Exception do PrintLn(e.Message);
   end;

   PrintLn('Testing indices count mismatch...');
   try
      b1.GetData([0]); // Expected 2, got 1
   except
      on e : Exception do PrintLn(e.Message);
   end;

except
   on e : Exception do PrintLn('UNEXPECTED ERROR: ' + e.Message);
end;
