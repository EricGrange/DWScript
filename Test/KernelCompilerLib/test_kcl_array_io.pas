try
   PrintLn('Testing Pascal Array of Float I/O...');

   var size := 1024;
   var buf := TKCLStridedBuffer.Create(TKCLDataType.Float32, [size]);

   var inArr : array of Float;
   inArr.SetLength(size);
   for var i := 0 to size - 1 do
      inArr[i] := i * 1.5;

   // 1. Test SetData
   buf.SetData(inArr);

   // Verify set was successful
   for var i := 0 to size - 1 do begin
      var v := buf.GetData([i]);
      if Abs(v - inArr[i]) > 0.0001 then
         raise Exception.Create('SetData mismatch at ' + IntToStr(i) + ': expected ' + FloatToStr(inArr[i]) + ', got ' + FloatToStr(v));
   end;
   PrintLn('SetData(array of Float) PASSED');

   // 2. Test GetData
   var outArr : array of Float;
   buf.GetData(outArr);

   if outArr.Length <> size then
      raise Exception.Create('GetData output array length mismatch: expected ' + IntToStr(size) + ', got ' + IntToStr(outArr.Length));

   for var i := 0 to size - 1 do begin
      if Abs(outArr[i] - inArr[i]) > 0.0001 then
         raise Exception.Create('GetData mismatch at ' + IntToStr(i) + ': expected ' + FloatToStr(inArr[i]) + ', got ' + FloatToStr(outArr[i]));
   end;
   PrintLn('GetData(var array of Float) PASSED');

   // 3. Test Failure cases (Float16 not supported yet)
   var buf16 := TKCLStridedBuffer.Create(TKCLDataType.Float16, [size]);
   try
      buf16.SetData(inArr);
      PrintLn('FAILED: SetData on Float16 should have raised exception');
   except
      on e: Exception do PrintLn('SetData Float16 error caught correctly: ' + e.Message);
   end;

   try
      buf16.GetData(outArr);
      PrintLn('FAILED: GetData on Float16 should have raised exception');
   except
      on e: Exception do PrintLn('GetData Float16 error caught correctly: ' + e.Message);
   end;

except
   on e: Exception do
      PrintLn('TEST FAILED: ' + e.Message);
end;
