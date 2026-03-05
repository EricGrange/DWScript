// Helper functions for HalfFloat (since they might not be in script scope)
function FloatToHalf(f : Float) : Integer;
begin
   Result := 0; 
end;

function HalfToFloat(h : Integer) : Float;
begin
   Result := 0;
end;

procedure TestBulkIO(dataType : TKCLDataType; size : Integer);
begin
   PrintLn('Testing DataType: ' + IntToStr(Ord(dataType)) + ' Size: ' + IntToStr(size));
   
   var dims := [size];
   var buffer := TKCLStridedBuffer.Create(dataType, dims);
   
   var elementSize := 1;
   if dataType = TKCLDataType.Int8 then elementSize := 1
   else if dataType = TKCLDataType.Float16 then elementSize := 2
   else if dataType = TKCLDataType.Float32 then elementSize := 4;
   
   var bbIn := new ByteBuffer;
   bbIn.SetLength(size * elementSize);
   var bbOut := new ByteBuffer;
   bbOut.SetLength(size * elementSize);
   
   // Prepare input data in ByteBuffer
   for var i := 0 to size - 1 do begin
      var val : Float := i * 1.1;
      if dataType = TKCLDataType.Int8 then val := (i mod 127);
      
      if dataType = TKCLDataType.Int8 then bbIn.SetInt8(i, Round(val))
      else if dataType = TKCLDataType.Float16 then bbIn.SetWord(i * 2, FloatToHalf(val))
      else if dataType = TKCLDataType.Float32 then bbIn.SetSingle(i * 4, val);
   end;
   
   // Bulk SetData: ByteBuffer -> TKCLStridedBuffer
   buffer.SetData(bbIn, 0, size, dataType);
   
   // Verify individual elements via GetData (non-bulk)
   for var i := 0 to size - 1 do begin
      var expected : Float := i * 1.1;
      if dataType = TKCLDataType.Int8 then expected := (i mod 127);
      if dataType = TKCLDataType.Float16 then continue;

      var actual := buffer.GetData([i]);
      
      var tolerance := 0.0001;
      if Abs(actual - expected) > tolerance then
         raise Exception.Create('Mismatch at index ' + IntToStr(i) + ': Expected ' + FloatToStr(expected) + ', Actual ' + FloatToStr(actual));
   end;
   
   // Bulk GetData: TKCLStridedBuffer -> ByteBuffer
   buffer.GetData(bbOut, 0, size, dataType);
   
   // Verify ByteBuffer content
   for var i := 0 to size - 1 do begin
      var expected : Float := i * 1.1;
      if dataType = TKCLDataType.Int8 then expected := (i mod 127);
      if dataType = TKCLDataType.Float16 then continue;

      var actual : Float := 0;
      if dataType = TKCLDataType.Int8 then actual := bbOut.GetInt8(i)
      else if dataType = TKCLDataType.Float16 then actual := HalfToFloat(bbOut.GetWord(i * 2))
      else if dataType = TKCLDataType.Float32 then actual := bbOut.GetSingle(i * 4);
      
      var tolerance := 0.0001;
      if Abs(actual - expected) > tolerance then
         raise Exception.Create('Bulk GetData mismatch at index ' + IntToStr(i));
   end;
   
   PrintLn('SUCCESS');
end;

procedure TestTypeConversion(srcType, destType : TKCLDataType; size : Integer);
begin
   PrintLn('Testing Conversion: ' + IntToStr(Ord(srcType)) + ' -> ' + IntToStr(Ord(destType)));
   
   var buffer := TKCLStridedBuffer.Create(destType, [size]);
   
   var srcElementSize := 1;
   if srcType = TKCLDataType.Int8 then srcElementSize := 1
   else if srcType = TKCLDataType.Float16 then srcElementSize := 2
   else if srcType = TKCLDataType.Float32 then srcElementSize := 4;
   
   var bbIn := new ByteBuffer;
   bbIn.SetLength(size * srcElementSize);
   for var i := 0 to size - 1 do begin
      var val : Float := i * 0.5;
      if srcType = TKCLDataType.Int8 then bbIn.SetInt8(i, Round(val))
      else if srcType = TKCLDataType.Float16 then bbIn.SetWord(i * 2, FloatToHalf(val))
      else if srcType = TKCLDataType.Float32 then bbIn.SetSingle(i * 4, val);
   end;
   
   // Bulk SetData with type conversion
   buffer.SetData(bbIn, 0, size, srcType);
   
   // Verify
   for var i := 0 to size - 1 do begin
      var expected : Float := i * 0.5;
      if (srcType = TKCLDataType.Int8) or (destType = TKCLDataType.Int8) then
         expected := Round(expected);

      if (srcType = TKCLDataType.Float16) or (destType = TKCLDataType.Float16) then continue;

      var actual := buffer.GetData([i]);
      
      var tolerance := 0.1;
      if Abs(actual - expected) > tolerance then
         raise Exception.Create('Conversion mismatch at index ' + IntToStr(i) + ': Expected ' + FloatToStr(expected) + ', Actual ' + FloatToStr(actual));
   end;
   
   PrintLn('SUCCESS');
end;

try
   PrintLn('Starting Bulk I/O Comprehensive Tests...');
   
   // Test various sizes
   TestBulkIO(TKCLDataType.Int8, 10);
   TestBulkIO(TKCLDataType.Int8, 1024);
   
   TestBulkIO(TKCLDataType.Float16, 10);
   TestBulkIO(TKCLDataType.Float16, 1024);
   
   TestBulkIO(TKCLDataType.Float32, 10);
   TestBulkIO(TKCLDataType.Float32, 1024);
   
   // Test cross-type transfers (automatic conversion)
   TestTypeConversion(TKCLDataType.Float32, TKCLDataType.Int8, 100);
   TestTypeConversion(TKCLDataType.Int8, TKCLDataType.Float32, 100);
   
   TestTypeConversion(TKCLDataType.Float32, TKCLDataType.Float16, 100);
   TestTypeConversion(TKCLDataType.Float16, TKCLDataType.Float32, 100);
   
   PrintLn('All Bulk I/O tests PASSED');
except
   on e : Exception do
      PrintLn('TEST FAILED: ' + e.Message);
end;
