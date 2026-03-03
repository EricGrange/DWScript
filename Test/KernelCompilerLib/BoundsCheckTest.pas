var b1 := TStridedBuffer.Create(TDataType.Float32, [4]);

PrintLn("Write index test");
try
   b1.SetData([1000], 1.0); // out of bounds
except
   on E: Exception do
      PrintLn(E.Message);
end;

PrintLn("Read index test");
try
   var v := b1.GetData([1000]); // out of bounds
   PrintLn(v);
except
   on E: Exception do
      PrintLn(E.Message);
end;

PrintLn("Write capacity test");
try
   b1.SetData([-1], 1.0); // out of bounds
except
   on E: Exception do
      PrintLn(E.Message);
end;
