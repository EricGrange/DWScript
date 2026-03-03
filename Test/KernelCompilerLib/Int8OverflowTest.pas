var b := TStridedBuffer.Create(TDataType.Int8, [1]);
b.SetData([0], 300);
PrintLn(b.GetData([0]));
b.SetData([0], -300);
PrintLn(b.GetData([0]));
