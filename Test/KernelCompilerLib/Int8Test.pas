var b := TKCLStridedBuffer.Create(TKCLDataType.Int8, [1]);
b.SetData([0], 127);
PrintLn(b.GetData([0]));
b.SetData([0], -128);
PrintLn(b.GetData([0]));
