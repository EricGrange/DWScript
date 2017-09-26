var bb : ByteBuffer;

bb.SetLength(8);
bb.SetDWord($40490fd8);
bb.SetSingle(3.141592);
try
    bb.SetSingle(0);
except
    on E : Exception do PrintLn(E.Message);
end;

PrintLn(bb.GetSingle(0));
PrintLn(bb.GetSingle(1));

bb.SetDouble(0, 3.141592);
PrintLn(bb.GetInt64(0).ToHexString(16));

try
    bb.SetDouble(1, 1.5);
except
    on E : Exception do PrintLn(E.Message);
end;
