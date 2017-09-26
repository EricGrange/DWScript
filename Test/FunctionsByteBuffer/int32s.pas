var bb : ByteBuffer;

bb.SetLength(5);

bb.SetInt32(-$01020304);
try
    bb.SetInt32(123456789);
except
    on E : Exception do PrintLn(E.Message);
end;
bb.SetByte(-$05);

PrintLn(bb.ToJSON);

bb.SetPosition(1);
PrintLn(bb.GetInt32);
try
    bb.GetInt32;
except
    on E : Exception do PrintLn(E.Message);
end;

PrintLn(bb.GetInt32(0));
PrintLn(bb.GetInt32(1));

