var bb : ByteBuffer;

bb.SetLength(9);

bb.SetInt64($01020304050708);
try
    bb.SetInt64(0);
except
    on E : Exception do PrintLn(E.Message);
end;
bb.SetByte($09);

PrintLn(bb.ToJSON);

bb.SetPosition(1);
PrintLn(bb.GetInt64);

PrintLn(bb.GetInt64(0));
PrintLn(bb.GetInt64(1));

