var bb : ByteBuffer;

bb.SetLength(3);

bb.SetInt16(-3);
try
    bb.SetInt16(-127);
except
    on E : Exception do PrintLn(E.Message);
end;
bb.SetByte(-8);

PrintLn(bb.ToJSON);

bb.SetPosition(1);
PrintLn(bb.GetInt16);
try
    bb.GetInt16;
except
    on E : Exception do PrintLn(E.Message);
end;

PrintLn(bb.GetInt16(0));
PrintLn(bb.GetInt16(1));

