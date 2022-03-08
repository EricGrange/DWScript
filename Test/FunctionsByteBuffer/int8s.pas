var bb : ByteBuffer;

bb.SetLength(3);

bb.SetInt8(-3);
try
    bb.SetInt8(-127);
except
    on E : Exception do PrintLn(E.Message);
end;
bb.SetInt8(-8);

PrintLn(bb.ToJSON);

bb.SetPosition(1);
PrintLn(bb.GetInt8);
try
    bb.GetInt8;
except
    on E : Exception do PrintLn(E.Message);
end;

PrintLn(bb.GetInt8(0));
PrintLn(bb.GetInt8(1));

