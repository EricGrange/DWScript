var bb : ByteBuffer;

try
    bb.GetByte(0);
except
    on E : Exception do PrintLn(E.Message);
end;

try
    bb.GetByte(1);
except
    on E : Exception do PrintLn(E.Message);
end;

bb.SetLength(1);

try
    bb.GetByte(-1);
except
    on E : Exception do PrintLn(E.Message);
end;

try
    bb.GetByte(0);
except
    on E : Exception do PrintLn(E.Message);
end;

try
    bb.GetByte(1);
except
    on E : Exception do PrintLn(E.Message);
end;

try
    bb.SetPosition(-1);
except
    on E : Exception do PrintLn(E.Message);
end;

try
    bb.SetPosition(2);
except
    on E : Exception do PrintLn(E.Message);
end;

bb.SetPosition(0);
bb.GetByte;
try
    bb.GetByte;
except
    on E : Exception do PrintLn(E.Message);
end;

