var bb : ByteBuffer;

bb.SetLength(4);
bb.SetData('ab');
bb.SetData('cd');
try
    bb.SetData('ef');
except
    on E : Exception do PrintLn(E.Message);
end;

PrintLn(bb.GetData(0, 4));
try
    bb.GetData(1, 4);
except
    on E : Exception do PrintLn(E.Message);
end;

bb.SetData(1, 'xy');
PrintLn(bb.GetData(0, 3));
PrintLn(bb.GetData(1, 3));

