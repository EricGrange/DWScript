var bb : ByteBuffer;

for var i := 1 to 20 do begin
    bb.SetLength(4 + (i div 4) * 16);
    for var k := 0 to bb.Length-1 do
        if bb.GetByte(k) <> 0 then PrintLn('bug');
end;

bb.SetLength(4);
bb.SetInt32(0, $12345678);
bb.SetLength(0);
bb.SetLength(4);
PrintLn(bb.GetInt32(0));
