var bb : ByteBuffer;

bb.SetLength(5);

bb.SetDWord($01020304);
try
    bb.SetDWord(123456789);
except
    on E : Exception do PrintLn(E.Message);
end;
bb.SetByte($05);

PrintLn(bb.ToJSON);

bb.SetPosition(1);
PrintLn(bb.GetDWord.ToHexString(8));
try
    bb.GetDWord;
except
    on E : Exception do PrintLn(E.Message);
end;

PrintLn(bb.GetDWord(0).ToHexString(8));
PrintLn(bb.GetDWord(1).ToHexString(8));

