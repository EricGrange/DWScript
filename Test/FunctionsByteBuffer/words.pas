var bb : ByteBuffer;

bb.SetLength(3);

bb.SetWord(34*256+12);
try
    bb.SetWord(56);
except
    on E : Exception do PrintLn(E.Message);
end;
bb.SetByte($56);

PrintLn(bb.ToJSON);

bb.SetPosition(1);
PrintLn(bb.GetWord);
try
    bb.GetWord;
except
    on E : Exception do PrintLn(E.Message);
end;

PrintLn(bb.GetWord(0));
PrintLn(bb.GetWord(1));

