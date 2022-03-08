var bb : ByteBuffer;

bb.SetLength(8);

try
   bb.SetByte(-1);
except on E: Exception do PrintLn(E.Message) end;
try
   bb.SetByte(256);
except on E: Exception do PrintLn(E.Message) end;

try
   bb.SetInt8(-129);
except on E: Exception do PrintLn(E.Message) end;
try
   bb.SetInt8(128);
except on E: Exception do PrintLn(E.Message) end;

try
   bb.SetWord(-1);
except on E: Exception do PrintLn(E.Message) end;
try
   bb.SetWord($10000);
except on E: Exception do PrintLn(E.Message) end;

try
   bb.SetInt16(-$8001);
except on E: Exception do PrintLn(E.Message) end;
try
   bb.SetInt16($8000);
except on E: Exception do PrintLn(E.Message) end;

try
   bb.SetDWord(-1);
except on E: Exception do PrintLn(E.Message) end;
try
   bb.SetDWord($100000000);
except on E: Exception do PrintLn(E.Message) end;

try
   bb.SetInt32(-$80000001);
except on E: Exception do PrintLn(E.Message) end;
try
   bb.SetInt32($80000000);
except on E: Exception do PrintLn(E.Message) end;
