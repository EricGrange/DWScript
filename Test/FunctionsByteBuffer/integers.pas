var buf : ByteBuffer;

buf.SetLength(32);
for var i := 0 to buf.Length-1 do
   buf.SetByte(128 + i - buf.Length div 2);

function ToHex1(i : Integer) : String; begin Result := i.ToHexString(1) end;
function ToHex2(i : Integer) : String; begin Result := i.ToHexString(2) end;
function ToHex4(i : Integer) : String; begin Result := i.ToHexString(4) end;
function ToHex8(i : Integer) : String; begin Result := i.ToHexString(8) end;

for var signed := 0 to 1 do begin
   PrintLn(buf.GetIntegers(0, 32, 1, signed <> 0).Map(ToHex1).Join(','));
   PrintLn(buf.GetIntegers(1, 15, 2, signed <> 0).Map(ToHex2).Join(','));
   PrintLn(buf.GetIntegers(2, 7, 4, signed <> 0).Map(ToHex4).Join(','));
end;
PrintLn(buf.GetIntegers(0, 4, 8, True).Map(ToHex8).Join(','));

