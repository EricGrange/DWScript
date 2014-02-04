var
  crc32Tab : array [0..255] of Integer;

function crc32(aCRC32: integer; const data: string): integer;
var i: integer;
begin
  result := Unsigned32(aCRC32);
  for i := 1 to length(data) do
    result := crc32Tab[(result xor ord(data[i])) and $ff] xor (result shr 8);
end;

procedure InitCrc32Tab;
var i,n,crc: integer;
begin // this code generates a 1KB table
  for i := 0 to 255 do begin
    crc := i;
    for n := 1 to 8 do
      if (crc and 1)<>0 then
        // $edb88320 from polynomial p=(0,1,2,4,5,7,8,10,11,12,16,22,23,26)
        crc := Unsigned32((crc shr 1) xor $edb88320)
      else
        crc := crc shr 1;
    CRC32Tab[i] := crc;
  end;
end;

InitCrc32Tab;

PrintLn(UpperCase(IntToHex(Unsigned32(not crc32(-1, 'TestCRC32')), 8)));