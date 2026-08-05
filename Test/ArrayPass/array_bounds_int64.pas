function MakeArr : array of Integer;
begin
   Result := new Integer[5];
   for var i := 0 to 4 do
      Result[i] := 100 + 10*i;
end;

var base : Integer := 0;
base := base + 4294967296;

try
   PrintLn(MakeArr()[base]);
   PrintLn('FAIL no exception (MakeArr()[2^32])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for 2^32');
end;

try
   PrintLn(MakeArr()[base + 4]);
   PrintLn('FAIL no exception (MakeArr()[2^32 + 4])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for 2^32 + 4');
end;

var a : array[0..4] of Integer;
for var i := 0 to 4 do
   a[i] := 100 + i;

try
   PrintLn(a[base]);
   PrintLn('FAIL no exception (a[2^32])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for 2^32');
end;

try
   PrintLn(a[base + 3]);
   PrintLn('FAIL no exception (a[2^32 + 3])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for 2^32 + 3');
end;

try
   a[base + 2] := 999;
   PrintLn('FAIL no exception on write (a[2^32 + 2])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised on write');
end;

PrintLn(a[2]);

var d : array of Integer;
d.SetLength(5);
for var i := 0 to 4 do
   d[i] := 200 + i;

try
   d[base + 1] := 999;
   PrintLn('FAIL no exception on dynamic write (d[2^32 + 1])');
except
   on E: Exception do PrintLn('OK out-of-bounds raised on dynamic write');
end;

PrintLn(d[1]);

try
   d.Delete(base);
   PrintLn('FAIL no exception (d.Delete(2^32))');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for Delete');
end;

try
   d.Insert(base, 1);
   PrintLn('FAIL no exception (d.Insert(2^32))');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for Insert');
end;

try
   d.Swap(base, 1);
   PrintLn('FAIL no exception (d.Swap(2^32, 1))');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for Swap');
end;

try
   d.Move(base, 1);
   PrintLn('FAIL no exception (d.Move(2^32, 1))');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for Move');
end;

try
   PrintLn(d.Copy(base, 1).Length);
   PrintLn('FAIL no exception (d.Copy(2^32, 1))');
except
   on E: Exception do PrintLn('OK out-of-bounds raised for Copy');
end;

PrintLn(d.Length);
