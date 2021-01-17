type TRange = enum (Low = 2, High = 1000);

var sieve : set of TRange;

var last : Integer;

for var e in TRange do begin
   if e in sieve then continue;
   PrintLn(e.Value);
   for var k:=e to TRange.High step e do
      sieve.Include(k);
end;