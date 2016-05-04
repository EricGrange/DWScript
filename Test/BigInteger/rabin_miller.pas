// Check Prime Sextuplet

function RabinMiller(n : BigInteger; k : Integer) : String;
begin
   if n <= 1 then exit 'prime';
   if not n.IsOdd then exit 'multiple of two';

   var d := n - 1;
   var s := 0;

   while not d.IsOdd do begin
      d := d div 2;
      s += 1;
   end;

   for var i := 1 to k do begin
   
      var a := 2 + RandomBigInteger( n-4 );

      var x := a.ModPow(d, n);
      if (x = 1) or (x = n - 1) then
         continue;

      for var j:=1 to s-1 do begin
         x := x.ModPow(2, n);
         if x = 1 then
            exit 'composite1';
         if x = n-1 then
            break;
      end;
      if x <> n-1 then
         exit 'composite2';
   end;
   Result := 'probably prime';
end;

//  from Riecoin block 500
var n := BigInteger('40378229068348060265902071710277325464903647442059563624162746921011818446300065249638243017389009856122930741656904767');
if not Odd(n) then Exit;

for var k := 0 to 22 step 2 do begin
   Print('n+'+k.ToString+' : ');
   PrintLn(RabinMiller(n+k, 6));
end;