var a : array [Integer] of Float;

for var i := 1 to 50 do
    a[i] := Sqrt(i);
    
var s := 0.0;    
var si := 0;

for var i in a.Keys do begin
    si += i;
    a[i] := Sqr(a[i]);
end;

PrintLn(si);

for var i := 50 downto 1 do
    if Round(a[i] * 1e8 - i * 1e8) <> 0 then 
        PrintLn('bug for ' + i.ToString);

    
        
    