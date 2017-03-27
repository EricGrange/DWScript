var a : array [Integer] of Boolean;

for var i := 2 to 150 do
    a[i] := True;
    
for var i := 2 to 150 do begin
    var prime := a[i];
    var k := i;
    while k <= 150 do begin
        a.Delete(k);
        k += i;
    end;
    if prime then begin
        PrintLn(i);
        a[i] := True;
    end;
end;
PrintLn('---');
PrintLn(a.Length);
PrintLn(a.Keys.Sort.Map(IntToStr).Join(','));
    
    