var a : array of String;

procedure Test(var b : array of String);
begin
   b := '3:2:1'.Split(':');
   b.Sort;
end;

Test(a);

PrintLn(a.Join(','));