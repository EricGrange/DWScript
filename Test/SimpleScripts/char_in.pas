procedure Test(c : String);
begin
    Print(Ord(c[1]).ToString + ' : ');
    if c in [#0..#255] then Print('0');
    if c in [#1..#255] then Print('1');
    PrintLn('');
end;

Test('a');
Test(#0);
Test(#1);
Test(#256);
