var v := JSON.Parse('[1,2,3]');

v.Swap(0, 2);

PrintLn(v);

v.Swap(2, 1);

PrintLn(v);

try
    v.Swap(-1, 0);
    PrintLn('Out of range expected');
except
end;
