var v := JSON.Parse('[1,2,3]');

v.Delete(1);

PrintLn(v);

v.Delete(0);

PrintLn(v);

try
    v.Delete(123);
    PrintLn('Out of range expected');
except
end;
