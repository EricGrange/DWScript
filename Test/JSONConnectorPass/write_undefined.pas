var v : JSONVariant;

try
    v.bug := 123;
except
    on E : Exception do
        PrintLn(E.Message);
end;

try
    v['bug here'] := 'foo';
except
    on E : Exception do
        PrintLn(E.Message);
end;

try
    v[456] := Null;
except
    on E : Exception do
        PrintLn(E.Message);
end;