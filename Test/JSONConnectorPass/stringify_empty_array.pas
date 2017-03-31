var ab : array of Boolean;

PrintLn(JSON.Stringify(ab));

var aa : array of array of Boolean;

PrintLn(JSON.Stringify(aa));
aa.Add(ab);
PrintLn(JSON.Stringify(aa));

function TestAB : array of Boolean;
begin
    Result := ab;
end;

PrintLn(JSON.Stringify(TestAB));

function TestAA : array of array of Boolean;
begin
    Result := aa;
end;

PrintLn(JSON.Stringify(TestAA));