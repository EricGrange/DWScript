var a : array [String] of array of Integer;

a['abc'] := [1,2,3];

PrintLn(JSON.Stringify(a));

a['abc'] := [];

PrintLn(JSON.Stringify(a));

a['def'] := [4,5];

PrintLn(JSON.Stringify(a['abc']));
PrintLn(JSON.Stringify(a['def']));
