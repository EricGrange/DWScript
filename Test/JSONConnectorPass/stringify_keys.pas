var a : array[String] of Integer;
a['toto'] := 1;
PrintLn(JSON.Stringify(a.Keys));
a['titi'] := 2;
PrintLn(JSON.Stringify(a.Keys.Sort));

var b :  array [Integer] of String;
PrintLn(JSON.Stringify(b.Keys));
b[1] := 'toto';
PrintLn(JSON.Stringify(b.Keys));
b[3] := 'tata';
PrintLn(JSON.Stringify(b.Keys.Sort));

