var v : JSONVariant = 1;
var a : Integer = v;    // OK
var b : array of Integer;
b.Add(v);               // OK
var c : array[String] of Integer;
c['test'] := v;         // KO

PrintLn(JSON.Stringify(c));